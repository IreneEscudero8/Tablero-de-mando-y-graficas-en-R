# ---------- Shiny Dashboard: World Happiness Report (2019-2024) ----------------------------- 
 
# Carga de Librerias 
library(shiny) 
library(ggplot2) 
library(dplyr) 
library(leaflet) 
library(forcats)  
library(sf)  
library(rnaturalearth)  
library(rnaturalearthdata)  
library(DT) 
library(tidyr)  
library(igraph)  
library(ggraph) 
library(ggrepel) 
 
#------------------ Carga y limpieza -------------------------------- 
 
df <- read.csv("WHR25_Data_Figure_2.1.csv") 
 
# Renombrando columnas y filtrando 
df_whr <- df %>%  
  filter(Year >= 2019) %>% 
  rename( 
    LogGDPperCapita = Explained.by..Log.GDP.per.capita, 
    Freedom = Explained.by..Freedom.to.make.life.choices, 
    SocialSupport = Explained.by..Social.support, 
    LifeExpectancy = Explained.by..Healthy.life.expectancy, 
    Generosity = Explained.by..Generosity, 
    PerceptionsOfCorruption = Explained.by..Perceptions.of.corruption, 
    Country = Country.name, 
    Score = Ladder.score 
  ) %>% 
  
select(Year,Rank,Score,Country,upperwhisker,lowerwhisker,LogGDPperCapita,Freedom,SocialSuppo
rt,LifeExpectancy,Generosity,PerceptionsOfCorruption) 
 
 
max_anio <- max(df_whr$Year,na.rm = TRUE)  
df_2024 <- df_whr %>%  
  filter(Year == max_anio)  
 
mapa_m <- ne_countries(scale = "medium", returnclass = "sf") %>%  
  select(sovereignt, name_long, geometry) %>%  
  rename(Country_Map = name_long)  
 
12 
top15 <- df_2024 %>%  
  arrange(desc(Score)) %>%  
  head(15) %>%  
  pull(Country)  
 
 
dfno_2024 <- mapa_m %>%  
  left_join(df_2024, by = c("Country_Map" = "Country")) %>% 
  rename(Country_Join = Country_Map) %>% 
  mutate(  
    Top_15 = Country_Join %in% top15 
  ) 
 
df3 <- df_whr 
 
# Grafico 2  
cor_data_2024 <- df_2024 %>% 
  select(Score, LogGDPperCapita, SocialSupport, LifeExpectancy, Freedom, Generosity, 
PerceptionsOfCorruption) %>% 
  rename( 
    Happiness_Score = Score,  
    GDP = LogGDPperCapita,  
    Social_Support = SocialSupport,  
    Life_Expectancy = LifeExpectancy,  
    Corruption = PerceptionsOfCorruption 
  ) 
 
 
# Calculo de la matriz de correlacion para el Grafico 2 
cor_matrix <- cor_data_2024 %>% 
  cor(use = "pairwise.complete.obs") 
 
# Matriz a formato tidy para ggraph 
cor_tidy <- cor_matrix %>% 
  as_tibble(rownames = "source") %>% 
  pivot_longer(-source, names_to = "target", values_to = "correlation") %>% 
  filter(source != target, abs(correlation) > 0.3) 
 
gdata <- graph_from_data_frame(d = cor_tidy, directed = FALSE) 
 
 
#Gráfica facets por Continente 
paises_america <- c("Mexico", "Canada", "United States", "Costa Rica", "Panama", "Chile") 
paises_europa <- c("Finland", "Germany", "France", "Spain", "United Kingdom", "Norway") 
paises_asia <- c("Japan", "South Korea", "China", "India", "Singapore", "Thailand") 
paises_africa <- c("South Africa", "Nigeria", "Egypt", "Kenya", "Morocco","Algeria") 
paises_ocean <- c("Australia", "New Zealand") 
paises_incluidos <- c(paises_america, paises_europa, paises_asia, paises_africa, paises_ocean) 
 
# Filtrar los datos y crear la columna de continente de forma manual 
df_combinado <- df3 %>% 
  filter(Country %in% paises_incluidos) %>% 
13 
  mutate( 
    # Asignación manual de faceta y orden 
    Continent_Manual = case_when( 
      Country %in% paises_america ~ "1. América", 
      Country %in% paises_europa ~ "2. Europa", 
      Country %in% paises_asia ~ "3. Asia", 
      Country %in% paises_africa ~ "4. África", 
      Country %in% paises_ocean ~ "5. Oceanía", 
      TRUE ~ "Otro" 
    ), 
    Year = as.numeric(Year) 
  ) %>% 
  # Filtrar por el rango de años 
  filter(Year >= 2019, Year <= 2024) 
 
# Factor ordenado para garantizar la secuencia de facetas 
orden_continentes <- c("1. América", "2. Europa","3. Asia","4. África","5. Oceanía") 
df_combinado$Continent_Manual <- factor( 
  df_combinado$Continent_Manual, 
  levels = orden_continentes 
) 
 
# Creamos el dataframe SOLO con el último punto de cada serie (Year == 2024) 
df_etiquetas <- df_combinado %>% 
  group_by(Country, Continent_Manual) %>% 
  filter(Year == max(Year)) %>%  
  ungroup() 
 
grafica_facets <- df_combinado %>% 
  ggplot(aes(x = Year, y = Score, color = Country)) + 
  geom_line(size = 1.2) + 
  geom_point() + 
  geom_text_repel( #Evitar que se encimen 
    data = df_etiquetas,  
    aes(label = Country),  
    size = 3.5, 
    nudge_x = 0.5,  
    direction = "y",  
    hjust = 0,  
    segment.color = NA,  
    show.legend = FALSE  
  ) + 
  facet_wrap(~ Continent_Manual,  
             scales = "free_y",  
             ncol = 5, 
             strip.position = "top"  
  ) + 
  labs( 
    title = "Evolución del Índice de Felicidad por Continente(2019-2024) ",  
    subtitle = "Tendencias en países seleccionados (fijese muy bien en el eje Y de cada continente, 
note como cambia el rango)", 
    x = "Año",  
14 
    y = "Puntaje", 
    color = NULL  
  ) + 
  scale_x_continuous( 
    breaks = unique(df_combinado$Year),  
    expand = expansion(mult = c(0.05, 0.25)) #Para que quepan las etiquetas por país 
  ) +  
  theme_minimal(base_size = 12) + 
  theme( 
    legend.position = "none", # Oculta la leyenda global 
    plot.title = element_text(face = "bold"), 
    # Continente por faceta 
    strip.text = element_text(face = "bold", size = 11, color = "white"), 
    strip.background = element_rect(fill = "#5DADE2", color = NA), 
     
     
  ) 
 
print(grafica_facets) 
 
palRank <- colorNumeric(palette = c("#E0E0E0","#FFD700"),domain=dfno_2024$Happiness_Score) 
palGDP <- colorNumeric(palette ="Greens",domain=dfno_2024$GDP) 
palSocSup <- colorNumeric(palette ="Purples",domain=dfno_2024$Social_Support) 
palLifE <- colorNumeric(palette ="Blues",domain=dfno_2024$Life_Expectancy) 
palFree <- colorNumeric(palette = c("#d74511","#179a62"),domain=dfno_2024$Freedom) 
palGen <- colorNumeric(palette ="Oranges",domain=dfno_2024$Generosity) 
palCorr <- colorNumeric(palette ="Reds",domain = dfno_2024$Corruption) 
 
 
cont_popup <- paste0( 
  "<strong>País: </strong>", dfno_2024$Country_Join, "<br>", 
  "<strong>Puesto: </strong>", dfno_2024$Score, "<br>", 
  "<strong>Puntiación de Felicidad (0-7): </strong>", round(dfno_2024$Score, 3), "<br>", 
  "<strong>PIB: </strong>", round(dfno_2024$LogGDPperCapita, 3), "<br>", 
  "<strong>Percepción de Apoyo Social: </strong>", round(dfno_2024$SocialSupport, 3), "<br>", 
  "<strong>Esperanza de Vida: </strong>", round(dfno_2024$LifeExpectancy, 3), "<br>", 
  "<strong>Percepción de Libertad: </strong>", round(dfno_2024$Freedom, 3), "<br>", 
  "<strong>Percepción de Generosidad: </strong>", round(dfno_2024$Generosity, 3), "<br>", 
  "<strong>Percepción de Corrupción: </strong>", round(dfno_2024$PerceptionsOfCorruption, 3) 
) 
 
#Gráfico con cada uno de los indicadores 
fel_glob <- leaflet(dfno_2024) %>% 
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addPolygons( 
    fillColor = ~palRank(Score), 
    weight = 1, 
    opacity = 1, 
    color = "black",  
    fillOpacity = 0.8, 
    popup = cont_popup, 
    highlightOptions = highlightOptions( 
15 
      weight = 3, 
      color = "#FFFF00",  
      bringToFront = TRUE), 
    group = "Score") %>% 
  addLegend( 
    pal = palRank, 
    values = ~Score, 
    opacity = 0.7, 
    title = paste("Score de Felicidad (", max_anio, ")"), 
    position = "bottomleft", 
    group = "Score") %>% 
  addPolygons( 
    fillColor = ~palGDP(LogGDPperCapita), 
    weight = 1, 
    opacity = 1, 
    color = "black",  
    fillOpacity = 0.8, 
    popup = cont_popup, 
    highlightOptions = highlightOptions( 
      weight = 3, 
      color = "#FFFF00",  
      bringToFront = TRUE), 
    group = "GDP") %>% 
  addLegend( 
    pal = palGDP, 
    values = ~LogGDPperCapita, 
    opacity = 0.7, 
    title = paste("PIB (", max_anio, ")"), 
    position = "bottomleft", 
    group = "GDP") %>% 
  addPolygons( 
    fillColor = ~palSocSup(SocialSupport), 
    weight = 1, 
    opacity = 1, 
    color = "black",  
    fillOpacity = 0.8, 
    popup = cont_popup, 
    highlightOptions = highlightOptions( 
      weight = 3, 
      color = "#FFFF00",  
      bringToFront = TRUE), 
    group = "Social Support") %>% 
  addLegend( 
    pal = palSocSup, 
    values = ~SocialSupport, 
    opacity = 0.7, 
    title = paste("Percepción de Social Support (", max_anio, ")"), 
    position = "bottomleft", 
    group = "Social Support") %>% 
  addPolygons( 
    fillColor = ~palLifE(LifeExpectancy), 
    weight = 1, 
16 
    opacity = 1, 
    color = "black",  
    fillOpacity = 0.8, 
    popup = cont_popup, 
    highlightOptions = highlightOptions( 
      weight = 3, 
      color = "#FFFF00",  
      bringToFront = TRUE), 
    group = "Life Expectancy") %>% 
  addLegend( 
    pal = palLifE, 
    values = ~LifeExpectancy, 
    opacity = 0.7, 
    title = paste("Esperanza de Vida (", max_anio, ")"), 
    position = "bottomleft", 
    group = "Life Expectancy") %>% 
  addPolygons( 
    fillColor = ~palFree(Freedom), 
    weight = 1, 
    opacity = 1, 
    color = "black",  
    fillOpacity = 0.8, 
    popup = cont_popup, 
    highlightOptions = highlightOptions( 
      weight = 3, 
      color = "#FFFF00",  
      bringToFront = TRUE), 
    group = "Freedom") %>% 
  addLegend( 
    pal = palFree, 
    values = ~Freedom, 
    opacity = 0.7, 
    title = paste("Percepción de libertad (", max_anio, ")"), 
    position = "bottomleft", 
    group = "Freedom") %>% 
  addPolygons( 
    fillColor = ~palGen(Generosity), 
    weight = 1, 
    opacity = 1, 
    color = "black",  
    fillOpacity = 0.8, 
    popup = cont_popup, 
    highlightOptions = highlightOptions( 
      weight = 3, 
      color = "#FFFF00",  
      bringToFront = TRUE), 
    group = "Generosity") %>% 
  addLegend( 
    pal = palGen, 
    values = ~Generosity, 
    opacity = 0.7, 
    title = paste("Percepción de Generosidad (", max_anio, ")"), 
17 
    position = "bottomleft", 
    group = "Generosity") %>% 
  addPolygons( 
    fillColor = ~palCorr(PerceptionsOfCorruption), 
    weight = 1, 
    opacity = 1, 
    color = "black",  
    fillOpacity = 0.8, 
    popup = cont_popup, 
    highlightOptions = highlightOptions( 
      weight = 3, 
      color = "#FFFF00",  
      bringToFront = TRUE), 
    group = "Corruption") %>% 
  addLegend( 
    pal = palCorr, 
    values = ~PerceptionsOfCorruption, 
    opacity = 0.7, 
    title = paste("Percepción de Corrupción (", max_anio, ")"), 
    position = "bottomleft", 
    group = "Corruption") %>% 
  addLayersControl(overlayGroups = c("Rank","GDP","Social Support","Life 
Expectancy","Freedom","Generosity","Corruption"), 
                   options = layersControlOptions(collapsed = F))%>% 
  setView(lng=0,lat=0,zoom=2) 
fel_glob   
 
 
 
#--------------------------- Interfaz de usuario (UI) -------------------------------------- 
 
ui <- fluidPage( 
  titlePanel("Cuadro de Mando: Felicidad Global (2019-2024)"), 
   
  # Menu de Navegacion 
  fluidRow( 
    column(12, 
           h4("Menú de Navegación"), 
           radioButtons("page_select", NULL, # NULL para no mostrar un titulo extra 
                        choices = c("Página 1: Introducción", "Página 2: Mapa y Progreso Anual", "Página 3: 
Tabla de Datos"), 
                        selected = "Página 1: Introducción", 
                        inline = TRUE) # Muestra las opciones en horizontal 
    ) 
  ), 
  hr(), 
   
  fluidRow( 
    column(12, 
           uiOutput("dynamic_content") 
    ) 
  ) 
18 
) 
 
#--------------------------------------- server ------------------------------------- 
 
server <- function(input, output, session) { 
   
  # Inicializa el pais seleccionado con el top 1 para mostrar algo al inicio 
  selected_country <- reactiveVal(df_2024 %>% arrange(desc(Score)) %>% head(1) %>% 
pull(Country)) 
   
  # Contenido dinámico (renderUI) 
  output$dynamic_content <- renderUI({ 
     
    if (input$page_select == "Página 1: Introducción") { 
      # Pagina 1: Introduccion (Combinada con texto y gráficos) 
      fluidPage( 
        h2("Análisis del World Happiness Report (2019-2024)", style="color: #3498db; font-weight: 
bold;"), 
        hr(), 
         
        # Texto de Bienvenida 
        fluidRow( 
          column(12, 
                 p("Este cuadro de mando interactivo presenta un análisis de los datos del World 
Happiness Report (Informe Mundial de la Felicidad) desde 2019 hasta 2024."), 
                 p("El", tags$b("World Happiness Report (WHR)") ,"es  publicado por el Wellbeing Research 
Centre at the University of Oxford (Centro de Investigación del Bienestar de la Universidad de Oxford) 
en colaboración con Gallup y la Red de Soluciones para el Desarrollo Sostenible de las Naciones 
Unidas. Clasifica a los países según qué tan felices perciben sus ciudadanos que son."), 
                 p("La", tags$b("Puntuación de Felicidad (Score)"), "se basa en la evaluación de vida que 
realizan los encuestados (la puntuación más alta es mejor)."), 
                 p("Los factores clave considerados en el cálculo de la puntuación de felicidad incluyen:"), 
                 tags$ul( 
                   tags$li(tags$b("Logaritmo del PIB per Cápita")), 
                   tags$li(tags$b("Apoyo Social")), 
                   tags$li(tags$b("Esperanza de Vida Saludable")), 
                   tags$li(tags$b("Libertad para Tomar Decisiones de Vida")), 
                   tags$li(tags$b("Generosidad")), 
                   tags$li(tags$b("Percepciones de Corrupción")) 
                 ), 
                 p("A continuación, se presentan análisis de correlación y distribución de factores. 
Selecciona una de las otras páginas en el menú superior para explorar el mapa o la tabla completa.") 
          ) 
        ), 
         
        hr(), 
         
        fluidRow( 
          column(4, 
                 h3("Distribución de Factores por Año"), 
                 # Selector de año 
                 selectInput("year_box", "Seleccione Año:", 
19 
                             choices = sort(unique(df_whr$Year)), 
                             selected = max_anio) 
          ), 
          column(8, 
                 # Output del Gráfico de Caja 
                 plotOutput("boxplot_factors", height = 400) 
          ) 
        ), 
         
        hr(), 
         
        fluidRow( 
          column(12, 
                 h3("Vínculos de Correlación entre Factores (Año: 2024)"), 
                 p(class = "text-muted", "Análisis de las relaciones entre las variables, destacando 
correlaciones fuertes."), 
                 # Output del Gráfico de Correlación 
                 plotOutput("correlation_network", height = 500) 
          ) 
        ), 
        fluidRow( 
          column(12, 
                 h3("Evolución del Índice de Felicidad por Continente (2019-2024)"), 
                 p(class = "text-muted", "Tendencias de felicidad en países seleccionados, agrupadas por 
continente para facilitar la comparación."), 
                 # Output del Gráfico de Facetas 
                 plotOutput("facet_plot", height = 400) # <- Aquí se muestra 
          ) 
        ) 
      ) 
       
    } else if (input$page_select == "Página 2: Mapa y Progreso Anual") { 
      # Pagina 2: Mapa y Progreso Anual 
      fluidPage( 
        h2("Análisis Interactivo del World Happiness Report", style="color: #3498db; font-weight: bold;"), 
         
        fluidRow( 
           
          # Grafico con el resultado  
          column(6, 
                 h3("Evolución de Felicidad"), 
                 htmlOutput("selected_country_title", style="color: #3498db; font-weight: bold;"),  
                 plotOutput("progress_plot", height = 450) 
          ), 
           
          # Mapa Leaflet interactivo  
          column(6, 
                 h3("Mapa Interactivo"), 
                 p(class = "text-muted", "Haz clic en un país en el mapa para ver su progreso de felicidad a 
lo largo de los años."), 
                 leafletOutput("happiness_map", height = 500) 
          ) 
20 
        ), 
        fluidRow( 
          column(12, 
                 h3("Mapa Interactivo con los Indicadores"), 
                 p(class = "text-muted", "Selecciona una indicador para ver como se comportan los 
países."), 
                  
                 leafletOutput("indicadores_map") 
          ) 
        ) 
      ) 
       
    } else { 
      # Pagina 3:Tabla de Datos  
      fluidPage( 
        h2("Tabla de Datos del World Happiness Report (2019-2024)"), 
        fluidRow( 
          column(12, 
                 h3("Dataset Completo"), 
                 DTOutput("data_table") 
          ) 
        ) 
      ) 
    } 
  }) 
   
  # Boxplot de Factores por Año Seleccionado 
  output$boxplot_factors <- renderPlot({ 
     
    # Filtrar datos por el año seleccionado 
    data_filtered <- df_whr %>% 
      filter(Year == input$year_box) %>% 
      select(LogGDPperCapita, Freedom, SocialSupport, LifeExpectancy, Generosity, 
PerceptionsOfCorruption) %>% 
      pivot_longer(cols = everything(), names_to = "Factor", values_to = "Valor") 
     
    # Mapear nombres largos para el gráfico 
    data_filtered$Factor_Largo <- factor(data_filtered$Factor, 
                                         levels = c("LogGDPperCapita", "SocialSupport", "LifeExpectancy", 
"Freedom", "Generosity", "PerceptionsOfCorruption"), 
                                         labels = c("Log PIB", "Soporte Social", "Esp. Vida", "Libertad", 
"Generosidad", "Corrupción")) 
     
    # Gráfico de Caja - Uso de color azul claro 
    ggplot(data_filtered, aes(x = Factor_Largo, y = Valor)) +  
      geom_boxplot(alpha = 0.7, outlier.color = "red", fill = "#A0C4FF") + # Azul claro forzado 
      labs( 
        title = paste("Distribución de Factores de Felicidad (", input$year_box, ")"), 
        x = "Factor", 
        y = "Valor de Contribución" 
      ) + 
      theme_minimal() + 
21 
      theme(axis.text.x = element_text( hjust = 1), 
            legend.position = "none")  
  }) 
   
  # Correlación de Vínculos 
  output$correlation_network <- renderPlot({ 
     
    ggraph(gdata, layout = "nicely") + 
      geom_edge_fan(aes(alpha = abs(correlation), colour = correlation, width=correlation),  
        end_cap = circle(5, 'mm')) + 
      scale_edge_color_gradient2( 
        low = "grey",  
        mid = "lightgrey",  
        high = "darkgreen",  
        midpoint = 1/2,  
        limits = c(0, 1), 
        name = "Coeficiente de Correlación") + 
      geom_node_point(size = 10, color = "#F0E68C", fill = "#B8860B", shape = 21) + 
      geom_node_text(aes(label = name), repel = TRUE, size = 4) + 
      labs( 
        #title = paste("Correlaciones entre los Factores de la Felicidad (Año: 2024)"), 
        subtitle = "Se muestran vínculos con correlación mayor a 0.3", 
        caption = "El color y el grosor indican correlación)") + 
      theme_void() + 
      theme( 
        #plot.title = element_text(hjust = 0.5, face = "bold"), 
        plot.subtitle = element_text(hjust = 0.5), 
        legend.position = "bottom" 
      ) 
  }) 
   
  output$facet_plot <- renderPlot({ print(grafica_facets) }) 
   
  # Paleta de colores para el mapa (Top 15 vs otros) 
  pal_map <- colorFactor( 
    palette = c("#E0E0E0", "#FFD700"), 
    domain = c(FALSE, TRUE) 
  ) 
   
  output$happiness_map <- renderLeaflet({ 
     
    #Popup con estadisticas del pais 
    map_popup <- paste0( 
      "<strong>País: </strong>", dfno_2024$Country_Join, "<br>", 
      "<strong>Puntuación (", max_anio, "): </strong>", round(dfno_2024$Score, 3), "<br>", 
      "<strong>Top 15: </strong>", ifelse(dfno_2024$Top_15, "Sí", "No"), "<br>", 
      "<strong>PIB (Log): </strong>", round(dfno_2024$LogGDPperCapita, 3)  
    ) 
     
    leaflet(dfno_2024) %>% 
      addProviderTiles(providers$CartoDB.Positron) %>% 
      setView(lng = 0, lat = 30, zoom = 2) %>% 
22 
      addPolygons( 
        fillColor = ~pal_map(Top_15), # Colorea Top 15 en Amarillo 
        weight = 1, 
        opacity = 1, 
        color = "white",   
        fillOpacity = 0.8, 
        popup = map_popup, 
        layerId = ~Country_Join, # ID para interactividad (Clave para el observeEvent) 
        highlightOptions = highlightOptions( 
          weight = 2, 
          color = "#0000FF",  
          fillOpacity = 0.9, 
          bringToFront = TRUE 
        ) 
      ) %>% 
      addLegend( 
        pal = pal_map,  
        values = ~Top_15,  
        opacity = 0.7,  
        title = "Top 15 Países",  
        position = "bottomleft", 
        labels = c("Otros Países", "Top 15 Países más Felices") 
      ) 
  }) 
   
  output$indicadores_map <- renderLeaflet({ print(fel_glob) }) 
     
     
  # Logica para el clic en el mapa  
  observeEvent(input$happiness_map_shape_click, { 
    # El ID del poligono clicado es el nombre del pais  
    if (!is.na(input$happiness_map_shape_click$id) && input$happiness_map_shape_click$id %in% 
df_whr$Country) { 
      selected_country(input$happiness_map_shape_click$id) 
    } 
  }) 
   
   
  # Titulo dinamico para el grafico basado en el pais seleccionado 
  output$selected_country_title <- renderUI({ 
    req(selected_country()) 
    HTML(paste0("<h3>País Seleccionado: ", selected_country(), "</h3>")) 
  }) 
   
  output$progress_plot <- renderPlot({ 
    req(selected_country()) 
     
    # Filtrar los datos año para el pais 
    data_filtered <- df_whr %>%  
      filter(Country == selected_country()) %>% 
      arrange(Year) 
     
23 
    # Grafico de lineas 
    ggplot(data_filtered, aes(x = Year, y = Score)) + 
      geom_line(color = "#3b82f6", linewidth = 1.8) + 
      geom_point(color = "#3b82f6", size = 5) + 
      labs( 
        title = paste("Evolución de Felicidad"), 
        x = "Año", 
        y = "Puntuación de Felicidad" 
      ) + 
      scale_x_continuous(breaks = unique(df_whr$Year)) + 
      theme_minimal()  
  }) 
   
   
  # Pagina 3 
  output$data_table <- renderDT({ 
    # Mostramos el dataset (df3) con las variables renombradas 
    data_display <- df3 %>% 
      select(Year, Rank, Country, Score, LogGDPperCapita, SocialSupport, LifeExpectancy, Freedom, 
Generosity, PerceptionsOfCorruption) 
     
    datatable(data_display,  
              options = list( 
                pageLength = 10 
              ), 
              # Titulo de la tabla  
              caption = htmltools::tags$caption( 
                style = 'caption-side: top; text-align: left; color:#3b82f6; font-size:150%;', 
                'Tabla de Felicidad Global (2019-2024)' 
              ) 
    ) %>% 
      # Formateamos las columnas numericas  
      formatRound(columns = c("Score", "LogGDPperCapita", "SocialSupport", "LifeExpectancy", 
"Freedom", "Generosity", "PerceptionsOfCorruption"), digits = 3) 
  }) 
} 
 
# Ejecutar  
shinyApp(ui, server) 
