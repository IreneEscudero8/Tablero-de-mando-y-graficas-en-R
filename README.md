# Análisis del Índice de Felicidad Mundial (2012-2024)

Este proyecto representa el trabajo final para la materia de **Visualización de Información** en el **Instituto Tecnológico Autónomo de México (ITAM)**, correspondiente al semestre de Otoño 2025.

## Descripción del Proyecto
El objetivo principal es analizar el **Indice de Felicidad Mundial (World Happiness Report)** utilizando datos que abarcan desde 2012 hasta 2024. El proyecto transforma cifras complejas en herramientas visuales e interactivas, como gráficas en R y una aplicación interactiva en Shiny, para facilitar la comprensión de cómo las personas califican su bienestar a nivel global.

### Autores
* Escudero Cázarez Irene 
* Brussi Staines O. Iñaki 

---

## Metodología y Datos
Se seleccionaron y unificaron datasets de **Kaggle** y del **World Happiness Report** para asegurar la continuidad del análisis después de 2019.

### Indicadores Clave:
* **Happiness Score:** Puntuación general de felicidad (escala 0-10).
* **Log GDP per Capita:** Estimación del nivel económico por persona.
* **Social Support:** Percepción de apoyo social en el entorno.
* **Healthy Life Expectancy:** Esperanza de vida saludable al nacer.
* **Freedom to Make Life Choices:** Grado de libertad percibido para tomar decisiones.
* **Generosity:** Niveles de altruismo o donaciones.
* **Perceptions of Corruption:** Nivel de confianza en la ausencia de corrupción.

---

## Implementación con Shiny
La aplicación interactiva se estructura en tres vistas principales:

1.  **Página 1: Introducción:** Presenta el contexto del proyecto, análisis de correlación y la distribución de factores por año.
2.  **Página 2: Mapa y Progreso Anual:** Incluye un **Mapa Leaflet** interactivo que permite explorar indicadores geográficamente y visualizar la evolución temporal de la felicidad de un país específico mediante un clic.
3.  **Página 3: Tabla de Datos:** Una vista de datos crudos que permite realizar consultas detalladas, filtrar y reordenar la información.

---

## Tecnologías Utilizadas
El desarrollo se realizó íntegramente en **R**, empleando las siguientes librerías:
* `shiny` y `shinydashboard` para la interfaz interactiva.
* `ggplot2` para la generación de gráficas estáticas (histogramas, boxplots, facetas).
* `leaflet` para la cartografía dinámica.
* `dplyr` y `tidyr` para la limpieza y unificación de datos.
* `DT` para la visualización de tablas de datos.
* `igraph` y `ggraph` para el análisis de redes de correlación.

---

## Hallazgos Relevantes
* **Factores determinantes:** El PIB per cápita y el Apoyo Social son los factores que muestran una relación positiva más fuerte con el puntaje de felicidad.
* **Países Líderes (2024):** Finlandia encabeza el ranking con una puntuación de 7.736, seguida de Dinamarca (7.521) e Islandia (7.515).
* **Distribución Geográfica:** 7 de los 10 países con mayor índice de felicidad en 2024 se encuentran en Europa.
* **Calidad de Datos:** Tras la unificación, solo se detectaron 22 valores ausentes en un total de 875 registros, lo que garantiza la integridad del análisis.
