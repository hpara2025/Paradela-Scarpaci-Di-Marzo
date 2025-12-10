library(readr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(maps)
library(geoAr)


provincias <- read.csv("~/Escritorio/Ejercicio R/provincias.csv")

provincias <- provincias %>%
  rename(situacion_calle = Personas.en.situación.de.calle..vía.pública.)

# Analisis: Porcentaje de personas en situacion de calle por provincia

# 1: Creacion de columna con porcentajes de personas en situacion de calle por provincia con la funcion mutate.

provincias_ranking <- provincias %>%
  mutate(porcentaje_calle = (situacion_calle / Población..2022.) * 100) 

# 2: Grafico de puntos en mapa de Argentina con la funciones ggplot + geom_sf para la division de provincias + geom point. Se aclaran los valores con geom_text.

ggplot() +
  geom_sf(data = get_geo("ARGENTINA", level = "provincia"), 
          fill = "white", color = "yellow", size = 0.5) +
  geom_point(data = provincias_ranking, 
             aes(x = Longitud.del.centroide, 
                 y = Latitud.del.centroide, 
                 size = porcentaje_calle, 
                 color = porcentaje_calle)) +
  geom_text(data = provincias_ranking,
            aes(x = Longitud.del.centroide,
                y = Latitud.del.centroide,
                label = round(porcentaje_calle, 3)),
            size = 3, vjust = -1.5) +
  labs(title = "Porcentaje de población en situación de calle por provincia",
       x = "Longitud",
       y = "Latitud",
       size = "Porcentaje (%)",
       color = "Porcentaje (%)")




