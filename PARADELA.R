x <- matrix (data= c(1,2,3,4), nrow=2, ncol=2)
x <- matrix(c(7,8,9,10),2,2)
y <- c(5,6)
z <- rbind(x,y)
w<- cbind(x,y)

data ("AirPassengers")
head (AirPassengers, n=3)

library("readr")
getwd()
write_csv(iris, path = "C:/Users/usuario/Documents/Paradela/iris.csv")
iris_importado_csv <- read_csv("./iris.csv")
col_names = FALSE
url<- "https://raw.githubusercontent.com/rominicky/materiales/main/assets/Provincias.csv"

install.packages("tidyverse")
install.packages("devtools")
devtools::install_github("r-lib/conflicted")
library(conflicted)
library(dplyr)
provincias <- read.csv('C:/git/materiales/assets/Provincias.csv')
"https://raw.githubusercontent.com/rominicky/materiales/main/assets/Provincias.csv"
destino <- "./provincias.csv"
download.file(url, destino)
getwd()
"C:/Users/usuario/Documents/Paradela"
provincias <- read.csv("C:/Users/usuario/Documents/Paradela/Provincias.csv")
View(provincias)
library(tidyverse)
provincias <- provincias %>%
  rename(situacion_calle = Personas.en.situación.de.calle..vía.pública.)
media_Personas.en.situación.de.calle..vía.pública. <- provincias %>%
  group_by(Nombre.de.provincia) %>%
  summarise(media_Personas.en.situación.de.calle..vía.pública. = mean(Personas.en.situación.de.calle..vía.pública., na.rm = TRUE))
ggplot(provincias, aes(x = reorder(Nombre.de.provincia, Personas.en.situación.de.calle..vía.pública.), y = Personas.en.situación.de.calle..vía.pública.)) +
  + geom_bar(stat = "identity", na.rm = TRUE) +
  + coord_flip() +
  + labs(title = "Personas en situaciones de calle por Provincias argentinas",
         + x = "Provincias",
         + y = "Cantidad de personas en situación de calle") +
  + theme_light()
ggplot(subset(media_Personas.en.situación.de.calle..vía.pública., media_Personas.en.situación.de.calle..vía.pública. > 0),
       ggplot(subset(media_Personas.en.situación.de.calle..vía.pública., media_Personas.en.situación.de.calle..vía.pública. > 0),
              + aes(x = reorder(Nombre.de.provincia, Personas.en.situación.de.calle..vía.pública.), y = media_Personas.en.situación.de.calle..vía.pública., fill = media_Personas.en.situación.de.calle..vía.pública.)) +
         + geom_bar(stat = "identity") +
         + labs(title = "Valor medio de personas en situación de calle por provincia",
                + x = "Provincia",
                + y = "Valor medio de personas en situación de calle") +
         + theme_minimal() +
         + theme(axis.text.x = element_text(angle = 45, hjust = 1),
                 + legend.position = "none")

INVENTARIO_MUSEO <- read_excel("Paradela/INVENTARIO MUSEO.xlsx")
View(INVENTARIO_MUSEO)
library(ggplot2)
INVENTARIO_MUSEO$`TIPO DE MATERIAL` <- as.factor(INVENTARIO_MUSEO$`TIPO DE MATERIAL`)
ggplot(INVENTARIO_MUSEO, aes(x = `TIPO DE MATERIAL`)) +
  + geom_text(stat = 'count', aes(label = after_stat(count)), vjust = -0.5, size = 4) +
  +     labs(
    + title = "Frecuencia de Tipos de Material en el Inventario del Museo",
    + x = "Tipo de Material",
    + y = "Frecuencia (Número de Objetos)"
    + )+
  + theme_minimal()

INVENTARIO_MUSEO$`TIPO DE MATERIAL` <- as.factor(INVENTARIO_MUSEO$`TIPO DE MATERIAL`
INVENTARIO_MUSEO$`Estado Estructural` <- as.factor(INVENTARIO_MUSEO$`Estado Estructural`)
ggplot(INVENTARIO_MUSEO, aes(x = `TIPO DE MATERIAL`, fill = `Estado estructural`)) +
  geom_bar(position = "fill", color = "black") +
