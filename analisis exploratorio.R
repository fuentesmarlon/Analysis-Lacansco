setwd("C:/Users/Cristian/Documents/uvg2019 2do semestre/data science/proyectos/proyecto2/data/Guatemala")
library("readxl")
library("xlsx")
library("tidyverse")
historia <- read_excel("Catálogo Guatemala 2018-2019.xlsx", sheet = 1)

colnames(historia)
names(historia)[names(historia) == "Pagina..7"] <- "Pagina"
names(historia)[names(historia) == "Pagina..22"] <- "Tipo pagina"
names(historia)[names(historia) == "%..24"] <- "Porcentaje precio"
names(historia)[names(historia) == "%..26"] <- "Porcentaje comision"

cleaned_historia <- historia[1:31]
cleaned_historia <- cleaned_historia[c(-cleaned_historia$`%..24`,-cleaned_historia$`%..26`)]
