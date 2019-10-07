setwd("C:/Users/Usuario Dell/Desktop/S8/Data Science/Analysis-Lacansco")
library("readxl")
library("readxl")
library("dplyr")
library(dplyr)
library(tidyr)
library(cluster) #Para calcular la silueta
library(e1071)#para cmeans
library(mclust) #mixtures of gaussians
library(fpc) #para hacer el plotcluster
library(NbClust) #Para determinar el nï¿½mero de clusters ï¿½ptimo
library(factoextra) #Para hacer grï¿½ficos bonitos de clustering
library(ggplot2)
#arboles
library(rpart)
library(caret)
library(rpart.plot)
library(randomForest)
library(e1071)
library(caret)
library(fastDummies)
library(rpart)
library(rpart.plot)
# series de tiempo
library(ggfortify)
library(corrplot)
library(forecast)
library(tseries)


historia <- read_excel("Catálogo Guatemala 2018-2019.xlsx", sheet = 1)
Paginacion20152019 <- read_excel("Paginacion 2015-2019.xlsx", sheet = 1)
UnidadesPorSector <- read_excel("UnidadesPorSectorNew.xlsx", sheet = 1)

# Catalogo Guatemala 2018-2019

#Limpieza
colnames(historia)
names(historia)[names(historia) == "Pagina..7"] <- "Pagina"
names(historia)[names(historia) == "Pagina..22"] <- "Tipo pagina"
names(historia)[names(historia) == "%..24"] <- "Porcentaje precio"
names(historia)[names(historia) == "%..26"] <- "Porcentaje comision"

# Todo en mayuscula
historia <- data.frame(lapply(historia, function(v) {
  if (is.character(v)) return(toupper(v))
  else return(v)
}))

# Separar ano y mes 
historia = separate(data =  historia, 
                              col  =  Año.Mes,  
                              into =  c("Año", "Mes"), 
                              sep  =  4, remove = TRUE,
                              convert = TRUE )

# quitar tildes
historia$Descripcion <- chartr('ÁÉÍÓÚÑ','AEIOUN', historia$Descripcion)

#Quitar nulls de precios
historia$Precio.Catalogo <- sub("NULL", NA, historia$Precio.Catalogo)
historia <- historia[complete.cases(historia$Precio.Catalogo),]

historia$Precio.Vta.s.iva <- sub("NULL", NA, historia$Precio.Vta.s.iva)
historia <- historia[complete.cases(historia$Precio.Vta.s.iva),]

historia$Costo <- sub("NULL", NA, historia$Costo)
historia <- historia[complete.cases(historia$Costo),]

head(sort(table(historia$Unidades.Vendidas) , decreasing = TRUE), n = 10)

#*******************************************************************************************
# Paginacion 

#Limpieza
# separar año y mes
Paginacion20152019 = separate(data =  Paginacion20152019, 
                              col  =  `Año Mes`,  
                              into =  c("Año", "Mes"), 
                              sep  =  4, remove = TRUE,
                              convert = TRUE )


# quitar tildes
Paginacion20152019$Descripcion <- chartr('ÁÉÍÓÚÑ','AEIOUN', Paginacion20152019$Descripcion)

Paginacion20152019 <- Paginacion20152019[complete.cases(Paginacion20152019$Pagina),]

#Quitar espacios al inicio y fin
Paginacion20152019$Pagina = gsub("^ ", "", Paginacion20152019$Pagina)
Paginacion20152019$Pagina = gsub(" $", "", Paginacion20152019$Pagina)

#Quitar nulls de precios
Paginacion20152019$`Precio Catalogo` <- sub("NULL", NA, Paginacion20152019$`Precio Catalogo`)
Paginacion20152019 <- Paginacion20152019[complete.cases(Paginacion20152019$`Precio Catalogo`),]

Paginacion20152019$`Precio Vta s/iva` <- sub("NULL", NA, Paginacion20152019$`Precio Vta s/iva`)
Paginacion20152019 <- Paginacion20152019[complete.cases(Paginacion20152019$`Precio Vta s/iva`),]

Paginacion20152019$Costo <- sub("NULL", NA, Paginacion20152019$Costo)
Paginacion20152019 <- Paginacion20152019[complete.cases(Paginacion20152019$Costo),]


# separar producto y descripcion 
UnidadesPorSector = separate(data =  UnidadesPorSector, 
                              col  =  `Producto - Descripcion`,  
                              into =  c("Producto", "Descripcion"), 
                              sep  =  10, remove = TRUE,
                              convert = TRUE )


tablaproductos <- sort(table(UnidadesPorSector$Descripcion) , decreasing = TRUE)
productos <- names(tablaproductos)

tablaproductoshistoria <- sort(table(UnidadesPorSector$Descripcion) , decreasing = TRUE)
productoshistoria <- names(tablaproductoshistoria)






porcentaje<-0.7
set.seed(123)
corte <- sample(nrow(historia),nrow(historia)*porcentaje)
train<-historia[corte,]
test<-historia[-corte,]


dt_model<-rpart(Energy.Chart~Unidades.Vendidas,data=train,method = "class")
rpart.plot(dt_model)
