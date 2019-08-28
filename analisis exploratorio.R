#setwd("C:/Users/Cristian/Documents/uvg2019 2do semestre/data science/proyectos/proyecto2/data/Guatemala")
library("readxl")

historia <- read_excel("Catalogo Guatemala 2018-2019.xlsx", sheet = 1)

UnidadesPorSector <- read_excel("UnidadesPorSectorNew.xlsx", sheet = 1)

Paginacion20152019 <- read_excel("Paginacion2015-2019.xlsx", sheet = 1)


colnames(historia)
names(historia)[names(historia) == "Pagina..7"] <- "Pagina"
names(historia)[names(historia) == "Pagina..22"] <- "Tipo pagina"
names(historia)[names(historia) == "%..24"] <- "Porcentaje precio"
names(historia)[names(historia) == "%..26"] <- "Porcentaje comision"

clean_historia <- historia[1:31]
clean_historia <- data.frame(lapply(clean_historia, function(v) {
  if (is.character(v)) return(toupper(v))
  else return(v)
}))

##barplots unidades por sector


ggplot(data = UnidadesPorSector)+
  xlab("Cantidad por categoria")+
  geom_bar(mapping = aes(x =`Descripcion Categoria`))+ theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(data = UnidadesPorSector)+
  xlab("Cantidad por sector")+
  geom_bar(mapping = aes(x =Sector))+ theme(axis.text.x = element_text(angle = 45, hjust = 1))


 

##barplots para clean historia


  ggplot(data = clean_historia)+
    xlab("Cantidad por categoria")+
    geom_bar(mapping = aes(x = ...6))+ theme(axis.text.x = element_text(angle = 45, hjust = 1))

  
  ggplot(data = clean_historia)+
    xlab("Cantidad por linea")+
    geom_bar(mapping = aes(x = ...8))+ theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggplot(data = clean_historia)+
    xlab("Cantidad por canal de venta")+
    geom_bar(mapping = aes(x =CAMPOS.A.LLENAR.POR.PAIS.Y.PERIODO ))+ theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggplot(data = clean_historia)+
    xlab("Cantidad por contingencia")+
    geom_bar(mapping = aes(x = ...21))+ theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggplot(data = clean_historia)+
    xlab("Cantidad por pagina")+
    geom_bar(mapping = aes(x = ...22))+ theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggplot(data = clean_historia)+
    xlab("Cantidad por tipo de precio")+
    geom_bar(mapping = aes(x = ...23))+ theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  
  ggplot(data = clean_historia)+
    xlab("Cantidad por tipo comision")+
    geom_bar(mapping = aes(x = ...25))+ theme(axis.text.x = element_text(angle = 45, hjust = 1))

  
  clean_historia$...27[clean_historia$...27 == "NO APLICA"] <- NA
  
  ggplot(data = clean_historia)+
    xlab("Cantidad por atributo neto")+
    geom_bar(mapping = aes(x = ...27))+ theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggplot(data = clean_historia)+
    xlab("Cantidad por Energy chart")+
    geom_bar(mapping = aes(x = ...28))+ theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggplot(data = clean_historia)+
    xlab("Cantidad por Promociones")+
    geom_bar(mapping = aes(x = ...29))+ theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggplot(data = clean_historia)+ 
    xlab("Cantidad por Recursos especial")+
    geom_bar(mapping = aes(x = ...30))+ theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
 
    
#setwd("C:/Users/Cristian/Documents/uvg2019 2do semestre/data science/proyectos/proyecto2/data/Guatemala")
library("readxl")
library("dplyr")


clean_historia[is.na(clean_historia$`Precio Catalogo`),] <- 0
checkformin_precio <- clean_historia[clean_historia$`Precio Catalogo` !=0,] 
#solo par ver precio catalogo lo demas no esta limpio
summary(checkformin_precio)
qqnorm(checkformin_precio$`Precio Catalogo`, main = "Precio catalogo", col = 2)
qqline(checkformin_precio$`Precio Catalogo`, col = 3)


#limpieza precios venta sin iva 
clean_historia[is.na(clean_historia$`Precio Vta s/iva`),] <- 0
checkformin_venta_sin_iva <- clean_historia[clean_historia$`Precio Vta s/iva` !=0,] 
summary(checkformin_venta_sin_iva)
qqnorm(checkformin_precio$`Precio Vta s/iva`, main = "Precio venta sin iva", col = 2)
qqline(checkformin_precio$`Precio Vta s/iva`, col = 3)
#pronostico
qqnorm(clean_historia$Pronostico, main = "Pronostico", col = 2)
qqline(clean_historia$Pronostico, col = 3)
#unidades vendidas
summary(clean_historia$`Unidades Vendidas`)
qqnorm(clean_historia$`Unidades Vendidas`, main = "Unidades vendidas", col = 2)
qqline(clean_historia$`Unidades Vendidas`, col = 3)
#venta neta sin iva
clean_historia[is.na(clean_historia$`Venta Neta s/iva`),] <- 0
checkformin_ventaneta_sin_iva <- clean_historia[clean_historia$`Venta Neta s/iva` !=0,] 
summary(checkformin_venta_sin_iva)
qqnorm(checkformin_precio$`Venta Neta s/iva`, main = "venta sin iva", col = 2)
qqline(checkformin_precio$`Venta Neta s/iva`, col = 3)
#costo
checkformin_costo <- clean_historia$Costo %>% replace(.=="NULL", NA)
checkformin_costo[is.na(checkformin_costo)] <- 0
checkformin_costo <- checkformin_costo[checkformin_costo !=0]
#todavia son strings
summary(checkformin_costo)
checkformin_costo <- as.numeric(checkformin_costo)
summary(checkformin_costo)
qqnorm(checkformin_costo, main = "Costo", col = 2)
qqline(checkformin_costo, col = 3)
#utilidad
summary(clean_historia$Utilidad)
qqnorm(clean_historia$Utilidad ,main = "Utilidad", col = 2)
qqline(clean_historia$Utilidad, col = 3)
#margen
summary(clean_historia$Margen)
qqnorm(clean_historia$Margen ,main = "Margen", col = 2)
qqline(clean_historia$Margen, col = 3)
#ratio
summary(clean_historia$Ratio)
qqnorm(clean_historia$Ratio ,main = "Ratio", col = 2)
qqline(clean_historia$Ratio, col = 3)
#diagrama de caja y bigotes
z <- UnidadesPorSector[,4]
boxplot(z)
boxplot(z, horizontal = TRUE, ylim = c(0, 11))
boxplot(z, horizontal = TRUE, ylim = c(0, 50))
boxplot(z, horizontal = TRUE, ylim = c(0, 100))
z <- UnidadesPorSector[,5]
boxplot(z, horizontal = TRUE, ylim = c(0, 100))
boxplot(z, horizontal = TRUE, ylim = c(0, 500))
boxplot(z)
boxplot(z, horizontal = TRUE, ylim = c(-10, 10000))
z <- UnidadesPorSector[,6]
boxplot(z, horizontal = TRUE, ylim = c(-10, 10000))
UnidadesPorSector <- read_excel("UnidadesPorSectorNew.xlsx", sheet = 1)
boxplot(z, horizontal = TRUE, ylim = c(-10, 10000))
View(UnidadesPorSector)
z <- UnidadesPorSector[,7]
boxplot(z, horizontal = TRUE, ylim = c(-10, 10000))
z <- UnidadesPorSector[,8]
boxplot(z, horizontal = TRUE, ylim = c(-10, 10000))
z <- UnidadesPorSector[,9]
boxplot(z, horizontal = TRUE, ylim = c(-10, 8000))
boxplot(z, horizontal = TRUE, ylim = c(0, 8000))
z <- UnidadesPorSector[,10]
boxplot(z, horizontal = TRUE, ylim = c(0, 8000))
z <- UnidadesPorSector[,11]
boxplot(z, horizontal = TRUE, ylim = c(0, 8000))
z <- UnidadesPorSector[,12]
boxplot(z, horizontal = TRUE, ylim = c(0, 8000))
z <- UnidadesPorSector[,13]
boxplot(z, horizontal = TRUE, ylim = c(0, 8000))
z <- UnidadesPorSector[,14]
boxplot(z, horizontal = TRUE, ylim = c(0, 8000))
z <- UnidadesPorSector[,15]
boxplot(z, horizontal = TRUE, ylim = c(0, 8000))
z <- UnidadesPorSector[,16]
boxplot(z, horizontal = TRUE, ylim = c(0, 8000))
z <- UnidadesPorSector[,17]
boxplot(z, horizontal = TRUE, ylim = c(0, 8000))
z <- UnidadesPorSector[,18]
boxplot(z, horizontal = TRUE, ylim = c(0, 8000))
z <- UnidadesPorSector[,19]
boxplot(z, horizontal = TRUE, ylim = c(0, 8000))
z <- UnidadesPorSector[,19]
boxplot(z, horizontal = TRUE, ylim = c(0, 8000))
z <- UnidadesPorSector[,20]
boxplot(z, horizontal = TRUE, ylim = c(0, 8000))
z <- UnidadesPorSector[,21]
boxplot(z, horizontal = TRUE, ylim = c(0, 8000))
z <- UnidadesPorSector[,22]
boxplot(z, horizontal = TRUE, ylim = c(0, 8000))
z <- UnidadesPorSector[,22]
z <- UnidadesPorSector[,23]
boxplot(z, horizontal = TRUE, ylim = c(0, 8000))
z <- UnidadesPorSector[,24]
boxplot(z, horizontal = TRUE, ylim = c(0, 1000))
z <- UnidadesPorSector[,25]
boxplot(z, horizontal = TRUE, ylim = c(0, 1000))
 z <- Paginacion20152019[,3]
View(Paginacion20152019)
boxplot(z, horizontal = TRUE, ylim = c(0, 1000))
z <- Paginacion20152019[,0]
boxplot(z, horizontal = TRUE, ylim = c(0, 1000))
z <- Paginacion20152019[,1]
z <- Paginacion20152019[,6]
boxplot(z, horizontal = TRUE, ylim = c(0, 1000))
boxplot(z, horizontal = TRUE, ylim = c(0, 300))
z <- Paginacion20152019[,8]
z <- Paginacion20152019[,9]
z <- Paginacion20152019[,10]
boxplot(z, horizontal = TRUE, ylim = c(0, 4000))
z <- Paginacion20152019[,11]
boxplot(z, horizontal = TRUE, ylim = c(0, 4000))
z <- Paginacion20152019[,12]
boxplot(z, horizontal = TRUE, ylim = c(0, 100000))
z <- Paginacion20152019[,14]
boxplot(z, horizontal = TRUE, ylim = c(0, 100000))
z <- Paginacion20152019[,15]
boxplot(z, horizontal = TRUE, ylim = c(0, 100000))
boxplot(z, horizontal = TRUE, ylim = c(0, 5000))
boxplot(z, horizontal = TRUE, ylim = c(0, 500))
boxplot(z, horizontal = TRUE, ylim = c(0, 100))
boxplot(z, horizontal = TRUE, ylim = c(0, 50))
boxplot(z, horizontal = TRUE, ylim = c(0, 20))
boxplot(z, horizontal = TRUE, ylim = c(0, 20))
z <- Paginacion20152019[,16]
boxplot(z, horizontal = TRUE, ylim = c(0, 20))
boxplot(z, horizontal = TRUE, ylim = c(0, 10))
boxplot(z, horizontal = TRUE, ylim = c(0, 1))
z <- Paginacion20152019[,17]
boxplot(z, horizontal = TRUE, ylim = c(0, 1))
