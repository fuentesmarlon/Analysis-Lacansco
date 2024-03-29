setwd("C:/Users/Cristian/Documents/uvg2019 2do semestre/data science/proyectos/proyecto2/data/")
setwd("C:/Users/jazmi/OneDrive/Documentos/GitHub/Analysis-Lacansco")
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
library(stringr)
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
datos$Descripcion[is.na(datos$Descripcion)] <- 0
sum(datos$Unidades.Vendidas[datos$Descripcion == "TOP SECRET ROLL-ON BABY POWDER 80G"], na.rm = T)

Paginacion20152019 = separate(data =  Paginacion20152019, 
                              col  =  `Año Mes`,  
                              into =  c("Año", "Mes"), 
                              sep  =  4, remove = TRUE,
                              convert = TRUE )

#Limpieza de paginacion20152019 hoja 1
# quitar tildes y comas
Paginacion20152019$Descripcion <- chartr('Ã¡Ã©Ã­Ã³ÃºÃ±Ã‘','aeiounN', Paginacion20152019$Descripcion)
Paginacion20152019$Descripcion = gsub("[[:punct:]]", "", Paginacion20152019$Descripcion)
pagina_sin_na <- Paginacion20152019[complete.cases(Paginacion20152019$Pagina),]

#Qitar espacios al inicio y fin
Paginacion20152019$Pagina = gsub("^ ", "", Paginacion20152019$Pagina)
Paginacion20152019$Pagina = gsub(" $", "", Paginacion20152019$Pagina)

#Quitar nulls de precios
Paginacion20152019$`Precio Catalogo` <- sub("NULL", NA, Paginacion20152019$`Precio Catalogo`)
precio_catalogo_sin_na <- Paginacion20152019[complete.cases(Paginacion20152019$`Precio Catalogo`),]

Paginacion20152019$`Precio Vta s/iva` <- sub("NULL", NA, Paginacion20152019$`Precio Vta s/iva`)
precio_vta_sin_na <- Paginacion20152019[complete.cases(Paginacion20152019$`Precio Vta s/iva`),]

Paginacion20152019$Costo <- sub("NULL", NA, Paginacion20152019$Costo)
costo_sin_na <- Paginacion20152019[complete.cases(Paginacion20152019$Costo),]

observaciones_sin_na <- Paginacion20152019[complete.cases(Paginacion20152019$Observaciones),]



##barplots unidades por sector
ggplot(data = clean_historia)+
  xlab("Cantidad por tipo de pagina")+
  ggtitle("Tipo de pagina")
  geom_bar(mapping = aes(x =Tipo.pagina))+ theme(axis.text.x = element_text(angle = 45, hjust = 1))



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
  
  ggplot(data = clean_historia[!is.na(clean_historia$Tipo.pagina),])+
    xlab("Cantidad por pagina")+
    ggtitle("Tipo Pagina")+
    geom_bar(mapping = aes(x = Tipo.pagina))+ theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggplot(data = clean_historia)+
    xlab("Cantidad por tipo de precio")+
    ggtitle("Tipo de Precio")
    geom_bar(mapping = aes(x = ...23))+ theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  #clean_historia$Tipo.Precio[na.omit(clean_historia$Tipo.Precio)]
  #clean_historia$Tipo.Precio[clean_historia$Tipo.Precio]
  #clean_historia$Tipo.Precio[str_remove_all(clean_historia$Tipo.Precio, "PRECIO NUNCA ANTES VISTO")]
  #clean_historia$Tipo.Precio[str_remove_all(clean_historia$Tipo.Precio, "NA")]
  ggplot(data = clean_historia[clean_historia$Tipo.Precio!="PRECIO NUNCA ANTES VISTO",])+
    xlab("Cantidad por tipo precio")+
    ggtitle("Tipo de Precio")+
    geom_bar(mapping = aes(x = Tipo.Precio))+ theme(axis.text.x = element_text(angle = 45, hjust = 1))

  #filtrar na
  Filter(function(x) !all(is.na(x)), clean_historia)
  
  clean_historia$...27[clean_historia$...27 == "NO APLICA"] <- NA
  
  ggplot(data = clean_historia)+
    xlab("Cantidad por atributo neto")+
    geom_bar(mapping = aes(x = ...27))+ theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  clean_historia <- clean_historia[!is.na(clean_historia$Energy.Chart),]
  clean_historia <- clean_historia[clean_historia$Energy.Chart !="OFERTA ESPECIAL",]
  
  ggplot(data = clean_historia)+
    xlab("Cantidad por Energy chart")+
    ggtitle("Energy chart")+
    geom_bar(mapping = aes(x = Energy.Chart))+ theme(axis.text.x = element_text(angle = 45, hjust = 1))
  #remover na
  clean_historia <- clean_historia[!is.na(clean_historia$Promociones),]
  clean_historia <- clean_historia[clean_historia$Promociones !="1X 3X",]
  clean_historia <- clean_historia[clean_historia$Promociones !="1X 4X",]
  clean_historia <- clean_historia[clean_historia$Promociones !="2 X 2 X",]
  
  ggplot(data = clean_historia)+
    xlab("Cantidad por Promociones")+
    ggtitle("Promociones")+
    geom_bar(mapping = aes(x = Promociones))+ theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggplot(data = clean_historia)+ 
    xlab("Cantidad por Recursos especial")+
    geom_bar(mapping = aes(x = ...30))+ theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
 
    
#setwd("C:/Users/Cristian/Documents/uvg2019 2do semestre/data science/proyectos/proyecto2/data/Guatemala")



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

#k-medias
datos<-historia
historiaCompleto<-historia[complete.cases(historia),]
km<-kmeans(historia[,1:29],3)
datos$grupo<-km$cluster

g1<- datos[datos$grupo==1,]
prop.table(table(g1$Species))*100
nrow(g1)
summary(g1)

g2<- datos[datos$grupo==2,]
prop.table(table(g2$Species))*100
g3<- datos[datos$grupo==3,]
prop.table(table(g3$Species))*100

plotcluster(historia[,1:29],km$cluster) #grafica la ubicaciÃ³n de los clusters

#Clustering jerÃ¡rquico
hc<-hclust(dist(historia[,1:29])) #Genera el clustering jerÃ¡rquico de los datos
plot(hc) #Genera el dendograma
rect.hclust(hc,k=3) #Dibuja el corte de los grupos en el grÃ¡fico
groups<-cutree(hc,k=3) #corta el dendograma, determinando el grupo de cada fila
datos$gruposHC<-groups


g1HC<-datos[datos$gruposHC==1,]
g2HC<-datos[datos$gruposHC==2,]
g3HC<-datos[datos$gruposHC==3,]

#Fuzzy C-Means
fcm<-cmeans(historia[,1:29],3)
datos$FCGrupos<-fcm$cluster
datos<-cbind(datos,fcm$membership)

#Mixture of gaussians
mc<-Mclust(historia[,1:29],3)
plot(mc, what = "classification", main="MClust Classification")
datos$mxGau<-mc$classification
g1MC<-datos[datos$mxGau==1,]
g2MC<-datos[datos$mxGau==2,]
g3MC<-datos[datos$mxGau==3,]

#MÃ©todo de la silueta para las k-medias
silkm<-silhouette(km$cluster,dist(historia[,1:29]))
mean(silkm[,3]) #0.55, no es la mejor particiÃ³n pero no estÃ¡ mal

#MÃ©todo de la silueta para clustering jerÃ¡rquico
silch<-silhouette(groups,dist(historia[,1:29]))
mean(silch[,3]) #0.51, no es la mejor particiÃ³n pero no estÃ¡ mal

#MÃ©todo de la silueta para fuzzy cmeans
silfcm<-silhouette(fcm$cluster,dist(historia[,1:29]))
mean(silfcm[,3]) #0.54, no es la mejor particiÃ³n pero no estÃ¡ mal

#MÃ©todo de la silueta para mixture of gaussians
silmg<-silhouette(mc$classification,dist(historia[,1:29]))
mean(silmg[,3]) #0.50, no es la mejor particiÃ³n pero no estÃ¡ mal

#MÃ©todo de Ward para determinar el nÃºmero correcto de clusteres con k-medias
#Para saber cual es el mejor numero de clusters
wss <- (nrow(historia[,1:29])-1)*sum(apply(historia[,1:29],2,var))

for (i in 2:10) 
  wss[i] <- sum(kmeans(historia[,1:29], centers=i)$withinss)

plot(1:10, wss, type="b", xlab="Number of Clusters",  ylab="Within groups sum of squares")

#Paquete para saber el mejor nï¿½mero de clusters
nb <- NbClust(historia[,1:29], distance = "euclidean", min.nc = 2,
              max.nc = 10, method = "complete", index ="all")

#Visualizaciï¿½n de los clusters con factoextra
#Visualizaciï¿½n de las k-medias
fviz_cluster(km, data = historia[,1:29],geom = "point", ellipse.type = "norm")

#Visualizaciï¿½n de cluster jerï¿½rquico
hc.cut<-hcut(historia[,1:29], k=3, hc_method = "complete")
fviz_dend(hc.cut, show_labels = FALSE, rect = TRUE)
fviz_cluster(hc.cut, ellipse.type = "convex")

#cluster 2

#k-medias
datos<-Paginacion20152019
Paginacion20152019Completo<-Paginacion20152019[complete.cases(Paginacion20152019),]
km<-kmeans(Paginacion20152019[,1:17],3)
datos$grupo<-km$cluster

g1<- datos[datos$grupo==1,]
prop.table(table(g1$Species))*100
nrow(g1)


g2<- datos[datos$grupo==2,]
prop.table(table(g2$Species))*100
g3<- datos[datos$grupo==3,]
prop.table(table(g3$Species))*100

plotcluster(Paginacion20152019[,1:17],km$cluster) #grafica la ubicaciÃ³n de los clusters

#Clustering jerÃ¡rquico
hc<-hclust(dist(Paginacion20152019[,1:17])) #Genera el clustering jerÃ¡rquico de los datos
plot(hc) #Genera el dendograma
rect.hclust(hc,k=3) #Dibuja el corte de los grupos en el grÃ¡fico
groups<-cutree(hc,k=3) #corta el dendograma, determinando el grupo de cada fila
datos$gruposHC<-groups


g1HC<-datos[datos$gruposHC==1,]
g2HC<-datos[datos$gruposHC==2,]
g3HC<-datos[datos$gruposHC==3,]

#Fuzzy C-Means
fcm<-cmeans(Paginacion20152019[,1:17],3)
datos$FCGrupos<-fcm$cluster
datos<-cbind(datos,fcm$membership)

#Mixture of gaussians
mc<-Mclust(Paginacion20152019[,1:17],3)
plot(mc, what = "classification", main="MClust Classification")
datos$mxGau<-mc$classification
g1MC<-datos[datos$mxGau==1,]
g2MC<-datos[datos$mxGau==2,]
g3MC<-datos[datos$mxGau==3,]

#MÃ©todo de la silueta para las k-medias
silkm<-silhouette(km$cluster,dist(Paginacion20152019[,1:17]))
mean(silkm[,3]) #0.55, no es la mejor particiÃ³n pero no estÃ¡ mal

#MÃ©todo de la silueta para clustering jerÃ¡rquico
silch<-silhouette(groups,dist(Paginacion20152019[,1:17]))
mean(silch[,3]) #0.51, no es la mejor particiÃ³n pero no estÃ¡ mal

#MÃ©todo de la silueta para fuzzy cmeans
silfcm<-silhouette(fcm$cluster,dist(Paginacion20152019[,1:17]))
mean(silfcm[,3]) #0.54, no es la mejor particiÃ³n pero no estÃ¡ mal

#MÃ©todo de la silueta para mixture of gaussians
silmg<-silhouette(mc$classification,dist(Paginacion20152019[,1:17]))
mean(silmg[,3]) #0.50, no es la mejor particiÃ³n pero no estÃ¡ mal

#MÃ©todo de Ward para determinar el nÃºmero correcto de clusteres con k-medias
#Para saber cual es el mejor numero de clusters
wss <- (nrow(Paginacion20152019[,1:17])-1)*sum(apply(Paginacion20152019[,1:17],2,var))

for (i in 2:10) 
  wss[i] <- sum(kmeans(Paginacion20152019[,1:17], centers=i)$withinss)

plot(1:10, wss, type="b", xlab="Number of Clusters",  ylab="Within groups sum of squares")

#Paquete para saber el mejor nï¿½mero de clusters
nb <- NbClust(Paginacion20152019[,1:17], distance = "euclidean", min.nc = 2,
              max.nc = 10, method = "complete", index ="all")

#Visualizaciï¿½n de los clusters con factoextra
#Visualizaciï¿½n de las k-medias
fviz_cluster(km, data = Paginacion20152019[,1:17],geom = "point", ellipse.type = "norm")

#Visualizaciï¿½n de cluster jerï¿½rquico
hc.cut<-hcut(Paginacion20152019[,1:17], k=3, hc_method = "complete")
fviz_dend(hc.cut, show_labels = FALSE, rect = TRUE)
fviz_cluster(hc.cut, ellipse.type = "convex")

#PCA
historia_centrada <- clean_historia[,c(9,10,11,12,13,15,17)]
historia_centrada$`Precio Catalogo`<- historia_centrada$`Precio Catalogo` - mean(historia_centrada$`Precio Catalogo`)
historia_centrada$`Precio Vta s/iva` <- historia_centrada$`Precio Vta s/iva` - mean(historia_centrada$`Precio Vta s/iva`)
historia_centrada$Pronostico <- historia_centrada$Pronostico - mean(historia_centrada$Pronostico)
historia_centrada$`Unidades Vendidas` <- historia_centrada$`Unidades Vendidas` - mean(historia_centrada$`Unidades Vendidas`)
historia_centrada$`Venta Neta s/iva`<- historia_centrada$`Venta Neta s/iva` - mean(historia_centrada$`Venta Neta s/iva`)
#limpieza costo
historia_centrada$Costo <- historia_centrada$Costo %>% replace(.=="NULL", NA)
historia_centrada$Costo[is.na(historia_centrada$Costo)] <- 0

#historia_centrada$Costo <- historia_centrada$Costo - mean(historia_centrada$Costo)
historia_centrada$Utilidad <- historia_centrada$Utilidad - mean(historia_centrada$Utilidad)
historia_centrada$`Pedido Real` <- historia_centrada$`Pedido Real` - mean(historia_centrada$`Pedido Real`)
matriz_cov <- cov(historia_centrada)
matriz_cov

eigenvalores <- eigen(matriz_cov)
eigenvalores$values
eigenvalores$vectors

t_eigenvectores <- t(eigenvalores)
t_eigenvectores

t_datos_centrados <- t(historia_centrada)
t_datos_centrados
pc_scores <- t_eigenvectores %*% t_datos_centrados
rownames(pc_scores) <- c("PC1", "PC2")
t(pc_scores)




##PRUEBAS PCA
library(tidyverse)

historia_centrada<-historia_centrada %>% 
  rename(
    Precio_Catalogo = ...9,
    Precio_Venta_sin_iva = ...10,
    Pronostico = ...11,
    Unidades_Vendidas = ...12,
    Venta_neta_sin_iva = ...13,
    Utilidad = ...15,
    PrecioReal = ...17
  )

historia_centrada <-historia_centrada[-1,]
indx <- sapply(historia_centrada, is.factor)
historia_centrada[indx] <- lapply(historia_centrada[indx], function(x) as.numeric(as.character(x)))

View(historia_centrada)
class(historia_centrada$Precio_Catalogo)
complete.cases(historia_centrada)
historia_centrada<-historia_centrada[complete.cases(historia_centrada), ]
historia_centrada$Precio_Catalogo <- historia_centrada$Precio_Catalogo - mean(historia_centrada$Precio_Catalogo )
historia_centrada$Precio_Venta_sin_iva <- historia_centrada$Precio_Venta_sin_iva- mean(historia_centrada$Precio_Venta_sin_iva)
historia_centrada$Pronostico <- historia_centrada$Pronostico- mean(historia_centrada$Pronostico)
historia_centrada$Unidades_Vendidas<- historia_centrada$Unidades_Vendidas - mean(historia_centrada$Unidades_Vendidas)
historia_centrada$Venta_neta_sin_iva<-historia_centrada$Venta_neta_sin_iva  - mean(historia_centrada$Venta_neta_sin_iva)
historia_centrada$Utilidad<-  historia_centrada$Utilidad- mean(historia_centrada$Utilidad)
historia_centrada$PrecioReal<- historia_centrada$PrecioReal- mean(historia_centrada$PrecioReal)
matriz_cov <- cov(historia_centrada)
matriz_cov
eigen <- eigen(matriz_cov)
eigen$values
eigen$vectors
t_eigenvectors <- t(eigen$vectors)
t_eigenvectors
t_historia_centrada <- t(historia_centrada)
t_historia_centrada
# Producto matricial
pc_scores <- t_eigenvectors %*% t_historia_centrada
rownames(pc_scores) <- c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7")

# Se vuelve a transponer para que los datos estén en modo tabla
t(pc_scores)

datos_recuperados <- t(eigen$vectors %*% pc_scores)
datos_recuperados[, 1] <- datos_recuperados[, 1] + mean(historia_centrada$Precio_Catalogo)
datos_recuperados[, 2] <- datos_recuperados[, 2] + mean(historia_centrada$Precio_Venta_sin_iva)
datos_recuperados[, 3] <- datos_recuperados[, 3] + mean(historia_centrada$Pronostico)
datos_recuperados[, 4] <- datos_recuperados[, 4] + mean(historia_centrada$Unidades_Vendidas)
datos_recuperados[, 5] <- datos_recuperados[, 5] + mean(historia_centrada$Venta_neta_sin_iva)
datos_recuperados[, 6] <- datos_recuperados[, 6] + mean(historia_centrada$Utilidad)
datos_recuperados[, 7] <- datos_recuperados[, 7] + mean(historia_centrada$PrecioReal)
datos_recuperados


datos_recuperados
historia_centrada


##mas 
apply(X = historia_centrada, MARGIN = 2, FUN = mean)
apply(X = historia_centrada, MARGIN = 2, FUN = var)
##generando pca
pca <- prcomp(historia_centrada, scale = TRUE)

names(pca)
pca$center
pca$scale
pca$rotation
head(pca$x)
dim(pca$x)
##graficando pca 
biplot(x = pca, scale = 0, cex = 0.6, col = c("blue4", "brown3"))
pca$rotation <- -pca$rotation
pca$x        <- -pca$x
biplot(x = pca, scale = 0, cex = 0.6, col = c("blue4", "brown3"))

library("factoextra")
fviz_eig(pca)
fviz_pca_ind(pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)


fviz_pca_var(pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

library(factoextra)
# Eigenvalues
eig.val <- get_eigenvalue(pca)
eig.val

# Results for Variables
res.var <- get_pca_var(pca)
res.var$coord          # Coordinates
res.var$contrib        # Contributions to the PCs
res.var$cos2           # Quality of representation 
# Results for individuals
res.ind <- get_pca_ind(pca)
res.ind$coord          # Coordinates
res.ind$contrib        # Contributions to the PCs
res.ind$cos2           # Quality of representation 

##link uti http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/118-principal-component-analysis-in-r-prcomp-vs-princomp/

##calculando y graficando propagacion de varianza
library(ggplot2)
pca$sdev^2
prop_varianza <- pca$sdev^2 / sum(pca$sdev^2)
prop_varianza

##graficando varianza que explica cada componente
ggplot(data = data.frame(prop_varianza, pc = 1:7),
       aes(x = pc, y = prop_varianza)) +
  geom_col(width = 0.3) +
  scale_y_continuous(limits = c(0,1)) +
  theme_bw() +
  labs(x = "Componente principal",
       y = "Prop. de varianza explicada")

##Calculando propagacion de varianza acumulada
prop_varianza_acum <- cumsum(prop_varianza)
prop_varianza_acum

##Grafica de propagacion de varianza acumulada
ggplot(data = data.frame(prop_varianza_acum, pc = 1:7),
       aes(x = pc, y = prop_varianza_acum, group = 1)) +
  geom_point() +
  geom_line() +  geom_label(aes(label = round(prop_varianza_acum,2))) +

  theme_bw() +
  labs(x = "Componente principal",
       y = "Prop. varianza explicada acumulada")




##Reglas de asociacion 
install.packages("arules")
 install.packages("arulesViz")
historia_limpianum <- clean_historia[,c(9,10,11,12,13,15,17)]
historia_limpianum<-historia_limpianum %>% 
  rename(
    Precio_Catalogo = ...9,
    Precio_Venta_sin_iva = ...10,
    Pronostico = ...11,
    Unidades_Vendidas = ...12,
    Venta_neta_sin_iva = ...13,
    Utilidad = ...15,
    PrecioReal = ...17
  )

historia_limpianum <-historia_limpianum[-1,]
historia_limpianum<-historia_limpianum[complete.cases(historia_limpianum), ]

historia_limpianum_rules <- apriori(historia_limpianum, parameter = list(support = 0.01, confidence = 0.5))
inspect(head(sort(historia_limpianum_rules, by = "confidence"), 3))


historia_limpianum_rules_increased_support <- apriori(historia_limpianum, parameter = list(support = 0.02, confidence = 0.5))
# This generates only one rule in the output.
inspect(head(sort(historia_limpianum_rules_increased_support, by = "confidence"), 3))

subsets <- which(colSums(is.subset(historia_limpianum_rules, historia_limpianum_rules)) > 2)
historia_limpianum_rules <- historia_limpianum_rules[-subsets]
inspect(head(sort(historia_limpianum_rules, by = "confidence"), 3))



historia_limpianum_rules_chi2 <- apriori(historia_limpianum, parameter = list(support = 0.01, confidence = 0.5,arem="chi2"))
inspect(head(sort(historia_limpianum_rules_chi2, by = "confidence"), 3))



##matriz de correlacion visual 
library(corrplot)

cor(historia_centrada)


##Correlation matrix 
prueba <- na.omit(historia_centrada)

prueba <-   cor(prueba,method  ="pearson")
prueba <-round (prueba, digits=2)
corrplot(prueba,tl.col = "black",tl.srt = 45)
corrplot(prueba, method ="shade",tl.col = "black",tl.srt = 45,order="AOE") ##ESTA ES LA QUE MEJOR SALE
col<- colorRampPalette(c("#BB4444","#EE9988","#FFFFFF","#77AADD","#4477AA"))
#series de tiempo en unidades vendidas
library(forecast)
library(tseries)
#Transformamos los datos en una serie temporal 
#prueba con codigo catalogo
co2ts<-ts(historia$`Codigo Catalogo`, start = c(1959,1), frequency = 12)
print(co2ts)
library(ggfortify)
autoplot(co2ts, ts.colour = "blue", ts.linetype = "dashed")
#venta neta iva
co2ts<-ts(historia$`Venta Neta s/iva`, start = c(1959,1), frequency = 12)
print(co2ts)
library(ggfortify)
autoplot(co2ts, ts.colour = "blue", ts.linetype = "dashed")
autoplot(acf(co2ts, plot = FALSE)
#unidades vendidas
co2ts<-ts(historia$`Unidades Vendidas`, start = c(1959,1), frequency = 12)
print(co2ts)
library(ggfortify)
autoplot(co2ts, ts.colour = "blue", ts.linetype = "dashed")
autoplot(acf(co2ts, plot = FALSE)
autoplot(stl(co2ts, s.window = "periodic"), ts.colour = "blue")nsdiffs(co2ts)
ndiffs(co2ts)
diff.co2ts<-autoplot(diff(co2ts), ts.linetype = "dashed", ts.colour = "darkmagenta")
diff.co2ts
autoplot(acf(diff(co2ts), plot = FALSE))
diffco2<-diff(co2ts)
boxplot(diffco2~cycle(diffco2))
diff.co2ts.12<-diff(co2ts, lag = 12)
autoplot(diff.co2ts.12, ts.colour = "darkorange4", ts.linetype = "dashed")
library(tseries)
adf<-adf.test(diff.co2ts.12)
adf$p.value
kpss<-kpss.test(diff.co2ts.12)
kpss$p.value
autoplot(acf(diff.co2ts.12, plot = FALSE))
library(forecast)
arima1<- Arima(co2ts, order=c(0,1,2), seasonal=list(order=c(0,1,1),period=12))
arima2<- Arima(co2ts, order=c(1,1,0), seasonal=list(order=c(2,1,0),period=12))
arima3<- Arima(co2ts, order=c(1,1,2), seasonal=list(order=c(2,1,1),period=12))
arima4<- Arima(co2ts, order=c(1,1,1), seasonal=list(order=c(2,1,1),period=12))
arima5<- Arima(co2ts, order=c(1,1,2), seasonal=list(order=c(1,1,1),period=12))
arima6<- Arima(co2ts, order=c(0,1,1), seasonal=list(order=c(0,1,1),period=12))
arima7<- Arima(co2ts, order=c(1,1,0), seasonal=list(order=c(1,1,0),period=12))
AIC(arima1,arima2,arima3,arima4,arima5,arima6,arima7)
BIC(arima1,arima2,arima3,arima4,arima5,arima6,arima7)
autoplot(pacf(arima6$residuals, plot = FALSE))
ggtsdiag(arima6)
bp <- Box.test(arima6$residuals) # Test de Box-Pierce
bp$p.value
lb <- Box.test(arima6$residuals, type="Ljung-Box") # Test de Ljung-Box
lb$p.value
jb <- jarque.bera.test(arima6$residuals) # Test de Jarque-Bera
jb$p.value
sht<-shapiro.test(arima6$residuals) $ # Test de Shapiro-Wilk
sht$p.value
auto.arima(co2ts, stepwise = FALSE, approximation = FALSE)
forecast1<-forecast(arima6, level = c(95), h = 50)
autoplot(forecast1)
