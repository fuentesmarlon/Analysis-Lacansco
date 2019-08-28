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
  
 
    