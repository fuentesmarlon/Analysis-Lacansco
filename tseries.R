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
library(forecast)
library(tseries)
library(fUnitRoots)
library(ggfortify)
library(ggplot2)

library(forecast)
library(tseries)
library(fUnitRoots)
library(ggfortify)


UnidadesPorSector[UnidadesPorSector<0] <- 0
Paginacion20152019[Paginacion20152019<0] <- 0
historia[historia<0] <- 0




# separar producto y descripcion 
UnidadesPorSector = separate(data =  UnidadesPorSector, 
                             col  =  `Producto - Descripcion`,  
                             into =  c("Producto", "Descripcion"), 
                             sep  =  10, remove = TRUE,
                             convert = TRUE )





##PRUEBA UNION
prueba<- aggregate(. ~ Descripcion, UnidadesPorSector[,-c(1:3)], sum)


prueba$totales= rowSums(prueba[,2:50])
head(prueba)

prueba = prueba[-1,]
prueba$totales= rowSums(prueba[,2:50])


View(prueba[order(prueba$totales),])

##Pasando los na a 0 para probar hacer serie de tiempo 
UnidadesPorSector[is.na(UnidadesPorSector)] <- 0

##AQUI ell "165319", es la fila donde esta el dato. solo eso se modifica
datos<-as.data.frame(t(UnidadesPorSector[6213,1:58]))
##Prueba para series de tiempo 
prueba <- ts(datos, start=c(2015, 1), end=c(2017, 12), frequency=12) 
co2ts<-prueba

print(co2ts)

library(ggfortify)

autoplot(co2ts, ts.colour = "blue", ts.linetype = "dashed")
autoplot(acf(co2ts, plot = FALSE))
autoplot(stl(co2ts, s.window = "periodic"), ts.colour = "blue")
nsdiffs(co2ts)
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

##EN BASE AL AIC Y BIC, SE ELIGE LOS QUE MENOR VALOR TIENEN PARA ELEGIR QUE ARIMA USAR


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



##PREDICCIONES PARA LOS 5 MAS VENDIDOS Y LOS 5 MENOS VENDIDOS

##Los top5 mas vendidos 
##1. TOP SECRET ROLL-ON BABY POWDER 80G
##2. FRESH FIT SPRAY AMNIOTICO 200
##3. SCENTIA ROLL-ON EXTREME LADY 80G
##4. TOP FAMILY JABON LIQUIDO NEUTRO PARA ZONAS INTIMAS 800ML	
##5. RABANO YODADO 240 ML	

##Columnas en las que estan 
##1. 6213 [6213,1:58]
##2. 2313
##3. 5296
##4. 6057
##5. 4084




##----------------------------------------1----------------------------------------
##AQUI ell "165319", es la fila donde esta el dato. solo eso se modifica
datos<-as.data.frame(t(UnidadesPorSector[6213,1:58]))
##Prueba para series de tiempo 
prueba <- ts(datos, start=c(2015, 1), end=c(2017, 12), frequency=12) 
co2ts<-prueba

print(co2ts)

library(ggfortify)

autoplot(co2ts, ts.colour = "blue", ts.linetype = "dashed")
autoplot(acf(co2ts, plot = FALSE))
autoplot(stl(co2ts, s.window = "periodic"), ts.colour = "blue")
nsdiffs(co2ts)
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

##EN BASE AL AIC Y BIC, SE ELIGE LOS QUE MENOR VALOR TIENEN PARA ELEGIR QUE ARIMA USAR


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
#fit<-Arima(co2ts,order = c(1,1,0), seasonal = c(1,0,0))

forecast1<-forecast(arima6, level = c(10), h = 15)
autoplot(forecast1)
#autoplot(forecast(fit3))
comparar1 <- ts(datos, start=c(2018, 1), end=c(2018, 11), frequency=12) 
forecast1
comparar1
##----------------------------------------2 2313----------------------------------------
datos<-as.data.frame(t(UnidadesPorSector[2313,1:58]))
##Prueba para series de tiempo 
prueba <- ts(datos, start=c(2015, 1), end=c(2017, 12), frequency=12) 
co2ts<-prueba

print(co2ts)

library(ggfortify)

autoplot(co2ts, ts.colour = "blue", ts.linetype = "dashed")
autoplot(acf(co2ts, plot = FALSE))
autoplot(stl(co2ts, s.window = "periodic"), ts.colour = "blue")
nsdiffs(co2ts)
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

##EN BASE AL AIC Y BIC, SE ELIGE LOS QUE MENOR VALOR TIENEN PARA ELEGIR QUE ARIMA USAR


autoplot(pacf(arima5$residuals, plot = FALSE))
ggtsdiag(arima5)
bp <- Box.test(arima5$residuals) # Test de Box-Pierce
bp$p.value
lb <- Box.test(arima5$residuals, type="Ljung-Box") # Test de Ljung-Box
lb$p.value
jb <- jarque.bera.test(arima5$residuals) # Test de Jarque-Bera
jb$p.value
sht<-shapiro.test(arima5$residuals) $ # Test de Shapiro-Wilk
  sht$p.value
auto.arima(co2ts, stepwise = FALSE, approximation = FALSE)
forecast2<-forecast(arima5, level = c(10), h = 15)
autoplot(forecast2)

comparar2 <- ts(datos, start=c(2018, 1), end=c(2018, 11), frequency=12) 
comparar2
forecast2


##----------------------------------------3 5296----------------------------------------
datos<-as.data.frame(t(UnidadesPorSector[5296,1:58]))
##Prueba para series de tiempo 
prueba <- ts(datos, start=c(2015, 1), end=c(2017, 11), frequency=12) 
co2ts<-prueba

print(co2ts)

library(ggfortify)

autoplot(co2ts, ts.colour = "blue", ts.linetype = "dashed")
autoplot(acf(co2ts, plot = FALSE))
autoplot(stl(co2ts, s.window = "periodic"), ts.colour = "blue")
nsdiffs(co2ts)
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

##EN BASE AL AIC Y BIC, SE ELIGE LOS QUE MENOR VALOR TIENEN PARA ELEGIR QUE ARIMA USAR


autoplot(pacf(arima1$residuals, plot = FALSE))
ggtsdiag(arima1)
bp <- Box.test(arima1$residuals) # Test de Box-Pierce
bp$p.value
lb <- Box.test(arima1$residuals, type="Ljung-Box") # Test de Ljung-Box
lb$p.value
jb <- jarque.bera.test(arima1$residuals) # Test de Jarque-Bera
jb$p.value
sht<-shapiro.test(arima1$residuals) $ # Test de Shapiro-Wilk
  sht$p.value
auto.arima(co2ts, stepwise = FALSE, approximation = FALSE)
forecast3<-forecast(arima1, level = c(10), h = 15)
autoplot(forecast3)

comparar3 <- ts(datos, start=c(2018, 1), end=c(2018, 11), frequency=12) 
comparar3
forecast3
##----------------------------------------4 6057----------------------------------------

datos<-as.data.frame(t(UnidadesPorSector[6057,1:58]))
##Prueba para series de tiempo 
prueba <- ts(datos, start=c(2015, 1), end=c(2017, 12), frequency=12) 
co2ts<-prueba

print(co2ts)

library(ggfortify)

autoplot(co2ts, ts.colour = "blue", ts.linetype = "dashed")
autoplot(acf(co2ts, plot = FALSE))
autoplot(stl(co2ts, s.window = "periodic"), ts.colour = "blue")
nsdiffs(co2ts)
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

##EN BASE AL AIC Y BIC, SE ELIGE LOS QUE MENOR VALOR TIENEN PARA ELEGIR QUE ARIMA USAR


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
forecast4<-forecast(arima6, level = c(10), h = 15)
autoplot(forecast4)

comparar4 <- ts(datos, start=c(2018, 1), end=c(2018, 11), frequency=12) 
comparar4
forecast4
##----------------------------------------5 4084----------------------------------------

datos<-as.data.frame(t(UnidadesPorSector[4084,1:58]))
##Prueba para series de tiempo 
prueba <- ts(datos, start=c(2015, 1), end=c(2017, 11), frequency=12) 
co2ts<-prueba

print(co2ts)

library(ggfortify)

autoplot(co2ts, ts.colour = "blue", ts.linetype = "dashed")
autoplot(acf(co2ts, plot = FALSE))
autoplot(stl(co2ts, s.window = "periodic"), ts.colour = "blue")
nsdiffs(co2ts)
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

##EN BASE AL AIC Y BIC, SE ELIGE LOS QUE MENOR VALOR TIENEN PARA ELEGIR QUE ARIMA USAR


autoplot(pacf(arima1$residuals, plot = FALSE))
ggtsdiag(arima1)
bp <- Box.test(arima1$residuals) # Test de Box-Pierce
bp$p.value
lb <- Box.test(arima1$residuals, type="Ljung-Box") # Test de Ljung-Box
lb$p.value
jb <- jarque.bera.test(arima1$residuals) # Test de Jarque-Bera
jb$p.value
sht<-shapiro.test(arima1$residuals) $ # Test de Shapiro-Wilk
  sht$p.value
auto.arima(co2ts, stepwise = FALSE, approximation = FALSE)
forecast5<-forecast(arima1, level = c(10), h = 15)
autoplot(forecast5)

comparar5 <- ts(datos, start=c(2018, 1), end=c(2018, 11), frequency=12) 
comparar5
forecast5

##Los top5 menos vendidos
##1.  ACNÉ SOLUTION MASCA MICRO ABRA OFE	
##2.  ACTION GEL FIJADOR ANTI CAÍDA 200 G	
##3.  ACTION SHAMPOO ANTICAIDA OFERTA	
##4.  ADVENTURE ROLL ON 80GRS	
##5.  AMUEBLADO DE COMEDOR 6 PERSONAS ESCOCIA	

##Columnas en las que estan 
##1. 79
##2. 83
##3. 86
##4. 91
##5. 96

##----------------------------------------1 79----------------------------------------

datos<-as.data.frame(t(UnidadesPorSector[79,1:58]))
##Prueba para series de tiempo 
prueba <- ts(datos, start=c(2015, 1), end=c(2018, 11), frequency=12) 
co2ts<-prueba

print(co2ts)

library(ggfortify)

autoplot(co2ts, ts.colour = "blue", ts.linetype = "dashed")
autoplot(acf(co2ts, plot = FALSE))
autoplot(stl(co2ts, s.window = "periodic"), ts.colour = "blue")
nsdiffs(co2ts)
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

##EN BASE AL AIC Y BIC, SE ELIGE LOS QUE MENOR VALOR TIENEN PARA ELEGIR QUE ARIMA USAR


autoplot(pacf(arima1$residuals, plot = FALSE))
ggtsdiag(arima1)
bp <- Box.test(arima1$residuals) # Test de Box-Pierce
bp$p.value
lb <- Box.test(arima1$residuals, type="Ljung-Box") # Test de Ljung-Box
lb$p.value
jb <- jarque.bera.test(arima1$residuals) # Test de Jarque-Bera
jb$p.value
sht<-shapiro.test(arima1$residuals) $ # Test de Shapiro-Wilk
  sht$p.value
auto.arima(co2ts, stepwise = FALSE, approximation = FALSE)
forecast1<-forecast(arima1, level = c(95), h = 50)
autoplot(forecast1)
##----------------------------------------2 83----------------------------------------
datos<-as.data.frame(t(UnidadesPorSector[83,1:58]))
##Prueba para series de tiempo 
prueba <- ts(datos, start=c(2015, 1), end=c(2018, 11), frequency=12) 
co2ts<-prueba

print(co2ts)

library(ggfortify)

autoplot(co2ts, ts.colour = "blue", ts.linetype = "dashed")
autoplot(acf(co2ts, plot = FALSE))
autoplot(stl(co2ts, s.window = "periodic"), ts.colour = "blue")
nsdiffs(co2ts)
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

##EN BASE AL AIC Y BIC, SE ELIGE LOS QUE MENOR VALOR TIENEN PARA ELEGIR QUE ARIMA USAR


autoplot(pacf(arima4$residuals, plot = FALSE))
ggtsdiag(arima4)
bp <- Box.test(arima4$residuals) # Test de Box-Pierce
bp$p.value
lb <- Box.test(arima4$residuals, type="Ljung-Box") # Test de Ljung-Box
lb$p.value
jb <- jarque.bera.test(arima4$residuals) # Test de Jarque-Bera
jb$p.value
sht<-shapiro.test(arima4$residuals) $ # Test de Shapiro-Wilk
  sht$p.value
auto.arima(co2ts, stepwise = FALSE, approximation = FALSE)
forecast1<-forecast(arima4, level = c(95), h = 50)
autoplot(forecast1)

##----------------------------------------3 86----------------------------------------
datos<-as.data.frame(t(UnidadesPorSector[86,1:58]))
##Prueba para series de tiempo 
prueba <- ts(datos, start=c(2015, 1), end=c(2018, 11), frequency=12) 
co2ts<-prueba

print(co2ts)

library(ggfortify)

autoplot(co2ts, ts.colour = "blue", ts.linetype = "dashed")
autoplot(acf(co2ts, plot = FALSE))
autoplot(stl(co2ts, s.window = "periodic"), ts.colour = "blue")
nsdiffs(co2ts)
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

##EN BASE AL AIC Y BIC, SE ELIGE LOS QUE MENOR VALOR TIENEN PARA ELEGIR QUE ARIMA USAR


autoplot(pacf(arima1$residuals, plot = FALSE))
ggtsdiag(arima1)
bp <- Box.test(arima1$residuals) # Test de Box-Pierce
bp$p.value
lb <- Box.test(arima1$residuals, type="Ljung-Box") # Test de Ljung-Box
lb$p.value
jb <- jarque.bera.test(arima1$residuals) # Test de Jarque-Bera
jb$p.value
sht<-shapiro.test(arima1$residuals) $ # Test de Shapiro-Wilk
  sht$p.value
auto.arima(co2ts, stepwise = FALSE, approximation = FALSE)
forecast1<-forecast(arima1, level = c(95), h = 50)
autoplot(forecast1)

##----------------------------------------4 91----------------------------------------
datos<-as.data.frame(t(UnidadesPorSector[91,1:58]))
##Prueba para series de tiempo 
prueba <- ts(datos, start=c(2015, 1), end=c(2018, 11), frequency=12) 
co2ts<-prueba

print(co2ts)

library(ggfortify)

autoplot(co2ts, ts.colour = "blue", ts.linetype = "dashed")
autoplot(acf(co2ts, plot = FALSE))
autoplot(stl(co2ts, s.window = "periodic"), ts.colour = "blue")
nsdiffs(co2ts)
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

##EN BASE AL AIC Y BIC, SE ELIGE LOS QUE MENOR VALOR TIENEN PARA ELEGIR QUE ARIMA USAR


autoplot(pacf(arima1$residuals, plot = FALSE))
ggtsdiag(arima1)
bp <- Box.test(arima1$residuals) # Test de Box-Pierce
bp$p.value
lb <- Box.test(arima1$residuals, type="Ljung-Box") # Test de Ljung-Box
lb$p.value
jb <- jarque.bera.test(arima1$residuals) # Test de Jarque-Bera
jb$p.value
sht<-shapiro.test(arima1$residuals) $ # Test de Shapiro-Wilk
  sht$p.value
auto.arima(co2ts, stepwise = FALSE, approximation = FALSE)
forecast1<-forecast(arima1, level = c(95), h = 50)
autoplot(forecast1)

##----------------------------------------5 96----------------------------------------

datos<-as.data.frame(t(UnidadesPorSector[96,1:58]))
##Prueba para series de tiempo 
prueba <- ts(datos, start=c(2015, 1), end=c(2018, 11), frequency=12) 
co2ts<-prueba

print(co2ts)

library(ggfortify)

autoplot(co2ts, ts.colour = "blue", ts.linetype = "dashed")
autoplot(acf(co2ts, plot = FALSE))
autoplot(stl(co2ts, s.window = "periodic"), ts.colour = "blue")
nsdiffs(co2ts)
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

##EN BASE AL AIC Y BIC, SE ELIGE LOS QUE MENOR VALOR TIENEN PARA ELEGIR QUE ARIMA USAR


autoplot(pacf(arima1$residuals, plot = FALSE))
ggtsdiag(arima1)
bp <- Box.test(arima1$residuals) # Test de Box-Pierce
bp$p.value
lb <- Box.test(arima1$residuals, type="Ljung-Box") # Test de Ljung-Box
lb$p.value
jb <- jarque.bera.test(arima1$residuals) # Test de Jarque-Bera
jb$p.value
sht<-shapiro.test(arima1$residuals) $ # Test de Shapiro-Wilk
  sht$p.value
auto.arima(co2ts, stepwise = FALSE, approximation = FALSE)
forecast1<-forecast(arima1, level = c(95), h = 50)
autoplot(forecast1)

#serie de tiempo categorica
mytable <- table(historia$Pagina...22)

lbls <- paste(names(mytable), "\n", mytable, sep="")

pie(mytable, labels = lbls, 
    
    main="Paginacion")

library(lubridate)
library(dplyr)
library(forecast)
library(ggplot2)
library(scales)
pjm<- historia
str(pjm)
pjm$Pagina...22 <- ymd_hms(pjm$Pagina...22)
ts_train<-pjm$`Unidades Vendidas` %>% ts(freq= 24)
ts_train %>% 
  tail(24*7*4) %>% 
  decompose() %>% 
  autoplot()
msts_cons<-pjm$`Unidades Vendidas` %>% msts( seasonal.periods = c(24, 24*7))
msts_cons  %>% head(  24 *7 *4 ) %>% mstl() %>% autoplot()   

msts_train <- head(msts_cons, length(msts_cons) - 24*7)
msts_test <- tail(msts_cons,  24*7)

#subset to more recent period
msts_train <- tail(msts_train, 24*7*4*3)
autoplot(msts_train)

stlm_model <- msts_train %>%
  stlm(lambda = 0) %>% 
  forecast(h = 24*7) 
plot(stlm_model)
