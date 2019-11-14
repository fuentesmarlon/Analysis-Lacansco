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
library(NbClust) #Para determinar el n�mero de clusters �ptimo
library(factoextra) #Para hacer gr�ficos bonitos de clustering
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

df_unidades <- read_excel("UnidadesPorSectorNew.xlsx")

clean_df <- df_unidades%>%
  na.omit()

new_df <- clean_df%>%
  group_by(`Producto - Descripcion`)%>%
  summarise(tot_151=sum(`20151`),
            tot_152=sum(`20152`),
            tot_153=sum(`20153`),
            tot_153=sum(`20153`),
            tot_154=sum(`20154`),
            tot_155=sum(`20155`),
            tot_156=sum(`20156`),
            tot_157=sum(`20157`),
            tot_158=sum(`20158`),
            tot_159=sum(`20159`),
            tot_1510=sum(`201510`),
            tot_1511=sum(`201511`),
            tot_1512=sum(`201512`),
            tot_161=sum(`20161`),
            tot_162=sum(`20162`),
            tot_163=sum(`20163`),
            tot_164=sum(`20164`),
            tot_165=sum(`20165`),
            tot_166=sum(`20166`),
            tot_167=sum(`20167`),
            tot_168=sum(`20168`),
            tot_169=sum(`20169`),
            tot_1610=sum(`201610`),
            tot_1611=sum(`201611`),
            tot_1612=sum(`201612`),
            tot_171=sum(`20171`),
            tot_172=sum(`20172`),
            tot_173=sum(`20173`),
            tot_174=sum(`20174`),
            tot_175=sum(`20175`),
            tot_176=sum(`20176`),
            tot_177=sum(`20177`),
            tot_178=sum(`20178`),
            tot_179=sum(`20179`),
            tot_1710=sum(`201710`),
            tot_1711=sum(`201711`),
            tot_1712=sum(`201712`),
            tot_181=sum(`20181`),
            tot_182=sum(`20182`),
            tot_183=sum(`20183`),
            tot_184=sum(`20184`),
            tot_185=sum(`20185`),
            tot_186=sum(`20186`),
            tot_187=sum(`20187`),
            tot_188=sum(`20188`),
            tot_189=sum(`20189`),
            tot_1810=sum(`201810`),
            tot_1811=sum(`201811`)
            )
N <- length(new_df$`Producto - Descripcion`)

var_df <- new_df%>%
  group_by(`Producto - Descripcion`)%>%
  summarise(
    med_151=tot_151/N,
    var151 = sum((tot_151 - med_151)^2, (N - n())*med_151^2)/(N - 1),
    
    med_152=tot_152/N,
    var152 = sum((tot_152 - med_151)^2, (N - n())*med_152^2)/(N - 1),
    
    med_153=tot_153/N,
    var153 = sum((tot_153 - med_153)^2, (N - n())*med_153^2)/(N - 1),
    
    med_154=tot_154/N,
    var154 = sum((tot_154 - med_154)^2, (N - n())*med_154^2)/(N - 1),
    
    med_155=tot_155/N,
    var155 = sum((tot_155 - med_155)^2, (N - n())*med_155^2)/(N - 1),
    
    med_156=tot_156/N,
    var156 = sum((tot_156 - med_156)^2, (N - n())*med_156^2)/(N - 1),
    
    med_157=tot_157/N,
    var157 = sum((tot_157 - med_157)^2, (N - n())*med_157^2)/(N - 1),
    
    med_158=tot_158/N,
    var158 = sum((tot_158 - med_158)^2, (N - n())*med_158^2)/(N - 1),
    med_159=tot_159/N,
    var159 = sum((tot_159 - med_159)^2, (N - n())*med_159^2)/(N - 1),
    
    med_1510=tot_1510/N,
    var1510 = sum((tot_1510 - med_1510)^2, (N - n())*med_1510^2)/(N - 1),
    
    med_1511=tot_1511/N,
    var1511 = sum((tot_1511 - med_1511)^2, (N - n())*med_1511^2)/(N - 1),
    
    med_1512=tot_1512/N,
    var1512 = sum((tot_1512 - med_1512)^2, (N - n())*med_1512^2)/(N - 1),
    med_161=tot_161/N,
    var161 = sum((tot_161 - med_161)^2, (N - n())*med_161^2)/(N - 1),
    
    med_162=tot_162/N,
    var162 = sum((tot_162 - med_161)^2, (N - n())*med_162^2)/(N - 1),
    
    med_163=tot_163/N,
    var163 = sum((tot_163 - med_163)^2, (N - n())*med_163^2)/(N - 1),
    
    med_164=tot_164/N,
    var164 = sum((tot_164 - med_164)^2, (N - n())*med_164^2)/(N - 1),
    
    med_165=tot_165/N,
    var165 = sum((tot_165 - med_165)^2, (N - n())*med_165^2)/(N - 1),
    
    med_166=tot_166/N,
    var166 = sum((tot_166 - med_166)^2, (N - n())*med_166^2)/(N - 1),
    
    med_167=tot_167/N,
    var167 = sum((tot_167 - med_167)^2, (N - n())*med_167^2)/(N - 1),
    
    med_168=tot_168/N,
    var168 = sum((tot_168 - med_168)^2, (N - n())*med_168^2)/(N - 1),
    med_169=tot_169/N,
    var169 = sum((tot_169 - med_169)^2, (N - n())*med_169^2)/(N - 1),
    
    med_1610=tot_1610/N,
    var1610 = sum((tot_1610 - med_1610)^2, (N - n())*med_1610^2)/(N - 1),
    
    med_1611=tot_1611/N,
    var1611 = sum((tot_1611 - med_1611)^2, (N - n())*med_1611^2)/(N - 1),
    
    med_1612=tot_1612/N,
    var1612 = sum((tot_1612 - med_1612)^2, (N - n())*med_1612^2)/(N - 1),
    med_171=tot_171/N,
    var171 = sum((tot_171 - med_171)^2, (N - n())*med_171^2)/(N - 1),
    
    med_172=tot_172/N,
    var172 = sum((tot_172 - med_171)^2, (N - n())*med_172^2)/(N - 1),
    
    med_173=tot_173/N,
    var173 = sum((tot_173 - med_173)^2, (N - n())*med_173^2)/(N - 1),
    
    med_174=tot_174/N,
    var174 = sum((tot_174 - med_174)^2, (N - n())*med_174^2)/(N - 1),
    
    med_175=tot_175/N,
    var175 = sum((tot_175 - med_175)^2, (N - n())*med_175^2)/(N - 1),
    
    med_176=tot_176/N,
    var176 = sum((tot_176 - med_176)^2, (N - n())*med_176^2)/(N - 1),
    
    med_177=tot_177/N,
    var177 = sum((tot_177 - med_177)^2, (N - n())*med_177^2)/(N - 1),
    
    med_178=tot_178/N,
    var178 = sum((tot_178 - med_178)^2, (N - n())*med_178^2)/(N - 1),
    med_179=tot_179/N,
    var179 = sum((tot_179 - med_179)^2, (N - n())*med_179^2)/(N - 1),
    
    med_1710=tot_1710/N,
    var1710 = sum((tot_1710 - med_1710)^2, (N - n())*med_1710^2)/(N - 1),
    
    med_1711=tot_1711/N,
    var1711 = sum((tot_1711 - med_1711)^2, (N - n())*med_1711^2)/(N - 1),
    
    med_1712=tot_1712/N,
    var1712 = sum((tot_1712 - med_1712)^2, (N - n())*med_1712^2)/(N - 1),
    
    med_181=tot_181/N,
    var181 = sum((tot_181 - med_181)^2, (N - n())*med_181^2)/(N - 1),
    
    med_182=tot_182/N,
    var182 = sum((tot_182 - med_181)^2, (N - n())*med_182^2)/(N - 1),
    
    med_183=tot_183/N,
    var183 = sum((tot_183 - med_183)^2, (N - n())*med_183^2)/(N - 1),
    
    med_184=tot_184/N,
    var184 = sum((tot_184 - med_184)^2, (N - n())*med_184^2)/(N - 1),
    
    med_185=tot_185/N,
    var185 = sum((tot_185 - med_185)^2, (N - n())*med_185^2)/(N - 1),
    
    med_186=tot_186/N,
    var186 = sum((tot_186 - med_186)^2, (N - n())*med_186^2)/(N - 1),
    
    med_187=tot_187/N,
    var187 = sum((tot_187 - med_187)^2, (N - n())*med_187^2)/(N - 1),
    
    med_188=tot_188/N,
    var188 = sum((tot_188 - med_188)^2, (N - n())*med_188^2)/(N - 1),
    med_189=tot_189/N,
    var189 = sum((tot_189 - med_189)^2, (N - n())*med_189^2)/(N - 1),
    
    med_1810=tot_1810/N,
    var1810 = sum((tot_1810 - med_1810)^2, (N - n())*med_1810^2)/(N - 1),
    
    med_1811=tot_1811/N,
    var1811 = sum((tot_1811 - med_1811)^2, (N - n())*med_1811^2)/(N - 1)
    )

var_df$sumVar <- rowSums (var_df[, c(3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49,51,53,55,57,59,61,63,65,67,69,71,73,75,77,79,81,83,85,87,89,91,93,95)])

var_prod <- var_df[, c(1,96)]

##----------------------------------------1 22----------------------------------------
datos<-as.data.frame(t(var_df[22, c(2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40,42,44,46,48,50,52,54,56,58,60,62,64,66,68,70,72,74,76,78,80,82,84,86,88,90,92,94)]))
##Prueba para series de tiempo 
prueba <- ts(datos, start=c(2015, 1), end=c(2017, 11), frequency=12) 
co2ts<-prueba

print(co2ts)
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

adf<-adf.test(diff.co2ts.12)
adf$p.value
kpss<-kpss.test(diff.co2ts.12)
kpss$p.value
autoplot(acf(diff.co2ts.12, plot = FALSE))

arima1<- Arima(co2ts, order=c(0,1,2), seasonal=list(order=c(0,1,1),period=12))
arima2<- Arima(co2ts, order=c(1,1,0), seasonal=list(order=c(2,1,0),period=12))
arima3<- Arima(co2ts, order=c(1,1,2), seasonal=list(order=c(2,1,1),period=12))
arima4<- Arima(co2ts, order=c(1,1,1), seasonal=list(order=c(2,1,1),period=12))
arima5<- Arima(co2ts, order=c(1,1,2), seasonal=list(order=c(1,1,1),period=12))
arima6<- Arima(co2ts, order=c(0,1,1), seasonal=list(order=c(0,1,1),period=12))
arima7<- Arima(co2ts, order=c(1,1,0), seasonal=list(order=c(1,1,0),period=12))
AIC(arima1,arima5,arima6,arima7)
BIC(arima1,arima5,arima6,arima7)

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
forecast1<-forecast(arima6, level = c(10), h = 15)
autoplot(forecast1)

comparar1 <- ts(datos, start=c(2018, 1), end=c(2018, 11), frequency=12) 
comparar1
forecast1


##----------------------------------------2 35----------------------------------------
datos<-as.data.frame(t(var_df[35, c(2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40,42,44,46,48,50,52,54,56,58,60,62,64,66,68,70,72,74,76,78,80,82,84,86,88,90,92,94)]))
##Prueba para series de tiempo 
prueba <- ts(datos, start=c(2015, 1), end=c(2017, 11), frequency=12) 
co2ts<-prueba

print(co2ts)
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

adf<-adf.test(diff.co2ts.12)
adf$p.value
kpss<-kpss.test(diff.co2ts.12)
kpss$p.value
autoplot(acf(diff.co2ts.12, plot = FALSE))

arima1<- Arima(co2ts, order=c(0,1,2), seasonal=list(order=c(0,1,1),period=12))
arima2<- Arima(co2ts, order=c(1,1,0), seasonal=list(order=c(2,1,0),period=12))
arima3<- Arima(co2ts, order=c(1,1,2), seasonal=list(order=c(2,1,1),period=12))
arima4<- Arima(co2ts, order=c(1,1,1), seasonal=list(order=c(2,1,1),period=12))
arima5<- Arima(co2ts, order=c(1,1,2), seasonal=list(order=c(1,1,1),period=12))
arima6<- Arima(co2ts, order=c(0,1,1), seasonal=list(order=c(0,1,1),period=12))
arima7<- Arima(co2ts, order=c(1,1,0), seasonal=list(order=c(1,1,0),period=12))
AIC(arima1,arima5,arima6,arima7)
BIC(arima1,arima5,arima6,arima7)

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
forecast2<-forecast(arima1, level = c(10), h = 15)
autoplot(forecast2)

comparar2 <- ts(datos, start=c(2018, 1), end=c(2018, 11), frequency=12) 
comparar2
forecast2



##----------------------------------------3 4----------------------------------------
datos<-as.data.frame(t(var_df[4, c(2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40,42,44,46,48,50,52,54,56,58,60,62,64,66,68,70,72,74,76,78,80,82,84,86,88,90,92,94)]))
##Prueba para series de tiempo 
prueba <- ts(datos, start=c(2015, 1), end=c(2017, 11), frequency=12) 
co2ts<-prueba

print(co2ts)
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

adf<-adf.test(diff.co2ts.12)
adf$p.value
kpss<-kpss.test(diff.co2ts.12)
kpss$p.value
autoplot(acf(diff.co2ts.12, plot = FALSE))

arima1<- Arima(co2ts, order=c(0,1,2), seasonal=list(order=c(0,1,1),period=12))
arima2<- Arima(co2ts, order=c(1,1,0), seasonal=list(order=c(2,1,0),period=12))
arima3<- Arima(co2ts, order=c(1,1,2), seasonal=list(order=c(2,1,1),period=12))
arima4<- Arima(co2ts, order=c(1,1,1), seasonal=list(order=c(2,1,1),period=12))
arima5<- Arima(co2ts, order=c(1,1,2), seasonal=list(order=c(1,1,1),period=12))
arima6<- Arima(co2ts, order=c(0,1,1), seasonal=list(order=c(0,1,1),period=12))
arima7<- Arima(co2ts, order=c(1,1,0), seasonal=list(order=c(1,1,0),period=12))
AIC(arima1,arima5,arima6,arima7)
BIC(arima1,arima5,arima6,arima7)

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
forecast3<-forecast(arima6, level = c(10), h = 15)
autoplot(forecast3)

comparar3 <- ts(datos, start=c(2018, 1), end=c(2018, 11), frequency=12) 
comparar3
forecast3



##----------------------------------------4 46----------------------------------------
datos<-as.data.frame(t(var_df[46, c(2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40,42,44,46,48,50,52,54,56,58,60,62,64,66,68,70,72,74,76,78,80,82,84,86,88,90,92,94)]))
##Prueba para series de tiempo 
prueba <- ts(datos, start=c(2015, 1), end=c(2017, 11), frequency=12) 
co2ts<-prueba

print(co2ts)
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

adf<-adf.test(diff.co2ts.12)
adf$p.value
kpss<-kpss.test(diff.co2ts.12)
kpss$p.value
autoplot(acf(diff.co2ts.12, plot = FALSE))

arima1<- Arima(co2ts, order=c(0,1,2), seasonal=list(order=c(0,1,1),period=12))
arima2<- Arima(co2ts, order=c(1,1,0), seasonal=list(order=c(2,1,0),period=12))
arima3<- Arima(co2ts, order=c(1,1,2), seasonal=list(order=c(2,1,1),period=12))
arima4<- Arima(co2ts, order=c(1,1,1), seasonal=list(order=c(2,1,1),period=12))
arima5<- Arima(co2ts, order=c(1,1,2), seasonal=list(order=c(1,1,1),period=12))
arima6<- Arima(co2ts, order=c(0,1,1), seasonal=list(order=c(0,1,1),period=12))
arima7<- Arima(co2ts, order=c(1,1,0), seasonal=list(order=c(1,1,0),period=12))
AIC(arima1,arima5,arima6,arima7)
BIC(arima1,arima5,arima6,arima7)

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
forecast4<-forecast(arima1, level = c(10), h = 15)
autoplot(forecast4)

comparar4 <- ts(datos, start=c(2018, 1), end=c(2018, 11), frequency=12) 
comparar4
forecast4



##----------------------------------------5 7----------------------------------------
datos<-as.data.frame(t(var_df[7, c(2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40,42,44,46,48,50,52,54,56,58,60,62,64,66,68,70,72,74,76,78,80,82,84,86,88,90,92,94)]))
##Prueba para series de tiempo 
prueba <- ts(datos, start=c(2015, 1), end=c(2017, 11), frequency=12) 
co2ts<-prueba

print(co2ts)
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

adf<-adf.test(diff.co2ts.12)
adf$p.value
kpss<-kpss.test(diff.co2ts.12)
kpss$p.value
autoplot(acf(diff.co2ts.12, plot = FALSE))

arima1<- Arima(co2ts, order=c(0,1,2), seasonal=list(order=c(0,1,1),period=12))
arima2<- Arima(co2ts, order=c(1,1,0), seasonal=list(order=c(2,1,0),period=12))
arima3<- Arima(co2ts, order=c(1,1,2), seasonal=list(order=c(2,1,1),period=12))
arima4<- Arima(co2ts, order=c(1,1,1), seasonal=list(order=c(2,1,1),period=12))
arima5<- Arima(co2ts, order=c(1,1,2), seasonal=list(order=c(1,1,1),period=12))
arima6<- Arima(co2ts, order=c(0,1,1), seasonal=list(order=c(0,1,1),period=12))
arima7<- Arima(co2ts, order=c(1,1,0), seasonal=list(order=c(1,1,0),period=12))
AIC(arima1,arima5,arima6,arima7)
BIC(arima1,arima5,arima6,arima7)

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


