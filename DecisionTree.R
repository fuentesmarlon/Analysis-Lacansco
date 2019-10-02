# instalación de paquetes -------------------------------------------------



# install.packages("tree")



library(rpart)

library(caret)

library(tree)

library(rpart.plot)

library(randomForest)



datos <- clean_historia

datos$Categoria[is.na(datos$Categoria)] <- 0
datos$Categoria[datos$Categoria == "USO DIARIO"] <- 1
datos$Categoria[datos$Categoria == "SALUD Y BIENESTAR"] <- 2
datos$Categoria[datos$Categoria == "PAQUETES"] <- 3
datos$Categoria[datos$Categoria == "ELLAS"] <- 4
datos$Categoria[datos$Categoria == "PROMOCIONALES VENDIDOS"] <- 5
datos$Categoria[datos$Categoria == "CABELLO"] <- 6
datos$Categoria[datos$Categoria == "MAQUILLAJE"] <- 7
datos$Categoria[datos$Categoria == "NI�OS"] <- 8
datos$Categoria[datos$Categoria == "ELLOS"] <- 9
datos$Categoria[datos$Categoria == "LIMPIEZA"] <- 10
datos$Categoria[datos$Categoria == "CUIDADO DE LA PIEL"] <- 11
datos$Categoria[datos$Categoria == "DIVISION FARMA"] <- 12

#check converted names at any time
tail(datos$Categoria,50)

# variable respuesta la clase de la flor

porciento <- 70/100



set.seed(123)



trainRowsNumber<-sample(1:nrow(datos),porciento*nrow(datos))

train<-datos[trainRowsNumber,]

test<-datos[-trainRowsNumber,]

#3 variables categoria
dt_model<-rpart(Categoria~Pedido.Real+Precio.Catalogo,datos, method = "class")

#categoria vs pedido
dt_model2<-rpart(Categoria~Pedido.Real,datos, method = "class")


#ventas
dt_model<-rpart(Pedido.Real~Categoria+Precio.Catalogo,datos, method = "anova")
#tipo de pagina vs pedido real
datos$Pedido.Real[datos$Pedido.Real == 0] <-NA
dt_model_tipo_pagina<-rpart(Tipo.pagina~Pedido.Real,datos, method = "class")


plot(dt_model);text(dt_model)

prp(dt_model)

rpart.plot(dt_model_tipo_pagina)



head(test)

prediccion <- predict(dt_model, newdata = test, na.action = na.pass)

#Apply: Para cada fila, determina el nombre de la columna del valor máximo entre los tres valores de una fila

columnaMasAlta<-apply(prediccion, 1, function(x) colnames(prediccion)[which.max(x)])

test$prediccion<-columnaMasAlta #Se le añade al grupo de prueba el valor de la predicción


table(prediccion, train$Pedido.Real)
cfm<-confusionMatrix(table(prediccion,datos$Pedido.Real))

cfm



#con caret

ct<-trainControl(method = "cv",train[,1:4],number=3, verboseIter=T)

modelorf<-train(Categoria~ Pagina + Precio.Catalogo,data=train,method="rf",trControl = ct)

prediccionRF<-predict(modelorf,newdata = test[,1:4])

testCompleto$predRFCaret<-prediccionRF

cfmCaret <- confusionMatrix(testCompleto$predRFCaret,testCompleto$Categoria)



#con random forest

modeloRF1<-randomForest(Categoria~.,data=train)

prediccionRF1<-predict(modeloRF1,newdata = test)

testCompleto<-test

testCompleto$predRF<-prediccionRF1

cfmRandomForest <- confusionMatrix(testCompleto$predRF, testCompleto$Categoria)
