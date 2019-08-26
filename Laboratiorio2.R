


########################################
## Universidad del Valle de Guatemala ##
## Laboratorio 2: Data Science        ##
## Autores:                           ##
##    Mayra Silva                     ##
##    Odalis Reyes                    ##           
##    Ivan Maldonado                  ##
########################################



# Instalación de paquetes
#install.packages("rela")
#install.packages("psych")
#install.packages("FactoMineR")
#install.packages("corrplot")
#install.packages("cluster")
#install.packages("fpc")
#install.packages("NbClust")
#install.packages("factoextra")
#install.packages("REdaS")
#install.packages("arules")
#install.packages("ggplot2")
#install.packages("ggpubr")
#install.packages("ggmap")
#install.packages("arulesViz")
#install.packages("dplyr")
#install.packages("caret")
#install.packages("class")
#install.packages("e1071")

require(ggpubr) # Para mejorar la visualización gráfica
require(tidyverse) # Para explotar, manipular y visualizar datos que comparten info
require(corrplot) # Para visualizar la matriz de correlación
require(cluster) #Para calcular la silueta
library(fpc) # Para hacer el plotcluster
library(NbClust) # Para determinar el numero de clusters optimo
library(factoextra) # Para hacer graficos bonitos de clustering
library(rela) # Para poder utilizar paf()
library(psych) # Para poder utilizar KMO()
library(FactoMineR)
library(corrplot)
library(REdaS)
library(ggplot2) # Graficas bonitas
library(ggpubr) # Graficas bonitas x2
#library(ggmap)
library(arules) # Reglas de asociacion
library(factoextra) 
library(arulesViz)
library(dplyr)
library(caret) # Muestreo estratificado
library(class) # Para KNN
library(e1071) # Requisito para la matriz de confusión

setwd("C:/Users/smayr/Documents/Tercer a�o/Semestre 6/Data Science/Laboratorio 2/Lab2DataSciencie")


# Leyendo el dataset de csv
train <- read.csv("train.csv", TRUE, ",")
# Volviendo el csv en un data frame
class(train)

# ------------ Laboratorio 1 ---------------

# Exploración rápida del dataset usando un resumen
summary(train)
# Se separan las variables cuantitativas del dataset train
trainCuan <- train[,c(4,5,18,19,20,21,27,35,37,38,39,44,45,46,47,48,49,50,51,52,53,55,57,60,62,63,67,68,69,70,71,72,76,77,78,81)]
# Se separan las variables cualitativas del dataset train
trainCual <- train[,-c(4,5,18,19,20,21,27,35,37,38,39,44,45,46,47,48,49,50,51,52,53,55,57,60,62,63,67,68,69,70,71,72,76,77,78,81)]






# --- Elaboración de la matriz de correlación --- 

# Se usa la función cor la cual calcula la correlación de variables numéricas
# Se usa la función round la cual redondea el resultado a 3 decimales
mcorrelacion <- round(cor(trainCuan,use="complete.obs"),3)
corre <- cor(trainCuan)
# Se visualiza la matriz de correlación de forma gráfica
corrplot(mcorrelacion, type="upper")
corrplot(mcorrelacion, type="lower")




# ----------- Graficas cuantitativas -------------
# Histograma para el precio de ventas
hist(train$SalePrice, main = "Histograma del precio de ventas", xlab = "Precios de venta")
# Grafica de dispersion para observar la relacion de dos variables
plot(x = train$SalePrice, y = train$GrLivArea, main = "Relacion del precio con el área de la casa", 
     xlab = "Precio de venta", 
     ylab = "Åra habitable de la casa")
# Siagrama de cajas y bigotes para observar el año de construccion
boxplot(train$YearBuilt, main = "Diagrama de caja y bigotes para el año de construcción",
        ylab="Año de construcción")
# Histograma de los meses de venta
hist(train$MoSold, main = "Cantidad de ventas en los 12 meses", 
     xlab = "Meses")



# ----------- Graficas cuanlitativas -------------
# Barras para las zonas de venta
plot(x = train$MSZoning, main = "Clasificación general de las zonas de venta",
     xlab = "Zonas")
# Grafica de pie
x <-  c(95, 1365)
labels <-  c("S�?", "No")
piepercent<- round(100*x/sum(x), 1) # Porcentaje
# Pie chart
pie(x, labels = piepercent, main = "Proporcion de casas con aire acondicionado",
    col = rainbow(length(x)))
legend("topright", c("S�?","No"), cex = 0.8,
       fill = rainbow(length(x)))





# ---------- Clustering -----------

# Elimina los NAs de las variables cuantitativas
trainCuan <- na.omit(trainCuan)

# Se calcula la cantidad de clusters necesarios según el método de Ward
ward <- (nrow(trainCuan[,1:36])-1)*sum(apply(trainCuan[,1:36],2,var))
for (i in 2:10) 
  ward[i] <- sum(kmeans(trainCuan[,1:36], centers=i)$withinss)

# Se grafica el resultado para visualizar mejor
plot(1:10, ward, type="b", xlab="Numero de clusters",  ylab="Grupos suma de cuadrados")

# Agrupamiento por medio de las k-medias
numericas <- trainCuan
km <- kmeans(trainCuan[,1:36],3) # Se calculan 3 clusters
numericas$grupo <- km$cluster # Se crea una nueva columna con el numero de cluster

# Se grafican los 3 grupos de cluster
fviz_cluster(km, data = trainCuan[,1:36],geom = "point", ellipse.type = "norm")

# Método de la silueta
silcluster <- silhouette(km$cluster,dist(trainCuan[,1:36]))
mean(silcluster[,3]) 
# Da como resultado 0.5616

# Grafica el numero optimo de clusters
fviz_nbclust(scale(numericas), kmeans, method = "silhouette", k.max = 10) + theme_minimal() + ggtitle("The Silhouette Plot")


# ---------- PCA -----------

KMO(corre) #Mala adecuacion muestral
bart_spher(mcorrelacion) # valor p aproximadamente 0. Se rechaza Ho. Se realiza el PCA.
#pcaTrainCuan <- prcomp(trainCuan, scale = T)
#pcaTrainCuan #Correr este código para observar las componentes de las componentes principales

trainCuanPCA <- PCA(trainCuan,ncp = 22) #Se crea un PCA de 22 componentes principales
summary(trainCuanPCA) #Se observa la varianza acumulada que es del 91%
var<-get_pca_var(trainCuanPCA) #Se separan sacan las variables del PCA
corrplot(var$cos2, is.corr = F) #Se hace una gráfica de cos2

#Se crean nuevas variables a partir de las variables con mayor aporte en en las 22 componentes principales
exteriorVivienda <- 0.103 * trainCuan$OverallCond + 0.113 * trainCuan$LotFrontage
temporadaCompra <- 0.355 * trainCuan$X3SsnPorch + 0.152 * trainCuan$PoolArea + 0.105 * trainCuan$MoSold
sotano <- 0.041 * trainCuan$BsmtFinSF1 + 0.345 * trainCuan$BsmtFinSF2
tamanoPrecio <-  0.49 * trainCuan$X1stFlrSF + 0.11 * trainCuan$X2ndFlrSF + 0.64 * trainCuan$GrLivArea + 0.57 * trainCuan$GarageCars + 0.55 * trainCuan$GarageArea # + 0.794 * trainCuan$SalePrice
areaMalaCal <- 0.43 * trainCuan$LowQualFinSF
ventaPrecio <- trainCuan$SalePrice
calidad <- 0.667*trainCuan$OverallQual

#Se genera un data frame a partir de estas componentes
compPrincipales <- cbind(ventaPrecio,exteriorVivienda, temporadaCompra, sotano, tamanoPrecio, areaMalaCal, calidad)
compPrincipales <- as.data.frame(compPrincipales)

# -------------- Apriori ------------------
reglas<-apriori(trainCual[,3:45], parameter = list(support = 0.50,
                                                   confidence = 0.60,
                                                   target = "rules"))

# rulesAP <- apriori(trainCuan, parameter = list(support = 0.5, condifence = 0.8, maxlen = 10, maxtime=5, target = "rules"))

#reglas m?s importantes
#top10subRules <- head(reglas, n = 10, by = "confidence")
#plot(top10subRules, method="graph", engine="htmlwidget")








# LABORATORIO 2

#-------------------------------------------------
# Muestreo aleatorio simple
#-------------------------------------------------

#Particionando los datos en conjunto de entrenamiento y prueba con muestreo aleatorio simple
set.seed(123)
porciento <- 60/100 #Porciento en el que se partirán los datos
muestra<-sample(1:nrow(compPrincipales),porciento*nrow(compPrincipales)) #Muestra aleatoria de numeros de un vector

compPrincipales <- na.omit(compPrincipales)
trainSet<-compPrincipales[muestra,] #Obtengo las filas de los elementos que estan en el sector de muestra
testSet<-compPrincipales[-muestra,] #Obtengo las filas de los elementos que no están en el vector de muestra


#-------------------------------------------------
# Regresion Lineal Multivariada 
#-------------------------------------------------

#Se crea un data frame que almacena las componentes principales y la variable precio de venta
comparacion <- as.data.frame(cbind(exteriorVivienda, temporadaCompra, sotano, tamanoPrecio, areaMalaCal, ventaPrecio, calidad))
pairs(comparacion, col = "red")

#Modelo de regresión lineal multivariado
trainModel <- lm(formula = trainSet$ventaPrecio ~ trainSet$tamanoPrecio + trainSet$exteriorVivienda +trainSet$temporadaCompra + trainSet$calidad, data = trainSet)
summary(trainModel) # Resumen del modelo con un R^2 de 0.78
trainModel$coefficients 
# Grafica de las variables
plot(y = trainSet$ventaPrecio, x = trainSet$tamanoPrecio + trainSet$exteriorVivienda + trainSet$temporadaCompra, xlab = "Cualidades de la casa/terreno", ylab = "Precio de venta de las casas")
abline(trainModel)




#-------------------------------------------------
# Prediccion del modelo
#-------------------------------------------------

step(object = trainModel, direction = "both", trace = 1)
  #las variables seleccionadas fueron tamano precio, exterior y calidad
#modelo mejorado
modelNew <- lm(formula = trainSet$ventaPrecio ~ trainSet$tamanoPrecio + trainSet$exteriorVivienda + trainSet$calidad, data = trainSet)
summary(modelNew)
confint(modelNew)

#error del modelo
sigma(modelNew)/mean(comparacion$ventaPrecio)


modeloLinealMulti<-lm(trainSet$ventaPrecio~., data = trainSet)
summary(modeloLinealMulti)

#predicci?n
#prediccion<-predict(modeloLinealSimple,newdata = testSet)
# Se agrega la predicci?n al conjunto de entrenamiento
#testSet$SalePricePred<-prediccion
#Ver la diferencia entre lo real y lo predicho
#dif<-abs(testSet$SalePricePred-testSet$SalePrice)


#-------------------------------------------------
# Variable categórica de precios
#-------------------------------------------------

#Se obtienen los cuartiles de los precios
quantile(trainCuan$SalePrice)
#0%    25%    50%    75%   100% 
#35311 131000 164900 219500 755000
#En vase a estos cuartiles se tomará como bajo a los precios entre 35311 y 131000. Como medio a los precios entre 131000 y 219500.
#Como alto a los precios entre 219500 en adelante.

#Se crea una columna de clasificacion para el precio
compPrincipales$precio <- case_when(compPrincipales$ventaPrecio < 131000 ~ 'Bajo',
                                  compPrincipales$ventaPrecio < 219500 ~ 'Medio',
                                  TRUE ~ 'Alto')


#-------------------------------------------------
# Muestreo estraficiado
#-------------------------------------------------

# nuevoTrainSet <- stratified(compPrincipales, c('precio'), 0.6)
# nuevoTestSet <- compPrincipales %>%filter(compPrincipales$ventaPrecio != nuevoTrainSet$ventaPrecio)
# } 

# Se crea una muestra estraficiada por el precio de casas
filas <- createDataPartition(compPrincipales$precio, p=0.6, list = FALSE)
nuevoTrainSet <- compPrincipales[filas,] # 60% de datos para entrenamiento
nuevoTestSet <- compPrincipales[-filas,] # 40% de datos para prueba


#-------------------------------------------------
# KNN
#-------------------------------------------------

#Se necesitan instalar los packages class y e1071
#Se ponen los train y test sets en c(2:6) para no tomar encuenta las variables ventaPrecio y precio

predKNN <- knn(nuevoTrainSet[,c(2:6)],nuevoTestSet[,c(2:6)],
               as.factor(nuevoTrainSet$precio),k=37)

# Matriz de confusión
cfm <- confusionMatrix(as.factor(nuevoTestSet$precio),predKNN)
cfm


#-------------------------------------------------
# Validacion cruzada
#-------------------------------------------------

set.seed(123)
trctrl <- trainControl(method = "repeatedcv",
                       number = 10,
                       repeats = 3)

#nuevoTrainSet$ventaPrecio<-as.factor(nuevoTrainSet$ventaPrecio)
#nuevoTestSet$ventaPrecio<-as.factor(nuevoTestSet$ventaPrecio)

knnTrain2 <- train(ventaPrecio ~., data = nuevoTrainSet, method = "knn",
                  trControl = trctrl, preProcess = c("center", "scale"), tuneLength=10)

predknn<-predict(knnTrain2,newdata = nuevoTestSet)


summary(knnTrain2)
cfm2 <- confusionMatrix(as.factor(nuevoTestSet$precio),predKNN)
cfm2



