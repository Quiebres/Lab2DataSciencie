

########################################
## Universidad del Valle de Guatemala ##
## Laboratorio 2: Data Science        ##
## Autores:                           ##
##    Mayra Silva                     ##
##    Odalis Reyes                    ##           
##    Ivan Maldonado                  ##
########################################



# Instalación de paquetes
install.packages("rela")
install.packages("psych")
install.packages("FactoMineR")
install.packages("corrplot")
install.packages("cluster")
install.packages("fpc")
install.packages("NbClust")
install.packages("factoextra")
install.packages("REdaS")
install.packages("arules")
install.packages("ggplot2")
install.packages("ggpubr")
install.packages("ggmap")



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


setwd("C:/Users/smayr/Documents/Tercer a?o/Semestre 6/Data Science/Laboratorio 2/Lab2DataSciencie")


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
labels <-  c("Sí", "No")
piepercent<- round(100*x/sum(x), 1) # Porcentaje
# Pie chart
pie(x, labels = piepercent, main = "Proporcion de casas con aire acondicionado",
    col = rainbow(length(x)))
legend("topright", c("Sí","No"), cex = 0.8,
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
pcaTrainCuan <- prcomp(trainCuan, scale = T)
pcaTrainCuan #Correr este código para observar las componentes de las componentes principales

trainCuanPCA <- PCA(trainCuan,ncp = 22)
summary(trainCuanPCA)
var<-get_pca_var(trainCuanPCA)
corrplot(var$cos2, is.corr = F)


# -------------- Apriori ------------------
reglas<-apriori(trainCual[,3:45], parameter = list(support = 0.50,
                                                   confidence = 0.60,
                                                   target = "rules"))

# rulesAP <- apriori(trainCuan, parameter = list(support = 0.5, condifence = 0.8, maxlen = 10, maxtime=5, target = "rules"))

#reglas m?s importantes
top10subRules <- head(reglas, n = 10, by = "confidence")
plot(top10subRules, method="graph", engine="htmlwidget")








# LABORATORIO 2

# ------------------- Particion de datos ----------------------



#Particionando los datos en conjunto de entrenamiento y prueba

set.seed(123)

porciento <- 60/100 #Porciento en el que se partirán los datos
muestra<-sample(1:nrow(trainCuan),porciento*nrow(trainCuan))#Muestra aleatoria de numeros de un vector

trainCuan <- na.omit(trainCuan)
trainSet<-trainCuan[muestra,] #Obtengo las filas de los elementos que estan en el sector de muestra
testSet<-trainCuan[-muestra,] #Obtengo las filas de los elementos que no están en el vector de muestra




#----------------------- Modelo de regresion lineal -------------------
#Generaci?n del modelo
m <- trainCuan[,c(3, 5, 8, 11,12, 15, 22, 25, 26, 36)]

modeloLinealSimple<-lm(data = m)
summary(modeloLinealSimple)


