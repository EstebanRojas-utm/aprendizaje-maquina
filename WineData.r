library(ggplot2)
library(dplyr)
library(tidyr)

setwd("C://Users//Alumnos//Documents//MIA-1//aprendizaje-maquina")
nameFile <- ".//wine.data"
datawine <- read.table(nameFile, header = FALSE, sep = ",")
summary(datawine)

View(datawine)

#Nombres a las columnas de datos
namesWineD <- c("clase", "alcohol", "malic_ac", "ash", "alc", "mag", "fenol", "flav", "no_flav", "proan", "color", "hue", "ode", "proline")

names(datawine) <- namesWineD
head(datawine)

#description of data
summary(datawine)

#Convertir a datos categoricos
datawine$clase <- factor(datawine$clase)
summary(datawine)

#Normalizar media = 0, std=1
datawineNorm <- datawine
malc <- mean(datawine$alcohol)
sdalc <- sd(datawine$alcohol)

datawineNorm$alcohol <- (datawineNorm$alcohol - malc)/sdalc
summary(datawineNorm$alcohol)

#Otra forma (malic_ac)
malc <- mean(datawine$malic_ac)
sdalc <- sd(datawine$malic_ac)
datawineNorm <- datawineNorm %>% 
              mutate(malic_ac=(malic_ac-malc)/sdalc)
summary(datawineNorm)

#Normalizar ash
mash <- mean(datawine$ash)
sdash <- sd(datawine$ash)
datawineNorm <- datawineNorm %>% 
  mutate(ash=(ash-mash)/sdash)
summary(datawineNorm)

#Valores extremos
Q1 <- quantile(datawineNorm$alcohol, c(0.25), type = 6)
Q2 <- quantile(datawineNorm$alcohol, c(0.50), type = 6)
Q3 <- quantile(datawineNorm$alcohol, c(0.75), type = 6)
IQR <- Q3-Q1
liminf <- Q1 - IQR*1.5
limsup <- Q3 + IQR*1.5
valExtr <- datawineNorm$alcohol[datawineNorm$alcohol < liminf | datawineNorm$alcohol > limsup]
print(valExtr)

#Ejemplo extremos
datos <- sample(18:65, 100, replace = TRUE) #Genera datos
datos
datos <- c(datos, c(-13,9,96,150)) #Agregar datos extremos
datos

q1 <- quantile(datos, c(0.25), type = 6)
q2 <- quantile(datos, c(0.50), type = 6)
q3 <- quantile(datos, c(0.75), type = 6)
IQR <- q3-q1
liminf <- Q1 - IQR*1.5
limsup <- Q3 + IQR*1.5
valExtr <- datos[datos < liminf | datos > limsup]
print(valExtr)

#Numero  de registros en el conjunto de datos
N <- dim(datawine)[1]
print(N)
#Data wine con N.A
set.seed(11991) #set seed
datawineNA <- datawine
datawineNA$alcohol[rbinom(N, 1, 0.1) == 1] <- NA
datawineNA$fenol[rbinom(N, 1, 0.1) == 1] <- NA
summary(datawineNA)
View(datawineNA)

#Eliminando registros con NA
datawine1 <- datawineNA[rowSums(is.na(datawineNA)) == 0,]
summary(datawine1)
print(dim(datawine1))
View(datawine1)

#Imputacion con valor de la media
datawine2 <- datawineNA
meanAlch <- mean(datawine1$alcohol)
meanFenol <- mean(datawine1$fenol)
meanAlch <- mean(datawineNA$alcohol, na.rm = TRUE)
meanFenol <- mean(datawineNA$fenol, na.rm = TRUE)
datawine2$alcohol[is.na(datawineNA$alcohol)] <- meanAlch
datawine2$fenol[is.na(datawineNA$fenol)] <- meanFenol
summary(datawine2)
View(datawine2)

#Imputacion con valor de la media condicionada
datawine3 <- datawineNA
View(datawine3)
indClase1 <- which(datawine1$clase==1)
meanAlchC1 <- mean(datawine1$alcohol[datawine1$clase==1])
meanAlchC2 <- mean(datawine1$alcohol[datawine1$clase==2])
meanAlchC3 <- mean(datawine1$alcohol[datawine1$clase==3])
meanFenolC1 <- mean(datawine1$fenol[datawine1$clase==1])
meanFenolC2 <- mean(datawine1$fenol[datawine1$clase==2])
meanFenolC3 <- mean(datawine1$fenol[datawine1$clase==3])

datawine3$alcohol[is.na(datawineNA$alcohol) & datawineNA$clase==1] <- meanAlchC1
datawine3$alcohol[is.na(datawineNA$alcohol) & datawineNA$clase==2] <- meanAlchC2
datawine3$alcohol[is.na(datawineNA$alcohol) & datawineNA$clase==3] <- meanAlchC3

datawine3$fenol[is.na(datawineNA$fenol) & datawineNA$clase==1] <- meanFenolC1
datawine3$fenol[is.na(datawineNA$fenol) & datawineNA$clase==2] <- meanFenolC2
datawine3$fenol[is.na(datawineNA$fenol) & datawineNA$clase==3] <- meanFenolC3

summary(datawine3)
View(datawine3)

#discretizacion igual amplitud T = 3
T <- 3
datawine4 <- datawine
vectorDis <- datawine4$alcohol
minAlc <- min(vectorDis)
maxAlc <- max(vectorDis)
w <- (maxAlc -minAlc)/T
indiceInt1 <- vectorDis >= minAlc & vectorDis < minAlc + w
indiceInt2 <- vectorDis >= minAlc + w & vectorDis < minAlc + 2*w
indiceInt3 <- vectorDis >= minAlc + 2*w & vectorDis <= minAlc + 3*w
vectorDis[indiceInt1] <- 1
vectorDis[indiceInt2] <- 2
vectorDis[indiceInt3] <- 3
summary(vectorDis)
vectorDis <- factor(vectorDis)
summary(vectorDis)

























