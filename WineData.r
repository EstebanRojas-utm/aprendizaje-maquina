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





#nuevo conjunto de datos discretizados
datawineDis1 <- datawine

#Columna de datos por amplitud
dataCol <- datawine$alcohol

#Discretizacion por amplitud
T<-5
maxCol <- max(dataCol)
minCol <- min(dataCol)
w <- (maxCol - minCol)
cat("Maximo: ", maxCol)
cat("Minimo: ", minCol)




























