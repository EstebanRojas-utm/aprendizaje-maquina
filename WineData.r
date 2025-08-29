library(ggplot2)
library(dplyr)
library(tidyr)

setwd("C://Users//Alumnos//Documents//MIA-1//aprendizaje-maquina")
nameFile <- ".//wine.data"
datawine <- read.table(nameFile, header = FALSE, sep = ",")
summary(datawine)

View(datawine)
