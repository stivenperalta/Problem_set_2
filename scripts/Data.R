############################## Problem Set 2 ###################################
# Autores: 
# fecha: 03/07/2023

# Preparación -------------------------------------------------------------

rm(list = ls()) # Limpiar Rstudio

require(pacman)
p_load(ggplot2, rio, tidyverse, skimr, caret, 
       rvest, magrittr, rstudioapi, stargazer, 
       boot, readxl, knitr, kableExtra, glmnet) # Cargar varios paquetes al tiempo


#Definir el directorio
path_script<-rstudioapi::getActiveDocumentContext()$path
path_folder<-dirname(path_script) 
setwd(path_folder)
getwd()

# Importing Data ----------------------------------------------------------

#training data
training<-read_csv("../stores/train.csv")
names(training) #vemos las variables disponibles
summary(training)

##Revisamos variables en la base inicial

#CITY
table(training$city) #revisamos que no hayan errores de entrada en esta variable, todas son Bogotá D.C

#PRICE
br = seq(3.000e+08,1.650e+09,by=50000000) # creamos los braquets/bins
ranges = paste(head(br,-1), br[-1], sep=" - ") #creamos los rangos en base a los braquets
freq   = hist(training$price, breaks=br, include.lowest=TRUE, plot=TRUE, 
              col="lightblue", border="grey",
              xlab="Precio en COP", ylab="Número de inmuebles",main="Distribución de Precios en Bogotá") #sacamos la frecuencia para cada uno de los braquets

data.frame(range = ranges, frequency = freq$counts) #tabla de frecuencia con bins

#MONTH YEAR- CREAMOS UNA VARIABLE UNIENDO MES Y AÑO
typeof(c(training$month, training$year))#revisamos que tipo son (double)




