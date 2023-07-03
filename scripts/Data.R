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
training<-read_csv(../stores/)
