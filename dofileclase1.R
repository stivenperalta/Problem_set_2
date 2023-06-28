# Set up

# Limpiar mi ambiente
# clear all de Stata en R
rm(list = ls())

# Cargar paquetes
if (!require(pacman)) install.packages("pacman"); require(pacman)

#automatiza todo el proceso de instalación de paquetes
p_load(ggplot2, rio, tidyverse, skimr, caret)

# Definir el directorio

getwd()
dir()
setwd()

# Forma campeona
p_load(rstudioapi)
path_script <- rstudioapi::getActiveDocumentContext()$path
path_folder <- dirname(path_script)
setwd(path_folder)

# Load data -> revisar bien los otros trucos para cargar archivos

read.csv()

#cargar datos mas facil, import es de la libreria rio

db <- import ("https://github.com/ignaciomsarmiento/datasets/blob/main/GEIH_sample1.Rds?raw=true")

#viene del paquete base
str(db)

#viene del paquete dplyr
glimpse(db)

#base
summary(db)

#estadisticas descriptivas más pro

skim(db)

# Variables booleanas

# Exportar a Excel
p_load(xlsx)
write.xlsx(estadisticas_tbl, file = "estadisticas_tbl.xlsx")


#$
# Para llamar/mostrar los datos de una variable en específico
db$estrato1

db[,"estrato1"]
db[, 8]
db[c(1,5,10), "estrato1"]

# para mostrar las primeras observaciones

head(db, n=3)
tail(db)

# concatenacion
x <- c("a", "b", 3)

# La interrogacion es para pedir ayuda
?ggplot()

#graficas

names(db)

#Paso 1 : ploteo en el plano cartesiano

ggplot(data = db, mapping = aes(x = age, y = y_ingLab_m)) +
  geom_point()

ggplot(data = db, mapping = aes(x = age)) +
  geom_histogram()

ggplot(db, aes(x = age))
  geom_boxplot()





