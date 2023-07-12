# Preparación -------------------------------------------------------------

rm(list = ls()) # Limpiar Rstudio

pacman::p_load(ggplot2, rio, tidyverse, skimr, caret, 
               rvest, magrittr, rstudioapi, stargazer, 
               boot, readxl, knitr, kableExtra,
               glmnet, sf, tmaptools, leaflet,
               tokenizers, stopwords, SnowballC,
               stringi, dplyr, stringr, sp, hunspell,
               car) # Cargar paquetes requeridos

#Definir el directorio
path_script<-rstudioapi::getActiveDocumentContext()$path
path_folder<-dirname(path_script) 
setwd(path_folder)
getwd()

# Importing Data ----------------------------------------------------------

#train data
db<-rbind(test,train) #juntamos ambas bases

names(db) #vemos las variables disponibles
summary(db)
# Evaluación de Outlier #######################################################


# Evalúo outliers de las variables continuas
var_outliers <- db[, c("price", "bedrooms", "banos", "area")]

# Establecer el diseño de la ventana de gráficos
par(mfrow = c(2, 2))  # Ajusta los valores de "filas" y "columnas" según tus necesidades

# Evalúo outliers de mis variables continuas con boxplot
for (variable in colnames(var_outliers)) {
  boxplot(var_outliers[[variable]], main = variable)
}

# Evalúo valores estadísticamente atípicos mediante prueba de significancia outlierTest
for (variable in colnames(var_outliers)) {
  formula <- paste(variable, "~ 1")
  lm_outliers <- lm(formula, data = var_outliers, na.action = na.omit)
  outlier_test <- outlierTest(lm_outliers)
  cat("Variable:", variable, "\n")
  summary(lm_outliers)
  print(outlier_test)
  cat("\n")
}


# analizo los outliers para evaluar la coherencia de las observaciones
db[c(17479, 24177, 10974, 12357, 22451, 27063, 30986, 16068, 34895, 16436), # seleccionar aquí los valores atípicos de la variable 1 (el número de la observación)
   c("price", "area", "bedrooms", "banos", "property_type","surface_total", "surface_covered", "sample")] # VARIABLE 1

#sacamos observaciones que no tienen coherencia
db <- db[-c(9408, 41744, 41728, 44194, 44195, 41591, 39557), ]
db<- db[-c(38403,47866, 27721,25878,29445),]

#reemplazando valores en base a texto

#Scatterplot de precios por area y tipo de vivienda (apartamento/casa) (para train)
ggplot(data = subset(db, sample == "train"), aes(x = price, y = area, color = property_type)) +
  geom_point(size = 2) +
  scale_color_manual(values = c("#00b6f1", "#d9bf0d")) +
  labs(x = "Precio", y = "Area", title = "Precios de Inmuebles por superficie")


# Imputación de valores a otras variables con k Nearest Neighbors (kNN) ########

# Evalúo variables con missing values para imputar
db$area<-ifelse(db$area==0,NA,db$area)
db$banos<-ifelse(db$banos==0,NA,db$banos)
db$`casa multifamiliar`<-ifelse(is.na(db$`casa multifamiliar`),0,db$`casa multifamiliar`)

missing_values <- colSums(is.na(db)) #sumo los NA's para cada variable
missing_table <- data.frame(Variable = names(missing_values), Missing_Values = missing_values) # lo reflejo en un data.frame
missing_table

# Creo método de imputación con el paquete mice para imputar las variables rooms Y bathrooms
install.packages("mice")
library(mice)

#Grabamos la base
saveRDS(db, file = "../stores/data1.rds")

db$tokens<-NULL
db$n2tokens<-NULL
db$n3tokens<-NULL

write.csv(db, file = "../stores/data1.csv")

# mice tiene varios métodos de imputación. Estos valores es recomendable ajustarlos a medida que se corren los modelos para evaluar cuál presenta la mejor imputación.
# Este artículo siento que es de ayuda: https://www.r-bloggers.com/2015/10/imputing-missing-data-with-r-mice-package/amp/
db_subset <- select(db, area, banos)  # Selecciono variables para imputar
db_subset$geometry <- NULL
db_subset
mice_data <- mice(db_subset, m = 5, method = "pmm", seed = 201718234) # imputo con mice.impute.2lonly.pmm: Método de imputación para datos numéricos utilizando Predictive Mean Matching (PMM) con dos etapas (dos niveles).
#Con la opción methods(mice) pueden ver los métodos de imputación para seleccionar el más acorde
# Algunos de los más relevantes que vi (solo reemplazan "pmm" por el que escojan en method =):
# "cart" "lasso.logreg" "lasso.norm" "lasso.select.logreg" 
# "lasso.select.norm" "logreg.boot" "mpmm" "polr" "polyreg"


#Evalúo las imputaciones
mice_data$imp # si incluyo $variable solo vería los valores para una sola variable

# Unifico valores imputados con valores de mi base maestra
db[db_subset] <- complete(mice) # Una recomendación sería imputar sobre una base de copia para que, en caso de error, no tengan que correr todo el código nuevamente

glimpse(db) compruebo
#################### FIN ######################################################
