############################# problem set 2, predicciones ###################################
##Andrea Clavijo
# correo: ay.clavijo@uniandes.edu.co
# fecha: 14/07/2023
###############################################################################

# Limpiar Rstudio
rm(list = ls())

###Instalo paquetes
install.packages("pacman")
install.packages("mice")
library("mice")
library("pacman")
p_load("tidyverse", "sf", "naniar", "tidymodels", "readxl", "psych","ranger","glmnet","naniar","tidyverse", "caret", "glmnet", "ggplot2","ggraph","gt","vtable","spatialsample", "stargazer")

##Cargo la base de datos

db_cln <- readRDS("C:/Users/andye/OneDrive/Documentos/GitHub/Problem_set_2/stores/db_cln.rds")## Cargando la base da datos unificada
#Ajusto algunos nombres de variables e identifico posibles missing values

names(db_cln)
db_cln <- rename(db_cln, i_rinas = i_riñas)
db_cln$ESTRATO <- as.factor(db_cln$ESTRATO)

missing_values <- colSums(is.na(db_cln)) #sumo los NA's para cada variable
missing_table <- data.frame(Variable = names(missing_values), Missing_Values = missing_values) # lo reflejo en un data.frame
missing_table
rm(missing_table, missing_values)  

###Hago imputación de esos pocos datos que me dan missing values, siguiendo la recomendación de: 
###https://www.r-bloggers.com/2015/10/imputing-missing-data-with-r-mice-package/amp/
                                     
data_imp <- select(db_cln, EPE, i_rinas, i_narcoticos, i_orden,
                   i_maltrato, d_homicidios, d_lesiones, d_hurto_personas,
                   d_hurto_residencias, d_hurto_comercio, d_hurto_autos,
                   d_hurto_motos, d_hurto_bici, d_hurto_cel,d_sexual, d_violencia)  # Selecciono variables para imputar
data_imp$geometry <- NULL

mice_data <- mice(data_imp, m = 5, method = "pmm", seed = 201718234)# imputo con mice.impute.2lonly.pmm: Método de imputación para datos numéricos utilizando Predictive Mean Matching (PMM) con dos etapas (dos niveles).                                                                      

# Unifico valores imputados con valores de mi base maestra
data_imp_1 <- (data_imp)##creo una copia por si me sale error
data_imp_1 <- mice::complete(mice_data) # Una recomendación sería imputar sobre una base de copia para que, en caso de error, no tengan que correr todo el código nuevamente

vars_to_keep <- setdiff(names(db_cln), names(data_imp_1))
data_to_keep <- db_cln[vars_to_keep]
data_na <- cbind(data_to_keep, data_imp_1)

data1 <- data_na
##########################################################
############ Predicciones del precio   ###################
##########################################################

###Para empezar defino set de train y test para toda la base de datos

train_data <- data1 %>%
    filter(sample == "train") %>%
    select(c(1, 3, 11, 12, 15, 17:29, 31, 33, 35, 43, 49:50)) %>%
    na.omit() %>%
    st_drop_geometry()

test_data<-data1  %>%
  filter(sample=="test") %>% 
  select(c(1, 3, 11, 12, 15, 17:29, 31, 33, 35, 43, 49:50)) %>%
  st_drop_geometry()

train <- train_data %>% 
  select(-c(1)) # omito "property_id" de las predicciones

test <- test_data %>% 
  select(-c(1)) # omito "property_id" de las predicciones

#Validamos ubicacion

# columnas <- colnames(train)
# for (i in seq_along(columnas)) {
#         ubicacion <- i
#        titulo <- columnas[i]
#        mensaje <- paste("Columna", ubicacion, "-", titulo)
#         print(mensaje)
# }

#####Regresiones regularizadas

#1.2 Ridge

# # Matrix of predictores de predictores seleccionados
# y <- train_data$price # creo variable predicha
# x <- as.matrix(train_data)
# 
# #hacemos prediccion del modelo
# 
# Lasso <- glmnet(
#   x = x,
#   y = y,
#   alpha = 0 #ridge
# )
# 
# # observo los valores lambda evaluados por el modelo
# plot(ridge1, xvar = "lambda", xlim = c(17.5, 26.5))
# plot(ridge1, xvar = "dev", s = "lambda")
#    
# 
# # 
# ####Lasso

# Matrix of predictores de predictores seleccionados
y <- train$price # creo variable predicha
x <- as.matrix(train)

lasso1 <- glmnet(
  x = x,
  y = y,
  alpha = 1, #lasso
  lambda=.03
)

###Guardo los lambdas
lasso1$beta



plot(lasso1, xvar = "lambda")
set.seed(222)
cv_error_m2_Lass_fb <- cv.glmnet(
  x      = x_trainb,
  y      = y_trainb,
  alpha  = 1,
  nfolds = 10,
  type.measure = "mse",
  standardize  = TRUE
)
