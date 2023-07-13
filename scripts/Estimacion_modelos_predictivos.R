############################## Problem Set 2 ###################################
# Autores: 
# fecha: 03/07/2023

# Preparación -------------------------------------------------------------

rm(list = setdiff(ls(), "data")) # Limpiar Rstudio excepto la base principal

pacman::p_load(ggplot2, rio, tidyverse, skimr, caret, 
               rvest, magrittr, rstudioapi, stargazer, 
               boot, readxl, knitr, kableExtra,
               glmnet, sf, tmaptools, leaflet,
               tokenizers, stopwords, SnowballC,
               stringi, dplyr, stringr, sp, hunspell,
               car, randomForest, rpart) # Cargar paquetes requeridos

#Definir el directorio
path_script<-rstudioapi::getActiveDocumentContext()$path
path_folder<-dirname(path_script) 
setwd(path_folder)
getwd()
rm(path_folder, path_script)
# Importing Data ----------------------------------------------------------

data <- st_read("../stores/db_cln.geojson")
names(data)
# Predicción de modelos con random Forest

# selecciono un primer conjunto de variables de interés (numéricas o factor)
data1 <- select(data, c(1, 3, 5, 9, 11, 15, 17:29, 31, 33, 43:57, 65:68))
names(data1)

# División de los datos en conjuntos de entrenamiento y prueba
train_data <- data1 %>%
  filter(sample == "train") %>%
  select(c(1:5, 7:41)) %>%
  na.omit() %>%
  st_drop_geometry()

test_data<-data1  %>%
  filter(sample=="test") %>% 
  select(c(1:5, 7:41)) %>%
  st_drop_geometry()

train_data1 <- train_data %>% 
  select(c(2:39)) # omito "property_id" de las predicciones

test_data1 <- test_data %>% 
  select(c(2:39)) # omito "property_id" de las predicciones

############ ESTIIMACIÓN DE MODELOS DE PREDICCIÓN ############################

# 1) Random forest------------------
set.seed(201718234) # creo semilla

# Creo función de validación cruzada para evaluar el mejor alpha
fitcontrol <- trainControl(method = "cv", number = 10)

train_data1 <- na.omit(train_data1) # verifico nuevamente que no hay NA's
tree <- train(
  price ~ .,
  data = train_data1,
  method = "rpart2",
  metric = "MAE",
  trControl = fitcontrol,
  tuneLength = 20
)
# Evalúo cost complexity prunning
tree

# Evalúo con cp calculado
tree1 <- train(
  price ~ .,
  data = train_data1,
  method = "rpart2",
  metric = "MAE",
  cp = 0.002724926,
)
tree1

# Paso 5: Evaluación del modelo
test_data$price <- predict(tree1, newdata = test_data)


# Paso 6: exporto la predicción a mi test_data
head(test_data %>% 
       select(property_id, price))  

test_data1 <- test_data %>%
  st_drop_geometry() %>% 
  select(property_id,price)

test_data1 <- test_data1 %>%
  mutate(price = round(price / 1000000) * 1000000)

head(test_data1)  

glimpse(test_data1)

#Paso 7: exporto la predicción para cargarla en Kaggle
write.csv(test_data1,"../stores/tree_3.csv",row.names=FALSE)

#### Ajustar #############################





train_data <- data1 %>% 
  filter(sample == "train") %>% 
  select(price, bedrooms, property_type, parqueadero,
         gimnasio, vigilancia, area) %>% 
  na.omit()

test_data<-data1  %>% filter(sample=="test")  

# Paso 3: Creación del modelo Random Forest
model <- randomForest(price ~ bedrooms + property_type + parqueadero +
                        gimnasio + vigilancia + area, data = train_data)

# segundo modelo a evaluar
set.seed(201718234)
tree <- train(
  price ~ .,
  data = train_data,
  method = "rpart",
  trcontrol = fitcontrol,
  tuneLength = 100
)

# Paso 4: Entrenamiento del modelo

# Paso 5: Evaluación del modelo
predictions <- predict(model, newdata = test_data)

# Paso 6: exporto la predicción a mi test_data
test_data$price <- predict(model, newdata = test_data)
head(test_data %>% 
       select(property_id, price))  

test_data1 <- test_data %>%
  st_drop_geometry() %>% 
  select(property_id,price)

glimpse(test_data1)

#Paso 7: exporto la predicción para cargarla en Kaggle
tree1<-test_data1  %>% select(property_id,pred_tree)
write.csv(test_data1,"tree_1.csv",row.names=FALSE)
getwd()









############ Elastic net ####################################################
# Definir el control para el ajuste del modelo
control <- trainControl(method = "cv", number = 5)

# Especificar los parámetros de alpha y lambda para la regresión elástica
tuneGrid <- expand.grid(alpha = seq(0, 1, by = 0.1), lambda = seq(0, 1, by = 0.1))

# Realizar la regresión elástica
elastic_net <- train(
  price ~ .,
  data = train_data,
  method = "glmnet",
  trControl = control,
  metric = "MAE"
)
#Evalúo lambdas, alphas y proporción ridge/lasso
elastic_net$finalModel$lambda
elastic_net$finalModel$alpha
elastic_net$finalModel$beta

# revisar lo de definición de parámetros con tuneGrid = tuneGrid

# Paso 5: Evaluación del modelo
test_data$price <- predict(elastic_net, newdata = test_data)

# Paso 6: exporto la predicción a mi test_data
head(test_data %>% 
       select(property_id, price))  

test_data1 <- test_data %>%
  st_drop_geometry() %>% 
  select(property_id,price)

test_data1 <- test_data1 %>%
  mutate(price = round(price / 10000000) * 10000000)

head(test_data1)  

glimpse(test_data1)




#Paso 7: exporto la predicción para cargarla en Kaggle
write.csv(test_data1,"../stores/elastic_net.csv",row.names=FALSE)


























###############
# Ajustar modelo con diferentes valores de cp y evaluar rendimiento
cp_values <- seq(0.01, 0.5, by = 0.01)  # Valores de cp a probar
error_cv <- numeric(length(cp_values))  # Vector para almacenar los errores de validación cruzada

for (i in 1:length(cp_values)) {
  tree <- rpart(price ~ ., data = train_data, cp = cp_values[i])
  pruned_tree <- prune(tree, cp = cp_values[i])
  
  if (!is.null(pruned_tree$frame) && nrow(pruned_tree$frame) > 0) {
    cv_error <- deviance(pruned_tree)
    error_cv[i] <- cv_error
  } else {
    error_cv[i] <- NA
  }
}

# Encontrar el valor óptimo de cp
optimal_cp <- cp_values[which.min(error_cv)]
cat("El valor óptimo de cp es:", optimal_cp, "\n")

# Ajustar el modelo con el valor óptimo de cp
optimal_tree <- rpart(price ~ ., data = train_data, cp = optimal_cp)

# Evaluar el rendimiento del modelo en el conjunto de prueba
predictions <- predict(optimal_tree, newdata = test_data)
mse <- mean((test_data$price - predictions)^2)
cat("El error cuadrático medio en el conjunto de prueba es:", mse, "\n")



estimacion_prueba <- test_data %>%
  st_drop_geometry() %>% 
  select(property_id)
estimacion_prueba$price <- 700000000
head(estimacion_prueba)
write.csv(estimacion_prueba,"../stores/prueba.csv",row.names=FALSE)
