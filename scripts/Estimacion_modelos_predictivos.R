############################## Problem Set 2 ###################################
# Autores: 
# fecha: 03/07/2023

# Preparación -------------------------------------------------------------

rm(list = ls()) # Limpiar Rstudio

pacman::p_load(ggplot2, rio, tidyverse, skimr, caret, 
               rvest, magrittr, rstudioapi, stargazer, 
               boot, readxl, knitr, kableExtra,
               glmnet, sf, tmaptools, leaflet,
               tokenizers, stopwords, SnowballC,
               stringi, dplyr, stringr, sp, hunspell,
               car, randomForest) # Cargar paquetes requeridos

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
data1 <- select(data, c(1, 3, 5, 9, 11, 15:29, 31, 33:68))
data2 <- select(data, c(1, 3, 5, 9, 11, 15:29, 31))
names(data2)
# Paso 2: División de los datos en conjuntos de entrenamiento y prueba
train_data <- data2 %>%
  filter(sample == "train") %>%
  select(c(2:5, 8:22)) %>%
  na.omit() %>%
  st_drop_geometry()

test_data<-data2  %>%
  filter(sample=="test") %>% 
  st_drop_geometry()
# Paso 3: Creación y evaouación del modelo Random Forest
# segundo modelo a evaluar
set.seed(201718234)
train_data <- na.omit(train_data)
tree <- train(
  price ~ .,
  data = train_data,
  method = "rpart",
  tuneLength = 10
)
# Paso 4: Entrenamiento del modelo

# Paso 5: Evaluación del modelo
test_data$price <- predict(tree, newdata = test_data)

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
write.csv(test_data1,"../stores/tree_2.csv",row.names=FALSE)
getwd()
glimpse(train_data)

#### Ajustar #############################





train_data <- data2 %>% 
  filter(sample == "train") %>% 
  select(price, bedrooms, property_type, parqueadero,
         gimnasio, vigilancia, area) %>% 
  na.omit()

test_data<-data2  %>% filter(sample=="test")  

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
