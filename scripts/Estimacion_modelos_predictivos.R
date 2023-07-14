############################## Problem Set 2 ###################################
# Autores: 
# fecha: 03/07/2023

# Preparación -------------------------------------------------------------

rm(list = setdiff(ls(), c("data", "mice_data")))  # Limpiar Rstudio excepto la base principal

pacman::p_load(ggplot2, rio, tidyverse, skimr, caret, 
               rvest, magrittr, rstudioapi, stargazer, 
               boot, readxl, knitr, kableExtra,
               glmnet, sf, tmaptools, leaflet,
               tokenizers, stopwords, SnowballC,
               stringi, dplyr, stringr, sp, hunspell,
               car, randomForest, rpart, mice) # Cargar paquetes requeridos

#Definir el directorio
path_script<-rstudioapi::getActiveDocumentContext()$path
path_folder<-dirname(path_script) 
setwd(path_folder)
getwd()
rm(path_folder, path_script)

# Importing Data
#data <- st_read("../stores/db_cln.geojson")
names(data)

# Evalúo missing values #######################################################
missing_values <- colSums(is.na(train)) #sumo los NA's para cada variable
missing_table <- data.frame(Variable = names(missing_values), Missing_Values = missing_values) # lo reflejo en un data.frame
missing_table
rm(missing_table, missing_values)

# Imputo valores de los missing values # Mayor información revisar: https://www.r-bloggers.com/2015/10/imputing-missing-data-with-r-mice-package/amp/
data_imp <- select(data, tegb, teat, EPE, BARRIO, i_riñas, i_narcoticos, i_orden,
                i_maltrato, d_homicidios, d_lesiones, d_hurto_personas,
                d_hurto_residencias, d_hurto_comercio, d_hurto_autos,
                d_hurto_motos, d_hurto_bici, d_hurto_cel,d_sexual, d_violencia)  # Selecciono variables para imputar
data_imp$geometry <- NULL

mice_data <- mice(data_imp, m = 5, method = "pmm", seed = 201718234) # imputo con mice.impute.2lonly.pmm: Método de imputación para datos numéricos utilizando Predictive Mean Matching (PMM) con dos etapas (dos niveles).

# Unifico valores imputados con valores de mi base maestra
data_imp <- mice::complete(mice_data) # Una recomendación sería imputar sobre una base de copia para que, en caso de error, no tengan que correr todo el código nuevamente

vars_to_keep <- setdiff(names(data), names(data_imp))
data_to_keep <- data[vars_to_keep]

data_na <- cbind(data_to_keep, data_imp)
names(data1)
rm(vars_to_keep,data_imp,data_to_keep)

# selecciono un primer conjunto de variables de interés (numéricas o factor)####
data1 <- select(data_na, c(1, 3, 5, 9, 15, 17:29, 31, 33, 46:49, 54:68))
names(data1)

# División de los datos en conjuntos de entrenamiento y prueba
train_data <- data1 %>%
  filter(sample == "train") %>%
  select(c(1:4, 6:39)) %>%
  na.omit() %>%
  st_drop_geometry()

test_data<-data1  %>%
  filter(sample=="test") %>% 
  select(c(1:4, 6:39)) %>%
  st_drop_geometry()

train <- train_data %>% 
  select(c(2:38)) # omito "property_id" de las predicciones

test <- test_data %>% 
  select(c(2:38)) # omito "property_id" de las predicciones

############ ESTIIMACIÓN DE MODELOS DE PREDICCIÓN ############################

# 1) CART's------------------
set.seed(201718234) # creo semilla

# Creo función de validación cruzada para evaluar el mejor alpha
fitcontrol <- trainControl(method = "cv", number = 10)

train <- na.omit(train) # verifico nuevamente que no hay NA's
tree <- train(
  price ~ .,
  data = train,
  method = "rpart2",
  metric = "MAE",
  trControl = fitcontrol,
  tuneLength = 20
)
tree # Evalúo cost complexity prunning

# estimo con grilla obtenida con cp
tree1 <- train(
  price ~ .,
  data = train,
  method = "rpart2",
  metric = "MAE",
  cp = 0.002724926,
)
tree1 # Evalúo con cp calculado

# Predicción del precio con el modelo
test_data$price <- predict(tree1, newdata = test_data)

test <- test_data %>%
  st_drop_geometry() %>% 
  select(property_id,price) %>% 
  mutate(price = round(price / 10000000) * 10000000) # redondeo valores a múltiplos de 10 millones

head(test) # evalúo que la base esté correctamente creada

# Exporto la predicción para cargarla en Kaggle
write.csv(test,"../stores/tree_3.csv",row.names=FALSE)

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

test <- test_data %>%
  st_drop_geometry() %>% 
  select(property_id,price)

glimpse(test)

#Paso 7: exporto la predicción para cargarla en Kaggle
tree1<-test  %>% select(property_id,pred_tree)
write.csv(test,"tree_1.csv",row.names=FALSE)
getwd()










# ridge ########################################################################
y <- train_data$price # creo variable predicha
x <- as.matrix(train)

ridge1 <- glmnet(
  x = x,
  y = y,
  alpha = 0, #1=lasso 0=ridge
) # en un primer ejercicio no establezco un lambda

# observo los valores lambda evaluados por el modelo
plot(ridge1, xvar = "lambda", xlim = c(17.5, 26.5))
plot(ridge1, xvar = "dev", s = "lambda")

ridge1$beta

# Selección de alpha
fitcontrol <- trainControl(method = "cv", number = 10) # establezco grilla de validación cruzada

ridge1_2 <- train(
  price ~ .,
  data = train,
  method = "glmnet",
  metric = "MAE",
  trControl = fitcontrol,
  tuneGrid = expand.grid(alpha = 0,
                         lambda = 14215000) # secuencia obtenida a partir de ridge1$lambda y se obtuvo secuencia hasta obtener el lambda escogido seq(14200000, 14230000, 5000)
)

# grafico los resultados obtenidos
plot(
  ridge1_2$results$lambda,
  ridge1_2$results$MAE, 
  xlab = "lambda",
  ylab = "MAE"
)

ridge1_2$bestTune

test_data$price <- predict(ridge1_2, newdata = test_data)

# Paso 6: exporto la predicción a mi test_data
head(test_data %>% 
       select(property_id, price))  

test1 <- test_data %>%
  st_drop_geometry() %>% 
  select(property_id,price)

test1 <- test1 %>%
  mutate(price = round(price / 10000000) * 10000000)


#Paso 7: exporto la predicción para cargarla en Kaggle
write.csv(test1,"../stores/ridge1.csv",row.names=FALSE)

# lasso ########################################################################
y <- train_data$price # creo variable predicha
x <- as.matrix(train)

lasso1 <- glmnet(
  x = x,
  y = y,
  alpha = 1, #1=lasso 0=ridge
) # en un primer ejercicio no establezco un lambda

# observo los valores lambda evaluados por el modelo
plot(lasso1, xvar = "lambda")
plot(lasso1, xvar = "dev", s = "lambda")

lasso1$beta

# Selección de alpha
fitcontrol <- trainControl(method = "cv", number = 10) # establezco grilla de validación cruzada

lasso1_2 <- train(
  price ~ .,
  data = train,
  method = "glmnet",
  metric = "MAE",
  trControl = fitcontrol,
  tuneGrid = expand.grid(alpha = 0,
                         lambda = 14200000) # secuencia obtenida a partir de lasso1$lambda y se obtuvo secuencia hasta obtener el lambda escogido seq(14200000, 14230000, 5000)
)


# grafico los resultados obtenidos
plot(
  lasso1_2$results$lambda,
  lasso1_2$results$MAE, 
  xlab = "lambda",
  ylab = "MAE"
)

lasso1_2$bestTune


test_data$price <- predict(lasso1_2, newdata = test_data)

# Paso 6: exporto la predicción a mi test_data
head(test_data %>% 
       select(property_id, price))  

test2 <- test_data %>%
  st_drop_geometry() %>% 
  select(property_id,price)

test2 <- test2 %>%
  mutate(price = round(price / 50000000) * 50000000)

#Paso 7: exporto la predicción para cargarla en Kaggle
write.csv(test2,"../stores/lasso1.csv",row.names=FALSE)

head(test2)

############ Elastic net ####################################################
fitcontrol <- trainControl(method = "cv", number = 10) # establezco grilla de validación cruzada

Elastic_net1 <- train(
  price ~ .,
  method = "glmnet",
  trControl = fitcontrol,
  
)

# Segundo modelo Definir el control para el ajuste del modelo
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

test <- test_data %>%
  st_drop_geometry() %>% 
  select(property_id,price)

test <- test %>%
  mutate(price = round(price / 10000000) * 10000000)

head(test)  

glimpse(test)




#Paso 7: exporto la predicción para cargarla en Kaggle
write.csv(test,"../stores/elastic_net.csv",row.names=FALSE)


























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
















#################### evaluación de variables numéricas #######################


#Submuestra de variables solo numéricas
datanum <- data1 %>% 
  select(c(2,4,20:40)) %>% 
  st_drop_geometry()


# Calcular la matriz de correlación
cor_matrix <- cor(datanum, use = "complete.obs")

# Extraer la columna de correlaciones con "price"
cor_with_price <- as.data.frame(cor_matrix[,"price"])

print(cor_with_price)

library(psych)

corPlot(datahigh, main = "Matriz de correlación")

#select only high corelations
CorHigh <- abs(cor_with_price) > 0.15

CorHigh 

#Submuestra de variables solo numéricas
datahigh <- datanum %>% 
  select(c(1:6, 8, 14,22)) %>% 
  st_drop_geometry()

# Calcular la matriz de correlación
cor_matrix <- cor(datahigh, use = "complete.obs")

# Extraer la columna de correlaciones con "price"
cor_with_price <- as.data.frame(cor_matrix[,"price"])
cor_with_price
