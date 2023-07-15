############################## Problem Set 2 ###################################
# Autores: David Peralta
# fecha: 15/07/2023

# Preparación de la script -----------------------------------------------------

#rm(list = ls())  # Limpiar Rstudio excepto la base principal
rm(list = setdiff(ls(), "data"))

pacman::p_load(vtable, # estadísticas descriptivas
               sf, # manejo de data espacial
               spatialsample, # validación cruzada espacial
               ggplot2,
               ggspatial, # visualización espacial,
               ranger, # random forest
               parallel, # conocer los cores de mi pc
               doParallel, # maximizar el procesamiento en r en función de los cores de mi pc
               rio, tidyverse, skimr, caret, 
               rvest, magrittr, rstudioapi, stargazer, 
               boot, readxl, knitr, kableExtra,
               glmnet, sf, tmaptools, leaflet,
               tokenizers, stopwords, SnowballC,
               stringi, dplyr, stringr, sp, hunspell,
               car, randomForest, rpart, mice, psych) # Cargar paquetes requeridos

#Definir el directorio
path_script<-rstudioapi::getActiveDocumentContext()$path
path_folder<-dirname(path_script) 
setwd(path_folder)
getwd()
rm(path_folder, path_script)

# maximizo el procesamiento de r
detectCores() # detecta los cores del computador
registerDoParallel(6) # 6 de 8 cores de mi computador para R
getDoParWorkers() # verifico el número de cores usados por R

# Importing Data----------------------------------------------------------------
#data <- st_read("../stores/db_cln.geojson")
names(data)
# Missing values
# Imputo valores de los missing values # Mayor información revisar: https://www.r-bloggers.com/2015/10/imputing-missing-data-with-r-mice-package/amp/
data <- data %>% # imputo con 0 estas variables dado que solo hay un missing value
  mutate(i_riñas = ifelse(is.na(i_riñas), 0, i_riñas),
         i_narcoticos = ifelse(is.na(i_narcoticos), 0, i_narcoticos),
         i_orden = ifelse(is.na(i_orden), 0, i_orden),
         i_maltrato = ifelse(is.na(i_maltrato), 0, i_maltrato),
         d_homicidios = ifelse(is.na(d_homicidios), 0, d_homicidios),
         d_lesiones = ifelse(is.na(d_lesiones), 0, d_lesiones),
         d_hurto_personas = ifelse(is.na(d_hurto_personas), 0, d_hurto_personas),
         d_hurto_residencias = ifelse(is.na(d_hurto_residencias), 0, d_hurto_residencias),
         d_hurto_comercio = ifelse(is.na(d_hurto_comercio), 0, d_hurto_comercio),
         d_hurto_autos = ifelse(is.na(d_hurto_autos), 0, d_hurto_autos),
         d_hurto_motos = ifelse(is.na(d_hurto_motos), 0, d_hurto_motos),
         d_hurto_bici = ifelse(is.na(d_hurto_bici), 0, d_hurto_bici),
         d_hurto_cel = ifelse(is.na(d_hurto_cel), 0, d_hurto_cel),
         d_sexual = ifelse(is.na(d_sexual), 0, d_sexual),
         d_violencia = ifelse(is.na(d_violencia), 0, d_violencia))
#identifico missing value de la variable barrio
subset(data, is.na(BARRIO)) # identifico la geolocalización del dato
data$BARRIO <- ifelse(is.na(data$BARRIO),"EL CHANCO I", data$BARRIO) #asigno el nombre del barrio

# Evalúo missing values ##
missing_values <- colSums(is.na(data)) #sumo los NA's para cada variable
missing_table <- data.frame(Variable = names(missing_values), Missing_Values = missing_values) # lo reflejo en un data.frame
missing_table
rm(missing_table, missing_values)

# creación de modelos de predicción--------------------------------------------
# 1) CART's -------------------------------------------------------------------
# selecciono un primer conjunto de variables de interés (numéricas o factor)
data1 <- select(data, c(1, 3, 9, 15:29, 31, 33, 42:63, 65:68))
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
  select(c(2:3, 5:38)) # omito "property_id" de las predicciones

test <- test_data %>% 
  select(c(2:3, 5:38)) # omito "property_id" de las predicciones

# 1) Estimo el primer modelo de árbol 
set.seed(201718234) # creo semilla para la reproducibilidad de los datos
fitcontrol <- trainControl(method = "cv", number = 10) # Creo función de validación cruzada para evaluar el mejor alpha
train <- na.omit(train) # verifico nuevamente que no hay NA's
modelo1tree <- train(
  price ~ .,
  data = train,
  method = "rpart2",
  metric = "MAE",
  trControl = fitcontrol,
  tuneLength = 20 # grilla obtenida cp = 0.002724926
)
modelo1tree # Evalúo el modelo

# Predicción del precio con el modelo1
y_hat_modelo1tree <- predict(modelo1tree, test_data)
# métricas para evaluar
MAE(y_pred = y_hat_modelo1tree, y_true = train$price) # se evalúa en la unidad de medida de price (y) es decir, en promedio, mi modelo se desacacha en x unidades de medida
mean(train$price) #es bueno comparar el mae en función de la media de mi variable de interés
MAPE(y_pred = y_hat_modelo1tree, y_true = train$price) # Hace lo mismo que mae pero en porcentaje

# Exporto la predicción en csv para cargar en Kaggle
test_data$price <- predict(modelo1tree, newdata = test_data)
test1 <- test_data %>% #organizo el csv para poder cargarlo en kaggle
  st_drop_geometry() %>% 
  select(property_id,price) %>% 
  mutate(price = round(price / 10000000) * 10000000) # redondeo valores a múltiplos de 10 millones
head(test) #evalúo que la base esté correctamente creada
write.csv(test,"../stores/tree_1.csv",row.names=FALSE) # Exporto la predicción para cargarla en Kaggle


# 2) Estimo el segundo modelo de árbol
train_data <- data1 %>% 
  filter(sample == "train") %>% 
  select(price, bedrooms, property_type, parqueadero,
         gimnasio, vigilancia, area) %>% # selecciono unas variables para la estimación
  st_drop_geometry() %>% 
  na.omit()

test_data<-data1  %>%
  filter(sample=="test") %>% 
  select(price, bedrooms, property_type, parqueadero,
         gimnasio, vigilancia, area) %>% # selecciono unas variables para la estimación
  st_drop_geometry()

# segundo modelo a evaluar
set.seed(201718234)
modelo2tree <- train(
  price ~ .,
  data = train_data,
  method = "rpart",
  trcontrol = fitcontrol,
  tuneLength = 100
)

# Predicción del precio con el modelo1
y_hat_modelo2tree <- predict(modelo2tree, test_data)
# métricas para evaluar dentro de muestra
MAE(y_pred = y_hat_modelo2tree, y_true = train$price) # se evalúa en la unidad de medida de price (y) es decir, en promedio, mi modelo se desacacha en x unidades de medida
mean(train$price) #es bueno comparar el mae en función de la media de mi variable de interés
MAPE(y_pred = y_hat_modelo2tree, y_true = train$price) # Hace lo mismo que mae pero en porcentaje

# Exporto la predicción en csv para cargar en Kaggle
test_data$price <- predict(modelo2tree, newdata = test_data)
test2 <- test_data %>% #organizo el csv para poder cargarlo en kaggle
  st_drop_geometry() %>% 
  select(property_id,price) %>% 
  mutate(price = round(price / 10000000) * 10000000) # redondeo valores a múltiplos de 10 millones
head(test2) #evalúo que la base esté correctamente creada
write.csv(test2,"../stores/tree_2.csv",row.names=FALSE) # Exporto la predicción para cargarla en Kaggle

# 2) ridge, lasso y elastic net ------------------------------------------------
# modelo 3 con ridge
# selecciono un primer conjunto de variables de interés (numéricas o factor)
rm(list = setdiff(ls), c("data", "data1"))
data1 <- select(data, c(1, 3, 9, 15:29, 31, 33, 42:63, 65:68))
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
  select(c(2:3, 5:38)) # omito "property_id" de las predicciones

test <- test_data %>% 
  select(c(2:3, 5:38)) # omito "property_id" de las predicciones

y <- train_data$price # creo variable predicha
x <- as.matrix(train)

# modelo 3 ridge sin selección de lambda
modelo1ridge <- glmnet(
  x = x,
  y = y,
  alpha = 0, #1=lasso 0=ridge
) # en un primer ejercicio no establezco un lambda

# observo los valores lambda evaluados por el modelo gráficamente
plot(modelo1ridge, xvar = "lambda")
plot(modelo1ridge, xvar = "dev", s = "lambda")
modelo1ridge$beta # evalúo los parámetros del modelo

# modelo 4 ridge con selección de lambda por validación cruzada
fitcontrol <- trainControl(method = "cv", number = 10) # establezco grilla de validación cruzada
modelo2ridge <- train(
  price ~ .,
  data = train,
  method = "glmnet",
  metric = "MAE",
  trControl = fitcontrol,
  tuneGrid = expand.grid(alpha = 0,
                         lambda = 14215000) # secuencia obtenida a partir de modelo2ridge$lambda y se obtuvo secuencia hasta obtener el lambda escogido seq(14200000, 14230000, 5000)
)

# grafico los resultados obtenidos
plot(
  modelo2ridge$results$lambda,
  modelo2ridge$results$MAE, 
  xlab = "lambda",
  ylab = "MAE"
)
modelo2ridge$bestTune # evalúo los parámetros del modelo

# Predicción del precio con el modelo 4
y_hat_modelo2ridge <- predict(modelo2ridge, test_data)
# métricas para evaluar
MAE(y_pred = y_hat_modelo2ridge, y_true = train$price) # se evalúa en la unidad de medida de price (y) es decir, en promedio, mi modelo se desacacha en x unidades de medida
mean(train$price) #es bueno comparar el mae en función de la media de mi variable de interés
MAPE(y_pred = y_hat_modelo2ridge, y_true = train$price) # Hace lo mismo que mae pero en porcentaje

# Exporto la predicción en csv para cargar en Kaggle
test_data$price <- predict(modelo2ridge, newdata = test_data)
test3 <- test_data %>% #organizo el csv para poder cargarlo en kaggle
  st_drop_geometry() %>% 
  select(property_id,price) %>% 
  mutate(price = round(price / 10000000) * 10000000) # redondeo valores a múltiplos de 10 millones
head(test3) #evalúo que la base esté correctamente creada
write.csv(test3,"../stores/ridge1.csv",row.names=FALSE) # Exporto la predicción para cargarla en Kaggle

# modelo 5 lasso sin selección de lambda
modelo1lasso <- glmnet(
  x = x,
  y = y,
  alpha = 1, #1=lasso 0=ridge
) # en un primer ejercicio no establezco un lambda

# observo los valores lambda evaluados por el modelo gráficamente
plot(modelo1lasso, xvar = "lambda")
plot(modelo1lasso, xvar = "dev", s = "lambda")
modelo1lasso$beta # evalúo los parámetros del modelo

# modelo 6 lasso con selección de lambda por validación cruzada
modelo2lasso <- train(
  price ~ .,
  data = train,
  method = "glmnet",
  metric = "MAE",
  trControl = fitcontrol,
  tuneGrid = expand.grid(alpha = 1,
                         lambda = 14200000) # secuencia obtenida a partir de modelo2lasso$lambda y se obtuvo secuencia hasta obtener el lambda escogido seq(14200000, 14230000, 5000)
)

# grafico los resultados obtenidos
plot(
  modelo2lasso$results$lambda,
  modelo2lasso$results$MAE, 
  xlab = "lambda",
  ylab = "MAE"
)
modelo2lasso$bestTune # evalúo los parámetros del modelo

# Predicción del precio con el modelo 6
y_hat_modelo2lasso <- predict(modelo2lasso, test_data)
# métricas para evaluar
MAE(y_pred = y_hat_modelo2lasso, y_true = train$price) # se evalúa en la unidad de medida de price (y) es decir, en promedio, mi modelo se desacacha en x unidades de medida
mean(train$price) #es bueno comparar el mae en función de la media de mi variable de interés
MAPE(y_pred = y_hat_modelo2lasso, y_true = train$price) # Hace lo mismo que mae pero en porcentaje

# Exporto la predicción en csv para cargar en Kaggle
test_data$price <- predict(modelo2lasso, newdata = test_data)
test4 <- test_data %>% #organizo el csv para poder cargarlo en kaggle
  st_drop_geometry() %>% 
  select(property_id,price) %>% 
  mutate(price = round(price / 10000000) * 10000000) # redondeo valores a múltiplos de 10 millones
head(test4) #evalúo que la base esté correctamente creada
write.csv(test4,"../stores/lasso1.csv",row.names=FALSE) # Exporto la predicción para cargarla en Kaggle

# modelo 7 con Elastic net y validación cruzaza
fitcontrol <- trainControl(method = "cv", number = 10) # establezco grilla de validación cruzada
tunegriden <- expand.grid(alpha = seq(0, 1, by = 0.1), lambda = seq(0, 1, by = 0.1)) # Especificar los parámetros de alpha y lambda para la elastic net

# Realizar la regresión elástica
elastic_net <- train(
  price ~ .,
  data = train_data,
  method = "glmnet",
  trControl = control,
  metric = "MAE",
  tuneGrid = tunegriden
)

#Evalúo lambdas, alphas y proporción ridge/lasso
elastic_net$finalModel$lambda
elastic_net$finalModel$alpha
elastic_net$finalModel$beta

# Exporto la predicción en csv para cargar en Kaggle
test_data$price <- predict(elastic_net, newdata = test_data)
test5 <- test_data %>% #organizo el csv para poder cargarlo en kaggle
  st_drop_geometry() %>% 
  select(property_id,price) %>% 
  mutate(price = round(price / 10000000) * 10000000) # redondeo valores a múltiplos de 10 millones
head(test5) #evalúo que la base esté correctamente creada
write.csv(test5,"../stores/elastic_net1.csv",row.names=FALSE) # Exporto la predicción para cargarla en Kaggle

#Predicción de prueba con una constante ######################################
estimacion_prueba <- test_data %>%
  st_drop_geometry() %>% 
  select(property_id)
estimacion_prueba$price <- 700000000
head(estimacion_prueba)
write.csv(estimacion_prueba,"../stores/prueba.csv",row.names=FALSE)

##### Spatial data analysis ###################################################
names(data)
data1 <- select(data, c(35, 42, 1, 3, 9, 15, 17:29, 31, 33, 43:63, 65:68))
data1$ESTRATO <- as.factor(data1$ESTRATO)
data1$sample <- as.factor(data1$sample)
sumtable(data1, out = "return")
as.data.frame(table(data1$LOCALIDAD)) # observo el total de observaciones por localidad
aggregate(cbind(observaciones = sample) ~ LOCALIDAD + sample, data = data1, FUN = length) # evalúo el total de observaciones por localidad y sample

# filtro por localidades de mayor relevancia por observaciones y cercanía
# primero me aseguro de que haya por lo menos una observaciones de cada localidad en train y test
set.seed(201718234)  # Establezco una semilla para reproducibilidad
data3 <- data.frame()  # Crear una nueva base de datos vacía para almacenar las observaciones seleccionadas
localidades_out <- c("ENGATIVA", "PUENTE ARANDA", "SANTA FE", "SUBA")
nueva_base <- data.frame() # creo el data frame para almacenas las observaciones

for (localidad in localidades_out) {
  observaciones_localidad <- subset(data1, LOCALIDAD == localidad)
  
  if (nrow(observaciones_localidad) >= 5) {
    observaciones_seleccionadas <- observaciones_localidad[sample(nrow(observaciones_localidad), 5), ]
    nueva_base <- rbind(nueva_base, observaciones_seleccionadas)
  } else {
    observaciones_seleccionadas <- observaciones_localidad
    nueva_base <- rbind(nueva_base, observaciones_seleccionadas)
  }
}
nueva_base$LOCALIDAD <- "OTROS"

# Selecciono las localidades de mayor relevancia
data4 <- data1 %>%
  filter(ifelse(sample == "train" & LOCALIDAD %in% c("BARRIOS UNIDOS", "CANDELARIA", "CHAPINERO", "USAQUEN", "TEUSAQUILLO"), TRUE, sample=="test"))
data4$LOCALIDAD[!data4$LOCALIDAD %in% c("BARRIOS UNIDOS", "CANDELARIA", "CHAPINERO", "USAQUEN", "TEUSAQUILLO")] <- "OTROS" #unifico el nombre de las observaciones de otras localidades con train y test

as.data.frame(table(data4$sample)) # compruebo que no se hayan eliminado valores de test

data5 <- rbind(data4, nueva_base) # Consolido las bases
#Compruebo que las bases estén correctamente especificadas
aggregate(cbind(observaciones = sample) ~ LOCALIDAD + sample, data = data5, FUN = length)
as.data.frame(table(data5$sample))

# convierto en factor variables dummie
dummies <- data5 %>%
  st_drop_geometry() %>% 
  select(7:19) %>% 
  mutate_all(as.factor)
data6 <- data5 %>%
  select(1:6, 20:46) 

data2 <- cbind(data6, dummies) # Consolido las bases
rm(list = setdiff(ls(), c("data", "data2"))) #limpio el ambiente
glimpse(data2)

# observo los datos visualmente
data2 <- st_transform(data2, 4326) # determino crs 4326 colombia
st_crs(data2) # verifico crs colombia
pal <- colorFactor(palette = "Dark2", domain = data2$sample) # creo la paleta de colores para pintar por barrio
map <- leaflet() %>% 
  addTiles() %>% 
  addCircleMarkers (data = data2, color = ~pal(sample))
map # observo visualmente el mapa

#creo los folds para la validación cruzada aleatoriamente
set.seed(201718234)
block_folds <- spatial_block_cv(data2, v = 5)
autoplot(block_folds)

# divido mi muestra en train y sample

train_data <- data2 %>%
  filter(sample == "train") %>%
  na.omit() %>%
  st_drop_geometry()

test_data<-data2  %>%
  filter(sample=="test") %>% 
  st_drop_geometry()

train <- train_data %>%
  select(c(1, 4:5, 7:23, 26:46)) %>%  # c(1, 4:5, 7, 9, 11:13, 15:19, 21:23, 26:46) # omito "property_id" de las predicciones
  na.omit()

test <- test_data %>% 
  select(c(1, 4:5, 7:23, 26:46)) # omito "property_id" de las predicciones

#creo los folds para la validación cruzada por localidad
location_folds <- spatial_leave_location_out_cv(
  train_data,
  group = LOCALIDAD
)
autoplot(location_folds) 


#Creo el folds
folds <- list()
length(location_folds$splits) # evalúo la extensión máxima de divisiones
for (i in 1:6) {
  folds[[i]] <- location_folds$splits[[i]]$in_id
}

#creo CV
fitcontrol1 <- trainControl(method = "cv",
                            index = folds)

#Creo el modelo de predicción
EN2 <- train(
  price ~ .,
  data = train,
  method = "glmnet", 
  trControl = fitcontrol1,
  metric = "MAE",
  tuneGrid = expand.grid(alpha = seq(0, 0.5, length.out =7),
                         lambda = seq(20000000, 31000000, length.out =3)) # bestTune = alpha  0.55 lambda 31446558
  
)

EN2$bestTune # evaluar el mejor alpha y lambda
round(EN2$results$MAE[which.min(EN2$results$lambda)],3) #Evalúo el error de predicción de ese lambda
plot(EN2, xvar = "lambda") # Grafico el error MAE

#predigo el resultado en mi test
test_data$price <- predict(EN2, newdata = test_data)

# exporto la predicción a mi test_data
head(test_data %>% 
       select(property_id, price))  

test3 <- test_data %>%
  st_drop_geometry() %>% 
  select(property_id,price)

test3 <- test3 %>%
  mutate(price = round(price / 50000000) * 50000000)

# exporto la predicción para cargarla en Kaggle
write.csv(test3,"../stores/EN2.csv",row.names=FALSE)

head(test3)

#Creo el modelo de predicción 3
EN3 <- train(
  price ~ .,
  data = train,
  method = "glmnet", 
  trControl = fitcontrol1,
  metric = "MAE",
  tuneGrid = expand.grid(alpha = seq(0.1, 0.7, length.out =10),
                         lambda = 34500000) # bestTune = alpha  0.15 lambda seq(3400000, 35000000, length.out =5)
  
)



EN3$bestTune # evaluar el mejor alpha y lambda
round(EN2$results$MAE[which.min(EN2$results$lambda)],3) #Evalúo el error de predicción de ese lambda

plot(EN3, xvar = "lambda") # Grafico el error MAE

test_data$price <- predict(EN3, newdata = test_data)

# exporto la predicción a mi test_data
head(test_data %>% 
       select(property_id, price))  

test4 <- test_data %>%
  st_drop_geometry() %>% 
  select(property_id,price)

test4 <- test4 %>%
  mutate(price = round(price / 50000000) * 50000000)

# exporto la predicción para cargarla en Kaggle
write.csv(test4,"../stores/EN3.csv",row.names=FALSE)

head(test4)




########### Random Forest #################################################
#creo la grilla
tunegrid_rf <- expand.grid(
  min.node.size = c(100, 500, 1000), # inicial c(3000, 6000, 9000, 12000)
  mtry = c(22, 26), #sqrt de variables #inicial c(6, 12, 18)
  splitrule = c("variance")
)

#Creo el modelo de predicción
modelo1rf <- train(
  price ~ .,
  data = train,
  method = "ranger", 
  trControl = fitcontrol1,
  maximize = F,
  metric = "MAE",
  tuneGrid = tunegrid_rf # bestTune = alpha  0.55 lambda 31446558
)

plot(modelo1rf) # observo gráficamente los resultados


modelo1rf$bestTune # evaluar el mejor alpha y lambda
round(modelo1rf$results$MAE[which.min(modelo1rf$results$lambda)],3) #Evalúo el error de predicción de ese lambda
plot(modelo1rf, xvar = "lambda") # Grafico el error MAE

#predigo el resultado en mi test
test_data$price <- predict(modelo1rf, newdata = test_data)

# exporto la predicción a mi test_data
head(test_data %>% 
       select(property_id, price))  

test3 <- test_data %>%
  st_drop_geometry() %>% 
  select(property_id,price)

test3 <- test3 %>%
  mutate(price = round(price / 50000000) * 50000000)

# exporto la predicción para cargarla en Kaggle
write.csv(test3,"../stores/EN2.csv",row.names=FALSE)

head(test3)



########### Bostosting #################################################
#creo la grilla
tunegrid_b <- expand.grid(
  learn.rate = c(0.1, 0.01, 0.001),
  ntrees = c(50, 300, 900, 5000),
  max_depth = 20,
  min_rows = 3000,
  col_sample_rate = 0.2
)

#Creo el modelo de predicción
modelo1rf <- train(
  price ~ .,
  data = train,
  method = "ranger", 
  trControl = fitcontrol1,
  maximize = F,
  metric = "MAE",
  tuneGrid = tunegrid_rf # bestTune = alpha  0.55 lambda 31446558
)

plot(modelo1rf) # observo gráficamente los resultados

# para graficar el arbol que sale
pacman::p_load(rattle)
modelo1rf$finalModel
fancyRpartPlot(modelo1rf$finalModel)

##### 
#evaluar las predicciones en muestra (in_sample)
in_sample <- predict(modelo, train)
# métricas para evaluar
MAE(y_pred = in_sample, y_true = train$price) # se evalúa en la unidad de medida de price (y) es decir, en promedio, mi modelo se desacacha en x unidades de medida
mean(train$price) #es bueno comparar el mae en función de la media de mi variable de interés
MAPE(y_pred = in_sample, y_true = train$price) # Hace lo mismo que mae pero en porcentaje
