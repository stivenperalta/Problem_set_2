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
               rattle, # graficar los árgoles
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


################## creación de modelos de predicción ##########################
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

# 4) Predicción de prueba con una constante ######################################
estimacion_prueba <- test_data %>% # modelo 8
  st_drop_geometry() %>% 
  select(property_id)
estimacion_prueba$price <- 700000000
head(estimacion_prueba)
write.csv(estimacion_prueba,"../stores/prueba.csv",row.names=FALSE)

# 5) modelos predictivos con Spatial data analysis ----------------------------
# 5.1) organización y limpieza de los datos para spatial data analysis --------
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
rm(map, pal)

# divido mi muestra en train y sample
train_data <- data2 %>%
  filter(sample == "train") %>%
  na.omit()

test_data<-data2  %>%
  filter(sample=="test")

#creo los folds para la validación cruzada por localidad (para crearlo, todavía no quito geometry)
location_folds <- spatial_leave_location_out_cv(
  train_data,
  group = LOCALIDAD
)
autoplot(location_folds) # VEO la validación cruzada por localidad

#creo los folds para la validación cruzada por barrio (para crearlo, todavía no quito geometry)
barrio_folds <- spatial_leave_location_out_cv(
  train_data,
  group = BARRIO
)
autoplot(barrio_folds) # VEO la validación cruzada por barrio

train <- train_data %>%
  st_drop_geometry() %>% 
  select(c(1, 4:5, 7:23, 26:46)) %>%  # c(1, 4:5, 7, 9, 11:13, 15:19, 21:23, 26:46) # omito "property_id" de las predicciones
  na.omit()

test <- test_data %>%
  st_drop_geometry() %>%
  select(c(1, 4:5, 7:23, 26:46)) # omito "property_id" de las predicciones

#Creo el folds por vc de localidad
foldslocalidad <- list()
length(location_folds$splits) # evalúo la extensión máxima de divisiones
for (i in 1:6) {
  foldslocalidad[[i]] <- location_folds$splits[[i]]$in_id
}
fitcontrol_localidad <- trainControl(method = "cv",
                            index = foldslocalidad)#creo CV

#Creo el folds por vc de barrio
foldsbarrio <- list()
length(barrio_folds$splits) # evalúo la extensión máxima de divisiones
for (i in 1:176) {
  foldsbarrio[[i]] <- barrio_folds$splits[[i]]$in_id
}
fitcontrol_barrio <- trainControl(method = "cv",
                            index = foldsbarrio)#creo CV

#creo los folds para la validación cruzada aleatoriamente
#set.seed(201718234)
#block_folds <- spatial_block_cv(train_data, v = 10)
#autoplot(block_folds)
#fitcontrol_random <- trainControl(method = "cv",
 #                                 index = block_folds)#creo CV
# 5.2) creación de modelos con spatial data analysis ---------------------------
#Creo el modelo 9 de predicción
modelo9EN <- train(
  price ~ .,
  data = train,
  method = "glmnet", 
  trControl = fitcontrol_localidad, # cv a emplear: fitcontrol_localidad, fitcontrol_barrio
  metric = "MAE",
  tuneGrid = expand.grid(alpha = seq(0.2, 0.5, length.out =7),
                         lambda = seq(28423157, 31423157, length.out =4)) # bestTune = alpha  0.55 lambda 31446558
)

round(modelo9EN$results$MAE[which.min(modelo9EN$results$lambda)],3) #Evalúo el error de predicción de ese lambda
modelo9EN$bestTune # evaluar el mejor alpha y lambda
plot(modelo9EN, xvar = "lambda") # Grafico el error MAE

#predigo el resultado en mi test
test_data$price <- predict(modelo9EN, newdata = test_data)
test7 <- test_data %>% #organizo el csv para poder cargarlo en kaggle
  st_drop_geometry() %>% 
  select(property_id,price) %>% 
  mutate(price = round(price / 10000000) * 10000000) # redondeo valores a múltiplos de 10 millones
head(test7) #evalúo que la base esté correctamente creada
write.csv(test7,"../stores/spatial_elastic_net_localidad.csv",row.names=FALSE) # Exporto la predicción para cargarla en Kaggle

# Creo el modelo 10 de predicción
modelo10EN <- train(
  price ~ .,
  data = train,
  method = "glmnet", 
  trControl = fitcontrol_barrio, 
  metric = "MAE",
  tuneGrid = expand.grid(alpha = seq(0.2, 0.4, length.out =7),
                         lambda = seq(3000000, 3150000, length.out =3)) # bestTune = alpha  0.15 lambda seq(3400000, 35000000, length.out =5)
)

round(modelo10EN$results$MAE[which.min(modelo10EN$results$lambda)],3) #Evalúo el error de predicción de ese lambda
modelo10EN$bestTune # evaluar el mejor alpha y lambda
plot(modelo10EN, xvar = "lambda") # Grafico el error MAE

#predigo el resultado en mi test
test_data$price <- predict(modelo10EN, newdata = test_data)
test8 <- test_data %>% #organizo el csv para poder cargarlo en kaggle
  st_drop_geometry() %>% 
  select(property_id,price) %>% 
  mutate(price = round(price / 10000000) * 10000000) # redondeo valores a múltiplos de 10 millones
head(test8) #evalúo que la base esté correctamente creada
write.csv(test8,"../stores/spatial_elastic_net_barrio.csv",row.names=FALSE) # Exporto la predicción para cargarla en Kaggle

# 5.3) Random Forest y boosting -----------------------------------------------------------
#creo la grilla
tunegrid_rf <- expand.grid(
  min.node.size = c(130, 140), # inicial c(3000, 6000, 9000, 12000)
  mtry = c(25, 26), #sqrt de variables #inicial c(6, 12, 18)
  splitrule = c("variance")
)

# Creo el modelo 11 de predicciónCreo con random forest
modelo11rf <- train(
  price ~ .,
  data = train,
  method = "ranger", 
  trControl = fitcontrol_localidad,
  maximize = F,
  metric = "MAE",
  tuneGrid = tunegrid_rf # bestTune = alpha  0.55 lambda 31446558
)

round(modelo11rf$results$MAE[which.min(modelo11rf$results$mtry)],3) #Evalúo el error de predicción de ese lambda
modelo11rf$bestTune # evaluar el mejor alpha y lambda
plot(modelo11rf, xvar = "lambda") # Grafico el error MAE
plot(modelo1rf) # observo gráficamente los resultados
fancyRpartPlot(modelo11rf$finalModel) # grafico el árbol (librería rattle)

#predigo el resultado en mi test
test_data$price <- predict(modelo11rf, newdata = test_data)
test9 <- test_data %>% #organizo el csv para poder cargarlo en kaggle
  st_drop_geometry() %>% 
  select(property_id,price) %>% 
  mutate(price = round(price / 10000000) * 10000000) # redondeo valores a múltiplos de 10 millones
head(test9) #evalúo que la base esté correctamente creada
write.csv(test9,"../stores/spatial_random_forest.csv",row.names=FALSE) # Exporto la predicción para cargarla en Kaggle

# Creo el modelo 12 de predicción con boosting

#creo la grilla
tunegrid_boosting <- expand.grid(
  mtry = c(23, 24, 25),
  splitrule = c("variance", "extratrees", "hellinger"),
  min.node.size = c(124, 128, 135)
)

modelo12boosting <- train(
  price ~ .,
  data = train,
  method = "ranger", 
  trControl = fitcontrol_localidad,
  maximize = F,
  metric = "MAE",
  tuneGrid = tunegrid_boosting # bestTune = alpha  0.55 lambda 31446558
)

round(modelo12boosting$results$MAE[which.min(modelo12boosting$results$MAE)],3) #Evalúo el error de predicción de ese lambda
modelo12boosting$bestTune # evaluar el mejor alpha y lambda
plot(modelo12boosting, xvar = "lambda") # Grafico el error MAE
plot(modelo12boosting) # observo gráficamente los resultados
fancyRpartPlot(modelo12boosting$finalModel) # grafico el árbol (librería rattle)

#predigo el resultado en mi test
test_data$price <- predict(modelo12boosting, newdata = test_data)
test10 <- test_data %>% #organizo el csv para poder cargarlo en kaggle
  st_drop_geometry() %>% 
  select(property_id,price) %>% 
  mutate(price = round(price / 10000000) * 10000000) # redondeo valores a múltiplos de 10 millones
head(test10) #evalúo que la base esté correctamente creada
write.csv(test10,"../stores/spatial_boosting.csv",row.names=FALSE) # Exporto la predicción para cargarla en Kaggle

# Evaluación y comparación de modelos -----------------------------------------
y_hat_price_boosting <- predict(modelo12boosting, test_data) # Predicción del precio con el modelo1
# métricas para evaluar
MAE(y_pred = test10, y_true = train$price) # se evalúa en la unidad de medida de price (y) es decir, en promedio, mi modelo se desacacha en x unidades de medida
mae(test10, train$price) # se evalúa en la unidad de medida de price (y) es decir, en promedio, mi modelo se desacacha en x unidades de medida
pacman::p_load("Metrics")
mean(train$price) #es bueno comparar el mae en función de la media de mi variable de interés
MAPE(y_pred = y_hat_price_boosting, y_true = train$price) # Hace lo mismo que mae pero en porcentaje

head(cbind(test7, test8, test9, test10))

################################ FIN ##########################################
# Resultados de estimaciones --------------------------------------------------
#M9
> round(modelo9EN$results$MAE[which.min(modelo9EN$results$lambda)],3) #Evalúo el error de predicción de ese lambda
[1] 219822596
> modelo9EN$bestTune # evaluar el mejor alpha y lambda
alpha   lambda
3   0.1 31423157

> round(modelo9EN$results$MAE[which.min(modelo9EN$results$lambda)],3) #Evalúo el error de predicción de ese lambda
[1] 210847666
> modelo9EN$bestTune # evaluar el mejor alpha y lambda
alpha   lambda
1   0.5 30423157

round(modelo9EN$results$MAE[which.min(modelo9EN$results$lambda)],3) #Evalúo el error de predicción de ese lambda
[1] 207172544
> modelo9EN$bestTune # evaluar el mejor alpha y lambda
alpha   lambda
1   0.2 28423157

#Sin incluir barrio
> round(modelo9EN$results$MAE[which.min(modelo9EN$results$lambda)],3) #Evalúo el error de predicción de ese lambda
[1] 205442049
> modelo9EN$bestTune # evaluar el mejor alpha y lambda
alpha   lambda
1   0.2 28423157

# M10
round(modelo10EN$results$MAE[which.min(modelo10EN$results$lambda)],3) #Evalúo el error de predicción de ese lambda
[1] 200524098
> modelo10EN$bestTune # evaluar el mejor alpha y lambda
alpha  lambda
5  0.55 3142316

round(modelo10EN$results$MAE[which.min(modelo10EN$results$lambda)],3) #Evalúo el error de predicción de ese lambda
[1] 198398341
> modelo10EN$bestTune # evaluar el mejor alpha y lambda
alpha  lambda
1   0.3 3100000

# M11 RF
> round(modelo11rf$results$MAE[which.min(modelo11rf$results$mtry)],3) #Evalúo el error de predicción de ese lambda
[1] 196304294
> modelo11rf$bestTune # evaluar el mejor alpha y lambda
mtry splitrule min.node.size
1   22  variance           100

round(modelo11rf$results$MAE[which.min(modelo11rf$results$mtry)],3) #Evalúo el error de predicción de ese lambda
[1] 203883800
> modelo11rf$bestTune # evaluar el mejor alpha y lambda
mtry splitrule min.node.size
9   22  variance           150

round(modelo11rf$results$MAE[which.min(modelo11rf$results$mtry)],3) #Evalúo el error de predicción de ese lambda
[1] 196743034
> modelo11rf$bestTune # evaluar el mejor alpha y lambda
mtry splitrule min.node.size
2   24  variance           140

round(modelo11rf$results$MAE[which.min(modelo11rf$results$mtry)],3) #Evalúo el error de predicción de ese lambda
[1] 196180645
> modelo11rf$bestTune # evaluar el mejor alpha y lambda
mtry splitrule min.node.size
2   25  variance           140

# M12 Boosting
round(modelo12boosting$results$MAE[which.min(modelo12boosting$results$MAE)],3) #Evalúo el error de predicción de ese lambda
[1] 195663965
> modelo12boosting$bestTune # evaluar el mejor alpha y lambda
mtry  splitrule min.node.size
3   25 extratrees           130

> round(modelo12boosting$results$MAE[which.min(modelo12boosting$results$MAE)],3) #Evalúo el error de predicción de ese lambda
[1] 195362746
> modelo12boosting$bestTune # evaluar el mejor alpha y lambda
mtry  splitrule min.node.size
24   25 extratrees           135