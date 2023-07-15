############################## Problem Set 2 ###################################
# Autores: 
# fecha: 03/07/2023

# Preparación -------------------------------------------------------------

#rm(list = ls())  # Limpiar Rstudio excepto la base principal
rm(list = setdiff(ls(), "data"))

pacman::p_load(vtable, # estadísticas descriptivas
               sf, # manejo de data espacial
               spatialsample, # validación cruzada espacial
               ggplot2,
               ggspatial, # visualización espacial,
               ranger, # random forest
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

# Importing Data
data <- st_read("../stores/db_cln.geojson")
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

############ ESTIIMACIÓN DE MODELOS DE PREDICCIÓN ##############################
# selecciono un primer conjunto de variables de interés (numéricas o factor)####
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

# rm(list = setdiff(ls(), c("data", "fitcontrol"))
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

# cart 2 #############################
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
# Predicción del precio con el modelo
predictions <- predict(model, newdata = test_data)

# exporto la predicción a mi test_data
test_data$price <- predict(model, newdata = test_data)
head(test_data %>% 
       select(property_id, price))  

test <- test_data %>%
  st_drop_geometry() %>% 
  select(property_id,price)

#exporto la predicción para cargarla en Kaggle
tree1<-test  %>% select(property_id,pred_tree)
write.csv(test,"tree_1.csv",row.names=FALSE)
getwd()

# 2) elastic net, ridge y lasso ################################################
# ridge #########################################################################
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

# exporto la predicción a mi test_data
head(test_data %>% 
       select(property_id, price))  

test1 <- test_data %>%
  st_drop_geometry() %>% 
  select(property_id,price)

test1 <- test1 %>%
  mutate(price = round(price / 10000000) * 10000000)


#exporto la predicción para cargarla en Kaggle
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

# exporto la predicción a mi test_data
head(test_data %>% 
       select(property_id, price))  

test2 <- test_data %>%
  st_drop_geometry() %>% 
  select(property_id,price)

test2 <- test2 %>%
  mutate(price = round(price / 50000000) * 50000000)

# exporto la predicción para cargarla en Kaggle
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

# Evaluación del modelo
test_data$price <- predict(elastic_net, newdata = test_data)

# exporto la predicción a mi test_data
head(test_data %>% 
       select(property_id, price))  

test <- test_data %>%
  st_drop_geometry() %>% 
  select(property_id,price)

test <- test %>%
  mutate(price = round(price / 10000000) * 10000000)

head(test)

# exporto la predicción para cargarla en Kaggle
write.csv(test,"../stores/elastic_net.csv",row.names=FALSE)


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


















# tips de clase
#p_load(rattle)
#modelo$finalmodel
#fancyRpartplot(modelo$finalmodel)

pacman::p_load(ggdist)
ggsurvplot(location_folds)
























































correlaciones <- data2 %>%
  select(c(4:5, 7:23)) %>%
  na.omit() %>% 
  st_drop_geometry()

cornum <- cor(correlaciones)

library(corrplot)
corrplot(cor_matrix, method = "circle")


# Verificar si "price" está presente en el conjunto de datos
if ("price" %in% names(correlaciones)) {
  # Filtrar solo las variables correlacionadas con "price"
  cor_with_price <- cor(correlaciones[, "price", drop = FALSE], correlaciones)
  
  # Mostrar la tabla de correlaciones
  print(as.data.frame(cor_with_price))
} else {
  print("La variable 'price' no está presente en el conjunto de datos 'correlaciones'.")
}


cor_matrix <- cor(correlaciones[, -1])  # Calcula la matriz de correlación excluyendo la variable objetivo

cor_threshold <- 0.8  # Umbral de correlación para considerar como alta correlación

cor_high <- findCorrelation(cor_matrix, cutoff = cor_threshold)  # Identifica las variables con alta correlación

train_data_filtered <- select(correlaciones, -cor_high)








# maximizar procesamiento
pacman::p_load(parallel)
detectCores() # detecta los cores del computador
pacman::p_load(doParallel)
registerDoParallel(6) # X depende del total de cores de mi computador (recomendado el # de núcleos menos 2)
getDoParWorkers() # verifico el número de cores usados por R

# maximizar procesamiento opción 2
library(h2o)
h2o.init(nthreads = X)


#borrador
# Spatial data analysis ###################################################
names(data)
data1 <- select(data, c(35, 42, 1, 3, 9, 15, 17:29, 31, 33, 43:63, 65:68))
data1$ESTRATO <- as.factor(data1$ESTRATO)
data1$sample <- as.factor(data1$sample)
sumtable(data1, out = "return")
as.data.frame(table(data1$LOCALIDAD)) # observo el total de observaciones por localidad
aggregate(cbind(observaciones = sample) ~ LOCALIDAD + sample, data = data1, FUN = length) # evalúo el total de observaciones por localidad y sample

# filtro por localidades de mayor relevancia por observaciones y cercanía
#data2 <- data1 %>%
#  filter(ifelse(sample == "train" & LOCALIDAD %in% c("BARRIOS UNIDOS", "CANDELARIA", "CHAPINERO", "USAQUEN", "TEUSAQUILLO"), TRUE, sample=="test"))
#as.data.frame(table(data2$sample)) # compruebo que no se hayan eliminado valores de test
# NO PUEDO FILTRAR POR LOCALIDAD HASTA NO ASEGURAR QUE HAYAN 5 OBSERVACIONES EN TRAIN DE LAS LOCALIDADES QUE NO TIENEN DATOS DE TEST "ENGATIVA", "PUENTE ARANDA", "SANTA FE", "SUBA", "CANDELARIA"
data2 <- select(data1, c(1:6, 20:36, 39:46))

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

#creo los folds para la validación cruzada por barrio
location_folds <- spatial_leave_location_out_cv(
  data2,
  group = LOCALIDAD
)
autoplot(location_folds) 

# divido mi muestra en train y sample
train_data <- data2 %>%
  filter(sample == "train") %>%
  na.omit() %>%
  st_drop_geometry()

test_data<-data2  %>%
  filter(sample=="test") %>% 
  st_drop_geometry()

train <- train_data %>%
  select(c(4:5, 7:21)) %>%  # omito "property_id" de las predicciones
  na.omit()

test <- test_data %>% 
  select(c(4:5, 7:21)) # omito "property_id" de las predicciones

#Creo el folds
folds <- list()
length(block_folds$splits) # evalúo la extensión máxima de divisiones
for (i in 1:5) {
  folds[[i]] <- block_folds$splits[[i]]$in_id
}

#creo CV
fitcontrol1 <- trainControl(method = "cv",
                            index = folds)

train$LOCALIDAD <- as.factor(train$LOCALIDAD)
EN2 <- train(
  price ~ bedrooms+area+banos+i_maltrato+d_hurto_autos+i_riñas,
  data = train,
  method = "glmnet",
  metric = "MAE",
  trControl = fitcontrol1
)

#tuneGrid = expand.grid(alpha = seq(0.40, 0.550, length.out =5),
#                      lambda = seq(31445000, 31447000, length.out =2)) # bestTune = alpha  0.55 lambda 31446558

EN2$bestTune # evaluar el mejor alpha y lambda 
round(EN2$results$MAE[which.min(EN2$results$lambda)],3) #Evalúo el error de predicción de ese lambda

plot(EN2, xvar = "lambda") #Grafico el error MAE

test_data$price <- predict(EN2, newdata = test_data)

# exporto la predicción a mi test_data
head(test_data %>% 
       select(property_id, price))  

test2 <- test_data %>%
  st_drop_geometry() %>% 
  select(property_id,price)

test2 <- test2 %>%
  mutate(price = round(price / 50000000) * 50000000)

# exporto la predicción para cargarla en Kaggle
write.csv(test2,"../stores/EN.csv",row.names=FALSE)

head(test2)




# tips de clase
#p_load(rattle)
#modelo$finalmodel
#fancyRpartplot(modelo$finalmodel)

pacman::p_load(ggdist)
ggsurvplot(location_folds)