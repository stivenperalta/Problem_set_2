############################## Problem Set 2 ###################################
# Autores: 
# fecha: 03/07/2023

# Preparación -------------------------------------------------------------

rm(list = ls())  # Limpiar Rstudio excepto la base principal

pacman::p_load(vtable, # estadísticas descriptivas
               sf, # manejo de data espacial
               spatialsample, # validación cruzada espacial
               ggplot2,
               ggspatial, # visualización espacial,
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

# Evalúo missing values #######################################################
missing_values <- colSums(is.na(data)) #sumo los NA's para cada variable
missing_table <- data.frame(Variable = names(missing_values), Missing_Values = missing_values) # lo reflejo en un data.frame
missing_table
rm(missing_table, missing_values)

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
aggregate(cbind(observaciones = sample) ~ sample + LOCALIDAD, data = data1, FUN = length) # evalúo el total de observaciones por localidad y sample

# filtro por localidades de mayor relevancia por observaciones y cercanía
data2 <- data1 %>%
  filter(ifelse(sample == "train" & LOCALIDAD %in% c("BARRIOS UNIDOS", "CANDELARIA", "CHAPINERO", "USAQUEN", "TEUSAQUILLO"), TRUE, sample=="test"))
as.data.frame(table(data2$sample)) # compruebo que no se hayan eliminado valores de test

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
  train,
  group = LOCALIDAD
)
autoplot(location_folds) 

# divido mi muestra en train y sample

train_data <- data2 %>%
  filter(sample == "train") %>%
  na.omit() %>%
  st_drop_geometry()

test_data<-data1  %>%
  filter(sample=="test") %>% 
  st_drop_geometry()

train <- train_data %>%
  select(c(1, 4:5, 7:38)) %>%  # omito "property_id" de las predicciones
  na.omit()

test <- test_data %>% 
  select(c(1, 4:5, 7:38)) # omito "property_id" de las predicciones

#Creo el folds
folds <- list()
length(location_folds$splits) # evalúo la extensión máxima de divisiones
for (i in 1:5) {
  folds[[i]] <- location_folds$splits[[i]]$in_id
}

#creo CV
fitcontrol1 <- trainControl(method = "cv",
                            index = folds)

EN2 <- train(
  price ~ .,
  data = train,
  method = "glmnet",
  metric = "MAE",
  trControl = fitcontrol1,
  tuneGrid = expand.grid(alpha = seq(0.40, 0.550, length.out =7),
                         lambda = seq(31445000, 31447000, length.out =3)) # bestTune = alpha  0.55 lambda 31446558
)

EN2$bestTune # evaluar el mejor alpha y lambda
round(EN2$results$MAE[which.min(EN2$results$lambda)],3) #Evalúo el error de predicción de ese lambda

plot(EN2, xvar = "lambda") # Grafico el error MAE

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
write.csv(test2,"../stores/lasso1.csv",row.names=FALSE)

head(test2)




# tips de clase
#p_load(rattle)
#modelo$finalmodel
#fancyRpartplot(modelo$finalmodel)

pacman::p_load(ggdist)
ggsurvplot(location_folds)







































































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