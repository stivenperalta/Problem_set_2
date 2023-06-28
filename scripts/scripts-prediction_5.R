############################# problem set 1 ###################################
# Autores: Sergio Jimenez y Andrea Clavijo
# SI ENCUENTRA ALGUN ERROR O SUGERENCIA POR FAVOR CONTACTEME
# correo: sa.jimenezo1@uniandes.edu.co/ ay.clavijo@uniandes.edu.co
# fecha: 24/06/2023
###############################################################################

rm(list = ls()) # Limpiar Rstudio
options(scipen = 20,  digits=3) # establezco la notacion cientÃ­fica y el nÃºmero de decimales
require(pacman)
# Cargo varios paquetes al tiempo
p_load(ggplot2, rio, tidyverse, skimr, caret, rvest, magrittr, openxlsx,
       rstudioapi, readxl, openxlsx, stargazer, broom, knitr, htmlTable) 

#Cargo base preliminar de trabajo
GEIH <- read_excel("../stores/GEIH")
#pero antes vamos a convertir algunas variables a categoricas y crear variables para la especificaci?n de modelos con variable sno lineales:

GEIH$estrato <- factor(GEIH$estrato)
GEIH$relacion_laboral <- factor(GEIH$relacion_laboral)
GEIH$tamaño_empresa <- factor(GEIH$tamaño_empresa)
GEIH <- GEIH %>%
  filter(!is.na(relacion_laboral)) %>%
  mutate(educacion_tiempo2 = educacion_tiempo^2,
         edad3 = edad^3)%>%
  rename(mujer=sexo)

#ESTADISTICAS DESCRIPTIVAS
stargazer(data.frame(GEIH), header=FALSE, type='text',title="Variables en el Data Set")

###############################################################################
######################### PUNTO 5 - PREDICCION ################################

#generación de grupo train - test
#Definimos semilla para hacerlo reproducible
set.seed(777)

#usar 70% del dataset como entrenamiento y 30% como test
sample <- sample(c(TRUE, FALSE), nrow(GEIH), replace=TRUE, prob=c(0.7,0.3))
head(sample)

train  <- GEIH[sample, ] 
test   <- GEIH[!sample, ] 

# Se verifican las dimensiones de cada dataframe
dim(train)
dim(test)

###############################################################################
#   Especificación de modelos
##############################################################################

# especificación 1: salario ~ edad + edad2
model1<-lm(log_salario_hora_imputado~edad + edad2,data=train)
summary(model1)
test$model1<-predict(model1,newdata = test)
MSE_model1<-with(test,mean((log_salario_hora_imputado-model1)^2))
MSE_model1

# especificación 2: salario ~ mujer 
model2<-lm(log_salario_hora_imputado~mujer,data=train)
summary(model2)
test$model2<-predict(model2,newdata = test)
MSE_model2<-with(test,mean((log_salario_hora_imputado-model2)^2))
MSE_model2

# especificación 3: salario ~ mujer+edad2+educacion_tiempo+tamaño_empresa

model3<-lm(log_salario_hora_imputado ~ edad + edad2 + educacion_tiempo + tamaño_empresa,data=train)
summary(model3)
test$model3<-predict(model3,newdata = test)
MSE_model3<-with(test,mean((log_salario_hora_imputado-model3)^2))
MSE_model3

# especificación 4: salario ~ mujer + edad + edad2 + educacion_tiempo + (interaccion entre edad y genero)
model4<-lm(log_salario_hora_imputado ~ mujer + edad + edad2 + educacion_tiempo + mujer * edad,data=train)
summary(model4)
test$model4<-predict(model4,newdata = test)
MSE_model4<-with(test,mean((log_salario_hora_imputado-model4)^2))
MSE_model4


# especificacion 5: salario ~ mujer + edad + edad2 + educacion_tiempo  + estrato + (Interacción entre edad y género) 
model5<-lm(log_salario_hora_imputado~ mujer + edad + edad2 + educacion_tiempo  + estrato + edad * mujer,data=train)
summary(model5)
test$model5<-predict(model5,newdata = test)
MSE_model5<-with(test,mean((log_salario_hora_imputado-model5)^2))
MSE_model5

# especificacion 6: salario ~ mujer + edad + edad2 + educacion_tiempo + educacion_tiempo2 + estrato + (Interaccion entre edad y genero)
model6<-lm(log_salario_hora_imputado~ mujer + edad + edad2 + educacion_tiempo + educacion_tiempo2 + estrato + edad * mujer,data=train)
summary(model6)
test$model6<-predict(model6,newdata = test)
MSE_model6<-with(test,mean((log_salario_hora_imputado-model6)^2))
MSE_model6

# especificacion 7: salario ~ mujer + edad + edad2 + edad3 + educacion_tiempo + educacion_tiempo2 + estrato + (Interacción entre edad y genero)
model7<-lm(log_salario_hora_imputado~ mujer + edad + edad2 + edad3 + educacion_tiempo + educacion_tiempo2 + estrato + edad * mujer,data=train)
summary(model7)
test$model7<-predict(model7,newdata = test)
MSE_model7<-with(test,mean((log_salario_hora_imputado-model7)^2))
MSE_model7

#Se presentan las estimaciones de todos los modelos 
stargazer(model1, model2, model3, model4, model5, model6, model7, 
          title = "Modelos predictivos",
          summary = TRUE, 
          type = "text", 
          out = "../views/modelos_predictivos.html",
          omit.stat = c("f", "ser"),
          omit = c("Constant", "R^2", "Adj. R^2"),
          notes = c("Errores estándar en paréntesis")
)

#Los resultados de la predicción (MSE) de los modelos para los modelos previos y nuevos se condensan a continuación:
MSE_table<-c(MSE_model1, MSE_model2, MSE_model3, MSE_model4, MSE_model5,MSE_model6,MSE_model7)
x_label<-c('Modelo 1','Modelo 2', 'Modelo 3', 'Modelo 4', 'Modelo 5','Modelo 6','Modelo 7')
MSEtabla<-data.frame(Columna1 = x_label,Columna2 = MSE_table)
prediccion_errores <- data.frame(Columna1 = MSEtabla$Columna1, Columna2 = MSEtabla$Columna2)
colnames(prediccion_errores) <- c("Modelo", "MSE")
print(prediccion_errores)

stargazer(prediccion_errores, summary = FALSE, title= "Tabla 5.2 Errores de prediccion", type = "text", out= "../views/errores de prediccion.html")

#Ahora se grafican los MSE de cada uno de los modelos predichos para poder compararlos
MSEmodelos<-ggplot(data=MSEtabla, aes(x = x_label, y = MSE_table, group=1)) + 
    geom_line() +  
    geom_point() +
    ggtitle("MSE de los modelos especificados") +
    ylab("MSE") +
    xlab ("Número de modelo")
MSEmodelos

# Guardamos el gráfico en formato JPG
ggsave(filename = "../views/MSE de los modelos especificados.jpg", plot = MSEmodelos)

#Posteriormente, para tener idea de cuales modelos tienen el MSE mas bajo
ordenMSE <- prediccion_errores[order(prediccion_errores$MSE), ]
View(ordenMSE)

stargazer(ordenMSE, summary = FALSE, title= "Tabla 5.3 Modelos escogidos para validación cruzada", type = "text", out= "../views/ordenMSE.html")

#predecimos los errores para los modelos para realizar el gráfico de predicción de los errores
#e identificar posibles outliers
test$error_prediccion_model1 = test$log_salario_hora_imputado - test$model1
test$error_prediccion_model2 = test$log_salario_hora_imputado - test$model2
test$error_prediccion_model3 = test$log_salario_hora_imputado - test$model3
test$error_prediccion_model4 = test$log_salario_hora_imputado - test$model4
test$error_prediccion_model5 = test$log_salario_hora_imputado - test$model5
test$error_prediccion_model6 = test$log_salario_hora_imputado - test$model6
test$error_prediccion_model7 = test$log_salario_hora_imputado - test$model7

#graficamos la distribucion de los errores
distribucion_error <- ggplot(test) +
  geom_density(aes(x = error_prediccion_model1, color = "Modelo 1"), fill = NA, size = .5) +
  geom_density(aes(x = error_prediccion_model2, color = "Modelo 2"), fill = NA, size = .5) +
  geom_density(aes(x = error_prediccion_model3, color = "Modelo 3"), fill = NA, size = .5) +
  geom_density(aes(x = error_prediccion_model4, color = "Modelo 4"), fill = NA, size = .5) +
  geom_density(aes(x = error_prediccion_model5, color = "Modelo 5"), fill = NA, size = .5) +
  geom_density(aes(x = error_prediccion_model6, color = "Modelo 6"), fill = NA, size = .5) +
  geom_density(aes(x = error_prediccion_model7, color = "Modelo 7"), fill = NA, size = .5) +
  labs(title = "DistribuciÃ³n de Errores de PredicciÃ³n",
       x = "Error de PredicciÃ³n", y = "Densidad",
       color = "Modelo") +
  scale_color_manual(values = c("#5E4FA2", "#3288BD", "#66C2A5", "#ABDDA4", "#FEE08B", "#FDAE61", "#D53E4F"),
                     labels = c("Modelo 1", "Modelo 2", "Modelo 3", "Modelo 4", "Modelo 5", "Modelo 6", "Modelo 7"))

# Guardamos el gráfico en formato JPG
ggsave(filename = "../views/distribucion de errores de prediccion.jpg", plot = distribucion_error, width = 6, height = 3, dpi = 300)

#Con estos test validamos la existencia de valores atípicos en las colas de 
#la distribución del modelo 6 y modelo 7

cola_inferior6 <- test %>%
  filter(error_prediccion_model6 < quantile(error_prediccion_model6, 0.05))
cola_superior6 <- test %>%
  filter(error_prediccion_model6 < quantile(error_prediccion_model6, 0.05))

cola_inferior7 <- test %>%
  filter(error_prediccion_model6 < quantile(error_prediccion_model6, 0.05))
cola_superior7 <- test %>%
  filter(error_prediccion_model6 < quantile(error_prediccion_model6, 0.05))


stargazer(data.frame(GEIH), header=FALSE, type='text',title="Variables en el Data Set")
stargazer(data.frame(cola_inferior), header=FALSE, type='text',title="Variables en cola inferior")
stargazer(data.frame(cola_superior), header=FALSE, type='text',title="Variables en cola superior")

# De todos los modelos presentados, los que tienen un menor MSE son los modelos 6 y 7  Es decir, en donde se tiene un mejor performance en la predicción. 
# Sin embargo, los MSE son muy similares a los modelos lineales mas sencillos.


###############################################################################

### LOOCV para el modelo con mejor performance predictivo, es decir, model 7

control <- trainControl(method = "LOOCV")
modelo_LOOCV1 <- train(log_salario_hora_imputado ~ mujer + edad + edad2 + edad3 + educacion_tiempo + educacion_tiempo2 + estrato + edad * mujer, 
                       data = test, ## cambiar por base GEIH
                       method = "lm", 
                       trControl = control)

# Resumen del modelo 7
LOOCV1 <- summary(modelo_LOOCV1)

# Obtener los coeficientes y estadísticas del modelo
coeficientes_loocv <- coef(modelo_LOOCV1$finalModel)
estadisticas_loocv <- summary(modelo_LOOCV1$finalModel)$coefficients[, c("Estimate", "Std. Error", "t value", "Pr(>|t|)")]

# Crear un tibble con los resultados
Resultados_LOOCV <- as_tibble(estadisticas_loocv)
Resultados_LOOCV$Variable <- rownames(estadisticas_loocv)

# Cambiar los nombres y orden de las columnas
colnames(Resultados_LOOCV) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)", "Variable")
Resultados_LOOCV <- dplyr::select(Resultados_LOOCV, Variable, Estimate, `Std. Error`, `t value`, `Pr(>|t|)`)

#exportar resultados de regresion
stargazer(Resultados_LOOCV, summary = FALSE, type = "text", out= "../views/modelo_LOOCV1.html")

#calculamos el RMSE para comparar con los de la validación cruzada
RMSE_modelLOOCV<-modelo_LOOCV$results
RMSE_modelLOOCV<-RMSE_modelLOOCV$RMSE
RMSE_modelLOOCV<-mean(RMSE_modelLOOCV)

view(RMSE_modelLOOCV)

###############################################################################
# LOOCV para el segundo mejor modelo de performance predictivo, es decir, model 6
#install.packages("caret")
#library(caret)
#train_control <- trainControl(method = "LOOCV")
control <- trainControl(method = "LOOCV")
modelo_LOOCV2 <- train(log_salario_hora_imputado~ mujer + edad + edad2 + educacion_tiempo + educacion_tiempo2 + estrato + edad * mujer, 
                data = GEIH,
                method = "lm", 
                trControl = control)

#### Resumen del modelo 6
LOOCV2 <- summary(modelo_LOOCV2)

# Obtener los coeficientes y estadísticas del modelo
coeficientes_loocv2 <- coef(modelo_LOOCV2$finalModel)
estadisticas_loocv2 <- summary(modelo_LOOCV2$finalModel)$coefficients[, c("Estimate", "Std. Error", "t value", "Pr(>|t|)")]

# Crear un tibble con los resultados
Resultados_LOOCV2 <- as_tibble(estadisticas_loocv2)
Resultados_LOOCV2$Variable <- rownames(estadisticas_loocv2)

# Cambiar los nombres y orden de las columnas
colnames(Resultados_LOOCV2) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)", "Variable")
Resultados_LOOCV2 <- dplyr::select(Resultados_LOOCV2, Variable, Estimate, `Std. Error`, `t value`, `Pr(>|t|)`)

#exportar resultados de regresion
stargazer(Resultados_LOOCV2, summary = FALSE, type = "text", out= "../views/modelo_LOOCV1.html")

RMSE_modelLOOCV2<-modelo_LOOCV2$results
RMSE_modelLOOCV2<-RMSE_modelLOOCV2$RMSE
RMSE_modelLOOCV2<-mean(RMSE_modelLOOCV2)

view(RMSE_modelLOOCV2)



