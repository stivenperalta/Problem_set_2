############################## Problem Set 1 ###################################
# Autores: David Stiven Peralta M, Jazmine Roxana Galdos G
# Si encuentra alg?n error o tiene sugerencias por favor cont?cteme
# correo: ds.peralta@uniandes.edu.co
# fecha: 20/06/2023

# Preparación -------------------------------------------------------------

rm(list = ls()) # Limpiar Rstudio

options(scipen = 20,  digits=10)
require(pacman)
p_load(ggplot2, rio, tidyverse, skimr, caret, rvest, magrittr, rstudioapi, stargazer, boot, openxlsx, knitr) # Cargar varios paquetes al tiempo


#Definir el directorio
path_script<-rstudioapi::getActiveDocumentContext()$path
path_folder<-dirname(path_script) 
setwd(path_folder)
getwd()

# Import filtered data ----------------------------------------------------

GEIH <- read_excel("../stores/GEIH")
summary(GEIH)
names(GEIH)
GEIH<-GEIH %>% rename (mujer="sexo")
GEIH<-GEIH[!is.na(GEIH$log_salario_hora),] #para poder correr todo el código

# Question 3- Estimating the Age-wage profile profile--------

#Model: log(w) = β1 + β2Age + β3Age2 + u
reg_w_age<-lm(formula=log_salario_hora~edad+edad2, data=GEIH, weights=fex_c) #modelo general con factor de expansión
reg_w_age_mujer<-lm(formula=log_salario_hora~edad+edad2, subset=mujer==1, data=GEIH, weights=fex_c) #modelo para mujeres con factor de expansión
reg_w_age_hombre<-lm(formula=log_salario_hora~edad+edad2, subset=mujer==0, data=GEIH, weights=fex_c) #modelo para hombres con factor de expansión

reg_w_age$AIC<-AIC(reg_w_age) #Akaike para modelo general
reg_w_age_mujer$AIC<-AIC(reg_w_age_mujer) #Akaike para modelo mujeres
reg_w_age_hombre$AIC<-AIC(reg_w_age_hombre) #Akaike para modelo hombres

#Con los tres modelos
stargazer(reg_w_age, reg_w_age_mujer, reg_w_age_hombre, type="text",title="Tabla 3.1: Regresión Salario-Edad", keep=c("edad","edad2"), 
          dep.var.labels="Log(salario)",covariate.labels=c("Edad","Edad2"),omit.stat=c("ser","f","adj.rsq","aic"), out="../views/age_wage2.html",
          add.lines=list(c("AIC", round(AIC(reg_w_age),1), round(AIC(reg_w_age_mujer),1), round(AIC(reg_w_age_hombre),1))))

#Solo modelo principal
stargazer(reg_w_age,type="text",title="Tabla 3.1: Regresión Salario-Edad", keep=c("edad","edad2"), 
          dep.var.labels="Log(salario)",covariate.labels=c("Edad","Edad2"),omit.stat=c("ser","f","adj.rsq","aic"), out="../views/age_wage1.html",
          add.lines=list(c("AIC", round(AIC(reg_w_age),1))))

#Coeficientes del modelo principal
coefs_w_age<-reg_w_age$coef
b1_w_age<-coefs_w_age[1]
b2_w_age<-coefs_w_age[2]
b3_w_age<-coefs_w_age[3]

edad_mean<-mean(GEIH$edad)
edad_mea2<-mean(GEIH$edad2)

#Coeficientes del modelo mujer
coefs_w_age_mujer<-reg_w_age_mujer$coef
b1_w_age_mujer<-coefs_w_age_mujer[1]
b2_w_age_mujer<-coefs_w_age_mujer[2]
b3_w_age_mujer<-coefs_w_age_mujer[3]

#Coeficientes del modelo hombre
coefs_w_age_hombre<-reg_w_age_hombre$coef
b1_w_age_hombre<-coefs_w_age_hombre[1]
b2_w_age_hombre<-coefs_w_age_hombre[2]
b3_w_age_hombre<-coefs_w_age_hombre[3]

#Predict yhat
GEIH$yhat<-predict(reg_w_age)
GEIH$yhat_mujer<-ifelse(GEIH$mujer==1, predict(reg_w_age_mujer),0)
GEIH$yhat_hombre<-ifelse(GEIH$mujer!=1, predict(reg_w_age_hombre),0)

#Cálculo edad donde se maximiza el salario
edad_max<- (-b2_w_age/(2*b3_w_age)) #modelo general
edad_max_mujer<- (-b2_w_age_mujer/(2*b3_w_age_mujer)) #modelo mujeres
edad_max_hombre<- (-b2_w_age_hombre/(2*b3_w_age_hombre)) #modelo hombres

resumen_edad_max <- data.frame(General=edad_max,
                    Mujeres=edad_max_mujer,
                    Hombres=edad_max_hombre)
knitr::kable(resumen_edad_max, format = "simple", caption="Edad en pico de sueldo")

#Standard errors usando bootstrap

#Función para Bootstrap
model_wage_age_fn<- function(data, index) {
                    f<- lm(formula=log_salario_hora~edad+edad2, data, subset=index)
                    
                    coefs<-f$coefficients
                    b2<-coefs[2]
                    b3<-coefs[3]
                    
                    edad_max_bt<-(-b2_w_age/(2*b3_w_age))
                    return(edad_max_bt)
}

model_wage_age_fn(GEIH,1:nrow(GEIH)) #para verificar que nos de el mismo peak age en el modelo general

err_est_wage_age<-boot(GEIH,model_wage_age_fn,R=1000)
err_est_wage_age

#Graficas
g1 <- ggplot(GEIH, aes(x=edad, y=predic2a)) + 
  geom_point(col = "red" , size = 1.5) +
  geom_smooth(method='lm', formula=y~x, se=FALSE, col='brown1') +
  theme_light()


# Question 4: The gender earnings GAP -------------------------------------

#Model1: log(w) = β1 + β2Female + u




