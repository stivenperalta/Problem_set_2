############################## Problem Set 1 ###################################
# Autores: David Stiven Peralta M, Jazmine Roxana Galdos G
# Si encuentra alg?n error o tiene sugerencias por favor cont?cteme
# correo: ds.peralta@uniandes.edu.co
# fecha: 20/06/2023

# Preparación -------------------------------------------------------------

rm(list = ls()) # Limpiar Rstudio

options(scipen = 20,  digits=10)
require(pacman)
p_load(ggplot2, rio, tidyverse, skimr, caret, rvest, magrittr, rstudioapi, stargazer, boot, readxl, knitr, kableExtra) # Cargar varios paquetes al tiempo


#Definir el directorio
path_script<-rstudioapi::getActiveDocumentContext()$path
path_folder<-dirname(path_script) 
setwd(path_folder)
getwd()

# Import filtered data ----------------------------------------------------

GEIH <- read_excel("../stores/GEIH")
names(GEIH)
summary(GEIH)

# Question 3- Estimating the Age-wage profile profile--------


# Models ------------------------------------------------------------------

#Model: log(w) = β1 + β2Age + β3Age2 + u
reg_w_age<-lm(formula=log_salario_hora_imputado~edad+edad2, data=GEIH) #modelo general
reg_w_age_mujer<-lm(formula=log_salario_hora_imputado~edad+edad2, subset=mujer==1, data=GEIH) #modelo para mujeres con controles
reg_w_age_hombre<-lm(formula=log_salario_hora_imputado~edad+edad2, subset=mujer==0, data=GEIH) #modelo para hombres con controles

reg_w_age$AIC<-AIC(reg_w_age) #Akaike para modelo general
reg_w_age_mujer$AIC<-AIC(reg_w_age_mujer) #Akaike para modelo mujeres
reg_w_age_hombre$AIC<-AIC(reg_w_age_hombre) #Akaike para modelo hombres

#Con los tres modelos
stargazer(reg_w_age, reg_w_age_mujer, reg_w_age_hombre, type="text",title="Regresión Salario-Edad", keep=c("edad","edad2"),
          dep.var.labels="Ln(salario)",covariate.labels=c("Edad","Edad2"),omit.stat=c("ser","f","adj.rsq","aic"), out="../views/age_wage2.html",
          add.lines=list(c("AIC", round(AIC(reg_w_age),1), round(AIC(reg_w_age_mujer),1), round(AIC(reg_w_age_hombre),1)),c('Variables de Control', 'No','No','No')))

#Solo modelo principal
stargazer(reg_w_age,type="text",title="Tabla 3.1: Regresión Salario-Edad", keep=c("edad","edad2"),
          dep.var.labels="Ln(salario)",covariate.labels=c("Edad","Edad2"),omit.stat=c("ser","f","adj.rsq","aic"), out="../views/age_wage1.html")


# Coefficients ------------------------------------------------------------

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


# Age where salary is max -------------------------------------------------

#Cálculo edad donde se maximiza el salario
edad_max<- (-b2_w_age/(2*b3_w_age)) #modelo general
edad_max_mujer<- (-b2_w_age_mujer/(2*b3_w_age_mujer)) #modelo mujeres
edad_max_hombre<- (-b2_w_age_hombre/(2*b3_w_age_hombre)) #modelo hombres

resumen_edad_max <- format(data.frame(General=edad_max,
                    Mujeres=edad_max_mujer,
                    Hombres=edad_max_hombre), digits=3)

path1<-"../views/tabla_edades.html"
tabla_edades <- kable(resumen_edad_max, format = "html", align = "c", caption = "Edades pico en salario sin controles") %>%
                kable_classic(full_width = F, html_font = "Cambria")

writeLines(tabla_edades, path1)
tabla_edades

#Gráfica diferencia de ybarra y yhat
summ <- GEIH %>%  #agrupamos los datos por edad y se calcula el ybarra y ypredicho del modelo
  group_by(
    edad, edad2
  ) %>%  
  summarize(
    mean_y_edad = mean(log_salario_hora_imputado),
    yhat_reg_edad = mean(yhat), .groups="drop"
  ) 

  #Para Mujeres
summM <- GEIH %>%  #agrupamos los datos por edad y se calcula el ybarra y ypredicho del modelo
  filter(mujer==1) %>%
  group_by(edad, edad2) %>%  
  summarize(
    mean_y_edad = mean(log_salario_hora_imputado),
    yhat_reg_edad = mean(yhat), .groups="drop"
  ) 

  #Para Hombres
summH <- GEIH %>%  #agrupamos los datos por edad y se calcula el ybarra y ypredicho del modelo
  filter(mujer==0) %>%
  group_by(edad, edad2) %>%  
  summarize(
    mean_y_edad = mean(log_salario_hora_imputado),
    yhat_reg_edad = mean(yhat), .groups="drop"
  ) 

#Creamos la gráfica y la grabamos como un dato separado
grafico3_1<-ggplot(summ) + 
  geom_point(
    aes(x = edad, y = mean_y_edad),
    color = 4, size = 2
  ) + 
  geom_line(
    aes(x = edad, y = yhat_reg_edad), 
    color = 7, size = 1.5
  ) + 
  geom_ribbon(aes(ymin= lwr, ymax=upr), fill="gray", alpha=0.5
  ) +
  labs(
    title = "Gráfico 3.1: ln Salarios promedio por Edad",
    x = "Edad",
    y = "ln Salarios"
  ) +
  theme_bw()

grafico3_1 #para visualizarlo 

#Grabamos la gráfica
ggsave(
  file="../views/Pregunta_3_bondad_ajuste.jpg",
  plot = grafico3_1,
  scale = 1,
  width = 15,
  height = 10,
  units = "cm",
  dpi = 300,
)


# Bootstrap ---------------------------------------------------------------

#Standard errors usando bootstrap

#Función para Bootstrap
model_wage_age_fn<- function(data, index) {
                    f<- lm(formula=log_salario_hora_imputado~edad+edad2, data, subset=index)
                    
                    coefs<-f$coefficients
                    b2<-coefs[2]
                    b3<-coefs[3]
                    
                    edad_max_bt<-(-b2/(2*b3))
                    return(edad_max_bt)
}

model_wage_age_fn(GEIH,1:nrow(GEIH)) #para verificar que nos de el mismo peak age en el modelo general

set.seed(12345) #para que sea reproducible
err_est_wage_age<-boot(GEIH,model_wage_age_fn,R=1000)
plot(err_est_wage_age) #para ver la distribución de los resultados de boot

se<- apply(err_est_wage_age$t,2,sd)[1] #grabamos el valor del error estándar en el objeto se


# Confidence Intervals ----------------------------------------------------

#Intervalos de confianza
#Cálculos de intervalos de confianza
conf_int<-boot.ci(boot.out=err_est_wage_age, type=c("norm"), conf=0.95) #cálculo de los intervalos de confianza boot
conf_int

#Extraemos los valores inferiores y superiores del intervalo de confianza
summ$ic_sup<-conf_int$normal[3]
summ$ic_inf<-conf_int$normal[2]
ic_sup
ic_inf

#Graficas

grafica <- ggplot(GEIH, aes(x=edad, y=log_salario_hora_imputado)) +
  geom_point(col=4, size=0.2) +
  geom_line(data = summ, aes(x = edad, y = yhat_reg_edad), color = 7, size=1)+
  geom_errorbar(aes(ymin=ic_inf, ymax=ic_sup), width=0.2)+
  labs (x='Edad', y="ln Salario", title='Gráfico 3.2: Ln Salario por edad', subtitle='Hombres y Mujeres')

grafica

#Exportamos la gráfica
ggsave("../views/lnsalario_grafica.jpg", grafica, dpi = 300, width = 6, height = 4, units = "in")


# Salaries Women/Men ------------------------------------------------------

#Gráficas para hombres y mujeres

#sacamos los yhat para cada x para mujeres y hombres (por separado)
summ<-summ %>% mutate(yhat_mujer=b1_w_age_mujer+b2_w_age_mujer*edad+b3_w_age_mujer*edad^2, #yhat para mujeres
                      yhat_hombre=b1_w_age_hombre+b2_w_age_hombre*edad+b3_w_age_hombre*edad^2) #capturamos los residuales del salario

colours<-c("Mujeres"="red", "Hombres"="blue")

graficaMH <- ggplot() +
  geom_line(data = summ, aes(x = edad, y = yhat_mujer, color="Mujeres"), size=0.5)+
  geom_line(data = summ, aes(x = edad, y = yhat_hombre, color="Hombres"), size=0.5)+
  labs (x='Edad', y="ln Salario", color="Legend", title='Gráfico 4.1: Salario por edad sin controles')
graficaMH

#Exportamos la gráfica
ggsave("../views/lnsalario_mujer_vs_hombre.jpg", graficaMH, dpi = 300, width = 6, height = 4, units = "in")




