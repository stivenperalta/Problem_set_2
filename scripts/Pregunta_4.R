# Preparación -------------------------------------------------------------

rm(list = ls()) # Limpiar Rstudio

options(scipen = 20,  digits=10)
require(pacman)
p_load(ggplot2, rio, tidyverse, skimr, caret, rvest, magrittr, rstudioapi, stargazer, boot, openxlsx, knitr, readxl, kableExtra) # Cargar varios paquetes al tiempo


#Definir el directorio
path_script<-rstudioapi::getActiveDocumentContext()$path
path_folder<-dirname(path_script) 
setwd(path_folder)
getwd()

# Import filtered data ----------------------------------------------------
GEIH <- read_excel("../stores/GEIH") #ajustar ruta store
summary(GEIH)
names(GEIH)

# Question 4: The gender earnings GAP -------------------------------------


# Model 1 -----------------------------------------------------------------
#Model1: log(w) = β1 + β2Female + u
GEIH<-GEIH[complete.cases(GEIH$tamaño_empresa),]
GEIH<-GEIH[complete.cases(GEIH$relacion_laboral),]

reg_w_fem<-lm(formula=log_salario_hora_imputado~mujer, data=GEIH) #modelo general
reg_w_fem$AIC<-AIC(reg_w_fem) #Akaike para modelo general

# Model 2: FWL ------------------------------------------------------------

#Modelo2: log(w) = β1 + β2Female + relacion_laboral + eduacion+ edad+ edad2 + tamaño empresa + u [usando FWL]

GEIH<-GEIH %>% mutate(muj_res=lm(mujer~edad+edad2+educacion_tiempo+as.factor(relacion_laboral)+as.factor(tamaño_empresa), GEIH)$residuals,#capturamos los residuales de mujer
                      sal_res=lm(log_salario_hora_imputado~edad+edad2+educacion_tiempo+as.factor(relacion_laboral)+as.factor(tamaño_empresa),GEIH)$residuals) #capturamos los residuales del salario

reg_fwl1<-lm(sal_res~muj_res,GEIH) #el coeficiente de mujer debería salir igual que si lo corremos como lm(log_salario_hora_imputado~mujer+edad+edad2+educacion_tiempo+as.factor(relacion_laboral)+as.factor(tamaño_empresa))

#probamos para ver si sale el mismo coeficiente
lm(log_salario_hora_imputado~mujer+edad+edad2+educacion_tiempo+as.factor(relacion_laboral)+as.factor(tamaño_empresa),GEIH)
reg_fwl1

# Results table -----------------------------------------------------------
#Con los dos modelos
stargazer(reg_w_fem, reg_fwl1, type="text",title="Tabla 4.1: Regresión Salario-Mujer", 
          dep.var.labels=c("Ln(salario)","Ln(salario)"),covariate.labels=c("Mujer","Mujer FWL"), omit.stat=c("ser","f","adj.rsq","aic"), 
          out="../views/age_fem.html", keep= c("mujer", "muj_res"),
          add.lines=list(c("AIC", round(AIC(reg_w_fem),1), round(AIC(reg_fwl1),1)),c('Variables de Control', 'No','Si')), 
          notes=c("El modelo FWL (2) ha sido calculado con las variable de control", 
          "edad, educación, ocupación y tamaño de la empresa."), notes.align="c")


# Model 3: Bootstrap and FWL ----------------------------------------------

#Utilizamos bootstrap para calcular los coeficientes y errores estándares
#Función para Bootstrap
model_fwl_boot<- function(data, index) {
  f<- lm(formula=sal_res~muj_res, data, subset=index)
  
  coefs<-f$coefficients
  b2<-coefs[2]
  
  return(b2)
}

set.seed(12345) #para que sea reproducible
model_fwl_boot(GEIH,1:nrow(GEIH)) #para verificar que nos de el mismo coeficiente de mujer

err_est_fwl_boot<-boot(GEIH,model_fwl_boot,R=1000)
err_est_fwl_boot


# Extracting Coefficients -------------------------------------------------
# Extraemos el coeficiente b2 y el error estándard
t1_stat <- err_est_fwl_boot$t0
se_t1 <- sd(err_est_fwl_boot$t)
fwl_boot<-c(t1_stat,se_t1)

#Extraemos el coeficientes de antiguas regresiones
t1_reg<-reg_w_fem$coef[2]
se_reg<-coef(summary(reg_w_fem))[, "Std. Error"][2]
reg_muj<-c(t1_reg,se_reg)

t1_fwl<-reg_fwl1$coef[2]
se_fwl<-coef(summary(reg_fwl1))[, "Std. Error"][2]
reg_fwl<-c(t1_fwl,se_fwl)  

tags<-c("Mujer","Std. Error")

#Juntamos todos los resultados de las regresiones
comparison.df<-format(data.frame(Modelo=tags,Regression=reg_muj,FWL=reg_fwl,FWL_BOOT=fwl_boot),digits=3)

# Exportar la tabla
ruta <- "../views/tabla_comp.html" 
tabla_comp <- kable(comparison.df, format = "html", align = "c", caption = "Tabla 4.2: Comparación de Modelos") %>%
  kable_classic(full_width = F, html_font = "Cambria")

writeLines(tabla_comp, ruta)
tabla_comp

# Peak salaries by gender with controls -----------------------------------

# Models ------------------------------------------------------------------

#Model: log(w) = β1 + β2Age + β3Age2 + u
reg_mh<-lm(formula=log_salario_hora_imputado~edad+edad2+educacion_tiempo+as.factor(relacion_laboral)+as.factor(tamaño_empresa), data=GEIH) #modelo general con controles
reg_m<-lm(formula=log_salario_hora_imputado~edad+edad2+educacion_tiempo+as.factor(relacion_laboral)+as.factor(tamaño_empresa), subset=mujer==1, data=GEIH) #modelo para mujeres con controles
reg_h<-lm(formula=log_salario_hora_imputado~edad+edad2+educacion_tiempo+as.factor(relacion_laboral)+as.factor(tamaño_empresa), subset=mujer==0, data=GEIH) #modelo para hombres con controles

reg_mh$AIC<-AIC(reg_mh) #Akaike para modelo general
reg_m$AIC<-AIC(reg_m) #Akaike para modelo mujeres
reg_h$AIC<-AIC(reg_h) #Akaike para modelo hombres

#Con los tres modelos
stargazer(reg_mh, reg_m, reg_h, type="text",title="Tabla 4.2: Regresión Salario-Edad por Género", keep=c("edad","edad2"),
          dep.var.labels="Ln(salario)",covariate.labels=c("Edad","Edad2"),omit.stat=c("ser","f","adj.rsq","aic"), out="../views/salario_ge.html",
          add.lines=list(c("AIC", round(AIC(reg_mh),1), round(AIC(reg_m),1), round(AIC(reg_h),1)),c('Variables de Control', 'Si','Si','Si')),
          notes=c("Las variable de control empleadas son edad, educación,", 
                  "ocupación y tamaño de la empresa."), notes.align="c")

# Coefficients ------------------------------------------------------------

#Coeficientes del modelo principal
coefs_mh<-reg_mh$coef
b1_mh<-coefs_mh[1]
b2_mh<-coefs_mh[2]
b3_mh<-coefs_mh[3]

#Coeficientes del modelo mujer
coefs_m<-reg_m$coef
b1_m<-coefs_m[1]
b2_m<-coefs_m[2]
b3_m<-coefs_m[3]

#Coeficientes del modelo hombre
coefs_h<-reg_h$coef
b1_h<-coefs_h[1]
b2_h<-coefs_h[2]
b3_h<-coefs_h[3]

# Age where salary is max -------------------------------------------------

#Cálculo edad donde se maximiza el salario
edad_mh<- (-b2_mh/(2*b3_mh)) #modelo general
edad_m<- (-b2_m/(2*b3_m)) #modelo mujeres
edad_h<- (-b2_h/(2*b3_h)) #modelo hombres

resumen_edad_cc <- format(data.frame(General=edad_mh,
                                     Mujeres=edad_m,
                                     Hombres=edad_h), digits=3)

path2<-"../views/tabla_edadescc.html"
tabla_edades_cc <- kable(resumen_edad_cc, format = "html", align = "c", caption = "Edades pico en salario con controles") %>%
  kable_classic(full_width = F, html_font = "Cambria")
writeLines(tabla_edades_cc, path2 )
tabla_edades_cc

#Función para Bootstrap
model_wa_MH_fn<- function(data, index, women) {
  subset_data<-data[data[['mujer']]==women,]
  f<- lm(log_salario_hora_imputado~edad+edad2+educacion_tiempo+as.factor(relacion_laboral)+as.factor(tamaño_empresa), 
         data=subset_data, 
         subset=index)
  
  coefs<-f$coefficients
  b2<-coefs[2]
  b3<-coefs[3]
  
  edad_max_bt<-(-b2/(2*b3))
  return(edad_max_bt)
}

model_wa_MH_fn(GEIH,1:nrow(GEIH),1) #para verificar que nos de el mismo peak age en el modelo general

#MUJERES
set.seed(12345) #para que sea reproducible
err_est_m<-boot(GEIH,model_wa_MH_fn,R=1000, women=1)
plot(err_est_m) #para ver la distribución de los resultados de boot

se_m<- apply(err_est_m$t,2,sd)[1] #grabamos el valor del error estándar en el objeto se
se_m

#HOMBRES
set.seed(12345) #para que sea reproducible
err_est_h<-boot(GEIH,model_wa_MH_fn,R=1000, women=0)
plot(err_est_h) #para ver la distribución de los resultados de boot

se_h<- apply(err_est_h$t,2,sd)[1] #grabamos el valor del error estándar en el objeto se
se_h

# Confidence Intervals ----------------------------------------------------

#MUJERES
#Intervalos de confianza
#Cálculos de intervalos de confianza
conf_int_m<-boot.ci(boot.out=err_est_m, type=c("norm"), conf=0.95) #cálculo de los intervalos de confianza boot
conf_int_m

#Extraemos los valores inferiores y superiores del intervalo de confianza
ic_supM<-conf_int_m$normal[3]
ic_infM<-conf_int_m$normal[2]
IC_Mujer=c(edad_m,ic_infM,ic_supM)
IC_Mujer

#HOMBRES
#Intervalos de confianza
#Cálculos de intervalos de confianza
conf_int_h<-boot.ci(boot.out=err_est_h, type=c("norm"), conf=0.95) #cálculo de los intervalos de confianza boot
conf_int_h

#Extraemos los valores inferiores y superiores del intervalo de confianza
ic_supH<-conf_int_h$normal[3]
ic_infH<-conf_int_h$normal[2]
IC_Homb=c(edad_h,ic_infH,ic_supH)
IC_Homb

#Juntamos los datos

resumen_MH <- format(data.frame(Mujeres=IC_Mujer,
                                Hombres=IC_Homb), digits=3)

path3<-"../views/tabla_edadescc.html"
tabla_MH<- kable(resumen_MH, format = "html", align = "c", caption = "Edades Max Salario IC") %>%
  kable_classic(full_width = F, html_font = "Cambria")

writeLines(tabla_MH, path2 )
tabla_MH



#Gráficas para hombres y mujeres con controles

#sacamos los yhat para cada x para mujeres y hombres (por separado)
gcc<-data.frame(Edad=18:99)

gcc<-gcc %>% mutate(yhat_mujer=b1_m+b2_m*Edad+b3_m*Edad^2, #yhat para mujeres
                    yhat_hombre=b1_h+b2_h*Edad+b3_h*Edad^2) #capturamos los residuales del salario

colours<-c("Mujeres"="red", "Hombres"="blue")

graficaMHCC <- ggplot() +
  geom_line(data = gcc, aes(x = Edad, y = yhat_mujer, color="Mujeres"), size=0.5)+
  geom_line(data = gcc, aes(x = Edad, y = yhat_hombre, color="Hombres"), size=0.5)+
  labs (x='Edad', y="ln Salario", color="Legend", title='Gráfico 4.2: Salario por edad con controles')
graficaMHCC

#Exportamos la gráfica
ggsave("../views/lnsalario_mujer_vs_hombreCC.jpg", graficaMHCC, dpi = 300, width = 6, height = 4, units = "in")





