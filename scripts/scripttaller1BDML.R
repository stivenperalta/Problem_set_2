library (pacman)
p_load(rvest, tidyverse)


table1 <- read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_1.html") %>%
  html_table()
head(table1)
as.data.frame(table1)

table2 <- read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_2.html") %>%
  html_table()
head(table2)
as.data.frame(table2)

table3 <- read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_3.html") %>%
  html_table()
head(table3)
as.data.frame(table3)

table4 <- read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_4.html") %>%
  html_table()
head(table4)
as.data.frame(table4)

table5 <- read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_5.html") %>%
  html_table()
head(table5)
as.data.frame(table5)

table6 <- read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_6.html") %>%
  html_table()
head(table6)
as.data.frame(table6)

table7 <- read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_7.html") %>%
  html_table()
head(table7)
as.data.frame(table7)

table8 <- read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_8.html") %>%
  html_table()
head(table8)
as.data.frame(table8)

table9 <- read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_9.html") %>%
  html_table()
head(table9)
as.data.frame(table9)

table10 <- read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_10.html") %>%
  html_table()
head(table10)
as.data.frame(table10)

GEIH <- bind_rows(table1, table2, table3, table4, table5, table6, table7, table8, table9, table10)

library(openxlsx)
write.xlsx(GEIH, "Datos GEIH.xlsx", )

#mostrar las primeras entradas, viene del paquete base
str(GEIH)

# viene del paquete dplyr
glimpse(GEIH)

#estadisticas descriptivas sin desviacion estandar
summary(GEIH)

#estadisticas descriptivas mas pros

p_load(rio,
       tidyverse,
       skimr,
       caret)
estadisticas <- skim(GEIH)
estadisticas_tbl <- as.data.frame(estadisticas)
p_load(stargazer)
stargazer(estadisticas_tbl, type = "latex", summary = F)
stargazer(GEIH, type = "latex", summary = T)

#Eliminar menores de 18 años
GEIHmod4 <- subset(GEIH, age>=18)

#Exportar BD a Excel
library(openxlsx)
write.xlsx(GEIHmod4, "Datos GEIHmod4.xlsx", )

#para contar cuantos missing hay
is.na(GEIHmod4$y_salary_m_hu) %>% table()

#Primero lo intentamos con todos los missing values a ver como da
#"Crear age2"#
GEIHmod4 = GEIHmod4 %>% 
  mutate(age2 = age*age)

#Crear log(w)#
GEIHmod4 = GEIHmod4 %>% 
  mutate(log_y_salary_m_hu = log(y_salary_m_hu))

library(openxlsx)
write.xlsx(GEIHmod4, "Datos GEIHmod5.xlsx", )


modelopunto3 <- lm(log_y_salary_m_hu~age + age2, data = GEIHmod4)

summary(modelopunto3)

ggplot(data = GEIHmod4 , mapping = aes(x = B0+ B1_model_p3*age+ B2_model_p3*age2, y = log_y_salary_m_hu)) +
  geom_point(col = "red" , size = 0.5)

B1_model_p3 <-6.031e-02
B2_model_p3 <- -6.550e-04 

#Calcular la edad que maximiza la funcion de salario#

peak_age2 <- -B1_model_p3/(2*B2_model_p3)

B0 <- 7.388e+00


#Ahora hay que intentarlo sin los missing values a ver cómo difieren los resultados

GEIHmod4_no_miss = GEIHmod4 %>% dplyr::filter(is.na(y_salary_m_hu)==F)

library(openxlsx)
write.xlsx(GEIHmod4_no_miss, "Datos GEIHmod_no_miss.xlsx", )

modelopunto3_no_miss <- lm(log_y_salary_m_hu~age + age2, data = GEIHmod4_no_miss)

summary(modelopunto3_no_miss)

B0 <- 7.388e+00
B1_model_p3_no_miss <-6.031e-02
B2_model_p3_no_miss <- -6.550e-04

ggplot(data = GEIHmod4 , mapping = aes(x = B0+ B1_model_p3_no_miss*age+ B2_model_p3_no_miss*age2, y = log_y_salary_m_hu)) +
  geom_point(col = "red" , size = 0.5)

#Calcular la edad que maximiza la funcion de salario#

peak_age2 <- -B1_model_p3/(2*B2_model_p3)

#Bootstrap

p_load("boot")

#Definimos el bootstrap para la edad máxima#
eta_mod3_fn<-function(data,index){
  
  f<-lm(log_y_salary_m_hu~age + age2, GEIHmod4, subset = index)
  
  coefs<-f$coefficients
  
  b1<-coefs[2]
  b2<-coefs[3]
  
  peak_age_b<- -b1/(2*b2)
  
  
  return(peak_age_b)
}

#Vemos que la funcion eta_mod3_fn da igual a peak_age2=46,03

eta_mod3_fn(GEIHmod4,1:nrow(GEIHmod4))

#Cuando hacemos bootstrap con R=1000 da un error estandar de 0.84971, entre mas R entonces más alto el error estandar y menos sesgo
results2 <- boot(data = GEIHmod4, statistic = eta_mod3_fn, R = 1000)
results2

#Definimos el intervalo de confianza del 95%#
z <- 1.96
std_error <- 0.9261287

#Calcular los intervalos de confianza### Nos dan 44.21861<peak_age_3<47.84903

peak_age_3 <- 46.03382 
confid_int1_eta_mod3_fn <- peak_age_3 + (z*std_error)
confid_int2_eta_mod3_fn <- peak_age_3 - (z*std_error)

confid_int1_eta_mod3_fn
confid_int2_eta_mod3_fn

#Punto 4------------------------------------------------FWL###


#####Script definitivo##### A 3 am 24 jun.....
 





rm(list = ls()) # Limpiar Rstudio
options(scipen = 20,  digits=3) # establezco la notacion científica y el número de decimales
require(pacman)
p_load(ggplot2, rio, tidyverse, skimr, caret, rvest, magrittr, openxlsx,
       rstudioapi, readxl, openxlsx, stargazer) # Cargo varios paquetes al tiempo

# Determino el directorio de trabajo
escritorio <- rstudioapi::getActiveDocumentContext()$path #accedo a la ruta del archivo
carpeta <- dirname(escritorio) # extraigo el nombre de la carpeta donde está guardado el archivo
setwd(carpeta) # Establezco el directorio de trabajo como aquel donde está guardado el archivo
getwd() # verifico el directorio
rm(carpeta, escritorio) # Limpio el ambiente

tablas_html <- vector("list", 10) # creo una lista para guardar los enlaces
for (i in 1:10) {urls <- paste0("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_", i, ".html")
tablas_html[[i]] <- read_html(urls) %>% html_table()
} 

# Concateno el contenido de las 10 tablas
base_geih2018 <- bind_rows(tablas_html, .id = "numero_de_base")
as.data.frame(base_geih2018)
glimpse(base_geih2018)

# Exporto la base unida a Excel
write.xlsx(base_geih2018, "Base_unida")

base_geih2018 <- Base_unida

GEIH <- base_geih2018 %>%
  select("age", "sex", "maxEducLevel", "pea", "wap", "p6240", "relab",
         "sizeFirm", "dsi",	"estrato1", "formal", "p6050", "p6426",
         "totalHoursWorked", "y_bonificaciones_m", "y_especie_m",
         "y_gananciaIndep_m", "y_gananciaIndep_m_hu", "y_salary_m",
         "y_salary_m_hu", "y_vivienda_m", "directorio", "dominio","fex_c",
         "fex_dpto", "fweight", "depto", "clase", "secuencia_p", "orden",
         "p6210s1","p7040", "cotPension", "regSalud", "cuentaPropia", "impa",
         "isa", "impaes", "isaes", "ingtot", "hoursWorkUsual") %>%
  
  
  rename(edad ="age",
         sexo = "sex", #1=hombre
         educacion_alcanzada = "maxEducLevel", # omitimos nivel_educativo = "p6210" ya que maxEduclevel tiene la información de esta variable
         years_educacion = "p6210s1",
         emprendedor = "cuentaPropia",
         poblacion_economicamente_activa = "pea",
         poblacion_edad_trabajar = "wap", # Omitimos "pet" ya que todos los valores dan 1
         ocupacion = "p6240",
         relacion_laboral = "relab",
         tamaño_empresa = "sizeFirm", # maybe funciona
         desempleado = "dsi", # 1 = desempleado
         estrato = "estrato1",
         formal_informal = "formal", # 1 = formal
         parentesco_jhogar = "p6050",
         tiempo_trabajando = "p6426", # la variable está en meses
         t_horas_trabajadas = "totalHoursWorked",
         ingreso_mensual = "y_bonificaciones_m", # revisar estadísticas si son coherentes
         ingreso_mensual_especie = "y_especie_m",
         ingreso_mensual_independientes = "y_gananciaIndep_m", # revisar estadísticas si son coherentes
         ingreso_hora_independiente = "y_gananciaIndep_m_hu", # revisar estadísticas si son coherentes
         salario_nominal_mensual = "y_salary_m", # revisar estadísticas si son coherentes
         salario_real_hora = "y_salary_m_hu", # revisar estadísticas si son coherentes
         ingreso_hogarmes_nominal = "y_vivienda_m",# revisar estadísticas si son coherentes
         grado_alcanzado = "p6210s1",
         pension = "cotPension",
         salud = "regSalud",
         cuenta_propia = "cuentaPropia",
         horas_trabajadas_sem = "hoursWorkUsual") %>%
  
  mutate(parentesco_jhogar = if_else(parentesco_jhogar == 1, 1, 0), # Dummy para jefes de hogar=1
         urbano = if_else(clase == 1, 1, 0), # dummy para cabecera municipal =1
         segundo_empleo = ifelse(p7040 ==1,1,0), #dummy para segundo empleo = 1
         sexo = ((sexo*-1)+1), #dummy que toma valor 1 para mujeres (punto 2)
         edad2 = edad*edad, # creo edad al cuadrado
         log_salario_hora = log(salario_real_hora)) %>% # log salario por hora con la variable creada por ignacio # REVISAR AJUSTE DE LA VARIABLE
  
  select(-ingreso_mensual_especie, -salario_nominal_mensual, -p7040, -impa, 
         -isa, -impaes, -isaes, -clase) %>%
  
  filter (edad >= 18) # filtro por mayores de edad


var_interes <- GEIH [, c("log_salario_hora",
                         "salario_real_hora", "edad", "sexo", "grado_alcanzado",
                         "ocupacion", "formal_informal",  "parentesco_jhogar",
                         "desempleado", "horas_trabajadas_sem", 
                         "tamaño_empresa", "relacion_laboral", "edad2")] 

#Propuesta para incluir salario en los datos que no reportan
Salario1 <- ifelse(GEIH$salario_hora == 0 & GEIH$secuencia_p > 1, GEIH$ingreso_hogar_hora, GEIH$salario_hora) # Primero validar si la persona tenía más integrantes en el hogar y si es así, duplicar el valor del integrante que sí tiene valor de ingreso en los demás integrantes del hogar (aplicar en personas que tengan NA en el salario). ## Evaluar cuántos datos quedarían al final.
Salario2 <- ifelse(GEIH$salario_real_hora == 0 & GEIH$directorio > 1, GEIH$ingreso_hogar_hora, GEIH$salario_real_hora)

GEIH$Llave <- paste(GEIH$directorio, GEIH$secuencia_p )

skim(var_interes) # obtengo las estadísticas descriptivas de las principales variables de interés

# JUSTIFICACIÓN PARA QUITAR LOS NA DE LA MUESTRA
sum(GEIH$desempleado != 0 & is.na(GEIH$salario_real_hora))
sum(GEIH$desempleado == 0 & is.na(GEIH$salario_real_hora))
sum(GEIH$secuencia_p == 1 & is.na(GEIH$salario_real_hora))


### Ahora si aca empieza el punto 3###-----------------------------------------------

###media_salario_h <- mean (var_interes$log_salario_hora)
###view(media_salario_h)
###media_salario_h2 <- mean (var_interes$log_salario_hora, na.rm = TRUE)

modelo_punto_3 <- lm(log_salario_hora~edad + edad2, data = var_interes)
summary(modelo_punto_3)

stargazer(modelo_punto_3,type="text",title="Tabla X: Resultado de la Regresión Salario-Edad", keep=c("Intercept", "edad","edad2"), 
          dep.var.labels="Log(salario)",covariate.labels=c("Edad","Edad2"),omit.stat=c("ser","f","adj.rsq"), out="../views/age_wage1.html")

B1_model_p3 <- 7.3883526
B2_model_p3 <- 0.0603050
B3_model_p3 <- -0.0006550

#Calcular la edad que maximiza la funcion de salario#

peak_age2 <- -B2_model_p3/(2*B3_model_p3)

###Graficar en el punto máximo###

ggplot(data = var_interes , mapping = aes(x = B1_model_p3 + B2_model_p3*edad+ B3_model_p3*(edad^2), y = log_salario_hora)) +
  geom_point(col = "red" , size = 0.5)

###Terminar punto 3 ####

p_load("boot")

#Definimos el bootstrap para la edad máxima#
eta_punto3_fn3<-function(data,index){
  
  f<-lm(log_salario_hora~edad + edad2, var_interes, subset = index)
  
  coefs<-f$coefficients
  
  b2<-coefs[2]
  b3<-coefs[3]
  
  peak_age_b<- -b2/(2*b3)
  
  
  return(peak_age_b)
}

eta_punto3_fn3(var_interes,1:nrow(var_interes))

p_load("boot")
#Cuando hacemos bootstrap con R=1000 da un error estandar de 0.84971, entre mas R entonces más alto el error estandar y menos sesgo
results2 <- boot(data = var_interes, statistic = eta_punto3_fn3, R = 1000)
results2

### Da un error estandar de 0,836###

#Ahora definimos el intervalo de confianza del 95%#
z <- 1.96
std_error_boots<- 0.836

#Calcular los intervalos de confianza### Nos dan 44.21861<peak_age_3<47.84903

peak_age_3 <- 46
confid_int1_eta_punto3_fn3 <- peak_age_3 + (z*std_error_boots)
confid_int2_eta_punto3_fn3 <- peak_age_3 - (z*std_error_boots)

confid_int1_eta_punto3_fn3
confid_int2_eta_punto3_fn3


###Punto 4#####

sexo

var_interes$Obrero_o_empl_gob=if_else(var_interes$relacion_laboral == 2, 1, 0)
var_interes$Empl_dom=if_else(var_interes$relacion_laboral == 3, 1, 0)
var_interes$Cuenta_prop=if_else(var_interes$relacion_laboral == 4, 1, 0)
var_interes$Patron_emple=if_else(var_interes$relacion_laboral == 5, 1, 0)
var_interes$Trab_fam=if_else(var_interes$relacion_laboral == 6, 1, 0)
var_interes$Trab_sin_r_otr_hog=if_else(var_interes$relacion_laboral == 7, 1, 0)
var_interes$Jorn_peon=if_else(var_interes$relacion_laboral == 8, 1, 0)
var_interes$Otro_empl=if_else(var_interes$relacion_laboral == 9, 1, 0)



var_interes$Gran_empr=if_else(var_interes$tamaño_empresa > 8, 1, 0)
var_interes$Med_empr=if_else(var_interes$tamaño_empresa < 6, 1, 0)


modelo1_punto_4_no_ctrl <- lm(log_salario_hora~sexo + grado_alcanzado + edad + edad2 + horas_trabajadas_sem +  Obrero_o_empl_gob+ Empl_dom+ Cuenta_prop+ Patron_emple+ Trab_fam+ Trab_sin_r_otr_hog+ Jorn_peon+ Otro_empl+ Gran_empr+ Med_empr, data = var_interes)
modelo_punto_4_ctrl <- lm(log_salario_hora~sexo , data = var_interes)


### Tabla con los requerimientos del profesor ###
stargazer(modelo_punto_4_ctrl, modelo1_punto_4_no_ctrl, type="text",title="Tabla X: Resultado de la Regresión Salario-Género", keep=c("sexo"), 
          dep.var.labels="Log(salario)",covariate.labels=c("Mujer"),omit.stat=c("ser","f","adj.rsq"), add.lines=list(c('Variables de Control', 'No', 'Si')))
)

stargazer(modelo_punto_4_ctrl, modelo1_punto_4_no_ctrl, type="text",title="Tabla X: Resultado de la Regresión Salario-Género", keep=c("sexo"), 
          dep.var.labels="Log(salario)",covariate.labels=c("Mujer"), add.lines=list(c('Variables de Control', 'No', 'Si')))
)

summary(modelo1_punto_4_no_ctrl)


