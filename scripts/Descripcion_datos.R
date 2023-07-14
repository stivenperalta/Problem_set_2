rm(list = ls()) # Limpiar Rstudio

pacman::p_load(
  rio,          # File import
  here,         # File locator
  skimr,        # get overview of data
  tidyverse,    # data management + ggplot2 graphics 
  gtsummary,    # summary statistics and tests
  rstatix,      # summary statistics and statistical tests
  janitor,      # adding totals and percents to tables
  scales,       # easily convert proportions to percents  
  flextable,    # converting tables to pretty images
  dplyr,
  expss,
  sf,
  gtsummary
)

#Definir el directorio
path_script<-rstudioapi::getActiveDocumentContext()$path
path_folder<-dirname(path_script) 
setwd(path_folder)
getwd()

# Importing Data ----------------------------------------------------------

db<-st_read("../stores/db_cln.geojson")

# Description Statistics --------------------------------------------------
names(db) #revisamos las variables

db<-db %>% select (-"property_id",-"city",-"month",-"year",-"surface_total",-"surface_covered",-"rooms",-"bathrooms",-"operation_type",-"title",-"description",-"area_texto", -"bano_texto",-"geometry") #sacamos las que no son de interés
db$geometry<-NULL

skim(db)

summary(db$price)

db%>% tabyl(price) #simple table with n, percentage and valid percent

db%>%tabyl(LOCALIDAD,sample) #cross-tabulation
db%>%tabyl(LOCALIDAD,sample, property_type) #cross-tabulation

db<-db %>% select (sample, price, bedrooms, property_type, estudio, parqueadero, balcon, chimenea, 
                   ascensor, bbq, gimnasio, vigilancia, jardin, parrilla, cuarto.servicio, 
                   conjunto.cerrado, zona.servicio, area, banos,LOCALIDAD, i_riñas, i_orden, 
                   i_narcoticos, i_maltrato, d_hurto_personas, d_hurto_autos, d_hurto_motos,
                   d_violencia, dist_CC, dist_col)

db %>% 
  tbl_summary(
    by=sample, #estratificación de tabla
    statistic= list(all_continuous()~ "{mean}({sd})", #stats y formato para variables continuas
                    all_categorical()~ "{n}/{N} ({p}%)", #stats y formato para variables categóricas
                    digits=all_continuous() ~ 1, #redondeo para variables continuas
                    type=all_categorical()~ "categorical" # forzamos a que se muestren todas las variables categóricas
                    #label= list( #mostrar lables para las columnas
                      
                    )
)

#price, bedrooms, property_type, sample,estudio, parqueadero, balcon, chimenea, ascensor, bbq, gimnasio, vigilancia, jardin, parrilla, cuarto.servicio, conjunto.cerrado, zona.servicio, area, banos,localidad, i.riñas, i.orden, i.narcoticos, i.maltrato, d_hurto_personas, d_hurto_autos, d_hurto_motos,d_violencia, dist_CC, dist_col,