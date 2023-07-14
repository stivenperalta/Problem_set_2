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

db<-db %>% select (sample, price, bedrooms, property_type, parqueadero, balcon, chimenea, 
                   ascensor, bbq, gimnasio, vigilancia, 
                   area, banos,LOCALIDAD, i_riñas, i_orden, 
                   i_narcoticos, i_maltrato, d_hurto_personas, d_hurto_autos, d_hurto_motos,
                   d_violencia, dist_CC, dist_col) #seleccionamos variables que queremos usar para tabla descriptiva

db <- db %>%
  mutate(across(where(is.numeric), ~ round(., digits = 1))) #redondeamos valores

db<- db %>%
  mutate(price= price/1000000) #ponemos los precios en millones

tbl_summary(db, 
            by = sample, #sacamos valores separados para train y test
            statistic = list(all_continuous() ~ "{mean} ({sd})", #estadisticas para valores continuos
                             all_categorical() ~ "{n}/{N} ({p}%)"),  #estadisticas para variables categoricas
            digits = all_continuous() ~ 1, 
            type = all_categorical() ~ "categorical",
            missing_text= "Missing",
            label= list(
              price ~ "Precio en millones COP",
              bedrooms ~ "Habitaciones",
              property_type ~ "Tipo de propiedad",
              parqueadero ~ "Parqueadero",
              balcon ~ "Balcon",
              chimenea ~ "Chimenea",
              ascensor ~ "Ascensor",
              bbq ~ "BBQ",
              gimnasio ~ "Gimnasio",
              vigilancia ~ "Vigilancia",
              area ~ "Área",
              banos ~ "Baños",
              LOCALIDAD ~ "Localidad",
              i_riñas ~ "Incidentes riñas 2019-2021",
              i_orden ~ "Incidentes orden público 2019-2021",
              i_narcoticos~ "Incidentes narcóticos 2019-2021",
              i_maltrato ~ "Incidentes maltrato 2019-2021",
              d_hurto_personas ~ "Delitos hurto personas 2019-2021",
              d_hurto_autos ~ "Delitos hurto autos 2019-2021",
              d_hurto_motos ~ "Delitos hurto motos 2019-2021",
              d_violencia ~ "Delitos violencia intrafamiliar 2019-2021",
              dist_CC ~ "Distancia centro comercial",
              dist_col ~ "Distancia colegios")
              
              
              
              
            )


#price, bedrooms, property_type, sample,estudio, parqueadero, balcon, chimenea, ascensor, bbq, gimnasio, vigilancia, jardin, parrilla, cuarto.servicio, conjunto.cerrado, zona.servicio, area, banos,localidad, i.riñas, i.orden, i.narcoticos, i.maltrato, d_hurto_personas, d_hurto_autos, d_hurto_motos,d_violencia, dist_CC, dist_col,