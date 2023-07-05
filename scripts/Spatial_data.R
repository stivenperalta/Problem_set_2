############################# problem set 1 ###################################
# Autor: Sergio Jimenez
# SI ENCUENTRA ALGUN ERROR O SUGERENCIA POR FAVOR CONTACTEME
# correo: sa.jimenezo1@uniandes.edu.co
# fecha: 05/07/2023
###############################################################################

rm(list = ls()) # Limpiar Rstudio
options(scipen = 20,  digits=3) # establezco la notacion científica y el número de decimales

#Definir el directorio
path_script<-rstudioapi::getActiveDocumentContext()$path
path_folder<-dirname(path_script) 
setwd(path_folder)
getwd()

#packages
require("pacman")
p_load("tidyverse", #data wrangling
       "vtable", #descriptive stats package
       "stargazer", #tidy regression results,
       "sf", #handling spatial data
       "spatialsample", #spatial CV
       "leaflet", #visualizacion open street maps
       "tmaptools") #conexion a open street maps

##cargamos la base de datos
train <- read.csv("../stores/train.csv")
train_sf <- st_as_sf(train, coords = c("lon", "lat"), crs = 4326) # indicamos como dato espacial

##visualizamos los datos
ggplot()+
  geom_sf(data=train_sf)

##cargamos la base de barrios
sector_catastral <- st_read("../stores/SECTOR.geojson")
print(sector_catastral)

# Transformar el sistema de coordenadas de la base train a MAGNA-SIRGAS para unir con las bases de BOGOTA
train_sf <- st_transform(train_sf, crs = 4686)
print(train_sf)

# graficamos las dos bases
ggplot() +
  geom_sf(data = sector_catastral, fill = "lightblue", color = "black") +
  geom_sf(data = train_sf)

#unimos la informacion
sector_catastral_valid <- st_make_valid(sector_catastral) # Validar las geometrías de sector catastral
data <- st_join(train_sf, sector_catastral_valid, join = st_within) # Realizar la unión espacial

##cargamos la base de valores de refrencia por manzana
v_ref_mzn <- st_read("../stores/valor_ref_2023.geojson")
v_ref_mzn <- st_transform(v_ref_mzn, crs = 4686)
print(v_ref_mzn)

#unimos la informacion
v_ref_mzn_valid <- st_make_valid(v_ref_mzn) # Validar las geometrías de sector catastral
data <- st_join(data, v_ref_mzn_valid, join = st_within) # Realizar la unión espacial
print(data) ## existen puntos por fuera de manzanas en la geolocalizacion

# se los valores de la manzana mas cercana

nearest_indices <- st_nearest_feature(data, v_ref_mzn_valid) # Obtener los índices de los polígonos más cercanos a cada punto en "data"

data <- data %>%
  mutate(valor_ref = v_ref_mzn_valid$V_REF[nearest_indices],
         cod_mzn = v_ref_mzn_valid$MANCODIGO[nearest_indices]
         )


