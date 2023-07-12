################# Consolidación de la base de datos ############################
################################################################################

rm(list = ls()) # Limpiar Rstudio
#options(scipen = 20,  digits=3) # establezco la notacion científica y el número de decimales

#Definir el directorio
path_script<-rstudioapi::getActiveDocumentContext()$path
path_folder<-dirname(path_script) 
setwd(path_folder)
getwd()

#packages
pacman::p_load("tidyverse", #data wrangling
               "vtable", #descriptive stats package
               "stargazer", #tidy regression results,
               "sf", #handling spatial data
               "spatialsample", #spatial CV
               "leaflet", #visualizacion open street maps
               "tmaptools") #conexion a open street maps

### Unifico bases con variables adicionales ##################################

# Cargo las bases
data1 <- st_read("../stores/data2.geojson")
data2 <-readRDS("../stores/data1.rds")

# Elimino variables repetidas en ambas bases
data1 <- select(data1, -c(2:14)) 

#indicamos la base como dato espacial
data1 <- st_as_sf(data1, coords = c("lon", "lat"), crs = 4326) #crs: WGS 1984
data1 <- st_transform(data1, crs = 4686) # crs: MAGNA-SIRGAS (referencia datos Bogota)

data2 <- st_as_sf(data2, coords = c("lon", "lat"), crs = 4326) #crs: WGS 1984
data2 <- st_transform(data2, crs = 4686) # crs: MAGNA-SIRGAS (referencia datos Bogota)

# confirmo que ambas bases tengan la misma ubicación espacial
st_crs(data1)
st_crs(data2)

# cambio la referencia de datos de data1
data1 <- st_transform(data1, crs = 4686) # crs: MAGNA-SIRGAS (referencia datos Bogota)
data2 <- st_transform(data2, crs = 4686)

# Consolido las bases
data <- st_union(data1, data2)