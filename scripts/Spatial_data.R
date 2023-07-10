############################# problem set 1 #################################
# Autor: Sergio Jimenez
# SI ENCUENTRA ALGUN ERROR O SUGERENCIA POR FAVOR CONTACTEME
# correo: sa.jimenezo1@uniandes.edu.co
# fecha: 05/07/2023
############################################################################

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


####          cargamos la base de datos (uniendo train y test)            ####
train <- read.csv("../stores/train.csv")
test  <-read.csv("../stores/test.csv")

test <- test%>% mutate(sample="test")
train <- train%>% mutate(sample="train")

data <- rbind(train,test)
summary(data)

#indicamos la base como dato espacial
data <- st_as_sf(data, coords = c("lon", "lat"), crs = 4326) #crs: WGS 1984
data <- st_transform(data, crs = 4686) # crs: MAGNA-SIRGAS (referencia datos Bogota)

print(data)

#visualizamos los datos
ggplot()+
  geom_sf(data=data)


####            INFORMACION DE FUENTES EXTERNAS              ####

#### CATASTRO ####

#localidades
localidad <- st_read("../stores/localidad/localidad.shp")
st_crs(localidad)
localidad <- st_transform(localidad, crs =4686) #proyectar a coordenadas MAGNA-SIRGAS
print(localidad)
localidad <- localidad %>%
  select(CODIGO_LOC,NOMBRE) %>%
  rename(COD_LOC = CODIGO_LOC,
         LOCALIDAD = NOMBRE)


#sectores catastrales (barrios)
sector_catastral <- st_read("../stores/SECTOR.geojson")
st_crs(sector_catastral)
print(sector_catastral)
summary(sector_catastral$SCACODIGO) #Variable de interes codigo ID
table(sector_catastral$SCANOMBRE) #Variable de interes nombre Barrio
sector_catastral <- sector_catastral %>% 
  select(SCACODIGO,SCANOMBRE) %>% #Dejamos solo variables de interes
  rename(COD_SEC=SCACODIGO,
         BARRIO=SCANOMBRE)


#valor de referencia comercial m2 terreno
v_ref_mzn <- st_read("../stores/valor_ref_2023.geojson")
st_crs(v_ref_mzn)
v_ref_mzn <- st_transform(v_ref_mzn, crs = 4686) #proyectar a coordenadas MAGNA-SIRGAS
print(v_ref_mzn)
summary(v_ref_mzn$MANCODIGO) #Variable de interes codigo manzana
summary(v_ref_mzn$V_REF) #Variable de interes valor de referencia comercial m2 terreno
v_ref_mzn <- v_ref_mzn %>%
  select(MANCODIGO,V_REF) %>%
  rename(COD_MZN=MANCODIGO,
         V_REF_22=V_REF)


#estratos
estratos <- st_read("../stores/manzanaestratificacion/ManzanaEstratificacion.shp")
st_crs(estratos)
print(estratos)
summary(estratos$ESTRATO) ##Variable de interes estratos
estratos <- estratos %>%
  select(ESTRATO)

####  TURISMO ####

#establecimientos gastronomia y bar
T_EGB <- st_read("../stores/establecimientos gastronomia y bar.geojson")
st_crs(T_EGB)
print(T_EGB)
summary(T_EGB$Division) ## Variable de interes densidad establecimientos (continua)
table(T_EGB$Clases) ## Variable de interes densidad establecimientos (categorica)
summary(T_EGB$CODIGO_UPZ) ## Variable de interes CODIGO UPZ (categorica)
table(T_EGB$NOMBRE) ## Variable de interes nombre UPZ (categorica)
T_EGB <- T_EGB %>%
  select(CODIGO_UPZ,NOMBRE,Clases) %>%
  rename(COD_UPZ = CODIGO_UPZ,
         UPZ = NOMBRE,
         tegb = Clases)


#establecimientos de alojamiento turistico
T_EAT <- st_read("../stores/establecimientos alojamiento turistico.geojson")
st_crs(T_EAT)
print(T_EAT)
summary(T_EAT$Division) ## Variable de interes densidad establecimientos (continua)
table(T_EAT$Clases) ## Variable de interes densidad establecimientos (categorica)
T_EAT <- T_EAT %>%
  select(CODIGO_UPZ,Clases) %>%
  rename(COD_UPZ = CODIGO_UPZ,
         teat = Clases)


#union de informacion de establecimientos turismo
est_turismo  <- st_join(T_EGB,T_EAT, join = st_equals)
print(est_turismo)
est_turismo <- est_turismo %>%
  select(-COD_UPZ.y) %>%
  rename(COD_UPZ = COD_UPZ.x)


#zonas de interes turistico
zonas_turisticas <- st_read("../stores/zitu.geojson")
st_crs(zonas_turisticas)
print(zonas_turisticas)
table(zonas_turisticas$TIPOLOGÍA) #variable de interes, tipologia de la zona NA cuando no aplica
zonas_turisticas<- zonas_turisticas %>%
  select(TIPOLOGÍA) %>%
  rename(tipologia_ZIT = TIPOLOGÍA)

#generar buffer para zonas turisticas
zonas_turisticas_buffer <- st_buffer(zonas_turisticas, dist = 100)

##grafico para revisar el buffer
# Combinar los datos de las bases de polígonos
datos_combinados <- rbind(
  mutate(zonas_turisticas, Categoria = "originales"),
  mutate(zonas_turisticas_buffer, Categoria = "buffer")
)

# Crear el plano con los polígonos combinados y transparencia
ggplot() +
  geom_sf(data = datos_combinados, aes(fill = Categoria), color = "black", alpha = 0.5) +
  labs(title = "Polígonos originales y con buffer") +
  scale_fill_manual(values = c("blue", "red"), labels = c("buffer", "original"))


####  CRIMEN  ####

#incidentes delictivos
incidentes <- st_read("../stores/IRSCAT.geojson")
st_crs(incidentes)
print(incidentes) ## pendiente revisar si vale la pena incluir estos datos o existe colinealidad con el barrio
incidentes <- incidentes %>%
  mutate(i_riñas = CMR19CONT + CMR20CONT + CMR21CONT, #sumatoria del registro de incidentes del tipo riññas entre 2019 a 2021
         i_narcoticos = CMN19CONT + CMN20CONT + CMN21CONT, #sumatoria del registro de incidentes del tipo narcoticos entre 2019 a 2021
         i_orden = CMAOP19CONT + CMAOP20CONT + CMAOP21CONT, #sumatoria del registro de incidentes del tipo orden publico entre 2019 a 2021
         i_maltrato = CMMM19CONT + CMMM20CONT + CMMM21CONT) %>% #sumatoria del registro de incidentes del tipo maltrato entre 2019 a 2021
  select(CMIUSCAT,CMNOMSCAT,i_riñas,i_narcoticos,i_orden,i_maltrato) %>%
  rename(COD_SEC = CMIUSCAT,
         BARRIO  = CMNOMSCAT)


#delitos de alto impacto
delitos <- st_read("../stores/DAISCAT.geojson")
st_crs(delitos)
print(delitos) ## pendiente revisar si vale la pena incluir estos datos o existe colinealidad con el barrio
delitos <- delitos %>%
  mutate(d_homicidios = CMH19CONT + CMH20CONT + CMH21CONT, #sumatoria de delitos del tipo homicidio (2019-2021)
         d_lesiones = CMLP19CONT + CMLP20CONT + CMLP21CONT, #sumatoria de delitos del tipo lesiones personales  (2019-2021)
         d_hurto_personas = CMHP19CONT + CMHP20CONT + CMHP21CONT, #sumatoria de delitos del tipo hurto a personas (2019-2021)
         d_hurto_residencias = CMHR19CONT + CMHR20CONT + CMHR21CONT, #sumatoria de delitos del tipo hurto a residencias (2019-2021)
         d_hurto_comercio = CMHC19CONT + CMHC20CONT + CMHC21CONT, #sumatoria de delitos del tipo hurto a comercios (2019-2021)      
         d_hurto_autos = CMHA19CONT + CMHA20CONT + CMHA21CONT, #sumatoria de delitos del tipo hurto automotores (2019-2021)
         d_hurto_motos = CMHM19CONT + CMHM20CONT + CMHM21CONT, #sumatoria de delitos del tipo hurto motocicletas (2019-2021)
         d_hurto_bici = CMHB19CONT + CMHB20CONT + CMHB21CONT, #sumatoria de delitos del tipo hurto bicicletas (2019-2021)
         d_hurto_cel = CMHCE19CONT + CMHCE20CONT + CMHCE21CONT, #sumatoria de delitos del tipo hurto celulares (2019-2021)
         d_sexual = CMDS19CONT + CMDS20CONT + CMDS21CONT, #sumatoria de delitos del tipo sexual (2019-2021)
         d_violencia = CMVI19CONT + CMVI20CONT + CMVI21CONT) %>% #sumatoria de delitos del tipo violencia intrafamiliar (2019-2021)
  select(CMIUSCAT,CMNOMSCAT,d_homicidios,d_lesiones,d_hurto_personas,
         d_hurto_residencias,d_hurto_comercio,d_hurto_autos,d_hurto_motos,
         d_hurto_bici,d_hurto_cel,d_sexual,d_violencia) %>%
  rename(COD_SEC = CMIUSCAT,
         BARRIO = CMNOMSCAT)


# union informacion de crimen
incidentes <- st_make_valid(incidentes)
delitos <- st_make_valid(delitos)
crimen <- st_join(incidentes,delitos, join = st_equals)
print(crimen)
crimen <- crimen %>%
  select(-COD_SEC.y, -BARRIO.y) %>%
  rename(COD_SEC = COD_SEC.x,
         BARRIO = BARRIO.x)

#### SERVICIOS Y ESPACIO PUBLICO ####

#Colegios
colegios <- st_read("../stores/colegios.geojson")
st_crs(colegios)
colegios <- st_transform(colegios, crs = 4686) #proyectar a coordenadas MAGNA-SIRGAS
print(colegios)


#parques
parques <- st_read("../stores/parques/Parque.shp")
st_crs(parques)
parques <- st_transform(parques, crs = 4686) #proyectar a coordenadas MAGNA-SIRGAS
print(parques)


#espacio publico efectivo
epe <- st_read("../stores/epe_upz/EPE_UPZ.shp")
st_crs(epe)
epe <- st_transform(epe, crs = 4686) #proyectar a coordenadas MAGNA-SIRGAS
print(epe)
epe <- epe %>% select(EPE)


#Estaciones de TM
TM <- st_read("../stores/Estaciones_TM.geojson")
st_crs(TM)
TM <- st_transform(TM, crs = 4686) #proyectar a coordenadas MAGNA-SIRGAS
print(TM)


#Centros comerciales
CC <- st_read("../stores/Grandes_centros_comerciales/Grandes_centros_comerciales.shp")
st_crs(CC)
CC <- st_transform(CC, crs = 4686) #proyectar a coordenadas MAGNA-SIRGAS
print(CC)


#### POBLACION ####

#censo DANE 2018
POB <- st_read("../stores/CENSO_2018_BOG/CENSO_2108_BOG.shp")
st_crs(POB)
print(POB)
POB <- POB %>% 
  select(DENSIDAD, TVIVIENDA, TP27_PERSO) %>% #Dejamos solo variables de interes
  rename(mzn_densidad = DENSIDAD,
         mzn_n_viv=TVIVIENDA,
         mzn_n_hab=TP27_PERSO)

####  UNION ESPACIAL SEGUN ESCALA DE AGREGACION  ####

#### nivel localidad ####
data <- st_join(data, localidad, join = st_within) # Realizar la unión espacial
print(data)

#### nivel upz ####
data  <- st_join(data,est_turismo, join = st_within)
print(data)

data <- st_join(data,epe, join = st_within)
print(data)

#### nivel barrio ####
sector_catastral = st_make_valid(sector_catastral)
data <- st_join(data,sector_catastral, join = st_within)
print(data)

data <- st_join(data,crimen, join = st_within)
print(data)
data <- data %>%
  select(-COD_SEC.y,-BARRIO.y) %>%
  rename(COD_SEC = COD_SEC.x,
         BARRIO = BARRIO.x)

#### nivel manzana (asignamos el valor mas cercano)  ####
v_ref_mzn <- st_make_valid(v_ref_mzn)
nearest_i_vref <- st_nearest_feature(data, v_ref_mzn) # Obtener los índices de los polígonos más cercanos a cada punto en "data"

estratos <- st_make_valid(estratos)
nearest_i_estratos <- st_nearest_feature(data, estratos)

nearest_i_POB <- st_nearest_feature(data, POB)

data <- data %>%
  mutate(COD_MZN = v_ref_mzn$COD_MZN[nearest_i_vref],
         V_REF_22 = v_ref_mzn$V_REF_22[nearest_i_vref],
         ESTRATO = estratos$ESTRATO[nearest_i_estratos],
         mzn_densidad = POB$mzn_densidad[nearest_i_POB],
         mzn_n_viv = POB$mzn_n_viv[nearest_i_POB],
         mzn_n_hab = POB$mzn_n_hab[nearest_i_POB])
print(data)


#### nivel poligono (zitu)  ####
data <- st_join(data,zonas_turisticas_buffer, join = st_within)
print(data)


#### distancia a servicios ####

#colegios
dist_col <- st_distance(data, colegios)
data$dist_col <- apply(dist_col,1,min)
print(data)
summary(data$dist_col)
hist(data$dist_col)

#parques
parques <- st_make_valid(parques)
dist_parq <- st_distance(data, parques)
data$dist_parq <- apply(dist_parq,1,min)
print(data)
summary(data$dist_parq)
hist(data$dist_parq)

#estaciones TM
dist_TM <- st_distance(data, TM)
data$dist_TM <- apply(dist_TM,1,min)
print(data)
summary(data$dist_TM)
hist(data$dist_TM)


#Centros comerciales
dist_CC <- st_distance(data,CC)
data$dist_CC <- apply(dist_CC,1,min)
print(data)
summary(data$dist_CC)
hist(data$dist_CC)


##### BASE CONSOLIDADA ####
print(data)
names(data)
data <- data %>%
  select(property_id,price, sample, year, month, surface_total, surface_covered,
         rooms, bedrooms, bathrooms, property_type, operation_type, title, description,
         COD_LOC, COD_UPZ, COD_SEC, COD_MZN, LOCALIDAD, UPZ, BARRIO, V_REF_22,
         ESTRATO, mzn_densidad, mzn_n_viv, mzn_n_hab, EPE, dist_parq, dist_TM,
         dist_col, dist_CC, tipologia_ZIT, tegb, teat, i_riñas, i_narcoticos,
         i_orden, i_maltrato, d_homicidios, d_lesiones, d_hurto_personas, 
         d_hurto_residencias, d_hurto_comercio, d_hurto_autos, d_hurto_motos,
         d_hurto_bici, d_hurto_cel, d_sexual, d_violencia)

#### exportamos dataset consolidado ####
st_write(data, "../stores/data.geojson", driver = "GeoJSON")



