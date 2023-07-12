############################## Problem Set 2 ###################################
# Autores: 
# fecha: 03/07/2023

# Preparación -------------------------------------------------------------

rm(list = ls()) # Limpiar Rstudio

pacman::p_load(ggplot2, rio, tidyverse, skimr, caret, 
       rvest, magrittr, rstudioapi, stargazer, 
       boot, readxl, knitr, kableExtra,
       glmnet, sf, tmaptools, leaflet,
       tokenizers, stopwords, SnowballC,
       stringi, dplyr, stringr, sp, hunspell,
       car) # Cargar paquetes requeridos

#Definir el directorio
path_script<-rstudioapi::getActiveDocumentContext()$path
path_folder<-dirname(path_script) 
setwd(path_folder)
getwd()

# Importing Data ----------------------------------------------------------

#train data
train<-read_csv("../stores/train.csv")
test<-read_csv("../stores/test.csv")

test<-test %>% mutate(sample="test")
train<-train %>% mutate(sample="train")

db<-rbind(test,train) #juntamos ambas bases

names(db) #vemos las variables disponibles
summary(db)

# Setting the location ---------------------------------------------------
db<- st_as_sf( #para convertirlo en un spatial data frame
  db,
  coords = c("lon", "lat"), #primero longitud, luego latitud
  crs = 4326 #EPSG:4326=WGS84 (World Geodetic System 1984)
)

pal <- colorFactor(
  palette = c('#d9bf0d', '#00b6f1'),
  domain = db$sample #variable for which the color vector should be applied to
)

map<-leaflet() %>% 
  addTiles() %>%  #capa base
  addCircles(data=db,col=~pal(sample)) #pintar casas en base ala funcion pal que creamos arriba
  
map 
rm(path_folder,path_script,pal) # Limpio objetos de poco interés en mi ambiente

# Checking existing variables ---------------------------------------------
#GENERAL
glimpse(db)

#CITY
table(db$city) #revisamos que no hayan errores de entrada en esta variable, todas son Bogotá D.C

#PRICE (para train)
hist(train$price,
     breaks = seq(3.000e+08,1.650e+09,by=50000000),
     col = "lightblue",
     main = "Distribución de Precios en Bogotá",
     xlab = "Precio en COP", ylab = "Número de inmuebles") #creo el histograma

# Tabla de frecuencia del precio train
frequency_table <- train$price %>%
  cut(breaks = seq(3.000e+08, 1.650e+09, by = 50000000), include.lowest = TRUE, right = FALSE) %>%
  table()
as.data.frame(frequency_table)

#Scatterplot de precios por tipo de vivienda (apartamento/casa) (para train)
ggplot(train, aes(x = surface_total, y = price, color = property_type)) +
  geom_point(size = 2) +
  scale_color_manual(values = c("#00b6f1","#d9bf0d")) +
  labs(x = "Superficie Total", y = "Precio", title = "Precios de Inmuebles por superficie")

#MONTH YEAR- CREAMOS UNA VARIABLE UNIENDO MES Y AÑO
typeof(c(db$month, db$year))#revisamos que tipo son (double)
db$date <- as.Date(paste(db$year, db$month,"1", sep = "-")) #se creo variable con formato YYYY-MM-01

#SURFACE TOTAL
 
#SURFACE COVERED

#ROOMS
table(train$rooms)

#BEDROOMS
table(train$bedrooms)

#BATHROOMS
table(train$bathrooms)

#PROPERTY TYPE
pt<-data.frame(table(train$property_type))
pie_pt <- ggplot(pt, aes(x = "", y = Freq, fill = Var1)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  scale_fill_manual(values = c("#d9bf0d", "#00b6f1")) +
  labs(title = "Gráfico Pie de la distribución entre
  casas y apartamentos a la venta")+
  theme(plot.title = element_text(hjust = 0.5))
pie_pt

#OPERATION TYPE
table(train$operation_type) #todos son para la venta

#TITLE
head(db$title)

#DESCRIPTION
head(test$description)
tail(db$description)# parece que no hay tildes ni puntos ni comas ni mayúsculas

# Getting info from Description -------------------------------------------
#area|metro|metros|mt|mets|cuadrado|cuadrados|m|metro|mts|mtrs|mtr

#Reemplazando
db$description <- gsub("(?<=\\d)(?<!\\s)(m2|mt2|mts2|metros|metro|m)", " \\1", db$description, perl = TRUE) #para reemplazar numeros pegados a m2 ie: 50m2 -> 50 m2
db$description <- gsub("\\b(mt[a-z]?[0-9]+)(m2)(m)(metro[a-z]?)\\b", "mts", db$description) #para arreglar errores cuando ponen por ejemplo 230mts23 y cuando hay m2 concatenado a mas numeros e.g. obs 360
db$description <- gsub("\\b(m2|mt2|mts2|mtrs2)\\b", "mts", db$description) #para evitar problemas con los "2" cuando hacemos los loops para sacar el area
db$description <- str_replace_all(db$description, # reemplazo las palabras numéricas en números
                                  c("\\buno\\b" = "1",
                                    "\\bdos\\b" = "2",
                                    "\\btres\\b" = "3",
                                    "\\bcuatro\\b" = "4",
                                    "\\bcinco\\b" = "5",
                                    "\\bseis\\b" = "6",
                                    "\\bsiete\\b" = "7",
                                    "\\bocho\\b" = "8",
                                    "\\bnueve\\b" = "9"))
#drop Stop words

for (i in seq_along(db$description)) {
  palabras_sin_significado <- stopwords("spanish")
  palabras <- unlist(strsplit(db$description[[i]], "\\s+"))
  palabras_limpias <- palabras[!(palabras %in% palabras_sin_significado)]
  db$description[[i]] <- paste(palabras_limpias, collapse = " ")
}

#Tokenization
db$tokens<-tokenize_words(db$description) #esto corta todas las palabras
db$n2tokens<-tokenize_ngrams(x=db$description, #uno de 2 para lo de areass
                            lowercase=TRUE, #convierte todo a lower case, aunque ya estaba, just in case
                            n=2L, #lenght del n-gram (trigram en este caso)
                            n_min=2L, #solo se hacen de 2
                            stopwords=character(), #stopwords que sean excluidas del tokenization. está vacío
                            ngram_delim=" ", #tokens separados por espacios
                            simplify=FALSE) #se crea lista de trigrams
db$n3tokens<-tokenize_ngrams(x=db$description,
                             lowercase=TRUE, #convierte todo a lower case, aunque ya estaba, just in case
                             n=3L, #lenght del n-gram (trigram en este caso)
                             n_min=3L, #solo se hacen de 3 
                             stopwords=character(), #stopwords que sean excluidas del tokenization. está vacío
                             ngram_delim=" ", #tokens separados por espacios
                             simplify=FALSE) #se crea lista de trigrams

#Stemming
db$raiztokens<-wordStem(db$tokens, "spanish")

#Variables of interest
caracteristicas<-c("parqueadero","chimenea","balcon",
                   "vigilancia", "gimnasio","jardin","bbq",
                   "parrilla","estudio","ascensor")
ncaracteristicas<-c("casa multifamiliar","cuarto servicio", "zona servicio","conjunto cerrado")


# Iterating through the list and create dummy variables

#tokens de una palabra
for (i in seq_along(db$tokens)) {
  for (j in seq_along(db$tokens[[i]])) {
    for (k in seq_along(caracteristicas)) {
      pattern <- caracteristicas[k]
      matches <- agrep(pattern, db$tokens[[i]][[j]], ignore.case = TRUE) #usando agrep hace fuzzy matching por si las palabras estan escritas un poco distintas
      if (length(matches) > 0) {
        db[i, caracteristicas[k]] <- 1
        break
      }
    }
  }
}

#tokens de 3 palabras
for (i in seq_along(db$n2tokens)) {
  for (j in seq_along(db$n2tokens[[i]])) {
    for (k in seq_along(ncaracteristicas)) {
      if (grepl(ncaracteristicas[k], db$n2tokens[[i]][[j]], ignore.case = TRUE)) {
        db[i, ncaracteristicas[k]] <- 1
        break
      }
    }
  }
}

#Replacing dummy variables with 0s 
columnas <- names(db)[22:34]  # Selecciono variables para reemplazar NA's
db <- db %>% # Reemplazo los valores
  mutate(
    across(all_of(columnas), ~ ifelse(is.na(.), 0, .)))

#BUSCANDO AREAS
db$area_texto <- sapply(db$n2tokens, function(tokens) {
  area_ngram <- grep("\\b(area|metro|metros|mt|mets|cuadrado|cuadrados|m|metro|mts|mtrs|mtr)\\b", tokens, ignore.case = TRUE, value = TRUE)
  if (length(area_ngram) > 0) {
    numbers <- gsub("\\D+", "", area_ngram)
    numeric_values <- as.numeric(numbers)
    numeric_values <- ifelse(db$property_type == "Apartamento", numeric_values[numeric_values >= 15 & numeric_values <= 500],
                             ifelse(db$property_type == "Casa", numeric_values[numeric_values >= 50 & numeric_values <= 2500], NA))
    numeric_values
  } else {
    NA
  }
})

db$area_texto <- sapply(db$area_texto, function(x) max(x, na.rm = TRUE, 0)) #reemplazo NA's por ceros y dejo el valor más alto

#juntando informacion de areas
db$area <- pmax(db$surface_total, db$surface_covered, na.rm = TRUE) #primero ponemos el area mas grande entre surface_total y surface_covered
db$area <- coalesce(db$area, db$area_texto) #reemplazamos la variable area con el valor sacado de la descripción en caso area sea NA

#BUSCANDO BAÑOS
db$bano_texto <- sapply(db$n2tokens, function(tokens) {
  match <- grep("(\\d+)\\s*(?=\\b(bano|banos|bao|baos)\\b)", tokens, ignore.case = TRUE, perl = TRUE, value = TRUE)
  if (length(match) > 0) {
    numbers <- gsub("\\D+", "", match)
    numbers <- as.numeric(numbers)
    numbers <- numbers[numbers <= 10] #le ponemos este control en base a el número más alto en bathrooms (13)
    if (length(numbers) > 0) {
      numbers
    } else {
      NA
    }
  } else {
    NA
  }
})

db$bano_texto[is.na(db$bano_texto)] <- 0 #reemplazando los NAs
db$bano_texto[sapply(db$bano_texto, function(x) all(is.na(x)))] <- 0 #reemplazando los que tienen c(NA,NA...)

db$bano_texto <- sapply(db$bano_texto, function(x) na.omit(unlist(x))) #sacamos de los elementos que tienen NAs y números, solo en numero
db$bano_texto <- sapply(db$bano_texto, function(x) max(x, na.rm = TRUE)) #sacamos de los elementos que tienen varios números, el número más alto

#los que tienen 0 contaran cuantas veces se repite la palabra bano, banos, bao, baos
counts <- sapply(db$tokens, function(tokens) {
  sum(grepl("\\b(bano|banos|bao|baos)\\b", tokens, ignore.case = TRUE))
})
db$bano_texto[db$bano_texto == 0] <- counts[db$bano_texto == 0]

#juntando informacion de banos
db$banos <- coalesce(db$bathrooms, db$bano_texto) #agregamos a la variable banos el valor de bathrooms. si es NA, usamos el de bano_texto

#arreglando variables
db$property_type<-as.factor(db$property_type)

# DATOS GEOESPACIALES -----------------------------------------------------
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

#indicamos la base como dato espacial
db <- st_as_sf(db, coords = c("lon", "lat"), crs = 4326) #crs: WGS 1984
db <- st_transform(db, crs = 4686) # crs: MAGNA-SIRGAS (referencia datos Bogota)

print(db)

#visualizamos los datos
ggplot()+
  geom_sf(db=db)


####            INFORMACION DE FUENTES EXTERNAS              ####

#### CATASTRO ####

#localidades
localidad <- read_sf("https://datosabiertos.bogota.gov.co/dataset/856cb657-8ca3-4ee8-857f-37211173b1f8/resource/497b8756-0927-4aee-8da9-ca4e32ca3a8a/download/loca.json")
st_crs(localidad)
localidad <- st_transform(localidad, crs =4686) #proyectar a coordenadas MAGNA-SIRGAS
print(localidad)
localidad <- localidad %>%
  select(LocCodigo,LocNombre) %>%
  rename(COD_LOC = LocCodigo,
         LOCALIDAD = LocNombre)


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
v_ref_mzn<- read_sf ("https://datosabiertos.bogota.gov.co/dataset/a0ad3bf4-1e97-4cf9-b853-76558158036f/resource/a58af59e-9e8e-4e3c-a2ec-2388231edac8/download/valor_ref_2023.geojson")
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
estratos <- read_sf("https://datosabiertos.bogota.gov.co/dataset/55467552-0af4-4524-a390-a2956035744e/resource/29f2d770-bd5d-4450-9e95-8737167ba12f/download/manzanaestratificacion.json")
st_crs(estratos)
print(estratos)
summary(estratos$ESTRATO) ##Variable de interes estratos
estratos <- estratos %>%
  select(ESTRATO)

####  TURISMO ####

#establecimientos gastronomia y bar
T_EGB <- read_sf("https://datosabiertos.bogota.gov.co/dataset/ea4d5bd3-ef6d-47ca-9357-4bf7075e1756/resource/b3dc815c-80ed-432c-bb23-ddbf134834ec/download/oprof.geojson")
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
T_EAT <- read_sf("https://datosabiertos.bogota.gov.co/dataset/f49c718e-d9e9-4271-8f8b-4e3a7450d357/resource/1826742f-57cc-47b4-ad7a-556daab781f6/download/eatu.geojson")
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
zonas_turisticas <- read_sf("https://datosabiertos.bogota.gov.co/dataset/0f36f844-5c2e-4e31-8486-d698a0a3f3dd/resource/8d9b4899-98cf-4d5b-bf36-c630bd959f9a/download/zitu.geojson")
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
colegios <- read_sf("https://datosabiertos.bogota.gov.co/dataset/d451b52f-e30c-43b3-9066-3a7816638fea/resource/4a6462ef-fa2e-4acf-96db-8521c65371e8/download/colegios_2022_09.geojson")
st_crs(colegios)
colegios <- st_transform(colegios, crs = 4686) #proyectar a coordenadas MAGNA-SIRGAS
print(colegios)


#parques
parques <- read_sf("https://datosabiertos.bogota.gov.co/dataset/1ca41514-3671-41d6-8c3b-a970dc8c24a7/resource/16288e7f-0345-4680-84aa-40e987706ea8/download/parque.json")
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
CC <- read_sf("https://datosabiertos.bogota.gov.co/dataset/ce479dd9-7d54-4400-a05d-8df538c43e29/resource/c91f8dbd-f0a4-4fe1-83be-935a2de908da/download/gran_centro_comercial.geojson")
st_crs(CC)
CC <- st_transform(CC, crs = 4686) #proyectar a coordenadas MAGNA-SIRGAS
print(CC)


#### POBLACION ####

#censo DANE 2018
POB <- st_read("../stores/CENSO_2018_BOG/MGN_ANM_MANZANA.shp")
st_crs(POB)
print(POB)
POB <- POB %>% 
  select(DENSIDAD, TVIVIENDA, TP27_PERSO) %>% #Dejamos solo variables de interes
  rename(mzn_densidad = DENSIDAD,
         mzn_n_viv=TVIVIENDA,
         mzn_n_hab=TP27_PERSO)

####  UNION ESPACIAL SEGUN ESCALA DE AGREGACION  ####

#### nivel localidad ####
db <- st_join(db, localidad, join = st_within) # Realizar la unión espacial
print(db)

#### nivel upz ####
db  <- st_join(db,est_turismo, join = st_within)
print(db)

db <- st_join(db,epe, join = st_within)
print(db)

#### nivel barrio ####
sector_catastral = st_make_valid(sector_catastral)
db <- st_join(db,sector_catastral, join = st_within)
print(db)

db <- st_join(db,crimen, join = st_within)
print(db)
db <- db %>%
  select(-COD_SEC.y,-BARRIO.y) %>%
  rename(COD_SEC = COD_SEC.x,
         BARRIO = BARRIO.x)

#### nivel manzana (asignamos el valor mas cercano)  ####
v_ref_mzn <- st_make_valid(v_ref_mzn)
nearest_i_vref <- st_nearest_feature(db, v_ref_mzn) # Obtener los índices de los polígonos más cercanos a cada punto en "data"

estratos <- st_make_valid(estratos)
nearest_i_estratos <- st_nearest_feature(db, estratos)

nearest_i_POB <- st_nearest_feature(db, POB)

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
st_write(data, "../stores/data.shp")

data <- st_read("../stores/data.geojson")
head(data)

  
#Guardamos la base
saveRDS(db, file = "../stores/data1.rds")

# Evaluación de Outlier #######################################################

# Evalúo outliers de las variables continuas
var_outliers <- db[, c("price", "bedrooms", "banos", "area")]

# Establecer el diseño de la ventana de gráficos
par(mfrow = c(2, 2))  # Ajusta los valores de "filas" y "columnas" según tus necesidades

# Evalúo outliers de mis variables continuas con boxplot
for (variable in colnames(var_outliers)) {
  boxplot(var_outliers[[variable]], main = variable)
}

# Evalúo valores estadísticamente atípicos mediante prueba de significancia outlierTest
for (variable in colnames(var_outliers)) {
  formula <- paste(variable, "~ 1")
  lm_outliers <- lm(formula, data = var_outliers, na.action = na.omit)
  outlier_test <- outlierTest(lm_outliers)
  cat("Variable:", variable, "\n")
  summary(lm_outliers)
  print(outlier_test)
  cat("\n")
}


# analizo los outliers para evaluar la coherencia de las observaciones
db[c(17479, 24177, 10974, 12357, 22451, 27063, 30986, 16068, 34895, 16436), # seleccionar aquí los valores atípicos de la variable 1 (el número de la observación)
     c("price", "area", "bedrooms", "banos", "property_type","surface_total", "surface_covered", "sample")] # VARIABLE 1

#sacamos observaciones que no tienen coherencia
db <- db[-c(9408, 41744, 41728, 44194, 44195, 41591, 39557), ]
db<- db[-c(38403,47866, 27721,25878,29445),]

#reemplazando valores en base a texto
db$area[8979]<-249.3
db$area[16984]<-248.5
db$area[28983]<-248.5
db$area[20630]<-147
db$area[23588]<-247.5
db$area[26817]<-246.2
db$area[27277]<-230
db$area[46489]<-1607.2
db$area[40153]<-243.1
db$area[39557]<-271
db$area[7420]<-271
db$area[37852]<-238.8
db$area[30705]<-238.2

db$area[10974]<-237.5
db$area[12357]<237.5
db$area[22451]<-237.5
db$area[27063]<-237.5


#Scatterplot de precios por area y tipo de vivienda (apartamento/casa) (para train)
ggplot(data = subset(db, sample == "train"), aes(x = price, y = area, color = property_type)) +
  geom_point(size = 2) +
  scale_color_manual(values = c("#00b6f1", "#d9bf0d")) +
  labs(x = "Precio", y = "Area", title = "Precios de Inmuebles por superficie")


# Imputación de valores a otras variables con k Nearest Neighbors (kNN) ########

# Evalúo variables con missing values para imputar
db$area<-ifelse(db$area==0,NA,db$area)
db$banos<-ifelse(db$banos==0,NA,db$banos)
db$`casa multifamiliar`<-ifelse(is.na(db$`casa multifamiliar`),0,db$`casa multifamiliar`)

missing_values <- colSums(is.na(db)) #sumo los NA's para cada variable
missing_table <- data.frame(Variable = names(missing_values), Missing_Values = missing_values) # lo reflejo en un data.frame
missing_table

# Creo método de imputación con el paquete mice para imputar las variables rooms Y bathrooms
install.packages("mice")
library(mice)

#Grabamos la base
saveRDS(db, file = "../stores/data1.rds")

db$tokens<-NULL
db$n2tokens<-NULL
db$n3tokens<-NULL

write.csv(db, file = "../stores/data1.csv")

# mice tiene varios métodos de imputación. Estos valores es recomendable ajustarlos a medida que se corren los modelos para evaluar cuál presenta la mejor imputación.
# Este artículo siento que es de ayuda: https://www.r-bloggers.com/2015/10/imputing-missing-data-with-r-mice-package/amp/
db_subset <- select(db, area, banos)  # Selecciono variables para imputar
db_subset$geometry <- NULL
db_subset
mice_data <- mice(db_subset, m = 5, method = "pmm", seed = 201718234) # imputo con mice.impute.2lonly.pmm: Método de imputación para datos numéricos utilizando Predictive Mean Matching (PMM) con dos etapas (dos niveles).
#Con la opción methods(mice) pueden ver los métodos de imputación para seleccionar el más acorde
# Algunos de los más relevantes que vi (solo reemplazan "pmm" por el que escojan en method =):
# "cart" "lasso.logreg" "lasso.norm" "lasso.select.logreg" 
# "lasso.select.norm" "logreg.boot" "mpmm" "polr" "polyreg"


#Evalúo las imputaciones
mice_data$imp # si incluyo $variable solo vería los valores para una sola variable

# Unifico valores imputados con valores de mi base maestra
db[db_subset] <- complete(mice) # Una recomendación sería imputar sobre una base de copia para que, en caso de error, no tengan que correr todo el código nuevamente

glimpse(db) compruebo
#################### FIN ######################################################
