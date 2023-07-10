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
       stringi, dplyr, stringr) # Cargar varios paquetes al tiempo

#Definir el directorio
path_script<-rstudioapi::getActiveDocumentContext()$path
path_folder<-dirname(path_script) 
setwd(path_folder)
getwd()
rm(path_folder,path_script)

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

# Checking existing variables ---------------------------------------------
#GENERAL
glimpse(db)

#CITY
table(db$city) #revisamos que no hayan errores de entrada en esta variable, todas son Bogotá D.C

#PRICE (para train)
br = seq(3.000e+08,1.650e+09,by=50000000) # creamos los braquets/bins
ranges = paste(head(br,-1), br[-1], sep=" - ") #creamos los rangos en base a los braquets
freq   = hist(train$price, breaks=br, include.lowest=TRUE, plot=TRUE, 
              
              col="#00b6f1", border="#d9bf0d",
              xlab="Precio en COP", ylab="Número de inmuebles",main="Distribución de Precios en Bogotá") #sacamos la frecuencia para cada uno de los braquets

data.frame(range = ranges, frequency = freq$counts) #tabla de frecuencia con bins

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
  labs(title = "Pie Chart")

pie_pt

#OPERATION TYPE
table(train$operation_type) #todos son para la venta

#TITLE
head(db$title)

#DESCRIPTION
head(test$description)
substr(db$description, 1, 5000) # evalúo las primeras 5000 palabras del texto

# Limpio texto
db$description <- stri_trans_general(str = db$description, id = "Latin-ASCII") # Elimino acentos
db$description <- gsub('[^A-Za-z0-9 ]+', ' ', db$description)
db$description <- documento <- tolower(db$description)


# Getting info from Description -------------------------------------------
#Reemplazando
db$description <- gsub("\\b(?<!\\s)(\\d+)(m2|mt2|mts2|metros|metro|m)\\b", " \\1 \\2", db$description, perl = TRUE) # reestructuro todo el texto relacionado con metros cuadrados
db$description <- gsub("\\b(mt[a-z]?[0-9]+)(m2[0-9]+)\\b", "mts", db$description) #para arreglar errores cuando ponen por ejemplo 230mts23 y cuando hay m2 concatenado a mas numeros e.g. obs 360
db$description <- gsub("\\bm2\\b|\\bmt2\\b|\\bmts2\\b|\\bmtrs2\\b", "m", db$description) #para evitar problemas con los "2" cuando hacemos los loops para sacar el area

#Tokenization
db$tokens<-tokenize_words(db$description) #esto corta todas las palabras

db$ntokens<-tokenize_ngrams(x=db$description,
                           lowercase=TRUE, #convierte todo a lower case, aunque ya estaba, just in case
                           n=3L, #lenght del n-gram (trigram en este caso)
                           n_min=3L, #solo se hacen de 3 
                           stopwords=character(), #stopwords que sean excluidas del tokenization. está vacío
                           ngram_delim=" ", #tokens separados por espacios
                           simplify=FALSE) #se crea lista de trigrams

db$n2tokens<-tokenize_ngrams(x=db$description, #uno de 2 para lo de areass
                            lowercase=TRUE, #convierte todo a lower case, aunque ya estaba, just in case
                            n=2L, #lenght del n-gram (trigram en este caso)
                            n_min=2L, #solo se hacen de 3 
                            stopwords=character(), #stopwords que sean excluidas del tokenization. está vacío
                            ngram_delim=" ", #tokens separados por espacios
                            simplify=FALSE) #se crea lista de trigrams
substr(db$description, 1, 1000)

#Stop words
palabras1<-stopwords(language="es",source="snowball")
palabras2<-stopwords(language="es", source="nltk")

palabras<-union(palabras1,palabras2)
palabras<-stri_trans_general(str=palabras,id="Latin-ASCII")#sacamos las tildes
palabras

for (i in seq_along(db$tokens)) { #eliminamos las stopwords de tokens
  db$tokens[[i]]<-setdiff(db$tokens[[i]],palabras)
} 
db$tokens[[2]]

#sacamos grams que comiencen o finalicen en palabras stop
#3gram
db$ntokens <- lapply(db$ntokens, function(row) { #aplica una función a cada fila de db$ntokens
  row[!sapply(row, function(ngram) {
    words <- unlist(strsplit(ngram, "\\s+")) #parte cada ngram en palabras y lo cuarda en words
    words[1] %in% palabras || words[length(words)] %in% palabras #chequea si la primera o ultima palabra en words 
                                                                #está en palabras usando %in%. Si alguna condición es verdad
                                                                #se marca para removerla
  })]
})

#Stemming- FALTA
#db$tokens<-wordStem(db$tokens, "spanish")

#Variables of interest
caracteristicas<-c("parqueadero","chimenea","balcon",
                   "vigilancia", "gimnasio","jardin","bbq",
                   "parrilla","estudio","ascensor")
ncaracteristicas<-c("casa multifamiliar","cuarto de servicio", "zona de servicio","conjunto cerrado")


# Iterating through the list and create dummy variables

#tokens de una palabra
for (i in seq_along(db$tokens)) {
  for (j in seq_along(db$tokens[[i]])) {
    for (k in seq_along(caracteristicas)) {
      if (grepl(caracteristicas[k], db$tokens[[i]][[j]], ignore.case = TRUE)) {
        db[i, caracteristicas[k]] <- 1
        break
      }
    }
  }
}

#tokens de 3 palabra
for (i in seq_along(db$ntokens)) {
  for (j in seq_along(db$ntokens[[i]])) {
    for (k in seq_along(ncaracteristicas)) {
      if (grepl(ncaracteristicas[k], db$ntokens[[i]][[j]], ignore.case = TRUE)) {
        db[i, ncaracteristicas[k]] <- 1
        break
      }
    }
  }
}

#Replacing dummy variables with 0s 

db <- db %>% 
  mutate(parqueadero = coalesce(parqueadero, 0),
         chimenea = coalesce(chimenea, 0),
         balcon= coalesce(balcon,0),
         vigilancia= coalesce(vigilancia,0),
         gimnasio= coalesce(gimnasio,0),
         jardin=coalesce(jardin,0),
         bbq=coalesce(bbq,0),
         parrilla=coalesce(parrilla,0),
         estudio=coalesce(estudio,0),
         ascensor=coalesce(ascensor,0),
         `casa multifamiliar`=coalesce(`casa multifamiliar`,0),
         `cuarto de servicio`= coalesce(`cuarto de servicio`,0),
         `zona de servicio`=coalesce(`zona de servicio`,0),
         `conjunto cerrado`=coalesce(`conjunto cerrado`,0))

#Buscando areas
db$area_texto <- sapply(db$n2tokens, function(tokens) {
  area_ngram <- grep("\\b(area|metro|metros|mt|mets|cuadrado|cuadrados|m|metro|mts|mtrs|,mtr)\\b", tokens, ignore.case = TRUE, value = TRUE)
  if (length(area_ngram) > 0) {
    numbers <- gsub("\\D+", "", area_ngram)
    as.numeric(numbers)
  } else {
    NA
  }
})

db$area_texto[is.na(db$area_texto)] <- 0 #reemplazando los NAs
db$area_texto[sapply(db$area_texto, function(x) all(is.na(x)))] <- 0 #reemplazando los que tienen c(NA,NA...)

db$area_texto <- sapply(db$area_texto, function(x) na.omit(unlist(x))) #sacamos de los elementos que tienen NAs y números, solo en numero
db$area_texto <- sapply(db$area_texto, function(x) max(x, na.rm = TRUE)) #sacamos de los elementos que tienen varios números, el número más alto

#juntando informacion de areas
db$area <- pmax(db$surface_total, db$surface_covered, na.rm = TRUE) #primero ponemos el area mas grande entre surface_total y surface_covered
db$area <- coalesce(db$area, db$area_texto) #reemplazamos la variable area con el valor sacado de la descripción en caso area sea NA

sum(db$area==0) #cuantos aún tienen missing

#Revisando progreso
arreglar <- db[db$area == 0, ] #para ver una lista de lo que falta arreglar
arreglar <- arreglar[, c("description", "n2tokens")] #dejamos solo las dos variables de interes para revisar con facilidad

#Buscando baños
db$bano_texto <- sapply(db$n2tokens, function(tokens) {
  match <- grep("\\b(\\d+)\\s*bano(s)?\\b", tokens, ignore.case = TRUE, value = TRUE)
  if (length(match) > 0) {
    number <- gsub("\\D+", "", match)
    as.numeric(number)
  } else {
    NA
  }
})

#cambiar palabras uno. dos. etc a numeros

#los que tienen NA contaran cuantas veces se repite la palabra bano
if (any(is.na(db$bano_texto))) {
  counts <- sapply(db$description, function(tokens) {
    sum(grepl("\\b(bano|banos)\\b", tokens, ignore.case = TRUE))
  })
  db$bano_texto[is.na(db$bano_texto)] <- counts[is.na(db$bano_texto)]
}

sum(db$bano_texto==NA) #cuantos aún tienen missing
#Buscando Rooms

#piso



