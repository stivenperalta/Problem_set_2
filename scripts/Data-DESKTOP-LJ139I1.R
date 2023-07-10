############################## Problem Set 2 ###################################
# Autores: 
# fecha: 03/07/2023

# Preparación -------------------------------------------------------------

rm(list = ls()) # Limpiar Rstudio

require(pacman)
p_load(ggplot2, rio, tidyverse, skimr, caret, 
       rvest, magrittr, rstudioapi, stargazer, 
       boot, readxl, knitr, kableExtra,
       glmnet, sf, tmaptools, leaflet,
       tokenizers, stopwords, SnowballC,
       stringi, dplyr) # Cargar varios paquetes al tiempo



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
  addCircles(data=db,col=~pal(sample))%>% #pintar casas en base ala funcion pal que creamos arriba
  
map 



# Checking existing variables ---------------------------------------------


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
train$date<-as.Date(paste(db$year, db$month,"1", sep = "-")) #se creo variable con formato YYYY-MM-01

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
tail(db$description)# parece que no hay tildes ni puntos ni comas ni mayúsculas

# Getting info from Description -------------------------------------------

#Tokenization
db$tokens<-tokenize_words(db$description) #esto corta todas las palabras
head(db$tokens)

db$ntokens<-tokenize_ngrams(x=db$description,
                           lowercase=TRUE, #convierte todo a lower case, aunque ya estaba, just in case
                           n=3L, #lenght del n-gram (trigram en este caso)
                           n_min=3L, #solo se hacen de 3 
                           stopwords=character(), #stopwords que sean excluidas del tokenization. está vacío
                           ngram_delim=" ", #tokens separados por espacios
                           simplify=FALSE) #se crea lista de trigrams

db$ntokens[[1]]

#Stop words
palabras1<-stopwords(language="es",source="snowball")
palabras2<-stopwords(language="es", source="nltk")

palabras<-union(palabras1,palabras2)
palabras<-stri_trans_general(str=palabras,id="Latin-ASCII")#sacamos las tildes
palabras<-c(palabras,"m2") #agregamos m2
palabras

for (i in seq_along(db$tokens)) { #eliminamos las stopwords de tokens
  db$tokens[[i]]<-setdiff(db$tokens[[i]],palabras)
} 
db$tokens[[2]]

#sacamos grams que comiencen o finalicen en palabras stop
db$ntokens <- lapply(db$ntokens, function(row) { #aplica una función a cada fila de db$ntokens
  row[!sapply(row, function(ngram) {
    words <- unlist(strsplit(ngram, "\\s+")) #parte cada ngram en palabras y lo cuarda en words
    words[1] %in% palabras || words[length(words)] %in% palabras #chequea si la primera o ultima palabra en words 
                                                                #está en palabras usando %in%. Si alguna condición es verdad
                                                                #se marca para removerla
  })]
})
db$ntokens[[2]]

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

#tokens de más de una palabra
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
         vigilancia= coalesce(seguridad,0),
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
db$area_texto <- sapply(db$ntokens, function(tokens) {
  area_ngram <- grep("\\barea\\b", tokens, ignore.case = TRUE, value = TRUE)
  if (length(area_ngram) > 0) {
    number <- gsub("\\D+", "", area_ngram)
    as.numeric(number)
  } else {
    NA
  }
})

db$area_texto[is.na(db$area_texto)] <- 0 #reemplazando los NAs
 #reemplazando los c(NA,Na..)

#Buscando baños

#Buscando Rooms

#superficie

