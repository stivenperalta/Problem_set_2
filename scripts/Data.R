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
       tokenizers) # Cargar varios paquetes al tiempo



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

db$ntoken<-tokenize_ngrams(x=db$description,
                           lowercase=TRUE, #convierte todo a lower case, aunque ya estaba, just in case
                           n=3L, #lenght del n-gram (trigram en este caso)
                           n_min=3L, #solo se hacen de 3 
                           stopwords=character(), #stopwords que sean excluidas del tokenization. está vacío
                           ngram_delim=" ", #tokens separados por espacios
                           simplify=FALSE) #se crea lista de trigrams

pc<-c("estar_de_tv","sala_de_estar","estudio_para_trabajo","alcoba_principal","bano privado","zona_social", "habitacion_principal","cuarto_de_servicio","cocina_cerrada","cocina_abierta","parqueadero_de_visitantes","ascensor_privado","centros_comerciales","centro_comercial","aparta_estudio","parqueadero_independiente")#para trabajar las palabras compuestas


head(db$ntoken)





