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
