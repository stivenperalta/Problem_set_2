############################# problem set 2, predicciones ###################################
##Andrea Clavijo
# correo: ay.clavijo@uniandes.edu.co
# fecha: 14/07/2023
###############################################################################

# Limpiar Rstudio
rm(list = ls())

###Instalo paquetes
install.packages("pacman")
install.packages("mice")
library("mice")
require(stargazer)
library("pacman")
p_load("tidyverse", "sf", "naniar", "tidymodels", "readxl", "psych","ranger","glmnet","naniar","tidyverse", "caret", "glmnet", "ggplot2","ggraph","gt","vtable","spatialsample")

##Cargo la base de datos

db_cln <- readRDS("C:/Users/andye/OneDrive/Documentos/GitHub/Problem_set_2/stores/db_cln.rds")## Cargando la base da datos unificada

###quito de mi dataset las filas correspondientes a variables de texto
##que se usaron para tener el dato de area,baños y habitaciones.

db_cln <- rename(db_cln, i_rinas = i_riñas)
db_cln$ESTRATO <- as.factor(db_cln$ESTRATO)
db_cln <- subset(db_cln, select = -c(bathrooms, area_texto, bano_texto,description,
                                     rooms,surface_total,surface_covered, title, tipologia_ZIT,
                                     COD_UPZ, BARRIO, city, property_id, COD_SEC, COD_MZN,
                                     tegb, teat, UPZ))

###Valido en cuales variables me quedan missing values 

missing_values <- colSums(is.na(db_cln)) #sumo los NA's para cada variable
missing_table <- data.frame(Variable = names(missing_values), Missing_Values = missing_values) # lo reflejo en un data.frame
missing_table
rm(missing_table, missing_values)  

###Hago imputación de esos pocos datos que me dan missing values, siguiendo la recomendación de: 
###https://www.r-bloggers.com/2015/10/imputing-missing-data-with-r-mice-package/amp/
                                     
data_imp <- select(db_cln, EPE, i_rinas, i_narcoticos, i_orden,
                   i_maltrato, d_homicidios, d_lesiones, d_hurto_personas,
                   d_hurto_residencias, d_hurto_comercio, d_hurto_autos,
                   d_hurto_motos, d_hurto_bici, d_hurto_cel,d_sexual, d_violencia)  # Selecciono variables para imputar
data_imp$geometry <- NULL

mice_data <- mice(data_imp, m = 5, method = "pmm", seed = 201718234)# imputo con mice.impute.2lonly.pmm: Método de imputación para datos numéricos utilizando Predictive Mean Matching (PMM) con dos etapas (dos niveles).                                                                      

# Unifico valores imputados con valores de mi base maestra
data_imp_1 <- (data_imp)##creo una copia por si me sale error
data_imp_1 <- mice::complete(mice_data) # Una recomendación sería imputar sobre una base de copia para que, en caso de error, no tengan que correr todo el código nuevamente

vars_to_keep <- setdiff(names(db_cln), names(data_imp_1))
data_to_keep <- db_cln[vars_to_keep]
data_na <- cbind(data_to_keep, data_imp_1)
rm(vars_to_keep,data_imp_1,data_to_keep)

data1 <- data_na
##########################################################
############ Predicciones del precio   ###################
##########################################################

###Para empezar defino set de train y test para toda la base de datos

train_data <- data1 %>%
    filter(sample == "train") %>%
    st_drop_geometry()

test_data<-data1  %>%
  filter(sample=="test", LOCALIDAD == "CHAPINERO") %>% 
  st_drop_geometry()

#####Regresiones regularizadas

#1.2 Ridge

# Matrix of predictores (de todo el set de datos, voy a incluirlos todos)
X <- as.matrix(train_data %>% select(bedrooms, property_type, operation_type, estudio,	parqueadero,	balcon,	chimenea,	ascensor,	bbq,	gimnasio,	vigilancia,	jardin,	
                                     parrilla,	cuarto.servicio,	conjunto.cerrado,	zona.servicio,	area,	banos,	V_REF_22,	ESTRATO,	mzn_densidad,	mzn_n_viv,	
                                     mzn_n_hab,	dist_CC	, dist_TM,	dist_col,	dist_parq,	geometry,	EPE,	i_rinas,	i_narcoticos,	i_orden,	i_maltrato,	
                                     d_homicidios,	d_lesiones,	d_hurto_personas,	d_hurto_residencias,	d_hurto_comercio,	d_hurto_autos,	d_hurto_motos,	d_hurto_bici,	
                                     d_hurto_cel,	d_sexual, d_violencia))

Y <- train_data$price # creo variable predicha

#hacemos prediccion del modelo
ridge1 <- glmnet(
  x = X,
  y = Y,
  alpha = 0 #ridge
)


   










lasso_reg <- linear_reg(penalty = 0.001, mixture = 1) |> 
  set_engine("glmnet") |> 
  fit(price ~ year + month + bedrooms + property_type + lat + lon + tasa_hom + tasa_hurtor + n_schools + n_parks + n_ips + tiene_terraza + tiene_parqueadero + tiene_patio + tiene_deposito,
      data = train_final)

lasso_reg_predict <- predict(lasso_reg_fit, new_data = test_final)

lasso_reg_output <- test_final |> 
  select(property_id) |> 
  bind_cols(lasso_reg_predict) |> 
  rename(price = .pred)

write_csv(lasso_reg_output, "stores/Predictions/lasso_reg.csv")
