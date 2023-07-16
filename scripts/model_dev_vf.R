#### Modelo 1- Lasso- K fold=2 ###
# 0. set-up
### Para saber en que directorio esta parado R### En lugar de Script Folder seria mi carpeta### ### R se para en la mismca carpeta en la que esta guardado el archivo### 
rm(list=ls())
SCRIPT_FOLDER=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(SCRIPT_FOLDER)
library(dplyr)
library(ggplot2)

###Agregacion Nico###
db_cln.geojson <- st_read("/Users/nicolas/Documents/Problem_set_2/scripts/db_cln.geojson")

#
# 1. load prices data
#
### se transforma de data frame para el aprendizaje de maquinas###se cambio de espaciales a data frame### por orden mental se deja en dataframe###
list.files()
prices_data=
  "db_cln.geojson" %>%
  sf::st_read() %>% #class()
  as.data.frame()
prices_data %>%
  nrow()

#
# basic descriptives
#

#
prices_data %>%
  ggplot(aes(x=price))+
  geom_histogram(
    fill=rgb(0,0,0,.3),
    color="white"
  )+
  geom_vline(
    xintercept = quantile(prices_data$price, na.rm=TRUE),
    linetype=2
  )
### tabla de frecuencias relativas para ver si el precio en nulo### con esa tabla de frecuencias se coge la cantidad de casas que tienen precio en valor nulo###
round(
  prop.table(table(is.na(prices_data$price)))*100,3
)

#
# 2. define some groups of variables
#

#### mirar una lista de identifica el tipo de datos por cada variable###
data_types=
  prices_data %>%
  sapply(function(data_col){
    class(data_col)
  })
table(data_types %>% unlist())
data_types
#### se idenfican cuales variables son numericas### R las lee como numericas###
num_cols=data_types[
  data_types %>%
    sapply(function(d){ any(d %in% c("numeric", "integer")) })
] %>%
  names()
num_cols

#### dplyr mas de 90 verbos para manipulacion de tablas### sapply voy a hacer algo por cada columna ### al final me quedo con las variables completas, que no tienen valores perdidos #### 
complete_num_cols=num_cols[
  prices_data %>%
    dplyr::select(num_cols) %>%
    sapply(function(data_col){ 
      sum( is.na(data_col) )==0
    })
] 
complete_num_cols

#### voy a tratar las siguientes variables y estan categoricas ####
string_cols=data_types[
  data_types %>%
    sapply(function(d){ any(d %in% c("character")) })
] %>%
  names()
string_cols

### identificar las variables que no tienen varianza por ejemplo constantes, estadisticamente no aportan nada###
zero_variance_cols=
  names(prices_data)[
    prices_data %>%
      sapply(function(data_col){
        length(unique(data_col))==1
      })
  ]
zero_variance_cols

### asi como la varianza 0 es un problema, en las categoricas no sirve tener mucha varianza, porque por cada valor voy a obtener una observacion, y eso no me sirve####
excess_variance_categorical_cols=
  names(prices_data%>%
          dplyr::select(string_cols))[
            prices_data %>%
              dplyr::select(string_cols) %>%
              sapply(function(data_col){
                length(unique(data_col))>100
              })
          ]
excess_variance_categorical_cols

#
# 3. select features per type, then transform
#

#
# (1) numeric -> standardize
#

#### por cada variable numerica la voy a estandarizar y luego vuelva a armar la tabla ### primero eran una tabla, luego una lista de vectores y luego las vuelvo a concatenar para armar la tabla, ej: va recorriendo variable por variable y la estandariza###
prices_data.numeric_features.standardize=
  prices_data %>%
  dplyr::select(c(complete_num_cols)) %>% ###filtro sobre las variables, es decir, escoger variables
  lapply(function(data_col){.   ###hago el ciclo 
    
    (data_col-mean(data_col, na.rm=TRUE))/sd(data_col, na.rm=TRUE)
    ### el precio quedó al final de la base de datos y no se estandarizó###
  }) %>%
  bind_cols() %>%
  dplyr::mutate(price=prices_data$price)
  #### lo vuelvo a poner en tabla###
prices_data.numeric_features.standardize %>%
  View()
#### ciclo por cada una de las columnas, hago unos calculos y se pegan verticalmente###
prices_data.numeric_features.standardize %>%
  lapply(function(data_col){
    class(data_col)
    data.frame(
      data.frame(
        "MU"=mean(data_col, na.rm=TRUE),
        "SD"=sd(data_col, na.rm=TRUE)
      )
    )
  }) %>%
  bind_rows()

#
# (2) categorical -> one hot encoding
#


####
prices_data.categorical_features.one_hot_encoding=
  prices_data %>%
  dplyr::select(string_cols) %>% ### me quedo con las de texto
  dplyr::select(-zero_variance_cols) %>% ### me quedo con las varían es decir, excluyo las constantes
  dplyr::select(-excess_variance_categorical_cols) %>% ### excluyo las que tienen muchas categorias como descripcion y barrio
  
  lapply(function(data_col){
    
    data_col[is.na(data_col)]="MISS";data_col ### imputacion de los valores perdidos, crear la categoria de valor perdido## si hay un valor perdido lo lee como otra categoria###
    
  }) %>%
  bind_cols() %>%
  {
    model.matrix(~., data=.) ###matriz de dummies###
  } %>%
  as.data.frame()
prices_data.categorical_features.one_hot_encoding %>%
  names()
#
prices_data.categorical_features.one_hot_encoding %>%
  View()

#
# 3. ensemble single estimation data
### tenemos en este momento dos matrices, y ahora se pegan horizontalmente para armar la matriz de estimacion###

est_data=
  list(
    prices_data.numeric_features.standardize,
    prices_data.categorical_features.one_hot_encoding 
  ) %>%
  bind_cols() 
#
est_data %>%
  View()

#
# 4. split on train (estimation , calibration) and test (evaluation)
#

### defino la data de entrenamiento como la que tiene precios ###
train_data=
  est_data %>%
  dplyr::filter(
    !is.na(price)
  ) 

### en la de entrenamiento una es de estimation y otra calibration, es decir, se divide en dos ###
train_subsamples=
  train_data %>%
  dplyr::mutate(
    subsample=sample(c("estimation", "calibration"),nrow(.), replace=TRUE)
  ) %>%
  split(.$subsample) %>%
  lapply(function(df){ ### es una forma compacta de crear un loop### me evalua directamente a la lista
    df %>%
      dplyr::select(-subsample)
  })
#
train_subsamples %>%
  lapply(dim)

#
test_data=
  est_data %>%
  dplyr::filter(
    is.na(price)
  )
nrow(test_data)

#
# 5. model as lasso / ridge
#

#
ALPHA_PER_MODEL=list(
  "lasso"=1,
  "ridge"=0
)
#
# coefficients plot
### modificacion del codigo del profesor para ver los lambdas
model_beta=
  names(ALPHA_PER_MODEL) %>%
  
  lapply(function(model){
    
    model_fit <- glmnet::glmnet(
      x = train_data %>% dplyr::select(-price)%>% as.matrix(),
      y = train_data$price %>% as.matrix(),
      alpha = ALPHA_PER_MODEL[model]  ###primero se hace para lasso y luego para Ridge
    )
    
    model_fit$beta %>%  ### crea la matriz de los betas, vamos a tener un grupo de betas para cada lambda y luego esa matriz la construye como una tabla
      as.matrix() %>%
      t() %>% 
      as_tibble() %>%
      mutate(lambda = model_fit$lambda) %>%
      
      tidyr::pivot_longer(
        cols = !lambda, 
        names_to = "predictor",
        values_to = "coeficientes"
      ) %>%
      dplyr::mutate(
        type=model
      )
    
  }) %>%
  bind_rows() 
model_beta%>%
  view()
  
### tabla auxiliar para identificar las variables mas importantes ### con el minimo lambdam cual es el maximo coeficiente### para las UPZ - las casas que estan en esas UPZ tienen un valor muy encima del resto de la muestra. 
max_importance_rank=
  model_beta  %>% 
  dplyr::arrange(lambda) %>%
  split(.$predictor) %>%
  lapply(function(df){
    data.frame(
      predictor=dplyr::first(df$predictor),
      max_coef=max(df$coeficientes)
    ) 
  }) %>%
  bind_rows() %>%
  dplyr::arrange(
    max_coef*(-1)
  )
max_importance_rank%>%
  View()

###
install.packages("ggrepel")
model_beta  %>% 
  dplyr::filter(
    predictor %in% setdiff(complete_num_cols, "area") ### aca es que se hace el filtro de las 50 primeras###
  ) %>%
  dplyr::arrange(lambda) %>%
  split(.$predictor) %>%
  lapply(function(df){
    df %>%
      dplyr::sample_frac(.5) %>%
      dplyr::arrange(lambda*(-1)) %>%
      dplyr::mutate(
        label_=ifelse(1:nrow(.)==sample(c(1:nrow(.)), 1), predictor,"")
      )
 
  }) %>%
  bind_rows() %>%
  ggplot(aes(x = lambda, y = coeficientes)) +
  geom_point(
    alpha=.3, aes(color = predictor)
  ) +
  geom_line(
    alpha=.3, aes(color = predictor)
  ) +
  ggrepel::geom_label_repel(
    aes(label=label_,fill= predictor,),
    max.overlaps = 500,
    size=2.5,
    alpha=.7
  )+
  theme(legend.position = "none")+
  facet_wrap(~type, nrow=2, scales="free")+
  scale_x_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10",
                                  scales::math_format(10^.x))
  ) +
  labs(title = "Coeficientes del modelo en función de la regularización", x = "Lambda", y = "Coeficientes") 

#
# 6 best model search
#

### aca queremos guardar el performance del modelo, es decir, los RMSE ###
model_performances=
  names(ALPHA_PER_MODEL) %>%
  lapply(function(model){
    
    model_fit <- glmnet::glmnet(
      x = train_subsamples$estimation %>% dplyr::select(-price)%>% as.matrix(),
      y = train_subsamples$estimation$price %>% as.matrix(),
      alpha = ALPHA_PER_MODEL[model]
    )
    model_preds =predict(model_fit, newx = as.matrix(train_subsamples$calibration %>% dplyr::select(-price)) )
    1:length(model_fit$lambda) %>% 
      lapply(function(lambda_index){
        
        y_hat_out2 <- model_preds[, lambda_index]
        rmse <- MLmetrics::RMSE(y_pred = y_hat_out2, y_true = train_subsamples$calibration$price)
        
        data.frame(
          lambda=model_fit$lambda[lambda_index],
          type=model,
          rmse=rmse
        )
      })
  }) %>%
  bind_rows() 

#  
model_performances %>%
  ggplot(aes(x = lambda, y = rmse, color = type)) +
  geom_point(
    alpha=.7
  ) +
  geom_line(
    alpha=.7
  ) +
  theme(legend.position = "none")+
  facet_wrap(~type, nrow=2, scales="free")+
  scale_x_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10",
                                  scales::math_format(10^.x))
  ) +
  labs(title = "RMSE del modelo en función de la regularización", x = "Lambda", y = "RMSE") 

#
# 7 predictions on the best model
### ordenar la tabla en orden decreciente ###

best_model=
  model_performances %>%
  dplyr::arrange(rmse) %>%
  head(1)
#
best_model$lambda
ALPHA_PER_MODEL[[best_model$type]]
#
best_model_fit <- glmnet::glmnet(
  x = train_subsamples$estimation %>% dplyr::select(-price)%>% as.matrix(),
  y = train_subsamples$estimation$price %>% as.matrix(),
  alpha =ALPHA_PER_MODEL[[best_model$type]],
  lambda = best_model$lambda
)
#
preds1=predict(best_model_fit, newx =  test_data %>% dplyr::select(-price)%>% as.matrix()) %>%
  unlist() %>%
  as.data.frame() 
names(preds)="price"
#
preds %>%
  View()



preds1 <- preds %>%
  mutate(price_rounded = round(price / 1000000) * 1000000)

preds1$price <- NULL

preds1 <- preds1 %>%
  rename(price = price_rounded)

#
preds1 %>%
  write.csv("/Users/nicolas/Downloads/ridge_lasso_nicolas/predicted_prices_rounded_Lasso.csv", row.names = FALSE)








#### aca hago cosas nuevas ####
df_test <- read_csv("/Users/nicolas/Documents/test.csv")

newpreds <- cbind(test_data, df_test)


%>%
  dplyr::select(-price) %>% ### me quedo con las varían es decir, excluyo las constantes
  dplyr::select(-excess_variance_categorical_cols)


df <- read_csv("/Users/nicolas/Documents/predicted_prices_Lasso_NB.csv")

df %>%
  write.csv("/Users/nicolas/Downloads/ridge_lasso_nicolas/predicted_prices_LassoG4.csv", row.names = FALSE)

######################################################################################################################


variable_names <- colnames(prices_data)
print(variable_names)

