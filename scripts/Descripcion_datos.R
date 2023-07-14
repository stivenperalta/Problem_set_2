rm(list = ls()) # Limpiar Rstudio

pacman::p_load(
  rio,          # File import
  here,         # File locator
  skimr,        # get overview of data
  tidyverse,    # data management + ggplot2 graphics 
  gtsummary,    # summary statistics and tests
  rstatix,      # summary statistics and statistical tests
  janitor,      # adding totals and percents to tables
  scales,       # easily convert proportions to percents  
  flextable,    # converting tables to pretty images
  dplyr,
  expss
)

#Definir el directorio
path_script<-rstudioapi::getActiveDocumentContext()$path
path_folder<-dirname(path_script) 
setwd(path_folder)
getwd()

# Importing Data ----------------------------------------------------------

db<-st_read("../stores/db_cln.geojson")


# Description Statistics --------------------------------------------------
names(db) #revisamos las variables

db<-db %>% select (-"property_id",-"city",-"month",-"year",-"surface_total",-"surface_covered",-"bedrooms",-"bathrooms",-"operation_type",-"title",-"description",-"area_texto", -"bano_texto",-"geometry") #sacamos las que no son de interés
db$geometry<-NULL

skim(db)

summary(db$price)

db%>% tabyl(price) #simple table with n, percentage and valid percent

db%>%tabyl(LOCALIDAD,sample) #cross-tabulation
db%>%tabyl(LOCALIDAD,sample, property_type) #cross-tabulation

sum_tbl<-linelist %>%
  group_by(sample)%>%
  summarise(
    
  )


