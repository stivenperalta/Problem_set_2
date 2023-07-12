getwd()

prueba<-read_csv("../stores/data1.csv")
open.rds

prueba2<-readRDS("../stores/data1.rds")


###recobrando info
missing_ids <- db$property_id[!(db$property_id %in% data2$property_id)]
missing_data <- db[db$property_id %in% missing_ids, ]
print(missing_data)
