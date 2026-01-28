##### QUE FAIT CE FICHIER ? #####
# Ce fichier à pour but de faire la jointure
# entre les bases emplois et logements

library(data.table)

setwd("~/Telechargements")

### JOINTURE ENTRE LES DEUX -----
 
 
data_log = fread("bdd_log-IRIS-2017_2022-format_long.csv")

 data_demo = fread("bdd_activite_residents-IRIS-2017_2022-format_long.csv") 

 
df = merge(data_log,
           data_demo,
           by = c("IRIS", "COM", "ANNEE"),
           all = TRUE) 


# fwrite(df, "bdd_logements_demo-IRIS-2017_2022.csv")
