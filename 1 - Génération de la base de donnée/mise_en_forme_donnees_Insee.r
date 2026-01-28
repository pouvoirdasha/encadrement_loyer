##### QUE FAIT CE FICHIER ? #####
# Ce fichier à pour but de mettre en forme
# les données issues du recensement de l'Insee
# pour en faire des bases exploitables dans notre étude



# 0 - Packages -----
library(data.table)

setwd("~/Telechargements")

#######################################
##### 1 - DONNEES LOGEMENTS ##### -----
#######################################

# On importe les bases logements à l'IRIS de 2017 à 2022
# On ne selectionne que les colonnes qui nous interesse


# Base logements 2022

d22 <- fread("base-ic-logement-2022.csv", encoding = "UTF-8")

d22 = d22[, c("IRIS", "COM", "P22_MEN", "P22_PMEN", "P22_LOG",
              "P22_RP_1P", "P22_RP_2P", "P22_RP_3P", "P22_RP_4P", "P22_RP_5PP",
              "P22_RP_LOC", "P22_RP_PROP",
              "P22_NPER_RP", "P22_NPER_RP_LOC", "P22_NPER_RP_PROP",
              "P22_RSECOCC", "P22_LOGVAC", "P22_RP")]


# Base logement 2020
d20 <- fread("base-ic-logement-2020.csv", encoding = "UTF-8")
d20 = d20[, c("IRIS", "COM", "P20_MEN", "P20_PMEN", "P20_LOG",
              "P20_RP_1P", "P20_RP_2P", "P20_RP_3P", "P20_RP_4P", "P20_RP_5PP",
              "P20_RP_LOC", "P20_RP_PROP",
              "P20_NPER_RP", "P20_NPER_RP_LOC", "P20_NPER_RP_PROP",
              "P20_RSECOCC", "P20_LOGVAC", "P20_RP")]

# Jointure entre les bases 2022 et 2020
df = merge(d22,
           d20,
           by = c("IRIS",
                  "COM"))

# Base 2019
d19 <- fread("base-ic-logement-2019.csv", encoding = "UTF-8")
d19 = d19[, c("IRIS", "COM", "P19_MEN", "P19_PMEN", "P19_LOG",
              "P19_RP_1P", "P19_RP_2P", "P19_RP_3P", "P19_RP_4P", "P19_RP_5PP",
              "P19_RP_LOC", "P19_RP_PROP",
              "P19_NPER_RP", "P19_NPER_RP_LOC", "P19_NPER_RP_PROP",
              "P19_RSECOCC", "P19_LOGVAC", "P19_RP")]

# Jointure
df = merge(df,
           d19,
           by = c("IRIS",
                  "COM"))


# Base 2018
d18 <- fread("base-ic-logement-2018.csv", encoding = "UTF-8")
d18 = d18[, c("IRIS", "COM", "P18_MEN", "P18_PMEN", "P18_LOG",
              "P18_RP_1P", "P18_RP_2P", "P18_RP_3P", "P18_RP_4P", "P18_RP_5PP",
              "P18_RP_LOC", "P18_RP_PROP",
              "P18_NPER_RP", "P18_NPER_RP_LOC", "P18_NPER_RP_PROP",
              "P18_RSECOCC", "P18_LOGVAC", "P18_RP")]

# Jointure
df = merge(df,
           d18,
           by = c("IRIS",
                  "COM"))

# Base 2017
d17 <- fread("base-ic-logement-2017.csv", encoding = "UTF-8")
d17 = d17[, c("IRIS", "COM", "P17_MEN", "P17_PMEN", "P17_LOG",
              "P17_RP_1P", "P17_RP_2P", "P17_RP_3P", "P17_RP_4P", "P17_RP_5PP",
              "P17_RP_LOC", "P17_RP_PROP",
              "P17_NPER_RP", "P17_NPER_RP_LOC", "P17_NPER_RP_PROP",
              "P17_RSECOCC", "P17_LOGVAC", "P17_RP")]

# Jointure
df = merge(df,
           d17,
           by = c("IRIS",
                  "COM"))

# Export des données en format large
# fwrite(df, "bdd_log-IRIS-2017_2022.csv")


# Pour terminer on exporte les données en format long
dt_long <- melt(
  df,
  id.vars = c("IRIS", "COM"),
  variable.name = "var",
  value.name = "val"
)

dt_long[, `:=`(
  TYPE  = substr(var, 1, 1),           # P ou C
  ANNEE = as.integer(paste0("20", substr(var, 2, 3))),
  VAR   = sub("^[PC][0-9]{2}_", "", var)
)]


dt_final <- dcast(
  dt_long,
  IRIS + COM + ANNEE ~ VAR,
  value.var = "val"
)

setnames(dt_final,
         c("LOG","LOGVAC","MEN",        
           "NPER_RP","NPER_RP_LOC","NPER_RP_PROP",
           "PMEN", "RP_1P","RP_2P",       
           "RP_3P","RP_4P", "RP_5PP",      
           "RP_LOC", "RP_PROP", "RSECOCC", "RP"),
         c("nb_logements", "nb_logements_vacants", "nb_menages",
           "nb_personnes_en_RP", "nb_personnes_en_RP_location", "nb_personnes_en_RP_proprio",
           "nb_personnes_menage", "nb_RP_1_piece", "nb_RP_2_pieces",
           "nb_RP_3_pieces", "nb_RP_4_pieces", "nb_RP_5_piece_et_plus",
           "nb_RP_en_loc", "nb_RP_proprio", "nb_residences_second_ou_occ", "nb_RP"))


# fwrite(dt_final, "bdd_log-IRIS-2017_2022-format_long.csv")