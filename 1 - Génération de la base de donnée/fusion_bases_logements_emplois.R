library(dplyr)
library(magrittr)
library(data.table)
library(readxl)

setwd("~/_ emplois log")


# logements_1 <- fread("logements_2012_2022.csv", sep = ";")
# logements_2 <- fread("logements_2006_2011.csv", sep = ";")
# logements = rbind(logements_1, logements_2)
# rm(logements_1, logements_2)

logements = fread("logements_2006_2022.csv")

emplois_1 <- fread("emplois_2012_2022.csv", sep = ";")
emplois_2 <- fread("emplois_2006_2011.csv", sep = ";")
emplois = rbind(emplois_1, emplois_2)
rm(emplois_1, emplois_2)


# On joint sur IRIS et annee
base_finale <- logements %>%
  left_join(emplois, by = c("IRIS", "annee"))

# Si COM existe dans les deux bases, dplyr crée COM.x et COM.y
if("COM.x" %in% names(base_finale)){
  base_finale <- base_finale %>%
    rename(COM = COM.x) %>%
    select(-any_of("COM.y"))
}

base_finale[is.na(base_finale)]<-0

# Traitement des IRIS ayant changé ----

chg_IRIS_2020_2024 <- as.data.table(read_excel("IRIS/reference_IRIS_geo2024.xlsx", sheet = 3, skip = 5))

table_de_passage = chg_IRIS_2020_2024[, c("IRIS_INI", "IRIS_FIN")]

setnames(table_de_passage, 
         c("IRIS_INI", "IRIS_FIN"),
         c("IRIS_2020", "IRIS_2024"))


chg_IRIS_2016_2020 <- as.data.table(read_excel("IRIS/reference_IRIS_geo2020.xlsx", sheet = 3, skip = 5))

setnames(chg_IRIS_2016_2020, 
         c("IRIS_INI", "IRIS_FIN"),
         c("IRIS_2016", "IRIS_2020"))

table_de_passage = merge(table_de_passage,
                         chg_IRIS_2016_2020[, c("IRIS_2016", "IRIS_2020")],
                         all = TRUE)

table_de_passage[, IRIS_2016 := fifelse(is.na(IRIS_2016), 
                                        IRIS_2020,
                                        IRIS_2016)]

table_de_passage[, IRIS_2024 := fifelse(is.na(IRIS_2024),
                                        IRIS_2020,
                                        IRIS_2024)]


chg_IRIS_2012_2016 <- as.data.table(read_excel("IRIS/reference_IRIS_geo2016.xls", sheet = 3, skip = 5))

setnames(chg_IRIS_2012_2016, 
         c("IRIS_INI", "IRIS_FIN"),
         c("IRIS_2012", "IRIS_2016"))

table_de_passage = merge(table_de_passage,
                         chg_IRIS_2012_2016[, c("IRIS_2012", "IRIS_2016")],
                         all = TRUE,
                         by = "IRIS_2016")

table_de_passage[, IRIS_2020 := fifelse(is.na(IRIS_2020), 
                                        IRIS_2016,
                                        IRIS_2020)]

table_de_passage[, IRIS_2024 := fifelse(is.na(IRIS_2024),
                                        IRIS_2020,
                                        IRIS_2024)]

table_de_passage[, IRIS_2012 := fifelse(is.na(IRIS_2012),
                                        IRIS_2016,
                                        IRIS_2012)]






# On récupère les aires des IRIS -----
# IRIS 2024 ----

IRIS = fread("aires_IRIS_2024.csv")

IRIS[, CODE_IRIS := as.character(CODE_IRIS)]

IRIS[, CODE_IRIS := fifelse(nchar(CODE_IRIS)==8,
                       paste0("0", CODE_IRIS),
                       CODE_IRIS)]

# IRIS 2019 ----
# On utilise l'aire des IRIS de 2019 pour compléter
# les données manquantes

IRIS_2019 = fread("aires_IRIS_2019.csv")


df_base_finale = merge(base_finale,
                       IRIS[, c("CODE_IRIS", "NOM_COM", "aire")],
                       all.x = TRUE,
                       by.x = "IRIS",
                       by.y = "CODE_IRIS")

df_base_finale = df_base_finale[substr(COM, 1, 2) != "97"]

a_corr = df_base_finale[is.na(aire)]

df_base_finale_ok = df_base_finale[!is.na(aire)]

a_corr[, NOM_COM := NULL]
a_corr[, aire := NULL]

a_corr_iris2019 = merge(a_corr,
                        IRIS_2019[, c("CODE_IRIS", "NOM_COM", "aire_2019")],
                        by.x = "IRIS",
                        by.y = "CODE_IRIS",
                        all.x = TRUE)

corr_2019 = a_corr_iris2019[!is.na(aire_2019)]
setnames(corr_2019, "aire_2019", "aire")

a_corr_supp = a_corr_iris2019[is.na(aire_2019)]

a_corr_supp[, NOM_COM := NULL]
a_corr_supp[, aire_2019 := NULL]


# IRIS 2015 ----
IRIS_2015 = fread("aires_IRIS_2015.csv")

a_corr_iris2015 = merge(a_corr_supp,
                        IRIS_2015[, c("CODE_IRIS", "NOM_COM", "aire_2015")],
                        by.x = "IRIS",
                        by.y = "CODE_IRIS",
                        all.x = TRUE)

corr_2015 = a_corr_iris2015[!is.na(aire_2015)]
setnames(corr_2015, "aire_2015", "aire")


a_corr_supp_supp = a_corr_iris2015[is.na(aire_2015)]
a_corr_supp_supp[, NOM_COM := NULL]
a_corr_supp_supp[, aire_2015 := NULL]


# IRIS 2010 ----
IRIS_2010 = fread("aires_IRIS_2010.csv")

a_corr_iris2010 = merge(a_corr_supp_supp,
                        IRIS_2010[, c("DCOMIRIS", "NOM_COM", "aire_2010")],
                        by.x = "IRIS",
                        by.y = "DCOMIRIS",
                        all.x = TRUE)

corr_2010 = a_corr_iris2010[!is.na(aire_2010)]
setnames(corr_2010, "aire_2010", "aire")

a_corr_supp_supp_supp = a_corr_iris2010[is.na(aire_2010)]

com_pb = a_corr_supp_supp_supp[, c("COM", "nb_personnes_menage")]

com_pb[nb_personnes_menage>4000]
# ce ne sont que des communes du 77 que l'on enlève ensuite

# On ajoute toutes les données pour finir

df_base_finale_ok = rbind(df_base_finale_ok,
                          corr_2019,
                          corr_2015,
                          corr_2010)

  
fwrite(df_base_finale_ok, "base_2006_2022_avec_aire.csv", row.names = FALSE)
