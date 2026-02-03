library(dplyr)
library(magrittr)
library(readxl)
setwd("~/ENSAE/3A/Projet PESSD/Création database/Donnees brutes")

logements_2012 <- read_excel("Logements/logements_2012.xls", skip = 5) %>% 
  rename_with(~ sub("^P12_", "", .x), starts_with("P12_")) %>%
  select(IRIS, COM, MEN, PMEN, LOG, RP_1P, RP_2P, RP_3P, RP_4P, RP_5PP, 
         RP_LOC, RP_PROP, NPER_RP, NPER_RP_LOC, NPER_RP_PROP, RSECOCC, LOGVAC, RP) %>%
  mutate(annee = 2012)

logements_2013 <- read_excel("Logements/logements_2013.xls", skip = 5) %>% 
  rename_with(~ sub("^P13_", "", .x), starts_with("P13_")) %>%
  select(IRIS, COM, MEN, PMEN, LOG, RP_1P, RP_2P, RP_3P, RP_4P, RP_5PP, 
         RP_LOC, RP_PROP, NPER_RP, NPER_RP_LOC, NPER_RP_PROP, RSECOCC, LOGVAC, RP) %>%
  mutate(annee = 2013)

logements_2014 <- read_excel("Logements/logements_2014.xls", skip = 5)%>% 
  rename_with(~ sub("^P14_", "", .x), starts_with("P14_")) %>%
  select(IRIS, COM, MEN, PMEN, LOG, RP_1P, RP_2P, RP_3P, RP_4P, RP_5PP, 
         RP_LOC, RP_PROP, NPER_RP, NPER_RP_LOC, NPER_RP_PROP, RSECOCC, LOGVAC, RP) %>%
  mutate(annee = 2014)

logements_2015 <- read_excel("Logements/logements_2015.xls", skip = 5) %>% 
  rename_with(~ sub("^P15_", "", .x), starts_with("P15_")) %>%
  select(IRIS, COM, MEN, PMEN, LOG, RP_1P, RP_2P, RP_3P, RP_4P, RP_5PP, 
         RP_LOC, RP_PROP, NPER_RP, NPER_RP_LOC, NPER_RP_PROP, RSECOCC, LOGVAC, RP) %>%
  mutate(annee = 2015)

logements_2016 <- read_excel("Logements/logements_2016.xls", skip = 5) %>% 
  rename_with(~ sub("^P16_", "", .x), starts_with("P16_")) %>%
  select(IRIS, COM, MEN, PMEN, LOG, RP_1P, RP_2P, RP_3P, RP_4P, RP_5PP, 
         RP_LOC, RP_PROP, NPER_RP, NPER_RP_LOC, NPER_RP_PROP, RSECOCC, LOGVAC, RP) %>%
  mutate(annee = 2016)

logements_2017 <- read_excel("Logements/logements_2017.xlsx", skip = 5) %>% 
  rename_with(~ sub("^P17_", "", .x), starts_with("P17_")) %>%
  select(IRIS, COM, MEN, PMEN, LOG, RP_1P, RP_2P, RP_3P, RP_4P, RP_5PP, 
         RP_LOC, RP_PROP, NPER_RP, NPER_RP_LOC, NPER_RP_PROP, RSECOCC, LOGVAC, RP) %>%
  mutate(annee = 2017)

logements_2018 <- read_excel("Logements/logements_2018.xlsx", skip = 5) %>% 
  rename_with(~ sub("^P18_", "", .x), starts_with("P18_")) %>%
  select(IRIS, COM, MEN, PMEN, LOG, RP_1P, RP_2P, RP_3P, RP_4P, RP_5PP, 
         RP_LOC, RP_PROP, NPER_RP, NPER_RP_LOC, NPER_RP_PROP, RSECOCC, LOGVAC, RP) %>%
  mutate(annee = 2018)

logements_2019 <- read_excel("Logements/logements_2019.xlsx", skip = 5) %>% 
  rename_with(~ sub("^P19_", "", .x), starts_with("P19_")) %>%
  select(IRIS, COM, MEN, PMEN, LOG, RP_1P, RP_2P, RP_3P, RP_4P, RP_5PP, 
         RP_LOC, RP_PROP, NPER_RP, NPER_RP_LOC, NPER_RP_PROP, RSECOCC, LOGVAC, RP) %>%
  mutate(annee = 2019)

logements_2020 <- read_excel("Logements/logements_2020.xlsx", skip = 5) %>% 
  rename_with(~ sub("^P20_", "", .x), starts_with("P20_")) %>%
  select(IRIS, COM, MEN, PMEN, LOG, RP_1P, RP_2P, RP_3P, RP_4P, RP_5PP, 
         RP_LOC, RP_PROP, NPER_RP, NPER_RP_LOC, NPER_RP_PROP, RSECOCC, LOGVAC, RP) %>%
  mutate(annee = 2020)

logements_2021 <- read_excel("Logements/logements_2021.xlsx", skip = 5) %>% 
  rename_with(~ sub("^P21_", "", .x), starts_with("P21_")) %>%
  select(IRIS, COM, MEN, PMEN, LOG, RP_1P, RP_2P, RP_3P, RP_4P, RP_5PP, 
         RP_LOC, RP_PROP, NPER_RP, NPER_RP_LOC, NPER_RP_PROP, RSECOCC, LOGVAC, RP) %>%
  mutate(annee = 2021)

logements_2022 <- read_excel("Logements/logements_2022.xlsx", skip = 5) %>% 
  rename_with(~ sub("^P22_", "", .x), starts_with("P22_")) %>%
  select(IRIS, COM, MEN, PMEN, LOG, RP_1P, RP_2P, RP_3P, RP_4P, RP_5PP, 
         RP_LOC, RP_PROP, NPER_RP, NPER_RP_LOC, NPER_RP_PROP, RSECOCC, LOGVAC, RP) %>%
  mutate(annee = 2022)

logements_2012_2022 <- bind_rows(logements_2012, logements_2013, logements_2014, logements_2015,
                                 logements_2016, logements_2017, logements_2018, logements_2019,
                                 logements_2020, logements_2021, logements_2022) %>%
  rename(
    nb_logements = LOG,
    nb_logements_vacants = LOGVAC,
    nb_menages = MEN,
    nb_personnes_en_RP = NPER_RP,
    nb_personnes_en_RP_location = NPER_RP_LOC,
    nb_personnes_en_RP_proprio = NPER_RP_PROP,
    nb_personnes_menage = PMEN,
    nb_RP_1_piece = RP_1P,
    nb_RP_2_pieces = RP_2P,
    nb_RP_3_pieces = RP_3P,
    nb_RP_4_pieces = RP_4P,
    nb_RP_5_piece_et_plus = RP_5PP,
    nb_RP_en_loc = RP_LOC,
    nb_RP_proprio = RP_PROP,
    nb_residences_second_ou_occ = RSECOCC,
    nb_RP = RP
  )

write.csv2(logements_2012_2022, "Logements/logements_2012_2022.csv", row.names = FALSE)