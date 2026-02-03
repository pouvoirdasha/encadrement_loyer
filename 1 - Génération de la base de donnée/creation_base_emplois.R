library(dplyr)
library(magrittr)
library(readxl)

# Configuration du dossier de travail
setwd("~/ENSAE/3A/Projet PESSD/Création database/Donnees brutes")

# --- 2012 ---
emplois_2012 <- read_excel("Emplois/emplois_2012.xls", skip = 5) %>% 
  select(IRIS, COM, 
         ACT1564 = C12_ACT1564, ACTOCC1564 = C12_ACTOCC1564, CHOM1564 = P12_CHOM1564, 
         ACTOCC1564_CS1 = C12_ACTOCC1564_CS1, ACTOCC1564_CS2 = C12_ACTOCC1564_CS2,
         ACTOCC1564_CS3 = C12_ACTOCC1564_CS3, ACTOCC1564_CS4 = C12_ACTOCC1564_CS4, 
         ACTOCC1564_CS5 = C12_ACTOCC1564_CS5, ACTOCC1564_CS6 = C12_ACTOCC1564_CS6, 
         ETUD1564 = P12_ETUD1564) %>%
  mutate(annee = 2012)

# --- 2013 ---
emplois_2013 <- read_excel("Emplois/emplois_2013.xls", skip = 5) %>% 
  select(IRIS, COM, 
         ACT1564 = C13_ACT1564, ACTOCC1564 = C13_ACTOCC1564, CHOM1564 = P13_CHOM1564, 
         ACTOCC1564_CS1 = C13_ACTOCC1564_CS1, ACTOCC1564_CS2 = C13_ACTOCC1564_CS2,
         ACTOCC1564_CS3 = C13_ACTOCC1564_CS3, ACTOCC1564_CS4 = C13_ACTOCC1564_CS4, 
         ACTOCC1564_CS5 = C13_ACTOCC1564_CS5, ACTOCC1564_CS6 = C13_ACTOCC1564_CS6, 
         ETUD1564 = P13_ETUD1564) %>%
  mutate(annee = 2013)

# --- 2014 ---
emplois_2014 <- read_excel("Emplois/emplois_2014.xls", skip = 5) %>% 
  select(IRIS, COM, 
         ACT1564 = C14_ACT1564, ACTOCC1564 = C14_ACTOCC1564, CHOM1564 = P14_CHOM1564, 
         ACTOCC1564_CS1 = C14_ACTOCC1564_CS1, ACTOCC1564_CS2 = C14_ACTOCC1564_CS2,
         ACTOCC1564_CS3 = C14_ACTOCC1564_CS3, ACTOCC1564_CS4 = C14_ACTOCC1564_CS4, 
         ACTOCC1564_CS5 = C14_ACTOCC1564_CS5, ACTOCC1564_CS6 = C14_ACTOCC1564_CS6, 
         ETUD1564 = P14_ETUD1564) %>%
  mutate(annee = 2014)

# --- 2015 ---
emplois_2015 <- read_excel("Emplois/emplois_2015.xls", skip = 5) %>% 
  select(IRIS, COM, 
         ACT1564 = C15_ACT1564, ACTOCC1564 = C15_ACTOCC1564, CHOM1564 = P15_CHOM1564, 
         ACTOCC1564_CS1 = C15_ACTOCC1564_CS1, ACTOCC1564_CS2 = C15_ACTOCC1564_CS2,
         ACTOCC1564_CS3 = C15_ACTOCC1564_CS3, ACTOCC1564_CS4 = C15_ACTOCC1564_CS4, 
         ACTOCC1564_CS5 = C15_ACTOCC1564_CS5, ACTOCC1564_CS6 = C15_ACTOCC1564_CS6, 
         ETUD1564 = P15_ETUD1564) %>%
  mutate(annee = 2015)

# --- 2016 ---
emplois_2016 <- read_excel("Emplois/emplois_2016.xls", skip = 5) %>% 
  select(IRIS, COM, 
         ACT1564 = C16_ACT1564, ACTOCC1564 = C16_ACTOCC1564, CHOM1564 = P16_CHOM1564, 
         ACTOCC1564_CS1 = C16_ACTOCC1564_CS1, ACTOCC1564_CS2 = C16_ACTOCC1564_CS2,
         ACTOCC1564_CS3 = C16_ACTOCC1564_CS3, ACTOCC1564_CS4 = C16_ACTOCC1564_CS4, 
         ACTOCC1564_CS5 = C16_ACTOCC1564_CS5, ACTOCC1564_CS6 = C16_ACTOCC1564_CS6, 
         ETUD1564 = P16_ETUD1564) %>%
  mutate(annee = 2016)

# --- 2017 ---
emplois_2017 <- read_excel("Emplois/emplois_2017.xlsx", skip = 5) %>% 
  select(IRIS, COM, 
         ACT1564 = C17_ACT1564, ACTOCC1564 = C17_ACTOCC1564, CHOM1564 = P17_CHOM1564, 
         ACTOCC1564_CS1 = C17_ACTOCC1564_CS1, ACTOCC1564_CS2 = C17_ACTOCC1564_CS2,
         ACTOCC1564_CS3 = C17_ACTOCC1564_CS3, ACTOCC1564_CS4 = C17_ACTOCC1564_CS4, 
         ACTOCC1564_CS5 = C17_ACTOCC1564_CS5, ACTOCC1564_CS6 = C17_ACTOCC1564_CS6, 
         ETUD1564 = P17_ETUD1564) %>%
  mutate(annee = 2017)

# --- 2018 ---
emplois_2018 <- read_excel("Emplois/emplois_2018.xlsx", skip = 5) %>% 
  select(IRIS, COM, 
         ACT1564 = C18_ACT1564, ACTOCC1564 = C18_ACTOCC1564, CHOM1564 = P18_CHOM1564, 
         ACTOCC1564_CS1 = C18_ACTOCC1564_CS1, ACTOCC1564_CS2 = C18_ACTOCC1564_CS2,
         ACTOCC1564_CS3 = C18_ACTOCC1564_CS3, ACTOCC1564_CS4 = C18_ACTOCC1564_CS4, 
         ACTOCC1564_CS5 = C18_ACTOCC1564_CS5, ACTOCC1564_CS6 = C18_ACTOCC1564_CS6, 
         ETUD1564 = P18_ETUD1564) %>%
  mutate(annee = 2018)

# --- 2019 ---
emplois_2019 <- read_excel("Emplois/emplois_2019.xlsx", skip = 5) %>% 
  select(IRIS, COM, 
         ACT1564 = C19_ACT1564, ACTOCC1564 = C19_ACTOCC1564, CHOM1564 = P19_CHOM1564, 
         ACTOCC1564_CS1 = C19_ACTOCC1564_CS1, ACTOCC1564_CS2 = C19_ACTOCC1564_CS2,
         ACTOCC1564_CS3 = C19_ACTOCC1564_CS3, ACTOCC1564_CS4 = C19_ACTOCC1564_CS4, 
         ACTOCC1564_CS5 = C19_ACTOCC1564_CS5, ACTOCC1564_CS6 = C19_ACTOCC1564_CS6, 
         ETUD1564 = P19_ETUD1564) %>%
  mutate(annee = 2019)

# --- 2020 ---
emplois_2020 <- read_excel("Emplois/emplois_2020.xlsx", skip = 5) %>% 
  select(IRIS, COM, 
         ACT1564 = C20_ACT1564, ACTOCC1564 = C20_ACTOCC1564, CHOM1564 = P20_CHOM1564, 
         ACTOCC1564_CS1 = C20_ACTOCC1564_CS1, ACTOCC1564_CS2 = C20_ACTOCC1564_CS2,
         ACTOCC1564_CS3 = C20_ACTOCC1564_CS3, ACTOCC1564_CS4 = C20_ACTOCC1564_CS4, 
         ACTOCC1564_CS5 = C20_ACTOCC1564_CS5, ACTOCC1564_CS6 = C20_ACTOCC1564_CS6, 
         ETUD1564 = P20_ETUD1564) %>%
  mutate(annee = 2020)

# --- 2021 ---
emplois_2021 <- read_excel("Emplois/emplois_2021.xlsx", skip = 5) %>% 
  select(IRIS, COM, 
         ACT1564 = C21_ACT1564, ACTOCC1564 = C21_ACTOCC1564, CHOM1564 = P21_CHOM1564, 
         ACTOCC1564_CS1 = C21_ACTOCC1564_CS1, ACTOCC1564_CS2 = C21_ACTOCC1564_CS2,
         ACTOCC1564_CS3 = C21_ACTOCC1564_CS3, ACTOCC1564_CS4 = C21_ACTOCC1564_CS4, 
         ACTOCC1564_CS5 = C21_ACTOCC1564_CS5, ACTOCC1564_CS6 = C21_ACTOCC1564_CS6, 
         ETUD1564 = P21_ETUD1564) %>%
  mutate(annee = 2021)

# --- 2022 ---
emplois_2022 <- read_excel("Emplois/emplois_2022.xlsx", skip = 5) %>%  
  select(IRIS, COM,  
         ACT1564    = C22_ACT1564, 
         ACTOCC1564 = C22_ACTOCC1564, 
         CHOM1564   = P22_CHOM1564, 
         # On pioche dans les colonnes ACTOCC (Occupés) et non ACT (Actifs)
         ACTOCC1564_CS1 = C22_ACTOCC1564_STAT_GSEC11,  
         ACTOCC1564_CS2 = C22_ACTOCC1564_STAT_GSEC12,
         ACTOCC1564_CS3 = C22_ACTOCC1564_STAT_GSEC13,  
         ACTOCC1564_CS4 = C22_ACTOCC1564_STAT_GSEC14,  
         ACTOCC1564_CS5 = C22_ACTOCC1564_STAT_GSEC15,  
         ACTOCC1564_CS6 = C22_ACTOCC1564_STAT_GSEC16,  
         ETUD1564 = P22_ETUD1564) %>%
  mutate(annee = 2022)

# --- ASSEMBLAGE FINAL ---
emplois_2012_2022 <- bind_rows(
  emplois_2012, emplois_2013, emplois_2014, emplois_2015, emplois_2016, 
  emplois_2017, emplois_2018, emplois_2019, emplois_2020, emplois_2021, 
  emplois_2022
) %>%
  rename(
    nb_actifs = ACT1564,
    nb_actifs_occ = ACTOCC1564,
    nb_agriculteurs = ACTOCC1564_CS1,
    nb_commercants = ACTOCC1564_CS2,
    nb_cadres = ACTOCC1564_CS3,
    nb_professions_inter = ACTOCC1564_CS4,
    nb_employes = ACTOCC1564_CS5,
    nb_ouvriers = ACTOCC1564_CS6,
    nb_chomeurs = CHOM1564,
    nb_etudiants = ETUD1564 
  )

write.csv2(emplois_2012_2022, "Emplois/emplois_2012_2022.csv", row.names = FALSE)