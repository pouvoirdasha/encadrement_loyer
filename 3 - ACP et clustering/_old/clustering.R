# idée : faire des tests sur des clusters sur certaines variables 
library(tidyverse)
library(MatchIt)
library(FactoMineR) # Pour une éventuelle ACP préalable
library(data.table)
library(explor)
library(dplyr)

# 1. Préparation des données (Format Wide pour le matching)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
data<- read_csv2("../data/base_2012_2022.csv")

# Calcul de la population par commune en 2022
liste_com <- data %>%
  filter(annee == 2022) %>%
  group_by(COM) %>%
  summarise(pop = sum(nb_personnes_en_RP, na.rm = TRUE)) %>%
  filter(pop > 10000) %>%
  pull(COM) # Extrait uniquement la colonne COM sous forme de vecteur

# Rajouter les colonnes departement et commune avec le reference IRIS de l'INSEE 

# Créer une variable binaire == 1 pour paris et Lille 



# Filtrage final des communes par 10 000 habitants
df <- data %>%
  filter(COM %in% liste_com)
setDT(df)

# Garder seuelment les données pour 2017, 2016 et 2015 
df_etude <- df[annee %in% 2015:2017]


# Quels sont les IRIS à garder pour Paris et Lille? 


# Testons le clustering par k-pp ? 

