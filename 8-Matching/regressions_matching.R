# idée : faire des tests sur des clusters sur certaines variables 
library(tidyverse)
library(MatchIt)
library(FactoMineR) # Pour une éventuelle ACP préalable
library(data.table)
library(explor)
library(dplyr)
library(readxl)
# ------ Données 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
data<- read_csv2("../data/base_2012_2022.csv")
liste_appar1517 = read_csv2("matching_trajectoires_2015_2017.csv")
tokeep <- c(liste_appar1517$IRIS_controle, liste_appar1517$IRIS_traite)

data_reg <- data %>% 
  filter(IRIS %in% tokeep)

data_reg$Traitement <- 0
data_reg$Traitement[data_reg$IRIS %in% liste_appar1517$IRIS_traite] <- 1


# variables à utiliser nb de logement nb de menage taux de vacace, densité rp, nb de residences secondaire, nb une pièce nb deux pièces nb 3 pièces 

# on veut regresser nb_RP_en_loc sur Traitement, nb_menages, nb_logemnts, nb_personnes_en_RP_proprio, nb_residences_second_ou_occ, nb_logements_vacants, nb_chomeurs, nb_actifs, nb_cadres, nb_ouvriers, nb_employes, nb_etudiants. Annee - diff in diff donc entre 2017 et 2022. Paris seulement dabord, puis Lille seulement,

# clusteriser les ecarts types par commune !

# 1. Installation et chargement des packages nécessaires
if(!require(fixest)) install.packages("fixest")
library(fixest)
library(tidyverse)

# 2. Préparation des données pour le DiD
data_did <- data_reg %>%
  # On ne garde que les deux années d'intérêt
  filter(annee %in% c(2017, 2022)) %>%
  # Création de la variable temporelle Post (1 pour l'année de traitement, 0 avant)
  mutate(Post = if_else(annee == 2022, 1, 0))

# 3. Spécification des variables de contrôle
# On les met dans un vecteur pour plus de clarté
controls <- c(
  "nb_menages", "nb_logements", "nb_personnes_en_RP_proprio", 
  "nb_residences_second_ou_occ", "nb_logements_vacants", 
  "nb_chomeurs", "nb_actifs", "nb_cadres", 
  "nb_ouvriers", "nb_employes", "nb_etudiants"
)

# 4. Estimation du modèle Diff-in-Diff
# Formule : Y ~ Traitement * Post + contrôles
# On clusterise les erreurs au niveau de l'IRIS (essentiel en DiD !)
formula_did <- as.formula(
  paste("nb_RP_en_loc ~ Traitement * Post +", paste(controls, collapse = " + "))
)
formula_did1 <- as.formula(
  paste("nb_RP_en_loc ~ Traitement +", paste(controls, collapse = " + "))
)

mod_did <- feols(formula_did1, data = data_did, cluster = ~IRIS)

# 5. Affichage des résultats
summary(mod_did) 
