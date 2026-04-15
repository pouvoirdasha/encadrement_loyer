# idée : faire des tests sur des clusters sur certaines variables 
library(tidyverse)
library(MatchIt)
library(FactoMineR) # Pour une éventuelle ACP préalable 
library(data.table)
library(explor)
library(dplyr)
library(readxl)
# ------ Données 

data<- read_csv("data/base_2006_2022_avec_aire.csv")

# Calcul de la population par commune en 2022 pour choisir les communes de + de 10 000 hab
liste_com <- data %>%
  filter(annee == 2022) %>%
  group_by(COM) %>%
  summarise(pop = sum(nb_personnes_en_RP, na.rm = TRUE)) %>%
  filter(pop > 10000) %>%
  pull(COM) # Extrait uniquement la colonne COM sous forme de vecteur

# Rajouter les colonnes departement et commune avec le reference IRIS de l'INSEE 
path_ref <- "data/reference_IRIS_geo2022.xlsx" # Adaptez le chemin si nécessaire

ref_iris <- read_excel(path_ref, skip = 5) %>%
  select(CODE_IRIS, DEPCOM, LIBCOM) %>%
  # On s'assure que CODE_IRIS est au format caractère pour correspondre à IRIS sinon pas match
  mutate(CODE_IRIS = as.character(CODE_IRIS))

# Jointure (Left Join) pour ajouter DEPCOM et LIBCOM à la base
# On fait correspondre "IRIS" (votre base) et "CODE_IRIS" (référence)
data <- data %>%
  mutate(IRIS = as.character(IRIS)) %>% # Précaution de type
  left_join(ref_iris, by = c("IRIS" = "CODE_IRIS"))

# Créer une variable binaire == 1 pour paris et Lille 

data <- data %>%
  mutate(
    # On normalise en minuscules pour faciliter la comparaison
    lib_norm = str_to_lower(LIBCOM),
    traitement = case_when(
      # Condition 1 : Paris ET Arrondissement (utilisation du "AND" logique &)
      str_detect(lib_norm, "paris") & str_detect(lib_norm, "arrondissement") ~ 1,
      # Condition 2 : Lille exactement 
      lib_norm == "lille" ~ 1,
      # Tout le reste est le groupe de contrôle
      TRUE ~ 0
    )
  ) %>%
  # On supprime la colonne temporaire de normalisation
  select(-lib_norm)


# Filtrage final des communes par 10 000 habitants
df <- data %>%
  filter(COM %in% liste_com)
setDT(df)

# Création des variables nécessaires pour le matching dans le df original 
df[, `:=`(
  taux_vacance   = nb_logements_vacants / (nb_logements + 1),
  part_cadres    = nb_cadres / (nb_actifs_occ + 1),
  log_logements  = log1p(nb_logements),
  densite_pop    = nb_personnes_en_RP / aire,
  part_hlm       = nb_RP_HLM / (nb_RP + 1),
  part_etudiants = nb_etudiants / (nb_personnes_en_RP + 1),
  part_ouvriers  = nb_ouvriers / (nb_actifs_occ + 1),
  taux_chomage   = nb_chomeurs / (nb_actifs + 1),
  part_1p        = nb_RP_1_piece / (nb_RP + 1),
  part_2p        = nb_RP_2_pieces / (nb_RP + 1),
  part_3p        = nb_RP_3_pieces / (nb_RP + 1)
)]

# important : exclure les communes dans la petite couronne (92, 93, 94) + limitrophes à Lille ! 


# pour lille - depcom à exclure:   59328, 59356,	59457, 59470, 59128, 59196, 59195, 59566, 59360, 59527, 59368, 59220, 	59648, 59378, 59410, 59009, 59346, 59507  

excl_lille <- c("59328", "59356", "59457", "59470", "59128", "59196", "59195", 
                "59566", "59360", "59527", "59368", "59220", "59648", "59378", 
                "59410", "59009", "59346", "59507")

# Nettoyage du dataset original
df <- df %>%
  # Exclure la petite couronne (92, 93, 94)
  filter(!str_starts(DEPCOM, "92|93|94")) %>%
  # Exclure les communes limitrophes de Lille
  filter(!DEPCOM %in% excl_lille)


#### 1 ------------------ Matching Paris & Lille taitement (ensemble) et sur les années 2007, 2012 et 2017 seulement
# Garder seuelment les données pour les années avant traitement pour les analyses - matching tous les 5 ans
# Sélection des années et calcul des variables
# On s'assure que df_etude est un data.table
df_etude <- df[annee %in% c(2007, 2012, 2017)]
setDT(df_etude)



# 2. Passage en format large pour le matching sur trajectoires
vars_matching <- c("taux_vacance","taux_chomage", "densite_pop", "part_cadres", "part_ouvriers", 
                   "part_etudiants", "part_hlm", "part_1p", "part_2p", "part_3p", "log_logements")

df_wide <- df_etude %>%
  select(IRIS, LIBCOM, annee, traitement, all_of(vars_matching)) %>%
  pivot_wider(
    id_cols = c(IRIS, LIBCOM, traitement),
    names_from = annee,
    values_from = all_of(vars_matching),
    names_glue = "{.value}_{annee}"
  ) %>%
  drop_na() %>%
  as.data.frame() # Conversion pour la stabilité de MatchIt

# 3. Création sécurisée de la formule (avec backticks)
# On exclut les colonnes ID (IRIS, LIBCOM) et la variable dépendante (traitement)
covariates <- setdiff(names(df_wide), c("IRIS", "LIBCOM", "traitement"))
formula_str <- paste("traitement ~", paste(paste0("`", covariates, "`"), collapse = " + "))
formula_match <- as.formula(formula_str)

# 4. Exécution du Matching
set.seed(123)
mod_match_wide <- matchit(
  formula_match,
  data = df_wide,
  method = "nearest",
  distance = "glm",
  replace = FALSE
)

# Extraction des résultats
df_pairs_wide <- match.data(mod_match_wide)
iris_retenus <- df_pairs_wide$IRIS



# message("Export terminé : data_matching_did_export.csv créé avec ", 
#         nrow(df_export_did), " lignes.")
df_paires <- match.data(mod_match_wide) %>%
  select(IRIS, LIBCOM, traitement, distance, subclass)

# 2. Réorganiser pour avoir une ligne par paire (Traitement vs Contrôle)
mapping_paires <- df_paires %>%
  pivot_wider(
    id_cols = subclass,
    names_from = traitement,
    values_from = c(IRIS, LIBCOM, distance),
    names_glue = "{.value}_{if_else(traitement == 1, 'trait', 'ctrl')}"
  ) %>%
  # Calcul de la différence de distance absolue entre les deux
  mutate(diff_distance = abs(distance_trait - distance_ctrl)) %>%
  select(subclass, IRIS_trait, LIBCOM_trait, IRIS_ctrl, LIBCOM_ctrl, diff_distance)


# Export CSV des paires
write_csv(mapping_paires, "matching_paires_2007_12_17_Paris_Lille.csv")


##### 2 - MATCHING Paris Spécifiquement - 2007, 2012, 2017 
# 
# On retire Lille du pool pour ne garder que Paris en traitement et le reste en contrôle
df_paris_pool <- df[LIBCOM != "Lille"]
setDT(df_paris_pool)
# Variables de matching
vars_matching <- c("taux_vacance","taux_chomage", "densite_pop", "part_cadres", "part_ouvriers", 
                   "part_etudiants", "part_hlm", "part_1p", "part_3p", "log_logements")

# ==============================================================================
# VARIANTE 1 : Matching sur 3 points (2007, 2012, 2017)
# ==============================================================================

df_3pts_wide <- df_paris_pool[annee %in% c(2007, 2012, 2017)] %>%
  select(IRIS, LIBCOM, annee, traitement, all_of(vars_matching)) %>%
  pivot_wider(id_cols = c(IRIS, LIBCOM, traitement), names_from = annee, 
              values_from = all_of(vars_matching), names_glue = "{.value}_{annee}") %>%
  drop_na() %>% as.data.frame()

# Formule dynamique
covs_3pts <- setdiff(names(df_3pts_wide), c("IRIS", "LIBCOM", "traitement"))
form_3pts <- as.formula(paste("traitement ~", paste(paste0("`", covs_3pts, "`"), collapse = " + ")))

set.seed(123)
mod_3pts <- matchit(form_3pts, data = df_3pts_wide, method = "nearest", distance = "lasso")

# Export des paires
match.data(mod_3pts) %>%
  select(subclass, IRIS, LIBCOM, traitement, distance) %>%
  pivot_wider(names_from = traitement, values_from = c(IRIS, LIBCOM, distance), 
              names_prefix = "g") %>%
  write_csv("matching_paires_Paris_3pts_2007_2017.csv")


# ==============================================================================
# VARIANTE 2 : Matching sur toutes les années (2006 - 2017)
# ==============================================================================
# 1. On s'assure que chaque IRIS a une ligne pour chaque année (2006-2017)
df_full_prepared <- df_paris_pool[annee %in% 2006:2017] %>%
  select(IRIS, LIBCOM, annee, traitement, all_of(vars_matching)) %>%
  # Force la présence de toutes les années pour chaque IRIS
  complete(IRIS, annee = 2006:2017) %>%
  # On récupère LIBCOM et traitement qui ont sauté avec complete()
  group_by(IRIS) %>%
  fill(LIBCOM, traitement, .direction = "downup") %>%
  # Imputation par la moyenne de l'IRIS (si une année isolée manque)
  mutate(across(all_of(vars_matching), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .))) %>%
  ungroup()

# 2. Passage en format large
df_full_wide <- df_full_prepared %>%
  pivot_wider(
    id_cols = c(IRIS, LIBCOM, traitement),
    names_from = annee, 
    values_from = all_of(vars_matching), 
    names_glue = "{.value}_{annee}"
  ) %>% 
  drop_na() # Ne devrait plus supprimer Paris maintenant

# vite fait une verif pour voir que Paris est ok
n_paris <- sum(df_full_wide$traitement == 1)
message("Nombre d'IRIS parisiens conservés pour le matching : ", n_paris)

if(n_paris == 0) stop("Paris est encore vide. Vérifiez si 'traitement' est bien à 1 dans df_paris_pool.")

df_full_wide <- as.data.frame(df_full_wide)
# Formule dynamique
covs_full <- setdiff(names(df_full_wide), c("IRIS", "LIBCOM", "traitement"))
form_full <- as.formula(paste("traitement ~", paste(paste0("`", covs_full, "`"), collapse = " + ")))

set.seed(123)
mod_full <- matchit(form_full, data = df_full_wide, method = "nearest", distance = "lasso")

# Export des paires
match.data(mod_full) %>%
  select(subclass, IRIS, LIBCOM, traitement, distance) %>%
  pivot_wider(names_from = traitement, values_from = c(IRIS, LIBCOM, distance), 
              names_prefix = "g") %>%
  write_csv("matching_paires_Paris_Trajectoire_Full.csv")


###### 3 - MATCHING Lille spécifiquement 2007, 12, 17 et puis pour toutes les dates: 



###### 4 MATCHING Lille & Paris mais sur toutes les années depuis 2006. 

vars_matching <- c("taux_vacance","taux_chomage", "densite_pop", "part_cadres", "part_ouvriers", 
                   "part_etudiants", "part_hlm", "part_1p", "part_2p", "part_3p", "log_logements")

df_full_prepared_both <- df[annee %in% 2006:2017] %>%
  select(IRIS, LIBCOM, annee, traitement, all_of(vars_matching)) %>%
  # Force la présence de toutes les années pour chaque IRIS
  complete(IRIS, annee = 2006:2017) %>%
  # On récupère LIBCOM et traitement qui ont sauté avec complete()
  group_by(IRIS) %>%
  fill(LIBCOM, traitement, .direction = "downup") %>%
  # Imputation par la moyenne de l'IRIS (si une année isolée manque)
  mutate(across(all_of(vars_matching), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .))) %>%
  ungroup()


df_full_wide_both <- df_full_prepared_both %>%
  pivot_wider(
    id_cols = c(IRIS, LIBCOM, traitement),
    names_from = annee, 
    values_from = all_of(vars_matching), 
    names_glue = "{.value}_{annee}"
  ) %>% 
  drop_na() # Ne devrait plus supprimer Paris maintenant

df_full_wide_both <- as.data.frame(df_full_wide_both) # Conversion pour la stabilité de MatchIt

# 3. Création sécurisée de la formule (avec backticks)
# On exclut les colonnes ID (IRIS, LIBCOM) et la variable dépendante (traitement)
covariates <- setdiff(names(df_wide), c("IRIS", "LIBCOM", "traitement"))
formula_str <- paste("traitement ~", paste(paste0("`", covariates, "`"), collapse = " + "))
formula_match <- as.formula(formula_str)

# 4. Exécution du Matching
set.seed(123)
mod_match_wide_both <- matchit(
  formula_match,
  data = df_full_wide_both,
  method = "nearest",
  distance = "lasso",
  replace = FALSE
)

# Extraction des résultats
df_pairs_wide_both <- match.data(mod_match_wide_both)
iris_retenus <- df_pairs_wide_both$IRIS




df_paires_both <- match.data(mod_match_wide_both) %>%
  select(IRIS, LIBCOM, traitement, distance, subclass)

# 2. Réorganiser pour avoir une ligne par paire (Traitement vs Contrôle)
mapping_paires_both <- df_paires_both %>%
  pivot_wider(
    id_cols = subclass,
    names_from = traitement,
    values_from = c(IRIS, LIBCOM, distance),
    names_glue = "{.value}_{if_else(traitement == 1, 'trait', 'ctrl')}"
  ) %>%
  # Calcul de la différence de distance absolue entre les deux
  mutate(diff_distance = abs(distance_trait - distance_ctrl)) %>%
  select(subclass, IRIS_trait, LIBCOM_trait, IRIS_ctrl, LIBCOM_ctrl, diff_distance)


# Export CSV des paires
write_csv(mapping_paires_both, "data/matching_paires_2006-17_Paris_Lille.csv")

write_csv(df, "data/bdd_finale_mathching.csv")



