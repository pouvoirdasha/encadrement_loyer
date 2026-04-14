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

# Calcul de la population par commune en 2022
liste_com <- data %>%
  filter(annee == 2022) %>%
  group_by(COM) %>%
  summarise(pop = sum(nb_personnes_en_RP, na.rm = TRUE)) %>%
  filter(pop > 15000) %>%
  pull(COM) # Extrait uniquement la colonne COM sous forme de vecteur

# Rajouter les colonnes departement et commune avec le reference IRIS de l'INSEE 
path_ref <- "data/reference_IRIS_geo2022.xlsx" # Adaptez le chemin si nécessaire

ref_iris <- read_excel(path_ref, skip = 5) %>%
  select(CODE_IRIS, DEPCOM, LIBCOM) %>%
  # On s'assure que CODE_IRIS est au format caractère pour correspondre à IRIS
  mutate(CODE_IRIS = as.character(CODE_IRIS))

# 2. Jointure (Left Join) pour ajouter DEPCOM et LIBCOM à votre base
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

# Garder seuelment les données pour les années avant traitement. 
# 1. Sélection des années et calcul des variables
# On s'assure que df_etude est un data.table
df_etude <- df[annee %in% c(2007, 2012, 2017)]
setDT(df_etude)

df_etude[, `:=`(
  taux_vacance   = nb_logements_vacants / (nb_logements + 1),
  part_cadres    = nb_cadres / (nb_actifs_occ + 1),
  log_logements  = log1p(nb_logements),
  densite_pop    = nb_personnes_en_RP / (aire + 0.01),
  part_hlm       = nb_RP_HLM / (nb_RP + 1),
  part_etudiants = nb_etudiants / (nb_personnes_en_RP + 1),
  part_ouvriers  = nb_ouvriers / (nb_actifs_occ + 1),
  part_1p        = nb_RP_1_piece / (nb_RP + 1),
  part_2p        = nb_RP_2_pieces / (nb_RP + 1),
  part_3p        = nb_RP_3_pieces / (nb_RP + 1),
  part_loc       = nb_RP_en_loc / (nb_RP + 1)
)]

# 2. Passage en format large pour le matching sur trajectoires
vars_matching <- c("taux_vacance", "densite_pop", "part_cadres", "part_ouvriers", 
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
  as.data.frame() # Conversion cruciale pour la stabilité de MatchIt

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

# --- A. Extraire les IRIS matchés et leur groupe ---
# df_pairs_wide contient déjà une colonne 'traitement' (1 ou 0)
iris_matchés_status <- df_pairs_wide %>%
  select(IRIS, traitement) %>%
  rename(groupe_match = traitement)

# --- B. Créer la base finale pour le DiD ---
# On repart de la base 'df' (toutes les années) et on ne garde que les IRIS sélectionnés
df_export_did <- df %>%
  inner_join(iris_matchés_status, by = "IRIS") %>%
  # On s'assure de garder les variables d'identification et la cible
  select(IRIS, NOM_COM, LIBCOM, annee, groupe_match, nb_RP_en_loc, nb_RP, everything())

# --- C. Calcul de la variable cible si besoin ---
# Il est souvent plus propre d'exporter le ratio directement
df_export_did <- df_export_did %>%
  mutate(part_loc = nb_RP_en_loc / (nb_RP + 1))

# Exportation
write_csv(df_export_did, "data_matching_did_export.csv")

message("Export terminé : data_matching_did_export.csv créé avec ", 
        nrow(df_export_did), " lignes.")
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


# 3. Export CSV
write_csv(mapping_paires, "matching_paires.csv")

# --- 5. Préparation complète de df_long_plot ---
df_long_plot <- df %>%
  filter(IRIS %in% iris_retenus) %>%
  mutate(
    # Variable cible
    part_loc       = nb_RP_en_loc / (nb_RP + 1),
    
    # Variables de structure (doivent correspondre à vars_matching)
    taux_vacance   = nb_logements_vacants / (nb_logements + 1),
    part_cadres    = nb_cadres / (nb_actifs_occ + 1),
    log_logements  = log1p(nb_logements),
    densite_pop    = nb_personnes_en_RP / (aire + 0.001),
    part_hlm       = nb_RP_HLM / (nb_RP + 1),
    part_etudiants = nb_etudiants / (nb_personnes_en_RP + 1),
    part_ouvriers  = nb_ouvriers / (nb_actifs_occ + 1),
    part_1p        = nb_RP_1_piece / (nb_RP + 1),
    part_2p        = nb_RP_2_pieces / (nb_RP + 1),
    part_3p        = nb_RP_3_pieces / (nb_RP + 1),
    
    # Label de groupe
    Groupe = if_else(traitement == 1, "Traitement (Paris/Lille)", "Contrôle Matché")
  )

# --- 6. Plots des variables de contrôle ---

# On s'assure que vars_matching est bien défini
vars_matching <- c("taux_vacance", "densite_pop", "part_cadres", "part_ouvriers", 
                   "part_etudiants", "part_hlm", "part_1p", "part_2p", "part_3p", "log_logements")

# Variables qu'on souhaite spécifiquement afficher sur le graph
vars_to_plot <- c("densite_pop", "part_hlm", "part_cadres", "taux_vacance")

df_plot_ctrl <- df_long_plot %>%
  filter(annee %in% c(2007, 2012, 2017)) %>%
  group_by(annee, Groupe) %>%
  summarise(across(all_of(vars_matching), mean, na.rm = TRUE), .groups = "drop") %>%
  pivot_longer(cols = all_of(vars_matching), names_to = "Variable", values_to = "Valeur")

ggplot(df_plot_ctrl %>% filter(Variable %in% vars_to_plot), 
       aes(x = annee, y = Valeur, color = Groupe, group = Groupe)) +
  geom_line(linewidth = 1) + 
  geom_point() +
  facet_wrap(~ Variable, scales = "free_y") +
  scale_x_continuous(breaks = c(2007, 2012, 2017)) +
  labs(title = "Qualité du matching : Évolution des covariables",
       subtitle = "Moyennes par groupe pour les années de matching",
       x = "Année", y = "Valeur Moyenne") +
  theme_minimal() +
  theme(legend.position = "bottom")

# --- 7. Plots de la variable TARGET (Validation visuelle des tendances parallèles) ---

plot_target <- function(data, start_yr, end_yr) {
  data %>%
    filter(annee >= start_yr, annee <= end_yr) %>%
    group_by(annee, Groupe) %>%
    summarise(m_part_loc = mean(part_loc, na.rm = TRUE), .groups = "drop") %>%
    ggplot(aes(x = annee, y = m_part_loc, color = Groupe, group = Groupe)) +
    geom_line(linewidth = 1.2) +
    geom_point() +
    labs(title = paste("Vérification : Part des locataires (", start_yr, "-", end_yr, ")"),
         subtitle = "Si les courbes sont parallèles avant traitement, le matching est bon",
         y = "Moyenne Part Locataires", x = "Année") +
    theme_minimal() +
    theme(legend.position = "bottom")
}

# Affichage
p1 <- plot_target(df_long_plot, 2007, 2017)
p2 <- plot_target(df_long_plot, 2012, 2022)

print(p1)
print(p2)
