# Charger les bibliothèques nécessaires
library(dplyr)
library(tidyr)
library(knitr)
library(kableExtra)
# 1. Lecture du fichier
df <- read_csv("8-Matching/regressions/coefs_Paris_Lille_2006_2017.csv")

# 2. Filtrage et Préparation
table_data <- df %>%
  filter(term == "G_i:T_t") %>%
  select(modele, estimate, std.error, p.value) # On retire matching ici pour le remettre manuellement

# 3. Remplissage MANUEL (Matching, R2, F-stat)
# Assure-toi d'avoir autant de valeurs que de lignes dans ta table
table_data <- table_data %>%
  mutate(
    matching = c("Appariement 2", "Appariement 2", "Appariement 2"), # À remplir
    R2 = c(0.45, 0.48, 0.50),                                      # À remplir
    F_stat = c(124.5, 130.2, 118.9)                                # À remplir
  )

# 4. Formatage (Standard Économétrique)
table_data <- table_data %>%
  mutate(
    sig = case_when(
      p.value < 0.01 ~ "***",
      p.value < 0.05 ~ "**",
      p.value < 0.1  ~ "*",
      TRUE           ~ ""
    ),
    Estimate = paste0(round(estimate, 3), sig),
    Std_Err = paste0("(", round(std.error, 3), ")"),
    P_Value = round(p.value, 4) # On garde la p-valeur formatée
  ) %>%
  # Sélection finale dans l'ordre souhaité
  select(matching, modele, Estimate, Std_Err, P_Value, R2, F_stat)

# 5. Génération du code LaTeX
latex_output <- kable(table_data, 
      format = "latex", 
      booktabs = TRUE, 
      col.names = c("Appariement", "Modèle", "Estimateur (DiD)", "Std. Error", "p-value", "$R^2$", "F-stat"),
      escape = FALSE, 
      caption = "Résultats de l'impact de l'encadrement des loyers (Estimateur $\\beta$)") %>%
  # RETRAIT de "striped" pour supprimer le cellcolor
  kable_styling(latex_options = c("hold_position")) %>% 
  add_header_above(c(" " = 2, "Résultats principaux" = 3, "Diagnostic" = 2))

# Afficher le code LaTeX
cat(latex_output)
