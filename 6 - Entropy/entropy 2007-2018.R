library(data.table)
library(dplyr)
library(tidyr)
library(ebal)
library(ggplot2)

# =========================================================
# 0. Lecture des données
# =========================================================

communes_cluster_paris <- fread("communes_sans_petite_couronne.csv")
liste_com <- as.character(communes_cluster_paris$COM)

data <- fread("base_2006_2022.csv")

paris_com <- as.character(75101:75120)

# =========================================================
# 1. Restriction à Paris + donor pool
# =========================================================

df_reduced <- data %>%
  mutate(code_com = as.character(COM)) %>%
  filter(code_com %in% liste_com | code_com %in% paris_com) %>%
  mutate(treated = as.integer(code_com %in% paris_com))

rm(data)

table(df_reduced$treated, useNA = "ifany")

# =========================================================
# 2. Restriction aux années utiles
#    Pré-traitement : 2007 à 2018
#    Post-traitement : 2022
# =========================================================

years_pre <- 2007:2018
years_all <- c(years_pre, 2022)

df_bal <- df_reduced %>%
  filter(annee %in% years_all)

table(df_bal$treated, useNA = "ifany")

# =========================================================
# 3. Passage en wide
# =========================================================

df_wide <- df_bal %>%
  select(IRIS, code_com, treated, annee, nb_RP_en_loc) %>%
  distinct() %>%
  pivot_wider(
    names_from = annee,
    values_from = nb_RP_en_loc,
    names_prefix = "y_"
  )

# =========================================================
# 4. Construction des différences annuelles 2007-2018
#    + différence post 2018-2022
# =========================================================

for (yr in 2007:2017) {
  varname <- paste0("d_", substr(yr, 3, 4), "_", substr(yr + 1, 3, 4))
  y1 <- paste0("y_", yr)
  y2 <- paste0("y_", yr + 1)
  df_wide[[varname]] <- df_wide[[y2]] - df_wide[[y1]]
}

df_wide <- df_wide %>%
  mutate(
    d_18_22 = y_2022 - y_2018
  )

# =========================================================
# 5. Restriction aux observations complètes
#    Il faut toutes les années 2007-2018 + 2022
# =========================================================

diff_vars <- paste0("d_", sprintf("%02d", 7:17), "_", sprintf("%02d", 8:18))

required_vars <- c(
  paste0("y_", 2007:2018),
  "y_2022",
  diff_vars,
  "d_18_22"
)

df_ebal <- df_wide %>%
  filter(if_all(all_of(required_vars), ~ !is.na(.)))

table(df_ebal$treated, useNA = "ifany")
stopifnot(length(unique(df_ebal$treated)) == 2)

# =========================================================
# 6. Entropy balancing
#    Matching sur TOUTES les différences annuelles pré-traitement
# =========================================================

X_ebal <- as.matrix(df_ebal %>% select(all_of(diff_vars)))

eb_out <- ebalance(
  Treatment = df_ebal$treated,
  X = X_ebal
)

# =========================================================
# 7. Attribution des poids
# =========================================================

df_ebal <- df_ebal %>%
  mutate(w_ebal = 1)

df_ebal$w_ebal[df_ebal$treated == 0] <- eb_out$w

# =========================================================
# 8. Vérification du calage sur chaque différence annuelle
# =========================================================

cat("\n--- Vérification du matching sur les différences annuelles 2007-2018 ---\n")

balance_check <- lapply(diff_vars, function(v) {
  data.frame(
    variable = v,
    paris = mean(df_ebal[[v]][df_ebal$treated == 1], na.rm = TRUE),
    controle_non_pondere = mean(df_ebal[[v]][df_ebal$treated == 0], na.rm = TRUE),
    controle_pondere = weighted.mean(
      df_ebal[[v]][df_ebal$treated == 0],
      df_ebal$w_ebal[df_ebal$treated == 0],
      na.rm = TRUE
    )
  )
}) %>%
  bind_rows()

print(balance_check)

# =========================================================
# 9. Effet estimé sur 2018-2022
# =========================================================

mean_treated_post <- weighted.mean(
  df_ebal$d_18_22[df_ebal$treated == 1],
  df_ebal$w_ebal[df_ebal$treated == 1],
  na.rm = TRUE
)

mean_control_post <- weighted.mean(
  df_ebal$d_18_22[df_ebal$treated == 0],
  df_ebal$w_ebal[df_ebal$treated == 0],
  na.rm = TRUE
)

ate_18_22 <- mean_treated_post - mean_control_post
ate_18_22

mod_18_22 <- lm(
  d_18_22 ~ treated,
  data = df_ebal,
  weights = w_ebal
)

summary(mod_18_22)

# Option : contrôle supplémentaire par les pré-trends annuels
formula_ctrl <- as.formula(
  paste("d_18_22 ~ treated +", paste(diff_vars, collapse = " + "))
)

mod_18_22_ctrl <- lm(
  formula_ctrl,
  data = df_ebal,
  weights = w_ebal
)

summary(mod_18_22_ctrl)

# =========================================================
# 10. Préparer les poids pour les graphiques annuels
# =========================================================

weights_iris <- df_ebal %>%
  select(IRIS, code_com, treated, w_ebal) %>%
  distinct()

# =========================================================
# 11. Préparer les données de plot (années de calage + 2022)
# =========================================================

plot_years <- c(2007:2018, 2022)

df_plot <- df_ebal %>%
  select(IRIS, code_com, treated, w_ebal, all_of(paste0("y_", plot_years))) %>%
  pivot_longer(
    cols = all_of(paste0("y_", plot_years)),
    names_to = "periode",
    values_to = "outcome"
  ) %>%
  mutate(
    annee = as.integer(sub("y_", "", periode)),
    groupe = ifelse(treated == 1, "Paris", "Contrôle pondéré")
  )

trend_plot <- df_plot %>%
  group_by(groupe, annee) %>%
  summarise(
    mean_outcome = weighted.mean(outcome, w_ebal, na.rm = TRUE),
    .groups = "drop"
  )

print(trend_plot)

# =========================================================
# 12. Plot des niveaux
# =========================================================

ggplot(trend_plot, aes(x = annee, y = mean_outcome, color = groupe, group = groupe)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  geom_vline(xintercept = 2018, linetype = "dashed") +
  scale_x_continuous(breaks = c(2007:2018, 2022)) +
  labs(
    title = "Évolution de nb_RP_en_loc : Paris vs contrôle pondéré",
    subtitle = "Calage par entropy balancing sur toutes les différences annuelles 2007-2018",
    x = "Année",
    y = "Moyenne pondérée de nb_RP_en_loc",
    color = "Groupe"
  ) +
  theme_minimal()

# =========================================================
# 13. Plot indexé sur 2007 = 0
# =========================================================

trend_plot_index <- trend_plot %>%
  group_by(groupe) %>%
  mutate(
    base_2007 = mean_outcome[annee == 2007],
    evol_depuis_2007 = mean_outcome - base_2007
  ) %>%
  ungroup()

ggplot(trend_plot_index, aes(x = annee, y = evol_depuis_2007, color = groupe, group = groupe)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  geom_vline(xintercept = 2018, linetype = "dashed") +
  scale_x_continuous(breaks = c(2007:2018, 2022)) +
  labs(
    title = "Évolution cumulée depuis 2007",
    subtitle = "Pré-trend calé sur toutes les variations annuelles 2007-2018",
    x = "Année",
    y = "Variation par rapport à 2007",
    color = "Groupe"
  ) +
  theme_minimal()

# =========================================================
# 14. Plot avec toutes les années 2007-2022
# =========================================================

df_micro_full <- df_reduced %>%
  filter(annee >= 2007, annee <= 2022) %>%
  left_join(weights_iris, by = c("IRIS", "code_com", "treated")) %>%
  filter(!is.na(w_ebal)) %>%
  mutate(
    groupe = ifelse(treated == 1, "Paris", "Contrôle pondéré")
  )

trend_full <- df_micro_full %>%
  group_by(groupe, annee) %>%
  summarise(
    mean_outcome = weighted.mean(nb_RP_en_loc, w_ebal, na.rm = TRUE),
    .groups = "drop"
  )

print(trend_full)

ggplot(trend_full, aes(x = annee, y = mean_outcome, color = groupe, group = groupe)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  geom_vline(xintercept = 2018, linetype = "dashed") +
  scale_x_continuous(breaks = 2007:2022) +
  labs(
    title = "Évolution de nb_RP_en_loc : Paris vs contrôle pondéré",
    subtitle = "Poids entropy balancing calibrés sur les pré-trends 2007-2018",
    x = "Année",
    y = "Moyenne pondérée de nb_RP_en_loc",
    color = "Groupe"
  ) +
  theme_minimal()

# =========================================================
# 15. Diagnostic des poids
# =========================================================

cat("\n--- Résumé des poids des contrôles ---\n")
summary(df_ebal$w_ebal[df_ebal$treated == 0])

ggplot(
  df_ebal %>% filter(treated == 0),
  aes(x = w_ebal)
) +
  geom_density(fill = "steelblue", alpha = 0.4) +
  labs(
    title = "Distribution des poids entropy balancing",
    x = "Poids",
    y = "Densité"
  ) +
  theme_minimal()

###


# =========================================================
# 16. Tendance annuelle fine sur la période pré-traitement
# =========================================================

df_micro <- df_reduced %>%
  filter(annee >= 2007, annee <= 2018) %>%
  left_join(weights_iris, by = c("IRIS", "code_com", "treated")) %>%
  filter(!is.na(w_ebal)) %>%
  mutate(
    groupe = ifelse(treated == 1, "Paris", "Contrôle pondéré")
  )

trend_micro <- df_micro %>%
  group_by(groupe, annee) %>%
  summarise(
    mean_outcome = weighted.mean(nb_RP_en_loc, w_ebal, na.rm = TRUE),
    .groups = "drop"
  )

print(trend_micro)

ggplot(trend_micro, aes(x = annee, y = mean_outcome, color = groupe, group = groupe)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2.2) +
  scale_x_continuous(breaks = 2007:2018) +
  labs(
    title = "Tendance annuelle 2007–2018",
    subtitle = "Paris vs contrôle pondéré",
    x = "Année",
    y = "Moyenne pondérée de nb_RP_en_loc",
    color = "Groupe"
  ) +
  theme_minimal()

# =========================================================
# 17. ATT total et en pourcentage
# =========================================================

ATT <- coef(mod_18_22)["treated"]
ATT

n_iris_paris <- sum(df_ebal$treated == 1)

effet_total <- ATT * n_iris_paris
effet_total

total_paris_2018 <- df_ebal %>%
  filter(treated == 1) %>%
  summarise(total = sum(y_2018, na.rm = TRUE)) %>%
  pull(total)

effet_total_pct <- (effet_total / total_paris_2018) * 100
effet_total_pct

# =========================================================
# Contribution des communes au groupe contrôle pondéré
# avec rattachement du nom via commune_2022.csv
# =========================================================

# 1. Somme des poids par commune du donor pool
poids_commune <- df_ebal %>%
  filter(treated == 0) %>%
  group_by(code_com) %>%
  summarise(poids_total = sum(w_ebal), .groups = "drop")

# 2. Lecture de la table de correspondance code commune -> nom commune
ref_communes <- fread("commune_2022.csv") %>%
  mutate(COM = as.character(COM))

# Supposons que la variable du nom s'appelle NOM_COM
# adapte si besoin (ex: LIBCOM, commune, NOM, etc.)
poids_commune <- poids_commune %>%
  left_join(
    ref_communes %>% select(COM, LIBELLE),
    by = c("code_com" = "COM")
  ) %>%
  mutate(
    nom_commune = ifelse(is.na(LIBELLE), code_com, LIBELLE)
  )

# 3. Graphique complet
ggplot(poids_commune, aes(x = reorder(nom_commune, poids_total), y = poids_total)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Contribution des communes au groupe contrôle pondéré",
    x = "Commune",
    y = "Poids total"
  ) +
  theme_minimal()

# 4. Top 30 des communes les plus contributrices
top_communes <- poids_commune %>%
  slice_max(order_by = poids_total, n = 30)

ggplot(
  top_communes,
  aes(x = reorder(nom_commune, poids_total), y = poids_total)
) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Top 30 des communes contribuant au contrefactuel de Paris",
    x = "Commune",
    y = "Poids total"
  ) +
  theme_minimal()