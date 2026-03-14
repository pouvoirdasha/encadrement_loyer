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
# 2. Restriction aux années 2012, 2017, 2022
# =========================================================

df_bal <- df_reduced %>%
  filter(annee %in% c(2012, 2017, 2022))

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
# 4. Construction des différences
# =========================================================

df_ebal <- df_wide %>%
  mutate(
    d_12_17 = y_2017 - y_2012,
    d_17_22 = y_2022 - y_2017
  ) %>%
  filter(
    !is.na(y_2012),
    !is.na(y_2017),
    !is.na(y_2022),
    !is.na(d_12_17),
    !is.na(d_17_22)
  )

table(df_ebal$treated, useNA = "ifany")
stopifnot(length(unique(df_ebal$treated)) == 2)

# =========================================================
# 5. Entropy balancing
#    On veut repondérer les contrôles pour matcher Paris
#    Donc Treatment = treated
# =========================================================

X_ebal <- as.matrix(df_ebal %>% select(d_12_17))

eb_out <- ebalance(
  Treatment = df_ebal$treated,
  X = X_ebal
)

# =========================================================
# 6. Attribution correcte des poids
#    IMPORTANT : ne pas utiliser ifelse()
# =========================================================

df_ebal <- df_ebal %>%
  mutate(w_ebal = 1)

df_ebal$w_ebal[df_ebal$treated == 0] <- eb_out$w

# =========================================================
# 7. Vérification du calage
# =========================================================

cat("\n--- Vérification du matching sur d_12_17 ---\n")

mean_treated_pre <- mean(df_ebal$d_12_17[df_ebal$treated == 1], na.rm = TRUE)
mean_control_pre_unw <- mean(df_ebal$d_12_17[df_ebal$treated == 0], na.rm = TRUE)
mean_control_pre_w <- weighted.mean(
  df_ebal$d_12_17[df_ebal$treated == 0],
  df_ebal$w_ebal[df_ebal$treated == 0],
  na.rm = TRUE
)

print(data.frame(
  groupe = c("Paris", "Contrôle non pondéré", "Contrôle pondéré"),
  d_12_17 = c(mean_treated_pre, mean_control_pre_unw, mean_control_pre_w)
))

# =========================================================
# 8. Effet estimé sur 2017-2022
# =========================================================

mean_treated_post <- weighted.mean(
  df_ebal$d_17_22[df_ebal$treated == 1],
  df_ebal$w_ebal[df_ebal$treated == 1],
  na.rm = TRUE
)

mean_control_post <- weighted.mean(
  df_ebal$d_17_22[df_ebal$treated == 0],
  df_ebal$w_ebal[df_ebal$treated == 0],
  na.rm = TRUE
)

ate_17_22 <- mean_treated_post - mean_control_post
ate_17_22

mod_17_22 <- lm(
  d_17_22 ~ treated,
  data = df_ebal,
  weights = w_ebal
)

summary(mod_17_22)

mod_17_22_ctrl <- lm(
  d_17_22 ~ treated + d_12_17,
  data = df_ebal,
  weights = w_ebal
)

summary(mod_17_22_ctrl)

# =========================================================
# 9. Préparer les données de plot
# =========================================================

df_plot <- df_ebal %>%
  select(IRIS, code_com, treated, w_ebal, y_2012, y_2017, y_2022) %>%
  pivot_longer(
    cols = c(y_2012, y_2017, y_2022),
    names_to = "periode",
    values_to = "outcome"
  ) %>%
  mutate(
    annee = recode(periode,
                   y_2012 = 2012L,
                   y_2017 = 2017L,
                   y_2022 = 2022L),
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
# 10. Plot des niveaux
# =========================================================

ggplot(trend_plot, aes(x = annee, y = mean_outcome, color = groupe, group = groupe)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  geom_vline(xintercept = 2017, linetype = "dashed") +
  scale_x_continuous(breaks = c(2012, 2017, 2022)) +
  labs(
    title = "Évolution de nb_RP_en_loc : Paris vs contrôle pondéré",
    subtitle = "Calage par entropy balancing sur d_12_17 uniquement",
    x = "Année",
    y = "Moyenne pondérée de nb_RP_en_loc",
    color = "Groupe"
  ) +
  theme_minimal()

# =========================================================
# 11. Plot indexé sur 2012 = 0
# =========================================================

trend_plot_index <- trend_plot %>%
  group_by(groupe) %>%
  mutate(
    base_2012 = mean_outcome[annee == 2012],
    evol_depuis_2012 = mean_outcome - base_2012
  ) %>%
  ungroup()

ggplot(trend_plot_index, aes(x = annee, y = evol_depuis_2012, color = groupe, group = groupe)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  geom_vline(xintercept = 2017, linetype = "dashed") +
  scale_x_continuous(breaks = c(2012, 2017, 2022)) +
  labs(
    title = "Évolution cumulée depuis 2012",
    subtitle = "Pré-trend imposé sur la variation 2012-2017",
    x = "Année",
    y = "Variation par rapport à 2012",
    color = "Groupe"
  ) +
  theme_minimal()

# =========================================================
# 12. Diagnostic des poids
# =========================================================

cat("\n--- Résumé des poids des contrôles ---\n")
summary(df_ebal$w_ebal[df_ebal$treated == 0])

ggplot(
  df_ebal %>% filter(treated == 0),
  aes(x = w_ebal)
) +
  geom_histogram(bins = 40) +
  labs(
    title = "Distribution des poids entropy balancing",
    x = "Poids",
    y = "Nombre d'observations"
  ) +
  theme_minimal()


# graphique année par année"
# =========================================================
# 1. Rattacher les poids à la base annuelle complète
# =========================================================

weights_iris <- df_ebal %>%
  select(IRIS, code_com, treated, w_ebal) %>%
  distinct()

df_micro <- df_reduced %>%
  filter(annee >= 2012, annee <= 2017) %>%
  left_join(weights_iris, by = c("IRIS", "code_com", "treated")) %>%
  filter(!is.na(w_ebal)) %>%
  mutate(
    groupe = ifelse(treated == 1, "Paris", "Contrôle pondéré")
  )

# Vérification rapide
table(df_micro$groupe, useNA = "ifany")
range(df_micro$annee, na.rm = TRUE)
# =========================================================
# 2. Moyennes pondérées par année
# =========================================================

trend_micro <- df_micro %>%
  group_by(groupe, annee) %>%
  summarise(
    mean_outcome = weighted.mean(nb_RP_en_loc, w_ebal, na.rm = TRUE),
    .groups = "drop"
  )

print(trend_micro)
# =========================================================
# 3. Graphique micro en niveau
# =========================================================

ggplot(trend_micro, aes(x = annee, y = mean_outcome, color = groupe, group = groupe)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2.2) +
  scale_x_continuous(breaks = 2012:2017) +
  labs(
    title = "Tendance annuelle 2012–2017",
    subtitle = "Paris vs contrôle pondéré (poids ebal fixés sur l'échantillon 2012/2017/2022)",
    x = "Année",
    y = "Moyenne pondérée de nb_RP_en_loc",
    color = "Groupe"
  ) +
  theme_minimal()

# ATT
mod_ATT <- lm(
  d_17_22 ~ treated,
  data = df_ebal,
  weights = w_ebal
)

summary(mod_ATT)

# on calcule l'effet total sur Paris
ATT <- coef(mod_17_22)["treated"]
ATT

n_iris_paris <- df_ebal %>%
  filter(treated == 1) %>%
  summarise(n = n()) %>%
  pull(n)

effet_total <- ATT * n_iris_paris
effet_total

#calcule en pourcentage
total_paris_2017 <- df_ebal %>%
  filter(treated == 1) %>%
  summarise(total = sum(y_2017, na.rm = TRUE)) %>%
  pull(total)

n_iris_paris <- sum(df_ebal$treated == 1)

effet_total <- ATT * n_iris_paris

effet_total_pct <- (effet_total / total_paris_2017) * 100
effet_total_pct
