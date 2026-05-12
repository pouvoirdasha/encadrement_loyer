library(data.table)
library(dplyr)

# Charger les données
data <- fread("base_2006_2022.csv")

# Codes communes Paris (arrondissements)
paris_com <- as.character(75101:75120)

# Création dataset restreint
df_reduced <- data %>%
  mutate(code_com = as.character(COM)) %>%
  filter(code_com %in% paris_com)

# =========================================================
# 1. Compter le nombre d’IRIS distincts par année
# =========================================================

iris_paris_year <- df_reduced %>%
  mutate(iris = as.character(IRIS)) %>%   # adapte si le nom diffère
  group_by(annee) %>%                    # adapte si "year" au lieu de ANNEE
  summarise(
    n_iris = n_distinct(iris)
  ) %>%
  arrange(annee)

print(iris_paris_year)

iris_presence <- df_reduced %>%
  mutate(iris = as.character(IRIS)) %>%
  group_by(iris) %>%
  summarise(
    first_year = min(annee),
    last_year  = max(annee),
    n_years    = n_distinct(annee)
  )

# IRIS non présents sur toute la période
iris_instables <- iris_presence %>%
  filter(n_years < length(unique(df_reduced$annee)))

print(iris_instables)

na_iris_year <- df_reduced %>%
  group_by(annee) %>%
  summarise(
    n_na_iris = sum(is.na(IRIS))
  ) %>%
  arrange(annee)

print(na_iris_year)

na_by_year <- df_reduced %>%
  group_by(annee) %>%
  summarise(
    across(
      everything(),
      ~ sum(is.na(.)),
      .names = "na_{.col}"
    )
  ) %>%
  arrange(annee)

print(na_by_year)

library(tidyr)

na_long <- df_reduced %>%
  group_by(annee) %>%
  summarise(
    across(everything(), ~ sum(is.na(.)))
  ) %>%
  pivot_longer(
    cols = -annee,
    names_to = "variable",
    values_to = "n_na"
  ) %>%
  arrange(annee, desc(n_na))

print(na_long)

df_2012 <- df_reduced %>%
  filter(annee == 2012)

summary_2012 <- df_2012 %>%
  mutate(
    n_na_row = rowSums(is.na(across(everything()))),
    n_non_na_row = rowSums(!is.na(across(everything())))
  ) %>%
  summarise(
    n_obs = n(),
    min_non_na = min(n_non_na_row),
    max_non_na = max(n_non_na_row),
    mean_non_na = mean(n_non_na_row)
  )

print(summary_2012)

iris_bad <- df_reduced %>%
  filter(annee == 2012) %>%
  mutate(
    n_na = rowSums(is.na(across(everything())))
  ) %>%
  filter(n_na > 10)   # seuil ajustable

print(iris_bad$IRIS)

iris_problematic <- iris_bad$IRIS

check_over_time <- df_reduced %>%
  filter(IRIS %in% iris_problematic) %>%
  group_by(IRIS, annee) %>%
  summarise(
    n_na = sum(is.na(nb_menages)),  # ou une autre variable clé
    .groups = "drop"
  )

print(check_over_time)

iris_compare_clean <- df_reduced %>%
  filter(IRIS %in% iris_problematic,
         annee %in% c(2011, 2012, 2013)) %>%
  select(
    IRIS, annee,
    nb_menages,
    nb_personnes_menage,
    nb_logements,
    nb_actifs,
    nb_chomeurs
  ) %>%
  arrange(IRIS, annee)

print(iris_compare_clean)

zero_rows <- df_reduced %>%
  filter(code_com %in% paris_com) %>%
  mutate(
    all_zero = if_else(
      rowSums(across(
        -c(IRIS, COM, annee, code_com),
        ~ . == 0
      ), na.rm = TRUE) == 
        ncol(select(., -c(IRIS, COM, annee, code_com))),
      1, 0
    )
  )

# Vérifier combien
zero_rows %>%
  summarise(n_zero = sum(all_zero))

iris_zero <- zero_rows %>%
  filter(all_zero == 1) %>%
  distinct(IRIS)

print(iris_zero)

iris_zero_years <- zero_rows %>%
  filter(all_zero == 1) %>%
  select(IRIS, annee) %>%
  arrange(IRIS, annee)

print(iris_zero_years)

iris_always_zero <- zero_rows %>%
  group_by(IRIS) %>%
  summarise(
    all_zero_all_years = all(all_zero == 1)
  ) %>%
  filter(all_zero_all_years)

print(iris_always_zero)

iris_sometimes_zero <- zero_rows %>%
  group_by(IRIS) %>%
  summarise(
    n_zero_years = sum(all_zero),
    n_years = n()
  ) %>%
  filter(n_zero_years > 0 & n_zero_years < n_years)

print(iris_sometimes_zero)