library(data.table)
library(dplyr)
library(tidyr)
library(ebal)

# On se base sur les clusters de l'ACP pour faire une préselection des communes qui pourront 
# être utilisée pour faire le contrôle de Paris
# On va faire un contrôle sur Paris au total pour faire un truc différent du controle synthetic

# lire le fichier CSV
communes_cluster_paris <- fread("communes_sans_petite_couronne.csv")
# récupérer le vecteur des communes
liste_com <- communes_cluster_paris$COM
#base total 
data <- fread("base_2006_2022.csv")
paris_com <- as.character(75101:75120)
df_reduced <- data[
  COM %in% liste_com | COM %in% paris_com
]
df_reduced <- df_reduced %>%
  mutate(treated = as.integer(COM %in% paris_com))

rm(data)

#--------------------------------------------------
# 1. Garder seulement les années utiles
#--------------------------------------------------
df_bal <- df_reduced %>%
  filter(annee %in% c(2006, 2011, 2017, 2022))

#--------------------------------------------------
# 2. Passer du format long au format large
#--------------------------------------------------
df_wide <- df_bal %>%
  select(IRIS, treated, annee, nb_RP_en_loc) %>%
  distinct() %>%
  pivot_wider(
    names_from = annee,
    values_from = nb_RP_en_loc,
    names_prefix = "y_"
  )

#--------------------------------------------------
# 3. Créer les variables de balancing
#--------------------------------------------------
df_wide <- df_wide %>%
  mutate(
    d_06_11 = y_2011 - y_2006,
    d_11_17 = y_2017 - y_2011,
    d_17_22 = y_2022 - y_2017
  )

#--------------------------------------------------
# 4. Vérifier les NA
#--------------------------------------------------
df_ebal <- df_wide %>%
  filter(
    !is.na(treated),
    !is.na(d_06_11),
    !is.na(d_11_17)
  )

# Si tu veux aussi caler sur le niveau 2017 :
df_ebal_2017 <- df_ebal %>%
  filter(!is.na(y_2017))

#--------------------------------------------------
# 5A. Entropy balancing sur les deux évolutions seulement
#--------------------------------------------------
X_controls <- df_ebal %>%
  filter(treated == 0) %>%
  select(d_06_11, d_11_17) %>%
  as.matrix()

target_treated <- df_ebal %>%
  filter(treated == 1) %>%
  summarise(
    d_06_11 = mean(d_06_11, na.rm = TRUE),
    d_11_17 = mean(d_11_17, na.rm = TRUE)
  ) %>%
  as.numeric()

eb_out <- ebalance(
  Treatment = df_ebal$treated,
  X = as.matrix(df_ebal %>% select(d_06_11, d_11_17))
)

# Ajouter les poids
df_ebal$w_ebal <- ifelse(df_ebal$treated == 1, 1, eb_out$w)

#--------------------------------------------------
# 5B. Entropy balancing sur les deux évolutions + niveau 2017
#--------------------------------------------------
eb_out_2017 <- ebalance(
  Treatment = df_ebal_2017$treated,
  X = as.matrix(df_ebal_2017 %>% select(d_06_11, d_11_17, y_2017))
)

df_ebal_2017$w_ebal <- ifelse(df_ebal_2017$treat == 1, 1, eb_out_2017$w)

#--------------------------------------------------
# 6. Vérifier le balancing
#--------------------------------------------------
check_balance <- function(data, vars, weight_var = "w_ebal") {
  res <- lapply(vars, function(v) {
    treated_mean <- weighted.mean(data[[v]][data$treated == 1],
                                  w = data[[weight_var]][data$treat == 1],
                                  na.rm = TRUE)
    control_mean <- weighted.mean(data[[v]][data$treated == 0],
                                  w = data[[weight_var]][data$treated == 0],
                                  na.rm = TRUE)
    data.frame(
      variable = v,
      mean_treated = treated_mean,
      mean_control_weighted = control_mean,
      diff = treated_mean - control_mean
    )
  })
  bind_rows(res)
}

# Balance sans niveau 2017
check_balance(df_ebal, c("d_06_11", "d_11_17"))

# Balance avec niveau 2017
check_balance(df_ebal_2017, c("d_06_11", "d_11_17", "y_2017"))

#--------------------------------------------------
# 7. Estimation de l’effet sur 2017-2022
#--------------------------------------------------
# Cas sans niveau 2017
mod1 <- lm(d_17_22 ~ treated, data = df_ebal, weights = w_ebal)
summary(mod1)

# Cas avec niveau 2017
mod2 <- lm(d_17_22 ~ treated, data = df_ebal_2017, weights = w_ebal)
summary(mod2)