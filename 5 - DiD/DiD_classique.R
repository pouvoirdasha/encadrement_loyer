library(data.table)
library(ggplot2)
library(dplyr)


setwd("~/0 ENSAE/3A/S2/Projet_socio_eco/encadrement_loyer/5 - DiD")

data = fread("../base_2012_2022.csv", encoding = "UTF-8")


correspondance_COM = fread("../correspondance_com.csv",
                           sep = ";", encoding = "UTF-8")

# Mise en forme pour DiD


data_did = data[annee %in% c("2017", "2022")]

data_did[, post_encadrement := fifelse(annee == "2022", 1, 0)]

data_did[, treated := fifelse(substr(COM, 1, 3) == "751",
                              1,
                              fifelse(COM == 59350, 
                                      1,
                                      0))]

groupe_control = c("13001", "38185",
                   "13055", "44109",
                   "06088", "67482",
                   "83137", "31555")

data_did = data_did[treated == 1 | COM %in% groupe_control]

data_did[, Ville := NULL]
data_did[, Date.de.début.de.l.encadrement := NULL]
data_did[, date_encadrement:= NULL]


# Parallel trend assumption

data[, treated := fifelse(substr(COM, 1, 3) == "751",
                              1,
                              fifelse(COM == 59350, 
                                      1,
                                      0))]

data[, control := fifelse(COM %in% groupe_control, 1, 0)]

data_com_ok = data[treated ==1 | control == 1]
data_com_ok[, nb_RP_en_loc := fifelse(is.na(nb_RP_en_loc), 0, nb_RP_en_loc)]
data_com_ok[, post_encadrement := fifelse(annee == "2022", 1, 0)]

# Moyennes par groupe et année
df_summary = data_com_ok[!is.na(nb_RP_en_loc),
                  .(moyenne_nb_RP_en_loc = mean(nb_RP_en_loc)),
                  by = c("annee", "treated")]


ggplot(df_summary, aes(x = annee,
                       y = moyenne_nb_RP_en_loc,
                       color = factor(treated),
                       group = treated)) +
  
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  
  scale_x_continuous(
    breaks = unique(df_summary$annee)  # années entières seulement
  ) +
  
  scale_color_manual(
    values = c("#1b9e77", "#d95f02"),
    labels = c("Contrôle", "Traité")
  ) +
  
  labs(
    x = "Année",
    y = "Nombre moyen de RP en location",
    color = "Groupe"
  ) +
  
  theme_minimal(base_size = 13) +
  
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank(),
    axis.title = element_text(face = "bold")
  )


# Test avec un modèle ----
# On essaie de retrouver les parallel trends assumption
# en controlant par des variables

data_fit = copy(data_com_ok)

model_did <- lm(
  nb_RP_en_loc ~ treated*post_encadrement + 
  (nb_chomeurs + nb_agriculteurs + nb_commercants +
  nb_cadres + nb_professions_inter + nb_employes +
  nb_ouvriers + nb_etudiants + nb_RP_1_piece)*COM,
  data = data_fit
)


data_fit$y_hat <- predict(model_did, newdata = data_fit)

df_summary_adj <- data_fit[annee %in% as.character(2013:2019), .(y_adj = mean(y_hat)),
                           by = c("annee", "treated")]
  

ggplot(df_summary_adj,
       aes(x = annee, y = y_adj,
           color = factor(treated),
           group = treated)) +
  
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  
  scale_x_continuous(breaks = unique(df_summary_adj$annee)) +
  
  labs(
    x = "Année",
    y = "Moyenne ajustée (contrôle CSP + COM + lgmnts 1 pièce)",
    color = "Groupe"
  ) +
  
  theme_minimal(base_size = 13)
