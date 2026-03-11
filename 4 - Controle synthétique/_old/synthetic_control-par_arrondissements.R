######## A QUOI SERT CE SCRIPT ? #########
# Ce script a pour but de réaliser
# un synthetic control par arrondissement.

# En effet, si on utlise Paris, le nombre
# de logements est beaucoup trop élevés
# pour recomposer un groupe controle
# avec des poids entre 0 et 1.
##########################################

##########################################
# 0 - PACKAGES ET IMPORT DES DONNEES -----
##########################################

library(data.table)
library(Synth)
library(ggplot2)

setwd("~/0 ENSAE/3A/S2/Projet_socio_eco/encadrement_loyer/4 - Controle synthétique")

data = fread("../base_2012_2022.csv", encoding = "UTF-8")


correspondance_COM = fread("../correspondance_com.csv",
                           sep = ";", encoding = "UTF-8")

# VARIABLES A AJUSTER ----
seuil_menages <- 10000  

# com = "75119"


#################################
# 1- PREPARATION DES DONNEES ----
#################################

# A - Aggrégation à la commune

num_cols <- names(data)[sapply(data, is.numeric)]
num_cols <- setdiff(num_cols, c("COM", "annee", "IRIS"))

data <- data[, 
                lapply(.SD, sum, na.rm = TRUE),
                by = .(COM, annee),
                .SDcols = num_cols
]

# B - Variables supplémentaires


# Ratio locataires / résidences principales (part de location)
data[, part_loc := nb_RP_en_loc / nb_RP]

# Taux de vacance
data[, taux_vacance := nb_logements_vacants / nb_logements]

# Densité résidentielle (personnes par logement RP)
data[, densite_RP := nb_personnes_en_RP / nb_RP]

# Taux de chômage approché
data[, taux_chomage := nb_chomeurs / (nb_actifs + 1)]

# Part cadres parmi actifs occupés
data[, part_cadres := nb_cadres / (nb_actifs_occ + 1)]

# Log des variables pour réduire l'asymétrie
data[, log_RP_loc    := log1p(nb_RP_en_loc)]
data[, log_menages   := log1p(nb_menages)]
data[, log_logements := log1p(nb_logements)]


# B - Filtrage sur les communes à exclure ----

# 1 - Grosses communes

# Pour Synth : on peut restreindre aux grandes villes (pop > seuil)
# pour améliorer la comparabilité
grandes_communes <- data[
  annee == 2012 & nb_personnes_menage > seuil_menages,
  .(COM)
]



# 2 - On exclut les communes d'Île de France
grandes_communes_hors_IDF = grandes_communes[
  !(substr(COM, 1, 2) %in% c("77", "78", "91", 
                             "92", "93", "94", "95"))
    ]


data_synth <- data[COM %in% grandes_communes_hors_IDF$COM]


cat("Ensemble des communes (avec Paris) :", uniqueN(data_synth$COM), "\n")

##################################################
# 2 - SYNTHETIC CONTROL 19E ARR. TOUS LES ANS ----
##################################################
sauv_pourcenge = c()

for (com in 75101:75120) {
  print(com)
  
  nom_graph = paste0("Paris, ", substr(com, 4, 5), "eme Arrond.")
  
  
# On ne garde que le 19e
data_19e = data_synth[
  !(substr(COM, 1, 5) %in% as.character(setdiff(75101:75120, com)))
    ]

# On vérifie qu'il y a bien 1 obs par COM et par an
obs_par_commune <- data_19e[, .(
  n_obs      = .N,
  annees_ok  = list(sort(unique(annee))),
  min_annee  = min(annee),
  max_annee  = max(annee)
), by = COM]


obs_par_commune[n_obs<11]


data_19e = data_19e[
  !(COM %in% obs_par_commune[n_obs<11, COM])
]

# Création d'un ID par commune
data_19e[, id_num := as.integer(factor(COM))]

id_paris <- unique(data_19e[COM == com]$id_num)

id_ctrl <- setdiff(unique(data_19e$id_num), id_paris)

annees_all <- sort(unique(data_19e$annee))


# Variables d'intérêt et formatage des données
# pour Synth

covariates_logement <- c(
  "nb_logements",
  "nb_menages",
  "taux_vacance",
  "densite_RP",
  "nb_residences_second_ou_occ",
  "nb_RP_1_piece",
  "nb_RP_2_pieces",
  "nb_RP_3_pieces"
)


# Periodes pre-traitement pour les predicteurs
pre_period  <- annees_all[annees_all < 2019]
post_period <- annees_all[annees_all >= 2019]




# Préparation du jeu de données

dataprep_out <- dataprep(
  foo                = as.data.frame(data_19e),
  predictors         = covariates_logement,
  predictors.op      = "mean",             # moyenne sur la période pré
  special.predictors = list(
    # On contrôle aussi le lag de la variable outcome à différentes dates
    list("nb_RP_en_loc", 2012, "mean"),
    list("nb_RP_en_loc", 2014, "mean"),
    list("nb_RP_en_loc", 2016, "mean"),
    list("nb_RP_en_loc", 2018, "mean")
  ),
  dependent          = "nb_RP_en_loc",
  unit.variable      = "id_num",
  unit.names.variable = "COM",
  time.variable      = "annee",
  treatment.identifier = id_paris,
  controls.identifier  = id_ctrl,
  time.predictors.prior = pre_period,
  time.optimize.ssr     = pre_period,
  time.plot             = annees_all
)


# B -  Fit et Premiers résultats  ----

synth_out <- synth(
  data.prep.obj = dataprep_out,
  method        = "BFGS",    # optimisation gradient
  verbose       = FALSE
)

# Tableau des poids
synth_tables <- synth.tab(
  dataprep.res = dataprep_out,
  synth.res    = synth_out
)

cat("\n=== POIDS DES COMMUNES DANS LE CONTRÔLE SYNTHÉTIQUE ===\n")
print(synth_tables$tab.w[synth_tables$tab.w[, "w.weights"] > 0.01, ])

res = as.data.table(merge(synth_tables$tab.w, 
            correspondance_COM[, c("Code géographique", "Libellé géographique")],
            by.x = "unit.names",
            by.y = "Code géographique"))

res[res$w.weights>0.01]

# cat("\n=== ÉQUILIBRE DES PRÉDICTEURS (",nom_graph, "vs Synthétique) ===\n")
# print(synth_tables$tab.pred)


cat("\n=== PLOT 1 ===\n")
jpeg(
  filename = paste0(nom_graph, "path_plot.jpeg"),
  width = 1200,   # largeur en pixels
  height = 800,   # hauteur en pixels
  res = 150       # résolution (dpi)
)

path.plot(
  synth.res = synth_out,
  dataprep.res = dataprep_out,
  Ylab = "nb_RP_en_loc",
  Xlab = "Année",
  Legend = c(nom_graph, paste0(nom_graph, " Synthétique")),
  Legend.position = "topleft"
)

abline(v = min(post_period), lty = 2)

dev.off()



cat("\n=== PLOT 2 ===\n")

jpeg(
  filename = paste0(nom_graph, "gaps_plot.jpeg"),
  width = 1200,   # largeur en pixels
  height = 800,   # hauteur en pixels
  res = 150       # résolution (dpi)
)

gaps.plot(
  synth.res = synth_out,
  dataprep.res = dataprep_out,
  Ylab = "Gap (Traitement - Synthétique)",
  Xlab = "Année"
)

abline(v = min(post_period), lty = 2)
abline(h = 0, lty = 3)

dev.off()


cat("\n=== SERIE SYNTHETIQUE ===\n")


Y1 <- c(dataprep_out$Y1plot)
Y0 <- c(dataprep_out$Y0plot %*% synth_out$solution.w)

df_plot <- data.frame(
  annee = dataprep_out$tag$time.plot,
  treated = Y1,
  synthetic = Y0,
  gap = Y1 - Y0
)


ggplot(df_plot, aes(x = annee)) +
  geom_line(aes(y = treated, linetype = nom_graph)) +
  geom_line(aes(y = synthetic, linetype = paste0(nom_graph, " Synthétique"))) +
  geom_vline(xintercept = min(post_period), linetype = 2) +
  labs(y = "nb_RP_en_loc", linetype = "") +
  theme_minimal()



df_plot[df_plot$annee %in% post_period,]

att_4_ans = df_plot$gap[df_plot$annee == 2022]

cat("\nATT post-traitement à 4 ans à", nom_graph, ":", att_4_ans, "\n")
cat("\nSoit", 
    round(100*(-att_4_ans)/data_19e[COM == com & annee == 2022, nb_RP_en_loc], 1), 
    "% du parc de logements en location en 2022\n")

sauv_pourcenge = c(sauv_pourcenge, round(100*(-att_4_ans)/data_19e[COM == com & annee == 2022, nb_RP_en_loc], 1))

}
