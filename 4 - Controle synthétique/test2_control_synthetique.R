library(data.table)
library(Synth)

setwd("~/0 ENSAE/3A/S2/Projet_socio_eco/encadrement_loyer/4 - Controle synthétique")

data = fread("../base_2012_2022.csv", encoding = "UTF-8")


# Agrégation à l'échelle de Paris (car 1 seule unité traitée)
data[, COM := fifelse(substr(COM, 1, 3) == "751", 
                      "75056", COM)]


num_cols <- names(data)[sapply(data, is.numeric)]
num_cols <- setdiff(num_cols, c("COM", "annee", "IRIS"))

data_ok <- data[, 
                   lapply(.SD, sum, na.rm = TRUE),
                   by = .(COM, annee),
                   .SDcols = num_cols
]

data = data_ok

rm(data_ok)

dt = data

rm(data)
#################################
# 1- Préparation des données ----
#################################

# A - Variables supplémentaires

# Traitement post-2019
dt[, post_2019 := fifelse(annee >= 2019, 1L, 0L)]

dt[, is_paris:= fifelse(COM == "75056", 1, 0)]

# Variable de traitement DiD
dt[, treated := is_paris * post_2019]

# Ratio locataires / résidences principales (part de location)
dt[, part_loc := nb_RP_en_loc / nb_RP]

# Taux de vacance
dt[, taux_vacance := nb_logements_vacants / nb_logements]

# Densité résidentielle (personnes par logement RP)
dt[, densite_RP := nb_personnes_en_RP / nb_RP]

# Taux de chômage approché
dt[, taux_chomage := nb_chomeurs / (nb_actifs + 1)]

# Part cadres parmi actifs occupés
dt[, part_cadres := nb_cadres / (nb_actifs_occ + 1)]

# Log des variables pour réduire l'asymétrie
dt[, log_RP_loc    := log1p(nb_RP_en_loc)]
dt[, log_menages   := log1p(nb_menages)]
dt[, log_logements := log1p(nb_logements)]


# B - Division en panel ----

data = dt

# Panel A : toutes années, variables logement uniquement
panel_logement <- data[annee %in% 2012:2022]

# Panel B : années RP (recensement), variables emploi disponibles
panel_emploi <- data[annee %in% c(2012, 2017, 2022)]

cat("Dimensions panel logement :", nrow(panel_logement), "x", ncol(panel_logement), "\n")
cat("Dimensions panel emploi   :", nrow(panel_emploi),   "x", ncol(panel_emploi),   "\n")
cat("Communes traitées (Paris) :", uniqueN(data[is_paris == 1]$COM), "\n")
cat("Communes contrôle         :", uniqueN(data[is_paris == 0]$COM), "\n")


communes_exclure <- c(
  "59350",  # Lille (encadrement 2020)
  "69123",  # Lyon (encadrement 2022)
  "33063",  # Bordeaux (encadrement 2022)
  "13055",  # Marseille (candidat mais non retenu - vérifier)
  "67482"   # Strasbourg (encadrement 2022)
)

data_clean <- data[!COM %in% communes_exclure]

# Pour Synth : on peut restreindre aux grandes villes (pop > seuil)
# pour améliorer la comparabilité
seuil_menages <- 15000  # à ajuster selon vos données
grandes_communes <- data_clean[
  annee == 2012 & nb_menages > seuil_menages,
  .(COM)
]

data_synth <- data_clean[COM %in% grandes_communes$COM]
cat("Communes dans pool Synth :", uniqueN(data_synth$COM), "\n")

############################################
# 2 - METHODE 1 : LOGEMENT TOUS LES ANS ----
############################################

data_methode1 = copy(data_synth)

data_methode1[, id_num := as.integer(factor(COM))]

id_paris <- unique(data_methode1[is_paris == 1]$id_num)

id_ctrl <- setdiff(unique(data_methode1$id_num), id_paris)

annees_all <- sort(unique(data_methode1$annee))


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


# Unbalenced : on corrige


# Compter les observations par commune
obs_par_commune <- data_methode1[, .(
  n_obs      = .N,
  annees_ok  = list(sort(unique(annee))),
  min_annee  = min(annee),
  max_annee  = max(annee)
), by = COM]

# c'est ok

 

dataprep_out <- dataprep(
  foo                = as.data.frame(data_methode1),
  predictors         = covariates_logement,
  predictors.op      = "mean",             # moyenne sur la période pré
  special.predictors = list(
    # On contrôle aussi le lag de la variable outcome à différentes dates
    list("part_loc", 2012, "mean"),
    list("part_loc", 2014, "mean"),
    list("part_loc", 2016, "mean"),
    list("part_loc", 2018, "mean")
  ),
  dependent          = "part_loc",
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

cat("\n=== ÉQUILIBRE DES PRÉDICTEURS (Paris vs Synthétique) ===\n")
print(synth_tables$tab.pred)


path.plot(
  synth.res = synth_out,
  dataprep.res = dataprep_out,
  Ylab = "nb_RP_en_loc",
  Xlab = "Année",
  Legend = c("Paris", "Paris synthétique"),
  Legend.position = "topleft"
)

abline(v = min(post_period), lty = 2)  # ligne verticale traitement

