#### QUE FAIT CE SCRIPT #####
# On réalise une ACP puis un clustering
# que l'on utilise pour ensuite réaliser le controle synthétique

# en effet, il y a trop d'observations sinon


library(data.table)
library(FactoMineR)
library(factoextra)

data = fread("../base_2012_2022.csv", encoding = "UTF-8")

# Préparation des données à l'échelle communale -----
# on garde les communes de plus de 5 000 habitants

df_com = data[, lapply(.SD, sum), by = c("COM", "annee"), .SDcols = is.numeric]

df_com = df_com[nb_personnes_menage> 5000]

df_com[, part_RP_1_2_pieces := 100*(nb_RP_1_piece + nb_RP_2_pieces)/nb_RP]
df_com[, part_RP_en_loc := 100*nb_RP_en_loc/nb_RP]

df_com[, part_RP := 100*nb_RP/nb_logements]
df_com[, part_logements_vacants := 100*nb_logements_vacants/nb_logements]

df_com[, part_RP_proprio := 100*nb_RP_proprio/nb_RP]

df_com[, part_actifs_pop := 100*nb_actifs/nb_personnes_menage]
df_com[, part_etudiants_pop := 100*nb_etudiants/nb_personnes_menage]

df_com[, part_chomeurs := 100*nb_chomeurs/nb_actifs]

df_com[, part_agriculteurs := 100*nb_agriculteurs/nb_actifs_occ]
df_com[, part_commercants := 100*nb_commercants/nb_actifs_occ]
df_com[, part_cadres := 100*nb_cadres/nb_actifs_occ]
df_com[, part_prof_inter := 100*nb_professions_inter/nb_actifs_occ]
df_com[, part_employes := 100*nb_employes/nb_actifs_occ]

df_com = df_com[, c("COM", "annee",
                    "nb_personnes_menage",
                    "part_actifs_pop", "part_etudiants_pop",
                    "part_chomeurs", "part_agriculteurs",
                    "part_commercants", "part_cadres", 
                    "part_prof_inter", "part_employes",
                    "nb_logements", "part_RP_1_2_pieces",
                    "part_RP_en_loc", "part_RP",
                    "part_logements_vacants", "part_RP_proprio"
)]

# Réalisation de l'ACP -----
df_com_num <- df_com[annee==2017, .SD, .SDcols = is.numeric]

df_com_num = df_com_num[,-"annee"]

res <- PCA(df_com_num,
           scale.unit = TRUE,
           graph = FALSE)

# on récupère les nouvelles coordonnées
ind_coords <- res$ind$coord

# on ne prend que les 5 premières coordonnées
ind_coords_sub <- ind_coords[, 1:5]  # sélection des axes 1 à 5

fviz_nbclust(ind_coords_sub, kmeans, method = "silhouette")
# on prend 5 clusters

# CLUSTERING ------
km <- kmeans(ind_coords_sub, centers = 5, nstart = 25)


df_5_cluster_2017_com = df_com[annee == 2017]
df_5_cluster_2017_com$cluster_com <- km$cluster

df_com_cluster = df_5_cluster_2017_com[, c("COM", "cluster_com")]

rm(df_5_cluster_2017_com)
rm(df_com_num)
rm(ind_coords)
rm(ind_coords_sub)
rm(km)
rm(res)
rm(df_com)

# CONTROL SYNTHETIQUE -----

correspondance_COM = fread("../correspondance_com.csv",
                           sep = ";", encoding = "UTF-8")

# Agrégation à l'échelle de la commune
num_cols <- names(data)[sapply(data, is.numeric)]
num_cols <- setdiff(num_cols, c("COM", "annee", "IRIS"))

data <- data[, 
             lapply(.SD, sum, na.rm = TRUE),
             by = .(COM, annee),
             .SDcols = num_cols
]

# Variables supplémentaires

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


# On exclut les communes d'Île de France
data_hors_IDF = data[
  !(substr(COM, 1, 2) %in% c("77", "78", "91", 
                             "92", "93", "94", "95"))
]

data_synth <- data[COM %in% data_hors_IDF$COM]

# on fait la jointure avec les cluster

data_synth = merge(data_synth,
                   df_com_cluster,
                   by = "COM")

# REALISATION DU CONTROL SYNTHETIC -----
sauv_pourcenge = data.table(arrondissement = NA,
                            effet = NA,
                            RMSPE_pre = NA,
                            RMSPE_post = NA,
                            ratio = NA)


for (com in 75101:75120) {
  print(com)
  
  nom_graph = paste0("Paris, ", substr(com, 4, 5), "eme Arrond.")
  
  
  # On ne garde que l'arrondissement
  data_arr = data_synth[
    !(substr(COM, 1, 5) %in% as.character(setdiff(75101:75120, com)))
  ]
  
  # on ne garde que les communes du même cluster
  
  cluster = data_arr[COM == com & annee == 2012, cluster_com]
  
  data_arr = data_arr[cluster_com == cluster]
  
  # On vérifie qu'il y a bien 1 obs par COM et par an
  obs_par_commune <- data_arr[, .(
    n_obs      = .N,
    annees_ok  = list(sort(unique(annee))),
    min_annee  = min(annee),
    max_annee  = max(annee)
  ), by = COM]
  
  
  # on supprime les communes pour lesquelles il manque
  # au moins 1 observation
  obs_par_commune[n_obs<11]
  
  
  data_arr = data_arr[
    !(COM %in% obs_par_commune[n_obs<11, COM])
  ]
  
  # Création d'un ID par commune
  data_arr[, id_num := as.integer(factor(COM))]
  
  id_paris <- unique(data_arr[COM == com]$id_num)
  
  id_ctrl <- setdiff(unique(data_arr$id_num), id_paris)
  
  annees_all <- sort(unique(data_arr$annee))
  
  
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
    foo                = as.data.frame(data_arr),
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
  
  fwrite(res[res$w.weights>0.01], 
         paste0("Synthetic_control-CLUSTER/", nom_graph, "-poids.csv"),
         sep = ";", dec = ",", encoding = "UTF-8")
  
  # cat("\n=== ÉQUILIBRE DES PRÉDICTEURS (",nom_graph, "vs Synthétique) ===\n")
  # print(synth_tables$tab.pred)
  
  
  cat("\n=== PLOT 1 ===\n")
  jpeg(
    filename = paste0("Synthetic_control-CLUSTER/", nom_graph, "path_plot-CLUSTER.jpeg"),
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
    filename = paste0("Synthetic_control-CLUSTER/", nom_graph, "gaps_plot-CLUSTER.jpeg"),
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
      round(100*(-att_4_ans)/data_arr[COM == com & annee == 2022, nb_RP_en_loc], 1), 
      "% du parc de logements en location en 2022\n")
  
  
  effet_num = round(100*(att_4_ans)/data_arr[COM == com & annee == 2022, nb_RP_en_loc], 1)
  
  # Index périodes
  pre_index  <- which(df_plot$annee %in% pre_period)
  post_index <- which(df_plot$annee %in% post_period)
  
  # RMSPE
  rmspe_pre_treated  <- sqrt(mean(df_plot$gap[pre_index]^2))
  rmspe_post_treated <- sqrt(mean(df_plot$gap[post_index]^2))
  
  # Ratio
  ratio_treated <- rmspe_post_treated / rmspe_pre_treated
  
  cat("RMSPE pré (traité) :", rmspe_pre_treated, "\n")
  cat("RMSPE post (traité):", rmspe_post_treated, "\n")
  cat("Ratio RMSPE traité :", ratio_treated, "\n")
  
  
  sauv_pourcenge = rbind(sauv_pourcenge, data.table(
    arrondissement = nom_graph,
    effet = effet_num,
    RMSPE_pre = rmspe_pre_treated,
    RMSPE_post = rmspe_post_treated,
    ratio = ratio_treated
  )
                         )

}

sauv_pourcenge = sauv_pourcenge[!is.na(ratio)]
fwrite(sauv_pourcenge, "Synthetic_control-CLUSTER/RESULTATS.csv", sep = ";",
       dec = ",", encoding = "UTF-8")
