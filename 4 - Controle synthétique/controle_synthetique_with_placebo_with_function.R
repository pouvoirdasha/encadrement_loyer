#### QUE FAIT CE SCRIPT #####
# On réalise une ACP puis un clustering
# que l'on utilise pour ensuite réaliser le controle synthétique

# en effet, il y a trop d'observations sinon


library(data.table)
library(FactoMineR)
library(factoextra)
library(Synth)



data = fread("base_2006_2022.csv", encoding = "UTF-8")

# VARIABLES A CHANGER
seuil_pop = 3000
annee_ACP = 2015
nb_clusters = 6

# Préparation des données à l'échelle communale -----
# on garde les communes de plus de 3 000 habitants

df_com = data[, lapply(.SD, sum, na.rm = TRUE),
              by = c("COM", "annee"), .SDcols = is.numeric]


df_com = df_com[nb_personnes_menage> seuil_pop]

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

# Réalisation de l'ACP SUR 2015 UNIQUEMENT -----
df_com_num <- df_com[annee==annee_ACP, .SD, .SDcols = is.numeric]

df_com_num = df_com_num[,-"annee"]

res <- PCA(df_com_num,
           scale.unit = TRUE,
           graph = FALSE)

res$eig

# on récupère les nouvelles coordonnées
ind_coords <- res$ind$coord

# on ne prend que les 5 premières coordonnées
ind_coords_sub <- ind_coords[, 1:5]  # sélection des axes 1 à 5

fviz_nbclust(ind_coords_sub, kmeans, method = "silhouette")
# on prend 6 clusters


# CLUSTERING ------
km <- kmeans(ind_coords_sub, centers = nb_clusters, nstart = 100)


data_cluster_com = df_com[annee == annee_ACP]
data_cluster_com$cluster_com <- km$cluster

df_com_cluster = data_cluster_com[, c("COM", "cluster_com")]

rm(data_cluster_com)
rm(ind_coords)
rm(ind_coords_sub)
rm(km)
rm(res)
rm(df_com)

# CONTROL SYNTHETIQUE -----
# A - Préparation des données à l'échelle communale -----

correspondance_COM = fread("correspondance_com.csv",
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
# pour éviter les effets de bord
# de l'encadrement des loyers à Paris
data_hors_IDF = data[
  !(substr(COM, 1, 2) %in% c("77", "78", "91", 
                             "92", "93", "94", "95"))
]

data_synth <- data[COM %in% data_hors_IDF$COM]

# on fait la jointure avec les cluster

data_synth = merge(data_synth,
                   df_com_cluster,
                   by = "COM")

rm(data)
rm(data_hors_IDF)
rm(df_com_num)
rm(df_com_cluster)

# B- Réalisation du contrôle synthétique -----
#source("fonctions_control_synthetique.R")

# table de sauvgarde des résultats
res <- data.table()
sauv_pourcenge <- data.table(
  arrondissement   = character(),
  effet            = numeric(),
  RMSPE_pre        = numeric(),
  RMSPE_post       = numeric(),
  ratio            = numeric(),
  n_placebo_valide = integer(),
  p_val_perm       = numeric(),
  rang_placebo     = integer()
)



control_synth_liste <- function(liste_com,
                                data_synth,
                                nom_dossier_export,
                                annee_debut = 2006,
                                liste_com_encadrement = c(as.character(75101:75120),
                                                          "59350", "59298", "59355"),
                                covariates_logement = c(
                                  "nb_logements",
                                  "taux_vacance",
                                  "nb_residences_second_ou_occ",
                                  "nb_RP_1_piece"
                                ),
                                annee_encadrement = 2019,
                                variable_dependante = "nb_RP_en_loc",
                                n_placebo = 50,          
                                seed_placebo = 42        
) {
  
  # --------------------------------------------------------------------------
  # Fonction interne : estime un synthetic control et renvoie le ratio RMSPE
  # --------------------------------------------------------------------------
  run_synth_unit <- function(data_dt, id_traite, id_ctrl_pool,
                             annees_all, pre_period, post_period,
                             covariates, dep_var) {
    tryCatch({
      dp <- dataprep(
        foo                  = as.data.frame(data_dt),
        predictors           = covariates,
        predictors.op        = "mean",
        special.predictors   = list(
          list(dep_var, 2008, "mean"),
          list(dep_var, 2014, "mean")
        ),
        dependent            = dep_var,
        unit.variable        = "id_num",
        unit.names.variable  = "COM",
        time.variable        = "annee",
        treatment.identifier = id_traite,
        controls.identifier  = id_ctrl_pool,
        time.predictors.prior = pre_period,
        time.optimize.ssr    = pre_period,
        time.plot            = annees_all
      )
      
      so <- synth(data.prep.obj = dp, method = "All", verbose = FALSE)
      
      Y1 <- c(dp$Y1plot)
      Y0 <- c(dp$Y0plot %*% so$solution.w)
      gap <- Y1 - Y0
      
      pre_idx  <- which(annees_all %in% pre_period)
      post_idx <- which(annees_all %in% post_period)
      
      rmspe_pre  <- sqrt(mean(gap[pre_idx]^2))
      rmspe_post <- sqrt(mean(gap[post_idx]^2))
      ratio      <- if (rmspe_pre > 0) rmspe_post / rmspe_pre else NA_real_
      
      list(dp = dp, so = so, gap = gap, ratio = ratio,
           rmspe_pre = rmspe_pre, rmspe_post = rmspe_post)
    }, error = function(e) {
      message("  [ERREUR synth] : ", conditionMessage(e))
      NULL
    })
  }
  
  # --------------------------------------------------------------------------
  # Boucle principale
  # --------------------------------------------------------------------------
  for (i in seq_along(liste_com)) {
    
    com <- liste_com[i]
    print(com)
    
    if (substr(com, 1, 2) == "75") {
      nom_graph <- paste0("Paris, ", substr(com, 4, 5), "eme Arrond.")
    } else {
      nom_graph <- correspondance_COM[`Code géographique` == com, `Libellé géographique`]
    }
    
    # --- Préparation du jeu de données (identique à avant) ------------------
    data_hors_encad <- data_synth[
      !(COM %in% as.character(setdiff(liste_com_encadrement, com)))
    ][annee >= annee_debut]
    
    cluster <- data_hors_encad[COM == com & annee == 2015, cluster_com]
    data_hors_encad <- data_hors_encad[cluster_com == cluster]
    
    obs_par_commune <- data_hors_encad[, .(
      n_obs     = .N,
      min_annee = min(annee),
      max_annee = max(annee)
    ), by = COM]
    
    min_annee <- max(obs_par_commune$min_annee)
    if (min_annee > 2012) message("PROBLEME ANNEES DISPO pour ", com)
    
    data_hors_encad <- data_hors_encad[annee >= min_annee]
    
    obs_par_commune <- data_hors_encad[, .(n_obs = .N), by = COM]
    nb_obs_attendu  <- max(obs_par_commune$n_obs)
    data_hors_encad <- data_hors_encad[
      !(COM %in% obs_par_commune[n_obs < nb_obs_attendu, COM])
    ]
    
    data_hors_encad[, id_num := as.integer(factor(COM))]
    
    id_ville <- unique(data_hors_encad[COM == com]$id_num)
    id_ctrl  <- setdiff(unique(data_hors_encad$id_num), id_ville)
    
    annees_all  <- sort(unique(data_hors_encad$annee))
    pre_period  <- annees_all[annees_all <  annee_encadrement]
    post_period <- annees_all[annees_all >= annee_encadrement]
    
    # --- Estimation sur la ville traitée ------------------------------------
    cat("\n>>> Ville traitée :", nom_graph, "\n")
    
    res_traite <- run_synth_unit(
      data_dt      = data_hors_encad,
      id_traite    = id_ville,
      id_ctrl_pool = id_ctrl,
      annees_all   = annees_all,
      pre_period   = pre_period,
      post_period  = post_period,
      covariates   = covariates_logement,
      dep_var      = variable_dependante
    )
    
    if (is.null(res_traite)) next
    
    dataprep_out <- res_traite$dp
    synth_out    <- res_traite$so
    
    # Tableau des poids
    synth_tables <- synth.tab(dataprep.res = dataprep_out, synth.res = synth_out)
    res_temp <- as.data.table(merge(
      synth_tables$tab.w,
      correspondance_COM[, c("Code géographique", "Libellé géographique")],
      by.x = "unit.names", by.y = "Code géographique"
    ))
    res_temp <- res_temp[w.weights > 0.01]
    res_temp[, ville_synthetise := com]
    res <- rbind(res, res_temp)
    rm(res_temp)
    
    # ATT et RMSPE ville traitée
    att_4_ans  <- res_traite$gap[annees_all == 2022]
    effet_num  <- round(100 * att_4_ans /
                          data_hors_encad[COM == com & annee == 2022,
                                          get(variable_dependante)], 1)
    
    cat("ATT à 4 ans :", att_4_ans, "(", effet_num, "% du parc)\n")
    cat("Ratio RMSPE traité :", res_traite$ratio, "\n")
    
    # -----------------------------------------------------------------------
    # TEST DE PERMUTATION : 20 villes placebo tirées aléatoirement
    # -----------------------------------------------------------------------
    set.seed(seed_placebo)
    
    # On ne garde que les contrôles avec un RMSPE pré raisonnable
    # (optionnel : filtrer sur rmspe_pre < 5 * rmspe_pre_traite)
    pool_placebo <- setdiff(
      unique(data_hors_encad[COM != com, COM]),
      as.character(liste_com_encadrement)
    )
    
    n_tirage    <- min(n_placebo, length(pool_placebo))
    villes_plac <- sample(pool_placebo, n_tirage, replace = FALSE)
    
    cat("Test de permutation sur", n_tirage, "villes placebo...\n")
    
    ratios_placebo <- numeric(0)
    
    for (plac_com in villes_plac) {
      
      id_plac      <- unique(data_hors_encad[COM == plac_com]$id_num)
      # Le pool de contrôles exclut la ville placebo ET la ville traitée réelle
      id_ctrl_plac <- setdiff(id_ctrl, id_plac)
      
      res_plac <- run_synth_unit(
        data_dt      = data_hors_encad,
        id_traite    = id_plac,
        id_ctrl_pool = id_ctrl_plac,
        annees_all   = annees_all,
        pre_period   = pre_period,
        post_period  = post_period,
        covariates   = covariates_logement,
        dep_var      = variable_dependante
      )
      
      if (!is.null(res_plac) && !is.na(res_plac$ratio)) {
        ratios_placebo <- c(ratios_placebo, res_plac$ratio)
        cat("  ", plac_com, "-> ratio =", round(res_plac$ratio, 2), "\n")
      }
    }
    
    # --- P-valeur de permutation -------------------------------------------
    # Rang de la ville traitée dans la distribution (ratio les plus élevés = effet)
    ratio_traite <- res_traite$ratio
    n_valides    <- length(ratios_placebo)
    
    p_val_perm <- if (n_valides > 0) {
      # proportion de placebos avec un ratio >= ratio traité
      mean(ratios_placebo >= ratio_traite)
    } else {
      NA_real_
    }
    
    rang_traite <- sum(ratios_placebo >= ratio_traite)   # nb placebos >= traité
    
    cat("\nDistribution des ratios placebo :\n")
    print(summary(ratios_placebo))
    cat("Ratio ville traitée :", round(ratio_traite, 2), "\n")
    cat("P-valeur de permutation :", round(p_val_perm, 3),
        "(", rang_traite, "placebos >=", round(ratio_traite, 2), "sur",
        n_valides, ")\n")
    
    # --- Sauvegarde des résultats -------------------------------------------
    sauv_pourcenge <- rbind(sauv_pourcenge, data.table(
      arrondissement   = nom_graph,
      effet            = effet_num,
      RMSPE_pre        = res_traite$rmspe_pre,
      RMSPE_post       = res_traite$rmspe_post,
      ratio            = ratio_traite,
      n_placebo_valide = n_valides,
      p_val_perm       = p_val_perm,
      rang_placebo     = rang_traite
    ))
    
    # --- Graphique du test de permutation -----------------------------------
    jpeg(
      filename = paste0(nom_dossier_export, "/", nom_graph, "_permutation_test.jpeg"),
      width = 1200, height = 800, res = 150
    )
    
    hist(
      ratios_placebo,
      breaks  = 10,
      col     = "lightgrey",
      border  = "white",
      main    = paste0("Test de permutation – ", nom_graph),
      xlab    = "Ratio RMSPE (post/pré)",
      ylab    = "Fréquence (villes placebo)"
    )
    abline(v = ratio_traite, col = "red", lwd = 2, lty = 1)
    legend(
      "topright",
      legend = c(
        paste0("Ville traitée (ratio = ", round(ratio_traite, 2), ")"),
        paste0("p-val. perm. = ", round(p_val_perm, 3))
      ),
      col = c("red", NA), lty = c(1, NA), lwd = c(2, NA), bty = "n"
    )
    
    dev.off()
    
    # --- Graphiques habituels (path plot + gaps plot) -----------------------
    jpeg(filename = paste0(nom_dossier_export, "/", nom_graph, "path_plot-CLUSTER.jpeg"),
         width = 1200, height = 800, res = 150)
    path.plot(
      synth.res = synth_out, dataprep.res = dataprep_out,
      Ylab = variable_dependante, Xlab = "Année",
      Legend = c(nom_graph, paste0(nom_graph, " Synthétique")),
      Legend.position = "topleft"
    )
    abline(v = min(post_period), lty = 2)
    dev.off()
    
    jpeg(filename = paste0(nom_dossier_export, "/", nom_graph, "gaps_plot-CLUSTER.jpeg"),
         width = 1200, height = 800, res = 150)
    gaps.plot(
      synth.res = synth_out, dataprep.res = dataprep_out,
      Ylab = "Gap (Traitement - Synthétique)", Xlab = "Année"
    )
    abline(v = min(post_period), lty = 2)
    abline(h = 0, lty = 3)
    dev.off()
    
  } # fin boucle principale
  
  # --- Exports CSV finaux ---------------------------------------------------
  fwrite(res[!is.na(ville_synthetise)],
         paste0(nom_dossier_export, "/resultats-communes_synthetiques.csv"),
         sep = ";", dec = ",")
  
  fwrite(sauv_pourcenge[!is.na(ratio)],
         paste0(nom_dossier_export, "/resultats-RMSPE.csv"),
         sep = ";", dec = ",")
}


liste_com = c("75101", 
              "75102", "75103",
              "75104", "75105", "75106",
              "75107", "75108",
              "75109",
              "75110", "75111",
              "75112", "75113", "75114",
              "75115", "75116", "75117",
              "75118", "75119", "75120",
              "59350")


control_synth_liste(
  liste_com          = liste_com,
  data_synth         = data_synth,
  nom_dossier_export = "Synthetic_control_all_with_placebo"
)
