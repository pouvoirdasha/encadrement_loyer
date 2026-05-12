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
                                variable_dependante = "nb_RP_en_loc"                                ) {
  
  for (i in 1:length(liste_com)) {
    com = liste_com[i]
    print(com)
    
    if (substr(com, 1, 2) == "75") {
      nom_graph = paste0("Paris, ", substr(com, 4, 5), "eme Arrond.")
    }
    else nom_graph = paste0(correspondance_COM[`Code géographique` == com])

    
    # on supprime les villes concernees par l'encadrement
    data_hors_encad = data_synth[
      !(COM %in% as.character(setdiff(liste_com_encadrement, com)))
    ]
    
    # on filtre sur l'année
    data_hors_encad = data_hors_encad[annee >= annee_debut]
    
    # on ne garde que les communes du même cluster
    
    cluster = data_hors_encad[COM == com & annee == 2015, cluster_com]
    
    data_hors_encad = data_hors_encad[cluster_com == cluster]
    
    # On vérifie qu'il y a bien 1 obs par COM et par an
    obs_par_commune <- data_hors_encad[, .(
      n_obs      = .N,
      annees_ok  = list(sort(unique(annee))),
      min_annee  = min(annee),
      max_annee  = max(annee)
    ), by = COM]
    
    # on tronque le jeu de données pour partir de l'année
    # la plus récente dans la 1ere data
    min_annee = max(obs_par_commune[, min_annee])
    if (min_annee > 2012) {
      print("IL Y A UN PROBLEME AVEC LE JEU DE DONNEE DANS LES ANNEES DISPO")
    }
    
    data_hors_encad = data_hors_encad[annee>=min_annee]
    
    # on supprime les communes pour lesquelles il manque
    # au moins 1 observation (pour années max)
    obs_par_commune <- data_hors_encad[, .(
      n_obs      = .N,
      annees_ok  = list(sort(unique(annee))),
      min_annee  = min(annee),
      max_annee  = max(annee)
    ), by = COM]
    
    nb_obs_attendu = max(obs_par_commune[, n_obs])

    data_hors_encad = data_hors_encad[
      !(COM %in% obs_par_commune[n_obs<nb_obs_attendu, COM])
    ]
    
    # Création d'un ID par commune
    data_hors_encad[, id_num := as.integer(factor(COM))]
    
    id_ville <- unique(data_hors_encad[COM == com]$id_num)
    
    id_ctrl <- setdiff(unique(data_hors_encad$id_num), id_ville)
    
    annees_all <- sort(unique(data_hors_encad$annee))
    
    
    # Variables d'intérêt et formatage des données
    # pour Synth
    
    covariates_logement <- covariates_logement
    
    
    # Periodes pre-traitement pour les predicteurs
    pre_period  <- annees_all[annees_all < annee_encadrement]
    post_period <- annees_all[annees_all >= annee_encadrement]
    
    
    
    
    # Préparation du jeu de données
    
    dataprep_out <- dataprep(
      foo                = as.data.frame(data_hors_encad),
      predictors         = covariates_logement,
      predictors.op      = "mean",             # moyenne sur la période pré
      special.predictors = list(
        # On contrôle aussi le lag de la variable outcome à différentes dates
        list("nb_RP_en_loc", 2008, "mean"),
        list("nb_RP_en_loc", 2014, "mean")
        #list("nb_RP_en_loc", 2012, "mean")
        #list("nb_RP_en_loc", 2016, "mean")
        #list("nb_RP_en_loc", 2016, "mean")
        #list("nb_RP_en_loc", 2018, "mean")
      ),
      dependent          = variable_dependante,
      unit.variable      = "id_num",
      unit.names.variable = "COM",
      time.variable      = "annee",
      treatment.identifier = id_ville,
      controls.identifier  = id_ctrl,
      time.predictors.prior = pre_period,
      time.optimize.ssr     = pre_period,
      time.plot             = annees_all
    )
  
    
    synth_out <- synth(
      data.prep.obj = dataprep_out,
      method        = "All",
      verbose       = FALSE
    )
    
    # Tableau des poids
    synth_tables <- synth.tab(
      dataprep.res = dataprep_out,
      synth.res    = synth_out
    )
    
    res_temp = as.data.table(merge(synth_tables$tab.w, 
                                   correspondance_COM[, c("Code géographique", "Libellé géographique")],
                                   by.x = "unit.names",
                                   by.y = "Code géographique"))
    
    res_temp = res_temp[res_temp$w.weights>0.01]
    res_temp[, ville_synthetise := com]
    
    res = rbind(res, res_temp)
    rm(res_temp)
    
    
    # calcul de l'indicateur de fiabilité des résultats
    Y1 <- c(dataprep_out$Y1plot)
    Y0 <- c(dataprep_out$Y0plot %*% synth_out$solution.w)
    
    df_plot <- data.frame(
      annee = dataprep_out$tag$time.plot,
      treated = Y1,
      synthetic = Y0,
      gap = Y1 - Y0
    )
    
    df_plot[df_plot$annee %in% post_period,]
    
    att_4_ans = df_plot$gap[df_plot$annee == 2022]
    
    cat("\nATT post-traitement à 4 ans à", nom_graph, ":", att_4_ans, "\n")
    cat("\nSoit", 
        round(100*(-att_4_ans)/data_hors_encad[COM == com & annee == 2022, nb_RP_en_loc], 1), 
        "% du parc de logements en location en 2022\n")
    
    
    effet_num = round(100*(att_4_ans)/data_hors_encad[COM == com & annee == 2022, nb_RP_en_loc], 1)
    
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
    
    
    # export des graphiques
    cat("\n=== PLOT 1 ===\n")
    jpeg(
      filename = paste0(nom_dossier_export, "/", nom_graph, "path_plot-CLUSTER.jpeg"),
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
    cat("\n===== OK =====\n")
    
    
    
    cat("\n=== PLOT 2 ===\n")

    
    jpeg(
      filename = paste0(nom_dossier_export, "/", nom_graph, "gaps_plot-CLUSTER.jpeg"),
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
    
    cat("\n===== OK =====\n")
    
    

  }
    
  fwrite(res[!is.na(ville_synthetise)], paste0(nom_dossier_export, "/resultats-communes_synthetiques.csv"),
         sep = ";", dec = ",")
  fwrite(sauv_pourcenge[!is.na(ratio)], paste0(nom_dossier_export, "/resultats-RMSPE.csv"),
                                               sep = ";", dec = ",")
}

