#### QUE FAIT CE SCRIPT #####
# données finales importées directement


library(data.table)
library(FactoMineR)
library(factoextra)
library(Synth)



data = fread("base_2006_2022_aire_cluster-COM.csv", encoding = "UTF-8")

data_synth = data

data_synth[, densite := nb_personnes_menage/aire]
data_synth[, cluster_com := cluster]


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
                                  "nb_RP_1_piece",
                                  "densite"
                                ),
                                annee_encadrement = 2019,
                                variable_dependante = "nb_RP_en_loc",
                                n_placebo = 100,          
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
