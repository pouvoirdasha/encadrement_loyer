library(data.table)
library(augsynth)

setwd("~/0 ENSAE/3A/S2/Projet_socio_eco/encadrement_loyer/7 - Controle synthétique augmenté/Traitement 2015")
data = fread("../../base_2006_2022_aire_cluster-COM.csv",
             sep = ";", dec = ",")


annees = 2007:2022
traites <- c(as.character(75101:75120))

data = data[annee!= "2006"]

correspondance_COM = fread("../../correspondance_com.csv",
                           sep = ";", encoding = "UTF-8")

setnames(correspondance_COM, "Code géographique", "COM")
correspondance_COM = correspondance_COM[, c("COM", "Libellé géographique")]

dt <- copy(data)

# Covariables
dt[, log_menages   := log(nb_menages + 1)]
dt[, log_logements := log(nb_logements + 1)]
dt[, taux_vacance  := nb_logements_vacants / nb_logements]
dt[, densite_RP    := log(nb_RP / aire + 1)]
dt[, part_HLM      := nb_RP_HLM / nb_RP]
dt[, part_cadres   := nb_cadres / nb_menages]
dt[, densite := nb_personnes_menage/aire]
dt[, treated       := as.integer(COM %in% traites)]
dt[, post          := as.integer(annee >= 2015)]
dt[, treat_post    := treated * post]

res_synth = data.table()


for (commune in as.character(c(75101:75120))) {
  
  dt_synth = dt[! COM %in% setdiff(traites, commune)]
  cluster_com = unique(dt[COM == commune, cluster])
  
  dt_synth = dt_synth[cluster == cluster_com]
  
  
  # MODELE 1 - SCM CLASSIQUE -----
  
  syn_classique <- augsynth(
    nb_RP_en_loc ~ treat_post,
    COM,
    annee,
    dt_synth,
    progfunc="None",
    scm=TRUE
  )
  
  png(paste0(commune, "-SCM_conformal.png"),
    width=1600,
    height=900,
    res=200)
  print(plot(syn_classique))
  dev.off()
  
  png(paste0(commune, "-SCM_jackknife.png"),
      width=1600,
      height=900,
      res=200)
  print(plot(syn_classique, inf_type = "jackknife+"))
  dev.off()
  
  poids <- as.data.table(
    syn_classique$weights,
    keep.rownames = "COM"
  )
  setnames(poids,"V1","weight")
  poids = poids[weight>0.01]
  poids = merge(poids, correspondance_COM)
  fwrite(poids, paste0(commune, "-SCM-poids.csv"))
  
  res = summary(syn_classique)
  res_l2_imbalance = res$l2_imbalance
  res_l2_imbalance_scale = res$scaled_l2_imbalance
  
  syn_classique_res = as.data.table(res$att)
  
  syn_classique_res = syn_classique_res[Time > 2014]
  
  nb_RP_loc = dt_synth[COM == commune & annee> 2014, c("annee", "nb_RP_en_loc")]
  
  syn_classique_res_complet = merge(syn_classique_res,
                                    nb_RP_loc,
                                    by.x = "Time",
                                    by.y = "annee")
  
  syn_classique_res_complet[, pourcentage_ATT := 100*Estimate/nb_RP_en_loc]
  syn_classique_res_complet[, modele := "SCM simple - conformal"]
  syn_classique_res_complet[, res_l2_imbalance := res_l2_imbalance]
  syn_classique_res_complet[, res_l2_imbalance_scale := res$scaled_l2_imbalance]
  syn_classique_res_complet[, COM := commune]
  
  res_synth = rbind(res_synth, syn_classique_res_complet, fill = TRUE)
  
  # Avec jackknif+
  
  res = summary(syn_classique, inf_type = "jackknife+")
  res_l2_imbalance = res$l2_imbalance
  res_l2_imbalance_scale = res$scaled_l2_imbalance
  
  syn_jack_res = as.data.table(res$att)
  
  syn_jack_res = syn_jack_res[Time > 2014]
  
  syn_jack_res_complet = merge(syn_jack_res,
                                    nb_RP_loc,
                                    by.x = "Time",
                                    by.y = "annee")
  
  syn_jack_res_complet[, pourcentage_ATT := 100*Estimate/nb_RP_en_loc]
  syn_jack_res_complet[, modele := "SCM simple -  Jackknife+"]
  syn_jack_res_complet[, res_l2_imbalance := res_l2_imbalance]
  syn_jack_res_complet[, res_l2_imbalance_scale := res$scaled_l2_imbalance]
  syn_jack_res_complet[, COM := commune]
  
  res_synth = rbind(res_synth,
                    syn_jack_res_complet, fill = TRUE)
  
  
  
  # MODELE 2 - SCMA Ridge -----
  
  syn_ridge <- augsynth(
    nb_RP_en_loc ~ treat_post,
    COM,
    annee,
    dt_synth,
    progfunc="Ridge",
    scm=TRUE
  )
  
  png(paste0(commune, "-SCMA_Ridge_conformal.png"),
      width=1600,
      height=900,
      res=200)
  print(plot(syn_ridge))
  dev.off()
  
  png(paste0(commune, "-SCMA_Ridge_jackknife.png"),
      width=1600,
      height=900,
      res=200)
  print(plot(syn_ridge, inf_type = "jackknife+"))
  dev.off()
  
  poids <- as.data.table(
    syn_ridge$weights,
    keep.rownames = "COM"
  )
  setnames(poids,"V1","weight")
  poids = poids[weight>0.01]
  poids = merge(poids, correspondance_COM)
  fwrite(poids, paste0(commune, "-SCMA_Ridge-poids.csv"))
  
  res = summary(syn_ridge)
  res_l2_imbalance = res$l2_imbalance
  res_l2_imbalance_scale = res$scaled_l2_imbalance
  
  syn_ridge_res = as.data.table(res$att)
  
  syn_ridge_res = syn_ridge_res[Time > 2014]
  
  nb_RP_loc = dt_synth[COM == commune & annee> 2014, c("annee", "nb_RP_en_loc")]
  
  syn_ridge_res_complet = merge(syn_ridge_res,
                                    nb_RP_loc,
                                    by.x = "Time",
                                    by.y = "annee")
  
  syn_ridge_res_complet[, pourcentage_ATT := 100*Estimate/nb_RP_en_loc]
  syn_ridge_res_complet[, modele := "SCMA Ridge - conformal"]
  syn_ridge_res_complet[, res_l2_imbalance := res_l2_imbalance]
  syn_ridge_res_complet[, res_l2_imbalance_scale := res$scaled_l2_imbalance]
  syn_ridge_res_complet[, COM := commune]
  
  res_synth = rbind(res_synth, syn_ridge_res_complet, fill = TRUE)
  
  # Avec jackknif+
  
  res = summary(syn_ridge, inf_type = "jackknife+")
  syn_jack_res = as.data.table(res$att)
  res_l2_imbalance = res$l2_imbalance
  res_l2_imbalance_scale = res$scaled_l2_imbalance
  
  syn_jack_res = syn_jack_res[Time > 2014]
  
  syn_jack_res_complet = merge(syn_jack_res,
                               nb_RP_loc,
                               by.x = "Time",
                               by.y = "annee")
  
  syn_jack_res_complet[, pourcentage_ATT := 100*Estimate/nb_RP_en_loc]
  syn_jack_res_complet[, modele := "SCMA Ridge -  Jackknife+"]
  syn_jack_res_complet[, res_l2_imbalance := res_l2_imbalance]
  syn_jack_res_complet[, res_l2_imbalance_scale := res$scaled_l2_imbalance]
  syn_jack_res_complet[, COM := commune]
  
  
  res_synth = rbind(res_synth,
                    syn_jack_res_complet, fill = TRUE)
  
  
  # MODELE 3 - SCMA Ridge FE -----
  
  syn_ridge_FE <- augsynth(
    nb_RP_en_loc ~ treat_post,
    COM,
    annee,
    dt_synth,
    progfunc="Ridge",
    scm=TRUE,
    fixedeff = TRUE
  )
  
  png(paste0(commune, "-SCMA_Ridge_conformal_FE.png"),
      width=1600,
      height=900,
      res=200)
  print(plot(syn_ridge_FE))
  dev.off()
  
  png(paste0(commune, "-SCMA_Ridge_jackknife_FE.png"),
      width=1600,
      height=900,
      res=200)
  print(plot(syn_ridge_FE, inf_type = "jackknife+"))
  dev.off()
  
  
  poids <- as.data.table(
    syn_ridge_FE$weights,
    keep.rownames = "COM"
  )
  setnames(poids,"V1","weight")
  poids = poids[weight>0.01]
  poids = merge(poids, correspondance_COM)
  fwrite(poids, paste0(commune, "-SCMA_Ridge_FE-poids.csv"))
  
  
  res = summary(syn_ridge_FE)
  res_l2_imbalance = res$l2_imbalance
  res_l2_imbalance_scale = res$scaled_l2_imbalance
  syn_ridge_FE_res = as.data.table(res$att)
  
  syn_ridge_FE_res = syn_ridge_FE_res[Time > 2014]
  
  nb_RP_loc = dt_synth[COM == commune & annee> 2014, c("annee", "nb_RP_en_loc")]
  
  syn_ridge_FE_res_complet = merge(syn_ridge_FE_res,
                                nb_RP_loc,
                                by.x = "Time",
                                by.y = "annee")
  
  syn_ridge_FE_res_complet[, pourcentage_ATT := 100*Estimate/nb_RP_en_loc]
  syn_ridge_FE_res_complet[, modele := "SCMA Ridge FE - conformal"]
  syn_ridge_FE_res_complet[, res_l2_imbalance := res_l2_imbalance]
  syn_ridge_FE_res_complet[, res_l2_imbalance_scale := res$scaled_l2_imbalance]
  syn_ridge_FE_res_complet[, COM := commune]
  
  
  res_synth = rbind(res_synth, syn_ridge_FE_res_complet, fill = TRUE)
  
  # Avec jackknif+
  
  res = summary(syn_ridge_FE, inf_type = "jackknife+")
  res_l2_imbalance = res$l2_imbalance
  res_l2_imbalance_scale = res$scaled_l2_imbalance
  syn_jack_res = as.data.table(res$att)
  
  syn_jack_res = syn_jack_res[Time > 2014]
  
  syn_jack_res_complet = merge(syn_jack_res,
                               nb_RP_loc,
                               by.x = "Time",
                               by.y = "annee")
  
  syn_jack_res_complet[, pourcentage_ATT := 100*Estimate/nb_RP_en_loc]
  syn_jack_res_complet[, modele := "SCMA Ridge FE -  Jackknife+"]
  syn_jack_res_complet[, res_l2_imbalance := res_l2_imbalance]
  syn_jack_res_complet[, res_l2_imbalance_scale := res$scaled_l2_imbalance]
  syn_jack_res_complet[, COM := commune]
  
  res_synth = rbind(res_synth,
                    syn_jack_res_complet, fill = TRUE)
  
  
  
  # MODELE 4 - SCMA Ridge Covariates -----
  
  syn_ridge_cov <- augsynth(
    nb_RP_en_loc ~ treat_post | densite + taux_vacance + taux_chomage + part_cadres + part_HLM,
    COM,
    annee,
    dt_synth,
    progfunc="Ridge",
    scm=TRUE,
    fixedeff = TRUE
  )
  
  png(paste0(commune, "-SCMA_Ridge_conformal_cov.png"),
      width=1600,
      height=900,
      res=200)
  print(plot(syn_ridge_cov))
  dev.off()
  
  png(paste0(commune, "-SCMA_Ridge_jackknife_cov.png"),
      width=1600,
      height=900,
      res=200)
  print(plot(syn_ridge_cov, inf_type = "jackknife+"))
  dev.off()
  
  
  poids <- as.data.table(
    syn_ridge_cov$weights,
    keep.rownames = "COM"
  )
  setnames(poids,"V1","weight")
  poids = poids[weight>0.01]
  poids = merge(poids, correspondance_COM)
  fwrite(poids, paste0(commune, "-SCMA_Ridge_cov-poids.csv"))
  
  
  
  res = summary(syn_ridge_cov)
  res_l2_imbalance = res$l2_imbalance
  res_l2_imbalance_scale = res$scaled_l2_imbalance
  syn_ridge_cov_res = as.data.table(res$att)
  
  syn_ridge_cov_res = syn_ridge_cov_res[Time > 2014]
  
  nb_RP_loc = dt_synth[COM == commune & annee> 2014, c("annee", "nb_RP_en_loc")]
  
  syn_ridge_cov_res_complet = merge(syn_ridge_cov_res,
                                   nb_RP_loc,
                                   by.x = "Time",
                                   by.y = "annee")
  
  syn_ridge_cov_res_complet[, pourcentage_ATT := 100*Estimate/nb_RP_en_loc]
  syn_ridge_cov_res_complet[, modele := "SCMA Ridge Cov - conformal"]
  syn_ridge_cov_res_complet[, res_l2_imbalance := res_l2_imbalance]
  syn_ridge_cov_res_complet[, res_l2_imbalance_scale := res$scaled_l2_imbalance]
  syn_ridge_cov_res_complet[, COM := commune]
  
  res_synth = rbind(res_synth, syn_ridge_cov_res_complet, fill = TRUE)
  
  # Avec jackknif+
  
  res = summary(syn_ridge_cov, inf_type = "jackknife+")
  syn_jack_res = as.data.table(res$att)
  res_l2_imbalance_scale = res$scaled_l2_imbalance
  res_l2_imbalance = res$l2_imbalance
  

  syn_jack_res = syn_jack_res[Time > 2014]
  
  syn_jack_res_complet = merge(syn_jack_res,
                               nb_RP_loc,
                               by.x = "Time",
                               by.y = "annee")
  
  syn_jack_res_complet[, pourcentage_ATT := 100*Estimate/nb_RP_en_loc]
  syn_jack_res_complet[, modele := "SCMA Ridge Cov -  Jackknife+"]
  syn_jack_res_complet[, res_l2_imbalance := res_l2_imbalance]
  syn_jack_res_complet[, res_l2_imbalance_scale := res$scaled_l2_imbalance]
  syn_jack_res_complet[, COM := commune]
  
  res_synth = rbind(res_synth,
                    syn_jack_res_complet, fill = TRUE)
  
  

}

fwrite(res_synth, "resultats_ATT_2015.csv", sep = ";", dec = ",")
