# ============================================================
# ASCM — Encadrement des loyers
# Boucle par unité traitée : pool de contrôle = même cluster PRÉ-2019
# Outcome : nb_RP_en_loc (niveau, interprétable directement en logements)
# ============================================================

# ---- 0. Packages ----
pkgs <- c("data.table", "augsynth", "ggplot2", "devtools")
new  <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
if (length(new)) install.packages(new, repos = "https://cloud.r-project.org")
if (!"augsynth" %in% installed.packages()[, "Package"])
  devtools::install_github("ebenmichael/augsynth")

library(data.table)
library(augsynth)
library(ggplot2)

# ============================================================
# ---- 1. Chargement ----
# ============================================================

setwd("~/0 ENSAE/3A/S2/Projet_socio_eco/encadrement_loyer/7 - Controle synthétique augmenté")
data = fread("../base_2006_2022_aire_cluster-COM.csv",
             sep = ";", dec = ",")

annees = 2007:2022
traites <- c(as.character(75101:75120), "59350")

data = data[annee!= "2006"]
# ============================================================
# ---- 2. Préparation panel ----
# ============================================================
dt <- copy(data)

# Covariables
dt[, log_menages   := log(nb_menages + 1)]
dt[, log_logements := log(nb_logements + 1)]
dt[, taux_vacance  := nb_logements_vacants / nb_logements]
dt[, densite_RP    := log(nb_RP / aire + 1)]
dt[, part_HLM      := nb_RP_HLM / nb_RP]
dt[, part_cadres   := nb_cadres / nb_menages]
dt[, treated       := as.integer(COM %in% traites)]
dt[, post          := as.integer(annee >= 2019)]
dt[, treat_post    := treated * post]

# Panel équilibré uniquement
n_yr <- length(annees)
ok   <- dt[, .N, by = COM][N == n_yr, COM]
dt   <- dt[COM %in% ok]


cat("Panel :", uniqueN(dt$COM), "communes x", n_yr, "années\n")
cat("Unités traitées dans le panel :",
    sum(traites %in% unique(dt$COM)), "/", length(traites), "\n\n")

# ============================================================
# ---- 3. CLUSTER PRÉ-2019 : mode sur les années pré-traitement ----
# ============================================================
# Pour chaque commune, on prend le cluster le plus fréquent AVANT 2019
# → stable, ne dépend pas des variations post-traitement

mode_cluster <- function(x) {
  tab <- table(x)
  as.integer(names(tab)[which.max(tab)])
}

dt_cluster_pre <- dt[annee < 2019,
                     .(cluster_pre = mode_cluster(cluster)),
                     by = COM]

dt <- merge(dt, dt_cluster_pre, by = "COM", all.x = TRUE)

cat("Distribution des clusters pré-2019 (unités traitées) :\n")
print(dt[treated == 1 & annee == 2006, .N, by = .(cluster_pre)][order(cluster_pre)])
cat("\n")

# ============================================================
# ---- 4. BOUCLE ASCM PAR UNITÉ TRAITÉE ----
# ============================================================
# Pour chaque unité traitée :
#   a) identifier son cluster pré-2019
#   b) construire le pool = unité traitée + contrôles du même cluster_pre
#   c) estimer augsynth (1 traité vs N contrôles)
#   d) extraire ATT par année

traites_panel <- intersect(traites, unique(dt$COM))
cat("=== Boucle ASCM — estimation par unité ===\n")
cat("Nombre d'unités à estimer :", length(traites_panel), "\n\n")

run_augsynth_unit <- function(com_traite) {

  # Cluster pré-2019 de cette unité
  clust_i <- dt_cluster_pre[COM == com_traite, cluster_pre]

  # Pool : unité traitée + communes contrôles du même cluster_pre (non traitées)
  controles_pool <- dt_cluster_pre[
    !COM %in% traites & cluster_pre == clust_i, COM
  ]

  if (length(controles_pool) < 5) {
    cat(sprintf("  [SKIP] %s — pool trop petit (%d contrôles)\n",
                com_traite, length(controles_pool)))
    return(NULL)
  }

  dt_i <- dt[COM %in% c(com_traite, controles_pool)]

  # Variable de traitement spécifique à cette unité
  dt_i[, treated_i  := as.integer(COM == com_traite)]
  dt_i[, treat_post_i := treated_i * post]

  tryCatch({
    syn_i <- augsynth(
      form      = nb_RP_en_loc ~ treat_post_i,   # outcome en NIVEAU
      unit      = COM,
      time      = annee,
      data      = dt_i,
      progfunc  = "ridge",   # augmentation ridge (ASCM)
      scm       = TRUE,
      lambda    = NULL
    )

    # Résumé : ATT par année
    summ_i <- summary(syn_i)
    dt_out <- as.data.table(summ_i$att)
    dt_out[, COM := com_traite]
    dt_out[, n_controles := length(controles_pool)]
    dt_out[, cluster_pre := clust_i]
    dt_out

  }, error = function(e) {
    cat(sprintf("  [ERREUR] %s : %s\n", com_traite, conditionMessage(e)))
    NULL
  })
}

# Lancement de la boucle
results_list <- lapply(traites_panel, function(com) {
  cat(sprintf("  Estimation : %s ...\n", com))
  run_augsynth_unit(com)
})

names(results_list) <- traites_panel

# Agréger les résultats
dt_results_raw <- rbindlist(Filter(Negate(is.null), results_list), fill = TRUE)

# ============================================================
# ---- 5. MISE EN FORME DES RÉSULTATS ----
# ============================================================
# augsynth nomme les colonnes : Time, Estimate, Std.Error, lower_bound, upper_bound
# (les bornes viennent du confint interne — on les complète par jackknife section 6)

setnames(dt_results_raw,
         old = c("Time", "Estimate", "Std.Error"),
         new = c("annee_att", "ATT", "SE_augsynth"),
         skip_absent = TRUE)

# Joindre labels
labels_ref <- unique(dt[treated == 1, .(COM, label)])
dt_results_raw <- merge(dt_results_raw, labels_ref, by = "COM", all.x = TRUE)

# Trier
dt_results_raw[, sort_key := fcase(
  grepl("^75", COM), as.integer(COM) - 75100L,
  COM == "59350", 21L,
  COM == "59298", 22L,
  COM == "59355", 23L,
  default, 99L
)]
setorder(dt_results_raw, sort_key, annee_att)

cat("\nRésultats bruts (10 premières lignes) :\n")
print(dt_results_raw[!is.na(annee_att)][1:10,
      .(label, annee_att, ATT, SE_augsynth, n_controles)])

# ============================================================
# ---- 6. JACKKNIFE PAR UNITÉ : IC ROBUSTES SUR ATT 2022 ----
# ============================================================
cat("\n=== Jackknife par unité — IC robustes ATT 2022 ===\n")

jack_unit_list <- lapply(traites_panel, function(com_out) {

  clust_i   <- dt_cluster_pre[COM == com_out, cluster_pre]
  ctrl_pool <- dt_cluster_pre[!COM %in% traites & cluster_pre == clust_i, COM]

  # On retire une commune contrôle à la fois pour cette unité traitée
  if (length(ctrl_pool) < 6) return(NULL)

  jack_ctrl <- lapply(ctrl_pool, function(ctrl_omis) {
    ctrl_j <- setdiff(ctrl_pool, ctrl_omis)
    dt_j   <- dt[COM %in% c(com_out, ctrl_j)]
    dt_j[, treated_i    := as.integer(COM == com_out)]
    dt_j[, treat_post_i := treated_i * post]

    tryCatch({
      syn_j  <- augsynth(
        form     = nb_RP_en_loc ~ treat_post_i,
        unit     = COM,
        time     = annee,
        data     = dt_j,
        progfunc = "ridge",
        scm      = TRUE,
        lambda   = NULL
      )
      sj <- as.data.table(summary(syn_j)$att)
      sj[Time == 2022, Estimate]
    }, error = function(e) NA_real_)
  })

  att_jack_vals <- unlist(jack_ctrl)
  att_jack_vals <- att_jack_vals[!is.na(att_jack_vals)]
  n_j <- length(att_jack_vals)
  if (n_j < 3) return(NULL)

  theta_dot <- mean(att_jack_vals)
  jack_var  <- ((n_j - 1) / n_j) * sum((att_jack_vals - theta_dot)^2)

  data.table(
    COM      = com_out,
    jack_se  = sqrt(jack_var),
    jack_n   = n_j
  )
})

dt_jack <- rbindlist(Filter(Negate(is.null), jack_unit_list))
cat("Jackknife calculé pour", nrow(dt_jack), "unités traitées.\n")

# ============================================================
# ---- 7. TABLE FINALE ATT 2022 PAR UNITÉ ----
# ============================================================
dt_att2022 <- dt_results_raw[annee_att == 2022,
                               .(COM, label, sort_key, ATT, SE_augsynth,
                                 n_controles, cluster_pre)]

dt_final <- merge(dt_att2022, dt_jack, by = "COM", all.x = TRUE)

# SE robuste : jackknife si dispo, sinon augsynth interne
dt_final[, se_rob := fifelse(!is.na(jack_se), jack_se, SE_augsynth)]

z95 <- qnorm(0.975); z90 <- qnorm(0.95)
dt_final[, `:=`(
  ic95_lo  = ATT - z95 * se_rob,
  ic95_hi  = ATT + z95 * se_rob,
  ic90_lo  = ATT - z90 * se_rob,
  ic90_hi  = ATT + z90 * se_rob,
  pval     = 2 * pnorm(-abs(ATT / se_rob)),
  ville    = fcase(grepl("^75", COM), "Paris",
                   COM %in% c("59350","59298","59355"), "Lille",
                   default, "Autre")
)]
dt_final[, sig := fcase(
  pval < 0.01, "p < 0.01",
  pval < 0.05, "p < 0.05",
  pval < 0.10, "p < 0.10",
  default,      "n.s."
)]
dt_final[, sig := factor(sig, levels = c("p < 0.01","p < 0.05","p < 0.10","n.s."))]

setorder(dt_final, sort_key)

cat("\n=== RÉSULTATS FINAUX — ATT 2022 (nb_RP_en_loc, en logements) ===\n")
print(dt_final[, .(label, ATT = round(ATT), se_rob = round(se_rob),
                    ic95_lo = round(ic95_lo), ic95_hi = round(ic95_hi),
                    pval = round(pval, 3), n_controles, cluster_pre)],
      nrows = Inf)

fwrite(dt_final, "/home/claude/resultats_ascm_boucle.csv")
cat("\nTable exportée : resultats_ascm_boucle.csv\n")

# ============================================================
# ---- 8. GRAPHIQUES ----
# ============================================================

# -- 8a. Forest plot : ATT 2022 en nombre de logements --
p_forest <- ggplot(dt_final,
                   aes(y = reorder(label, -sort_key),
                       x = ATT, colour = ville)) +
  geom_vline(xintercept = 0, linetype = "dashed",
             colour = "grey50", linewidth = 0.8) +
  geom_errorbarh(aes(xmin = ic95_lo, xmax = ic95_hi),
                 height = 0, linewidth = 0.55, alpha = 0.55) +
  geom_errorbarh(aes(xmin = ic90_lo, xmax = ic90_hi),
                 height = 0, linewidth = 1.3,  alpha = 0.90) +
  geom_point(size = 3, shape = 21, fill = "white", stroke = 1.6) +
  scale_colour_manual(values = c("Paris" = "#2166ac", "Lille" = "#d6604d"),
                      name = NULL) +
  facet_grid(ville ~ ., scales = "free_y", space = "free_y") +
  labs(
    title    = "ASCM — Effet sur nb_RP_en_loc en 2022",
    subtitle = "ATT en nombre de logements locatifs | IC épais = 90%, fin = 95% | Jackknife",
    x        = "ATT (nombre de logements locatifs)",
    y        = NULL,
    caption  = "Un modèle augsynth distinct par unité traitée. Pool = communes du même cluster pré-2019."
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title         = element_text(face = "bold", size = 13),
    plot.subtitle      = element_text(colour = "grey40", size = 9),
    strip.text         = element_text(face = "bold", size = 10),
    legend.position    = "none",
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank()
  )

ggsave("/home/claude/plot_forest_boucle.png", p_forest,
       width = 9, height = 11, dpi = 150)
cat("Forest plot sauvegardé.\n")

# -- 8b. Trajectoires ATT dans le temps (nb de logements) --
dt_traj <- copy(dt_results_raw[!is.na(annee_att)])
dt_traj[, ville := fcase(
  grepl("^75", COM), "Paris",
  COM %in% c("59350","59298","59355"), "Lille",
  default, "Autre"
)]
# IC approx sur trajectoire : ATT ± 1.96 * SE_augsynth
dt_traj[, ci_lo := ATT - z95 * SE_augsynth]
dt_traj[, ci_hi := ATT + z95 * SE_augsynth]

p_traj <- ggplot(dt_traj,
                 aes(x = annee_att, y = ATT,
                     group = label, colour = label)) +
  geom_hline(yintercept = 0, linetype = "dashed",
             colour = "grey50", linewidth = 0.7) +
  geom_vline(xintercept = 2018.5, linetype = "dotted",
             colour = "grey30", linewidth = 0.7) +
  geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi, fill = label),
              alpha = 0.07, colour = NA) +
  geom_line(linewidth = 0.75, alpha = 0.8) +
  geom_point(size = 1.8) +
  facet_wrap(~ ville, ncol = 1, scales = "free_y") +
  scale_x_continuous(breaks = annees) +
  labs(
    title    = "ASCM — Trajectoires ATT par unité traitée",
    subtitle = "ATT en nombre de logements locatifs | IC 95% (SE augsynth)",
    x        = "Année", y = "ATT (nb logements locatifs)",
    caption  = "Ligne pointillée = 2019 (début traitement)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title       = element_text(face = "bold", size = 13),
    plot.subtitle    = element_text(colour = "grey40", size = 9),
    strip.text       = element_text(face = "bold"),
    axis.text.x      = element_text(angle = 45, hjust = 1),
    legend.position  = "right",
    legend.key.size  = unit(0.4, "cm"),
    legend.text      = element_text(size = 7),
    panel.grid.minor = element_blank()
  ) +
  guides(colour = guide_legend(ncol = 1, title = NULL), fill = "none")

ggsave("/home/claude/plot_trajectoires_boucle.png", p_traj,
       width = 11, height = 10, dpi = 150)
cat("Graphique trajectoires sauvegardé.\n")

# -- 8c. Heatmap ATT par unité × année (Paris uniquement) --
dt_paris <- dt_traj[ville == "Paris"]

p_heat <- ggplot(dt_paris,
                 aes(x = annee_att, y = reorder(label, -sort_key),
                     fill = ATT)) +
  geom_tile(colour = "white", linewidth = 0.4) +
  geom_vline(xintercept = 2018.5, colour = "black",
             linewidth = 1, linetype = "solid") +
  scale_fill_gradient2(
    low      = "#d6604d",
    mid      = "white",
    high     = "#2166ac",
    midpoint = 0,
    name     = "ATT\n(logements)"
  ) +
  scale_x_continuous(breaks = annees) +
  labs(
    title    = "Heatmap ATT — Paris par arrondissement",
    subtitle = "ATT en nombre de logements locatifs | Rouge = effet négatif",
    x = "Année", y = NULL,
    caption = "Trait vertical = 2019 (début encadrement)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title       = element_text(face = "bold", size = 13),
    plot.subtitle    = element_text(colour = "grey40", size = 9),
    axis.text.x      = element_text(angle = 45, hjust = 1),
    panel.grid       = element_blank(),
    legend.position  = "right"
  )

ggsave("/home/claude/plot_heatmap_paris.png", p_heat,
       width = 10, height = 8, dpi = 150)
cat("Heatmap Paris sauvegardée.\n")

cat("\n=== Analyse terminée ===\n")
cat("RAPPEL : Résultats issus de données SIMULÉES.\n")
