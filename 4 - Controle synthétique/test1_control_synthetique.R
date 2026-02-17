# =============================================================================
# SYNTHETIC CONTROL — Encadrement des loyers à Paris
# Données INSEE historiques (RP 1968–2022)
# Variable d'intérêt : résidences principales occupées par des nb_RP_en_loc
#
# Auteur  : [Votre nom]
# Date    : 2025
# Package : data.table + Synth (Abadie, Diamond & Hainmueller 2010)
# =============================================================================
#
# ── CONTEXTE ──────────────────────────────────────────────────────────────────
# L'encadrement des loyers à Paris a été instauré une première fois en 2015
# (loi ALUR), suspendu en 2017 après annulation judiciaire, puis réintroduit
# en juillet 2019. Selon votre question de recherche, vous pouvez traiter
# 2015 ou 2019 comme date de traitement.
# Ici on adopte 2019 comme date pivot (entre RP2016 et RP2022).
#
# ── VARIABLE OUTCOME ──────────────────────────────────────────────────────────
# Les fichiers INSEE "Évolution et structure de la nb_personnes_menage" ne fournissent
# pas directement le nombre de nb_RP_en_loc. Ce chiffre se trouve dans la table
# "Logement" (fichier MOD ou LOG détaillé). Dans les données agrégées de la
# base "Évolution" décrite dans votre document, on dispose de :
#   - P22_RP  : résidences principales totales
#   - P22_LOG : total logements
# Le taux de nb_RP_en_loc doit provenir d'un fichier complémentaire (détaillé
# ci-dessous en section 0). Si vous n'avez que les variables du document
# fourni, la section 0 explique comment les joindre.
#
# ── STRUCTURE DU SCRIPT ───────────────────────────────────────────────────────
# 0. Packages & constantes
# 1. Import et nettoyage des données
# 2. Construction du panel long
# 3. Calcul de la variable outcome (nb_RP_en_loc)
# 4. Sélection du groupe de contrôle (communes > 50 000 hab.)
# 5. Préparation des données pour Synth
# 6. Estimation du contrôle synthétique
# 7. Graphiques & tableaux de résultats
# 8. Tests de robustesse (placebo in-time, placebo in-space)
# =============================================================================


# =============================================================================
# 0. PACKAGES & CONSTANTES
# =============================================================================

# Installation si nécessaire
# install.packages(c("data.table", "Synth", "ggplot2", "scales", "knitr"))

library(data.table)   # Manipulation de données rapide
library(Synth)        # Contrôle synthétique (Abadie et al. 2010)
library(ggplot2)      # Graphiques
library(scales)       # Formatage des axes

# ── Constantes ────────────────────────────────────────────────────────────────
ANNEE_TRAITEMENT <- 2017   # Réintroduction de l'encadrement des loyers
SEUIL_POP       <- 50000   # Seuil de nb_personnes_menage minimale pour le pool de contrôle
CODE_PARIS      <- "75056" # COM de Paris commune entière

# Années des recensements disponibles dans les données INSEE "Évolution"
ANNEES_RP <- c(2012:2022)

# Mapping colonne → année pour la variable OUTCOME (nb_RP_en_loc, cf. section 3)
# Mapping colonne → année pour les covariables utilisées dans Synth


# =============================================================================
# 1. IMPORT ET NETTOYAGE DES DONNÉES
# =============================================================================

data = fread("../data_com_2012_2022.csv", sep = ";",
       dec = ",", encoding = "UTF-8")

panel = data

panel[, COM := fifelse(
  substr(COM, 1, 3) == "751", 
  "75056",
  COM
)]


num_cols <- names(panel)[sapply(panel, is.numeric)]
num_cols <- setdiff(num_cols, c("COM", "annee", "IRIS"))

panel_com <- panel[, 
                 lapply(.SD, sum, na.rm = TRUE),
                 by = .(COM, annee),
                 .SDcols = num_cols
]



# ── 3a. Variables dérivées ────────────────────────────────────────────────────
panel[, taux_nb_RP_en_loc  := nb_RP_en_loc / nb_RP]
panel[, taux_vacance     := nb_logements_vacants / nb_logements]
panel[, taille_moy_men   := nb_personnes_menage / nb_menages]  # personnes/ménage



communes_grandes <- panel[annee == 2022 & nb_personnes_menage >= SEUIL_POP, COM]

cat(sprintf("Communes avec pop >= %d en 2022 : %d\n",
            SEUIL_POP, length(communes_grandes)))

# on exclut les communes d'Ile de France pour éviter les
# effets de bords


communes_traitees <- c(
  "75056",  # Paris        — encadrement depuis juil. 2019
  "59350")  # Lille        — encadrement depuis mars 2020

# Pool de contrôle : grandes communes NON traitées
pool_control <- setdiff(communes_grandes, communes_traitees)
pool_control = pool_control[substr(pool_control, 1, 2) != "75"]


cat(sprintf("Communes dans le pool de contrôle : %d\n", length(pool_control)))

# ── 4c. Filtrer le panel sur les communes pertinentes ─────────────────────────
communes_retenues <- c(CODE_PARIS, pool_control)
panel_sc <- panel[COM %in% communes_retenues]



# =============================================================================
# 5. PRÉPARATION DES DONNÉES POUR Synth
# =============================================================================
# Le package Synth requiert :
#   - Un data.frame avec les colonnes : outcome, predictors, unit.variable,
#     time.variable
#   - Des identifiants numériques pour les unités

# ── 5a. Créer un identifiant numérique ────────────────────────────────────────
communes_uniques <- sort(unique(panel_sc$COM))

id_map <- data.table(
  COM = communes_uniques,
  unit_id = seq_along(communes_uniques)
)
panel_sc <- panel_sc[id_map, on = "COM"]

ID_PARIS <- id_map[COM %in% CODE_PARIS, unit_id]
ID_CONTROLS <- id_map[!(COM %in% CODE_PARIS), unit_id]

cat(sprintf("ID numérique de Paris : %d\n", ID_PARIS))
cat(sprintf("Nombre de communes de contrôle : %d\n", length(ID_CONTROLS)))

# ── 5b. Convertir en data.frame pour Synth ────────────────────────────────────
# ICI -----
# Synth n'accepte pas data.table, il faut passer par as.data.frame()
df_synth <- as.data.frame(panel_sc[, .(
  unit_id,
  annee,
  nb_RP_en_loc,          # variable outcome
  nb_personnes_menage,
  taux_vacance,
  taille_moy_men,
  taux_nb_RP_en_loc      # peut servir de prédicteur alternatif
)])

# ── 5c. Années pré-traitement et post-traitement ──────────────────────────────
# Traitement en 2019 → dernière obs. pré-traitement = 2016, post = 2022
ANNEES_PRETRAITEMENT  <- ANNEES_SC[ANNEES_SC <= 2017]  # 1999, 2006, 2011, 2016
ANNEES_POSTTRAITEMENT <- ANNEES_SC[ANNEES_SC > 2017]   # 2022

# Années utilisées pour l'optimisation des poids (toute la période pré)
TIME_OPTIMIZE_SSR <- ANNEES_PRETRAITEMENT

# Années pour le calcul des prédicteurs moyens
TIME_PREDICTORS_PRIOR <- ANNEES_PRETRAITEMENT

# ── 5d. Appel à dataprep() ────────────────────────────────────────────────────
# dataprep() structure les données au format attendu par synth()


dataprep_out <- dataprep(
  foo                   = df_synth,
  predictors            = c("nb_personnes_menage", "taux_vacance", "taille_moy_men"),
  predictors.op         = "mean",           # moyenne sur la période pré-traitement
  special.predictors    = list(
    # On ajoute des valeurs de l'outcome à des années charnières comme
    # prédicteurs supplémentaires — approche recommandée par Abadie et al.
    list("nb_RP_en_loc", 2012, "mean"),
    list("nb_RP_en_loc", 2013, "mean"),
    list("nb_RP_en_loc", 2014, "mean"),
    list("nb_RP_en_loc", 2015, "mean")
  ),
  dependent             = "nb_RP_en_loc",
  unit.variable         = "unit_id",
  unit.names.variable   = NULL,             # optionnel : nom textuel des unités
  time.variable         = "annee",
  treatment.identifier  = ID_PARIS,
  controls.identifier   = ID_CONTROLS,
  time.predictors.prior = TIME_PREDICTORS_PRIOR,
  time.optimize.ssr     = TIME_OPTIMIZE_SSR,
  time.plot             = ANNEES_SC
)


# =============================================================================
# 6. ESTIMATION DU CONTRÔLE SYNTHÉTIQUE
# =============================================================================

# ── 6a. Optimisation des poids ────────────────────────────────────────────────
# synth() minimise la distance entre Paris et le Paris synthétique sur la
# période pré-traitement en trouvant :
#   W* : poids des communes de contrôle (somme = 1, ≥ 0)
#   V* : matrice de pondération des prédicteurs (importance relative)

set.seed(42)   # Reproductibilité

synth_out <- synth(
  data.prep.obj = dataprep_out,
  method        = "BFGS"        # Méthode d'optimisation (alternative : "Nelder-Mead")
)

# ── 6b. Tableaux de résultats ─────────────────────────────────────────────────
# Poids des communes de contrôle
tab_w <- synth.tab(
  dataprep.res = dataprep_out,
  synth.res    = synth_out
)

cat("\n=== POIDS DES COMMUNES DE CONTRÔLE (W*) ===\n")
# Filtrer les poids non négligeables
w_table <- as.data.table(tab_w$tab.w)
setnames(w_table, c("w.weights","unit.names","unit.numbers"),
         c("poids","COM","unit_id"), skip_absent = TRUE)
# Joindre les libellés
w_table <- w_table[id_map, on = "unit_id", nomatch = NA]
w_table_nonzero <- w_table[poids > 0.001][order(-poids)]
print(w_table_nonzero[, .(COM, poids)])

cat("\n=== ÉQUILIBRE DES PRÉDICTEURS (V*) ===\n")
print(tab_w$tab.v)

cat("\n=== COMPARAISON PARIS / PARIS SYNTHÉTIQUE (pré-traitement) ===\n")
print(tab_w$tab.pred)


# =============================================================================
# 7. GRAPHIQUES ET RÉSULTATS
# =============================================================================

# ── 7a. Extraire les trajectoires ─────────────────────────────────────────────
# Trajectoire observée de Paris
Y_paris_obs   <- dataprep_out$Y1plot          # vecteur (années × 1)

# Trajectoire du Paris synthétique = Y_donneur × W*
Y_paris_synth <- dataprep_out$Y0plot %*% synth_out$solution.w

# Effet causal estimé (diff post-traitement)
effet_annuel <- Y_paris_obs - Y_paris_synth

# Data.frame pour ggplot
df_plot <- data.frame(
  annee   = ANNEES_SC,
  observee  = as.numeric(Y_paris_obs),
  synthetique = as.numeric(Y_paris_synth)
)

# ── 7b. Graphique principal ───────────────────────────────────────────────────
p_main <- ggplot(df_plot, aes(x = annee)) +
  geom_line(aes(y = observee,    colour = "Paris observé"),   linewidth = 1.2) +
  geom_line(aes(y = synthetique, colour = "Paris synthétique"),
            linetype = "dashed", linewidth = 1.2) +
  geom_vline(xintercept = ANNEE_TRAITEMENT, linetype = "dotted",
             colour = "grey40", linewidth = 0.9) +
  annotate("text", x = ANNEE_TRAITEMENT + 0.3, y = max(df_plot$observee) * 0.98,
           label = "Encadrement\ndes loyers", hjust = 0, size = 3.5,
           colour = "grey40") +
  scale_colour_manual(
    values = c("Paris observé" = "#1a6fba", "Paris synthétique" = "#d44f0c")
  ) +
  scale_y_continuous(labels = scales::comma_format(big.mark = " ")) +
  scale_x_continuous(breaks = ANNEES_SC) +
  labs(
    title    = "Contrôle synthétique — Résidences principales en location à Paris",
    subtitle = paste0("Communes de contrôle : pop. ≥ ", format(SEUIL_POP, big.mark = " "),
                      " hab., non soumises à l'encadrement"),
    x        = "Année du recensement",
    y        = "Nb de résidences principales — nb_RP_en_loc",
    colour   = NULL,
    caption  = "Source : INSEE (RP 1999–2022). Méthode : Abadie, Diamond & Hainmueller (2010)."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position  = "top",
    panel.grid.minor = element_blank(),
    plot.title       = element_text(face = "bold")
  )

print(p_main)
ggsave("sc_paris_loyers_principal.png", p_main, width = 10, height = 6, dpi = 300)

# ── 7c. Graphique de l'effet causal (gap plot) ────────────────────────────────
df_gap <- data.frame(
  annee = ANNEES_SC,
  gap   = as.numeric(effet_annuel)
)

p_gap <- ggplot(df_gap, aes(x = annee, y = gap)) +
  geom_hline(yintercept = 0, colour = "grey60") +
  geom_vline(xintercept = ANNEE_TRAITEMENT, linetype = "dotted",
             colour = "grey40", linewidth = 0.9) +
  geom_line(colour = "#1a6fba", linewidth = 1.2) +
  geom_point(colour = "#1a6fba", size = 3) +
  scale_y_continuous(labels = scales::comma_format(big.mark = " ")) +
  scale_x_continuous(breaks = ANNEES_SC) +
  labs(
    title   = "Effet causal estimé (gap Paris observé − Paris synthétique)",
    x       = "Année du recensement",
    y       = "Différence (nb de logements locatifs)",
    caption = "Valeurs positives = Paris a plus de nb_RP_en_loc que prédit."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  )

print(p_gap)
ggsave("sc_paris_loyers_gap.png", p_gap, width = 10, height = 5, dpi = 300)


# =============================================================================
# 8. TESTS DE ROBUSTESSE
# =============================================================================

# ── 8a. TEST PLACEBO DANS L'ESPACE (In-space placebo) ────────────────────────
# Idée : appliquer la même méthode à chaque commune du groupe de contrôle
# comme si elle avait été traitée. Si l'effet de Paris est bien identifié,
# les gaps des communes placebos devraient être petits comparés à celui de Paris.
#
# Durée : potentiellement long (1 optimisation par commune de contrôle)

run_placebo_space <- TRUE   # Mettre FALSE pour sauter cette section

if (run_placebo_space) {
  
  placebo_gaps <- lapply(ID_CONTROLS, function(id_placebo) {
    
    tryCatch({
      # Le "traité placebo" est la commune id_placebo
      # Le groupe de contrôle du placebo = toutes les autres communes SAUF Paris
      controls_placebo <- setdiff(ID_CONTROLS, id_placebo)
      
      dp_placebo <- dataprep(
        foo                   = df_synth,
        predictors            = c("nb_personnes_menage","taux_vacance","taille_moy_men"),
        predictors.op         = "mean",
        special.predictors    = list(
          list("nb_RP_en_loc", 1999, "mean"),
          list("nb_RP_en_loc", 2006, "mean"),
          list("nb_RP_en_loc", 2011, "mean"),
          list("nb_RP_en_loc", 2016, "mean")
        ),
        dependent             = "nb_RP_en_loc",
        unit.variable         = "unit_id",
        time.variable         = "annee",
        treatment.identifier  = id_placebo,
        controls.identifier   = controls_placebo,
        time.predictors.prior = TIME_PREDICTORS_PRIOR,
        time.optimize.ssr     = TIME_OPTIMIZE_SSR,
        time.plot             = ANNEES_SC
      )
      
      synth_placebo <- synth(dp_placebo, method = "BFGS", verbose = FALSE)
      
      gap <- dp_placebo$Y1plot -
        dp_placebo$Y0plot %*% synth_placebo$solution.w
      
      data.frame(
        unit_id = id_placebo,
        annee   = ANNEES_SC,
        gap     = as.numeric(gap)
      )
      
    }, error = function(e) {
      message(sprintf("Erreur pour unit_id %d : %s", id_placebo, e$message))
      NULL
    })
  })
  
  df_placebos <- rbindlist(Filter(Negate(is.null), placebo_gaps))
  
  # Ajouter le gap de Paris
  df_paris_gap <- data.frame(
    unit_id = ID_PARIS,
    annee   = ANNEES_SC,
    gap     = as.numeric(effet_annuel),
    is_paris = TRUE
  )
  df_placebos[, is_paris := FALSE]
  df_all_gaps <- rbind(df_placebos, df_paris_gap, fill = TRUE)
  df_all_gaps[is.na(is_paris), is_paris := FALSE]
  
  # ── Graphique placebo in-space ─────────────────────────────────────────────
  p_placebo <- ggplot() +
    geom_line(
      data = df_all_gaps[is_paris == FALSE],
      aes(x = annee, y = gap, group = unit_id),
      colour = "grey75", alpha = 0.6, linewidth = 0.5
    ) +
    geom_line(
      data = df_all_gaps[is_paris == TRUE],
      aes(x = annee, y = gap),
      colour = "#1a6fba", linewidth = 1.5
    ) +
    geom_hline(yintercept = 0, colour = "grey40") +
    geom_vline(xintercept = ANNEE_TRAITEMENT, linetype = "dotted",
               colour = "grey40") +
    scale_x_continuous(breaks = ANNEES_SC) +
    scale_y_continuous(labels = scales::comma_format(big.mark = " ")) +
    labs(
      title   = "Test placebo dans l'espace",
      subtitle = "Bleu = Paris | Gris = communes placebos",
      x       = "Année",
      y       = "Gap (observé − synthétique)",
      caption = "Si le gap de Paris post-2019 dépasse nettement ceux des placebos, l'effet est significatif."
    ) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold")
    )
  
  print(p_placebo)
  ggsave("sc_paris_placebo_espace.png", p_placebo, width = 10, height = 6, dpi = 300)
  
  # ── Ratio RMSPE post/pré (mesure d'inférence) ─────────────────────────────
  # Un ratio élevé pour Paris signifie que son gap post est anormalement grand
  # par rapport à l'ajustement pré-traitement.
  rmspe_table <- df_all_gaps[, .(
    rmspe_pre  = sqrt(mean(gap[annee %in% ANNEES_PRETRAITEMENT]^2)),
    rmspe_post = sqrt(mean(gap[annee %in% ANNEES_POSTTRAITEMENT]^2))
  ), by = .(unit_id, is_paris)]
  
  rmspe_table[, ratio := rmspe_post / rmspe_pre]
  rmspe_table <- rmspe_table[order(-ratio)]
  
  rang_paris <- which(rmspe_table$unit_id == ID_PARIS)
  p_value_empirique <- rang_paris / nrow(rmspe_table)
  
  cat(sprintf(
    "\n=== INFÉRENCE PLACEBO ===\nRatio RMSPE post/pré de Paris : %.2f\n",
    rmspe_table[unit_id == ID_PARIS, ratio]
  ))
  cat(sprintf(
    "Rang de Paris parmi %d unités : %d\n",
    nrow(rmspe_table), rang_paris
  ))
  cat(sprintf(
    "p-value empirique : %.3f\n", p_value_empirique
  ))
}


# ── 8b. TEST PLACEBO DANS LE TEMPS (In-time placebo) ─────────────────────────
# Idée : avancer fictivement la date de traitement (ex. 2011 au lieu de 2019).
# Si on trouve un grand effet même avant le traitement réel, c'est suspect.

run_placebo_time <- TRUE

if (run_placebo_time) {
  
  ANNEE_FAUSSE <- 2011   # Date fictive de traitement
  
  ANNEES_PRE_FAKE  <- ANNEES_SC[ANNEES_SC <  ANNEE_FAUSSE]
  ANNEES_POST_FAKE <- ANNEES_SC[ANNEES_SC >= ANNEE_FAUSSE &
                                  ANNEES_SC <  ANNEE_TRAITEMENT]
  
  if (length(ANNEES_PRE_FAKE) < 2) {
    message("Pas assez d'années pré-traitement fictives. Test placebo temps ignoré.")
  } else {
    
    dp_time_placebo <- dataprep(
      foo                   = df_synth,
      predictors            = c("nb_personnes_menage","taux_vacance","taille_moy_men"),
      predictors.op         = "mean",
      special.predictors    = list(
        list("nb_RP_en_loc", 1999, "mean"),
        list("nb_RP_en_loc", 2006, "mean")
      ),
      dependent             = "nb_RP_en_loc",
      unit.variable         = "unit_id",
      time.variable         = "annee",
      treatment.identifier  = ID_PARIS,
      controls.identifier   = ID_CONTROLS,
      time.predictors.prior = ANNEES_PRE_FAKE,
      time.optimize.ssr     = ANNEES_PRE_FAKE,
      time.plot             = c(ANNEES_PRE_FAKE, ANNEES_POST_FAKE)
    )
    
    synth_time_placebo <- synth(dp_time_placebo, method = "BFGS")
    gap_time <- dp_time_placebo$Y1plot -
      dp_time_placebo$Y0plot %*% synth_time_placebo$solution.w
    
    df_gap_time <- data.frame(
      annee = c(ANNEES_PRE_FAKE, ANNEES_POST_FAKE),
      gap   = as.numeric(gap_time)
    )
    
    p_placebo_time <- ggplot(df_gap_time, aes(x = annee, y = gap)) +
      geom_hline(yintercept = 0) +
      geom_vline(xintercept = ANNEE_FAUSSE, linetype = "dashed",
                 colour = "firebrick") +
      geom_line(colour = "#1a6fba", linewidth = 1.2) +
      geom_point(colour = "#1a6fba", size = 3) +
      annotate("text", x = ANNEE_FAUSSE + 0.3,
               y = max(abs(df_gap_time$gap)) * 0.9,
               label = paste0("Traitement fictif\n(", ANNEE_FAUSSE, ")"),
               colour = "firebrick", hjust = 0, size = 3.5) +
      scale_x_continuous(breaks = c(ANNEES_PRE_FAKE, ANNEES_POST_FAKE)) +
      scale_y_continuous(labels = scales::comma_format(big.mark = " ")) +
      labs(
        title    = paste0("Test placebo dans le temps (traitement fictif en ", ANNEE_FAUSSE, ")"),
        subtitle = "Si le gap reste proche de 0 avant 2019, la tendance pré-traitement est bien captée.",
        x        = "Année",
        y        = "Gap (observé − synthétique fictif)"
      ) +
      theme_minimal(base_size = 12) +
      theme(panel.grid.minor = element_blank(),
            plot.title = element_text(face = "bold"))
    
    print(p_placebo_time)
    ggsave("sc_paris_placebo_temps.png", p_placebo_time,
           width = 10, height = 5, dpi = 300)
  }
}


# =============================================================================
# FIN DU SCRIPT
# =============================================================================
cat("\n✓ Script terminé. Fichiers produits :\n")
cat("  - sc_paris_loyers_principal.png\n")
cat("  - sc_paris_loyers_gap.png\n")
cat("  - sc_paris_placebo_espace.png\n")
cat("  - sc_paris_placebo_temps.png\n")
