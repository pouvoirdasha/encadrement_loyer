# =============================================================================
# RÉGRESSIONS DiD – ENCADREMENT DES LOYERS
# Spécification :
#   Y_it = α + β(G_i × T_t) + γ G_i + δ T_t + X_it'θ + μ_i + ν_commune + ε_it
#
#   où T_t = 1 si annee == 2022 (post-traitement)
#      G_i = 1 si IRIS traité
#      μ_i     = effets fixes IRIS
#      ν_comm  = effets fixes commune
#
# DID bitemporel : 2017 (pré) vs 2022 (post)
# Trois jeux de matchings : Paris-Lille full | Paris-Lille 3 ans | Paris full
# Sorties : tableaux LaTeX + CSV dans 8-Matching/regressions/
# =============================================================================

# -----------------------------------------------------------------------------
# 0. PACKAGES
# -----------------------------------------------------------------------------
library(tidyverse)
library(glue)
library(fixest)      # feols() : OLS avec effets fixes haute dimension + clustered SE
library(modelsummary) # export de tables de régression multi-modèles
library(kableExtra)  # formatage LaTeX/HTML
library(scales)

# -----------------------------------------------------------------------------
# 1. CHEMINS & PARAMÈTRES
# -----------------------------------------------------------------------------

PATH_PAIRES_PARIS_LILLE_FULL  <- "data/matching_paires_2006-17_Paris_Lille.csv"
PATH_PAIRES_PARIS_LILLE_3YRS  <- "data/matching_paires_2007_12_17_Paris_Lille.csv"
PATH_PAIRES_PARIS_FULL        <- "data/matching_paires_Paris_Trajectoire_Full.csv"
PATH_BDD                      <- "data/bdd_finale_mathching.csv"
 
DIR_REG <- "8-Matching/regressions"

# Années DiD  (pré = 2017, post = 2022)
ANNEE_PRE  <- 2017
ANNEE_POST <- 2022
ANNEES_DID <- c(ANNEE_PRE, ANNEE_POST)

# Seuil population
SEUIL_POP <- 10000

# Variable dépendante principale
VAR_DEP <- "nb_RP_en_loc"

# Variables de contrôle (X_it) — seront conservées si présentes dans la BDD
VARS_CONTROLE <- c(
  "taux_vacance",
  "taux_chomage",
  "densite_pop",
  "part_cadres",
  "part_ouvriers",
  "part_etudiants",
  "part_hlm",
  "part_1p", "part_2p", "part_3p",
  "log_logements",
  "pop_totale"          # population totale commune
)

# Noms lisibles pour les tableaux
COEF_LABELS <- c(
  "G_i:T_t"       = "Encadrement × Post (β)",
  "G_i"           = "Groupe traité (γ)",
  "T_t"           = "Post-2022 (δ)",
  "taux_vacance"  = "Taux de vacance",
  "taux_chomage"  = "Taux de chômage",
  "densite_pop"   = "Densité de population",
  "part_cadres"   = "Part des cadres",
  "part_ouvriers" = "Part des ouvriers",
  "part_etudiants"= "Part des étudiants",
  "part_hlm"      = "Part HLM",
  "part_1p"       = "Part logements 1p",
  "part_2p"       = "Part logements 2p",
  "part_3p"       = "Part logements 3p",
  "log_logements" = "Log(logements)",
  "pop_totale"    = "Population totale"
)

# -----------------------------------------------------------------------------
# 2. FONCTIONS UTILITAIRES
# -----------------------------------------------------------------------------

#' Créer les dossiers de sortie
create_dirs <- function(path) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
}

#' Charger les paires de matching
load_paires <- function(path) {
  read_csv(path, show_col_types = FALSE) %>%
    select(IRIS_trait, IRIS_ctrl) %>%
    distinct()
}

#' Construire le panel DiD à partir des paires et de la BDD
#'
#' @param paires    data.frame (IRIS_trait, IRIS_ctrl)
#' @param bdd       base de données complète (long, col: IRIS, annee, ...)
#' @param annees    vecteur des deux années DiD
#' @param seuil_pop seuil minimum de population par commune
#'
#' @return data.frame prêt pour la régression, avec colonnes :
#'   IRIS, annee, commune, G_i, T_t, interaction G_i:T_t, Y, X...
prepare_did <- function(paires, bdd, annees = ANNEES_DID,
                         seuil_pop = SEUIL_POP) {

  iris_trait <- unique(paires$IRIS_trait)
  iris_ctrl  <- unique(paires$IRIS_ctrl)
  iris_all   <- union(iris_trait, iris_ctrl)

  panel <- bdd %>%
    filter(IRIS %in% iris_all, annee %in% annees) %>%
    mutate(
      # Indicatrice de groupe
      G_i = as.integer(IRIS %in% iris_trait),
      # Indicatrice temporelle (post = 2022)
      T_t = as.integer(annee == ANNEE_POST)
    )

  # Filtrage par population >= seuil_pop (par commune, non par IRIS)
  if ("pop_totale" %in% names(panel) && "commune" %in% names(panel)) {
    pop_comm <- panel %>%
      filter(annee == ANNEE_PRE) %>%
      group_by(commune) %>%
      summarise(pop_comm = sum(pop_totale, na.rm = TRUE), .groups = "drop")

    comm_keep <- pop_comm %>%
      filter(pop_comm >= seuil_pop) %>%
      pull(commune)

    n_avant <- n_distinct(panel$IRIS)
    panel <- panel %>% filter(commune %in% comm_keep)
    message(glue("  Filtrage population : {n_avant} → {n_distinct(panel$IRIS)} IRIS"))

  } else if ("pop_totale" %in% names(panel)) {
    # Filtrage par IRIS si commune absente
    pop_iris <- panel %>%
      filter(annee == ANNEE_PRE) %>%
      group_by(IRIS) %>%
      summarise(pop = mean(pop_totale, na.rm = TRUE), .groups = "drop")

    iris_keep <- pop_iris %>% filter(pop >= seuil_pop) %>% pull(IRIS)
    panel <- panel %>% filter(IRIS %in% iris_keep)
    message(glue("  Filtrage population (IRIS) : {n_distinct(panel$IRIS)} IRIS retenus"))

  } else {
    message("  ⚠ Variable 'pop_totale' absente – filtrage population ignoré.")
  }

  # Variables de contrôle disponibles
  vars_ctrl_dispo <- intersect(VARS_CONTROLE, names(panel))
  if (length(vars_ctrl_dispo) < length(VARS_CONTROLE)) {
    manquantes <- setdiff(VARS_CONTROLE, names(panel))
    message(glue("  ⚠ Variables de contrôle absentes : {paste(manquantes, collapse=', ')}"))
  }

  # Colonne commune (extraire les 5 premiers chiffres de l'IRIS = code commune INSEE)
  if (!"commune" %in% names(panel)) {
    panel <- panel %>%
      mutate(commune = str_sub(as.character(IRIS), 1, 5))
  }

  # S'assurer que IRIS et commune sont des facteurs pour les effets fixes
  panel <- panel %>%
    mutate(
      IRIS    = factor(IRIS),
      commune = factor(commune)
    )

  panel
}

#' Estimer les 3 spécifications DiD pour un panel donné
#'
#' Spécification (1) : OLS naïf sans EF
#' Spécification (2) : OLS + EF commune
#' Spécification (3) : TWFE (EF IRIS + EF commune) — absorbe G_i et T_t
#'   → dans ce cas β est identifié par la variation within-IRIS
#'
#' Note : avec EF IRIS, G_i est colinéaire (time-invariant) et donc absorbé.
#'        La spécification (3) inclut donc uniquement l'interaction G_i:T_t.
#'
#' @param panel   data.frame préparé par prepare_did()
#' @param label   label du matching pour les messages
#'
#' @return liste nommée de modèles fixest
run_did <- function(panel, label) {

  message(glue("\n  --- Régressions : {label} ---"))
  message(glue("  N obs = {nrow(panel)} | N IRIS = {n_distinct(panel$IRIS)}"))
  message(glue("  Traités : {sum(panel$G_i == 1 & panel$T_t == 0)} obs pré, ",
               "{sum(panel$G_i == 1 & panel$T_t == 1)} obs post"))
  message(glue("  Contrôles : {sum(panel$G_i == 0 & panel$T_t == 0)} obs pré, ",
               "{sum(panel$G_i == 0 & panel$T_t == 1)} obs post"))

  # Variables de contrôle disponibles
  ctrl <- intersect(VARS_CONTROLE, names(panel))
  ctrl_formula <- if (length(ctrl) > 0) paste(ctrl, collapse = " + ") else "1"

  # Vérification variable dépendante
  if (!VAR_DEP %in% names(panel)) {
    stop(glue("❌ Variable dépendante '{VAR_DEP}' absente du panel."))
  }

  # --------------------------------------------------------------------------
  # Spécification (1) : OLS naïf – G_i + T_t + G_i:T_t + contrôles
  #   SE clusterisées par commune
  # --------------------------------------------------------------------------
  f1 <- as.formula(glue(
    "{VAR_DEP} ~ G_i * T_t + {ctrl_formula}"
  ))

  m1 <- feols(f1, data = panel,
              cluster = ~commune,
              notes   = FALSE)

  # --------------------------------------------------------------------------
  # Spécification (2) : OLS + effets fixes commune
  #   G_i absorbé par EF commune si commune = unité d'analyse,
  #   mais IRIS ≠ commune donc G_i reste identifié.
  # --------------------------------------------------------------------------
  f2 <- as.formula(glue(
    "{VAR_DEP} ~ G_i * T_t + {ctrl_formula} | commune"
  ))

  m2 <- feols(f2, data = panel,
              cluster = ~commune,
              notes   = FALSE)

  # --------------------------------------------------------------------------
  # Spécification (3) : TWFE — EF IRIS + EF commune
  #   G_i est absorbé (time-invariant within-IRIS).
  #   T_t est commun à tous → potentiellement absorbé par EF temps
  #   → on le laisse explicite et fixest gère la colinéarité.
  # --------------------------------------------------------------------------
  f3 <- as.formula(glue(
    "{VAR_DEP} ~ G_i:T_t + T_t + {ctrl_formula} | IRIS + commune"
  ))

  m3 <- feols(f3, data = panel,
              cluster = ~commune,
              notes   = FALSE)

  list(
    "(1) OLS"              = m1,
    "(2) EF Commune"       = m2,
    "(3) TWFE IRIS+Commune"= m3
  )
}

#' Exporter les résultats : table console, CSV des coefficients, LaTeX
#'
#' @param modeles  liste nommée de modèles (output de run_did)
#' @param label    label du matching (pour noms de fichiers)
#' @param dir_out  dossier de sortie
export_results <- function(modeles, label, dir_out) {

  safe_label <- gsub("[^a-zA-Z0-9]", "_", label)

  # ---- 1. Table affichée en console ----------------------------------------
  message(glue("\n  ── Résultats : {label} ──"))
  print(
    modelsummary(
      modeles,
      coef_rename  = COEF_LABELS,
      coef_omit    = "Intercept",
      statistic    = "({std.error})",
      stars        = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
      gof_map      = c("nobs", "r.squared", "adj.r.squared",
                       "rmse", "FE: IRIS", "FE: commune"),
      output       = "dataframe"
    )
  )

  # ---- 2. CSV brut des coefficients ----------------------------------------
  coef_df <- map_dfr(names(modeles), function(nm) {
    m <- modeles[[nm]]
    tidy_m <- broom::tidy(m, conf.int = TRUE)
    tidy_m %>%
      mutate(
        modele  = nm,
        matching = label
      )
  })

  csv_path <- file.path(dir_out, glue("coefs_{safe_label}.csv"))
  write_csv(coef_df, csv_path)
  message(glue("  ✓ CSV coefficients : {csv_path}"))

  # ---- 3. Table LaTeX -------------------------------------------------------
  latex_table <- modelsummary(
    modeles,
    coef_rename  = COEF_LABELS,
    coef_omit    = "Intercept",
    statistic    = "({std.error})",
    stars        = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
    gof_map      = c("nobs", "r.squared", "adj.r.squared",
                     "rmse", "FE: IRIS", "FE: commune"),
    title        = glue("Estimation DiD – {label}"),
    notes        = list(
      "Erreurs-types clusterisées au niveau commune entre parenthèses.",
      "* p<0,10 ; ** p<0,05 ; *** p<0,01",
      glue("Variable dépendante : {VAR_DEP}. Période : {ANNEE_PRE}–{ANNEE_POST}.")
    ),
    output = "latex_tabular"
  )

  latex_path <- file.path(dir_out, glue("table_{safe_label}.tex"))
  writeLines(paste(as.character(latex_table), collapse = "\n"), path)
  message(glue("  ✓ Table LaTeX    : {latex_path}"))

  # ---- 4. Table HTML (pour vérification rapide) ----------------------------
  html_table <- modelsummary(
    modeles,
    coef_rename  = COEF_LABELS,
    coef_omit    = "Intercept",
    statistic    = "({std.error})",
    stars        = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
    gof_map      = c("nobs", "r.squared", "adj.r.squared",
                     "rmse", "FE: IRIS", "FE: commune"),
    title        = glue("Estimation DiD – {label}"),
    output       = "html"
  )

  html_path <- file.path(dir_out, glue("table_{safe_label}.html"))
  writeLines(html_table, html_path)
  message(glue("  ✓ Table HTML     : {html_path}"))

  invisible(coef_df)
}

#' Graphique des coefficients β (coefficient plot) sur les 3 spécifications
#'
#' @param coef_df   data.frame issu de export_results (tous matchings combinés)
#' @param dir_out   dossier de sortie
plot_coef <- function(coef_df, dir_out) {

  # Filtrer uniquement le coefficient d'intérêt
  beta_df <- coef_df %>%
    filter(str_detect(term, "G_i.*T_t|T_t.*G_i")) %>%
    mutate(
      modele   = factor(modele,
                         levels = c("(1) OLS", "(2) EF Commune",
                                    "(3) TWFE IRIS+Commune")),
      sig      = case_when(
        p.value < 0.01 ~ "***",
        p.value < 0.05 ~ "**",
        p.value < 0.10 ~ "*",
        TRUE           ~ ""
      )
    )

  if (nrow(beta_df) == 0) {
    message("  ⚠ Aucun coefficient β trouvé pour le coefficient plot.")
    return(invisible(NULL))
  }

  p <- ggplot(beta_df,
              aes(x = estimate, y = modele,
                  colour = matching, shape = matching)) +
    geom_vline(xintercept = 0, linetype = "dashed",
               colour = "grey50", linewidth = 0.6) +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                   height = 0.25, linewidth = 0.7) +
    geom_point(size = 3) +
    geom_text(aes(label = sig, x = conf.high),
              hjust = -0.4, size = 4, fontface = "bold") +
    facet_wrap(~matching, ncol = 1, scales = "free_y") +
    scale_colour_brewer(palette = "Dark2") +
    labs(
      title    = glue("Effet de l'encadrement des loyers sur {VAR_DEP}"),
      subtitle = glue("Coefficient β (G_i × T_t), DiD {ANNEE_PRE}–{ANNEE_POST}"),
      x        = "Estimateur DiD (β) avec IC 95%",
      y        = "Spécification",
      caption  = "Erreurs-types clusterisées par commune. * p<0,10 ; ** p<0,05 ; *** p<0,01"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title       = element_text(face = "bold"),
      strip.text       = element_text(face = "bold"),
      legend.position  = "none",
      panel.grid.minor = element_blank()
    )

  path_plot <- file.path(dir_out, "coefficient_plot_beta.png")
  ggsave(path_plot, plot = p, width = 9, height = 7, dpi = 150, bg = "white")
  message(glue("  ✓ Coefficient plot : {path_plot}"))

  p
}

# -----------------------------------------------------------------------------
# 3. CHARGEMENT DES DONNÉES
# -----------------------------------------------------------------------------

message("\n", strrep("=", 65))
message("  CHARGEMENT DES DONNÉES")
message(strrep("=", 65))

bdd <- read_csv(PATH_BDD, show_col_types = FALSE)
message(glue("  BDD : {nrow(bdd)} lignes × {ncol(bdd)} colonnes"))

# Harmonisation du nom de colonne 'annee'
if (!"annee" %in% names(bdd)) {
  bdd <- bdd %>%
    rename(any_of(c(annee = "year", annee = "ANNEE", annee = "Year")))
  if (!"annee" %in% names(bdd))
    stop("Colonne 'annee' introuvable dans la BDD.")
}

# Vérification que les deux années DiD sont bien présentes
annees_dispo <- sort(unique(bdd$annee))
message(glue("  Années disponibles : {paste(annees_dispo, collapse = ', ')}"))

if (!all(ANNEES_DID %in% annees_dispo)) {
  manq <- setdiff(ANNEES_DID, annees_dispo)
  stop(glue("Années DiD manquantes dans la BDD : {paste(manq, collapse=', ')}"))
}

# Création du code commune si absent (5 premiers caractères de l'IRIS)
if (!"commune" %in% names(bdd)) {
  bdd <- bdd %>%
    mutate(commune = str_sub(as.character(IRIS), 1, 5))
  message("  Code commune créé depuis les 5 premiers chiffres de l'IRIS.")
}

# -----------------------------------------------------------------------------
# 4. CHARGEMENT DES PAIRES
# -----------------------------------------------------------------------------


message("  CHARGEMENT DES PAIRES DE MATCHING")

safe_load <- function(path, label) {
  tryCatch(
    { p <- load_paires(path)
      message(glue("  ✓ {label} : {nrow(p)} paires"))
      p },
    error = function(e) {
      message(glue("  ⚠ Fichier introuvable : {path}"))
      NULL
    }
  )
}

paires_pl_full   <- safe_load(PATH_PAIRES_PARIS_LILLE_FULL,
                               "Paris-Lille full (2006–2017)")
paires_pl_3yrs   <- safe_load(PATH_PAIRES_PARIS_LILLE_3YRS,
                               "Paris-Lille 3 ans (2007/2012/2017)")
paires_paris_full <- safe_load(PATH_PAIRES_PARIS_FULL,
                                "Paris full (2006–2017)")

# -----------------------------------------------------------------------------
# 5. CRÉATION DU DOSSIER DE SORTIE
# -----------------------------------------------------------------------------

create_dirs(DIR_REG)

# -----------------------------------------------------------------------------
# 6. RÉGRESSIONS PAR TYPE DE MATCHING
# -----------------------------------------------------------------------------

message("\n", strrep("=", 65))
message("  RÉGRESSIONS DiD")
message(strrep("=", 65))

# Conteneur pour agréger les β de tous les matchings (coefficient plot global)
all_coefs <- list()

# ---- Helper : préparer + estimer + exporter ---------------------------------
run_and_export <- function(paires, label) {
  if (is.null(paires)) {
    message(glue("\n  ⚠ Matching '{label}' ignoré (paires non chargées)."))
    return(invisible(NULL))
  }

  panel   <- prepare_did(paires, bdd)
  modeles <- run_did(panel, label)
  coefs   <- export_results(modeles, label, DIR_REG)

  all_coefs[[label]] <<- coefs
  invisible(modeles)
}

# ---- 6a. Paris & Lille – full -----------------------------------------------
modeles_pl_full <- run_and_export(
  paires_pl_full,
  "Paris_Lille_2006_2017"
)

# ---- 6b. Paris & Lille – 3 années -------------------------------------------
modeles_pl_3yrs <- run_and_export(
  paires_pl_3yrs,
  "Paris_Lille_2007_2012_2017"
)

# ---- 6c. Paris – full -------------------------------------------------------
modeles_paris_full <- run_and_export(
  paires_paris_full,
  "Paris_2006_2017"
)

# -----------------------------------------------------------------------------
# 7. TABLEAU COMPARATIF TOUS MATCHINGS
# -----------------------------------------------------------------------------

message("\n", strrep("=", 65))
message("  TABLEAU COMPARATIF TOUS MATCHINGS")
message(strrep("=", 65))

# Récupérer uniquement les spécifications TWFE (3) de chaque matching
modeles_twfe <- list()
if (!is.null(modeles_pl_full))   modeles_twfe[["PL Full – TWFE"]]   <- modeles_pl_full[["(3) TWFE IRIS+Commune"]]
if (!is.null(modeles_pl_3yrs))   modeles_twfe[["PL 3ans – TWFE"]]   <- modeles_pl_3yrs[["(3) TWFE IRIS+Commune"]]
if (!is.null(modeles_paris_full)) modeles_twfe[["Paris – TWFE"]]    <- modeles_paris_full[["(3) TWFE IRIS+Commune"]]

if (length(modeles_twfe) > 0) {

  # Table LaTeX comparative
  latex_comp <- modelsummary(
    modeles_twfe,
    coef_rename  = COEF_LABELS,
    coef_omit    = "Intercept",
    statistic    = "({std.error})",
    stars        = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
    gof_map      = c("nobs", "r.squared", "adj.r.squared",
                     "rmse", "FE: IRIS", "FE: commune"),
    title        = "Comparaison des estimateurs TWFE par type de matching",
    notes        = list(
      "Erreurs-types clusterisées au niveau commune entre parenthèses.",
      "* p<0,10 ; ** p<0,05 ; *** p<0,01",
      glue("Variable dépendante : {VAR_DEP}. Période : {ANNEE_PRE}–{ANNEE_POST}.")
    ),
    output = "latex_tabular"
  )

  comp_latex_path <- file.path(DIR_REG, "table_comparative_TWFE.tex")
  writeLines(paste(as.character(latex_table), collapse = "\n"), path)
  message(glue("  ✓ Table comparative LaTeX : {comp_latex_path}"))

  html_comp <- modelsummary(
    modeles_twfe,
    coef_rename  = COEF_LABELS,
    coef_omit    = "Intercept",
    statistic    = "({std.error})",
    stars        = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
    gof_map      = c("nobs", "r.squared", "adj.r.squared",
                     "rmse", "FE: IRIS", "FE: commune"),
    title        = "Comparaison des estimateurs TWFE par type de matching",
    output       = "html"
  )

  comp_html_path <- file.path(DIR_REG, "table_comparative_TWFE.html")
  writeLines(html_comp, comp_html_path)
  message(glue("  ✓ Table comparative HTML  : {comp_html_path}"))
}

# -----------------------------------------------------------------------------
# 8. COEFFICIENT PLOT GLOBAL
# -----------------------------------------------------------------------------

message("\n", strrep("=", 65))
message("  COEFFICIENT PLOT GLOBAL")
message(strrep("=", 65))

if (length(all_coefs) > 0) {
  all_coefs_df <- bind_rows(all_coefs)
  plot_coef(all_coefs_df, DIR_REG)
} else {
  message("  ⚠ Aucun résultat agrégé – coefficient plot ignoré.")
}

# -----------------------------------------------------------------------------
# 9. FIN
# -----------------------------------------------------------------------------

message("\n", strrep("=", 65))
message("  ✅ Toutes les régressions terminées.")
message(glue("  → Sorties dans : {DIR_REG}"))
message(strrep("=", 65))