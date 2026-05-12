# L'idée de ce fichier est de générer les plots pour toutes les communes matchées  pour voir visuellement 
# Si on a bien les tendances parallèles communes qui tiennent. 
# Tous les fichiers seront exportés dans le dossier 'visualisation' dans 8 - Matching. 

library(tidyverse)
library(ggplot2)
library(scales)
library(glue)

PATH_PAIRES_PARIS_LILLE_FULL  <- "data/matching_paires_2006-17_Paris_Lille.csv"
PATH_PAIRES_PARIS_LILLE_3YRS  <- "data/matching_paires_2007_12_17_Paris_Lille.csv"
PATH_PAIRES_PARIS_FULL        <- "data/matching_paires_Paris_Trajectoire_Full.csv"
PATH_BDD                      <- "data/bdd_finale_mathching.csv"
 
DIR_VIZ_BASE <- "8-Matching/visualisation"
 
# Sous-dossiers pour chaque type de matching
DIRS <- list(
  paris_lille_full = file.path(DIR_VIZ_BASE, "Paris_Lille_2006_2017"),
  paris_lille_3yrs = file.path(DIR_VIZ_BASE, "Paris_Lille_2007_2012_2017"),
  paris_full       = file.path(DIR_VIZ_BASE, "Paris_2006_2017")
)
 
# Variables à visualiser (matching + outcome)
vars_matching <- c(
  "taux_vacance", "taux_chomage", "densite_pop",
  "part_cadres", "part_ouvriers", "part_etudiants",
  "part_hlm", "part_1p", "part_2p", "part_3p",
  "log_logements"
)
vars_target   <- "nb_RP_en_loc"
vars_all      <- c(vars_matching, vars_target)
 
# Années à visualiser (avant traitement uniquement)
ANNEES_PRE_TRAIT <- 2006:2017
 
# Seuil population pour filtrage commune
SEUIL_POP <- 10000
 
# Palette couleurs : Traités = orange, Contrôles = bleu
PALETTE <- c("Traité"   = "#E07B39",
             "Contrôle" = "#4A90D9")
 
# Labels lisibles pour chaque variable
VAR_LABELS <- c(
  taux_vacance   = "Taux de vacance (%)",
  taux_chomage   = "Taux de chômage (%)",
  densite_pop    = "Densité de population (hab/km²)",
  part_cadres    = "Part des cadres (%)",
  part_ouvriers  = "Part des ouvriers (%)",
  part_etudiants = "Part des étudiants (%)",
  part_hlm       = "Part des logements HLM (%)",
  part_1p        = "Part des logements 1 pièce (%)",
  part_2p        = "Part des logements 2 pièces (%)",
  part_3p        = "Part des logements 3 pièces (%)",
  log_logements  = "Log(nombre de logements)",
  nb_RP_en_loc   = "Nombre de résidences principales en location"
)
 
# -----------------------------------------------------------------------------
# 2. FONCTIONS UTILITAIRES
# -----------------------------------------------------------------------------
 
#' Créer les dossiers de sortie s'ils n'existent pas
create_output_dirs <- function(dirs) {
  walk(dirs, ~ dir.create(.x, recursive = TRUE, showWarnings = FALSE))
  message("✓ Dossiers de sortie créés/vérifiés.")
}
 
#' Charger et nettoyer les paires de matching
#' @param path Chemin vers le CSV des paires (colonnes : IRIS_trait, IRIS_ctrl)
load_paires <- function(path) {
  read_csv(path, show_col_types = FALSE) %>%
    select(IRIS_trait, IRIS_ctrl) %>%
    distinct()
}
 
#' Préparer le panel long pour un jeu de paires donné
#'
#' Correction matching multi-villes :
#'   - Un IRIS controle peut etre matche a plusieurs traites (with replacement)
#'     → on déduplique proprement AVANT de tagger les groupes
#'   - Un IRIS present dans les deux colonnes (chevauchement) est classe Traite
#'     (priorité au groupe de traitement)
#'   - On diagnostique le chevauchement et le taux de reutilisation des controles
#'
#' @param paires    data.frame avec IRIS_trait / IRIS_ctrl
#' @param bdd       data.frame base de données complète
#' @param annees    vecteur d'années à conserver
#' @param seuil_pop seuil de population (filtrage commune)
prepare_panel <- function(paires, bdd, annees = ANNEES_PRE_TRAIT) {
 
  # --- 1. Diagnostics sur les paires ----------------------------------------
  # Forcer les deux colonnes en character pour eviter les faux positifs
  # lors de intersect/setdiff dus a un mismatch de types (ex: dbl vs chr)
  iris_trait_all <- as.character(paires$IRIS_trait)
  iris_ctrl_all  <- as.character(paires$IRIS_ctrl)
 
  iris_trait <- unique(iris_trait_all)
  iris_ctrl  <- unique(iris_ctrl_all)
 
  # IRIS controles reutilises (matching with replacement)
  freq_ctrl <- table(iris_ctrl_all)
  n_reutilises <- sum(freq_ctrl > 1)
  if (n_reutilises > 0) {
    message(glue("  ℹ {n_reutilises} IRIS controles reutilises (matching with replacement) ",
                 "sur {length(iris_ctrl)} controles uniques"))
    message(glue("  ℹ Reutilisation max : {max(freq_ctrl)} fois pour un meme controle"))
  }
 
  iris_all <- union(iris_trait, iris_ctrl)
 
  message(glue("  → IRIS traites uniques  : {length(iris_trait)}"))
  message(glue("  → IRIS controles uniques (apres nettoyage) : {length(iris_ctrl)}"))
  message(glue("  → IRIS total dans le panel : {length(iris_all)}"))
 
  # --- 2. Construction du panel ---------------------------------------------
  # Meme cast sur la BDD pour que le %in% compare bien chr vs chr
  panel <- bdd %>%
    mutate(IRIS = as.character(IRIS)) %>%
    filter(IRIS %in% iris_all, annee %in% annees) %>%
    mutate(
      # Priorité Traité > Contrôle en cas de chevauchement résiduel
      Groupe = case_when(
        IRIS %in% iris_trait ~ "Traite",
        IRIS %in% iris_ctrl  ~ "Controle",
        TRUE                  ~ NA_character_
      )
    ) %>%
    filter(!is.na(Groupe)) %>%
    # Facteur ordonné pour que la légende soit toujours T avant C
    mutate(Groupe = factor(Groupe, levels = c("Traite", "Controle"),
                           labels = c("Traité", "Contrôle")))
 
  # --- 3. Diagnostic couverture annuelle ------------------------------------
  # Le filtrage population est fait en amont (bdd_finale_matching).
  # On affiche juste la couverture pour détecter les années avec IRIS manquants.
  couv <- panel %>%
    group_by(annee, Groupe) %>%
    summarise(n_iris = n_distinct(IRIS), .groups = "drop") %>%
    pivot_wider(names_from = Groupe, values_from = n_iris, values_fill = 0)
  message("  → Couverture IRIS par annee et groupe :")
  print(as.data.frame(couv))
 
  panel
}
 
#' Agréger par groupe x annee (moyenne +/- SE)
#'
#' Comportement vis-a-vis des IRIS manquants :
#'   - La moyenne est calculee sur les IRIS PRESENTS cette annee-la uniquement
#'     (na.rm = TRUE gere les NA de la variable, pas les IRIS absents)
#'   - La colonne n_iris compte les IRIS distincts contribuant a la moyenne
#'     cette annee, ce qui permet de detecter une composition changeante
#'   - n_obs = nb de lignes (peut etre > n_iris si plusieurs logements/IRIS)
#'
#' @param panel data.frame long avec colonnes Groupe, annee, + vars
#' @param var   nom de la variable a agreger
aggregate_group <- function(panel, var) {
  # Pas de filter(!is.na()) ici : on garde toutes les lignes IRIS x annee.
  # na.rm = TRUE dans mean() et sd() ignore les NA variable par variable
  # sans supprimer l'IRIS pour les autres variables ou les autres annees.
  panel %>%
    group_by(Groupe, annee) %>%
    summarise(
      moyenne = mean(.data[[var]], na.rm = TRUE),
      se      = sd(.data[[var]],  na.rm = TRUE) / sqrt(sum(!is.na(.data[[var]]))),
      n_obs   = sum(!is.na(.data[[var]])),   # N effectif pour cette variable cette annee
      n_iris  = n_distinct(IRIS),            # N IRIS presents cette annee (toutes vars)
      .groups = "drop"
    )
}
 
#' Générer un graphique de tendances parallèles pour une variable
#' @param df_agg  data.frame agrégé (Groupe, annee, moyenne, se)
#' @param var     nom de la variable
#' @param titre   titre du graphique (optionnel, sinon auto)
#' @param annees_trait vecteur d'années de traitement (trait vertical optionnel)
plot_tendances <- function(df_agg, var,
                            titre = NULL,
                            annees_trait = NULL) {
 
  y_label <- VAR_LABELS[var] %||% var
  if (is.null(titre)) titre <- glue("Évolution de '{y_label}'\nTraités vs Contrôles (avant traitement)")
 
  p <- ggplot(df_agg, aes(x = annee, y = moyenne,
                           colour = Groupe, fill = Groupe)) +
    # Ruban IC 95%
    geom_ribbon(aes(ymin = moyenne - 1.96 * se,
                    ymax = moyenne + 1.96 * se),
                alpha = 0.15, colour = NA) +
    # Ligne + points
    geom_line(linewidth = 0.9) +
    geom_point(size = 2.2) +
    # Traits verticaux optionnels pour les années de traitement
    {
      if (!is.null(annees_trait))
        geom_vline(xintercept = annees_trait,
                   linetype = "dashed", colour = "grey40", linewidth = 0.5)
    } +
    scale_colour_manual(values = PALETTE, name = "Groupe") +
    scale_fill_manual(values   = PALETTE, name = "Groupe") +
    scale_x_continuous(breaks = ANNEES_PRE_TRAIT,
                       labels = ANNEES_PRE_TRAIT) +
    scale_y_continuous(labels = label_comma(big.mark = " ",
                                             decimal.mark = ",")) +
    labs(
      title    = titre,
      x        = "Année",
      y        = y_label,
      caption  = {
      nt_rng <- df_agg %>% filter(Groupe == "Traité")   %>% pull(n_iris) %>% range()
      nc_rng <- df_agg %>% filter(Groupe == "Contrôle") %>% pull(n_iris) %>% range()
      glue("IRIS Traités : {nt_rng[1]}–{nt_rng[2]} selon l'année  |  ",
           "IRIS Contrôles : {nc_rng[1]}–{nc_rng[2]} selon l'année  ",
           "(variation = IRIS changeant de contour ou données manquantes)")
    }
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title       = element_text(face = "bold", size = 13, hjust = 0),
      axis.text.x      = element_text(angle = 45, hjust = 1),
      panel.grid.minor = element_blank(),
      legend.position  = "bottom",
      legend.title     = element_text(face = "bold"),
      plot.caption     = element_text(colour = "grey50", size = 9)
    )
 
  p
}
 
#' Générer et exporter tous les graphiques pour un jeu de données
#' @param paires      data.frame des paires IRIS
#' @param bdd         base de données complète
#' @param output_dir  dossier de sortie
#' @param label       label court pour les messages
#' @param annees_trait vecteur d'années de traitement (traits verticaux)
export_all_plots <- function(paires, bdd, output_dir, label,
                              annees_trait = NULL) {
 
 
  message(glue("  Matching : {label}"))
  message(glue("  Dossier  : {output_dir}"))

 
  # Préparer le panel
  panel <- prepare_panel(paires, bdd)
 
  n_trait <- n_distinct(panel$IRIS[panel$Groupe == "Traité"])
  n_ctrl  <- n_distinct(panel$IRIS[panel$Groupe == "Contrôle"])
  message(glue("  IRIS traités : {n_trait}  |  IRIS contrôles : {n_ctrl}"))
 
  # Boucle sur toutes les variables
  walk(vars_all, function(var) {
 
    if (!var %in% names(panel)) {
      message(glue("  ⚠ Variable '{var}' absente de la BDD – ignorée."))
      return(invisible(NULL))
    }
 
    df_agg <- aggregate_group(panel, var)
 
    if (nrow(df_agg) == 0) {
      message(glue("  ⚠ Aucune donnée pour '{var}' – graphique ignoré."))
      return(invisible(NULL))
    }
 
    p <- plot_tendances(df_agg, var, annees_trait = annees_trait)
 
    # Nom de fichier sécurisé
    safe_var  <- gsub("[^a-zA-Z0-9_]", "_", var)
    file_name <- file.path(output_dir, glue("{safe_var}.png"))
 
    ggsave(file_name, plot = p,
           width = 8, height = 5, dpi = 150, bg = "white")
 
    message(glue("  ✓ Exporté : {basename(file_name)}"))
  })
 
  message(glue("  → Tous les graphiques exportés dans {output_dir}\n"))
}

# -----------------------------------------------------------------------------
# 4. CHARGEMENT DES PAIRES
# -----------------------------------------------------------------------------
 
message("\nChargement des fichiers de paires…")
 
# Paris & Lille – full (2006–2017, toutes les années)
paires_pl_full <- tryCatch(
  load_paires(PATH_PAIRES_PARIS_LILLE_FULL),
  error = function(e) {
    message(glue(" Fichier Paris-Lille full non trouvé ({PATH_PAIRES_PARIS_LILLE_FULL})."))
    NULL
  }
)
 
# Paris & Lille – 3 années (2007, 2012, 2017)
paires_pl_3yrs <- tryCatch(
  load_paires(PATH_PAIRES_PARIS_LILLE_3YRS),
  error = function(e) {
    message(glue("Fichier Paris-Lille 3 ans non trouvé ({PATH_PAIRES_PARIS_LILLE_3YRS})."))
    NULL
  }
)
 
# Paris uniquement – full
paires_paris_full <- tryCatch(
  load_paires(PATH_PAIRES_PARIS_FULL),
  error = function(e) {
    message(glue("Fichier Paris full non trouvé ({PATH_PAIRES_PARIS_FULL})."))
    NULL
  }
)


# -----------------------------------------------------------------------------
# 5. CRÉATION DES DOSSIERS DE SORTIE
# -----------------------------------------------------------------------------
 
create_output_dirs(DIRS)
 
# -----------------------------------------------------------------------------
# 6. GÉNÉRATION DES GRAPHIQUES
# -----------------------------------------------------------------------------
 
# ---- 6a. Paris & Lille – full (2006–2017) -----------------------------------
if (!is.null(paires_pl_full)) {
  export_all_plots(
    paires      = paires_pl_full,
    bdd         = bdd,
    output_dir  = DIRS$paris_lille_full,
    label       = "Paris & Lille – toutes années 2006–2017",
    annees_trait = NULL   # pas de rupture particulière dans ce matching
  )
}
 
# ---- 6b. Paris & Lille – 2007, 2012, 2017 -----------------------------------
if (!is.null(paires_pl_3yrs)) {
  export_all_plots(
    paires       = paires_pl_3yrs,
    bdd          = bdd,
    output_dir   = DIRS$paris_lille_3yrs,
    label        = "Paris & Lille – années 2007, 2012, 2017",
    annees_trait = c(2007, 2012, 2017)
  )
}
 
# ---- 6c. Paris uniquement – full --------------------------------------------
if (!is.null(paires_paris_full)) {
  export_all_plots(
    paires      = paires_paris_full,
    bdd         = bdd,
    output_dir  = DIRS$paris_full,
    label       = "Paris uniquement – toutes années 2006–2017",
    annees_trait = NULL
  )
}
 
