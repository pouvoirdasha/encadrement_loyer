setwd("~/0 ENSAE/3A/S2/Projet_socio_eco/encadrement_loyer/2 - Statistiques descriptives/Statistiques_base_finale")
library(data.table)
data = fread("../../base_2006_2022_aire_cluster-COM.csv", encoding = "UTF-8")


# ============================================================
# ANALYSE STATISTIQUE DES DONNÉES LOGEMENT
# Univariée + Multivariée — Focus : nb_RP_en_loc
# ============================================================

library(data.table)
library(ggplot2)
library(corrplot)
library(scales)
library(patchwork)  # pour assembler les graphiques



# ============================================================
# 1. STATISTIQUES UNIVARIÉES
# ============================================================

## 1.1 Variables clés à analyser
vars_univar <- c(
  "nb_RP_en_loc", "part_loc", "taux_vacance", "taux_chomage",
  "part_cadres", "densite_RP", "nb_RP_HLM", "aire"
)

## 1.2 Fonction résumé détaillé
resume_var <- function(dt, var) {
  x <- dt[[var]]
  data.table(
    variable    = var,
    n           = length(x),
    min         = min(x, na.rm = TRUE),
    p25         = quantile(x, 0.25, na.rm = TRUE),
    mediane     = median(x, na.rm = TRUE),
    moyenne     = mean(x, na.rm = TRUE),
    p75         = quantile(x, 0.75, na.rm = TRUE),
    max         = max(x, na.rm = TRUE),
    ecart_type  = sd(x, na.rm = TRUE),
    cv          = sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE),  # coeff. variation
    skewness    = moments::skewness(x, na.rm = TRUE),
    na_count    = sum(is.na(x))
  )
}

stats_univar <- rbindlist(lapply(vars_univar, resume_var, dt = data))
print(stats_univar)

## 1.3 Histogrammes — variables de stock (absolues)
vars_stock <- c("nb_RP_en_loc", "nb_RP_HLM", "nb_logements_vacants")

plot_histo_stock <- function(var) {
  ggplot(data, aes(x = .data[[var]])) +
    geom_histogram(bins = 60, fill = "#2166ac", color = "white", linewidth = 0.1) +
    scale_x_log10(labels = label_comma()) +
    labs(title = paste("Distribution (log) :", var),
         x = var, y = "Nombre de communes") +
    theme_minimal(base_size = 11)
}
lapply(vars_stock, plot_histo_stock)

## 1.4 Histogrammes — ratios / taux
vars_ratio <- c("part_loc", "taux_vacance", "taux_chomage", "part_cadres")

plot_histo_ratio <- function(var) {
  ggplot(data, aes(x = .data[[var]])) +
    geom_histogram(bins = 60, fill = "#d6604d", color = "white", linewidth = 0.1) +
    scale_x_continuous(labels = percent_format()) +
    labs(title = paste("Distribution :", var),
         x = var, y = "Nombre de communes") +
    theme_minimal(base_size = 11)
}
lapply(vars_ratio, plot_histo_ratio)

## 1.5 Évolution temporelle moyenne de part_loc par année
data[, .(part_loc_moy = mean(part_loc, na.rm = TRUE),
         nb_RP_loc_moy = mean(nb_RP_en_loc, na.rm = TRUE)),
     by = annee][order(annee)] |>
  ggplot(aes(x = annee, y = part_loc_moy)) +
  geom_line(color = "#2166ac", linewidth = 1.2) +
  geom_point(size = 2.5) +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "Évolution de la part des locataires (moyenne communale)",
       x = "Année", y = "Part loc. moyenne") +
  theme_minimal()

## 1.6 Distribution par cluster
ggplot(data, aes(x = factor(cluster), y = part_loc, fill = factor(cluster))) +
  geom_boxplot(alpha = 0.7, outlier.size = 0.4) +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Part des locataires par cluster",
       x = "Cluster", y = "Part locataires", fill = "Cluster") +
  theme_minimal()


# ============================================================
# 2. ANALYSE BIVARIÉE — focus nb_RP_en_loc / part_loc
# ============================================================

## 2.1 Corrélations avec nb_RP_en_loc et part_loc
vars_cor <- c(
  "nb_RP_en_loc", "part_loc", "taux_vacance", "taux_chomage",
  "part_cadres", "densite_RP", "nb_RP_HLM", "aire",
  "log_RP_loc", "log_menages", "nb_etudiants", "nb_chomeurs"
)

mat_cor <- cor(data[, ..vars_cor], use = "pairwise.complete.obs")

corrplot(mat_cor,
         method = "color",
         type   = "upper",
         addCoef.col = "black",
         number.cex  = 0.65,
         tl.cex = 0.75,
         col = colorRampPalette(c("#d6604d", "white", "#2166ac"))(200),
         title = "Matrice de corrélations — variables logement",
         mar = c(0, 0, 2, 0))

## 2.2 Nuages de points clés avec part_loc en Y
scatter_vs_loc <- function(x_var, log_x = FALSE) {
  p <- ggplot(data[sample(.N, min(5000, .N))],
              aes(x = .data[[x_var]], y = part_loc)) +
    geom_point(alpha = 0.15, size = 0.8, color = "#2166ac") +
    geom_smooth(method = "loess", se = TRUE, color = "#d6604d", linewidth = 1) +
    scale_y_continuous(labels = percent_format()) +
    labs(title = paste("part_loc ~", x_var), x = x_var, y = "Part locataires") +
    theme_minimal(base_size = 10)
  if (log_x) p <- p + scale_x_log10()
  p
}

(scatter_vs_loc("densite_RP") | scatter_vs_loc("taux_chomage")) /
  (scatter_vs_loc("part_cadres") | scatter_vs_loc("taux_vacance"))

# Relation nb_RP_HLM vs nb_RP_en_loc
ggplot(data[sample(.N, min(5000, .N))],
       aes(x = nb_RP_HLM, y = nb_RP_en_loc)) +
  geom_point(alpha = 0.15, size = 0.8, color = "#4d9221") +
  geom_smooth(method = "lm", color = "#d6604d", linewidth = 1) +
  scale_x_log10(labels = label_comma()) +
  scale_y_log10(labels = label_comma()) +
  labs(title = "Logements HLM vs Résidences principales en location (log-log)",
       x = "nb_RP_HLM (log)", y = "nb_RP_en_loc (log)") +
  theme_minimal()

## 2.3 part_loc par décile de taille (log_menages)
data[, decile_taille := cut(log_menages, breaks = quantile(log_menages, probs = seq(0, 1, 0.1)),
                            labels = paste0("D", 1:10), include.lowest = TRUE)]

data[, .(part_loc_moy = mean(part_loc, na.rm = TRUE),
         part_loc_med = median(part_loc, na.rm = TRUE)),
     by = decile_taille][order(decile_taille)] |>
  ggplot(aes(x = decile_taille, y = part_loc_moy, group = 1)) +
  geom_col(fill = "#2166ac", alpha = 0.8) +
  geom_line(aes(y = part_loc_med), color = "#d6604d", linewidth = 1.2) +
  geom_point(aes(y = part_loc_med), color = "#d6604d", size = 2.5) +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "Part locataires selon la taille de la commune (déciles)",
       subtitle = "Barres = moyenne | Ligne rouge = médiane",
       x = "Décile de taille (nb ménages, log)", y = "Part locataires") +
  theme_minimal()


# ============================================================
# 3. ANALYSE MULTIVARIÉE
# ============================================================

## 3.1 Régression linéaire : part_loc ~ variables structurelles
# Utilisation du log pour les variables de stock
data[, log_HLM    := log(nb_RP_HLM + 1)]
data[, log_etud   := log(nb_etudiants + 1)]
data[, log_aire   := log(aire + 0.01)]

modele_lm <- lm(
  part_loc ~ densite_RP + taux_chomage + part_cadres + taux_vacance +
    log_HLM + log_etud + log_aire + factor(cluster) + factor(annee),
  data = data
)

summary(modele_lm)

## 3.2 Coefficients standardisés (pour comparer les effets)
# install.packages("lm.beta") si nécessaire
# library(lm.beta)
# lm.beta(modele_lm)

## 3.3 Résidus : communes sous/sur-représentées en location
data[, residus_loc := residuals(modele_lm)]

# Communes avec résidus extrêmes (dernière année disponible)
data[annee == max(annee)][order(-residus_loc)][1:20, .(COM, part_loc, residus_loc)]
data[annee == max(annee)][order(residus_loc)][1:20, .(COM, part_loc, residus_loc)]

## 3.4 ACP sur les ratios/taux
vars_acp <- c("part_loc", "taux_vacance", "taux_chomage", "part_cadres",
              "densite_RP", "taux_proprio" )
# Créer taux_proprio si absent
data[, taux_proprio := nb_RP_proprio / nb_RP]

# Sous-échantillon sans NA, une seule année pour l'ACP
data_acp <- na.omit(data[annee == max(annee), ..vars_acp])
acp <- prcomp(data_acp, scale. = TRUE)
summary(acp)

# Biplot
biplot(acp, scale = 0, cex = 0.5,
       main = "ACP — variables de structure du parc logement")

## 3.5 Décomposition de la variance de part_loc par cluster et année
# Modèle à effets fixes
modele_fe <- lm(part_loc ~ factor(cluster) + factor(annee), data = data)
summary(modele_fe)

# Part de variance expliquée
cat("R² cluster + année :", summary(modele_fe)$r.squared, "\n")

## 3.6 Profil moyen par cluster
profil_cluster <- data[, .(
  part_loc      = mean(part_loc,      na.rm = TRUE),
  taux_chomage  = mean(taux_chomage,  na.rm = TRUE),
  part_cadres   = mean(part_cadres,   na.rm = TRUE),
  densite_RP    = mean(densite_RP,    na.rm = TRUE),
  taux_vacance  = mean(taux_vacance,  na.rm = TRUE),
  nb_RP_HLM_moy= mean(nb_RP_HLM,     na.rm = TRUE),
  aire_moy      = mean(aire,          na.rm = TRUE),
  n             = .N
), by = cluster][order(cluster)]

print(profil_cluster)


# ============================================================
# 4. FOCUS NB_RP_EN_LOC — stock absolu
# ============================================================

## 4.1 Part du HLM dans la location
data[, part_HLM_dans_loc := nb_RP_HLM / nb_RP_en_loc]

data[, .(
  part_HLM_med  = median(part_HLM_dans_loc, na.rm = TRUE),
  part_HLM_moy  = mean(part_HLM_dans_loc,   na.rm = TRUE)
), by = cluster][order(cluster)]

## 4.2 Évolution du stock de locations par cluster
data[, .(nb_RP_loc_moy = mean(nb_RP_en_loc, na.rm = TRUE)),
     by = .(annee, cluster)][order(annee)] |>
  ggplot(aes(x = annee, y = nb_RP_loc_moy,
             color = factor(cluster), group = factor(cluster))) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2) +
  scale_y_continuous(labels = label_comma()) +
  scale_color_brewer(palette = "Set1") +
  labs(title = "Évolution du stock moyen de logements en location par cluster",
       x = "Année", y = "nb_RP_en_loc moyen", color = "Cluster") +
  theme_minimal()

## 4.3 Relation location / vacance (tension du marché)
ggplot(data[sample(.N, min(5000, .N))],
       aes(x = taux_vacance, y = part_loc, color = factor(cluster))) +
  geom_point(alpha = 0.3, size = 0.9) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.2) +
  scale_x_continuous(labels = percent_format()) +
  scale_y_continuous(labels = percent_format()) +
  scale_color_brewer(palette = "Set1") +
  labs(title = "Taux de vacance vs Part des locataires — par cluster",
       x = "Taux de vacance", y = "Part locataires", color = "Cluster") +
  theme_minimal()

## 4.4 Corrélations spécifiques nb_RP_en_loc (stock)
vars_focus_loc <- c(
  "nb_RP_en_loc", "nb_RP_HLM", "nb_RP_proprio",
  "nb_logements_vacants", "nb_etudiants", "nb_chomeurs",
  "nb_menages", "aire", "densite_RP"
)

cor_loc <- cor(data[, ..vars_focus_loc], use = "pairwise.complete.obs")
corrplot(cor_loc,
         method = "circle",
         type   = "upper",
         addCoef.col = "black",
         number.cex  = 0.7,
         col = colorRampPalette(c("#d6604d", "white", "#2166ac"))(200),
         title = "Corrélations — nb_RP_en_loc et variables de stock",
         mar = c(0, 0, 2, 0))


# ============================================================
# 5. FOCUS PARIS
# ============================================================
# Les données sont à la maille arrondissement (75101 à 75120).
# On agrège d'abord en une ligne par année (Paris entier),
# puis on recalcule tous les ratios sur les stocks agrégés.

## 5.0 Extraction et agrégation par année
paris_arr  <- data[substr(COM, 1, 2) == "75"]   # 20 arrondissements × n années
hors_paris <- data[substr(COM, 1, 2) != "75"]   # reste de la France

cat("=== Arrondissements trouvés :", uniqueN(paris_arr$COM), "\n")
cat("=== Années disponibles :", paste(sort(unique(paris_arr$annee)), collapse = ", "), "\n")

# Variables de stock à sommer
vars_stock_agg <- c(
  "nb_menages", "nb_personnes_menage", "nb_logements",
  "nb_RP_1_piece", "nb_RP_2_pieces", "nb_RP_3_pieces",
  "nb_RP_4_pieces", "nb_RP_5_piece_et_plus",
  "nb_RP_en_loc", "nb_RP_proprio", "nb_RP_HLM",
  "nb_personnes_en_RP", "nb_personnes_en_RP_location", "nb_personnes_en_RP_proprio",
  "nb_residences_second_ou_occ", "nb_logements_vacants", "nb_RP",
  "nb_actifs", "nb_actifs_occ", "nb_chomeurs",
  "nb_agriculteurs", "nb_commercants", "nb_cadres",
  "nb_professions_inter", "nb_employes", "nb_ouvriers", "nb_etudiants",
  "aire"
)

paris <- paris_arr[, lapply(.SD, sum, na.rm = TRUE), by = annee, .SDcols = vars_stock_agg]

# Recalcul des ratios sur les stocks agrégés
paris[, part_loc     := nb_RP_en_loc      / nb_RP]
paris[, taux_vacance := nb_logements_vacants / nb_logements]
paris[, taux_chomage := nb_chomeurs        / nb_actifs]
paris[, part_cadres  := nb_cadres          / nb_actifs_occ]
paris[, densite_RP   := log(nb_RP / aire)]   # cohérent avec la variable d'origine
paris[, part_HLM_loc := nb_RP_HLM          / nb_RP_en_loc]
paris[, part_prive_loc := 1 - part_HLM_loc]

cat("=== Vérification agrégation (nb lignes Paris) :", nrow(paris), "— doit = nb années\n")
print(paris[, .(annee, nb_RP, nb_RP_en_loc, part_loc, taux_vacance)])

annee_max <- max(data$annee)
paris_last <- paris[annee == annee_max]


## 5.1 Profil statique de Paris vs médiane nationale (dernière année)
vars_profil_stock <- c(
  "nb_RP_en_loc", "nb_RP_HLM", "nb_RP_proprio",
  "nb_logements_vacants", "nb_etudiants"
)
vars_profil_ratio <- c(
  "part_loc", "taux_vacance", "taux_chomage", "part_cadres"
)

# Stocks : comparaison directe (Paris agrégé vs somme nationale)
profil_stock <- data.table(
  variable       = vars_profil_stock,
  paris          = unlist(paris_last[, ..vars_profil_stock]),
  total_france   = sapply(vars_profil_stock, function(v)
    sum(hors_paris[annee == annee_max][[v]], na.rm = TRUE))
)
profil_stock[, part_paris_france := paris / (paris + total_france)]
cat("\n=== Poids de Paris dans les stocks nationaux ===\n")
print(profil_stock)

# Ratios : comparaison Paris vs distribution communale nationale
profil_ratio <- data.table(
  variable        = vars_profil_ratio,
  paris           = unlist(paris_last[, ..vars_profil_ratio]),
  mediane_france  = sapply(vars_profil_ratio, function(v)
    median(hors_paris[annee == annee_max][[v]], na.rm = TRUE)),
  moyenne_france  = sapply(vars_profil_ratio, function(v)
    mean(hors_paris[annee == annee_max][[v]],   na.rm = TRUE))
)
profil_ratio[, ratio_paris_vs_med := paris / mediane_france]
cat("\n=== Ratios Paris vs médiane nationale ===\n")
print(profil_ratio)


## 5.2 Évolution temporelle des indicateurs clés à Paris
vars_ts <- c("part_loc", "taux_vacance", "taux_chomage", "part_cadres")

paris_long <- melt(paris[, c("annee", vars_ts), with = FALSE],
                   id.vars = "annee", variable.name = "indicateur")

ggplot(paris_long, aes(x = annee, y = value, color = indicateur, group = indicateur)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  facet_wrap(~indicateur, scales = "free_y",
             labeller = labeller(indicateur = c(
               part_loc     = "Part locataires",
               taux_vacance = "Taux de vacance",
               taux_chomage = "Taux de chômage",
               part_cadres  = "Part des cadres"
             ))) +
  scale_y_continuous(labels = percent_format()) +
  scale_color_brewer(palette = "Set1") +
  labs(title = "Paris (agrégé) — Évolution des indicateurs clés",
       x = "Année", y = NULL) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "none")


## 5.3 Paris vs grandes villes — benchmark transversal (dernière année)
# Les autres grandes villes sont sur une seule ligne, pas d'agrégation nécessaire.
# Pour Lyon (69xxx) et Marseille (13xxx) on agrège aussi si besoin — ici on
# prend la commune principale pour simplifier.
grandes_villes <- data.table(
  dept  = c("13", "69", "31", "06", "59", "33", "34", "76", "67"),
  ville = c("Marseille", "Lyon", "Toulouse", "Nice",
            "Lille", "Bordeaux", "Montpellier", "Rouen", "Strasbourg")
)

# Agrégation par département pour les grandes villes (commune principale)
# On prend la commune de chaque ville par son code INSEE connu
codes_villes <- c("13055", "69123", "31555", "06088",
                  "59350", "33063", "34172", "76351", "67482")
labels_villes <- c("Marseille", "Lyon", "Toulouse", "Nice",
                   "Lille", "Bordeaux", "Montpellier", "Rouen", "Strasbourg")

benchmark_autres <- data[annee == annee_max & COM %in% codes_villes,
                         .(COM, part_loc, taux_vacance, taux_chomage, part_cadres)]
benchmark_autres[, ville := labels_villes[match(COM, codes_villes)]]

# Ligne Paris agrégée
paris_bench <- data.table(
  COM          = "75_agg",
  part_loc     = paris_last$part_loc,
  taux_vacance = paris_last$taux_vacance,
  taux_chomage = paris_last$taux_chomage,
  part_cadres  = paris_last$part_cadres,
  ville        = "Paris"
)

benchmark <- rbind(benchmark_autres[, .(ville, part_loc, taux_vacance, taux_chomage, part_cadres)],
                   paris_bench[,      .(ville, part_loc, taux_vacance, taux_chomage, part_cadres)])

p_loc <- ggplot(benchmark, aes(x = reorder(ville, part_loc), y = part_loc,
                               fill = ville == "Paris")) +
  geom_col(alpha = 0.85) + coord_flip() +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values = c("TRUE" = "#d6604d", "FALSE" = "#2166ac")) +
  labs(title = "Part des locataires", x = NULL, y = NULL) +
  theme_minimal(base_size = 10) + theme(legend.position = "none")

p_cad <- ggplot(benchmark, aes(x = reorder(ville, part_cadres), y = part_cadres,
                               fill = ville == "Paris")) +
  geom_col(alpha = 0.85) + coord_flip() +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values = c("TRUE" = "#d6604d", "FALSE" = "#4d9221")) +
  labs(title = "Part des cadres", x = NULL, y = NULL) +
  theme_minimal(base_size = 10) + theme(legend.position = "none")

p_vac <- ggplot(benchmark, aes(x = reorder(ville, taux_vacance), y = taux_vacance,
                               fill = ville == "Paris")) +
  geom_col(alpha = 0.85) + coord_flip() +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values = c("TRUE" = "#d6604d", "FALSE" = "#762a83")) +
  labs(title = "Taux de vacance", x = NULL, y = NULL) +
  theme_minimal(base_size = 10) + theme(legend.position = "none")

p_chom <- ggplot(benchmark, aes(x = reorder(ville, taux_chomage), y = taux_chomage,
                                fill = ville == "Paris")) +
  geom_col(alpha = 0.85) + coord_flip() +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values = c("TRUE" = "#d6604d", "FALSE" = "#1b7837")) +
  labs(title = "Taux de chômage", x = NULL, y = NULL) +
  theme_minimal(base_size = 10) + theme(legend.position = "none")

(p_loc | p_cad) / (p_vac | p_chom) +
  plot_annotation(title = paste("Benchmark grandes villes —", annee_max),
                  theme = theme(plot.title = element_text(size = 13, face = "bold")))


## 5.4 Décomposition du parc locatif parisien dans le temps (HLM vs privé)
paris_parc <- melt(
  paris[, .(annee, part_HLM_loc, part_prive_loc)],
  id.vars = "annee", variable.name = "type_loc"
)

ggplot(paris_parc, aes(x = annee, y = value, fill = type_loc)) +
  geom_area(alpha = 0.75) +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(
    values = c("part_HLM_loc"   = "#2166ac",
               "part_prive_loc" = "#d6604d"),
    labels = c("part_HLM_loc"   = "HLM",
               "part_prive_loc" = "Locatif privé")
  ) +
  labs(title = "Paris — Structure du parc locatif agrégé (HLM vs privé)",
       x = "Année", y = "Part dans le parc locatif", fill = NULL) +
  theme_minimal()


## 5.5 Positionnement de Paris dans la distribution nationale (rang percentile)
rang_paris <- sapply(vars_profil_ratio, function(v) {
  val_paris <- paris_last[[v]]
  dist_fr   <- hors_paris[annee == annee_max][[v]]
  round(mean(dist_fr <= val_paris, na.rm = TRUE) * 100, 1)
})

rang_dt <- data.table(variable = vars_profil_ratio, percentile_national = rang_paris)
cat("\n=== Rang percentile de Paris dans la distribution nationale ===\n")
print(rang_dt[order(-percentile_national)])

ggplot(rang_dt, aes(x = reorder(variable, percentile_national),
                    y = percentile_national,
                    fill = percentile_national > 75)) +
  geom_col(alpha = 0.85) +
  geom_hline(yintercept = 50, linetype = "dashed", color = "grey40") +
  coord_flip() +
  scale_fill_manual(values = c("TRUE" = "#d6604d", "FALSE" = "#2166ac"),
                    labels = c("TRUE" = "Au-dessus de la médiane", "FALSE" = "En-dessous")) +
  scale_y_continuous(limits = c(0, 100), labels = function(x) paste0(x, "e")) +
  labs(title = paste("Paris — Rang percentile national (", annee_max, ")"),
       subtitle = "Ratios calculés sur Paris agrégé, comparés à la distribution communale",
       x = NULL, y = "Percentile", fill = NULL) +
  theme_minimal(base_size = 11)


## 5.6 Zoom arrondissements — hétérogénéité interne (dernière année)
arr_last <- paris_arr[annee == annee_max]

# Recalcul des ratios à la maille arrondissement
arr_last[, part_loc_arr     := nb_RP_en_loc       / nb_RP]
arr_last[, taux_vacance_arr := nb_logements_vacants / nb_logements]
arr_last[, part_cadres_arr  := nb_cadres           / nb_actifs_occ]
arr_last[, part_HLM_arr     := nb_RP_HLM           / nb_RP_en_loc]
arr_last[, arr_label        := paste0(substr(COM, 4, 5), "e")]

# Part locataires par arrondissement
ggplot(arr_last, aes(x = reorder(arr_label, part_loc_arr), y = part_loc_arr)) +
  geom_col(fill = "#2166ac", alpha = 0.85) +
  geom_hline(yintercept = paris_last$part_loc, linetype = "dashed",
             color = "#d6604d", linewidth = 1) +
  coord_flip() +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "Part des locataires par arrondissement parisien",
       subtitle = "Ligne rouge = Paris agrégé",
       x = NULL, y = "Part locataires") +
  theme_minimal(base_size = 11)

# Scatter part_loc vs part_cadres par arrondissement
ggplot(arr_last, aes(x = part_cadres_arr, y = part_loc_arr, label = arr_label)) +
  geom_point(aes(size = nb_RP), color = "#2166ac", alpha = 0.7) +
  ggrepel::geom_text_repel(size = 3.2) +
  scale_x_continuous(labels = percent_format()) +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "Arrondissements parisiens : part cadres vs part locataires",
       x = "Part des cadres", y = "Part des locataires",
       size = "Nb résidences\nprincipales") +
  theme_minimal(base_size = 11)


## 5.7 Focus nb_RP_en_loc — stock locatif absolu
# -------------------------------------------------------
# Trois niveaux d'analyse :
#   A. Paris agrégé dans le temps
#   B. Par arrondissement (dernière année + évolution)
#   C. Comparaison avec le reste de la France

### A. Paris agrégé — évolution du stock locatif absolu

# Stock total et décomposition HLM / privé
paris[, nb_RP_loc_prive := nb_RP_en_loc - nb_RP_HLM]

paris_stock_long <- melt(
  paris[, .(annee, nb_RP_HLM, nb_RP_loc_prive)],
  id.vars = "annee", variable.name = "type"
)

# Graphique en aires empilées (stocks absolus)
ggplot(paris_stock_long, aes(x = annee, y = value, fill = type)) +
  geom_area(alpha = 0.8) +
  scale_y_continuous(labels = label_comma()) +
  scale_fill_manual(
    values = c("nb_RP_HLM"       = "#2166ac",
               "nb_RP_loc_prive" = "#d6604d"),
    labels = c("nb_RP_HLM"       = "HLM",
               "nb_RP_loc_prive" = "Locatif privé")
  ) +
  labs(title = "Paris — Stock de logements locatifs (nb_RP_en_loc)",
       subtitle = "Décomposition HLM / privé, Paris entier agrégé",
       x = "Année", y = "Nombre de résidences principales en location",
       fill = NULL) +
  theme_minimal(base_size = 11)

# Taux de croissance annuel du stock locatif parisien
paris_sorted <- paris[order(annee)]
paris_sorted[, croissance_loc := (nb_RP_en_loc / shift(nb_RP_en_loc) - 1)]

ggplot(paris_sorted[!is.na(croissance_loc)],
       aes(x = annee, y = croissance_loc,
           fill = croissance_loc >= 0)) +
  geom_col(alpha = 0.85) +
  geom_hline(yintercept = 0, color = "grey30") +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values = c("TRUE" = "#2166ac", "FALSE" = "#d6604d"),
                    guide = "none") +
  labs(title = "Paris — Taux de croissance du stock locatif entre millésimes",
       x = "Année", y = "Variation nb_RP_en_loc") +
  theme_minimal(base_size = 11)


### B. Par arrondissement — stock locatif absolu

# Recalcul nb_RP_loc_prive à la maille arrondissement
arr_last[, nb_RP_loc_prive_arr := nb_RP_en_loc - nb_RP_HLM]

# Barres empilées HLM / privé par arrondissement (dernière année)
arr_stock_long <- melt(
  arr_last[, .(arr_label, nb_RP_HLM, nb_RP_loc_prive_arr)],
  id.vars = "arr_label", variable.name = "type"
)

ggplot(arr_stock_long,
       aes(x = reorder(arr_label, value, sum),
           y = value, fill = type)) +
  geom_col(alpha = 0.85) +
  geom_hline(yintercept = paris_last$nb_RP_en_loc / 20,
             linetype = "dashed", color = "grey30", linewidth = 0.8) +
  coord_flip() +
  scale_y_continuous(labels = label_comma()) +
  scale_fill_manual(
    values = c("nb_RP_HLM"           = "#2166ac",
               "nb_RP_loc_prive_arr" = "#d6604d"),
    labels = c("nb_RP_HLM"           = "HLM",
               "nb_RP_loc_prive_arr" = "Locatif privé")
  ) +
  labs(title = paste("Stock locatif par arrondissement —", annee_max),
       subtitle = "Ligne pointillée = moyenne arrondissement (Paris / 20)",
       x = NULL, y = "nb_RP_en_loc", fill = NULL) +
  theme_minimal(base_size = 11)

# Scatter : stock locatif absolu vs part locataires par arrondissement
ggplot(arr_last, aes(x = nb_RP_en_loc, y = part_loc_arr, label = arr_label)) +
  geom_point(aes(size = nb_RP_HLM, color = part_HLM_arr), alpha = 0.85) +
  ggrepel::geom_text_repel(size = 3.2) +
  scale_x_continuous(labels = label_comma()) +
  scale_y_continuous(labels = percent_format()) +
  scale_color_gradient(low = "#d6604d", high = "#2166ac",
                       labels = percent_format()) +
  scale_size_continuous(labels = label_comma()) +
  labs(title = "Arrondissements : stock locatif absolu vs part locataires",
       subtitle = "Taille = nb HLM | Couleur = part HLM dans la location",
       x = "nb_RP_en_loc", y = "Part locataires",
       color = "Part HLM", size = "nb_RP_HLM") +
  theme_minimal(base_size = 11)

# Évolution du stock locatif par arrondissement dans le temps
paris_arr[, arr_label := paste0(substr(COM, 4, 5), "e")]

ggplot(paris_arr, aes(x = annee, y = nb_RP_en_loc,
                      color = arr_label, group = arr_label)) +
  geom_line(linewidth = 0.8, alpha = 0.7) +
  geom_point(size = 1.5, alpha = 0.7) +
  scale_y_continuous(labels = label_comma()) +
  scale_color_viridis_d(option = "turbo") +
  labs(title = "Évolution du stock locatif par arrondissement",
       x = "Année", y = "nb_RP_en_loc", color = "Arrondissement") +
  theme_minimal(base_size = 11) +
  guides(color = guide_legend(ncol = 2, override.aes = list(linewidth = 2)))


### C. Comparaison Paris vs reste de la France

# C.1 Poids de Paris dans le stock locatif national, par année
stock_national <- data[, .(nb_RP_en_loc_total = sum(nb_RP_en_loc, na.rm = TRUE)),
                       by = annee]
stock_paris    <- paris[, .(annee, nb_RP_en_loc_paris = nb_RP_en_loc)]

poids_paris <- merge(stock_national, stock_paris, by = "annee")
poids_paris[, part_paris := nb_RP_en_loc_paris / nb_RP_en_loc_total]

ggplot(poids_paris, aes(x = annee, y = part_paris)) +
  geom_line(color = "#d6604d", linewidth = 1.3) +
  geom_point(size = 3, color = "#d6604d") +
  geom_text(aes(label = percent(part_paris, accuracy = 0.1)),
            vjust = -0.8, size = 3.2) +
  scale_y_continuous(labels = percent_format(), limits = c(0, NA)) +
  labs(title = "Poids de Paris dans le stock locatif national",
       subtitle = "nb_RP_en_loc Paris / nb_RP_en_loc France entière",
       x = "Année", y = "Part de Paris") +
  theme_minimal(base_size = 11)

# C.2 Distribution nationale de nb_RP_en_loc avec Paris en repère
ggplot(hors_paris[annee == annee_max],
       aes(x = nb_RP_en_loc)) +
  geom_histogram(bins = 80, fill = "#2166ac", color = "white",
                 linewidth = 0.1, alpha = 0.8) +
  geom_vline(xintercept = paris_last$nb_RP_en_loc, color = "#d6604d",
             linewidth = 1.4, linetype = "dashed") +
  annotate("text",
           x     = paris_last$nb_RP_en_loc * 1.02,
           y     = Inf, vjust = 1.5, hjust = 0,
           label = paste0("Paris agrégé\n",
                          label_comma()(round(paris_last$nb_RP_en_loc))),
           color = "#d6604d", size = 3.5) +
  scale_x_log10(labels = label_comma()) +
  labs(title = paste("Distribution nationale de nb_RP_en_loc —", annee_max),
       subtitle = "Échelle log | Ligne rouge = Paris agrégé",
       x = "nb_RP_en_loc (log)", y = "Nombre de communes") +
  theme_minimal(base_size = 11)

# C.3 Évolution comparée : Paris vs moyenne France hors Paris
evol_france <- hors_paris[, .(nb_RP_en_loc_moy_fr = mean(nb_RP_en_loc, na.rm = TRUE),
                              nb_RP_en_loc_med_fr = median(nb_RP_en_loc, na.rm = TRUE)),
                          by = annee]

evol_comp <- merge(
  paris[, .(annee, nb_RP_en_loc_paris = nb_RP_en_loc)],
  evol_france, by = "annee"
)

# Indexation base 100 sur la première année
base_yr <- min(evol_comp$annee)
evol_comp[, idx_paris  := nb_RP_en_loc_paris   / nb_RP_en_loc_paris[annee == base_yr]   * 100]
evol_comp[, idx_moy_fr := nb_RP_en_loc_moy_fr  / nb_RP_en_loc_moy_fr[annee == base_yr]  * 100]
evol_comp[, idx_med_fr := nb_RP_en_loc_med_fr  / nb_RP_en_loc_med_fr[annee == base_yr]  * 100]

evol_long <- melt(
  evol_comp[, .(annee, idx_paris, idx_moy_fr, idx_med_fr)],
  id.vars = "annee", variable.name = "serie"
)

ggplot(evol_long, aes(x = annee, y = value, color = serie, group = serie)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  geom_hline(yintercept = 100, linetype = "dashed", color = "grey50") +
  scale_color_manual(
    values = c("idx_paris"  = "#d6604d",
               "idx_moy_fr" = "#2166ac",
               "idx_med_fr" = "#4d9221"),
    labels = c("idx_paris"  = "Paris (agrégé)",
               "idx_moy_fr" = "Moyenne France hors Paris",
               "idx_med_fr" = "Médiane France hors Paris")
  ) +
  labs(title = "Évolution du stock locatif — Paris vs France hors Paris",
       subtitle = paste("Indice base 100 =", base_yr),
       x = "Année", y = paste0("Indice (base 100 = ", base_yr, ")"),
       color = NULL) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")
