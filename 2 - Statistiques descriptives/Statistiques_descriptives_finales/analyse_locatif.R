# =============================================================================
# ANALYSE STATISTIQUE - DONNÉES IRIS / LOGEMENT
# Thème : Effet de l'encadrement des loyers sur l'offre locative (Paris & Lille)
# =============================================================================

# ── 0. PACKAGES ──────────────────────────────────────────────────────────────
packages <- c(
  "tidyverse", "skimr", "ggplot2", "GGally", "corrplot",
  "factoextra", "FactoMineR", "knitr", "cluster",
  "scales", "patchwork", "ggridges", "ggrepel",
  "car", "lmtest", "sandwich", "broom", "moments",
  "dunn.test", "rstatix", "gt", "gtsummary", "data.table"
)
installed <- rownames(installed.packages())
to_install <- setdiff(packages, installed)
if (length(to_install)) install.packages(to_install, repos = "https://cloud.r-project.org")
lapply(packages, library, character.only = TRUE)

# ── 1. CHARGEMENT DES DONNÉES ─────────────────────────────────────────────────
setwd("~/0 ENSAE/3A/S2/Projet_socio_eco/encadrement_loyer/2 - Statistiques descriptives/Statistiques_descriptives_finales")
df <- fread("../../base_2006_2022.csv", sep = ",", dec = ".")

df <- df[, lapply(.SD, sum, na.rm = TRUE), 
         by = .(COM, annee),
         .SDcols = setdiff(names(df)[sapply(df, is.numeric)], "annee")]

cluster_coms = fread("../../clusters_ville.csv")

df = merge(df, cluster_coms)

# ── 2. VARIABLES DÉRIVÉES ─────────────────────────────────────────────────────
df <- df %>%
  mutate(
    # Taux de location = part des RP en location
    taux_location    = nb_RP_en_loc / nb_RP,
    # Taux de propriété
    taux_proprio     = nb_RP_proprio / nb_RP,
    # Taux de vacance
    taux_vacance     = nb_logements_vacants / nb_logements,
    # Taux de résidences secondaires
    taux_res_second  = nb_residences_second_ou_occ / nb_logements,
    # Taille moyenne des ménages
    taille_menage    = nb_personnes_en_RP / nb_menages,
    # Taux de chômage
    taux_chomage = nb_chomeurs / (nb_actifs_occ + nb_chomeurs),
    # Part des cadres parmi les actifs occupés
    part_cadres      = nb_cadres / nb_actifs_occ,
    # Part des ouvriers parmi les actifs occupés
    part_ouvriers    = nb_ouvriers / nb_actifs_occ,
    # Densité de logements (logements / ménages)
    ratio_log_men    = nb_logements / nb_menages,
    # Ville (code commune, 4 premiers chiffres de l'IRIS ou COM)
    ville = case_when(
      str_starts(as.character(COM), "75") ~ "Paris",
      str_starts(as.character(COM), "59350") ~ "Lille",
      TRUE ~ paste0("COM_", COM)
    )
  )

df[, taux_chomage := fifelse(taux_chomage == Inf | is.na(taux_chomage), 0, taux_chomage)]
df[, part_cadres := fifelse(is.na(part_cadres), 0, part_cadres)]
df[, part_ouvriers := fifelse(is.na(part_ouvriers), 0, part_ouvriers)]
df[, ratio_log_men := fifelse(nb_menages == 0, 0, ratio_log_men)]


# ── 3. STATISTIQUES DESCRIPTIVES UNIVARIÉES ───────────────────────────────────
cat("\n========== APERÇU GÉNÉRAL ==========\n")
glimpse(df)

cat("\n========== RÉSUMÉ COMPLET (skim) ==========\n")
print(skim(df))

# Variables numériques
vars_num <- df %>% select(where(is.numeric)) %>% names()

# Tableau des stats descriptives détaillées
stats_desc <- df %>%
  select(all_of(vars_num)) %>%
  summarise(across(everything(), list(
    n       = ~sum(!is.na(.)),
    mean    = ~mean(., na.rm = TRUE),
    sd      = ~sd(., na.rm = TRUE),
    min     = ~min(., na.rm = TRUE),
    Q1      = ~quantile(., 0.25, na.rm = TRUE),
    median  = ~median(., na.rm = TRUE),
    Q3      = ~quantile(., 0.75, na.rm = TRUE),
    max     = ~max(., na.rm = TRUE)
  ), .names = "{.col}__{.fn}")) %>%
  pivot_longer(everything(), names_to = "variable__stat", values_to = "value") %>%
  separate(variable__stat, into = c("variable", "stat"), sep = "__") %>%
  pivot_wider(names_from = stat, values_from = value)

cat("\n========== STATISTIQUES DESCRIPTIVES DÉTAILLÉES ==========\n")
print(stats_desc)

fwrite(stats_desc, "statistiques_descriptives_variables_FR.csv",
       sep = ";", dec = ",")

# ── 4. DISTRIBUTIONS – HISTOGRAMMES ET DENSITÉS ──────────────────────────────
vars_plot <- c("nb_RP_en_loc","nb_RP_proprio","taux_location","taux_vacance",
               "nb_logements","nb_menages","taille_menage","taux_chomage",
               "part_cadres","part_ouvriers","nb_logements_vacants")

# Histogrammes
df_long <- df %>%
  select(all_of(intersect(vars_plot, names(df)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "valeur")

p_hist <- ggplot(df_long, aes(x = valeur)) +
  geom_histogram(bins = 30, fill = "#2C6E91", color = "white", alpha = 0.85) +
  geom_density(aes(y = after_stat(count)), color = "#E07B39", linewidth = 0.8) +
  facet_wrap(~variable, scales = "free", ncol = 3) +
  labs(title = "Distributions des variables principales",
       subtitle = "Histogrammes + courbe de densité",
       x = NULL, y = "Effectif") +
  theme_minimal(base_size = 11) +
  theme(strip.text = element_text(face = "bold", size = 9))

print(p_hist)
ggsave("distributions_univariees.png", p_hist, width = 14, height = 10, dpi = 150)

# Boxplots généraux
p_box <- ggplot(df_long, aes(y = valeur, x = "")) +
  geom_boxplot(fill = "#2C6E91", outlier.colour = "#E07B39", outlier.size = 2) +
  facet_wrap(~variable, scales = "free_y", ncol = 3) +
  labs(title = "Boxplots des variables principales", x = NULL, y = NULL) +
  theme_minimal(base_size = 11)

print(p_box)
ggsave("boxplots_univaries.png", p_box, width = 14, height = 10, dpi = 150)



# ── 5. ANALYSE DU cluster_com ────────────────────────────────────────────────────
cat("\n========== DISTRIBUTION DES cluster_comS ==========\n")
print(table(df$cluster_com))
cat("\nProportion par cluster_com :\n")
print(prop.table(table(df$cluster_com)) * 100)

# Stats descriptives par cluster_com
stats_cluster_com <- df %>%
  group_by(cluster_com) %>%
  summarise(across(
    c(nb_RP_en_loc, nb_RP_proprio, taux_location, taux_vacance,
      nb_logements, nb_menages, taille_menage, taux_chomage,
      part_cadres, part_ouvriers, nb_logements_vacants),
    list(mean = mean, sd = sd, median = median),
    na.rm = TRUE
  ))

cat("\n========== STATISTIQUES PAR cluster_com ==========\n")
print(stats_cluster_com)

# Boxplots par cluster_com – variables clés
vars_cluster_com <- c("nb_RP_en_loc","taux_location","taux_vacance",
                  "taille_menage","taux_chomage","part_cadres")

df_clust_long <- df %>%
  select(cluster_com, all_of(intersect(vars_cluster_com, names(df)))) %>%
  pivot_longer(-cluster_com, names_to = "variable", values_to = "valeur") %>%
  mutate(cluster_com = as.factor(cluster_com))

p_cluster_com_box <- ggplot(df_clust_long, aes(x = cluster_com, y = valeur, fill = cluster_com)) +
  geom_boxplot(alpha = 0.8, outlier.size = 1.5) +
  scale_fill_brewer(palette = "Set1") +
  facet_wrap(~variable, scales = "free_y", ncol = 3) +
  labs(title = "Distribution des variables par cluster_com",
       x = "cluster_com", y = NULL, fill = "cluster_com") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

print(p_cluster_com_box)
ggsave("boxplots_par_cluster_com.png", p_cluster_com_box, width = 14, height = 8, dpi = 150)




# ── 6. STATISTIQUES MULTIVARIÉES GLOBALES ────────────────────────────────────
# Matrice de corrélation
vars_corr <- df %>%
  select(nb_RP_en_loc, nb_RP_proprio, taux_location, taux_vacance,
         nb_logements, taille_menage, taux_chomage,
         part_cadres, part_ouvriers, nb_etudiants) %>%
  select(where(~sum(!is.na(.)) > 1))

mat_corr <- cor(vars_corr, use = "pairwise.complete.obs")

cat("\n========== MATRICE DE CORRÉLATION ==========\n")
print(round(mat_corr, 3))

png("matrice_correlation.png", width = 1200, height = 1000, res = 150)
corrplot(mat_corr,
         method  = "color",
         type    = "upper",
         order   = "hclust",
         addCoef.col = "black",
         number.cex  = 0.7,
         tl.col  = "black",
         tl.siz  = 0.8,
         col     = colorRampPalette(c("#E07B39","white","#2C6E91"))(200),
         title   = "Matrice de corrélation – variables logement",
         mar     = c(0,0,2,0))
dev.off()

df[, cluster_com := as.factor(cluster_com)]

# Pairs plot (GGally)
# Rajouter les cluster_coms
p_pairs <- GGally::ggpairs(
  df %>% select(nb_RP_en_loc, taux_location, taux_vacance,
                taille_menage, taux_chomage, part_cadres, cluster_com),
  aes(color = cluster_com, alpha = 0.7),
  upper = list(continuous = wrap("cor", size = 3)),
  lower = list(continuous = wrap("points", size = 1.5)),
  diag  = list(continuous = wrap("densityDiag", alpha = 0.6))
) +
  labs(title = "Pairs plot coloré par cluster_com") +
  theme_minimal(base_size = 9)

print(p_pairs)
ggsave("pairs_plot_cluster_com.png", p_pairs, width = 14, height = 12, dpi = 150)


# ── 8. FOCUS PARIS & LILLE ───────────────────────────────────────────────────
# Filtrer Paris (code INSEE commençant par 75) et Lille (59350)
df_pl <- df %>%
  filter(
    str_starts(as.character(COM), "75") |     # Paris
    str_starts(as.character(COM), "59350")    # Lille
  ) %>%
  mutate(ville = ifelse(str_starts(as.character(COM), "75"), "Paris", "Lille"))


# Boxplots comparatifs Paris vs Lille
vars_pl <- c("nb_RP_en_loc","taux_location","taux_vacance",
             "taille_menage","taux_chomage","part_cadres")

df_pl_long <- df_pl %>%
  select(ville, all_of(intersect(vars_pl, names(df_pl)))) %>%
  pivot_longer(-ville, names_to = "variable", values_to = "valeur")

p_pl_box <- ggplot(df_pl_long, aes(x = ville, y = valeur, fill = ville)) +
  geom_boxplot(alpha = 0.8, outlier.size = 1.5) +
  scale_fill_manual(values = c("Paris" = "#2C6E91", "Lille" = "#E07B39")) +
  facet_wrap(~variable, scales = "free_y", ncol = 3) +
  labs(title = "Paris vs Lille – Variables logement",
       x = NULL, y = NULL, fill = "Ville") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

print(p_pl_box)
ggsave("paris_lille_boxplots.png", p_pl_box, width = 14, height = 8, dpi = 150)



# cluster_coms par ville
if ("cluster_com" %in% names(df_pl)) {
  cat("\n========== DISTRIBUTION DES cluster_comS PAR VILLE ==========\n")
  print(table(df_pl$ville, df_pl$cluster_com))
  cat("\nProportion :\n")
  print(prop.table(table(df_pl$ville, df_pl$cluster_com), margin = 1) * 100)

  p_cluster_com_ville <- ggplot(df_pl, aes(x = cluster_com, fill = ville)) +
    geom_bar(position = "dodge", alpha = 0.9) +
    scale_fill_manual(values = c("Paris" = "#2C6E91", "Lille" = "#E07B39")) +
    labs(title = "Répartition des cluster_coms – Paris et Lille",
         x = "cluster_com", y = "Effectif IRIS", fill = "Ville") +
    theme_minimal(base_size = 12)

  print(p_cluster_com_ville)
  ggsave("cluster_com_par_ville.png", p_cluster_com_ville, width = 8, height = 5, dpi = 150)
}

# ── 9. FOCUS nb_RP_EN_LOC – ANALYSE APPROFONDIE ──────────────────────────────
cat("\n========== FOCUS : nb_RP_en_loc ==========\n")

# Corrélations de nb_RP_en_loc avec les autres variables
corr_loc <- df %>%
  select(where(is.numeric)) %>%
  cor(use = "pairwise.complete.obs") %>%
  as.data.frame() %>%
  rownames_to_column("variable") %>%
  select(variable, nb_RP_en_loc) %>%
  filter(variable != "nb_RP_en_loc") %>%
  arrange(desc(abs(nb_RP_en_loc)))

cat("\n--- Corrélations avec nb_RP_en_loc (triées par valeur absolue) ---\n")
print(corr_loc)

# Scatter plots nb_RP_en_loc vs prédicteurs clés
vars_scatter <- c("nb_logements","nb_menages","taux_vacance",
                  "taille_menage","taux_chomage","part_cadres")

df_scat_long <- df %>%
  select(nb_RP_en_loc, cluster_com, all_of(intersect(vars_scatter, names(df)))) %>%
  pivot_longer(-c(nb_RP_en_loc, cluster_com), names_to = "variable", values_to = "valeur")

p_scatter <- ggplot(df_scat_long, aes(x = valeur, y = nb_RP_en_loc, color = cluster_com)) +
  geom_point(alpha = 0.6, size = 1.8) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 0.8,
              color = "#333333", fill = "grey80") +
  scale_color_brewer(palette = "Set1") +
  facet_wrap(~variable, scales = "free_x", ncol = 3) +
  labs(title = "nb_RP_en_loc vs prédicteurs clés",
       x = NULL, y = "nb_RP_en_loc", color = "cluster_com") +
  theme_minimal(base_size = 10)

print(p_scatter)
ggsave("scatter_nb_RP_en_loc.png", p_scatter, width = 14, height = 8, dpi = 150)

# nb_RP_en_loc par cluster_com
p_loc_cluster_com <- ggplot(df, aes(x = cluster_com, y = nb_RP_en_loc, fill = cluster_com)) +
  geom_violin(alpha = 0.6, trim = FALSE) +
  geom_boxplot(width = 0.15, fill = "white", outlier.size = 1.5) +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "nb_RP_en_loc selon le cluster_com",
       x = "cluster_com", y = "Nombre de RP en location") +
  theme_minimal()

print(p_loc_cluster_com)
ggsave("nb_RP_en_loc_par_cluster_com.png", p_loc_cluster_com, width = 8, height = 5, dpi = 150)

# nb_RP_en_loc Paris vs Lille
if (nrow(df_pl) > 0) {
  p_loc_pl <- ggplot(df_pl, aes(x = ville, y = nb_RP_en_loc, fill = ville)) +
    geom_violin(alpha = 0.6, trim = FALSE) +
    geom_boxplot(width = 0.15, fill = "white", outlier.size = 1.5) +
    geom_jitter(width = 0.1, alpha = 0.3, size = 1) +
    scale_fill_manual(values = c("Paris" = "#2C6E91", "Lille" = "#E07B39")) +
    labs(title = "nb_RP_en_loc : Paris vs Lille",
         x = NULL, y = "Nombre de RP en location") +
    theme_minimal()

  print(p_loc_pl)
  ggsave("nb_RP_en_loc_paris_lille.png", p_loc_pl, width = 7, height = 5, dpi = 150)

  # Évolution temporelle (si plusieurs années)
  if (n_distinct(df_pl$annee) > 1) {
    p_evol <- ggplot(df_pl, aes(x = annee, y = nb_RP_en_loc,
                                 color = ville, group = ville)) +
      stat_summary(fun = mean, geom = "line", linewidth = 1.2) +
      stat_summary(fun = mean, geom = "point", size = 2.5) +
      stat_summary(fun.data = mean_se, geom = "ribbon",
                   aes(fill = ville), alpha = 0.2, color = NA) +
      scale_color_manual(values = c("Paris" = "#2C6E91", "Lille" = "#E07B39")) +
      scale_fill_manual(values  = c("Paris" = "#2C6E91", "Lille" = "#E07B39")) +
      labs(title = "Évolution de nb_RP_en_loc (Paris vs Lille)",
           x = "Année", y = "Moyenne nb_RP_en_loc",
           color = "Ville", fill = "Ville") +
      theme_minimal()

    print(p_evol)
    ggsave("evolution_nb_RP_en_loc.png", p_evol, width = 10, height = 5, dpi = 150)
  }

  # nb_RP_en_loc par cluster_com, distinguant Paris et Lille
  if ("cluster_com" %in% names(df_pl)) {
    p_loc_cl_ville <- ggplot(df_pl,
      aes(x = cluster_com, y = nb_RP_en_loc, fill = ville)) +
      geom_boxplot(alpha = 0.8, outlier.size = 1.5,
                   position = position_dodge(0.8)) +
      scale_fill_manual(values = c("Paris" = "#2C6E91", "Lille" = "#E07B39")) +
      labs(title = "nb_RP_en_loc par cluster_com et par ville",
           x = "cluster_com", y = "nb_RP_en_loc", fill = "Ville") +
      theme_minimal()

    print(p_loc_cl_ville)
    ggsave("nb_RP_en_loc_cluster_com_ville.png", p_loc_cl_ville,
           width = 10, height = 6, dpi = 150)
  }
}
