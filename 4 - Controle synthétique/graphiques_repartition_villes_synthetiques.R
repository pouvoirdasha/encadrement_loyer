library(data.table)
library(ggplot2)

# --- 1. Lecture du fichier (adapter le chemin et le séparateur) ---
dt <- fread("Synthetic_control-CLUSTER_depuis_2006_nb_log/resultats-communes_synthetiques.csv",
            encoding = "UTF-8",
            sep = ";",
            dec = ",")          # décimale en virgule comme dans vos données

# Renommer les colonnes proprement si besoin
setnames(dt, c("unit.names", "w.weights", "unit.numbers", "libelle", "ville_synthetise"))

# S'assurer que ville_synthetise est un character et w.weights un numeric
dt[, ville_synthetise := as.character(ville_synthetise)]
dt[, w.weights := as.numeric(w.weights)]

# --- 2. Calcul de la part "Autre" ---
dt_totaux <- dt[, .(total_poids = sum(w.weights)), by = ville_synthetise]

dt_autre <- dt_totaux[round(total_poids, 6) < 1, .(
  unit.names      = NA_integer_,
  w.weights       = 1 - total_poids,
  unit.numbers    = NA_integer_,
  libelle         = "Autre",
  ville_synthetise = ville_synthetise
)]

dt_final <- rbindlist(list(dt, dt_autre), use.names = TRUE, fill = TRUE)

# --- 3. Palette de couleurs ---
villes_uniques <- sort(unique(dt_final[libelle != "Autre", libelle]))
n_villes       <- length(villes_uniques)

couleurs_villes <- setNames(
  colorRampPalette(c("#E63946", "#457B9D", "#2A9D8F", "#E9C46A", "#F4A261",
                     "#6A4C93", "#1982C4", "#8AC926", "#FF595E", "#FFCA3A",
                     "#6A994E", "#BC4749", "#F77F00", "#023E8A", "#7B2D8B"))(n_villes),
  villes_uniques
)
couleurs_villes["Autre"] <- "#CCCCCC"
couleurs_finales <- c(couleurs_villes[villes_uniques], couleurs_villes["Autre"])

# Ordonner les niveaux : Autre en dernier (en haut de la barre)
dt_final[, libelle := factor(libelle, levels = c(villes_uniques, "Autre"))]

# --- 4. Graphique ---
ggplot(dt_final, aes(x = ville_synthetise, y = w.weights, fill = libelle)) +
  geom_bar(stat = "identity", width = 0.7, color = "white", linewidth = 0.3) +
  scale_fill_manual(values = couleurs_finales) +
  scale_y_continuous(labels = scales::percent_format(),
                     limits = c(0, 1.01), expand = c(0, 0)) +
  labs(
    title    = "Composition synthétique des villes",
    subtitle = "Poids des villes de contrôle dans le contrôle synthétique",
    x        = "Villes synthétiques",
    y        = "Poids",
    fill     = "Ville de contrôle"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x      = element_text(angle = 45, hjust = 1, size = 8),
    legend.position  = "right",
    legend.key.size  = unit(0.5, "cm"),
    panel.grid.major.x = element_blank(),
    plot.title       = element_text(face = "bold", size = 13),
    plot.subtitle    = element_text(color = "grey40", size = 10)
  )

