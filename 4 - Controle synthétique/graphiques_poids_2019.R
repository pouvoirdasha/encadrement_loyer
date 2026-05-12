setwd("~/0 ENSAE/3A/S2/Projet_socio_eco/encadrement_loyer/4 - Controle synthétique")

library(data.table)
library(ggplot2)
library(scales)


dt <- fread("Synthetic_control_all_with_placebo/resultats-communes_synthetiques.csv")


# renommer pour clarté (optionnel mais fortement recommandé)
setnames(dt, old = c("unit.names", "w.weights"),
         new = c("ville", "weight"))

# normalisation (sécurité)
dt[, weight := weight / sum(weight), by = ville_synthetise]

# seuil
seuil <- 0.05

# garder les gros poids
dt_big <- dt[weight >= seuil]

# créer "Autre"
dt_other <- dt_big[
  , .(weight = 1 - sum(weight)),
  by = ville_synthetise
]

dt_other[, ville := "Autre"]

# assembler
dt_plot <- rbind(
  dt_big[, .(ville_synthetise, ville, weight)],
  dt_other,
  fill = TRUE
)

correspondance_COM = fread("../correspondance_com.csv",
                           sep = ";", encoding = "UTF-8")


dt_plot = merge(dt_plot, correspondance_COM[, c("Code géographique", "Libellé géographique")],
                by.x = "ville",
                by.y = "Code géographique",
                all.x = TRUE)


# tri
setorder(dt_plot, ville_synthetise, -weight)

# palette
villes_unique <- unique(dt_plot$`Libellé géographique`)

palette_villes <- c("Autre" = "grey70")

couleurs_autres <- hue_pal()(length(villes_unique))
names(couleurs_autres) <- setdiff(villes_unique, "Autre")

palette_villes <- c(palette_villes, couleurs_autres)

dt_plot[, ville_synthetise := as.factor(ville_synthetise)]


# graphique
ggplot(dt_plot, aes(x = ville_synthetise, y = weight, fill = `Libellé géographique`)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = percent) +
  scale_fill_manual(values = palette_villes) +
  theme_minimal() +
  labs(
    x = "Ville traitée",
    y = "Poids",
    fill = "Villes donneuses",
    title = "Poids du contrôle synthétique"
  ) +
  theme(
    axis.text.x = element_text(angle = 90),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
  )
