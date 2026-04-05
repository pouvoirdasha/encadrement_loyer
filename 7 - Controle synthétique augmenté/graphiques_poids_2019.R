setwd("~/0 ENSAE/3A/S2/Projet_socio_eco/encadrement_loyer/7 - Controle synthétique augmenté/Traitement 2019")

library(data.table)
library(ggplot2)

# liste des fichiers
fichiers <- list.files(
  pattern="Ridge-poids",
  full.names=TRUE
)

# lecture des fichiers
dt <- rbindlist(lapply(fichiers, function(f){
  
  tmp <- fread(f)
  
  # code commune traité (depuis le nom du fichier)
  commune <- sub("-.*", "", basename(f))
  
  tmp[, commune := commune]
  
  tmp
}))

dt[, weight_norm := weight / sum(weight), by=commune]

# seuil 10%
seuil <- 0.05

# 1. agréger les poids par ville donneuse
dt2 <- dt[
  , .(weight = sum(weight_norm)),
  by = .(commune, ville = `Libellé géographique`)
]

# 2. garder seulement les poids >= seuil
dt_big <- dt2[weight >= seuil]

# 3. créer "Autre" = reste pour arriver à 1
dt_other <- dt_big[
  , .(weight = 1 - sum(weight)),
  by = commune
]

dt_other[, ville := "Autre"]

# 4. assembler
dt_plot <- rbind(
  dt_big,
  dt_other,
  fill=TRUE
)

# 5. trier pour affichage propre
setorder(dt_plot, commune, -weight)

# vérification (doit faire 1)
dt_plot[, sum(weight), by=commune]


# récupérer toutes les villes uniques dans l'ordre que tu veux
villes_unique <- unique(dt_plot$ville)

# créer une palette (ici avec des couleurs distinctes pour chaque ville)
palette_villes <- c("Autre" = "grey70")  # Autre en gris

# ajouter les autres couleurs
couleurs_autres <- scales::hue_pal()(length(villes_unique) - 1) 
names(couleurs_autres) <- setdiff(villes_unique, "Autre")

palette_villes <- c(palette_villes, couleurs_autres)


ggplot(dt_plot, aes(x=commune, y=weight, fill=ville)) +
  geom_bar(stat="identity") +
  scale_y_continuous(labels=scales::percent) +
  scale_fill_manual(values = palette_villes) +
  theme_minimal() +
  labs(
    x="Commune traitée",
    y="Poids",
    fill="Communes donneuses",
    title="Poids SCMA-Ridge par commune pour l'année 2019"
  ) +
  theme(axis.text.x = element_text(angle=90)) +
  theme(
    axis.text.x = element_text(angle=90),
    plot.title = element_text(size=16, face="bold", hjust=0.5) # titre centré et plus visible
  )




# liste des fichiers
fichiers <- list.files(
  pattern="SCM-poids",
  full.names=TRUE
)

# lecture des fichiers
dt <- rbindlist(lapply(fichiers, function(f){
  
  tmp <- fread(f)
  
  # code commune traité (depuis le nom du fichier)
  commune <- sub("-.*", "", basename(f))
  
  tmp[, commune := commune]
  
  tmp
}))

dt[, weight_norm := weight / sum(weight), by=commune]

# seuil 5%
seuil <- 0.05

# 1. agréger les poids par ville donneuse
dt2 <- dt[
  , .(weight = sum(weight_norm)),
  by = .(commune, ville = `Libellé géographique`)
]

# 2. garder seulement les poids >= seuil
dt_big <- dt2[weight >= seuil]

# 3. créer "Autre" = reste pour arriver à 1
dt_other <- dt_big[
  , .(weight = 1 - sum(weight)),
  by = commune
]

dt_other[, ville := "Autre"]

# 4. assembler
dt_plot <- rbind(
  dt_big,
  dt_other,
  fill=TRUE
)

# 5. trier pour affichage propre
setorder(dt_plot, commune, -weight)

# vérification (doit faire 1)
dt_plot[, sum(weight), by=commune]


ggplot(dt_plot, aes(x=commune, y=weight, fill=ville)) +
  geom_bar(stat="identity") +
  scale_y_continuous(labels=scales::percent) +
  scale_fill_manual(values = palette_villes) +
  theme_minimal() +
  labs(
    x="Commune traitée",
    y="Poids",
    fill="Communes utilisées",
    title="Poids SCM classique par commune pour l'année 2019"
  )  +
  theme(
    axis.text.x = element_text(angle=90),
    plot.title = element_text(size=16, face="bold", hjust=0.5) # titre centré et plus visible
  )


setwd("~/0 ENSAE/3A/S2/Projet_socio_eco/encadrement_loyer/7 - Controle synthétique augmenté/Traitement 2015")

library(data.table)
library(ggplot2)

# liste des fichiers
fichiers <- list.files(
  pattern="Ridge-poids",
  full.names=TRUE
)

# lecture des fichiers
dt <- rbindlist(lapply(fichiers, function(f){
  
  tmp <- fread(f)
  
  # code commune traité (depuis le nom du fichier)
  commune <- sub("-.*", "", basename(f))
  
  tmp[, commune := commune]
  
  tmp
}))

dt[, weight_norm := weight / sum(weight), by=commune]

# seuil 10%
seuil <- 0.05

# 1. agréger les poids par ville donneuse
dt2 <- dt[
  , .(weight = sum(weight_norm)),
  by = .(commune, ville = `Libellé géographique`)
]

# 2. garder seulement les poids >= seuil
dt_big <- dt2[weight >= seuil]

# 3. créer "Autre" = reste pour arriver à 1
dt_other <- dt_big[
  , .(weight = 1 - sum(weight)),
  by = commune
]

dt_other[, ville := "Autre"]

# 4. assembler
dt_plot <- rbind(
  dt_big,
  dt_other,
  fill=TRUE
)

# 5. trier pour affichage propre
setorder(dt_plot, commune, -weight)

# vérification (doit faire 1)
dt_plot[, sum(weight), by=commune]


# récupérer toutes les villes uniques dans l'ordre que tu veux
villes_unique <- unique(dt_plot$ville)

# créer une palette (ici avec des couleurs distinctes pour chaque ville)
palette_villes <- c("Autre" = "grey70")  # Autre en gris

# ajouter les autres couleurs
couleurs_autres <- scales::hue_pal()(length(villes_unique) - 1) 
names(couleurs_autres) <- setdiff(villes_unique, "Autre")

palette_villes <- c(palette_villes, couleurs_autres)


ggplot(dt_plot, aes(x=commune, y=weight, fill=ville)) +
  geom_bar(stat="identity") +
  scale_y_continuous(labels=scales::percent) +
  scale_fill_manual(values = palette_villes) +
  theme_minimal() +
  labs(
    x="Commune traitée",
    y="Poids",
    fill="Communes donneuses",
    title="Poids SCMA-Ridge par commune pour l'année 2015"
  ) +
  theme(axis.text.x = element_text(angle=90)) +
  theme(
    axis.text.x = element_text(angle=90),
    plot.title = element_text(size=16, face="bold", hjust=0.5) # titre centré et plus visible
  )




# liste des fichiers
fichiers <- list.files(
  pattern="SCM-poids",
  full.names=TRUE
)

# lecture des fichiers
dt <- rbindlist(lapply(fichiers, function(f){
  
  tmp <- fread(f)
  
  # code commune traité (depuis le nom du fichier)
  commune <- sub("-.*", "", basename(f))
  
  tmp[, commune := commune]
  
  tmp
}))

dt[, weight_norm := weight / sum(weight), by=commune]

# seuil 5%
seuil <- 0.05

# 1. agréger les poids par ville donneuse
dt2 <- dt[
  , .(weight = sum(weight_norm)),
  by = .(commune, ville = `Libellé géographique`)
]

# 2. garder seulement les poids >= seuil
dt_big <- dt2[weight >= seuil]

# 3. créer "Autre" = reste pour arriver à 1
dt_other <- dt_big[
  , .(weight = 1 - sum(weight)),
  by = commune
]

dt_other[, ville := "Autre"]

# 4. assembler
dt_plot <- rbind(
  dt_big,
  dt_other,
  fill=TRUE
)

# 5. trier pour affichage propre
setorder(dt_plot, commune, -weight)

# vérification (doit faire 1)
dt_plot[, sum(weight), by=commune]


ggplot(dt_plot, aes(x=commune, y=weight, fill=ville)) +
  geom_bar(stat="identity") +
  scale_y_continuous(labels=scales::percent) +
  scale_fill_manual(values = palette_villes) +
  theme_minimal() +
  labs(
    x="Commune traitée",
    y="Poids",
    fill="Communes utilisées",
    title="Poids SCM classique par commune pour l'année 2015"
  )  +
  theme(
    axis.text.x = element_text(angle=90),
    plot.title = element_text(size=16, face="bold", hjust=0.5) # titre centré et plus visible
  )



setwd("~/0 ENSAE/3A/S2/Projet_socio_eco/encadrement_loyer/7 - Controle synthétique augmenté/Traitement 2017")

library(data.table)
library(ggplot2)

# liste des fichiers
fichiers <- list.files(
  pattern="Ridge-poids",
  full.names=TRUE
)

# lecture des fichiers
dt <- rbindlist(lapply(fichiers, function(f){
  
  tmp <- fread(f)
  
  # code commune traité (depuis le nom du fichier)
  commune <- sub("-.*", "", basename(f))
  
  tmp[, commune := commune]
  
  tmp
}))

dt[, weight_norm := weight / sum(weight), by=commune]

# seuil 10%
seuil <- 0.05

# 1. agréger les poids par ville donneuse
dt2 <- dt[
  , .(weight = sum(weight_norm)),
  by = .(commune, ville = `Libellé géographique`)
]

# 2. garder seulement les poids >= seuil
dt_big <- dt2[weight >= seuil]

# 3. créer "Autre" = reste pour arriver à 1
dt_other <- dt_big[
  , .(weight = 1 - sum(weight)),
  by = commune
]

dt_other[, ville := "Autre"]

# 4. assembler
dt_plot <- rbind(
  dt_big,
  dt_other,
  fill=TRUE
)

# 5. trier pour affichage propre
setorder(dt_plot, commune, -weight)

# vérification (doit faire 1)
dt_plot[, sum(weight), by=commune]


# récupérer toutes les villes uniques dans l'ordre que tu veux
villes_unique <- unique(dt_plot$ville)

# créer une palette (ici avec des couleurs distinctes pour chaque ville)
palette_villes <- c("Autre" = "grey70")  # Autre en gris

# ajouter les autres couleurs
couleurs_autres <- scales::hue_pal()(length(villes_unique) - 1) 
names(couleurs_autres) <- setdiff(villes_unique, "Autre")

palette_villes <- c(palette_villes, couleurs_autres)


ggplot(dt_plot, aes(x=commune, y=weight, fill=ville)) +
  geom_bar(stat="identity") +
  scale_y_continuous(labels=scales::percent) +
  scale_fill_manual(values = palette_villes) +
  theme_minimal() +
  labs(
    x="Commune traitée",
    y="Poids",
    fill="Communes donneuses",
    title="Poids SCMA-Ridge par commune pour l'année 2017"
  ) +
  theme(axis.text.x = element_text(angle=90)) +
  theme(
    axis.text.x = element_text(angle=90),
    plot.title = element_text(size=16, face="bold", hjust=0.5) # titre centré et plus visible
  )




# liste des fichiers
fichiers <- list.files(
  pattern="SCM-poids",
  full.names=TRUE
)

# lecture des fichiers
dt <- rbindlist(lapply(fichiers, function(f){
  
  tmp <- fread(f)
  
  # code commune traité (depuis le nom du fichier)
  commune <- sub("-.*", "", basename(f))
  
  tmp[, commune := commune]
  
  tmp
}))

dt[, weight_norm := weight / sum(weight), by=commune]

# seuil 5%
seuil <- 0.05

# 1. agréger les poids par ville donneuse
dt2 <- dt[
  , .(weight = sum(weight_norm)),
  by = .(commune, ville = `Libellé géographique`)
]

# 2. garder seulement les poids >= seuil
dt_big <- dt2[weight >= seuil]

# 3. créer "Autre" = reste pour arriver à 1
dt_other <- dt_big[
  , .(weight = 1 - sum(weight)),
  by = commune
]

dt_other[, ville := "Autre"]

# 4. assembler
dt_plot <- rbind(
  dt_big,
  dt_other,
  fill=TRUE
)

# 5. trier pour affichage propre
setorder(dt_plot, commune, -weight)

# vérification (doit faire 1)
dt_plot[, sum(weight), by=commune]


ggplot(dt_plot, aes(x=commune, y=weight, fill=ville)) +
  geom_bar(stat="identity") +
  scale_y_continuous(labels=scales::percent) +
  scale_fill_manual(values = palette_villes) +
  theme_minimal() +
  labs(
    x="Commune traitée",
    y="Poids",
    fill="Communes utilisées",
    title="Poids SCM classique par commune pour l'année 2017"
  )  +
  theme(
    axis.text.x = element_text(angle=90),
    plot.title = element_text(size=16, face="bold", hjust=0.5) # titre centré et plus visible
  )
