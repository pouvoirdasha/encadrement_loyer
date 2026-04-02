data = fread("../base_2006_2022_aire_cluster.csv",
       sep = ";", dec = ",")


df_com = data[, lapply(.SD, sum, na.rm = TRUE),
              by = c("COM", "annee", "cluster_com"), .SDcols = is.numeric]

df_com = df_com[, cluster := cluster_com]

df_com[, cluster_com := NULL]
df_com[, cluster_com := NULL]

df_com = df_com[, annee_ok := annee]

df_com[, annee := NULL]
df_com[, annee := NULL]

setnames(df_com, "annee_ok", "annee")

# Variables supplémentaires

# Ratio locataires / résidences principales (part de location)
df_com[, part_loc := nb_RP_en_loc / nb_RP]

# Taux de vacance
df_com[, taux_vacance := nb_logements_vacants / nb_logements]

# Densité résidentielle (personnes par logement RP)
df_com[, densite_RP := nb_personnes_en_RP / nb_RP]

# Taux de chômage approché
df_com[, taux_chomage := nb_chomeurs / (nb_actifs + 1)]

# Part cadres parmi actifs occupés
df_com[, part_cadres := nb_cadres / (nb_actifs_occ + 1)]

# Log des variables pour réduire l'asymétrie
df_com[, log_RP_loc    := log1p(nb_RP_en_loc)]
df_com[, log_menages   := log1p(nb_menages)]
df_com[, log_logements := log1p(nb_logements)]


# On exclut les communes d'Île de France
# pour éviter les effets de bord
# de l'encadrement des loyers à Paris
df_com_hors_IDF = df_com[
  !(substr(COM, 1, 2) %in% c("77", "78", "91", 
                             "92", "93", "94", "95"))
]


data_synth <- df_com[COM %in% df_com_hors_IDF$COM]

fwrite(data_synth, "../base_2006_2022_aire_cluster-COM.csv",
       sep = ";", dec = ",")
