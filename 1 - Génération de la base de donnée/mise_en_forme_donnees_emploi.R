##### QUE FAIT CE FICHIER ? #####
# Ce fichier à pour but de mettre en forme
# les données issues du recensement de l'Insee
# pour en faire des bases exploitables dans notre étude


# 0 - Packages -----
library(data.table)

setwd("~/Telechargements")

##########################################
##### 1- Activité des résidents en 2022 #####
##########################################

d22 <- fread("base-ic-activite-residents-2022.csv", encoding = "UTF-8")

d22 = d22[, c("IRIS", "COM",
              "P22_ACT1564", "P22_ACTOCC1564",
              "P22_CHOM1564",
              "C22_ACT1564_STAT_GSEC11_21", "C22_ACT1564_STAT_GSEC12_22",
              "C22_ACT1564_STAT_GSEC13_23", "C22_ACT1564_STAT_GSEC14_24", 
              "C22_ACT1564_STAT_GSEC15_25", "C22_ACT1564_STAT_GSEC16_26"
              )]

setnames(d22,
         c("C22_ACT1564_STAT_GSEC11_21", "C22_ACT1564_STAT_GSEC12_22",
           "C22_ACT1564_STAT_GSEC13_23", "C22_ACT1564_STAT_GSEC14_24", 
           "C22_ACT1564_STAT_GSEC15_25", "C22_ACT1564_STAT_GSEC16_26"
         ),
         c("C22_ACTOCC1564_CS1", "C22_ACTOCC1564_CS2",
           "C22_ACTOCC1564_CS3", "C22_ACTOCC1564_CS4", 
           "C22_ACTOCC1564_CS5", "C22_ACTOCC1564_CS6"))

d20 <- fread("base-ic-activite-residents-2020.csv", encoding = "UTF-8")
d20 = d20[, c("IRIS", "COM",
              "P20_ACT1564", "P20_ACTOCC1564",
              "P20_CHOM1564",
              "C20_ACTOCC1564_CS1", "C20_ACTOCC1564_CS2",
              "C20_ACTOCC1564_CS3", "C20_ACTOCC1564_CS4", 
              "C20_ACTOCC1564_CS5", "C20_ACTOCC1564_CS6"
)]

df = merge(d22,
           d20,
           by = c("IRIS",
                  "COM"))


d19 <- fread("base-ic-activite-residents-2019.csv", encoding = "UTF-8")
d19 = d19[, c("IRIS", "COM",
              "P19_ACT1564", "P19_ACTOCC1564",
              "P19_CHOM1564",
              "C19_ACTOCC1564_CS1", "C19_ACTOCC1564_CS2",
              "C19_ACTOCC1564_CS3", "C19_ACTOCC1564_CS4", 
              "C19_ACTOCC1564_CS5", "C19_ACTOCC1564_CS6"
)]

df = merge(df,
           d19,
           by = c("IRIS",
                  "COM"))



d18 <- fread("base-ic-activite-residents-2018.csv", encoding = "UTF-8")

d18 = d18[, c("IRIS", "COM",
              "P18_ACT1564", "P18_ACTOCC1564",
              "P18_CHOM1564",
              "C18_ACTOCC1564_CS1", "C18_ACTOCC1564_CS2",
              "C18_ACTOCC1564_CS3", "C18_ACTOCC1564_CS4", 
              "C18_ACTOCC1564_CS5", "C18_ACTOCC1564_CS6"
)]


df = merge(df,
           d18,
           by = c("IRIS",
                  "COM"))




d17 <- fread("base-ic-activite-residents-2017.csv", encoding = "UTF-8")

d17 = d17[, c("IRIS", "COM",
              "P17_ACT1564", "P17_ACTOCC1564",
              "P17_CHOM1564",
              "C17_ACTOCC1564_CS1", "C17_ACTOCC1564_CS2",
              "C17_ACTOCC1564_CS3", "C17_ACTOCC1564_CS4", 
              "C17_ACTOCC1564_CS5", "C17_ACTOCC1564_CS6"
)]


df = merge(df,
           d17,
           by = c("IRIS",
                  "COM"))


# fwrite(df, "bdd_activite_residents-IRIS-2017_2022.csv")

 dt_long <- melt(
  df,
  id.vars = c("IRIS", "COM"),
  variable.name = "var",
  value.name = "val"
)

 dt_long[, `:=`(
   TYPE  = substr(var, 1, 1),           # P ou C
   ANNEE = as.integer(paste0("20", substr(var, 2, 3))),
   VAR   = sub("^[PC][0-9]{2}_", "", var)
 )]
 
 
 dt_final <- dcast(
   dt_long,
   IRIS + COM + ANNEE ~ VAR,
   value.var = "val"
 )

setnames(dt_final,
         c("ACT1564", "ACTOCC1564", "ACTOCC1564_CS1",
           "ACTOCC1564_CS2", "ACTOCC1564_CS3", "ACTOCC1564_CS4",
           "ACTOCC1564_CS5", "ACTOCC1564_CS6", "CHOM1564"), 
         c("nb_actifs", "nb_actifs_occ", "nb_agriculteurs",
           "nb_commerants", "nb_cadres", "nb_professions_inter",
           "nb_employes", "nb_ouvriers", "nb_chomeurs"))

# fwrite(dt_final, "bdd_activite_residents-IRIS-2017_2022-format_long.csv")
 