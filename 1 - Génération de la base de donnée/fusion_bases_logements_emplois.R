library(dplyr)
library(magrittr)
library(data.table)


logements_1 <- fread("logements_2012_2022.csv", sep = ";")
logements_2 <- fread("logements_2006_2011.csv", sep = ";")
logements = rbdind(logements_1, logements_2)
rm(logements_1, logements_2)

emplois_1 <- fread("emplois_2012_2022.csv", sep = ";")
emplois_2 <- fread("emplois_2006_2011.csv", sep = ";")
emplois = rbind(emplois_1, emplois_2)
rm(emplois_1, emplois_2)


# On joint sur IRIS et annee
base_finale <- logements %>%
  left_join(emplois, by = c("IRIS", "annee"))

# Si COM existe dans les deux bases, dplyr crée COM.x et COM.y
if("COM.x" %in% names(base_finale)){
  base_finale <- base_finale %>%
    rename(COM = COM.x) %>%
    select(-any_of("COM.y"))
}

fwrite(base_finale, "base_2006_2022.csv", row.names = FALSE)