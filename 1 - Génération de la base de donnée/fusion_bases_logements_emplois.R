library(dplyr)
library(magrittr)

logements <- read.csv("Logements/logements_2012_2022.csv", sep = ";")
emplois <- read.csv("Emplois/emplois_2012_2022.csv", sep = ";")
date <- read.csv("date_encadrement_des_loyers.csv", sep = ";") %>%
  mutate(INSEE_COM = as.character(INSEE_COM), date_encadrement = as.Date(Date.de.début.de.l.encadrement, format = "%d/%m/%y"))

# On joint sur IRIS et annee
base_finale <- logements %>%
  left_join(emplois, by = c("IRIS", "annee"))

# Si COM existe dans les deux bases, dplyr crée COM.x et COM.y
if("COM.x" %in% names(base_finale)){
  base_finale <- base_finale %>%
    rename(COM = COM.x) %>%
    select(-any_of("COM.y"))
}

base_finale <- base_finale %>% left_join(date, by = c("COM" = "INSEE_COM")) 

write.csv2(base_finale, "base_2012_2022.csv", row.names = FALSE)