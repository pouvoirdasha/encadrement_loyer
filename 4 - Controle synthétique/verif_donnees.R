library(data.table)

data_1 = fread("../base_2012_2022.csv")

data_1_com <- data_1[, lapply(.SD, sum, na.rm = TRUE),
                     by = .(COM, annee),
                     .SDcols = is.numeric]

data_2 = fread("../base_2006_2022.csv")
data_2_com <- data_2[, lapply(.SD, sum),
                     by = .(COM, annee),
                     .SDcols = is.numeric]
