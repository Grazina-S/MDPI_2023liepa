# Reikalingi packages #####
install.packages("readxl")
install.packages("writexl")

# #########################
library(readxl)
library(openxlsx)

# Nuskaitymas is excel #########################
dmy <- read_excel("00_raw_data\\Ugnė_nuo 2009.xlsx", sheet = "DMY")
scores <- read_excel("00_raw_data\\Ugnė_nuo 2009.xlsx", sheet = "scores")
quality <- read_excel("00_raw_data\\Ugnė_nuo 2009.xlsx", sheet = "quality")
file.path <- "01_tidy_data/dmy.csv"
write.csv(dmy, file = file.path, row.names = FALSE)
