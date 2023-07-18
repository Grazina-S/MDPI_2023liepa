# Reikalingi packages #####
install.packages("readxl")
install.packages("writexl")
install.packages('agricolae')
install.packages("metan")
install.packages("toolStability")

# laibrariai #########################
library(readxl)
library(openxlsx)
library(agricolae)
library(metan)
library(tidyverse)
library(toolStability)
library(rstatix)

# Nuskaitymas is excel #########################
dmy <- read_excel("00_raw_data\\Ugnė_nuo 2009.xlsx", sheet = "DMY")
scores <- read_excel("00_raw_data\\Ugnė_nuo 2009.xlsx", sheet = "scores")
quality <- read_excel("00_raw_data\\Ugnė_nuo 2009.xlsx", sheet = "quality")
#          perkelimas i csv
file.path <- "01_tidy_data/dmy.csv"            # Kad atsidurtu subfolderyje
write.csv(dmy, file = file.path, row.names = FALSE)
path.scores <- "01_tidy_data/scores.csv"
write.csv(scores, file = path.scores, row.names = FALSE)
path.quality <- "01_tidy_data/quality.csv"
write.csv(quality, file = path.quality, row.names = F)

# datafreimu tvarkymas ####
#                DMY
dmy <- dmy[-c(37, 44), ] # pasalinau elutes 37 ir 44 nes buvo tuscios
dmy$year <- as.factor(dmy$year)
dmy$use_year <- as.factor(dmy$use_year)
dmy$total <- rowSums(dmy[, 3:6], na.rm = TRUE) # padarau nauja stulpeli su visu metu derlium
dmy[, 3:7] <- round(dmy[, 3:7], 2) # suapvalinau stulpelius 3-7 iki 2 vietu po kablelio
dmy %>% group_by(year, use_year) %>% get_summary_stats(cut1, cut2, cut3, cut4, total,
                                                       show = c("mean", "sd", "se"))
# Basic statistika ##### 
dmy_stats <- dmy %>% group_by(year, use_year) %>% get_summary_stats(cut1, cut2, cut3, cut4, total,
                                                                    show = c("mean", "sd", "se"))

