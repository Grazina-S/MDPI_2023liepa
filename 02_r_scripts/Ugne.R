# Reikalingi packages #####
install.packages("readxl")
install.packages("writexl")
install.packages('agricolae')
install.packages("metan")
install.packages("toolStability")
install.packages("patchwork") #reikalinga sukombinuoti plots
install.packages("colorspace")

# laibrariai #########################
library(readxl)
library(openxlsx)
library(agricolae)
library(metan)
library(tidyverse)
library(toolStability)
library(rstatix)
library(patchwork)
library(colorspace)

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
dmy_perc <- dmy %>% mutate_at(vars(starts_with("cut")), ~ . / total * 100) #padare nauja faila 
# jame kiekvina pjutis % nuo total


# Basic statistika ##### 
dmy_stats <- dmy %>% group_by(year, use_year) %>% get_summary_stats(cut1, cut2, cut3, cut4, total,
                                                                    show = c("mean", "sd", "se"))

#paveikslai ####
# Noriu dmy stacked paveikslo
#pasidarau tinkamai duomenu faila
dmy_fig <- dmy_stats %>% select(year, use_year, variable, mean) %>%
  pivot_wider(names_from = variable, values_from = mean) %>% 
  select(-total) %>%
  pivot_longer(cols = starts_with("cut"), names_to = "cut", values_to = "dmy")
  #pivot_longer(cols = matches("^cut\\d$|^proc\\d$"), names_to = c(".value", "variable"), names_pattern = "^(cut\\d|proc\\d)$",values_to = c("dmy", "proc_value")) 

dmyfig.path <- "01_tidy_data/dmyfig.csv"  
write.csv(dmy_fig, file = dmyfig.path, row.names = F) #issisaugau del visa ko
#dabar pasidarau atskirai naudojimo metus
row2015 <- data.frame(year = 2015, use_year = "1", cut = NA, dmy = NA)
#nera duomenu tais metais, noriu pridet kad butu tuscias stulpelis
row2016 <- data.frame(year = 2016, use_year = "2", cut = NA, dmy = NA)

dmy_fig_A <- dmy_fig %>% filter(use_year == "1") %>% rbind(row2015)
dmy_fig_B <- dmy_fig %>% filter(use_year == "2") %>% rbind(row2016)

spalvos <- c( "#26A63A",  "#9BB306", "#E1BB4E", "#FFC59E", "#F1F1F1")

Pav_DMY_A <- ggplot(dmy_fig_A, aes(x = year, y = dmy, fill = cut)) +
  geom_col(position = position_stack(reverse = TRUE), colour = "black") + 
  scale_fill_manual(values = spalvos) +
  guides(fill = guide_legend(reverse = T))  +
  xlab('Year') + ylab(bquote("DMY kg ha"^"-1")) +
  theme_minimal()

PAV_DMY_B <- ggplot(dmy_fig_B, aes(x = year, y = dmy, fill = cut)) +
  geom_col(position = position_stack(reverse = TRUE), colour = "black") + 
  scale_fill_manual(values = spalvos) +
  guides(fill = guide_legend(reverse = T))  +
  xlab('Year') + ylab(bquote("DMY kg ha"^"-1")) +
  theme_minimal()
  
#uzsimaniau prideti % i paveikslus. Reikia kitaip pasidaryt faila
dmy_fig2 <- dmy_stats %>% select(year, use_year, variable, mean) %>%
  pivot_wider(names_from = variable, values_from = mean) %>%
  mutate(proc1 = cut1/total*100,
         proc2 = cut2/total*100,
         proc3 = cut3/total*100,
         proc4 = cut4/total*100) %>%
  select(-total)

dmy_fig_A2 <- dmy_fig2 %>% filter(use_year == "1") %>% select(-use_year)
dmy_fig_B2 <- dmy_fig2 %>% filter(use_year == "2") %>% select(-use_year)








