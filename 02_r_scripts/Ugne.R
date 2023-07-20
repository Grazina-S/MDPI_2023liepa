# Reikalingi packages #####
install.packages("readxl")
install.packages("writexl")
install.packages('agricolae')
install.packages("metan")
install.packages("toolStability")
install.packages("patchwork") #reikalinga sukombinuoti plots
install.packages("colorspace")
install.packages("tidyverse")
install.packages("rstatix")

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
#dmy <- dmy[-c(37, 44), ] # pasalinimas elutes 37 ir 44 nes buvo tuscios, paskui apsigalvojau nes grafike reikia tuscio langelio
dmy$year <- as.factor(dmy$year)
dmy$use_year <- as.factor(dmy$use_year)
dmy$total <- rowSums(dmy[, 3:6], na.rm = TRUE) # padarau nauja stulpeli su visu metu derlium PERSEIVINAU I TIDY_DATA
#dmy[, 3:7] <- round(dmy[, 3:7], 2) # suapvalina stulpelius 3-7 iki 2 vietu po kablelio
dmy_perc <- dmy %>% mutate_at(vars(starts_with("cut")), ~ . / total * 100) #padare nauja faila 
# jame kiekvina pjutis % nuo total



# Basic statistika ##### 
dmy_stats <- dmy %>% group_by(year, use_year) %>% get_summary_stats(cut1, cut2, cut3, cut4, total,
                                                                    show = c("mean", "sd", "se"))
dmy_stats_perc <- dmy_perc %>% group_by(year, use_year) %>% get_summary_stats(starts_with("cut"), show = c("mean", "sd", "se"))

#paveikslai ####
# Noriu dmy stacked paveikslo
#pasidarau tinkamai duomenu faila
dmy_fig <- dmy_stats %>% select(year, use_year, variable, mean) %>%
  pivot_wider(names_from = variable, values_from = mean) %>% 
  select(-total) %>%
  pivot_longer(cols = starts_with("cut"), names_to = "cut", values_to = "dmy") #cia taip sudetingai nes norejau total isimti

dmy_fig_perc <- dmy_stats_perc %>% select(year, use_year, variable, mean) %>%
  pivot_wider(names_from = variable, values_from = mean) %>%
  pivot_longer(cols = starts_with("cut"), names_to = "cut", values_to = "proc") # o cia sudetingai nes norejau kad visur atsirastu "cut4"

dmy_fig_perc$proc <- round(dmy_fig_perc$proc)
ar_100_proc <- dmy_fig_perc %>% group_by(year, use_year) %>%
  summarise(sum_proc = sum(proc, na.rm = TRUE))
years_not_equal_100 <- ar_100_proc %>%
  filter(sum_proc != 100) %>%
  pull(year)  
print(years_not_equal_100)
#2014 1-99 2016 1-101 2017-1-99 2017-2-101 2018-1-99 2019-1-101 2021-2-99
#reikia sutvarkyti
dmy_fig_perc[41, 4] <- 24
dmy_fig_perc[54, 4] <- 27
dmy_fig_perc[57, 4] <- 45
dmy_fig_perc[62, 4] <- 35
dmy_fig_perc[65, 4] <- 55
dmy_fig_perc[76, 4] <- 6
dmy_fig_perc[93, 4] <- 49
  
ar_100_proc <- dmy_fig_perc %>% group_by(year, use_year) %>%
  summarise(sum_proc = sum(proc, na.rm = TRUE))
years_not_equal_100 <- ar_100_proc %>%
  filter(sum_proc != 100) %>%
  pull(year)  
print(years_not_equal_100)
# viskas OK
#dabar reikia sulyginti eiluciu skaiciu nes dmy_fig_perc truksta 2015 1 use_year ir 2016 2 use_year
pridet <- data.frame(year = c(2015, 2015, 2015, 2015, 2016, 2016, 2016, 2016),
                     use_year = c(1, 1, 1, 1, 2, 2, 2, 2),
                     cut = c("cut1", "cut2", "cut3", "cut4", "cut1", "cut2", "cut3", "cut4"),
                     proc = NA)

dmy_fig_perc <- rbind(dmy_fig_perc, pridet)
rm(pridet, ar_100_proc, years_not_equal_100) #pasalinu is environment ko jau nereikia

#dabar noriu sulieti dmy_fig ir dmy_fig_perc
merged_dmy_fig <- left_join(dmy_fig, dmy_fig_perc, by = c("year", "use_year", "cut"))

#viskas graziai susideliojo #issisaugau del visa ko

dmyfig.path <- "01_tidy_data/dmyfig.csv"  
write.csv(dmy_fig, file = dmyfig.path, row.names = F) 

dmyperc.path <- "01_tidy_data/dmyperc.csv"
write.csv(dmy_fig_perc, file = dmyperc.path, row.names = F) 

dmymerged.path <- "01_tidy_data/dmyperc_merged.csv"
write.csv(merged_dmy_fig, file = dmymerged.path, row.names = F) 

#dabar pasidarau atskirai naudojimo metus

dmy_fig_A <- merged_dmy_fig %>% filter(use_year == "1") 
dmy_fig_B <- merged_dmy_fig %>% filter(use_year == "2")

spalvos <- c( "#26A63A",  "#9BB306", "#E1BB4E", "#FFC59E", "#F1F1F1")

Pav_DMY_A <- ggplot(dmy_fig_A, aes(x = year, y = dmy, fill = cut)) +
  geom_col(position = position_stack(reverse = TRUE), colour = "black") + 
  scale_fill_manual(values = spalvos) +
  guides(fill = guide_legend(reverse = T))  +
  xlab('Year') + ylab(bquote("DMY kg ha"^"-1"))
  

Pav_DMY_B <- ggplot(dmy_fig_B, aes(x = year, y = dmy, fill = cut)) +
  geom_col(position = position_stack(reverse = TRUE), colour = "black") + 
  scale_fill_manual(values = spalvos) +
  guides(fill = guide_legend(reverse = T))  +
  xlab('Year') + ylab(bquote("DMY kg ha"^"-1"))
 

Pav_DMY_A
Pav_DMY_B
#kaip ir OK bet noriu prideti procentus i stacked bar
aaaa <- Pav_DMY_A + geom_text(aes(label = proc), position = position_stack(vjust = 0.5, reverse = TRUE)) + 
  scale_y_continuous(breaks = seq(0, 15000, 500), minor_breaks = seq(0, 15000, 250)) + theme_bw()
bbbb <- Pav_DMY_B + geom_text(aes(label = proc), position = position_stack(vjust = 0.5, reverse = TRUE)) + 
  scale_y_continuous(limits = c(0, 14500), breaks = seq(0, 145000, 500)) + theme_bw()
plot_dmy <- aaaa/bbbb


tiff(filename = "03_plots/DMY.tif", width = 20, height = 20, units =  "cm", res = 600)
plot
dev.off()

#biski kitaip perpaisyta, naudojant facet_wrap
dmy_wrap <- ggplot(merged_dmy_fig, aes(x = year, y = dmy, fill = cut)) +
  geom_col(position = position_stack(reverse = TRUE), colour = "black") + 
  scale_fill_manual(values = spalvos) +
  guides(fill = guide_legend(reverse = T))  +
  xlab('Year') + ylab(bquote("DMY kg ha"^"-1")) +
  facet_wrap(~ use_year, nrow = 2)
dmy_wrap <- dmy_wrap + theme_bw() + scale_y_continuous(limits = c(0, 14500), breaks = seq(0, 145000, 500)) +
  theme(strip.background = element_rect(fill = "white", colour = "white", size = 1)
  )

tiff(filename = "03_plots/DMY_su_wrap.tif", width = 21, height = 22, units =  "cm", res = 600)
dmy_wrap
dev.off()
