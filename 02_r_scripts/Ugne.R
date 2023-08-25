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
install.packages("flextable")
install.packages("rlist")

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
library(flextable)
library(rlist)

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

# ANOVA ir HSD ####

#DMY

#dmy_long <- dmy %>% pivot_longer(cols = c(cut1, cut2, cut3, cut4, total), names_to = "cut", values_to = "dmy")
dmytot_aov <- aov(total ~ year*use_year, data = dmy)
summary.aov(dmytot_aov)

#radau kaip viskam is karto OOO KOKS GERAS!!!!!!!
cols <- names(dmy)[3:ncol(dmy)]  # cia pradzia turi but kai baigias faktoriai ir prasideda pozymiai
aov.model <- lapply(cols, FUN = function(x) 
  aov(reformulate(termlabels = "year*use_year", 
                  response = x), 
      data = dmy))
anova(aov.model[[5]]) #paima penkta pozymi
final = anova(aov.model[[5]])[,c(1,3,5)] # paima penkta pozymi
final
rnames = rownames(final)
rnames
colnames(final) = c("DF", "MS", "P-value")
colnames(final)[2] = cols[5] # cols[5] CIA 1 REISKIA 5-o pozymio pavadinima
final
final = as.data.frame(final)
final
# Assign astericks according to p values
final$sign[final$`P-value` < 0.05] <- "*" 
final$sign[final$`P-value` < 0.01] <- "**"
final$sign[final$`P-value` > 0.05] <- "ns"
# Merge MS and significance column together
final[[2]] = paste(final[[2]], ifelse(is.na(final[[4]]), "", final[[4]]))
final

final = final[-c(3,4)]
final

anova = writexl::write_xlsx(final, path = paste(cols[5], '-ANOVA.xlsx')) # cia irgi svarbu cols[5]
# reikia viska pakartoti su visais pozymiais
# Kodo neperkopinsiu, tiesiog keisiu kur reik

# Print final ANOVA object
file.list <- list.files(pattern='*-ANOVA.xlsx')

df.list <- lapply(X = file.list, FUN = read_excel)
df.list
# Combined ANOVA table for all variables
aov.table = rlist::list.cbind(df.list)

# Remove duplicate columns for DF
dup.cols = which(duplicated(names(aov.table)))
aov.table = aov.table[,-dup.cols]
aov.table
# Names for sources of variation in ANOVA
rownames(aov.table) = rnames
aov.table

# Nu fantastika. Butu dar geriau jei suprasciau koda
# Isisaugau
write.csv(aov.table, file = "04_reports/DMY_ANOVA.csv")


# Darau HSD tik total, kiekvieniems naudojimo metams atskirai
dmy_1 <- dmy %>% select(year, use_year, total) %>% filter(use_year == "1" & total > 0)
shapiro.test(dmy_1$total) # p-value = 0.002451 REIKIA transformuoti
total1_normal <- bestNormalize::bestNormalize(dmy_1$total)
shapiro.test(total1_normal$x.t) #  p-value = 0.108
dmy_1$total_Norm <- total1_normal$x.t

dmy1.anova <- aov(total_Norm~ year, data = dmy_1)
anova(dmy1.anova)
dmy1.HSD <- HSD.test(dmy1.anova, "year", console = T)


dmy1.HSD$groups
# Step 1: Extract the "groups" element from the dmy1.HSD list
groups_data <- dmy1.HSD$groups
# Step 2: Create a new data frame with "groups" and add a new column for years
df1 <- data.frame(year = rownames(groups_data), groups_data$groups, stringsAsFactors = FALSE)
# Remove the row names
rownames(df1) <- NULL
# susortinti pagal metus didejancia tvarka
df1 <- df1 %>% arrange(year)

# OK dabar ta pat su 2 naudojimo metais

dmy_1 <- dmy %>% select(year, use_year, total) %>% filter(use_year == "2" & total > 0)
shapiro.test(dmy_1$total) # p-value = 0.9643 

dmy1.anova <- aov(total~ year, data = dmy_1)
anova(dmy1.anova)
dmy1.HSD <- HSD.test(dmy1.anova, "year", console = T)


dmy1.HSD$groups
groups_data <- dmy1.HSD$groups
df2 <- data.frame(year = rownames(groups_data), groups_data$groups, stringsAsFactors = FALSE)
rownames(df2) <- NULL
df2 <- df2 %>% arrange(year)

totalDMY_HSD <- cbind(df1, df2)

names_df1 <- c("year1", "groups1")
names_df2 <- c("year2", "groups2")
colnames(totalDMY_HSD) <- c(names_df1, names_df2) # Pakeiciau vardus nes dubliavosi

write.csv(totalDMY_HSD, file = "04_reports/HSDgroupsDMY.csv")



# ####

# nORIU PAZIURET kiek % 2ais naudojimo metais maziau derliaus
df <- dmy_stats %>% select(year, use_year, variable, mean) %>%
  filter(variable == "total")
df <- df %>% pivot_wider(names_from = use_year, values_from = mean) %>%
  filter(year != c("2015", "2016"))
df$variable <- NULL
colnames(df) <- c("year", "tot1", "tot2")
df$skirtumas <- df$tot1 - df$tot2
df$proc_skirtumas <- round(df$skirtumas/df$tot1*100)

sheet_name <- list('derlius' = df)
openxlsx::write.xlsx(sheet_name, file = "04_reports/Other_results.xlsx")

# METEO DEKADINE ####
#meteo_dekad$year <- as.factor(meteo_dekad$year)
#meteo_dekad$month <- as.factor(meteo_dekad$month)
#meteo_dekad$decade <- as.factor(meteo_dekad$decade)
#meteo_decad$date <- as.Date(meteo_decad$date, format = "%Y-%b-%d")
#meteo_dekad$`y-m-d` <- as.factor(meteo_dekad$`y-m-d`)

meteo_decad$year <- as.factor(meteo_decad$year)
meteo_decad$month <- as.factor(meteo_decad$month)
meteo_decad$decade <- as.factor(meteo_decad$decade)
meteo_decad$date <- as.Date(meteo_decad$date, format = "%Y-%b-%d")
str(meteo_decad)

ggplot(meteo_decad, aes(x = eile, y = precip), color = "blue") + geom_line()



meteo_decad$eile2 <- substring(meteo_decad$eile, 2)  
str(meteo_decad)
meteo_decad$eile2 <- as.numeric(meteo_decad$eile2)
str(meteo_decad)
mazas <- meteo_decad %>% filter(year == c(2010, 2011))

ggplot(meteo_decad, aes(x = eile2, y = precip)) + geom_col() +
  facet_wrap(~year, scales = "free_x") +
  scale_x_continuous(breaks = meteo_decad$eile2[seq(1, nrow(meteo_decad), by = 3)],
                     labels = meteo_decad$date2[seq(1, nrow(meteo_decad), by = 3)])

precip_fig <- ggplot(meteo_decad) + geom_col( aes(x = eile2, y = precip), fill = "skyblue", color = "black") + geom_line(aes(x = eile2, y = precip_longterm), color = "gray30", size = 1) +
  facet_wrap(~year, scales = "free_x", ncol = 3, strip.position = "bottom") +
  scale_x_continuous(breaks = meteo_decad$eile2[seq(1, nrow(meteo_decad), by = 3)],
                     labels = meteo_decad$month[seq(1, nrow(meteo_decad), by = 3)]) +
  theme_metan_minimal() + xlab("Date") + ylab("Precipitation, mm")


#ggplot(meteo_decad) + geom_col( aes(x = eile2, y = precip), fill = "skyblue", color = "black") + geom_line(aes(x = eile2, y = precip_longterm), color = "gray30", size = 1) +
#  facet_wrap(~year, scales = "free_x", ncol = 3, strip.position = "bottom") +
#  scale_x_continuous(breaks = meteo_decad$eile2[seq(1, nrow(meteo_decad), by = 3)],
#                     labels = meteo_decad$month[seq(1, nrow(meteo_decad), by = 3)]) +
#  theme_metan_minimal() + xlab("Date") + ylab("Precipitation, mm")

temperature$year <- as.factor(temperature$year)
temperature$date <- as.Date(temperature$date, format = "%Y-%b-%d")

str(temperature)

temperature$eile2 <- substring(temperature$eile, 2)  
temperature$eile2 <- as.numeric(temperature$eile2)

temper_fig <- ggplot(temperature, aes(x = eile2, y = temp, color = kind)) + geom_line() +
  facet_wrap( ~year, scales = "free_x", ncol = 3, strip.position = "bottom") +
  scale_x_continuous(breaks = temperature$eile2[seq(1, nrow(temperature), by = 3)],
                     labels = temperature$month[seq(1, nrow(temperature), by = 3)]) +
  theme_metan_minimal() + scale_color_manual(values = c("mean" = "blue", "long-term" = "gray30")) +
  geom_hline(yintercept = 0,  color = 'black') +
  xlab("Date") + ylab("Temperature, \u00b0C")


 
