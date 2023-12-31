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
install.packages("lubridate")
install.packages("FactoMineR")
install.packages(c("Factoshiny","missMDA","FactoInvestigate"))
install.packages("sjstats") # ANOVA effect size apskaiciuoti

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
library(lubridate)
library(FactoMineR)
library(Factoshiny)
library(missMDA)
library(FactoInvestigate)
library(sjstats)
library(car)

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

#jeigu naudojam TIK 1 naudojimo metus, reikia perdaryt ____________________________
dmy_fig_AA <- dmy_fig_A %>% filter(year != "2015")
Pav_DMY_AA <- ggplot(dmy_fig_AA, aes(x = year, y = dmy, fill = cut)) +
  geom_col(position = position_stack(reverse = TRUE), colour = "black") + 
  scale_fill_manual(values = spalvos) +
  #guides(fill = guide_legend(reverse = T))  +
  xlab('Year') + ylab(bquote("DMY kg ha"^"-1")) + theme_metan_minimal() + theme(legend.position = "top", legend.justification = "left")
Pav_DMY_AA
# Nu tarkim OK atrodo
tiff(filename = "03_plots/DMY_1styear.tif", width = 15, height = 15, units =  "cm", res = 600)
Pav_DMY_AA
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

# HSD PIRMAI PJUCIAI

dmy_1year <- dmy %>%  filter(use_year == "1" & total > 0)
shapiro.test(dmy_1year$cut1)
normal1stcut <- bestNormalize::bestNormalize(dmy_1year$cut1)
dmy_1year$cut1NORM <- normal1stcut$x.t
dmy1_norm <- aov(cut1NORM ~ year, data = dmy_1year)
HSD.test(dmy1_norm, "year", console = TRUE) # nu biski skiriasi rezultatas, maziau reiksmingu skirtumu
hist(dmy_1year$cut1)  # nera baisi histograma. Gal tiek tos
dmy1_1aove <- aov(cut1 ~ year, data = dmy_1year)
summary.aov(dmy1_1aove)
dmy1cut_hsd <- HSD.test(dmy1_1aove, "year", console = TRUE)
capture.output(dmy1cut_hsd, file = "04_reports/HSD_1styear_1stcut.csv")


# HSD ANTRAI PJUCIAI

hist(dmy_1year$cut2) #normaliai
dmy1_2aov <- aov(cut2 ~ year, data = dmy_1year)
summary.aov(dmy1_2aov)
dmy1_2cut.HSD <- HSD.test(dmy1_2aov, "year", console = TRUE)
capture.output(dmy1_2cut.HSD, file = "04_reports/HSD_1styear_2ndcut.csv")

# HSD TRECIAI PJUCIAI
hist(dmy_1year$cut3) #nenormaliai visiskai. nu bet tiek to
dmy1_3aov <- aov(cut3 ~ year, data = dmy_1year)
summary.aov(dmy1_3aov)
dmy1_3cut.HSD <- HSD.test(dmy1_3aov, "year", console = TRUE)
capture.output(dmy1_3cut.HSD, file = "04_reports/HSD_1styear_3ndcut.csv")

dmy1_4cut <- aov(cut4 ~ year, data = dmy_1year)
summary.aov(dmy1_4cut)
dmy1_4cut.HSD <- HSD.test(dmy1_4cut, "year", console = TRUE)
#





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



meteo_decad$snow[is.na(meteo_decad$snow)] <- 0

snow_fig <- ggplot(meteo_decad, aes(x = eile2, y = snow)) + geom_col(fill = "gray50", color = "black") + 
  facet_wrap(~year, scales = "free_x", ncol = 3, strip.position = "bottom") +
  scale_x_continuous(breaks = meteo_decad$eile2[seq(1, nrow(meteo_decad), by = 3)],
                     labels = meteo_decad$month[seq(1, nrow(meteo_decad), by = 3)]) +
  theme_metan_minimal() + xlab("Date") + ylab("Snow cover, mm")
snow_fig

tiff(filename = "03_plots/temperature.tif", width = 19, height = 29, units =  "cm", res = 600)
temper_fig + theme(legend.position = "top", legend.justification = "left", axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()

tiff(filename = "03_plots/precipitation.tif", width = 19, height = 28, units =  "cm", res = 600)
precip_fig + theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()

tiff(filename = "03_plots/snow.tif", width = 19, height = 28, units =  "cm", res = 600)
snow_fig + theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()

# ---------------------------------------- WEATHER-------------------
#duomenu failas weather.txt

weather$Date <- as.Date(weather$Date, format = "%m/%d/%Y")
str(weather)
weather$T_max <- as.numeric(weather$T_max)
sum(is.na(weather$T_max))
which(is.na(weather$T_max))
weather[2950,5] <- -5.1
weather$T_avg <- as.numeric(weather$T_avg)
sum(is.na(weather$T_avg))
which(is.na(weather$T_avg))
weather[4109, 6] <- 18.0
weather$precipitation <- as.numeric(weather$precipitation)
which(is.na(weather$precipitation))
sum(is.na(weather$precipitation))
weather[2405, 7] <- 2.2
weather$precipitation[is.na(weather$precipitation)] <- 0
weather$year <- year(weather$Date)
weather$year <- as.factor(weather$year)
weather$period <- as.factor(weather$period)
weather$period_cuts <- as.factor(weather$period_cuts)
head(weather)
weather$T_i <- (weather$T_max + weather$T_min)/2 # nesugalvojau kaip protingiau pavadinti
weather$DD <- ifelse(weather$T_i > 5, weather$T_i - 5, 5 - weather$T_i)
# cia esme kad jei auskciau nei 5C, tai bus GDD (Tmax+Tmin)/2 - Tbase
# o CDD yra Tbase - (Tmax+Tmin)/2
# pagal https://www.energy-a.eu/cooling-degree-days-and-heating-degree-days/

# kai kur ruduo yra HF vietoj FH

weather$period <- ifelse(weather$period == "HF", "FH", weather$period)
weather$period_cuts <- ifelse(weather$period_cuts == "HF", "FH", weather$period_cuts)
summary(weather)
# WATAFAK cia buvo?? vietoj periodu dabar stovi skaiciukai????? nu bliaaaa
rm(wtf, fall_harden, weather)
# matyt taip atsitiko del to kad tie stulpeliai buvo as.factor.
# Isikeliau duomenis is naujo ir pritaikiau tuos ifelse PRIES nurodant
# laikyti faktorium, ir viskas OK
# reik issisaugot nes pasirodo daug klaidu buvo
write.csv(weather, file = "01_tidy_data/weather.csv")
head(weather, 3)
# Assuming your dataframe is called weather
# Load the dplyr package (if not already loaded)

# Create a custom grouping variable based on the sequential occurrence of each period type
weather <- weather %>%
  mutate(
    block_id = paste(period, cumsum(period != lag(period, default = first(period))), sep = "")
  )

# Group by the custom grouping variable and calculate the duration of each period
result <- weather %>%
  group_by(block_id) %>%
  summarize(
    start_date = min(Date),
    end_date = max(Date),
    duration = as.numeric(difftime(max(Date), min(Date), units = "days"))
  )

# Print the result
print(result)
# nesigavo. Tiesiog rankom surasiau metus ("year") taip, kad FH butu vienais metais, o WP kitais
# 2020 ziema asiuminau nuo sausio 1 nes NEBUVO -10
rm(result, weather)
# isikeliu is naujo, vel nurodau kas yra kas
weather$Date <- as.Date(weather$Date, format = "%m/%d/%Y")
weather$year <- as.factor(weather$year)
weather$period <- as.factor(weather$period)
weather$period_cuts <- as.factor(weather$period_cuts)
summary(weather)
duration <- weather %>% group_by(year, period_cuts) %>% summarize(
  start_date = min(Date),
  end_date = max(Date),
  duration = as.numeric(difftime(max(Date), min(Date), units = "days")),
  T_mean = mean(T_avg), tot_prec = sum(precipitation), GDD = sum(DD))
duration$T_mean <- round(duration$T_mean, 1)

#OK dabar be cuts
periods_forPCA <-  weather %>% group_by(year, period) %>% summarize(
  start_date = min(Date),
  end_date = max(Date),
  duration = as.numeric(difftime(max(Date), min(Date), units = "days")),
  T_mean = mean(T_avg), tot_prec = sum(precipitation))
duration$T_mean <- round(duration$T_mean, 1)
view(periods_forPCA)
head(periods_forPCA, 2)

periods_forPCA <- periods_forPCA %>%
  pivot_wider(
    names_from = period,
    values_from = c(duration, T_mean, tot_prec),
    names_sep = "_"
  )
view(periods_forPCA)
write.csv(periods_forPCA, file = "01_tidy_data/periods_forPCA.csv")
write.csv(duration, file = "01_tidy_data/weather_indices.csv")
# biski patvarkyt rankom reikia. Ir idet derliu
yield <- dmy_1 %>% group_by(year) %>% summarize(mean(total))
# biski ne taip padare
periods_forPCA$yield <- NULL
periods_forPCA$yield <- yield$`mean(total)`
periods_forPCA$year <- as.factor(periods_forPCA$year)
# Assuming your tibble is called periods_forPCA
# Convert the tibble to a data frame
periods_forPCA <- as.data.frame(periods_forPCA)

# Set the first column as the row names (index)
row.names(periods_forPCA) <- periods_forPCA[, 1]

# Remove the first column
periods_forPCA <- periods_forPCA[, -1]
res.pca = PCA(periods_forPCA, scale.unit = TRUE, graph = T)
PCAshiny(res.pca)

#------------------------------------------------------- CUT1 -----------------------------------
# CUT1.csv su klaidingais GDD, pataisiau bet neperseivinau
cut1Anova <- aov(yield ~  prec + GDD + FH_length + FHtemp + Fhprec + WP_legth + Wptemp + WPprec, data = CUT1)
summary.aov(cut1Anova)
# supylus viska viska niekas nesigauna, tik Wptemp 0.08, kitu daug didesni
#    TIK RUDENS FAKTORIAI
cut1Anova <- aov(yield ~ FH_length* FHtemp*Fhprec, data = CUT1 )
summary.aov(cut1Anova)
# Visi P beveik 1
# TIK ZIEMOS FAKTORIAI
cut1Anova <- aov(yield ~  Wptemp*cold_days, data = CUT1 )
summary.aov(cut1Anova)
# irgi nieko

# TIK PAVASARIS
cut1Anova <- aov(yield ~ prec* GDD, data = CUT1 )
summary.aov(cut1Anova)
# irgi nieko

#   TIK TEMPERATUROS
cut1Anova <- aov(yield ~ GDD * Wptemp*cold_days, data = CUT1 )
summary.aov(cut1Anova)
# nieko

# ZIEMOS temperatura + PAVASARIO KRITULIAI IR TEMPERATURA
cut1Anova <- aov(yield ~ GDD * Wptemp * prec, data = CUT1 )
summary.aov(cut1Anova)
# sitaip padarius reiksminga Ziemos vidutine temp ir pavasario krituliai
# ZIEMOS temperatura + PAVASARIO KRITULIAI
cut1Anova <- aov(yield ~  Wptemp + prec, data = CUT1 )
summary.aov(cut1Anova)
# sitaip padarius irgi reiksminga, bet tik jei neitraukiu saveikos

# Pabandau regresine

model <- lm(yield ~ Wptemp + prec, data = CUT1)
summary(model)

avPlots(model)
# zodziu, tik sita kombinacija veikia ir daugiau jokia kita

#------------------------------CUT2 ----------------------------
CUT2 <- duration %>% filter(period_cuts == "cut2")
CUT2$yield <- cut2yield

cut2anova <- aov(yield ~ GDD+tot_prec+duration+T_mean, data = CUT2)
summary(cut2anova)
# nu nieko sau blia niekas neturejo jokios reiksmes
cut2anova <- aov(yield ~ duration, data = CUT2)
summary(cut2anova)
# T_mean turi. Bet tai kai lietus neturi, mistika
# O jei pridet pavasarini lietu
CUT2$cut1prec <- CUT1$prec
CUT2$sum_prec <- CUT2$tot_prec + CUT2$cut1prec

cut2anova <- aov(yield ~ tot_prec*cut1prec, data = CUT2)
summary(cut2anova)
# Nu niekaip neislauziu kad lietus but reiksmingas
model <- lm(yield ~ T_mean, data = CUT2)
summary(model)
CUT2$mean_prec <- CUT2$tot_prec/CUT2$duration
cut2anova <- aov(yield ~ mean_prec*T_mean, data = CUT2)
summary(cut2anova)
# nu ne ir vidurkis lietaus nepaeina

#--------------------------- CUT3---------------
CUT3 <- duration %>% filter(period_cuts == "cut3")
view(CUT3)
cut3dmy <- dmy_fig_A %>% filter(cut == "cut3", year != "2015")
CUT3$yield <- cut3dmy$dmy
rm(cut3dmy)

cut3anova <- aov(yield ~ GDD+tot_prec+duration+T_mean, data = CUT3)
summary(cut3anova)
# fakin niekas nereiksminga. paziuriu po kiekviena atskirai
cut3anova <- aov(yield ~ T_mean, data = CUT3)
summary(cut3anova)
plot(CUT3$T_mean, CUT3$yield)
model <- lm(yield ~  T_mean, data = CUT3)
summary(model)

cut2_3 <- rbind(CUT2, CUT3)
fig2 <- ggplot(cut2_3, aes(x = yield, y = T_mean,   fill = period_cuts) ) +
  geom_point(size = 3, shape = 21) + geom_smooth(method = "lm", se=FALSE) + scale_fill_manual(values = c("#26A63A", "blue")) +
  theme_metan_minimal() + xlab(bquote("DMY kg ha"^"-1")) + ylab("Average temperature, \u00b0C")
fig2 <- fig2 + theme(legend.position = c(0, 0), legend.justification = c(0, 0)) 
  
# Nu visai nieko
tiff(filename = "03_plots/cut_Tavg.tif", width = 10, height = 10, units =  "cm", res = 600)
fig2
dev.off()
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Nu OK, su derlium susitvarkiau daugmaz. Dabar kiti pozymiai
###### ----------------------BALAIS VERTINTI POZYMIAI -----------------------------------------------------------------------------
scores <- scores %>% filter(use_year == 1, year != 2015)
scores$year <- as.factor(scores$year)

wr_anova <- aov(wr_score ~ year, data = scores)
summary.aov(wr_anova)
wr_HSD <- HSD.test(wr_anova, "year", console = T)

spring_anova <- aov(spring_g ~ year, data = scores)
summary.aov(spring_anova)
springHSD <- HSD.test(spring_anova, "year", console = TRUE) # skiriasi tik 2014

summer_anova <- aov(summer_g ~ year, data = scores)
summer_HSD <- HSD.test(summer_anova, "year", console = TRUE)

fh_wp <- CUT1[, 6:12]
view(fh_wp)
fh_wp$year <- c(2009, 2010, 2011, 2012, 2013, 2014, 2016, 2017, 2018, 2019,
                2020, 2021, 2022)
mean_scores <- scores %>% group_by(year) %>% summarise(WR_score = mean(wr_score),
                                                       SPRING_G = mean(spring_g),
                                                       SUMMER_G = mean(summer_g),
                                                       SPOT = mean(spot),
                                                       RUST = mean(rust))
view(mean_scores)
fh_wp$wr_score <- round(mean_scores$WR_score, 1)
fh_wp$spring_g <- round(mean_scores$SPRING_G, 1)
# OK reik bandyt kazkokiu sasaju ieskot
model <- lm(wr_score ~  Wptemp + FHtemp , data = fh_wp)
summary(model)
#FH_length + FHtemp + Fhprec + WP_legth + Wptemp +WPprec + cold_days

# NIFIGA nesigavo
model <- lm(spring_g ~ FH_length + FHtemp + Fhprec + WP_legth + Wptemp +WPprec + cold_days,
            data = fh_wp)
summary(model)
cor.test(fh_wp$wr_score, fh_wp$spring_g, method = 'spearman')
# OK koreliacija patikima 0.59. Kas yra labai logiska. 
#Ir kas reiskia kad su regresija be abejo nieko nesigaus
plot(fh_wp$wr_score, fh_wp$spring_g)

# Dabar itaka pirmai pjuciai

scores$cut1 <- dmy_1year$cut1

dmy_scoresaov <- aov(cut1 ~ wr_score * spring_g, data = scores)
summary.aov(dmy_scoresaov)
model <- lm(cut1 ~ wr_score*spring_g, data = scores)
summary((model))
CUT1$wr_score <- fh_wp$wr_score

model2 <- lm(yield ~  Wptemp*wr_score , data = CUT1)
summary((model2))
# bandziau izlauzt kazkokias sasajas
#Jo OK Multiple R-squared:  0.5439 bet p-value: 0.05979
# zodziu, suprantu kad visiskai nesuprantu. Kodel sudejus reiksmingus
# faktorius nesigauna didelis modelio patikimumas? thefuck


weather_filtered <- weather %>% filter(period == "GP", T_i >5.4)
GP_weather <- weather_filtered %>% group_by(year) %>% summarise(Tavg = mean(T_avg),
                                                                prec_sum = sum(precipitation),
                                                                GDD = sum(DD))
GP_duration <- duration %>% filter(period_cuts %in% c("cut1", "cut2", "cut3", "cut4")) %>%
  group_by(year) %>% summarize(length = sum(duration))
GP_weather$length <- GP_duration$length
GP_weather$Tavg <- round(GP_weather$Tavg, 1)
GP_weather$prec_sum <- round(GP_weather$prec_sum)
GP_weather$GDD <- round(GP_weather$GDD)
view(GP_weather)
ugne <- genotypic_stability(data = ugne_stab, trait = "trait", genotype = "gen", environment = "env", unit.correct = FALSE)
# 0 gavos
ugne2 <- stability_variance(data = ugne_stab, trait = "trait", genotype = "gen", environment = "env", unit.correct = FALSE)
# turbut reik su pakartojimais
rm(ugne_stab)
ugne_stab <- dmy_1
ugne_stab$year <- as.factor(ugne_stab$year)
ugne_stab$use_year <- as.character(ugne_stab$use_year)

levels(ugne_stab$year)
# THEFUCK gaunu tipo yra 2015, nors akim ziurint jo nera!!!
# View the current levels of the 'year' factor column
current_levels <- levels(ugne_stab$year)

# Check if "2015" is in the current levels and remove it if it is present
if ("2015" %in% current_levels) {
  current_levels <- current_levels[current_levels != "2015"]
  
  # Update the factor levels
  ugne_stab$year <- factor(ugne_stab$year, levels = current_levels)
}

# OK dabar vel bandau stabiluma skaiciuot

ugne <- genotypic_stability(data = ugne_stab,
                            trait = "total",
                            genotype = "use_year",
                            environment = "year",
                            unit.correct = FALSE)
ugne2 <- stability_variance(data = ugne_stab,
                            trait = "total",
                            genotype = "use_year",
                            environment = "year",
                            unit.correct = FALSE)

summary.table.ugne <- table_stability(data = ugne_stab,
                                      trait = "total",
                                      genotype = "use_year",
                                      environment = "year",
                                      unit.correct = FALSE,
                                      lambda = median(ugne_stab$total))
view(summary.table.ugne)
ugne
view(ugne2)
data(Data)
genotypic.stability <- genotypic_stability(
  data = Data,
  trait = "Yield",
  genotype = "Genotype",
  environment = "Environment",
  unit.correct = FALSE)
view(genotypic.stability)

# Nu zodziu, paskaiciavo bet kas is to kai nera su kuo palygint

view(GP_weather)
write.csv(GP_weather, file = "01_tidy_data/GP_weather.csv", row.names = FALSE)
rm(GP_weather)
GP_weather$year <- as.factor(GP_weather$year)
# blyn turejau istrinti ir is naujo ikelti kad dingtu tas nematomas 2015

scores$total <- dmy_1year$total
cor.test(scores$summer_g, scores$total)
anovv <- aov(total ~ summer_g*spring_g, data = scores)
summary.aov(anovv)


GP_weather$re_growth <- mean_scores$SUMMER_G
anovv <- aov(total ~ summer_g*spring_g, data = scores)
summary.aov(anovv)
# jezusmarija kur derliu suma ...
derlius <- dmy_1year %>% group_by(year) %>% summarize(dmy_total = mean(total))
GP_weather$dmy <- derlius$dmy_total



plot(GP_weather$dmy, GP_weather$prec_sum)
plot(GP_weather$dmy, GP_weather$GDD)
plot(GP_weather$dmy, GP_weather$re_growth)
anovv <- aov(dmy ~ re_growth*prec_sum*GDD, data = GP_weather)
summary.aov(anovv)
anovv <- aov(re_growth ~ Tavg, data = GP_weather)
summary.aov(anovv)
# NIEKO

# ------------------------ LIGOS -----------------------

ligos_anova <- aov(spot ~ year, data = scores)
summary.aov(ligos_anova)
ligos_HSD <- HSD.test(ligos_anova, 'year', console = TRUE)
ligos_anova <- aov(rust ~ year, data = scores)
summary.aov(ligos_anova)
ligos_HSD <- HSD.test(ligos_anova, 'year', console = TRUE)
# NU OK. Ligos skiriasi tarp metu
GP_weather$rust <- mean_scores$RUST
GP_weather$spot <- mean_scores$SPOT  
view(GP_weather)

ligos_oras <- aov(rust ~ prec_sum+GDD+Tavg+length, data = GP_weather)
summary.aov(ligos_oras)
# NIEKO
ligos_oras <- aov(spot ~ prec_sum+GDD+Tavg+length, data = GP_weather)
summary.aov(ligos_oras)
# NIEKO
ligos_oras <- aov(spot ~ prec_sum*Tavg, data = GP_weather)
summary.aov(ligos_oras)
# NU i NAFIK. Niekas su niekuo nesigauna
http://127.0.0.1:38561/graphics/plot_zoom_png?width=1920&height=1129
# Viskas. Padarau paveiksla su A)overwinter+Spring_growth+re_growth
# B) rust + spot. Ir viskas. Nes niekur niekas nesigauna, gaidys totalus
spalvos_scores <- c( "orange",  "springgreen", "lightblue")
figA_scores$year <- as.factor(figA_scores$year)
Pav_scores_A <- ggplot(figA_scores, aes(x = year, y = score, fill = a_part)) +
  geom_col(position = position_dodge2(reverse = TRUE), colour = "black", width = 0.8) + 
  scale_fill_manual(values = spalvos_scores) +
  xlab('Year') + ylab("Score") + theme_metan_minimal() +
  theme(legend.position = "top", legend.justification = "right") +
  guides(fill = guide_legend(reverse = T)) + 
  scale_y_continuous(limits = c(0, 9), breaks = seq(0, 9, by = 1)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
                                   legend.key.size = unit(0.3, 'cm'))
  
Pav_scores_A
# Nu pagaliau gavos kaip noriu
figB_scores$year <- as.factor(figB_scores$year)
Pav_scores_B <- ggplot(figB_scores, aes(x = year, y = score, fill = disease)) +
  geom_col(position = "dodge", colour = "black", width = 0.8) + 
  scale_fill_manual(values = spalvos_scores) +
  xlab('Year') + ylab("Score") + theme_metan_minimal() +
  theme(legend.position = "top", legend.justification = "right") +
  #guides(fill = guide_legend(reverse = T)) + 
  scale_y_continuous(limits = c(0, 9), breaks = seq(0, 9, by = 1)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.key.size = unit(0.3, 'cm'))

Pav_scores_B

fig_scores <- Pav_scores_A + Pav_scores_B
tiff(filename = "03_plots/scores.tif", width = 20, height = 10, units =  "cm", res = 600)
fig_scores
dev.off()

# ----------------- KOKYBE -------------------------------------------------

quality$year <- as.factor(quality$year)

kokybe_aov <- aov(WSC ~ year, data = quality)
summary.aov(kokybe_aov)
kokybe_HSD <- HSD.test(kokybe_aov, "year", console = T)


# PAVEIKSLAS
quality_mean <- quality %>% group_by(year) %>% summarize(mean_cp = mean(CP),
                                                         mean_cf = mean(CF),
                                                         mean_dmd = mean(DMD),
                                                         mean_wsc = mean(WSC))
view(quality_mean)
quality_mean$mean_cp <- round(quality_mean$mean_cp, 1)
quality_mean$mean_cf <- round(quality_mean$mean_cf, 1)
quality_mean$mean_dmd <- round(quality_mean$mean_dmd, 1)
quality_mean$mean_wsc <- round(quality_mean$mean_wsc, 1)

fig_cp <- ggplot(quality_mean, aes(x = year, y = mean_cp)) + 
  geom_col(color = "black", fill = "springgreen", width = 0.8) +
  xlab("Year") + ylab("Crude protein, %") +
  theme_metan_minimal()

fig_cf <- ggplot(quality_mean, aes(x = year, y = mean_cf)) + 
  geom_col(color = "black", fill = "lightblue", width = 0.8) +
  xlab("Year") + ylab("Crude fiber, %") +
  theme_metan_minimal()
fig_cf

fig_dmd <- ggplot(quality_mean, aes(x = year, y = mean_dmd)) + 
  geom_col(color = "black", fill = "springgreen3", width = 0.8) +
  xlab("Year") + ylab("Dry matter digestibility, %") +
  theme_metan_minimal()
fig_dmd

fig_wsc <- ggplot(quality_mean, aes(x = year, y = mean_wsc)) + 
  geom_col(color = "black", fill = "lightblue3", width = 0.8) +
  xlab("Year") + ylab("Water soluble carbohydrates, %") +
  theme_metan_minimal()
fig_wsc

quality_fig <- (fig_cp | fig_cf) / (fig_dmd | fig_wsc)
quality_fig

tiff(filename = "03_plots/quality.tif", width = 15, height = 15, units =  "cm", res = 600)
quality_fig
dev.off()


# CV formule
calculate_cv <- function(data) {
  cv <- sd(data) / mean(data) * 100
  return(cv)
}
calculate_cv(GP_weather$dmy)
# 26.9

calculate_cv(CUT1$yield)
# 58.5
calculate_cv(CUT2$yield)
# 18.7
calculate_cv(CUT3$yield)
#50.4

