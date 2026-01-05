# ----------------------------------------------------------
# Analyses MADRE data in the IFAN project.  
# (c) Luca A. Naudszus, Essen, 22.08.2023/19.03.2024, Vang Vieng, 21.07.2025
# Institute for Psychology, University of Duisburg-Essen, Essen
# ----------------------------------------------------------
#
#
# Input: 
### madre.csv, containing all MADRE items for all participants. 
### neo.csv, containing all NEO items for all participants.
### export of IFAN_dream as csv
### excluded_participants.csv, containing IDs for all excluded participants and reasons for exclusion
#-----------
# Analyses MADRE data and generates correlation tables. 
#-----------
#-----------
# Output: 
### (!) mood_MadreProject.csv, containing MADRE data, NEO items, mood and information on exclusion criteria
### moodCorrelations.csv, containing correlation table for mood
### nightmareCorrelations.csv, containing correlation table for nightmares
---------------------------------------------------------

# -----------------------
# preparations
# -----------------------

rm(list=ls())
setwd('/Users/lucanaudszus/sciebo/Imaginary Friends at Night/Traumstudie_2023')
### load packages and sources
library(haven)
library(Hmisc)
library(lme4)
library(tidyverse)
source("scripts//Basics//IFAN_key.R")
### set working directory
setwd("data")
### load data
df.madre <- as_tibble(read.csv(list.files(path = "2_preprocessed-data", pattern = "madre", full.names = T)))
df.neo <- as_tibble(read.csv(list.files(path = "2_preprocessed-data", pattern = "neo", full.names = T)))
df.data <- as_tibble(read.csv2(list.files(path = "1_raw-data//2_dream-data", full.names = T)))
df.data <- df.data %>%
  select(vpncode:content, datetime)
df.data <- key(df.data, F)
excluded_participants <- read.csv(list.files(path = "2_preprocessed-data", pattern = 'excluded', full.names = T))
df.final <- as_tibble(read.csv2(list.files(path = "1_raw-data//3_final-data", full.names = T)))
df.final <- df.final %>%
  select(c(vpncode, sleep_disorder_1:medication_10))
df.final <- key(df.final, T)
### initialize variables
use_poor_slpq = T #include participants with poor PSQI scores?
use_all = T #include all participants?
### exclude participants
if (use_poor_slpq){
  excluded_participants <- excluded_participants[excluded_participants$reason != "Sleep Quality (PSQI)",]
}
if (use_all){
  excluded_participants <- excluded_participants[excluded_participants$reason == "Piloting participants",]
}
df.data <- df.data[!(df.data$vpncode %in% excluded_participants$participant),]
df.madre <- df.madre[!(df.madre$vpncode %in% excluded_participants$participant),]
### tidy up data
df.data[df.data == -77] <- NA
df.data <- df.data %>%
  mutate(day = as.Date(datetime, format = "%d.%m.%Y")) %>%
  mutate(nightmare = case_when(
    nightmare == 2 ~ 0,
    T ~ as.numeric(nightmare)
  ))
df.data <- df.data[!duplicated(select(df.data, c(vpncode, content, day)), fromLast = T),]
df.data <- df.data[!(duplicated(select(df.data, c(vpncode, day)), fromLast = T) & df.data$content == -66),]


# -----------------------
# generate data table for MADRE project (regarding mood)
# -----------------------

# select mood items from MADRE
df.madreMood = df.madre %>%
  select(vpncode, madre_1_end, madre_1_start)

# select NEO items
df.neoI = df.neo %>%
  select(vpncode:neo_60)

# bring mood data into required format
### this requires a different format for the dream data
# (!) as a temporary solution: 
df.mood <- data.frame(matrix(ncol = 24, nrow = length(unique(df.data$vpncode))))
colnames(df.mood) <- c('vpncode', 'mood_01', 'mood_02', 'mood_03', 'mood_04', 'mood_05', 'mood_06', 'mood_07', 'mood_08', 'mood_09', 'mood_10', 
                       'mood_11', 'mood_12', 'mood_13', 'mood_14', 'mood_15', 'mood_16', 'mood_17', 'mood_18', 'mood_19', 'mood_20', 
                       'mood_21', 'mood_22', 'mood_23')
df.mood$vpncode = unique(df.data$vpncode)
for (in_id in unique(df.data$vpncode)) {
  foo = df.data$mood[df.data$vpncode == in_id]
  df.mood[df.mood$vpncode == in_id, 2:(length(foo)+1)] = foo
}
# rename columns for exclusion criteria
names(df.final) <- c("vpncode", "insomnia", "narcolepsia", "nightmare_disorder", "somnambulism", "night_terror", 
                     "sleep_apnea", "REM_sleep_disorder", "other_sleep_disorder", "other_sleep_disorder_open", 
                     "no_sleep_disorder", "depression", "PTSD", "schizophrenia", "anxiety_disorder", "anxiety_open", 
                     "other_psychological_disorder", "other_psychological_disorder_open", "no_psychological_disorder",
                     "benzodiazepine", "antidepressiva", "clonazepam", "flunitrazepam", "prazosin", "psychotropic_substances", 
                     "Z_drugs", "other_medication", "other_medication_open", "no_medication")

# join table
df.allData <- df.madreMood %>%
  inner_join(df.neoI, by = "vpncode") %>%
  inner_join(df.mood, by = "vpncode") %>%
  inner_join(df.final, by = "vpncode")

# get included participants
### we exclude anyone with a sleep or psychiatric disorder and anyone taking medication known to influence dream content
### we exclude anyone with less than 3 dreams
madreParticipants <- df.allData$vpncode[!(is.na(df.allData$mood_04)) & rowSums(df.allData[c(87:94, 97:100, 102, 105:111)] == 1) == 0]

# -----------------------
# Compute reliability by ICC
# -----------------------
df.data_excluded <- df.data[df.data$vpncode %in% madreParticipants & df.data$dream == 1,]
model <- lmer(mood ~ 1 + (1 | vpncode), data = df.data_excluded)

# Calculate ICC (variance due to person / total variance)
var_components <- as.data.frame(VarCorr(model))
icc <- var_components$vcov[1] / sum(var_components$vcov)
print(icc)


# -----------------------
# Compute split-half reliability
# -----------------------

df.data_sh <- df.data_excluded %>%
  group_by(vpncode) %>%
  arrange(datetime) %>%  # Replace with a time or date variable
  mutate(row_number = row_number(),
         group = if_else(row_number %% 2 == 0, "even", "odd"), 
         n = n(),
         half = if_else(row_number() <= floor(n / 2), "first", "second"))

odd_even_means <- df.data_sh %>%
  group_by(vpncode, group) %>%
  summarise(mean_mood = mean(mood, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = group, values_from = mean_mood)

split_half_corr <- cor(odd_even_means$odd, odd_even_means$even, use = "complete.obs")

# Apply Spearman-Brown correction
sb_corrected <- (2 * split_half_corr) / (1 + split_half_corr)

first_second_means <- df.data_sh %>%
  group_by(vpncode, half) %>%
  summarise(mean_mood = mean(mood, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = half, values_from = mean_mood)

# Correlation across participants
split_half_corr <- cor(first_second_means$first, first_second_means$second, use = "complete.obs")

# Apply Spearman-Brown correction
sb_corrected <- (2 * split_half_corr) / (1 + split_half_corr)

# -----------------------
# compute correlations between mood / nightmares, MADRE data and Big Five traits
# -----------------------

# select NEO factors
df.neoF = df.neo %>%
  select(vpncode, neur:cons)

# calculate mood and nightmares
df.dreams <- df.data %>%
  group_by(vpncode) %>%
  summarise(mood = mean(mood, na.rm = T), nightmares = sum(nightmare, na.rm = T) / n())

# join tables
df.cordata <- df.madre %>%
  inner_join(df.dreams, by = "vpncode") %>%
  inner_join(df.neoF, by = "vpncode")
df.cordata_corr <- df.cordata[df.cordata$vpncode %in% madreParticipants,]

# correlation tests
mood_corr <- rcorr(as.matrix(df.cordata_corr[,c(6,8,4,2,10:14)]))
nightmares_corr <- rcorr(as.matrix(df.cordata_corr[,c(7,9,5,3,10:14)]))

# Fisher z transformation
r_mood <- as_tibble(mood_corr$r)
z_mood <- r_mood %>%
  mutate_all(~ FisherZ(.))

# -----------------------
# write data
# -----------------------

write.csv(df.allData, '2_preprocessed-data//mood_MadreProject.csv')
write_sav(df.allData, '2_preprocessed-data//mood_MadreProject.sav')
write.csv(madreParticipants, '2_preprocessed-data//marcusParticipants.csv')

write.csv(mood_corr$r, '3_analysed-data//moodCorrelations.csv')
write.csv(nightmares_corr$r, '3_analysed-data//nightmareCorrelations.csv')
write.csv(mood_corr$P, '3_analysed-data//moodP.csv')
write.csv(nightmares_corr$P, '3_analysed-data//nightmareP.csv')
write.csv(mood_corr$n, '3_analysed-data//moodN.csv')
write.csv(nightmares_corr$n, '3_analysed-data//nightmareN.csv')