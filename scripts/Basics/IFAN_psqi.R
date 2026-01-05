# ----------------------------------------------------------
# Calculates PSQI scores for participants in the IFAN project. 
# (c) Luca A. Naudszus, Essen, 24.08.2023 / Da Nang, 12.08.2025
# Institute for Psychology, University of Duisburg-Essen, Essen
# Social Brain Sciences Lab, ETH Zurich, Zurich
# ----------------------------------------------------------
#
#
# Input: 
### export of final inquiry as csv (with manually corrected PSQI items)
#-----------
# Calculates PSQI scores. 
# !!! A large number of participants fulfills the exclusion criterion for sleep quality. Why? !!!
#-----------
#-----------
# Output: 
### psqi.csv, containing PSQI scores for all participants
# ---------------------------------------------------------

# initialize
rm(list=ls())
### load packages and sources
library(tidyverse)
source(file.path("scripts", "basics", "IFAN_key.R"))

### load data
pilots <- pull(read.csv2(
  list.files(path = file.path("data", "1_raw-data"), 
             pattern = "pilots", full.names = T)))
df_final <- as_tibble(read.csv2(
  list.files(path = file.path("data", "1_raw-data", "3_final-data"), 
             full.names = T))) %>%
  select(c(vpncode:password, date_of_last_access)) %>%
  rename(id = vpncode) %>% key(T) %>% filter(!(id %in% pilots))
df_psqi <- df_final %>%
  select(id, psqi_1:psqi_21) %>%
  rename(psqi_5a = psqi_5, 
         psqi_5b = psqi_6, 
         psqi_5c = psqi_7,
         psqi_5d = psqi_8,
         psqi_5e = psqi_9,
         psqi_5f = psqi_10,
         psqi_5g = psqi_11,
         psqi_5h = psqi_12,
         psqi_5i = psqi_13,
         psqi_5j = psqi_14,
         psqi_5j_which = psqi_15,
         psqi_6 = psqi_16, 
         psqi_7 = psqi_18,
         psqi_8 = psqi_19, 
         psqi_9 = psqi_20,
         psqi_10 = psqi_21)

df_psqi[df_psqi == 0] <- NA

# (1) numerify all columns --- dringendst überprüfen!
### warum sind manche Werte 0? Das dürfen sie nicht. 
df_psqi <- df_psqi %>%
  select(id, psqi_1:psqi_10)
df_psqi[df_psqi == 0] <- NA
### manually recode values
# --- Item 1
df_psqi$psqi_1_t <- df_psqi$psqi_1
df_psqi$psqi_1_t[grepl("11", df_psqi$psqi_1)] <- 23
df_psqi$psqi_1_t[grepl(paste(c("Mitternacht", "null", "zwischen 22Uhr und 2 Uhr", 
                               "12", "12 Uhr", "23:00 und 01:00", "11 -1 Uhr", 
                               "24 Uhr", "24:00:00"), collapse = '|'), df_psqi$psqi_1)] <- 0
df_psqi$psqi_1_t[grepl("zwischen 0:00 Uhr und 0:30 Uhr", df_psqi$psqi_1)] <- 0.25
df_psqi$psqi_1_t[grepl("00:20", df_psqi$psqi_1)] <- 1/3
df_psqi$psqi_1_t[grepl(paste(c("0:30 Uhr", "00:30", "0.5", "23:00-02:00", 
                               "zwischen 23 Uhr und 2 Uhr", "12-1 uhr", 
                               "11 und 2", "0:00-1:00"), collapse = "|"), df_psqi$psqi_1)] <- 0.5
df_psqi$psqi_1_t[grepl(paste(c("ca. 24 Uhr bis 2.00 Uhr", "ca. 1Uhr"), collapse = "|"), df_psqi$psqi_1)] <- 1
df_psqi$psqi_1_t[grepl(paste(c("01:30", "1-2 Uhr nachts"), collapse = "|"), df_psqi$psqi_1)] <- 1.5
df_psqi$psqi_1_t[grepl(paste(c("Drei bis vier Uhr", "3.5"), collapse = "|"), df_psqi$psqi_1)] <- 3.5
df_psqi$psqi_1_t[grepl(paste(c("9", "21 Uhr", "21"), collapse = "|"), df_psqi$psqi_1)] <- 21 
df_psqi$psqi_1_t[grepl("21/22 Uhr", df_psqi$psqi_1)] <- 21.5
df_psqi$psqi_1_t[grepl(paste(c("ca. 22 Uhr", "21-23Uhr"), collapse = "|"), df_psqi$psqi_1)] <- 22
df_psqi$psqi_1_t[grepl("22.10 Uhr", df_psqi$psqi_1)] <- 22 + 1/6
df_psqi$psqi_1_t[grepl(paste(c("22.30", "22:30", "22 und 23", "22-23", "22.5", "22/23"), collapse = "|"), df_psqi$psqi_1)] <- 22.5
df_psqi$psqi_1_t[grepl("22.45", df_psqi$psqi_1)] <- 22.75
df_psqi$psqi_1_t[grepl(paste(c("ca. 23 Uhr", "22 und 24", "22-0",
                               "Zwischen 21 und 1 Uhr nachts"), collapse = "|"), df_psqi$psqi_1)] <- 23
df_psqi$psqi_1_t[grepl(paste(c("23/24", "23 und 24", "23:30", "23-24", "23 - 0", 
                               "23.30", "11:30", "11/12 Uhr", "zwischen 21Uhr und 2Uhr"), collapse = "|"), df_psqi$psqi_1)] <- 23.5
df_psqi$psqi_1_t[grepl(paste(c("23-23.30", "23.25"), collapse = "|"), df_psqi$psqi_1)] <- 23.25
df_psqi$psqi_1_t[grepl("nein", df_psqi$psqi_1)] <- NA
# --- Item 2
df_psqi$psqi_2_t <- df_psqi$psqi_2
df_psqi$psqi_2_t[grepl(paste(c("schnell", "sek", "sehr kurz", "10sek"), collapse = "|"), df_psqi$psqi_2)] <- 0
df_psqi$psqi_2_t[grepl("1-3", df_psqi$psqi_2)] <- 2
df_psqi$psqi_2_t[grepl(paste(c("5 Minuten", "paar Minuten", "00:05"), collapse = "|"), df_psqi$psqi_2)] <- 5
df_psqi$psqi_2_t[grepl("5-7min", df_psqi$psqi_2)] <- 6
df_psqi$psqi_2_t[grepl(paste(c("5-10 Minuten", "5-10 min"), collapse = "|"), df_psqi$psqi_2)] <- 7.5
df_psqi$psqi_2_t[grepl(paste(c("5-15 Minuten", "max. 10 Min."), collapse = "|"), df_psqi$psqi_2)] <- 10
df_psqi$psqi_2_t[grepl("10-15 Minuten", df_psqi$psqi_2)] <- 12.5
df_psqi$psqi_2_t[grepl(paste(c("höchstens 15 min.", "15 Minuten", 
                               "10-20 Minuten", "10-20 min"), collapse = "|"), df_psqi$psqi_2)] <- 15
df_psqi$psqi_2_t[grepl("15-20", df_psqi$psqi_2)] <- 17.5
df_psqi$psqi_2_t[grepl("10 bis 30 Minuten", df_psqi$psqi_2)] <- 20
df_psqi$psqi_2_t[grepl(paste(c("20-25", "15-30min", "15-30 Minuten", "15 bis 30 Minuten",
                               "zwischen 15 und 30"), collapse = "|"), df_psqi$psqi_2)] <- 22.5
df_psqi$psqi_2_t[grepl(paste(c("20-30 Minuten", "25 Minuten"), collapse = "|"), df_psqi$psqi_2)] <- 25
df_psqi$psqi_2_t[grepl(paste(c("0,5 h", "30", "1/2", "Halbe"), collapse = '|'), df_psqi$psqi_2)] <- 30
df_psqi$psqi_2_t[grepl("30-40min", df_psqi$psqi_2)] <- 35
df_psqi$psqi_2_t[grepl("20-60", df_psqi$psqi_2)] <- 40
df_psqi$psqi_2_t[grepl(paste(c("0\\.5-1", "halbe bis eine Stunde", 
                               "halbe Stunde bis Stunde", "45 Minuten", 
                               "Halbe bis ganze Stunde", "ca. 30-60 Minuten"), collapse = '|'), df_psqi$psqi_2)] <- 45
df_psqi$psqi_2_t[grepl(paste(c("eine Stunde", "ca. 1 Stunde", "1 stunde"), collapse = '|'), df_psqi$psqi_2)] <- 60
df_psqi$psqi_2_t[grepl("3/4 bis 1,5 Stunden", df_psqi$psqi_2)] <- 67.5
df_psqi$psqi_2_t[grepl(paste(c("30-120 min", "ca. 1 -1 1/2 Stunden", 
                               "30 min-2 h"), collapse = "|"), df_psqi$psqi_2)] <- 75
df_psqi$psqi_2_t[grepl(paste(c("1-2", "Anderthalb"), collapse = "|"), df_psqi$psqi_2)] <- 90
df_psqi$psqi_2_t[grepl("2 stunden", df_psqi$psqi_2)] <- 120
# --- Item 3
df_psqi$psqi_3_t <- df_psqi$psqi_3
df_psqi$psqi_3_t[grepl(paste(c("5:30", "05:30"), collapse = "|"), df_psqi$psqi_3)] <- 5.5
df_psqi$psqi_3_t[grepl(paste(c("5.40"), collapse = "|"), df_psqi$psqi_3)] <- 5 + 2/3
df_psqi$psqi_3_t[grepl("6:10", df_psqi$psqi_3)] <- 6.17
df_psqi$psqi_3_t[grepl(paste(c("Zwischen 5 und 8", "6:30", "06:30", "Jun 30"), collapse = "|"), df_psqi$psqi_3)] <- 6.5
df_psqi$psqi_3_t[grepl("6:45", df_psqi$psqi_3)] <- 6.75
df_psqi$psqi_3_t[grepl(paste(c("6:30-8", "7:15"), collapse = "|"), df_psqi$psqi_3)] <- 7.25

df_psqi$psqi_3_t[grepl(paste(c("08:15", "Aug 15"), collapse = "|"), df_psqi$psqi_3)] <- 8.25
df_psqi$psqi_3_t[grepl(paste(c("08:30", "8:30", "8-9", "8 und 9", "8 oder 9", 
                             "8/9:00", "8:00-9:00", "8 - 9 Uhr", "Aug 30"), collapse = "|"), df_psqi$psqi_3)] <- 8.5
df_psqi$psqi_3_t[grepl(paste(c("sieben und neun", "07:00 und 09:00", "7-9", "Unter der Woche um 7:30-8 Uhr", 
                               "Unter der Woche zwischen 7 und 8 Uhr"), collapse = "|"), df_psqi$psqi_3)] <- 8
df_psqi$psqi_3_t[grepl("7:45-8:30", df_psqi$psqi_3)] <- 8.125
df_psqi$psqi_3_t[grepl(paste(c("7/8", "7 /8", "07:30", "7 bis 8", "7 und 8", "07. Aug", "7:30", "7-8", "7.30",
                               "3 x wöchentlich um 5 Uhr und 4 x wöchentlich um 10", 
                               "4:30-9:00, meistens so 7-8 Uhr", "6:40-8:30"), collapse = "|"), df_psqi$psqi_3)] <- 7.5
df_psqi$psqi_3_t[grepl(paste(c("zwischen 8 und 10", "08-10"), collapse = "|"), df_psqi$psqi_3)] <- 9
df_psqi$psqi_3_t[grepl(paste(c("9:30", "8 und 11", "9-10", "9 und 10", "9/10"), collapse = "|"), df_psqi$psqi_3)] <- 9.5
df_psqi$psqi_3_t[grepl(paste(c("8\\.30-9\\.00", "8:00 - 9:30 Uhr"), collapse = "|"), df_psqi$psqi_3)] <- 8.75
df_psqi$psqi_3_t[grepl(paste(c("zehn", "8 Uhr unter der Woche 12 am Wochenende"), collapse = "|"), df_psqi$psqi_3)] <- 10
df_psqi$psqi_3_t[grepl(paste(c("10:30", "zwischen 9 \\(selten\\) und 12", "10/11"), collapse = "|"), df_psqi$psqi_3)] <- 10.5
df_psqi$psqi_3_t[grepl("10:45", df_psqi$psqi_3)] <- 10.75
# --- Item 4
df_psqi$psqi_4_t <- df_psqi$psqi_4
df_psqi$psqi_4_t[grepl("Viereinhalb", df_psqi$psqi_4)] <- 5 # abbreviated
df_psqi$psqi_4_t[grepl(paste(c("05. Mai", "5 -6"), collapse = "|"), df_psqi$psqi_4)] <- 5.5
df_psqi$psqi_4_t[grepl(paste(c("06. Jul", "06. Mai", "6[-–]7", "6,5"), collapse = "|"), df_psqi$psqi_4)] <- 6.5
df_psqi$psqi_4_t[grepl("5-9", df_psqi$psqi_4)] <- 7
df_psqi$psqi_4_t[grepl("7:15", df_psqi$psqi_4)] <- 7.25
df_psqi$psqi_4_t[grepl(paste(c("sieben bis acht", "07. Aug", "7 - 8", "7 bis 8", 
                             "7-8", "7/8", "7 und 8", "7,5"), collapse = "|"), df_psqi$psqi_4)] <- 7.5
df_psqi$psqi_4_t[grepl("ca. 8h", df_psqi$psqi_4)] <- 8
df_psqi$psqi_4_t[grepl(paste(c("08. Sep", "8 1/2", "8-9", "8,3", "8,5", 
                             "8h 30", "8:30h"), collapse = "|"), df_psqi$psqi_4)] <- 8.5
df_psqi$psqi_4_t[grepl("08:40", df_psqi$psqi_4)] <- 8 + 2/3
df_psqi$psqi_4_t[grepl(paste(c("in der Woche 6 am Wochenende 12", "8-10"), collapse = "|"), df_psqi$psqi_4)] <- 9
df_psqi$psqi_4_t[grepl(paste(c("09:30", "9/10", "9,5"), collapse = "|"), df_psqi$psqi_4)] <- 9.5

### parse columns, consider time format
df_psqi$psqi_1_num <- parse_number(df_psqi$psqi_1_t)
df_psqi$psqi_2_num <- parse_number(df_psqi$psqi_2_t)
df_psqi$psqi_2_num[grepl(paste(c("h", "Stunde"), collapse = "|"), df_psqi$psqi_2_t)] <- 60 * df_psqi$psqi_2_num[grepl(paste(c("h", "Stunde"), collapse = "|"), df_psqi$psqi_2_t)]
df_psqi$psqi_3_num <- parse_number(df_psqi$psqi_3_t)
df_psqi$psqi_4_num <- parse_number(df_psqi$psqi_4_t)
# (2) calculate scores according to PSQI scoring algorithm
df_psqi <- df_psqi %>%
  mutate(across(c(psqi_5a:psqi_5j, psqi_6:psqi_10), ~ . - 1)) %>%
  mutate(
    psqi_slpqual = psqi_9, # Component 1: general sleep quality
    # Component 2: sleep latency
    psqi_2new = case_when(
      psqi_2_num > 60 ~ 3,
      psqi_2_num > 30 ~ 2,
      psqi_2_num > 15 ~ 1,
      psqi_2_num >= 0 ~ 0),
    psqi_laten_raw = psqi_2new + psqi_5a,
    psqi_laten = case_when(
      psqi_laten_raw >= 5 ~ 3,
      psqi_laten_raw >= 3 ~ 2,
      psqi_laten_raw >= 1 ~ 1,
      psqi_laten_raw == 0 ~ 0
    ),
    # Component 3: sleep duration
    psqi_durat = case_when(
      psqi_4_num > 7 ~ 0,
      psqi_4_num >= 6 ~ 1, 
      psqi_4_num >= 5 ~ 2,
      psqi_4_num < 5 ~ 3), 
    # Component 4: sleep efficiency
    df_psqi <- df_psqi %>%
  mutate(across(c(psqi_5a:psqi_5j, psqi_6:psqi_10), ~ . - 1)) %>%
  mutate(
    psqi_slpqual = psqi_9, # Component 1: general sleep quality
    # Component 2: sleep latency
    psqi_2new = case_when(
      psqi_2_num > 60 ~ 3,
      psqi_2_num > 30 ~ 2,
      psqi_2_num > 15 ~ 1,
      psqi_2_num > 0 ~ 0),
    psqi_laten_raw = psqi_2new + psqi_5a,
    psqi_laten = case_when(
      psqi_laten_raw >= 5 ~ 3,
      psqi_laten_raw >= 3 ~ 2,
      psqi_laten_raw >= 1 ~ 1,
      psqi_laten_raw == 0 ~ 0
    ),
    # Component 3: sleep duration
    newtib = (psqi_3_num - psqi_1_num + 24) %% 24,
    newtib = pmax(newtib, 0.01),
    psqi_4_num = pmin(newtib, psqi_4_num),
    psqi_durat = case_when(
      psqi_4_num >= 7 ~ 0,
      psqi_4_num >= 6 ~ 1, 
      psqi_4_num >= 5 ~ 2,
      psqi_4_num < 5 ~ 3), 
    # Component 4: sleep efficiency
    tmphse_raw = (psqi_4_num / newtib) * 100,
    tmphse = case_when(
      tmphse_raw >= 85 ~ 0,
      tmphse_raw >= 75 ~ 1,
      tmphse_raw >= 65 ~ 2,
      tmphse_raw < 65 ~ 3
    ),
    # Component 5: sleep disturbance
    psqi_5j = if_else(psqi_5j_which == -99, 0, as.double(psqi_5j)), 
    psqi_distb_raw = rowSums(select(., psqi_5b:psqi_5j)), 
    psqi_distb = case_when(
      psqi_distb_raw > 18 ~ 3, 
      psqi_distb_raw > 9 ~ 2,
      psqi_distb_raw > 0 ~ 1,
      psqi_distb_raw == 0 ~ 0
    ), 
    psqi_meds = psqi_6, # Component 6: sleep medication
    # Component 7: daytime dysfunction
    psqi_daydys_raw = psqi_7 + psqi_8, 
    psqi_daydys = case_when(
      psqi_daydys_raw >= 5 ~ 3,
      psqi_daydys_raw >= 3 ~ 2, 
      psqi_daydys_raw >=1 ~ 1,
      psqi_daydys_raw == 0 ~ 0
    ),
    # Total score
    psqi_total = psqi_slpqual + psqi_laten + psqi_durat + tmphse + psqi_distb + psqi_meds + psqi_daydys
  )
)

# Sanity checks
summary(select(df_psqi, psqi_slpqual, psqi_laten, psqi_durat, tmphse, psqi_distb, psqi_meds, psqi_daydys))
range(df_psqi$psqi_total, na.rm = TRUE)
any(df_psqi$psqi_total %% 1 != 0, na.rm = TRUE)

# Write data
write.csv(df_psqi, file = file.path("data", "2_preprocessed-data", 
                                    "meta", "psqi.csv"), 
          row.names = F)
