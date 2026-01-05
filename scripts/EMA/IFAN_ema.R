# ----------------------------------------------------------
# Preprocessing of EMA data for the IFAN project. 
# (c) Luca A. Naudszus, Essen, 28.08.2023/03.10.2023
# Institute for Psychology, University of Duisburg-Essen, Essen
# ----------------------------------------------------------
#
#
# Input: 
### export of m-Path results as csv
#-----------
# Prepares long table as well as first descriptive variables for EMA data. 
# Checks for reactivity effects in the form of drifts.
#-----------
#-----------
# Output: 
### ema_%ID%.csv, containing all EMA data points for one ID
### ema_daily_%ID%.csv, containing within-day EMA data for one ID
### ema_all.csv, containg all EMA data points for all participants
### ema_daily_all.csv, containing within-day EMA data for all participants
### ema_person.csv, containing within-person EMA data (mean)
# ---------------------------------------------------------

# initialize

rm(list=ls())
Sys.setenv(TZ = "UTC")
### load packages and sources
library(anytime)
library(lme4)
library(tidyverse)
source("scripts/Basics/IFAN_key.R")
source("scripts/EMA/IFAN_preprocess-ema.R")
### set working directory
setwd("data")
### load data
starttimes <- as_tibble(read.csv(file.path("1_raw-data", "5_EMA", "starttimes", "starttimes.csv")))
starttimes <- key(starttimes, T)
df.ema <- as_tibble(read.csv2(list.files(path = file.path("1_raw-data", "5_EMA"), pattern = "\\.csv$", full.names = T)))
df.ema <- df.ema %>%
  select(c(alias, scheduledBeepId, sentBeepId, questionListName, timeStampScheduled, timeStampSent, timeStampStart, 
                    timeStampStop, timeZoneOffset, deltaUTC, rosenberg1_1_sliderNegPos, rosenberg2_1_sliderNegPos, 
                    rosenberg3_1_sliderNegPos, rosenberg4_1_sliderNegPos, social_situations_sliderNeutralPos,
                    social_interactions_sliderNeutralPos, exclusion_sliderNegPos))
excluded_participants <- read.csv(list.files(path = file.path("2_preprocessed-data", "meta"), pattern = 'excluded', full.names = T))
included_participants <- read.csv(list.files(path = file.path("2_preprocessed-data", "meta"), pattern = "included", full.names = T))
colnames(df.ema) <- c("id", "scheduledBeepId", "sentBeepId", "name", "scheduledTime", "sentTime", "startTime", 
                       "stopTime", "timeZoneOffset", "deltaUTC", "rsesema_1", "rsesema_2", "rsesema_3", "rsesema_4",
                       "socialSituations", "socialInteractions", "exclusion")
piloting_participants <- c("ANNONYMUS", "ELIAS", "ES", "FRÖSCH", "HEKTOR", "JUDITH", "LUCA", "MARCUS", "NEO", "TOBUS")
df.ema <- key(df.ema, F)
df.ema <- df.ema[!(df.ema$id %in% piloting_participants),]
# reformat time
df.ema$scheduledTime <- as.POSIXct(df.ema$scheduledTime, 
                                    origin = "1970-01-01", 
                                    tz = "Europe/Berlin")
df.ema$date <- strftime(df.ema$scheduledTime, format = "%d/%m/%Y")
df.ema$time <- strftime(df.ema$scheduledTime, format = "%H:%M")
df.ema <- df.ema[as.Date(df.ema$date, format = "%d/%m/%Y") != "1970-01-01",]
df.ema_all <- NULL
df.ema_daily_all <- NULL
problematic_participants <- c("KR03HN", "MN26RL", "SL03DR", "SN07WL", "SS09AD", "UR03BR")
# KR03HN, SL03DR, SS09AD, UR03BR were admitted but never started the study
# MN26RL, SN07WL started but never administered the EMA questionnaires

# recode variables
for (id in sort(unique(df.ema$id))) {
  if (id %in% problematic_participants) {
    next
  } 
  onset = starttimes$onset[starttimes$id == id]
  start = as.Date(starttimes$start[starttimes$id == id], format = "%m/%d/%y")
  end = as.Date(starttimes$end[starttimes$id == id], format = "%m/%d/%y")
  df.id <- preprocess_ema(id, onset, start, end, df.ema)
  df.daily <- daily_ema(df.id)

  # save df.data and df.daily
  write.csv(df.id, sprintf("2_preprocessed-data/ema/ema_%s.csv", id), row.names = F)
  write.csv(df.daily, sprintf("2_preprocessed-data/ema-daily/ema-daily_%s.csv", id), row.names = F)
  
  # write into large tables
  df.ema_all <- rbind(df.ema_all, df.id)
  df.ema_daily_all <- rbind(df.ema_daily_all, df.daily)
}
# AD12HU will produce a warning because participant quit and rejoined, data from first phase is discarded
# SE06HL, US01MR will produce a warning because participants failed to stop EMA phase and proceed to final inquiry, data from after end of dream recording is discarded

# calculate within-person mean, compliance and duration
df.ema_person <- df.ema_all %>%
  group_by(id) %>%
  summarize(rses_1 = mean(rsesema_1, na.rm = T), 
            rses_2 = mean(rsesema_2r, na.rm = T),
            rses_3 = mean(rsesema_3, na.rm = T),
            rses_4 = mean(rsesema_4r, na.rm = T),
            socSit = mean(socialSituations, na.rm = T),
            socInt = mean(socialInteractions, na.rm = T),
            excl = mean(exclusion, na.rm = T),
            rses_total = mean(rses_total, na.rm = T),
            compliance = 1-mean(is.na(rsesema_1)),
            duration = n()/7)

# save data
write.csv(df.ema_all, "2_preprocessed-data/ema_all.csv", row.names = F)
write.csv(df.ema_daily_all, "2_preprocessed-data/ema_daily_all.csv", row.names = F)
write.csv(df.ema_person, "2_preprocessed-data/ema_person.csv", row.names = F)