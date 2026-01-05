# ----------------------------------------------------------
# Preprocessing of dream data for the IFAN project. 
# (c) Luca A. Naudszus, Essen, 30.08.2023 / Zurich, 13.04.2024 / Luang Prabang, 19.07.2025
# Institute for Psychology, University of Duisburg-Essen, Essen
# Social Brain Sciences Lab, ETH Zurich, Zurich
# ----------------------------------------------------------
#
#
# Input: 
### export of IFAN_dream as csv
### split and coded dream series
#-----------
# Prepares long tables containing peripheric data, content and social content analysis data for all dreams and all participants
# Attention: does not account for intersubjects!
# Attention: there is a datetime issue for HI01IG
#-----------
#-----------
# Output: 
### alldreams.csv containing all data for all dream situations
### dreams_daily.csv containing most important data for all dreams on day level
# ---------------------------------------------------------

rm(list=ls())
Sys.setenv(TZ = "Europe/Berlin")
### load packages and sources
library(anytime)
library(readxl)
library(tidyverse)
source(file.path("scripts", "basics", "IFAN_key.R"))
source(file.path("scripts", "dreams", "IFAN_preprocess-dreams.R"))

### load data
df_data <- as_tibble(read.csv2(list.files(
  path = file.path("data", "1_raw-data", "2_dream-data"), full.names = T)))
df_data <- df_data %>%
  select(vpncode:content, datetime) %>%
  rename(id = vpncode)

df_data <- key(df_data, F)
df_initial <- as_tibble(read.csv2(list.files(
  path = file.path("data", "1_raw-data", "1_initial-data"), full.names = T))) %>% 
  rename(id = vpncode)
df_initial <- key(df_initial)
df_excluded <- read.csv(list.files(
  path = file.path("data", "2_preprocessed-data", "meta"), 
  pattern = 'excluded', full.names = T))
### initialize variables
alldreams = NULL
alldreams_daily = NULL
use_poor_slpq = T #include participants with poor PSQI scores?
### exclude participants
if (use_poor_slpq){
  df_excluded <- df_excluded[df_excluded$reason != "Sleep Quality (PSQI)",]
}
df_data <- df_data[!(df_data$id %in% df_excluded$id[
  df_excluded$reason == "Piloting participants"]),]
# reformat time
df_data$datetime <- as.POSIXct(df_data$datetime,
                                   origin = "1970-01-01", 
                                   tz = "Europe/Berlin",
                               format = "%d/%m/%Y %H:%M")
problematic_participants <- c()
for (id in unique(df_data$id)){
  if (id %in% problematic_participants) {
    #TODO: Segment missing dream series
    next
  }
  # preprocess all subjects
  result <- preprocess_dreams(id, df_data)
  if (is.null(result[[1]]) && is.null(result[[2]])) {next} 
  else {
    df_ratings <- result[[1]]
    df_dreams_daily <- result[[2]]
  }
  # bind data
  alldreams <- rbind(alldreams, df_ratings)
  alldreams_daily <- rbind(alldreams_daily, df_dreams_daily)
  
  # save data
  write.csv(df_ratings, file.path("data", '2_preprocessed-data', 'dreams', sprintf("dreams_%s.csv", id)), row.names = F)
  write.csv(df_dreams_daily, file.path("data", '2_preprocessed-data', 'dreams-daily', sprintf("dreams-daily_%s.csv", id)), row.names = F)
}

# save data
write.csv(alldreams, file.path("data", '2_preprocessed-data', 'alldreams.csv'), row.names = F)
write.csv(alldreams_daily, file.path('data', '2_preprocessed-data', 'dreams_daily.csv'), row.names = F)