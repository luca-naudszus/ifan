# ----------------------------------------------------------
# Merges EMA and SCA data for IFAN project. 
# (c) Luca A. Naudszus, Da Nang, 13.08.2025
# Institute for Psychology, University of Duisburg-Essen, Essen
# Social Brain Sciences Lab, ETH Zurich, Zurich
# ----------------------------------------------------------
#
#
# Input: 
### Folder with daily dream data for all participants
### Folder with daily EMA data for all participants
#-----------
# 
#-----------
# Output: 
### 
# ---------------------------------------------------------

# initialize ----------------------------------------------
rm(list=ls())

### load packages and sources
library(tidyverse)
source(file.path("scripts", "basics", "IFAN_key.R"))

### problematic ids
problematic_participants <- c("UE06UR")
# --- UE06UR has no EMA data
#TODO: Check KT07HN and SB12KR

# ----------------------------------------------
# Merge data for all participants
ids_dreams <- substr(list.files(file.path("data", "2_preprocessed-data", "dreams-daily")), 14, 19)
ids_ema <- substr(list.files(file.path("data", "2_preprocessed-data", "ema-daily")), 11, 16)
missing_ema <- ids_dreams[!(ids_dreams %in% ids_ema) & !(ids_dreams %in% problematic_participants)]
if (length(missing_ema) > 0) {
  stop('Error: There is at least one unidentified participant without EMA data.')
} 
missing_dreams <- ids_ema[!(ids_ema %in% ids_dreams) & !(ids_ema %in% problematic_participants)]
#TODO: Check participants with EMA but without dream data. 
#      They should be participants who did not finish the study. 

for (id in ids_dreams) {
  if (id %in% problematic_participants) {
    next
  }
  
  ### load data
  fn_dreams <- sprintf("dreams-daily_%s.csv", id)
  fn_ema <- sprintf("ema-daily_%s.csv", id)
  df.dreams <- read.csv(file.path("data", "2_preprocessed-data", "dreams-daily", fn_dreams))
  df.ema <- read.csv(file.path("data", "2_preprocessed-data", "ema-daily", fn_ema))
  
  ### subtract one day so that each dream report is matched with EMA data from 
  #   the day before
  df.dreams$dayCounter <- df.dreams$dayCounter - 1
  df.dreams <- rename(df.dreams, 
         dateDream = date)
  df.ema <- rename(df.ema,
         dateEMA = date)
  
  ### join data frames
  df.data <- inner_join(df.dreams, df.ema, by = c("id", "dayCounter"))
  
  ### calculate proportions
  df.data <- df.data %>% 
    mutate(propDreamSocSit = dreamSocSit / n_sit, # social situations among all situations
           propDreamSocIntOld = dreamSocInt / dreamSocSit, # social interactions among all social situations
           propDreamSocIntOld2 = dreamSocInt / n_sit, # social interactions among all situations
           propDreamMulti = multi / dreamSocInt, # Type V situations among all social interactions
           propDreamTypeV = realMulti / n_sit, # Type V situations among all situations
           propDreamSocIntOld3 = inter / dreamSocSit, # social interactions (III, V) among all social situations
           propDreamSocInt = inter / n_sit, # social interactions (III, V) among all situations
           propDreamSocEvent = n_socEve / n_eve # social events among all events
           ) %>%
    rename(propEMAsocSit = socSit,
           propEMAsocInt = socInt)
  
  ### remove Infs
  df.data[df.data == "Inf"] <- NA
  
  ### save data
  write.csv(df.data, file.path("data", "2_preprocessed-data", "multilevel", sprintf("merged-daily_%s.csv", id)))
}