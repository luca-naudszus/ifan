# ----------------------------------------------------------
# Preprocessing of IAT data for the IFAN project. 
# (c) Luca A. Naudszus, Essen, 25.08.2023 / Zürich, 24.11.2025
# Institute for Psychology, University of Duisburg-Essen, Essen
# ----------------------------------------------------------
#
#
# Input: 
### Pavlovia exports of IAT data as csv
#-----------
# Applies the improved scoring algorithm (Greenwald et al., 2003) to the raw IAT data for all participants.
# Renders d-scores for all participants as well as d-scores applying an odd-even split. 
# !!! Something doesn't add up here. Number of participants is way too low. (177 started, 158 valid, 106 in final data.) Check again. !!!
#-----------
#-----------
# Output: 
### iat.csv, contains d-scores for all participants
# ---------------------------------------------------------

# initialize

rm(list=ls())

### load packages and sources
library(tidyverse)
source(file.path("scripts", "basics", "IFAN_key.R"))
score_iat <- function(df){
  # initialize
  blockmean <- c()
  # compute mean of correct latencies for each block
  for (in_block in unique(df$trials.thisRepN)){
    blockmean[in_block] <- mean(df$rt[df$trials.thisRepN == in_block & df$corr == T])
  }
  # compute pooled SDs for B3 + B6, for B4 + B7
  SD1 <- sd(df$rt[df$trials.thisRepN %in% c(3, 6)])
  SD2 <- sd(df$rt[df$trials.thisRepN %in% c(4, 7)])
  # replace each error latency with block mean + 600 ms
  df <- df %>%
    mutate(rt = case_when(
      corr == 0 ~ blockmean[trials.thisRepN] + 0.6,
      TRUE ~ rt))
  # average all resulting latencies for each block
  for (in_block in unique(df$trials.thisRepN)){
    blockmean[in_block] <- mean(df$rt[df$trials.thisRepN == in_block])
  }
  # compute difference scores
  dscore <- mean((blockmean[6] - blockmean[3])/SD1, (blockmean[7] - blockmean[4])/SD2)
  # return dscore
  return(dscore)
}

### settings
use_poor_slpq = F

### load data
df_all <- NULL
for (filename in list.files(path = file.path("data", "1_raw-data", "4_IAT", "data"), pattern = '.csv', full.name = T)){
  if (file.size(filename) > 5){
    data <- read.csv(filename)
  }
  if ('instr_done_touch.clicked_name' %in% names(data)){
    data <- subset(data, select = -instr_done_touch.clicked_name)
  }
  if ('key_resp.rt' %in% names(data)){
    data <- subset(data, select = -c(key_resp.rt, key_resp.duration))
  }
  if (dim(data)[2] == 31){
    df_all <- rbind(df_all, data)
  }
}
colnames(df_all)[1] <- "instr_done_touch.x"
colnames(df_all)[25] <- "id"

### load data from db
db <- read.csv(list.files(path = file.path("data", "1_raw-data", "4_IAT", "db"), pattern = '.csv', full.name = T))
db <- subset(db, select = -c(X__experimentName, instr_done_touch.clicked_name, X__participant, X__datetime, key_resp.duration, key_resp.rt, X__session))
colnames(db)[19] <- 'id'
colnames(db)[1] <- 'psychopyVersion'

### bind data
df_all <- rbind(df_all, db)
df_all <- key(df_all, F)


df_included <- read.csv(list.files(path = file.path("data", "2_preprocessed-data", "meta"), pattern = 'included', full.names = T))
df_all <- df_all[(df_all$id %in% df_included$x),]


# calculate d-score
df_iat <- NULL
### split again into separate files per participant
participants <- unique(df_all$id)
participants <- participants[!is.na(participants) & participants != ""]
for (in_participant in participants){
  # initialize
  blockmean <- c()
  data <- df_all[df_all$id == in_participant & !is.na(df_all$id),]
  if (dim(data)[1] < 120) {
    df_iat <- rbind(df_iat, cbind(in_participant, NA, NA, NA))
    next
  }
  # number blocks
  data$trials.thisRepN = c(rep(1, 20), rep(2, 20), rep(3, 20), rep(4, 40), rep(5, 20), rep(6, 20), rep(7, 40), rep(NA, dim(data)[1]-180))
  # eliminate blocks 1, 2 and 5
  data <- data[data$trials.thisRepN %in% c(3, 4, 6, 7),]
  # eliminate trials with latencies above 10.000 ms
  data <- data[data$rt < 10,]
  # eliminate subject in case more than 10% of latencies are below 300 ms
  too_fast <- data[data$rt < .3,]
  if ((dim(too_fast)[1] / dim(data)[1]) > .1){
    df_iat <- rbind(df_iat, cbind(in_participant, NA, NA, NA))
    next
  }
  # split data for split-half reliability calculations
  data.odd <- data[seq(from = 1, to = dim(data)[1], by = 2),]
  data.even <- data[seq(from = 2, to = dim(data)[1], by = 2),]
  # use above specified function on all three data frames
  dscore <- score_iat(data)
  dscore.odd <- score_iat(data.odd)
  dscore.even <- score_iat(data.even)
  # save d-score
  df_iat <- rbind(df_iat, cbind(in_participant, dscore, dscore.odd, dscore.even))
}

# save data
df_iat <- as_tibble(df_iat)
df_iat$dscore <- as.numeric(df_iat$dscore)
df_iat$dscore.odd <- as.numeric(df_iat$dscore.odd)
df_iat$dscore.even <- as.numeric(df_iat$dscore.even)
colnames(df_iat)[1] <- "id"
write.csv(df_iat, file.path('data', '2_preprocessed-data', 'personality', 'implicit-association-test.csv'))