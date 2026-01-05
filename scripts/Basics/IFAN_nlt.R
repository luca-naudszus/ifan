# ----------------------------------------------------------
# Processing of Name Letter Test data in the IFAN project. 
# (c) Luca A. Naudszus, Utrecht, 19.08.2023
# Institute for Psychology, University of Duisburg-Essen, Essen
# ----------------------------------------------------------
#
#
# Input: 
### export of final inquiry as csv
### list of excluded participants
#-----------
# Applies the I-algorithm to the NLT data, rendering Initial Preference Scores (not stored) and Name Letter Effect Scores for all participants.  
# Cannot calculate IPF correlations yet. 
#-----------
#-----------
# Output: 
### nlt.csv, containing all letter assessments, initials and scores
# ---------------------------------------------------------

# initialize

rm(list=ls())
setwd("C://Users//Luca Naudszus//sciebo//Imaginary Friends at Night//Traumstudie_2023")
### load packages and sources
library(tidyverse)
source("scripts//IFAN_key.R")
### set working directory
setwd("data")
### load data
df.final <- as_tibble(read.csv2(list.files(path = "1_raw-data//3_final-data", full.names = T)))
df.final <- key(df.final, T)
excluded_participants <- read.csv(list.files(path = "2_preprocessed-data", pattern = 'excluded', full.names = T))
### initialize variables
alphabet = c('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 
             'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z')
use_poor_slpq = T #include participants with poor PSQI scores?
### exclude participants
if (use_poor_slpq){
  excluded_participants <- excluded_participants[excluded_participants$reason != "Sleep Quality (PSQI)",]
}
df.nlt <- df.final[!(df.final$vpncode %in% excluded_participants$participant),]

# prepare data
df.nlt <- df.nlt %>%
  select(vpncode, nlt_1:nlt_26, initial) %>% #select necessary items
  mutate_at(.funs = list(noni = ~.), .vars = vars(nlt_1:nlt_26)) %>% #create new variables for selection of noninitials
  mutate(initial_r = gsub("[^a-zA-Z]", "", df.nlt$initial)) %>% #recode initials, remove all non-letters
  mutate(initial_r = str_to_upper(initial_r)) %>% #capitalize all letters
  mutate(initial_r1 = substr(initial_r, 1, 1), #extract initials in separate columns (for no specific purpose)
         initial_r2 = substr(initial_r, 2, 2), 
         initial_r3 = substr(initial_r, 3, 3),
         initial_r4 = substr(initial_r, 4, 4))


# remove initials from columns containing noninitials
for (in_participant in df.nlt$vpncode){
  for (in_letter in nchar(df.nlt$initial_r[df.nlt$vpncode == in_participant])){
    # for every single initial get the position number in the alphabet
    julia <- which(alphabet == substr(df.nlt$initial_r[df.nlt$vpncode == in_participant], in_letter, in_letter)) 
    # replace initial's assessment with NA in the noninitial section, rendering only noninitial assessments
    df.nlt[df.nlt$vpncode == in_participant,julia+28] <- NA
  }
}


# calculate individual noninitial baselines
df.nlt <- df.nlt %>%
  mutate(mean_noni = rowMeans(select(., nlt_1_noni:nlt_26_noni), na.rm = T)) %>%
  # substract baseline score from all assessments (including initials)
  mutate_at(.funs = list(sub = ~. - mean_noni), .vars = vars(nlt_1:nlt_26))


# calculate interindividual baseline scores
df.baseline <- select(df.nlt, nlt_1_sub:nlt_26_sub)
letter_baseline <- colMeans(df.baseline, na.rm = T)


# calculate Initial Preference Scores and NLT Score
###initialize columns in tibble
df.nlt$ipf1 = NA
df.nlt$ipf2 = NA
df.nlt$ipf3 = NA
df.nlt$ipf4 = NA
df.nlt$ipf5 = NA
df.nlt$score = NA 
### loop over participants
for (in_participant in df.nlt$vpncode){
  ipf = rep(NA, times=5) #initialize variable
  if (is.na(df.nlt$initial_r[df.nlt$vpncode == in_participant]) | df.nlt$initial_r[df.nlt$vpncode == in_participant] == -99 | df.nlt$initial_r[df.nlt$vpncode == in_participant] == ""){
    next
  }
  for (in_letter in 1:nchar(df.nlt$initial_r[df.nlt$vpncode == in_participant])){
    # for every single initial get the position number in the alphabet
    julia <- which(alphabet == substr(df.nlt$initial_r[df.nlt$vpncode == in_participant], in_letter, in_letter))
    # calculate Initial Preference Score as initial's assessment minus interindividual baseline score
    ipf[in_letter] <- as.numeric(df.nlt[df.nlt$vpncode == in_participant, julia+1]) - as.numeric(letter_baseline[julia])
  }
  # add IPF scores to tibble
  df.nlt$ipf1[df.nlt$vpncode == in_participant] <- ipf[1]
  df.nlt$ipf2[df.nlt$vpncode == in_participant] <- ipf[2]
  df.nlt$ipf3[df.nlt$vpncode == in_participant] <- ipf[3]
  df.nlt$ipf4[df.nlt$vpncode == in_participant] <- ipf[4]
  df.nlt$ipf5[df.nlt$vpncode == in_participant] <- ipf[5]
  # average from all Initial Preference Scores of one participant is their NLT score
  df.nlt$score[df.nlt$vpncode == in_participant] <- mean(ipf, na.rm = T)
}

write.csv(df.nlt, "2_preprocessed-data/nlt.csv", row.names = F)