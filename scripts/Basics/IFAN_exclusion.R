# ----------------------------------------------------------
# Exclusion of participants in the IFAN project. 
# (c) Luca A. Naudszus, Essen, 24.08.2023
# Institute for Psychology, University of Duisburg-Essen, Essen
# ----------------------------------------------------------
#
#
# Input: 
### export of initial inquiry as csv
### export of final inquiry as csv (with manually corrected PSQI items)
#-----------
# Creates list of participants meeting and failing to meet the inclusion criteria.
# !!! Calculation of PSQI scores seems problematic. A large number of participants fulfills the exclusion criterion for sleep quality. Why? !!!
#-----------
#-----------
# Output: 
### df_excluded.csv, table containing all excluded participants
### included_participants.csv, list containing all included participants
# ---------------------------------------------------------

# --- initialize

rm(list=ls())

### settings
criteria_not_applied <- c("antidepressiva", "benzodiazepine", 
                          "insomnia", "Sleep Quality (PSQI)")

### load packages and sources
library(tidyverse)
source(file.path("scripts", "basics", "IFAN_key.R"))

### load data
pilots <- pull(read.csv2(
  list.files(path = file.path("data", "1_raw-data"), 
             pattern = "pilots", full.names = T)))

df_initial <- as_tibble(read.csv2(
  list.files(path = file.path("data", "1_raw-data", "1_initial-data"), 
             full.names = T))) %>%
  select(c(age:ema, date_of_last_access)) %>%
  rename(id = vpncode) %>% key(T) %>% filter(!(id %in% pilots))

df_final <- as_tibble(read.csv2(
  list.files(path = file.path("data", "1_raw-data", "3_final-data"), 
             full.names = T))) %>%
  select(c(vpncode:password, date_of_last_access)) %>%
  rename(id = vpncode) %>% key(T) %>% filter(!(id %in% pilots))

df_psqi <- as_tibble(read.csv(
  list.files(path = file.path("data", "2_preprocessed-data", "meta"), 
             pattern = "psqi", full.names = T)))

df_ema_person <- as_tibble(read.csv(
  list.files(path = file.path("data", "2_preprocessed-data"), 
             pattern = "ema_person", full.names = T))) #TODO: Where does this length come from?

### initialize variables
df_excluded = tibble(id = character(), reason = character())

# --- exclude piloting participants
pilots_table <- cbind(pilots, rep('Piloting participants', 
                                  length = length(pilots)))
colnames(pilots_table) <- c('id', 'reason')
df_excluded = rbind(df_excluded, pilots_table) # add to exclusion list
colnames(df_excluded) <- c('id', 'reason')

# --- age
if (any(df_initial$age < 18)) {
  age_exclusions <- cbind(df_initial$id[df_initial$age < 18], rep('age'))
  colnames(age_exclusions) <- c('id', 'reason')
  df_excluded = rbind(df_excluded, age_exclusions) # add to exclusion list
}

# --- Pittsburgh Sleep Quality Index, im Codebuch und dann hier korrigieren!
### exclude participants due to suspicious PSQI values
psqi_exclusions <- c(df_psqi$id[df_psqi$psqi_total > 5 & !is.na(df_psqi$psqi_total)])
psqi_excl_table <- cbind(psqi_exclusions, rep('Sleep Quality (PSQI)', length = length(psqi_exclusions)))
colnames(psqi_excl_table) <- c('id', 'reason')
df_excluded <- rbind(df_excluded, psqi_excl_table)

# --- sleep disorders
#TODO: Account for multiple sleep disorders
df_sleepdis <- df_final %>%
  select(id, sleep_disorder_1:sleep_disorder_10) %>%
  mutate_if(is.integer, as.numeric) %>%
  mutate(sleepdis_existence = sign(rowSums(select(., c(sleep_disorder_1:sleep_disorder_8)))), #is there any sleep disorder?
         #create one variable containing specific sleep disorder, if appropriate (attention: does not account for multiple diagnoses)
         which = case_when( 
           sleep_disorder_1 == 1 ~ "insomnia",
           sleep_disorder_2 == 1 ~ "narcolepsia",
           sleep_disorder_3 == 1 ~ "nightmare disorder",
           sleep_disorder_4 == 1 ~ "somnambulism",
           sleep_disorder_5 == 1 ~ "night terror",
           sleep_disorder_6 == 1 ~ "sleep apnea", 
           sleep_disorder_7 == 1 ~ "REM sleep disorder", 
           sleep_disorder_8 == 1 ~ sleep_disorder_9,
           sleep_disorder_10 == 1 ~ "no sd"
         ))
### look for participants that claim both to have and not to have a sleep disorder
df_sleepdis$id[sign(df_sleepdis$sleep_disorder_10 + df_sleepdis$sleepdis_existence - 1) == 1]

### manually checked participants: CR01TO, DN01SE, SN01WL have no diagnosed sleep disorder
manually_checked <- c("CR10TO", "DN01SE", "DN09BR", "SN01WL")
df_sleepdis$sleepdis_existence[df_sleepdis$id %in% manually_checked] <- 0

### exclude participants due to sleep disorders
sleep_excl_table <- cbind(df_sleepdis$id[df_sleepdis$sleepdis_existence == 1], df_sleepdis$which[df_sleepdis$sleepdis_existence == 1])
colnames(sleep_excl_table) <- c('id', 'reason')
df_excluded <- rbind(df_excluded, sleep_excl_table)


# --- psychiatric disorders
#TODO: Account for multiple psychiatric disorders
df_psychdis <- df_final %>%
  select(id, psych_disorder_1:psych_disorder_7) %>%
  mutate_if(is.integer, as.numeric) %>%
  mutate(psychdis_existence = sign(rowSums(select(., c(psych_disorder_1:psych_disorder_4, psych_disorder_6)))), #is there any psychiatric disorder?
         #create one variable containing specific psychiatric disorder, if appropriate (attention: does not account for multiple diagnoses)
         which = case_when( 
           psych_disorder_1 == 1 ~ "depression",
           psych_disorder_2 == 1 ~ "PTSD",
           psych_disorder_3 == 1 ~ "schizophrenia",
           psych_disorder_4 == 1 ~ psych_disorder_5,
           psych_disorder_6 == 1 ~ psych_disorder_7,
           TRUE ~ "no psychiatric disorder"
         ))
### recode missing values
df_psychdis$psychdis_existence[df_psychdis$psychdis_existence == -1] <- NA
df_psychdis$psychdis_existence[df_psychdis$which == "no psychiatric disorder"] <- 0

### manually checked participants: 
### AD09HL, AN02DE, EA03AE, BO08FA, EI06AE, SE01EI, SF06IM, DR02RL, RZ12JN, 
###         TN10HL, UE05TO, MR11HI, AT04IA have no psychiatric disorder
### IG07RL has Borderline PD which is no exclusion criterion
### IN08OE has autism which is no exclusion criterion
### NE01TH, PG11WL have Social Phobia which is no exclusion criterion
### SB12HL has ADHD which is no exclusion criterion
### SS05HR has adaptation disorder which is no exclusion criterion
### EK12AI has panic disorder, maybe include?
### SS08CR has panic disorder and agoraphobia, maybe include?
manually_checked <- c('AD09HL', 'AN02DE', 'EA03AE', 'BO08FA', 'EI06AE', 
                      'IG07RL', 'IN08OE', 'PG11WL', 'SB12HL', 'SE01EI', 
                      'SF06IM', 'DR02RL', 'RZ12JN', 'UE05TO', 'MR11HI', 
                      'AT04IA', 'NE01TH', 'SS05HR')
### look for participants that claim both to have and not to have a psychiatric disorder, zu überarbeiten!
#TODO: Participants who claim to have and not to have a psychiatric disorder
# df_sleepdis$vpncode[sign(df_sleepdis$sleep_disorder_8 + df_sleepdis$sleepdis_existence - 1) == 1]
df_psychdis$psychdis_existence[df_psychdis$id %in% manually_checked] <- 0

### exclude participants due to psychiatric disorders
psych_excl_table <- cbind(
  df_psychdis$id[
    df_psychdis$psychdis_existence == 1 & !is.na(
      df_psychdis$psychdis_existence)], 
  df_psychdis$which[
    df_psychdis$psychdis_existence == 1 & !is.na(
      df_psychdis$psychdis_existence)])
colnames(psych_excl_table) <- c('id', 'reason')
df_excluded <- rbind(df_excluded, psych_excl_table)

# --- medication
#TODO: Account for multiple medication
df_med <- df_final %>%
  select(id, medication_1:medication_10) %>%
  mutate_if(is.integer, as.numeric) %>%
  mutate(med_taking = sign(rowSums(select(., c(medication_1:medication_8)))), #do they take any medication?
         #create one variable containing specific medication, if appropriate (attention: does not account for multiple medication!)
         which = case_when( 
           medication_1 == 1 ~ "benzodiazepine",
           medication_2 == 1 ~ "antidepressiva",
           medication_3 == 1 ~ "clonazepam",
           medication_4 == 1 ~ "flunitrazepam",
           medication_5 == 1 ~ "prazosin",
           medication_6 == 1 ~ "psychotropic substances",
           medication_7 == 1 ~ "z-drugs",
           medication_8 == 1 ~ medication_9,
           medication_10 == 1 ~ "no medication"
         ))
### manually checked participants: 
### AD09DR, AJ10AK, AN09RO, BR02HR, KT04MT, MR11HI, OG01SR, RF04PT, RO12MA, 
###         SL07PT, SS05HR, YO06RL take medication that does not fulfill any exclusion criteria
manually_checked <- c('AD09DR', 'AJ10AK', 'AN09RO', 'BR02HR', 'KT04MT', 
                      'LN07AD', 'MR11HI', 'OG01SR', 'RF04PT', 'RO12MA', 
                      'SL07PT', 'SS05HR', 'YO06RL')
### look for participants that claim both to take and not to take any medication
df_med$id[sign(df_med$medication_10 + df_med$med_taking - 1) == 1] #SM06HN takes medication
df_med$med_taking[df_med$id %in% manually_checked] <- 0

### exclude participants due to medication
med_excl_table <- cbind(
  df_med$id[df_med$med_taking == 1 & !is.na(df_med$med_taking)], 
  df_med$which[df_med$med_taking == 1 & !is.na(df_med$med_taking)])
colnames(med_excl_table) <- c('id', 'reason')
df_excluded <- rbind(df_excluded, med_excl_table)

# --- compliance
low_compliance <- df_ema_person$id[df_ema_person$compliance < .5]
low_compliance_table <- cbind(low_compliance, rep('compliance', length(low_compliance)))
colnames(low_compliance_table) <- c('id', 'reason')
df_excluded <- rbind(df_excluded, low_compliance_table)

# --- no dreams
#TODO: Do not hardcode this. 
no_dream_table <- cbind("DN01SE", "no dreams")
colnames(no_dream_table) <- c('id', 'reason')
df_excluded <- rbind(df_excluded, no_dream_table)

# --- last work
### remove criteria specified in the beginning
df_excluded <- df_excluded[!(df_excluded$reason %in% criteria_not_applied),]

### included participants
df_included <- df_final$id[!(df_final$id %in% df_excluded$id)]

# --- save data
write.csv(df_psqi, 
          file.path("data", '2_preprocessed-data', 'meta', 'psqi.csv'), 
          row.names = F)
write.csv(df_excluded, 
          file.path('data', '2_preprocessed-data', 'meta', 'excluded.csv'), 
          row.names = F)
write.csv(df_included, 
          file.path('data', '2_preprocessed-data', 'meta', 'included.csv'), 
          row.names = F)