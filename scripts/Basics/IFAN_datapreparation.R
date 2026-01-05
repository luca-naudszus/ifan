# ----------------------------------------------------------
# Final data preprocessing for the IFAN project. 
# (c) Luca A. Naudszus, Essen, 22.08.2023
# Institute for Psychology, University of Duisburg-Essen, Essen
# ----------------------------------------------------------
#
#
# Input: 
### export of initial inquiry as csv
### export of final inquiry as csv (with manually corrected PSQI items)
#-----------
# Prepares questionnaire data and data from open questions for all participants that finalized the study. 
#-----------
# !!! Calculation of NPI scores does not work !!!
# !!! Check duplicates of final data. !!! 
# !!! CN10MC and SB01MC exist twice. Check for correct usage of initial data. !!!
#-----------
# Output: 
### bfi.csv, contains BFI-10
### neo.csv, contains NEO
### rses.csv, contains RSES
### iso.csv, contains isolation items
### huo.csv, contains sociometer items
### iri.csv, contains IRI 
### npi.csv, contains NPI
### hsns.csv, contains HSNS
### narq.csv, contains NARQ
### madre.csv, contains items from MADRE
### initial.csv, contains other important items from the initial inquiry
### final.csv, contains other important items from the final inquiry
# ---------------------------------------------------------

# initialize

rm(list=ls())
### load packages and sources
library(tidyverse)
source(file.path("scripts", "basics", "IFAN_key.R"))

### variables
pilots <- pull(read.csv2(
  list.files(path = file.path("data", "1_raw-data"), 
             pattern = "pilots", full.names = T)))

### load data
df_initial <- as_tibble(read.csv2(
  list.files(path = file.path("data", "1_raw-data", "1_initial-data"), 
             full.names = T))) %>%
  select(c(age:ema, date_of_last_access)) %>%
  rename(madre_1_start = MADRE_1, madre_2_start = MADRE_2, MRD_start = MRD, 
         MRD_age_start = MRD_age, initial_date = date_of_last_access,
         id = vpncode) %>% key(T) %>% filter(!(id %in% pilots))

df_final <- as_tibble(read.csv2(
  list.files(path = file.path("data", "1_raw-data", "3_final-data"), 
             full.names = T))) %>%
  select(c(vpncode:password, date_of_last_access)) %>%
  rename(madre_1_end = madre_1, madre_2_end = madre_2, 
         MRD_end = most_dream_1, MRD_age_end = most_dream_2,
         id = vpncode) %>% key(T) %>% filter(!(id %in% pilots))

# preprocess data

### demographics
df_demo <- df_initial %>% select(c(id, age:study_programme)) 

### Big Five Inventory-10 
reversed_cols <- c("bfi_1", "bfi_3", "bfi_4", "bfi_5", "bfi_7")
df_bfi <- df_initial %>%
  select(id, bfi_1:bfi_10) %>%
  mutate_at(vars(all_of(reversed_cols)), ~ 6 - .) %>% #recode items
  mutate(bfi_e = (bfi_1 + bfi_6)/2, bfi_n = (bfi_4 + bfi_9)/2, #compute scales
         bfi_o = (bfi_5 + bfi_10)/2, bfi_c = (bfi_3 + bfi_8)/2, 
         bfi_a = (bfi_2 + bfi_7)/2)

### NEO-Five Factor Inventory
reversed_cols <- c("neo_1", "neo_3", "neo_8", "neo_9", "neo_12", "neo_14", "neo_15", "neo_16", "neo_18", "neo_23", "neo_24", 
                   "neo_27", "neo_29", "neo_30", "neo_31", "neo_33", "neo_38", "neo_39", "neo_42", "neo_44", "neo_45", 
                   "neo_46", "neo_48", "neo_54", "neo_55", "neo_57","neo_59")
df_neo <- df_final %>%
  select(id, neo_1:neo_60) %>%
  mutate_at(vars(starts_with("neo_")), ~na_if(., 0)) %>%
  mutate_at(vars(all_of(reversed_cols)), ~ 6 - .) %>% #recode items
  mutate(neur = rowMeans(select(., c(neo_1, neo_6, neo_11, neo_16, neo_21, neo_26, neo_31, neo_36, neo_41, neo_46, neo_51, neo_56)), na.rm = T), #compute scales
         xtra = rowMeans(select(., c(neo_2, neo_7, neo_12, neo_17, neo_22, neo_27, neo_32, neo_37, neo_42, neo_47, neo_52, neo_57)), na.rm = T), 
         open = rowMeans(select(., c(neo_3, neo_8, neo_13, neo_18, neo_23, neo_28, neo_33, neo_38, neo_43, neo_48, neo_53, neo_58)), na.rm = T),
         agre = rowMeans(select(., c(neo_4, neo_9, neo_14, neo_19, neo_24, neo_29, neo_34, neo_39, neo_44, neo_49, neo_54, neo_59)), na.rm = T), 
         cons = rowMeans(select(., c(neo_5, neo_10, neo_15, neo_20, neo_25, neo_30, neo_35, neo_40, neo_45, neo_50, neo_55, neo_60)), na.rm = T))

### Rosenberg Self-Esteem Questionnaire
reversed_cols <- c("rses_2", "rses_5", "rses_6", "rses_8", "rses_9")
df_rses <- df_final %>%
  select(id, rses_1:rses_10) %>%
  mutate_at(vars(all_of(reversed_cols)), ~ 8 - .) %>% #recode items
  mutate(rses = rowMeans(select(., rses_1:rses_10), na.rm = T)) #compute scales
  
### Isolation items
df_iso <- df_final %>%
  select(id, iso_1:iso_3) %>%
  mutate(isolation = rowMeans(select(., iso_1:iso_3))) #compute total score

### Sociometer (Huo et al.)
df_huo <- df_final %>%
  select(id, iso_4:iso_7) %>%
  mutate(sociometer = rowMeans(select(., iso_4:iso_7))) #compute total score

### Interpersonal Reactivity Index (Saarbruecker Persoenlichkeitsfragebogen)
df_iri <- df_final %>%
  select(id, iri_1:iri_16) %>%
  mutate(iri_ec = rowMeans(select(., c(iri_1, iri_5, iri_9, iri_11))), #compute emotional concern score
         iri_pt = rowMeans(select(., c(iri_4, iri_10, iri_14, iri_16))), #compute perspective taking score
         iri_f = rowMeans(select(., c(iri_2, iri_7, iri_12, iri_15))), #compute fantasy score
         iri_pd = rowMeans(select(., c(iri_3, iri_6, iri_8, iri_13)))) #compute personal distress score

### Narcissistic Personality Inventory
df_npi <- df_final %>%
  select(id, npi_1:npi_13) %>%
  mutate(npi_1r = case_when(npi_1 == 1 ~ 1, npi_1 == 2 ~ 0, T ~ NA),
    npi_2r = case_when(npi_2 == 1 ~ 1, npi_2 == 2 ~ 0, T ~ NA),
    npi_3r = case_when(npi_3 == 1 ~ 0, npi_3 == 2 ~ 1, T ~ NA),
    npi_4r = case_when(npi_4 == 1 ~ 1, npi_4 == 2 ~ 0, T ~ NA),
    npi_5r = case_when(npi_5 == 1 ~ 0, npi_5 == 2 ~ 1, T ~ NA),
    npi_6r = case_when(npi_6 == 1 ~ 0, npi_6 == 2 ~ 1, T ~ NA),
    npi_7r = case_when(npi_7 == 1 ~ 0, npi_7 == 2 ~ 1, T ~ NA),
    npi_8r = case_when(npi_8 == 1 ~ 0, npi_8 == 2 ~ 1, T ~ NA),
    npi_9r = case_when(npi_9 == 1 ~ 1, npi_9 == 2 ~ 0, T ~ NA),
    npi_10r = case_when(npi_10 == 1 ~ 1, npi_10 == 2 ~ 0, T ~ NA),
    npi_11r = case_when(npi_11 == 1 ~ 1, npi_11 == 2 ~ 0, T ~ NA),
    npi_12r = case_when(npi_12 == 1 ~ 1, npi_12 == 2 ~ 0, T ~ NA),
    npi_13r = case_when(npi_13 == 1 ~ 1, npi_13 == 2 ~ 0, T ~ NA),
  ) %>%
  mutate(gnpi_la = rowMeans(select(., c(npi_1r:npi_4r))), #leadership / authority
         gnpi_ge = rowMeans(select(., c(npi_5r:npi_9r))), #grandiose exhibitionism
         gnpi_ee = rowMeans(select(., c(npi_10r:npi_13r))), #entitlement/exploitativeness
         gnpi = rowMeans(select(., c(npi_1r:npi_13r)))) #total score
### Hypersensitive Narcissism Scale
df_hsns <- df_final %>%
  select(id, hsns_1:hsns_10) %>%
  mutate(hsns = rowMeans(select(., c(hsns_1:hsns_10))))
### Narcissism Admiration and Rivalry Questionnaire, items are wrongly numbered in Unipark
df_narq <- df_final %>%
  select(id, narq_1:narq_21) %>%
  mutate(narq = rowMeans(select(., c(narq_1:narq_21))),
         narq_adm = rowMeans(select(., c(narq_1:narq_3, narq_5, narq_8, 
                                         narq_9, narq_18, narq_19, narq_21))),
         narq_riv = rowMeans(select(., c(narq_4, narq_6, 
                                         narq_10:narq_15, narq_20))))

### Mannheim Dream Questionnaire
df_madre1 <- df_initial %>%
  select(id, madre_1_start, madre_2_start) %>%
  mutate(madre_2_start = 8 - madre_2_start)
df_madre2 <- df_final %>%
  select(id, madre_1_end, madre_2_end, madre_2weeks_1, madre_2weeks_2) %>%
  mutate(across(c(madre_1_end, madre_2weeks_1), ~ 6 - .)) %>%
  mutate(madre_2weeks_2 = 5 - madre_2weeks_2,
         madre_2_end = 9 - madre_2_end)
df_madre <- inner_join(df_madre2, df_madre1, by = "id")

### Most Recent Dream
#TODO: Add preprocessing for most recent dream

### other items from initial inquiry: SISES and EMA time
df_initial_rest <- select(df_initial, c(id, SISES, ema))

### final information + open questions
### (dream recall, hypotheses, events, postal code, feedback, advertising, evaluation, label, password)
df_final_rest <- select(df_final, c(id, recall:password))

# save data
write.csv(df_demo, 
          file.path("data", "2_preprocessed-data", 
                    "demographics-initial.csv"), row.names = F)
write.csv(df_bfi, 
          file.path("data", "2_preprocessed-data", "personality", 
                    "bfi.csv"), row.names = F)
write.csv(df_neo, 
          file.path("data", "2_preprocessed-data", "personality", 
                    "neo.csv"), row.names = F)
write.csv(df_rses, 
          file.path("data", "2_preprocessed-data", "personality", 
                    "rses.csv"), row.names = F)
write.csv(df_iso, 
          file.path("data", "2_preprocessed-data", "personality", 
                    "isolation.csv"), row.names = F)
write.csv(df_huo, 
          file.path("data", "2_preprocessed-data", "personality", 
                    "huo.csv"), row.names = F)
write.csv(df_iri, 
          file.path("data", "2_preprocessed-data", "personality", 
                    "iri.csv"), row.names = F)
write.csv(df_npi, 
          file.path("data", "2_preprocessed-data", "personality", 
                    "npi.csv"), row.names = F)
write.csv(df_hsns, 
          file.path("data", "2_preprocessed-data", "personality", 
                    "hsns.csv"), row.names = F)
write.csv(df_narq, 
          file.path("data", "2_preprocessed-data", "personality", 
                    "narq.csv"), row.names = F)
write.csv(df_madre, 
          file.path("data", "2_preprocessed-data", 
                    "madre.csv"), row.names = F)
write.csv(df_initial_rest, 
          file.path("data", "2_preprocessed-data", 
                    "sises-ema.csv"), row.names = F)
write.csv(df_final_rest, 
          file.path("data", "2_preprocessed-data", 
                    "final_rest.csv"), row.names = F)