# ----------------------------------------------------------
# Creates sample description for the IFAN project. 
# (c) Luca A. Naudszus, Zurich, 12.09.2023
# Institute for Psychology, University of Duisburg-Essen, Essen
# ----------------------------------------------------------
#
#
# Input: 
### demographics.csv, containing all demographic information for all participants at intake
### included_participants.csv, containing vpncodes of all included participants
#-----------
# Gives an extensive description of the sample at the beginning and at the end of the study.
# Checks for differences between participants who finalized the study and those who dropped out.
# Correlations correlation coefficients between short and long scales.
#-----------
# !!! TODO: Account for participants that were expelled rather than dropped out. !!!
#-----------
# Output: 
### unknown
# ---------------------------------------------------------

# initialize
rm(list=ls())

### load packages and sources
library(car)
library(plyr)
library(psych)
library(tidyverse)
#TODO: load package for chi-squared post-hoc test
source(file.path("scripts", "basics", "IFAN_key.R"))

### custom functions
load_file <- function(pathname, pattern) {
  if (is.null(pathname)) {
    df = read.csv(list.files(path = file.path("data", "2_preprocessed-data"),
                             pattern = pattern, full.names = TRUE)) %>%
      as_tibble()
    } else {
    df = read.csv(list.files(path = file.path("data", "2_preprocessed-data", pathname),
                           pattern = pattern, full.names = TRUE)) %>%
      as_tibble()
    }
  if (!("id" %in% names(df))) {
    if ("vpncode" %in% names(df)) {
      df <- df %>% dplyr::rename(id = vpncode)
    } else if ("participant" %in% names(df)) {
      df <- df %>% dplyr::rename(id = participant)
    } else if ("x" %in% names(df)) {
      df <- df %>% dplyr::rename(id = x)
    }
  } 
  df <- df %>% filter(!(id %in% pilots)) %>% key(TRUE)
}

### set working directory

### initialize variables
use_poor_slpq = T #include participants with poor PSQI scores?
pilots <- c('ZZZZZZ', 'ZZXXZZ', '-99', 'LUCA02', 'XX05XX', 'ZZ99ZZ', 'XX88XX')

### load data
df_demo <- load_file("meta", "demographics-initial")
df_bfi <- load_file("personality", "bfi")
df_neo <- load_file("personality", "neo")
df_rses <- load_file("personality", "rses")
df_initial <- load_file(pathname = NULL, "sises-ema")
df_final <- load_file(pathname = NULL, "final_rest")
df_excluded <- load_file("meta", "excluded") 
df_included <- load_file("meta", "included") 
df_ema_person <- load_file(pathname = NULL, "ema_person")
df_counts <- load_file("meta", "counts") 

### exclude participants
if (use_poor_slpq){
  df_excluded <- df_excluded[df_excluded$reason != "Sleep Quality (PSQI)",]
}
df_compliance <- df_excluded[df_excluded$reason == "compliance",]
df_excluded <- df_excluded[df_excluded$reason != "compliance",]

df_demofinal <- df_demo[!(df_demo$id %in% df_excluded$id),]
df_dropout <- join_all(list(df_demo, df_bfi, df_initial), 
                       type = "inner", by = "id") %>%
  mutate(do = factor(case_when(
    id %in% df_included$id ~ "included", # participants who stayed
    id %in% df_excluded$id ~ "excluded", # participants who stayed but were excluded
    !(id %in% df_ema_person$id) & !(id %in% c(df_included$id, df_excluded$id, df_compliance$id)) ~ "before journalling/EMA", # participants dropped out before EMA
    id %in% df_counts$id[df_counts$n < 14] & !(id %in% df_final$id) ~ "during EMA", # dropped out during EMA
    id %in% df_counts$id[df_counts$n >= 14] & !(id %in% df_final$id) ~ "after EMA", # dropped out after EMA
    !(id %in% df_counts$id) ~ "before journalling/EMA",
    id %in% df_compliance$id ~ "compliance", # participants with too low EMA compliance
    id == "UE06UR" ~ "excluded",
    TRUE ~ "unknown"
  ), levels = c("included", "excluded", "compliance", 
                "after EMA", "during EMA", "before journalling/EMA", "unknown")))

df_demofinal <- inner_join(df_demofinal, df_final, by = "id") %>%
  filter(id %in% df_included$id)

# describe final sample
describe(df_demofinal$age)
table(df_demofinal$gender)
table(df_demofinal$education)
table(df_demofinal$occupation)

# test for differences between dropped out participants and those that stayed
### age
describeBy(df_dropout$age,df_dropout$do)
leveneTest(df_dropout$age~df_dropout$do)
bartlett.test(df_dropout$age~df_dropout$do)
summary(aov(df_dropout$age~df_dropout$do))
### gender
table_gender <- with(df_dropout %>% filter(gender != 3), table(do, gender))
table_gender <- table_gender[rowSums(table_gender) > 0, ]
chisq.test(table_gender)
chisq.posthoc.test(table_gender)
### education
table(df_dropout$education,df_dropout$do)
chisq.test(table(df_dropout$education,df_dropout$do), simulate.p.value = T)
### occupation
table(df_dropout$occupation,df_dropout$do)
chisq.test(table(df_dropout$occupation,df_dropout$do), simulate.p.value = T)
### openness for experience
describeBy(df_dropout$bfi_o,df_dropout$do)
leveneTest(df_dropout$bfi_o~df_dropout$do)
bartlett.test(df_dropout$bfi_o~df_dropout$do)
summary(aov(df_dropout$bfi_o~df_dropout$do))
TukeyHSD(aov(df_dropout$bfi_o~df_dropout$do))
### conscientiousness
describeBy(df_dropout$bfi_c,df_dropout$do)
leveneTest(df_dropout$bfi_c~df_dropout$do)
bartlett.test(df_dropout$bfi_c~df_dropout$do)
summary(aov(df_dropout$bfi_c~df_dropout$do))
TukeyHSD(aov(df_dropout$bfi_c~df_dropout$do))
### extraversion
describeBy(df_dropout$bfi_e,df_dropout$do)
leveneTest(df_dropout$bfi_e~df_dropout$do)
bartlett.test(df_dropout$bfi_e~df_dropout$do)
summary(aov(df_dropout$bfi_e~df_dropout$do))
### agreeableness
describeBy(df_dropout$bfi_a,df_dropout$do)
leveneTest(df_dropout$bfi_a~df_dropout$do)
bartlett.test(df_dropout$bfi_a~df_dropout$do)
summary(aov(df_dropout$bfi_a~df_dropout$do))
### neuroticism
describeBy(df_dropout$bfi_n,df_dropout$do)
leveneTest(df_dropout$bfi_n~df_dropout$do)
bartlett.test(df_dropout$bfi_n~df_dropout$do)
summary(aov(df_dropout$bfi_n~df_dropout$do))
TukeyHSD(aov(df_dropout$bfi_n~df_dropout$do))
### single-item self-esteem scale
describeBy(df_dropout$SISES,df_dropout$do)
leveneTest(df_dropout$SISES~df_dropout$do)
bartlett.test(df_dropout$SISES~df_dropout$do)
summary(aov(df_dropout$SISES~df_dropout$do))
### Sport, insb. Kampfsport  
#TODO: Add sport

# correlations of SISES with RSES and BFI with NEO-FFI
df_cor <- join_all(list(df_bfi, df_sises_ema, df_neo, df_rses), type = "inner", by = "vpncode")
cor.test(df_cor$SISES, df_cor$rses)
cor.test(df_cor$bfi_o, df_cor$open)
cor.test(df_cor$bfi_c, df_cor$cons)
cor.test(df_cor$bfi_e, df_cor$xtra)
cor.test(df_cor$bfi_a, df_cor$agre)
cor.test(df_cor$bfi_n, df_cor$neur)

# save data
write.csv(df_demofinal, file.path('2_preprocessed-data', 'meta', 'demographics-final.csv'), row.names = F)