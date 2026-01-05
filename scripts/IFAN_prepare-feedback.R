# ----------------------------------------------------------
# Preprocesses data for feedback to individual participants in the IFAN project. 
# (c) Luca A. Naudszus, Zurich, 30.09.2023
# Institute for Psychology, University of Duisburg-Essen, Essen
# ----------------------------------------------------------
#
#
# Input: 
### bfi.csv, contains BFI-10
### neo.csv, contains NEO
### rses.csv, contains RSES
### iso.csv, contains isolation items
### huo.csv, contains sociometer items
### iri.csv, contains IRI items
### initial.csv, contains other important items from the initial inquiry
### final.csv, contains other important items from the final inquiry
#-----------
# Calculates z-scores and percentiles for all participants. 
#-----------
#-----------
# Output: 
### all.csv, containing all necessary scales
### scales.csv, containing all z-scored scales
### pnorms.csv, containing all percentiles for all scales
### julia.csv, containing feedback only for HI05MR and LUCA02
# ---------------------------------------------------------

rm(list=ls())
setwd("C://Users//Luca Naudszus//sciebo//Imaginary Friends at Night//Traumstudie_2023")
### load packages and sources
library(plyr)
library(tidyverse)
source("scripts//IFAN_key.R")
### set working directory
setwd("data")
# load data
df.bfi <- read.csv(list.files(path = "2_preprocessed-data", pattern = 'bfi', full.names = T))
df.neo <- read.csv(list.files(path = "2_preprocessed-data", pattern = 'neo', full.names = T))
df.rses <- read.csv(list.files(path = "2_preprocessed-data", pattern = 'rses', full.names = T))
df.iso <- read.csv(list.files(path = "2_preprocessed-data", pattern = 'iso.csv', full.names = T))
df.huo <- read.csv(list.files(path = "2_preprocessed-data", pattern = 'huo', full.names = T))
df.iri <- read.csv(list.files(path = "2_preprocessed-data", pattern = 'iri', full.names = T))
df.npi <- read.csv(list.files(path = "2_preprocessed-data", pattern = "npi", full.names = T))
df.hsns <- read.csv(list.files(path = "2_preprocessed-data", pattern = "hsns", full.names = T))
df.narq <- read.csv(list.files(path = "2_preprocessed-data", pattern = "narq", full.names = T))
df.iat <- read.csv(list.files(path = "2_preprocessed-data", pattern = "iat", full.names = T))
df.nlt <- read.csv(list.files(path = "2_preprocessed-data", pattern = 'nlt', full.names = T))
colnames(df.nlt)[87] <- 'nlt'
df.madre <- read.csv(list.files(path = "2_preprocessed-data", pattern = 'madre', full.names = T))
df.psqi <- read.csv(list.files(path = "2_preprocessed-data", pattern = "psqi", full.names = T))
df.ema <- read.csv(list.files(path= "2_preprocessed-data", pattern = "within_person", full.names = T))
df.dreams <- read.csv(list.files(path= "2_preprocessed-data", pattern = "dreams_daily", full.names = T))

# preprocess dream data
df.dreams <- df.dreams %>%
  group_by(vpncode) %>%
  dplyr::summarise(
    n_epi = sum(n_epi),
    n_eve = sum(n_eve),
    n_sit = sum(n_sit),
    dreamSocSit = sum(dreamSocSit),
    dreamSocInt = sum(dreamSocInt),
    inter = sum(inter),
    wordcount = sum(wordcount)
  )
df.dreams$dream_pSS <- df.dreams$dreamSocSit / df.dreams$n_sit
df.dreams$dream_pSI <- df.dreams$dreamSocInt / df.dreams$dreamSocSit


# merge all into one df
list <- list(df.bfi, df.neo, df.rses, df.iso, df.huo, df.iri, df.npi, df.hsns, df.narq, df.iat, df.nlt, df.madre, df.psqi, df.ema, df.dreams)
df.all <- join_all(list, type = "full", by = "vpncode")

# remove pilots
pilots <- as_tibble(read.csv2(list.files(path = "1_raw-data", pattern = "pilots", full.names = T)))
pilots <- pilots[pilots != "LUCA02"]
df.all <- df.all[!(df.all$vpncode %in% pilots),]

# get scales into one df
df.scales <- df.all %>%
  select(vpncode, bfi_o, bfi_c, bfi_e, bfi_a, bfi_n, open, cons, xtra, agre, neur, rses, isolation, sociometer, 
         iri_ec, iri_pt, iri_f, iri_pd, gnpi_la, gnpi_ge, gnpi_ee, gnpi, hsns, narq, narq_adm, narq_riv, dscore, 
         nlt, madre_1_start, madre_2_start, madre_2weeks_1, madre_2weeks_2, madre_1_end, madre_2_end, psqi_total, 
         socSit, socInt, excl, rses_total, compliance, duration, n_epi, n_eve, n_sit, dreamSocSit, dreamSocInt, 
         inter, wordcount, dream_pSS, dream_pSI)

# z-score all variables in df except for code
k = dim(df.scales)[2]
df.scales[,2:k] <- scale(df.scales[,2:k])
df.pnorm <- df.scales
df.pnorm[,2:k] <- pnorm(as.matrix(df.pnorm[,2:k]))

# drop all except for HI05MR and LUCA02
df.julia <- df.scales[df.scales$vpncode == "HI05MR" | df.scales$vpncode == "LUCA02",]

# save data
write.csv(df.all, '4_feedback/data/all.csv', row.names = F)
write.csv(df.scales, '4_feedback/data/scales.csv', row.names = F)
write.csv(df.pnorm, '4_feedback/data/pnorms.csv', row.names = F)
write.csv(df.julia, '4_feedback/julia.csv', row.names = F)