# ----------------------------------------------------------
# Principal component analysis of Hypersensitive Narcissism Scale
# (c) Luca A. Naudszus, Essen, 26.03.2024
# Institute for Psychology, University of Duisburg-Essen, Essen
# ----------------------------------------------------------
#
#
# Input: 
### df.npi
#-----------
# Principal component analysis of Hypersensitive Narcissism Scale (HSNS)
# Extraction of factors
# Calculation of factor loading for all participants
#-----------
# !!! Is not implemented yet !!!
#-----------
# Output: 
### currently none
# ---------------------------------------------------------
rm(list=ls())
# Settings: 
use_poor_slpq = T #include participants with poor PSQI scores?
use_all = F #include all participants?
rootpath = "C://Users//Luca Naudszus//sciebo//Imaginary Friends at Night//Traumstudie_2023"
# ---------------------------------------------------------

# ---------------------------------------------------------
# preparations
### load packages
library(tidyverse)

### clear memory, set working directory
setwd(rootpath)

### load data
df.hsns <- read.csv(file.path("data", "2_preprocessed-data", "hsns.csv"))
excluded_participants <- read.csv(list.files(path = file.path("data", "2_preprocessed-data"), pattern = 'excluded', full.names = T))

### exclude participants
if (use_poor_slpq){
  excluded_participants <- excluded_participants[excluded_participants$reason != "Sleep Quality (PSQI)",]
}
if (use_all){
  excluded_participants <- excluded_participants[excluded_participants$reason == "Piloting participants",]
}
df.npi <- df.npi[!(df.npi$vpncode %in% excluded_participants$participant),]
# ---------------------------------------------------------

mod1 <- '
NPI 
'
