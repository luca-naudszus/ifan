# ----------------------------------------------------------
# Preprocessing of EMA data for the IFAN project. 
# (c) Luca A. Naudszus, Essen, 28.08.2023/03.10.2023
# Institute for Psychology, University of Duisburg-Essen, Essen
# ----------------------------------------------------------
#
#
# Input: 
### ema_all.csv, containing all EMA data points for all participants
### ema_daily_all.csv, containing within day EMA data for all participants
### ema_person.csv, containing within person EMA data
#-----------
# Checks for reactivity effects in EMA data in the form of drifts.
#-----------
#-----------
# Output: 
### none
# ---------------------------------------------------------

# initialize
rm(list=ls())
setwd('/Users/lucanaudszus/sciebo/Imaginary Friends at Night/Traumstudie_2023')
### load packages and sources
library(anytime)
library(lme4)
library(tidyverse)
source("scripts/Basics/IFAN_key.R")
setwd("data")

# load data
df.ema <- read.csv2("2_preprocessed-data/ema_all.csv")
df.ema_daily <- read.csv2("2_preprocessed-data/ema_daily_all.csv")
df.ema_person <- read.csv2("2_preprocessed-data/ema_person.csv")
excluded_participants <- read.csv(list.files(path = "2_preprocessed-data", pattern = 'excluded', full.names = T))
included_participants <- read.csv(list.files(path = "2_preprocessed-data", pattern = "included", full.names = T))

# first: exclude participants that did not finalize the study
df.ema <- df.ema[df.ema$vpncode %in% included_participants$x | df.ema$vpncode %in% excluded_participants$participant,]
df.ema_daily <- df.ema_daily[df.ema_daily$vpncode %in% included_participants$x | df.ema_daily$vpncode %in% excluded_participants$participant,]
df.ema_person <- df.ema_person[df.person$vpncode %in% included_participants$x | df.ema_person$vpncode %in% excluded_participants$participant,]

# compliance
### (1a) Random-Intercept only model for compliance
fit.rio <- lmer(compliance~ 1 + (1|vpncode), data = df.daily)
summary(fit.rio)

### (1b) Random-Intercept model for compliance
fit.ri <- lmer(compliance ~ day_counter + (1|vpncode), data = df.daily)
summary(fit.ri)

### (1c) Random-Slopes model for compliance, fails to converge!
fit.rs <- lmer(compliance ~ day_counter + (1 + day_counter|vpncode), data = df.daily)
summary(fit.rs)

# Short Rosenberg Self-Esteem Scale
### (2a) Random-Intercept only model for Rosenberg
fit.rio <- lmer(rses_totalMean~ 1 + (1|vpncode), data = df.daily)
summary(fit.rio)

### (2b) Random-Intercept model for Rosenberg
fit.ri <- lmer(rses_totalMean ~ day_counter + (1|vpncode), data = df.daily)
summary(fit.ri)

### (2c) Random-Slopes model for Rosenberg, fails to converge!
fit.rs <- lmer(rses_totalMean ~ day_counter + (1 + day_counter|vpncode), data = df.daily)
summary(fit.rs)

# Estimated proportion of social situations
### (3a) Random-Intercept only model for EPSS
fit.rio <- lmer(socSit ~ 1 + (1|vpncode), data = df.daily)
summary(fit.rio)

### (3b) Random-Intercept model for EPSS
fit.ri <- lmer(socSit ~ day_counter + (1|vpncode), data = df.daily)
summary(fit.ri)

### (3c) Random-Slopes model for EPSS, fails to converge!
fit.rs <- lmer(socSit ~ day_counter + (1 + day_counter|vpncode), data = df.daily)
summary(fit.rs)

# Estimated proportion of social interactions
### (4a) Random-Intercept only model for EPSI
fit.rio <- lmer(socInt ~ 1 + (1|vpncode), data = df.daily)
summary(fit.rio)

### (4b) Random-Intercept model for EPSI
fit.ri <- lmer(socInt ~ day_counter + (1|vpncode), data = df.daily)
summary(fit.ri)

### (4c) Random-Slopes model for EPSI, fails to converge!
fit.rs <- lmer(socInt ~ day_counter + (1 + day_counter|vpncode), data = df.daily)
summary(fit.rs)

# Exclusion
### (5a) Random-Intercept only model for exclusion
fit.rio <- lmer(excl ~ 1 + (1|vpncode), data = df.daily)
summary(fit.rio)

### (5b) Random-Intercept model for exclusion
fit.ri <- lmer(excl ~ day_counter + (1|vpncode), data = df.daily)
summary(fit.ri)

### (5c) Random-Slopes model for exclusion, fails to converge!
fit.rs <- lmer(excl ~ day_counter + (1 + day_counter|vpncode), data = df.daily)
summary(fit.rs)

# Short Rosenberg Self-Esteem Scale, variance
### (6a) Random-Intercept only model for Rosenberg variance
fit.rio <- lmer(rses_totalSd~ 1 + (1|vpncode), data = df.daily)
summary(fit.rio)

### (6b) Random-Intercept model for Rosenberg variance
fit.ri <- lmer(rses_totalSd ~ day_counter + (1|vpncode), data = df.daily)
summary(fit.ri)

### (6c) Random-Slopes model for Rosenberg variance, fails to converge!
fit.rs <- lmer(rses_totalSd ~ day_counter + (1 + day_counter|vpncode), data = df.daily)
summary(fit.rs)

# Estimated proportion of social situations, variance
### (7a) Random-Intercept only model for EPSS variance
fit.rio <- lmer(socSitSd ~ 1 + (1|vpncode), data = df.daily)
summary(fit.rio)

### (7b) Random-Intercept model for EPSS variance
fit.ri <- lmer(socSitSd ~ day_counter + (1|vpncode), data = df.daily)
summary(fit.ri)

### (7c) Random-Slopes model for EPSS variance, fails to converge!
fit.rs <- lmer(socSitSd ~ day_counter + (1 + day_counter|vpncode), data = df.daily)
summary(fit.rs)

# Estimated proportion of social interactions, variance
### (8a) Random-Intercept only model for EPSI variance
fit.rio <- lmer(socIntSd ~ 1 + (1|vpncode), data = df.daily)
summary(fit.rio)

### (8b) Random-Intercept model for EPSI variance
fit.ri <- lmer(socIntSd ~ day_counter + (1|vpncode), data = df.daily)
summary(fit.ri)

### (8c) Random-Slopes model for EPSI variance, fails to converge!
fit.rs <- lmer(socIntSd ~ day_counter + (1 + day_counter|vpncode), data = df.daily)
summary(fit.rs)

# Exclusion, variance
### (9a) Random-Intercept only model for exclusion variance
fit.rio <- lmer(exclSd ~ 1 + (1|vpncode), data = df.daily)
summary(fit.rio)

### (9b) Random-Intercept model for exclusion variance
fit.ri <- lmer(exclSd ~ day_counter + (1|vpncode), data = df.daily)
summary(fit.ri)

### (9c) Random-Slopes model for exclusion variance, fails to converge!
fit.rs <- lmer(exclSd ~ day_counter + (1 + day_counter|vpncode), data = df.daily)