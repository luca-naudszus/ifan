# ----------------------------------------------------------
# Analyses intranocturnal drifts in dream content. 
# (c) Luca A. Naudszus, Zurich, 01.10.2023
# Institute for Psychology, University of Duisburg-Essen, Essen
# ----------------------------------------------------------
#
#
# Input: 
### alldreams.csv, containing dream data on situation level
#-----------
# Checks for intranocturnal drift effects of social content
# Cannot yet check for intranocturnal drifts in wordcount and quality
#-----------
#-----------
# Output: 
### 
# ---------------------------------------------------------

# initialize ----------------------------------------------
rm(list=ls())
setwd("C://Users//lanau//sciebo//Imaginary Friends at Night//Traumstudie_2023")
### load packages and sources
library(lme4)
library(ordinal)
library(tidyverse)
source("scripts//IFAN_key.R")
### set working directory
setwd("data")
### load data
df.alldreams <- as_tibble(read.csv(list.files(path = '2_preprocessed-data', pattern = 'alldreams', full.names = T)))

# add variables
df.alldreams <- df.alldreams %>%
  group_by(vpncode, day) %>%
  mutate(situation_counter = cumsum(c(F, situation[-1]!=situation[-n()]))) %>% #counter
  mutate(interaction = (case_when(
    type >= 3 ~ 1,
    T ~ 0
  ))) %>% #interaction
  mutate(multi = (case_when(
    type >= 4 ~ 1,
    T ~ 0
  ))) %>% # multi I
  mutate(realMulti = (case_when(
    type == 5 ~ 1,
    T ~ 0
  ))) %>% # multi II
  mutate(intcor = (case_when(
    type == 3 ~ 1,
    type == 5 ~ 1,
    T ~ 0
  ))) %>% # multi III
  mutate(quality_simple = case_when(
    quality %/% 10 == 1 ~ 1,
    quality %/% 10 == 2 ~ -1,
    quality == 31 ~ 1,
    quality == 32 ~ -1,
    quality == 1 ~ 1,
    quality == 2 ~ -1,
    T ~ 0
  ))

#######################
# FIT MULTILEVEL MODELS
#######################

# using social situations as outcome
### (1a) Random-Intercept logistic regression model using time as predictor
fit.socsit <- glmer(as.factor(social)~situation_counter+(1|vpncode/day), data=df.alldreams, family = binomial, na.action=na.omit)
summary(fit.socsit)

### (1b) Random-slopes logistic regression model using time as predictor
fit.socsitRs <- glmer(as.factor(social)~situation_counter+(1 + situation_counter|vpncode/day), data=df.alldreams, family=binomial, na.action=na.omit)
summary(fit.socsitRs)

# using social interaction as outcome
### (2a) Random-Intercept logistic regression model using time as predictor
fit.socint <- glmer(as.factor(interaction)~situation_counter+(1|vpncode/day), data=df.alldreams, family = binomial, na.action=na.omit)
summary(fit.socint)

### (2b) Random-slopes logistic regression model using time as predictor
fit.socintRs <- glmer(as.factor(interaction)~situation_counter+(1 + situation_counter|vpncode/day), data=df.alldreams, family=binomial, na.action=na.omit)
summary(fit.socintRs)

# using multilateral interactions I as outcome
### (3a) Random-Intercept logistic regression model using time as predictor
fit.multi <- glmer(as.factor(multi)~situation_counter+(1|vpncode/day), data=df.alldreams, family = binomial, na.action=na.omit)
summary(fit.multi)

### (3b) Random-slopes logistic regression model using time as predictor
fit.multiRs <- glmer(as.factor(multi)~situation_counter+(1 + situation_counter|vpncode/day), data=df.alldreams, family=binomial, na.action=na.omit)
summary(fit.multiRs)

# using multilateral interactions II as outcome
### (4a) Random-Intercept logistic regression model using time as predictor
fit.realMulti <- glmer(as.factor(realMulti)~situation_counter+(1|vpncode/day), data=df.alldreams, family = binomial, na.action=na.omit)
summary(fit.realMulti)

### (4b) Random-slopes logistic regression model using time as predictor
fit.realMultiRs <- glmer(as.factor(realMulti)~situation_counter+(1 + situation_counter|vpncode/day), data=df.alldreams, family=binomial, na.action=na.omit)
summary(fit.realMultiRs)

# using multilateral interactions III as outcome
### (5a) Random-Intercept logistic regression model using time as predictor
fit.intcor <- glmer(as.factor(intcor)~situation_counter+(1|vpncode/day), data=df.alldreams, family = binomial, na.action=na.omit)
summary(fit.intcor)

### (5b) Random-slopes logistic regression model using time as predictor
fit.intcorRs <- glmer(as.factor(intcor)~situation_counter+(1 + situation_counter|vpncode/day), data=df.alldreams, family=binomial, na.action=na.omit)
summary(fit.intcorRs)

# using wordcount as outcome
### (6a) Random-Intercept Poisson regression model using time as predictor, nearly unidentifiable!
fit.wc <- glmer(wordcount~situation_counter+(1|vpncode/day), family=poisson, data=df.alldreams)
summary(fit.wc)

### (6b) Random-Slopes model using time as predictor, fails to converge!
fit.wcRs <- lmer(wordcount~situation_counter+(1 + situation_counter|vpncode/day), data=df.alldreams)
summary(fit.wcRs)

# using quality as outcome
### (7a) Random-Intercept cumulative link mixed model using time as predictor, does not work yet!
fit.q <- clmm(as.factor(quality_simple)~situation_counter+(1|vpncode/day), data=df.alldreams, na.action=na.omit)
summary(fit.q)

### (7b) Random-slopes cumulative link mixed model using time as predictor, does not work yet!
fit.qRs <- clmm(as.factor(quality_simple)~situation_counter+(1 + situation_counter|vpncode/day), data=df.alldreams, na.action=na.omit)
summary(fit.qRs)