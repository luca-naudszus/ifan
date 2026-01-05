# ----------------------------------------------------------
# Analyses time effects on dream content. 
# (c) Luca A. Naudszus, Salzburg, 27.09.2023
# Institute for Psychology, University of Duisburg-Essen, Essen
# ----------------------------------------------------------
#
#
# Input: 
### multilevel.csv, containing ordered and joint EMA and dream data on a daily level
#-----------
# Analyses time effect on social content in dreams using negative binomial regressions on count data
# Analyses time effect on social content in dreams using regressions on proportion data
# Cannot yet analyse moderation of time effect via wordcount, (n_epi), n_eve or n_sit via negative binomial regressions
# Cannot yet analyse moderation using regressions on proportion data
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
library(tidyverse)
source("scripts//IFAN_key.R")
### set working directory
setwd("data")
### load data
df.data <- as_tibble(read.csv(list.files(path = "2_preprocessed-data", pattern = 'multilevel', full.names = T)))

# center variables
df.data$CevePerEpi <- df.data$evePerEpi - mean(df.data$evePerEpi, na.rm = T)
df.data$CsitPerEve <- df.data$sitPerEve - mean(df.data$sitPerEve, na.rm = T)
df.data$CwordPerSit <- df.data$wordPerSit - mean(df.data$wordPerSit, na.rm = T)
# rescale variables
df.data$wordcount <- df.data$wordcount / 100

# exclude participants with too few data points
k = 4 # set lower limit, k = 5? 7?
df.data <- df.data[!is.na(df.data$propSocSit),]
df.participants <- df.data %>%
  group_by(vpncode) %>%
  summarise(count = n())
validIds <- df.participants$vpncode[df.participants$count > k]
df.dataE <- df.data[df.data$vpncode %in% validIds,]

# build longitudinal multi-level models

##########################################
####### BLOCK 1: Generalized linear models using count data as outcomes and only time as predictor
##########################################

### using social situations as outcome

# (1) Random-Intercept Only for social situations
fit.rio <- glmer(dreamSocSit ~ (1|vpncode), family = "poisson", data=df.dataE)
summary(fit.rio)

# (2a) Random-Intercept model for social situations, poisson regression
fit.riP <- glmer(dreamSocSit ~ counter + (1|vpncode), family = "poisson", data=df.dataE)
summary(fit.riP)

# (3a) Random-Slopes model for social situations, poisson regression
fit.rsP <- glmer(dreamSocSit ~ counter + (1+counter|vpncode), family = "poisson", data=df.dataE)
summary(fit.rsP)

# (2b) Random-Intercept model for social situations, quasipoisson regression
### not possible atm

# (3b) Random-Slopes model for social situations, quasipoisson regression
### not possible atm

# (2c) Random-Intercept model for social situations, negative binomial
fit.riNB <- glmer.nb(dreamSocSit ~ counter + (1|vpncode), data=df.dataE)
summary(fit.riNB)

# (3c) Random-Slopes model for social situations, negative binomial
fit.rsNB <- glmer.nb(dreamSocSit ~ counter + (1+counter|vpncode), data=df.dataE)
summary(fit.rsNB)

# ANOVA
anova(fit.riP, fit.rsP, fit.riNB, fit.rsNB)

### using social interactions as outcome

# (4) Random-Intercept Only for social interactions
fit.rio <- glmer(dreamSocInt ~ (1|vpncode), family = "poisson", data=df.dataE)
summary(fit.rio)

# (5a) Random-Intercept model for social interactions, poisson regression
fit.riP <- glmer(dreamSocInt ~ counter + (1|vpncode), family = "poisson", data=df.dataE)
summary(fit.riP)

# (6a) Random-Slopes model for social interactions, poisson regression
fit.rsP <- glmer(dreamSocInt ~ counter + (1+counter|vpncode), family = "poisson", data=df.dataE)
summary(fit.rsP)

# (5b) Random-Intercept model for social interactions, quasipoisson regression
### not possible atm

# (6b) Random-Slopes model for social interactions, quasipoisson regression
### not possible atm

# (5c) Random-Intercept model for social interactions, negative binomial
fit.riNB <- glmer.nb(dreamSocInt ~ counter + (1|vpncode), data=df.dataE)
summary(fit.riNB)

# (6c) Random-Slopes model for social interactions, negative binomial
fit.rsNB <- glmer.nb(dreamSocInt ~ counter + (1+counter|vpncode), data=df.dataE)
summary(fit.rsNB)

# ANOVA
anova(fit.riP, fit.rsP, fit.riNB, fit.rsNB)

### using multilateral interactions I as outcome

# (7) Random-Intercept Only for multilateral interactions I
fit.rio <- glmer(multi ~ (1|vpncode), family = "poisson", data=df.dataE)
summary(fit.rio)

# (8a) Random-Intercept model for multilateral interactions I, poisson regression
fit.riP <- glmer(multi ~ counter + (1|vpncode), family = "poisson", data=df.dataE)
summary(fit.riP)

# (9a) Random-Slopes model for multilateral interactions I, poisson regression
fit.rsP <- glmer(multi ~ counter + (1+counter|vpncode), family = "poisson", data=df.dataE)
summary(fit.rsP)

# (8b) Random-Intercept model for multilateral interactions I, quasipoisson regression
### not possible atm

# (9b) Random-Slopes model for multilateral interactions I, quasipoisson regression
### not possible atm

# (8c) Random-Intercept model for multilateral interactions I, negative binomial
fit.riNB <- glmer.nb(multi ~ counter + (1|vpncode), data=df.dataE)
summary(fit.riNB)

# (9c) Random-Slopes model for multilateral interactions I, negative binomial
fit.rsNB <- glmer.nb(multi ~ counter + (1+counter|vpncode), data=df.dataE)
summary(fit.rsNB)

# ANOVA
anova(fit.riP, fit.rsP, fit.riNB, fit.rsNB)

### using multilateral interactions II as outcome

# (10) Random-Intercept Only for multilateral interactions II
fit.rio <- glmer(realMulti ~ (1|vpncode), family = "poisson", data=df.dataE)
summary(fit.rio)

# (11a) Random-Intercept model for multilateral interactions II, poisson regression
fit.riP <- glmer(realMulti ~ counter + (1|vpncode), family = "poisson", data=df.dataE)
summary(fit.riP)

# (12a) Random-Slopes model for multilateral interactions II, poisson regression
fit.rsP <- glmer(realMulti ~ counter + (1+counter|vpncode), family = "poisson", data=df.dataE)
summary(fit.rsP)

# (11b) Random-Intercept model for multilateral interactions II, quasipoisson regression
### not possible atm

# (12b) Random-Slopes model for multilateral interactions II, quasipoisson regression
### not possible atm

# (11c) Random-Intercept model for multilateral interactions II, negative binomial
fit.riNB <- glmer.nb(realMulti ~ counter + (1|vpncode), data=df.dataE)
summary(fit.riNB)

# (12c) Random-Slopes model for multilateral interactions II, negative binomial
fit.rsNB <- glmer.nb(realMulti ~ counter + (1+counter|vpncode), data=df.dataE)
summary(fit.rsNB)

# ANOVA
anova(fit.riP, fit.rsP, fit.riNB, fit.rsNB)

### using multilateral interactions III as outcome

# (13) Random-Intercept Only for multilateral interactions III
fit.rio <- glmer(inter ~ (1|vpncode), family = "poisson", data=df.dataE)
summary(fit.rio)

# (14a) Random-Intercept model for multilateral interactions III, poisson regression
fit.riP <- glmer(inter ~ counter + (1|vpncode), family = "poisson", data=df.dataE)
summary(fit.riP)

# (15a) Random-Slopes model for multilateral interactions III, poisson regression
fit.rsP <- glmer(inter ~ counter + (1+counter|vpncode), family = "poisson", data=df.dataE)
summary(fit.rsP)

# (14b) Random-Intercept model for multilateral interactions III, quasipoisson regression
### not possible atm

# (15b) Random-Slopes model for multilateral interactions III, quasipoisson regression
### not possible atm

# (14c) Random-Intercept model for multilateral interactions III, negative binomial
fit.riNB <- glmer.nb(inter ~ counter + (1|vpncode), data=df.dataE)
summary(fit.riNB)

# (15c) Random-Slopes model for multilateral interactions III, negative binomial
fit.rsNB <- glmer.nb(inter ~ counter + (1+counter|vpncode), data=df.dataE)
summary(fit.rsNB)

# ANOVA
anova(fit.riP, fit.rsP, fit.riNB, fit.rsNB)

### using wordcount as outcome

# (16) Random-Intercept Only for wordcount 
fit.rio <- glmer(wordcount ~ (1|vpncode), family = "poisson", data=df.dataE)
summary(fit.rio)

# (17a) Random-Intercept model for wordcount, poisson regression
fit.riP <- glmer(wordcount ~ counter + (1|vpncode), family = "poisson", data=df.dataE)
summary(fit.riP)

# (18a) Random-Slopes model for wordcount, poisson regression
fit.rsP <- glmer(wordcount ~ counter + (1+counter|vpncode), family = "poisson", data=df.dataE)
summary(fit.rsP)

# (17b) Random-Intercept model for wordcount, quasipoisson regression
### not possible atm

# (18b) Random-Slopes model for wordcount, quasipoisson regression
### not possible atm

# (17c) Random-Intercept model for wordcount, negative binomial
fit.riNB <- glmer.nb(wordcount ~ counter + (1|vpncode), data=df.dataE)
summary(fit.riNB)

# (18c) Random-Slopes model for wordcount, negative binomial
fit.rsNB <- glmer.nb(wordcount ~ counter + (1+counter|vpncode), data=df.dataE)
summary(fit.rsNB)

# ANOVA
anova(fit.riP, fit.rsP, fit.riNB, fit.rsNB)

### using number of episodes as outcome

# (19) Random-Intercept Only for n_epi 
fit.rio <- glmer(n_epi ~ (1|vpncode), family = "poisson", data=df.dataE)
summary(fit.rio)

# (20a) Random-Intercept model for n_epi, poisson regression
fit.riP <- glmer(n_epi ~ counter + (1|vpncode), family = "poisson", data=df.dataE)
summary(fit.riP)

# (21a) Random-Slopes model for n_epi, poisson regression
fit.rsP <- glmer(n_epi ~ counter + (1+counter|vpncode), family = "poisson", data=df.dataE)
summary(fit.rsP)

# (20b) Random-Intercept model for n_epi, quasipoisson regression
### not possible atm

# (21b) Random-Slopes model for n_epi, quasipoisson regression
### not possible atm

# (20c) Random-Intercept model for n_epi, negative binomial
fit.riNB <- glmer.nb(n_epi ~ counter + (1|vpncode), data=df.dataE)
summary(fit.riNB)

# (21c) Random-Slopes model for n_epi, negative binomial
fit.rsNB <- glmer.nb(n_epi ~ counter + (1+counter|vpncode), data=df.dataE)
summary(fit.rsNB)

# ANOVA
anova(fit.riP, fit.rsP, fit.riNB, fit.rsNB)

### using number of events as outcome

# (22) Random-Intercept Only for n_eve 
fit.rio <- glmer(n_eve ~ (1|vpncode), family = "poisson", data=df.dataE)
summary(fit.rio)

# (23a) Random-Intercept model for n_eve, poisson regression
fit.riP <- glmer(n_eve ~ counter + (1|vpncode), family = "poisson", data=df.dataE)
summary(fit.riP)

# (24a) Random-Slopes model for n_eve, poisson regression
fit.rsP <- glmer(n_eve ~ counter + (1+counter|vpncode), family = "poisson", data=df.dataE)
summary(fit.rsP)

# (23b) Random-Intercept model for n_eve, quasipoisson regression
### not possible atm

# (24b) Random-Slopes model for n_eve, quasipoisson regression
### not possible atm

# (23c) Random-Intercept model for n_eve, negative binomial
fit.riNB <- glmer.nb(n_eve ~ counter + (1|vpncode), data=df.dataE)
summary(fit.riNB)

# (24c) Random-Slopes model for n_eve, negative binomial
fit.rsNB <- glmer.nb(n_eve ~ counter + (1+counter|vpncode), data=df.dataE)
summary(fit.rsNB)

# ANOVA
anova(fit.riP, fit.rsP, fit.riNB, fit.rsNB)

### using number of situations as outcome

# (25) Random-Intercept Only for n_sit 
fit.rio <- glmer(n_sit ~ (1|vpncode), family = "poisson", data=df.dataE)
summary(fit.rio)

# (26a) Random-Intercept model for n_sit, poisson regression
fit.riP <- glmer(n_sit ~ counter + (1|vpncode), family = "poisson", data=df.dataE)
summary(fit.riP)

# (27a) Random-Slopes model for n_sit, poisson regression
fit.rsP <- glmer(n_sit ~ counter + (1+counter|vpncode), family = "poisson", data=df.dataE)
summary(fit.rsP)

# (26b) Random-Intercept model for n_sit, quasipoisson regression
### not possible atm

# (27b) Random-Slopes model for n_sit, quasipoisson regression
### not possible atm

# (26c) Random-Intercept model for n_sit, negative binomial
fit.riNB <- glmer.nb(n_sit ~ counter + (1|vpncode), data=df.dataE)
summary(fit.riNB)

# (27c) Random-Slopes model for n_sit, negative binomial
fit.rsNB <- glmer.nb(n_sit ~ counter + (1+counter|vpncode), data=df.dataE)
summary(fit.rsNB)

# ANOVA
anova(fit.riP, fit.rsP, fit.riNB, fit.rsNB)

##########################################
####### BLOCK 2: MLMs using proportion data as outcomes and only time as predictor
##########################################

### using social situations as outcome

# (1) Random-Intercept only for social situations
fit.rio <- lmer(CpropSocSit ~ (1|vpncode), data=df.dataE)
summary(fit.rio)

# (2) Random-Intercepts model for social situations
fit.ri <- lmer(CpropSocSit ~ counter + (1|vpncode), data = df.dataE)
summary(fit.ri)

# (3) Random-Slopes model for social situations
fit.rs <- lmer(CpropSocSit ~ counter + (1 + counter|vpncode), data = df.dataE)
summary(fit.rs)

# ANOVA
anova(fit.rio, fit.ri)
anova(fit.ri, fit.rs)

### using social interactions as outcome

# (4) Random-Intercept only for social interactions
fit.rio <- lmer(CpropSocInt ~ (1|vpncode), data=df.dataE)
summary(fit.rio)

# (5) Random-Intercepts model for social interactions
fit.ri <- lmer(CpropSocInt ~ counter + (1|vpncode), data = df.dataE)
summary(fit.ri)

# (6) Random-Slopes model for social interactions
fit.rs <- lmer(CpropSocInt ~ counter + (1 + counter|vpncode), data = df.dataE)
summary(fit.rs)

# ANOVA
anova(fit.rio, fit.ri)
anova(fit.ri, fit.rs)

### using multilateral interactions I as outcome

# (7) Random-Intercept only for multilateral interactions I
fit.rio <- lmer(CpropMulti ~ (1|vpncode), data=df.dataE)
summary(fit.rio)

# (8) Random-Intercepts model for multilateral interactions I
fit.ri <- lmer(CpropMulti ~ counter + (1|vpncode), data = df.dataE)
summary(fit.ri)

# (9) Random-Slopes model for multilateral interactions I
fit.rs <- lmer(CpropMulti ~ counter + (1 + counter|vpncode), data = df.dataE)
summary(fit.rs)

# ANOVA
anova(fit.rio, fit.ri)
anova(fit.ri, fit.rs)

### using multilateral interactions II as outcome

# (10) Random-Intercept only for multilateral interactions II
fit.rio <- lmer(CpropRealMulti ~ (1|vpncode), data=df.dataE)
summary(fit.rio)

# (11) Random-Intercepts model for multilateral interactions II
fit.ri <- lmer(CpropRealMulti ~ counter + (1|vpncode), data = df.dataE)
summary(fit.ri)

# (12) Random-Slopes model for multilateral interactions II
fit.rs <- lmer(CpropRealMulti ~ counter + (1 + counter|vpncode), data = df.dataE)
summary(fit.rs)

# ANOVA
anova(fit.rio, fit.ri)
anova(fit.ri, fit.rs)

### using multilateral interactions III as outcome

# (13) Random-Intercept only for multilateral interactions III
fit.rio <- lmer(CpropIntcor ~ (1|vpncode), data=df.dataE)
summary(fit.rio)

# (14) Random-Intercepts model for multilateral interactions III
fit.ri <- lmer(CpropIntcor ~ counter + (1|vpncode), data = df.dataE)
summary(fit.ri)

# (15) Random-Slopes model for multilateral interactions III
fit.rs <- lmer(CpropIntcor ~ counter + (1 + counter|vpncode), data = df.dataE)
summary(fit.rs)

# ANOVA
anova(fit.rio, fit.ri)
anova(fit.ri, fit.rs)

### using events per episode as outcome

# (16) Random-Intercept only for eveperepi
fit.rio <- lmer(CevePerEpi ~ (1|vpncode), data=df.dataE)
summary(fit.rio)

# (17) Random-Intercepts model for eveperepi
fit.ri <- lmer(CevePerEpi ~ counter + (1|vpncode), data = df.dataE)
summary(fit.ri)

# (18) Random-Slopes model for eveperepi
fit.rs <- lmer(CevePerEpi ~ counter + (1 + counter|vpncode), data = df.dataE)
summary(fit.rs)

# ANOVA
anova(fit.rio, fit.ri)
anova(fit.ri, fit.rs)

### using situations per event as outcome

# (19) Random-Intercept only for sitpereve
fit.rio <- lmer(CsitPerEve ~ (1|vpncode), data=df.dataE)
summary(fit.rio)

# (20) Random-Intercepts model for sitpereve
fit.ri <- lmer(CsitPerEve ~ counter + (1|vpncode), data = df.dataE)
summary(fit.ri)

# (21) Random-Slopes model for sitpereve
fit.rs <- lmer(CsitPerEve ~ counter + (1 + counter|vpncode), data = df.dataE)
summary(fit.rs)

# ANOVA
anova(fit.rio, fit.ri)
anova(fit.ri, fit.rs)

### using words per situation as outcome

# (22) Random-Intercept only for wordpersit
fit.rio <- lmer(CwordPerSIt ~ (1|vpncode), data=df.dataE)
summary(fit.rio)

# (23) Random-Intercepts model for wordpersit
fit.ri <- lmer(CwordPerSit ~ counter + (1|vpncode), data = df.dataE)
summary(fit.ri)

# (24) Random-Slopes model for wordpersit
fit.rs <- lmer(CwordPerSit ~ counter + (1 + counter|vpncode), data = df.dataE)
summary(fit.rs)

# ANOVA
anova(fit.rio, fit.ri)
anova(fit.ri, fit.rs)

##########################################
####### BLOCK 3: Generalized linear models including moderators
##########################################

### using social situations as outcome

# (1) Random-Intercept only for social situations
fit.rio <- glmer.nb(dreamSocSit ~ (1|vpncode), data = df.dataE)
summary(fit.rio)

# (2) Random-Intercepts model for social situations
fit.ri <- glmer.nb(dreamSocSit ~ counter + (1|vpncode), data = df.dataE)
summary(fit.ri)

# (3) Random-Intercepts model for social situations including moderators
fit.rm <- glmer.nb(dreamSocSit ~ counter + wordcount + wordcount*counter + (1|vpncode), data = df.dataE)
summary(fit.rm)

###---- models do not converge, check at a later phase!

##########################################
####### BLOCK 4: MLMs including moderators
##########################################

### using social situations as outcome

# (1) Random-Intercept only for social situations
fit.rio <- glmer(CpropSocSit ~ (1|vpncode), data = df.dataE)
summary(fit.rio)

# (2) Random-Intercepts model for social situations
fit.ri <- lmer(CpropSocSit ~ counter + (1|vpncode), data = df.dataE)
summary(fit.ri)

# (3) Random-Intercepts model for social situations including moderator wordcount
fit.rim <- lmer(CpropSocSit ~ counter + wordcount + wordcount*counter + (1|vpncode), data = df.dataE)
summary(fit.rim)

# (4) Random-Slopes model for social situations including moderator wordcount
fit.rsm <- lmer(CpropSocSit ~ counter + wordcount + wordcount*counter + (1 + counter + wordcount + wordcount*counter|vpncode), data = df.dataE)
summary(fit.rsm)

###---- interpretation is not clear at the moment!

