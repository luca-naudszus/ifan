library(glmmTMB)
library(performance)
library(tidyverse)

# initialize ----------------------------------------------
rm(list=ls())

# read data
df_data <- read.csv('multilevel.csv')
df_person <- as_tibble(read.csv(
  list.files(path = file.path("data", "2_preprocessed-data"), 
             pattern = "ema_person", full.names = T)))

# ----------------------------------------------
# settings
fam <- beta_family()
cd = 0.5 # set lower limit for compliance on days, c = 0.5?
cp = 0.5 # set lower limit for compliance within-person, cp = 0.5?
k = 3 # set lower limit, k = 5? 7?
outcome = "propDreamSocSit"
predictor = "propEMAsocSit_cwc"
predictor_cm = "propEMAsocSit_cm"

# ----------------------------------------------
# exclusion
### exclude participants with too few data points

df_data <- df_data[!is.na(df_data$propDreamSocSit),]
df_participants <- df_data %>%
  group_by(id) %>%
  summarise(count = n())
validIds <- df_participants$id[df_participants$count >= k]

### exclude participants with too low compliance
df_data_cleaned <- df_data[df_data$compliance >= cd,]
validIds <- validIds[validIds %in% df_person$id[df_person$compliance >= cp]]
df_data_cleaned <- df_data_cleaned[df_data_cleaned$id %in% validIds,]
write.csv(validIds, file.path('2_preprocessed-data", "meta", "mla_participants.csv'), row.names = F)

# choose outcome and squeeze 0 and 1 for beta mixed model
df_data_cleaned$outcome <- (
  df_data_cleaned[[outcome]] * (nrow(df_data_cleaned) - 1) + 0.5) / nrow(
    df_data_cleaned) # Smithson—Verkuilen transformation
df_data_cleaned$predictor <- df_data_cleaned[[predictor]]
df_data_cleaned$predictor_cm <- df_data_cleaned[[predictor_cm]]

d <- df_data_cleaned
##########################################
# null-models

# random intercept only
m0 <- glmmTMB(outcome ~ 1 + (1|id), 
           family = fam, data = d)

icc(m0) # large ICC: people differ in propDreamSocSit, small ICC in propDreamSocInt

# random intercept and autocorrelation
# m0_ar <- glmmTMB(propDreamSocSit_sq ~ 1 + (1|id), 
#                 family = fam,
#                 dispformula = ~ ar1(dayCounter + 0 | id),
#                 data = d)
# summary(m0_ar)
# anova(m0, m0_ar) # huge difference, propDreamSocSit is not independent across days

##########################################
# random-intercept models

### self-esteem as predictor
m1a <- glmmTMB(outcome ~ 1 + rses_totalMean_cwc + day_c + (1|id), 
                   family = fam, data = d)
summary(m1a)
anova(m0, m1a) # does not explain any variance

### propEMAsocSit as predictor
m1b <- glmmTMB(outcome ~ 1 + predictor + day_c + (1|id), 
              family = fam, data = d)
summary(m1b)
anova(m0, m1b) # does not explain any variance

### self-esteem and propEMAsocSit as predictors
m1c <- glmmTMB(outcome ~ 1 + rses_totalMean_cwc+predictor + day_c + (1|id), 
               family = fam, data = d)
summary(m1c)
anova(m0, m1c) # does not explain any variance

### exclusion as predictor
m1y <- glmmTMB(outcome ~ 1 + excl_cwc+predictor + day_c + (1|id), 
               family = fam, data = d)
summary(m1y)
anova(m0, m1y) # does not explain any variance

##########################################
# random-slopes models

### self-esteem
m2a <- glmmTMB(outcome ~ 1 + rses_totalMean_cwc + predictor + day_c + (1 + rses_totalMean_cwc|id), 
               family = fam, data = d)
summary(m2a)
anova(m1c, m2a) # does not improve fit, removing random slopes

### propEMAsocSit
m2b <- glmmTMB(outcome ~ 1 + rses_totalMean_cwc + predictor + day_c + (1 + predictor|id), 
               family = fam, data = d)
summary(m2b)
anova(m1c, m2b) # does not improve fit, removing random slopes

### exclusion
m2y <- glmmTMB(outcome ~ 1 + rses_totalMean_cwc + predictor + day_c + (1 + excl_cwc|id), 
               family = fam, data = d)
summary(m2y)
anova(m1c, m2y) # does not improve fit, removing random slopes

##########################################
# contextual effects

### self-esteem
m3a <- glmmTMB(outcome ~ 1 + rses_totalMean_cwc + predictor + rses_totalMean_cm + day_c + (1|id), 
               family = fam, data = d)
summary(m3a)
anova(m1c, m3a)

### propEMAsocSit
m3b <- glmmTMB(outcome ~ 1 + rses_totalMean_cwc + predictor + predictor_cm + day_c + (1|id), 
              family = fam, data = d)
summary(m3b)
anova(m1c, m3b)

m3c <- glmmTMB(outcome ~ 1 + rses_totalMean_cwc + predictor + rses_totalMean_cm + predictor_cm + day_c + (1|id), 
               family = fam, data = d)
summary(m3c)
anova(m1c, m3c)

### exclusion
m3y <- glmmTMB(outcome ~ 1 + rses_totalMean_cwc + predictor + excl_cm + day_c + (1|id), 
               family = fam, data = d)
summary(m3y)
anova(m1c, m3y)

##########################################
# level-2 predictors

#TODO: Find missing IAT values, only 92 IDs
#TODO: Find missing NLT values, only 144 IDs
m4 <- glmmTMB(outcome ~ 1 + rses_totalMean_cwc + predictor + rses_totalMean_cm + predictor_cm + rses_overall_gmc + iat_gmc + nlt_gmc + day_c + (1|id), 
               family = fam, data = d %>% filter(!is.na(nlt_gmc) & !is.na(iat_gmc) & !is.na(rses_overall_gmc)))

m4a <- glmmTMB(outcome ~ 1 + rses_totalMean_cwc + predictor + rses_overall_gmc + day_c + (1|id), 
              family = fam, data = d %>% filter(!is.na(rses_overall_gmc)))

m4b <- glmmTMB(outcome ~ 1 + rses_totalMean_cwc + predictor + iat_gmc + day_c + (1|id), 
              family = fam, data = d %>% filter(!is.na(iat_gmc)))

m4c <- glmmTMB(outcome ~ 1 + rses_totalMean_cwc + predictor + nlt_gmc + day_c + (1|id), 
              family = fam, data = d %>% filter(!is.na(nlt_gmc)))

m4xtra1 <- glmmTMB(outcome ~ 1 + rses_totalMean_cwc + predictor + xtra_gmc + day_c + (1|id), 
               family = fam, data = d)

m4xtra2 <- glmmTMB(outcome ~ 1 + xtra_gmc + day_c + (1|id), 
                   family = fam, data = d)

##########################################
# exploratory level-2 predictors

m5 <- glmmTMB(outcome ~ 1 + rses_totalMean_cwc + predictor + iso_gmc + huo_gmc + day_c + (1|id),  
              family = fam, data = d)

m5a <- glmmTMB(outcome ~ 1 + rses_totalMean_cwc + predictor + rses_overall_gmc + iat_gmc + nlt_gmc + iso_gmc + huo_gmc + day_c + (1|id), 
              family = fam, data = d %>% filter(!is.na(nlt_gmc) & !is.na(iat_gmc) & !is.na(rses_overall_gmc)))

m5b <- glmmTMB(outcome ~ 1 + rses_totalMean_cwc + predictor + rses_totalMean_cm + predictor_cm + rses_overall_gmc + iat_gmc + nlt_gmc + iso_gmc + huo_gmc + day_c + (1|id), 
               family = fam, data = d %>% filter(!is.na(nlt_gmc) & !is.na(iat_gmc) & !is.na(rses_overall_gmc)))

m5c <- glmmTMB(outcome ~ 1 + rses_totalMean_cwc + predictor + xtra_gmc + open_gmc + agre_gmc + cons_gmc + neur_gmc + day_c + (1|id),  
              family = fam, data = d)

##########################################
# wordcount
m6a <- glmmTMB(outcome ~ 1 + scale(wordcount) + (1|id), 
              family = fam, data = d)

m6b <- glmmTMB(outcome ~ 1 + scale(wordcount) + rses_totalMean_cwc+predictor + day_c + (1|id), 
               family = fam, data = d)
anova(m6b, m1c)

m6c <- glmmTMB(outcome ~ 1 + scale(wordcount) + rses_totalMean_cwc + predictor + rses_totalMean_cm + day_c + (1|id), 
               family = fam, data = d)


m6d <- glmmTMB(outcome ~ 1 + scale(wordcount) + rses_totalMean_cwc + predictor + rses_overall_gmc + iat_gmc + nlt_gmc + day_c + (1|id), 
               family = fam, data = d %>% filter(!is.na(nlt_gmc) & !is.na(iat_gmc) & !is.na(rses_overall_gmc)))

##########################################
# further structural properties
m7a <- glmmTMB(outcome ~ 1 + scale(wordPerSit) + scale(sitPerEve) + scale(evePerEpi) + (1|id), 
               family = fam, data = d)

m7b <- glmmTMB(outcome ~ 1 + scale(wordPerSit) + scale(sitPerEve) + scale(evePerEpi) + rses_totalMean_cwc+predictor + day_c + (1|id), 
               family = fam, data = d)
anova(m7b, m1c)

m7c <- glmmTMB(outcome ~ 1 + scale(wordPerSit) + scale(sitPerEve) + scale(evePerEpi)  + rses_totalMean_cwc + predictor + rses_overall_gmc + iat_gmc + nlt_gmc + day_c +  + (1|id), 
               family = fam, data = d %>% filter(!is.na(nlt_gmc) & !is.na(iat_gmc) & !is.na(rses_overall_gmc)))