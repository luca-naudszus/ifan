# ----------------------------------------------------------
# First multi-level analyses. 
# (c) Luca A. Naudszus, Essen, 31.08.2023 / Da Nang, 13.08.2025
# Institute for Psychology, University of Duisburg-Essen, Essen
# Social Brain Sciences Lab, ETH Zurich, Zurich
# ----------------------------------------------------------
#
#
# Input: 
### 
#-----------
# Prepares long tables containing peripheric data, content and social content analysis data for all dreams and all participants
#-----------
#-----------
# Output: 
### 
# ---------------------------------------------------------

# initialize ----------------------------------------------
rm(list=ls())

### load packages and sources
library(DHARMa)
library(glmmTMB)
library(lme4)
library(performance)
library(tidyverse)
source(file.path("scripts", "Basics", "IFAN_key.R"))

### load data
df_rses <- as_tibble(read.csv(
  list.files(path = file.path("data", "2_preprocessed-data", "personality"), 
             pattern = 'rses', full.names = T)))
df_iat <- as_tibble(read.csv(
  list.files(path = file.path("data", "2_preprocessed-data", "personality"), 
             pattern = 'implicit-association-test', full.names = T))) # find out which 
df_iso <- as_tibble(read.csv(
  list.files(path = file.path("data", "2_preprocessed-data", "personality"), 
             pattern = "isolation", full.names = T)))
df_huo <- as_tibble(read.csv(
  list.files(path = file.path("data", "2_preprocessed-data", "personality"), 
             pattern = "huo", full.names = T)))
df_nlt <- as_tibble(read.csv(
  list.files(path = file.path("data", "2_preprocessed-data", "personality"), 
             pattern = "nlt", full.names = T)))
df_neo <- as_tibble(read.csv(
  list.files(path = file.path("data", "2_preprocessed-data", "personality"), 
             pattern = "neo", full.names = T)))
df_person <- as_tibble(read.csv(
  list.files(path = file.path("data", "2_preprocessed-data"), 
             pattern = "ema_person", full.names = T)))
df_excluded <- read.csv(
  list.files(path = file.path("data", "2_preprocessed-data", "meta"), 
             pattern = 'excluded', full.names = T))
df_included <- pull(read.csv(
  list.files(path = file.path("data", "2_preprocessed-data", "meta"), 
             pattern = 'included', full.names = T)))
df_data <- NULL
for (fn in list.files(path = file.path("data", "2_preprocessed-data", "multilevel"))) {
  df_id <- as_tibble(read.csv(file.path("data", "2_preprocessed-data", "multilevel", fn)))
  df_data <- rbind(df_data, 
                    df_id)
}
df_data <- df_data %>% select(-X)

### initialize variables
use_poor_slpq = T #include participants with poor PSQI scores?
options(contrasts = c("contr.sum","contr.poly"))

### exclude participants
if (use_poor_slpq){
  df_excluded <- df_excluded[df_excluded$reason != "Sleep Quality (PSQI)",]
}
df_data <- df_data[!(df_data$id %in% df_excluded$id),]

# ----------------------------------------------
# prepare data frame 
### add overall self-esteem
df_data$rses_overall <- NA
df_data$iat <- NA
df_data$nlt <- NA
df_data$iso <- NA
df_data$huo <- NA
df_data$xtra <- NA
for (irow in 1:dim(df_data)[1]){
  df_data$rses_overall[irow] <- df_rses$rses[df_data$id[irow] == df_rses$id]
  if (df_data$id[irow] %in% df_iat$id){
    df_data$iat[irow] <- df_iat$dscore[df_data$id[irow] == df_iat$id]
  }
  if (df_data$id[irow] %in% df_nlt$vpncode){
    df_data$nlt[irow] <- df_nlt$score[df_data$id[irow] == df_nlt$vpncode]
  }
  df_data$iso[irow] <- df_iso$isolation[df_data$id[irow] == df_iso$id]
  df_data$huo[irow] <- df_huo$sociometer[df_data$id[irow] == df_huo$id]
  df_data$xtra[irow] <- df_neo$xtra[df_data$id[irow] == df_neo$id]
  df_data$open[irow] <- df_neo$open[df_data$id[irow] == df_neo$id]
  df_data$neur[irow] <- df_neo$neur[df_data$id[irow] == df_neo$id]
  df_data$cons[irow] <- df_neo$cons[df_data$id[irow] == df_neo$id]
  df_data$agre[irow] <- df_neo$agre[df_data$id[irow] == df_neo$id]
}

### tidy data frame
df_data <- df_data %>%
  mutate(
    id = factor(id),
    dateDream = factor(dateDream),
    datetime = factor(datetime)
  )

### center all important variables
df_data <- df_data %>%
  group_by(id) %>%
  # cluster-mean centering on level 1
  mutate(across(c( 
    rses_totalMean, propEMAsocSit, propEMAsocInt, excl,
    rses_totalSd, socSitSd, socIntSd, exclSd, 
    propDreamSocSit, propDreamSocInt, propDreamTypeV, propDreamSocEvent, 
    wordcount, wordPerSit, sitPerEve, evePerEpi,
  ), ~ .x - mean(.x, na.rm = TRUE), .names = "{.col}_cwc"),
  across(c( # cluster mean 
    rses_totalMean, propEMAsocSit, propEMAsocInt, excl,
    rses_totalSd, socSitSd, socIntSd, exclSd, 
    propDreamSocSit, propDreamSocInt, propDreamTypeV, propDreamSocEvent, 
  ), ~ mean(.x, na.rm = TRUE), .names = "{.col}_cm"),
  day0 = dayCounter - min(dayCounter, na.rm = TRUE)) %>%
  ungroup() %>%
  # grand mean centering on level 2
  mutate(across(c(
    rses_overall, nlt, iat, iso, huo, xtra, open, cons, neur, agre,
    ), ~ .x - mean(.x, na.rm = T), .names = "{.col}_gmc"),
    day_c = day0 - mean(day0, na.rm = TRUE),
    day_c2 = day_c^2)

# save data
write.csv(df_data, 'multilevel.csv', row.names = F)

# ----------------------------------------------
# exclusion
### exclude participants with too few data points
k = 1 # set lower limit, k = 5? 7?
df_data <- df_data[!is.na(df_data$propDreamSocSit),]
df_participants <- df_data %>%
  group_by(id) %>%
  summarise(count = n())
validIds <- df_participants$id[df_participants$count >= k]

### exclude participants with too low compliance
cd = 0.5 # set lower limit for compliance on days, c = 0.5?
cp = 0.5 # set lower limit for compliance within-person, cp = 0.5?
df_data_cleaned <- df_data[df_data$compliance >= cd,]
validIds <- validIds[validIds %in% df_person$id[df_person$compliance >= cp]]
df_data_cleaned <- df_data_cleaned[df_data_cleaned$id %in% validIds,]
write.csv(validIds, file.path('2_preprocessed-data", "meta", "mla_participants.csv'), row.names = F)

# ----------------------------------------------
# Visual EDA
ggplot(df_data_cleaned, 
       aes(x = propDreamSocSit_cwc, y = propEMAsocSit_cwc, group = id)) +
  geom_point(alpha = .3) + 
  geom_smooth(method = "lm", se = FALSE)

# ----------------------------------------------
# correlation of EMA variables: social situations
#fit1null <- lmer(propEMAsocSit_cwc ~ 1 + (1|id), data = df_data_cleaned)
#summary(fit1null)

#fit.ri <- lmer(propEMAsocSit_C ~ 1 + rses_overall_C + excl_C + (1|id), data = df_data_cleaned)
#summary(fit.ri)

#fit.rs <- lmer(propEMAsocSit_C ~ 1 + rses_overall_C + excl_C + (1+ rses_overall_C + excl_C|id), data = df_data_cleaned)
#summary(fit.rs)

# correlation of EMA variables: social interactions
#fit <- lmer(propEMAsocInt_C ~ 1 + (1|id), data = df_data_cleaned)
#summary(fit)

#fit.ri <- lmer(propEMAsocInt_C ~ 1 + rses_overall_C + excl_C + (1|id), data = df_data_cleaned)
#summary(fit.ri)

#fit.rs <- lmer(propEMAsocInt_C ~ 1 + rses_overall_C + excl_C + (1+ rses_overall_C + excl_C|id), data = df_data_cleaned)
#summary(fit.rs)

# correlation of EMA variables: exclusion
#fit <- lmer(excl_C ~ 1 + (1|id), data = df_data_cleaned)
#summary(fit)

#fit.ri <- lmer(excl_C ~ 1 + rses_overall_C + propEMAsocSit_C + propEMAsocInt_C + (1|id), data = df_data_cleaned)
#summary(fit.ri)

#fit.rs <- lmer(excl_C ~ 1 + rses_overall_C + propEMAsocSit_C + propEMAsocInt_C + (1 + rses_overall_C + propEMAsocSit_C + propEMAsocInt_C|id), data = df_data_cleaned)




#-----------------------------------------