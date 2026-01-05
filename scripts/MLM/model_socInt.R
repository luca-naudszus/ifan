# initialize ----------------------------------------------
rm(list=ls())

# read data
read.csv(df_data, 'multilevel.csv', row.names = F)

# ----------------------------------------------
# exclusion
### exclude participants with too few data points
k = 3 # set lower limit, k = 5? 7?
df_data <- df_data[!is.na(df_data$propDreamSocInt),]
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

# squeeze 0 and 1 for beta mixed model
df_data_cleaned$propDreamSocInt_sq <- (
  df_data_cleaned$propDreamSocInt2 * (nrow(df_data_cleaned) - 1) + 0.5) / nrow(
    df_data_cleaned) # Smithson—Verkuilen transformation
df_data_cleaned$day_numeric <- as.numeric(df_data_cleaned$dayCounter)

##########################################
# null-models

# random intercept only
m0 <- glmmTMB(propDreamSocInt_sq ~ 1 + (1|id), 
              family = beta_family(),
              data = df_data_cleaned)
summary(m0)
r2(m0)
icc(m0) # large ICC: people differ in propDreamSocInt

# random intercept and autocorrelation
m0_ar <- glmmTMB(propDreamSocInt_sq ~ 1 + (1|id), 
                 family = beta_family(),
                 dispformula = ~ ar1(dayCounter + 0 | id),
                 data = df_data_cleaned)
summary(m0_ar)
anova(m0, m0_ar) # huge difference, propDreamSocInt is not independent across days

##########################################
# random-intercept models

### self-esteem as predictor
m1a <- glmmTMB(propDreamSocInt_sq ~ 1 + rses_totalMean_cwc + (1|id), 
               family = beta_family(link = "logit"),
               dispformula = ~ar1(dayCounter + 0|id),
               data = df_data_cleaned)
summary(m1a)
anova(m0_ar, m1a) # does not explain any variance

### propEMAsocInt as predictor
m1b <- glmmTMB(propDreamSocInt_sq ~ 1 + propEMAsocInt_cwc + (1|id), 
               family = beta_family(link = "logit"),
               dispformula = ~ar1(dayCounter + 0|id),
               data = df_data_cleaned)
summary(m1b)
anova(m0_ar, m1b) # does not explain any variance

### exclusion as predictor
m1c <- glmmTMB(propDreamSocInt_sq ~ 1 + excl_cwc + (1|id), 
               family = beta_family(link = "logit"),
               dispformula = ~ar1(dayCounter + 0|id),
               data = df_data_cleaned)
summary(m1c)
anova(m0_ar, m1c) # does not explain any variance

### self-esteem, exclusion and propEMAsocInt as predictors
m1d <- glmmTMB(propDreamSocInt_sq ~ 1 + rses_totalMean_cwc*propEMAsocInt_cwc*excl_cwc + (1|id), 
               family = beta_family(link = "logit"),
               dispformula = ~ar1(dayCounter + 0|id),
               data = df_data_cleaned)
summary(m1d)
anova(m0_ar, m1d) # does not explain any variance

##########################################
# level-2 predictors

m2a <- glmmTMB(propDreamSocInt_sq ~ 1 + rses_overall_gmc + (1|id),
               family = beta_family(link = "logit"), 
               dispformula = ~ar1(dayCounter + 0|id),
               data = df_data_cleaned)
summary(m2a)

m2b <- glmmTMB(propDreamSocInt_sq ~ 1 + iat_gmc + (1|id), 
               family = beta_family(link = "logit"),
               dispformula = ~ar1(dayCounter + 0|id),
               data = df_data_cleaned)
summary(m2b) # does not explain any variance

m2c <- glmmTMB(propDreamSocInt_sq ~ 1 + nlt_gmc + (1|id), 
               family = beta_family(link = "logit"),
               dispformula = ~ar1(dayCounter + 0|id),
               data = df_data_cleaned)
summary(m2c) # does not explain any variance

m2d <- glmmTMB(propDreamSocInt_sq ~ 1 + iso_gmc + (1|id), 
               family = beta_family(link = "logit"),
               dispformula = ~ar1(dayCounter + 0|id),
               data = df_data_cleaned)
summary(m2d)

m2e <- glmmTMB(propDreamSocInt_sq ~ 1 + huo_gmc + (1|id), 
               family = beta_family(link = "logit"),
               dispformula = ~ar1(dayCounter + 0|id),
               data = df_data_cleaned)
summary(m2e) # does not explain any variance

##########################################
# random-slopes models

### self-esteem
m3a <- glmmTMB(propDreamSocInt_sq ~ 1 + rses_totalMean_cwc + (1 + rses_totalMean_cwc|id), 
               family = beta_family(link = "logit"),
               dispformula = ~ar1(dayCounter + 0|id),
               data = df_data_cleaned)
summary(m3a) # does not explain any variance

### propEMAsocInt
m3b <- glmmTMB(propDreamSocInt_sq ~ 1 + propEMAsocInt_cwc + (1 + propEMAsocInt_cwc|id), 
               family = beta_family(link = "logit"),
               dispformula = ~ar1(dayCounter + 0|id),
               data = df_data_cleaned)
summary(m3b) # does not explain any variance

##########################################
# random-intercept models with variance

### self-esteem 
m4a <- glmmTMB(propDreamSocInt_sq ~ 1 + rses_totalSd_cwc + (1|id), 
               family = beta_family(link = "logit"),
               dispformula = ~ar1(dayCounter + 0|id),
               data = df_data_cleaned)
summary(m4a)
anova(m0_ar, m4a) # does not explain any variance

### propEMAsocInt as predictor, that would be no hypothesis
m4b <- glmmTMB(propDreamSocInt_sq ~ 1 + socIntSd_cwc + (1|id), 
               family = beta_family(link = "logit"),
               dispformula = ~ar1(dayCounter + 0|id),
               data = df_data_cleaned)
summary(m4b)
anova(m0_ar, m4b) # scratches at significance, but multiple testing?

### exclusion as predictor
m4c <- glmmTMB(propDreamSocInt_sq ~ 1 + exclSd_cwc + (1|id), 
               family = beta_family(link = "logit"),
               dispformula = ~ar1(dayCounter + 0|id),
               data = df_data_cleaned)
summary(m4c) # significant!
anova(m0_ar, m4c) # significant!