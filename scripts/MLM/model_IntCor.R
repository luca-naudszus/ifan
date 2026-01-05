library(glmmTMB)
library(performance)

# initialize ----------------------------------------------
rm(list=ls())

# read data
# read data
df_data <- read.csv('multilevel.csv')
df_person <- as_tibble(read.csv(
  list.files(path = file.path("data", "2_preprocessed-data"), 
             pattern = "ema_person", full.names = T)))

# ----------------------------------------------
# exclusion
### exclude participants with too few data points
k = 3 # set lower limit, k = 5? 7?
df_data <- df_data[!is.na(df_data$propDreamIntcor),]
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
df_data_cleaned$propDreamIntcor_sq <- (
  df_data_cleaned$propDreamIntcor2 * (nrow(df_data_cleaned) - 1) + 0.5) / nrow(
    df_data_cleaned) # Smithson—Verkuilen transformation
df_data_cleaned$day_numeric <- as.numeric(df_data_cleaned$dayCounter)

d <- df_data_cleaned

##########################################
# null-models

# random intercept only
m0 <- glmmTMB(propDreamIntcor_sq ~ 1 + (1|id), 
              family = beta_family(),
              data = df_data_cleaned)
summary(m0)
r2(m0)
icc(m0) # small ICC: people don't differ in propDreamIntcor

# random intercept and autocorrelation
m0_ar <- glmmTMB(propDreamIntcor_sq ~ 1 + (1|id), 
                 family = beta_family(),
                 dispformula = ~ ar1(dayCounter + 0 | id),
                 data = df_data_cleaned)
summary(m0_ar)
anova(m0, m0_ar) # huge difference, propDreamIntcor is not independent across days

##########################################
# random-intercept models

### self-esteem as predictor
m1a <- glmmTMB(propDreamIntcor_sq ~ 1 + rses_totalMean_cwc + (1|id), 
               family = beta_family(link = "logit"),
               dispformula = ~ar1(dayCounter + 0|id),
               data = df_data_cleaned)
summary(m1a)
anova(m0_ar, m1a) # does not explain any variance

### propEMAsocInt as predictor
m1b <- glmmTMB(propDreamIntcor_sq ~ 1 + propEMAsocInt_cwc + (1|id), 
               family = beta_family(link = "logit"),
               dispformula = ~ar1(dayCounter + 0|id),
               data = df_data_cleaned)
summary(m1b)
anova(m0_ar, m1b) # does not explain any variance

### exclusion as predictor
m1c <- glmmTMB(propDreamIntcor_sq ~ 1 + excl_cwc + (1|id), 
               family = beta_family(link = "logit"),
               dispformula = ~ar1(dayCounter + 0|id),
               data = df_data_cleaned)
summary(m1c)
anova(m0_ar, m1c) # does not explain any variance

### self-esteem, exclusion and propEMAsocInt as predictors
m1d <- glmmTMB(propDreamIntcor_sq ~ 1 + rses_totalMean_cwc*propEMAsocInt_cwc*excl_cwc + (1|id), 
               family = beta_family(link = "logit"),
               dispformula = ~ar1(dayCounter + 0|id),
               data = df_data_cleaned)
summary(m1d)
anova(m0_ar, m1d) # does not explain any variance

##########################################
# level-2 predictors

m2a <- glmmTMB(propDreamIntcor_sq ~ 1 + rses_overall_gmc + (1|id),
               family = beta_family(link = "logit"), # no AR because no convergence
               data = df_data_cleaned)
summary(m2a)

m2b <- glmmTMB(propDreamIntcor_sq ~ 1 + iat_gmc + (1|id), 
               family = beta_family(link = "logit"),
               data = df_data_cleaned)
summary(m2b) # tendency when using IntCor1, 

m2c <- glmmTMB(propDreamIntcor_sq ~ 1 + nlt_gmc + (1|id), 
               family = beta_family(link = "logit"),
               data = df_data_cleaned)
summary(m2c) # here is a significant result with IntCor2!

m2d <- glmmTMB(propDreamIntcor_sq ~ 1 + iso_gmc + (1|id), 
               family = beta_family(link = "logit"),
               data = df_data_cleaned)
summary(m2d)

m2e <- glmmTMB(propDreamIntcor_sq ~ 1 + huo_gmc + (1|id), 
               family = beta_family(link = "logit"),
               dispformula = ~ar1(dayCounter + 0|id),
               data = df_data_cleaned)
summary(m2e) # does not explain any variance

##########################################
# random-slopes models

### self-esteem
m3a <- glmmTMB(propDreamIntcor_sq ~ 1 + rses_totalMean_cwc + (1 + rses_totalMean_cwc|id), 
               family = beta_family(link = "logit"),
               data = df_data_cleaned)
summary(m3a) # does not explain any variance

### propEMAsocInt
# does not converge for IntCor2
m3b <- glmmTMB(propDreamIntcor_sq ~ 1 + propEMAsocInt_cwc + (1 + propEMAsocInt_cwc|id), 
               family = beta_family(link = "logit"),
               data = df_data_cleaned)
summary(m3b) # does not explain any variance
anova(m1b, m3b)

##########################################
# random-intercept models with variance

### self-esteem 
m4a <- glmmTMB(propDreamIntcor_sq ~ 1 + rses_totalSd_cwc + (1|id), 
               family = beta_family(link = "logit"),
               dispformula = ~ar1(dayCounter + 0|id),
               data = df_data_cleaned)
summary(m4a)
anova(m0_ar, m4a) # does not explain any variance

### propEMAsocInt as predictor
m4b <- glmmTMB(propDreamIntcor_sq ~ 1 + socIntSd_cwc + (1|id), 
               family = beta_family(link = "logit"),
               dispformula = ~ar1(dayCounter + 0|id),
               data = df_data_cleaned)
summary(m4b)
anova(m0_ar, m4b) # does not explain any variance

### exclusion as predictor
m4c <- glmmTMB(propDreamIntcor_sq ~ 1 + exclSd_cwc + (1|id), 
               family = beta_family(link = "logit"),
               dispformula = ~ar1(dayCounter + 0|id),
               data = df_data_cleaned)
summary(m4c)
anova(m0_ar, m4c) # does not explain any variance