# ----------------------------------------------------------
# Principal component analysis of Hypersensitive Narcissism Scale
# (c) Luca A. Naudszus, Essen, 26.03.2024
# Institute for Psychology, University of Duisburg-Essen, Essen
# ----------------------------------------------------------
#
#
# Input: 
### df.hsns
#-----------
# Compares Hypersensitive Narcissism Scale values with results of Fossati et al. (2009)
# Principal component analysis with promax rotation
# Extraction of principal component scores
# Calculation of two theoretical subscores following Fossati et al. (2009)
#-----------
#-----------
# Output: 
### df.hsns, now containing two theoretical subscores and two (or more) empirical ones
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
library(fungible)
library(tidyverse)
library(psych)

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
df.hsns <- df.hsns[!(df.hsns[,1] %in% excluded_participants$participant),]
rownames(df.hsns) <- df.hsns[,1]
df.hsns <- df.hsns[,2:12] # use only items and general hsns score, delete any previous results
# ---------------------------------------------------------

# ---------------------------------------------------------
# Internal consistency

item.scale <- psych::alpha(df.hsns[,1:10])
### check whether values are consistent with Fossati et al. (2009)
item.scale$total$std.alpha # should be close to .69
mean(item.scale$item.stats$r.cor) # should be close to .35
sd(item.scale$item.stats$r.cor) # should be close to .05

# ---------------------------------------------------------
# Principal Component Analysis

### correlation matrix
cormat <- cor(df.hsns[,1:10], use = 'pairwise.complete.obs')
### scree plot
VSS.scree(cormat)
nFactors = 2 # visual inspection of scree plot suggests 2 factors (26.03.2024)
### principal component analysis with promax rotation
results = promaxQ(R = cormat, facMethod = 'pca', numFactors = nFactors, power = 4)
### get explained variance
variance_explained <- apply(results$loadings^2, 2, sum)
variance_explained <- variance_explained / sum(variance_explained) # normalize
variance_explained # should be close to 0.427 and 0.411

# ---------------------------------------------------------
# Get principal component scores

pc_scores <- as.matrix(df.hsns[,1:10]) %*% results$loadings
pc_scores <- as.data.frame(pc_scores)
# Following Fossati et al. (2009), one principal component should reflect oversensitivity to judgment,
# while the other one should identify egocentrism. It is recommended to evaluate the components content-wise. 
colnames(pc_scores) <- c('hsns_pc1Over', 'hsns_pc2Ego')

# ---------------------------------------------------------
# Get theoretical subscores
df.hsns$hsns_over <- rowMeans(df.hsns[,c(1:3, 6, 7, 9)])
df.hsns$hsns_ego <- rowMeans(df.hsns[,c(4,5,8,10)])

# ---------------------------------------------------------
# Merge and write data

df.hsns$id <- rownames(df.hsns)
pc_scores$id <- rownames(pc_scores)
df.hsns <- inner_join(df.hsns, pc_scores)
df.hsns <- df.hsns %>%
  select(id, hsns_1:hsns_10, hsns, hsns_over, hsns_ego, hsns_pc1Over, hsns_pc2Ego)
write.csv(df.hsns, file.path("data", "2_preprocessed-data/hsns.csv"), row.names = F)
# ---------------------------------------------------------