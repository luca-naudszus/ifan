# ----------------------------------------------------------
# Calculates reliability for questionnaires in the IFAN project. 
# (c) Luca A. Naudszus, Mainz, 05.04.2024 / Zürich, 06.10.2025
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
### hsns.csv, contains HSNS items
### narq.csv, contains NARQ items
### iat.csv, contains SE-IAT d-scores
#-----------
# Calculates item statistics, scale statistics and internal consistency (both Cronbach's alpha and McDonald's Omega), 
# and implements factor analysis for all questionnaires used in the IFAN project. 
# Calculates odd-even split-half reliability for SE-IAT and NLT.
#TODO: Do I want to use polychoric correlations?
#-----------
#-----------
# Output: 
### none, atm
# ---------------------------------------------------------

# initialize

rm(list=ls())
### load packages and sources
library(Hmisc)
library(lavaan)
library(psych)
library(tidyverse)
source(file.path("scripts", "basics", "IFAN_key.R"))

### custom functions
load_file <- function(pathname, pattern) {
  if (is.null(pathname)) {
    df = read.csv(list.files(path = file.path("data", "2_preprocessed-data"),
                             pattern = pattern, full.names = TRUE)) %>%
      as_tibble()
  } else {
    df = read.csv(list.files(path = file.path("data", "2_preprocessed-data", pathname),
                             pattern = pattern, full.names = TRUE)) %>%
      as_tibble()
  }
  if (!("id" %in% names(df))) {
    if ("vpncode" %in% names(df)) {
      df <- df %>% rename(id = vpncode)
    } else if ("participant" %in% names(df)) {
      df <- df %>% rename(id = participant)
    } else if ("x" %in% names(df)) {
      df <- df %>% rename(id = x)
    }
  } 
  df <- df %>% filter(!(id %in% pilots)) %>% key(TRUE)
}

### initialize variables
use_poor_slpq = T #include participants with poor PSQI scores?
pilots <- c('ZZZZZZ', 'ZZXXZZ', '-99', 'LUCA02', 'XX05XX', 'ZZ99ZZ', 'XX88XX')

### load data
df_excluded <- load_file('meta', 'excluded')
df_bfi <- load_file('personality', 'bfi')
df_neo <- load_file('personality', 'neo')
df_rses <- load_file('personality', "rses")
df_iat <- load_file('personality', 'iat.csv')
df_huo <- load_file('personality', 'huo')
df_iso <- load_file('personality', 'iso')

df_rses <- as_tibble(read.csv(list.files(path = file.path("2_preprocessed-data", "personality"), pattern = 'rses', full.names = T)))
df_iso <- as_tibble(read.csv(list.files(path = file.path("2_preprocessed-data", "personality"), pattern = 'iso', full.names = T)))

df_iri <- as_tibble(read.csv(list.files(path = file.path("2_preprocessed-data", "personality"), pattern = 'iri', full.names = T)))
df_npi <- as_tibble(read.csv(list.files(path = file.path("2_preprocessed-data", "personality"), pattern = 'npi', full.names = T)))
df_hsns <- as_tibble(read.csv(list.files(path = file.path("2_preprocessed-data", "personality"), pattern = 'hsns', full.names = T)))
colnames(df_hsns)[1] <- "id"
df_narq <- as_tibble(read.csv(list.files(path = file.path("2_preprocessed-data", "personality"), pattern = 'narq', full.names = T)))

df_nlt <- as_tibble(read.csv(list.files(path = file.path("2_preprocessed-data", "personality"), pattern = 'nlt', full.names = T)))


### exclude participants
if (use_poor_slpq){
  df_excluded <- df_excluded[df_excluded$reason != "Sleep Quality (PSQI)",]
}
df_bfi <- df_bfi[!(df_bfi$id %in% df_excluded$id),2:16]
df_neo <- df_neo[!(df_neo$id %in% df_excluded$id),2:66]
df_rses <- df_rses[!(df_rses$id %in% df_excluded$id),2:12]
df_iat <- df_iat[!(df_iat$id %in% df_excluded$id),] 
df_huo <- df_huo[!(df_huo$id %in% df_excluded$id),2:6]
df_iso <- df_iso[!(df_iso$id %in% df_excluded$id),2:5]


df_npi <- df_npi[!(df_npi$id %in% df_excluded$participant),15:31]
df_hsns <- df_hsns[!(df_hsns$id %in% df_excluded$participant),2:12]
df_narq <- df_narq[!(df_narq$id %in% df_excluded$participant), 2:22]
df_iat <- df_iat[!(df_iat$id %in% df_excluded$id),] 
df_nlt <- df_nlt[!(df_nlt$vpncode %in% df_excluded$participant),] %>% rename(id = vpncode)

# CFA and internal consistency for Big Five Inventory-10
pairs.panels(df_bfi[,1:10])
VSS.scree(cor(df_bfi[,1:10]))
fa.parallel(df_bfi[,1:10], main = "Parallel Analysis of BFI")
FA_BFI <- fa(df_bfi[,1:10], nfactors=5, rotate="Varimax")
loadings <- loadings(FA_BFI)
mod.bfi <- '
  bfi_e =~ bfi_1 + 1*bfi_6
  bfi_n =~ bfi_4 + 1*bfi_9
  bfi_o =~ bfi_5 + 1*bfi_10
  bfi_c =~ bfi_3 + 1*bfi_8
  bfi_a =~ bfi_2 + 1*bfi_7'
fit.mod.bfi = cfa(mod.bfi, data = df_bfi[,1:10], std.lv = F)
bfi_pairs <- list(
  extraversion = c("bfi_1", "bfi_6"),
  agreeableness = c("bfi_2", "bfi_7"),
  conscientiousness = c("bfi_3", "bfi_8"),
  neuroticism = c("bfi_4", "bfi_9"),
  openness = c("bfi_5", "bfi_10")
)
results <- lapply(names(bfi_pairs), function(scale) {
  items <- df_bfi[, bfi_pairs[[scale]]]
  r <- as.numeric(cor(items[,1], items[,2], use = "pairwise.complete.obs"))
  sb <- (2 * r) / (1 + r)  # Spearman–Brown formula
  bfi.alpha <- psych::alpha(df_bfi[,bfi_pairs[[scale]]])
  data.frame(scale = scale, r = round(r, 2), spearman_brown = round(sb, 2), 
             std.alpha = bfi.alpha$total$std.alpha,
             raw.alpha = bfi.alpha$total$raw_alpha)
})
results <- do.call(rbind, results)

# CFA and internal consistency for Rosenberg Self-Esteem Scale
pairs.panels(df_rses)
VSS.scree(cor(df_rses[,1:10])) #scree plot indicates two factors
fa.parallel(df_rses[,1:10], main="Parallel Analysis of RSES") #parallel analysis indicates two factors
FA_RSES <- fa(df_rses[,1:10],nfactors=2,rotate="oblimin") #using an oblimin rotation
loadings <- loadings(FA_RSES)
rses.scale <- psych::alpha(df_rses[,1:10]) #Cronbach's psych::alpha
# McDonald's Omega using two factors as specified above, 
# --- setting equal general-factor loadings for identification
rses.omega_2f <- omega(df_rses[,1:10], nfactors = 2, option = "equal") 
# McDonald's Omega using one factor, as common in the literature
rses.omega_1f <- omega(df_rses[,1:10], nfactors = 1)

# CFA and internal consistency for NEO-Five Factor Inventory
pairs.panels(df_neo[,1:60])
VSS.scree(df_neo[,1:60])
fa.parallel(df_neo[,1:60], main = "Parallel Analysis of NEO-FFI")
FA_NEO <- fa(df_neo[,1:60], nfactors=5, rotate="Varimax")
loadings <- loadings(FA_NEO)
mod.neo <- '
  neo_e =~ neo_2 + neo_7 + neo_12 + neo_17 + neo_22 + neo_27 + neo_32 + neo_37 + neo_42 + neo_47 + neo_52 + neo_57
  neo_n =~ neo_1 + neo_6 + neo_11 + neo_16 + neo_21 + neo_26 + neo_31 + neo_36 + neo_41 + neo_46 + neo_51 + neo_56
  neo_o =~ neo_3 + neo_8 + neo_13 + neo_18 + neo_23 + neo_28 + neo_33 + neo_38 + neo_43 + neo_48 + neo_53 + neo_58
  neo_c =~ neo_4 + neo_9 + neo_14 + neo_19 + neo_24 + neo_29 + neo_34 + neo_39 + neo_44 + neo_49 + neo_54 + neo_59
  neo_a =~ neo_5 + neo_10 + neo_15 + neo_20 + neo_25 + neo_30 + neo_35 + neo_40 + neo_45 + neo_50 + neo_55 + neo_60'
fit.mod.neo <- cfa(mod.neo, data = df_neo[,1:60], std.lv = F)
neo_n.alpha <- psych::alpha(df_neo[,c(1,6,11,16,21,26,31,36,41,46,51,56)])
neo_e.alpha <- psych::alpha(df_neo[,c(2,7,12,17,22,27,32,37,42,47,52,57)])
neo_o.alpha <- psych::alpha(df_neo[,c(3,8,13,18,23,28,33,38,43,48,53,58)])
neo_a.alpha <- psych::alpha(df_neo[,c(4,9,14,19,24,29,34,39,44,49,54,59)])
neo_c.alpha <- psych::alpha(df_neo[,c(5,10,15,20,25,30,35,40,45,50,55,60)])
neo.omega <- omega(df_neo[,1:60], factor = 5)
neo_n.omega <- omega(df_neo[,c(1,6,11,16,21,26,31,36,41,46,51,56)], nfactors = 1)
neo_e.omega <- omega(df_neo[,c(2,7,12,17,22,27,32,37,42,47,52,57)], nfactors = 1)
neo_o.omega <- omega(df_neo[,c(3,8,13,18,23,28,33,38,43,48,53,58)], nfactors = 1)
neo_a.omega <- omega(df_neo[,c(4,9,14,19,24,29,34,39,44,49,54,59)], nfactors = 1)
neo_c.omega <- omega(df_neo[,c(5,10,15,20,25,30,35,40,45,50,55,60)], nfactors = 1)

# CFA and internal consistency for isolation items
pairs.panels(df_iso[,1:3])
VSS.scree(cor(df_iso[,1:3]))
fa.parallel(df_iso[,1:3], main = "Parallel Analysis of isolation items")
FA_ISO <- fa(df_iso[,1:3], nfactors=1, rotate="Varimax")
loadings <- loadings(FA_ISO)
iso.alpha <- psych::alpha(df_iso[,1:3])
iso.omega <- omega(df_iso[,1:3], nfactors = 1)

# CFA and internal consistency for sociometer items
pairs.panels(df_huo[,1:4])
VSS.scree(cor(df_huo[,1:4]))
fa.parallel(df_huo[,1:4], main = "Parallel Analysis of sociometer items")
FA_HUO <- fa(df_huo[,1:4], nfactors=1)
loadings <- loadings(FA_HUO)
mod.huo <- '
  huo =~ iso_4 + iso_5 + iso_6 + iso_7'
fit.mod.huo = cfa(mod.huo, data = df_huo[,1:4], std.lv = F)
huo.alpha <- psych::alpha(df_huo[,1:4])
huo.omega <- omega(df_huo[,1:4], nfactors = 1)

# CFA and internal consistency for Interpersonal Reactivity Index
pairs.panels(df_iri[,1:16])
VSS.scree(cor(df_iri[,1:16]))
fa.parallel(df_iri[,1:16], main = "Parallel Analysis of IRI")
FA_IRI <- fa(df_iri[,1:16], nfactors=4, rotate="Varimax")
loadings <- loadings(FA_IRI)
iri_ec.alpha <- psych::alpha(df_iri[,c(1,5,9,11)])
iri_pt.alpha <- psych::alpha(df_iri[,c(4,10,14,16)])
iri_f.alpha <- psych::alpha(df_iri[,c(2,7,12,15)])
iri_pd.alpha <- psych::alpha(df_iri[,c(3,6,8,13)])
iri_ec.omega <- omega(df_iri[,c(1,5,9,11)])
iri_pt.omega <- omega(df_iri[,c(4,10,14,16)])
iri_f.omega <- omega(df_iri[,c(2,7,12,15)])
iri_pd.omega <- omega(df_iri[,c(3,6,8,13)])
iri.omega <- omega(df_iri[,1:16], nfactors = 4)

# CFA and internal consistency for Narcissistic Personality Inventory
### Attention! Omega estimation is probably incorrect, ultra-Heywood case detected
pairs.panels(df_npi[,1:13])
VSS.scree(cor(df_npi[,1:13]))
fa.parallel(df_npi[,1:13], main = "Parallel Analysis of NPI")
FA_NPI <- fa(df_npi[,1:13], nfactors = 2, rotate="oblimin")
loadings.npi <- loadings(FA_NPI)
npi.alpha <- psych::alpha(df_npi[,1:13])
npi_la.alpha <- psych::alpha(df_npi[,1:4])
npi_ge.alpha <- psych::alpha(df_npi[,5:9])
npi_ee.alpha <- psych::alpha(df_npi[,10:13])
npi.omega_3f <- omega(df_npi[,1:13], nfactors = 3) # theory-guided
npi.omega_2f <- omega(df_npi[,1:13], nfactors = 2, var = "equal") # empirical

# CFA and internal consistency for Hypersensitive Narcissism Scale
pairs.panels(df_hsns[,1:10])
VSS.scree(cor(df_hsns[,1:10]))
fa.parallel(df_hsns[,1:10], main = "Parallel Analysis of HSNS")
FA_HSNS <- fa(df_hsns[,1:10], nfactors=1)
loadings <- loadings(FA_HSNS)
hsns.alpha <- psych::alpha(df_hsns[,1:10])
hsns.omega_1f <- omega(df_hsns[,1:10], nfactors = 1) # theory-guided
hsns.omega_3f <- omega(df_hsns[,1:10], nfactors = 3) # empirical

# CFA and internal consistency for Narcissism Admiration and Rivalry Questionnaire
pairs.panels(df_narq[,1:18])
VSS.scree(cor(df_narq[,1:18]))
fa.parallel(df_narq[,1:18], main = "Parallel Analysis of NARQ")
FA_NARQ <- fa(df_narq[,1:18], nfactors=2, rotate="oblimin")
loadings.narq <- loadings(FA_NARQ)
narq.alpha <- psych::alpha(df_narq[,1:18])
narq_adm.alpha <- psych::alpha(df_narq[,c(1:3, 5, 7, 8, 15, 16, 18)])
narq_riv.alpha <- psych::alpha(df_narq[,c(4,6,9:14,17)])
narq.omega_2f <- omega(df_narq[,1:18], nfactors = 2, var = "equal") # theory-guided
narq.omega_4f <- omega(df_narq[,1:18], nfactors = 4) # empirical

# split-half reliability for Self-Esteem Implicit Association Test using an odd-even split
rel_se.iat <- cor.test(df_iat$dscore.odd, df_iat$dscore.even)$estimate
rel_se.iat_corr <- 2*rel_se.iat/(1+rel_se.iat) # Spearman Brown correction

# kind-of split-half reliability for Name Letter Test using Initial Preference Scores
rel_nlt <- as_tibble(rcorr(as.matrix(df_nlt[,c(87:89)]))$r)
rel_nlt_corr <- rel_nlt %>% 
  mutate_all(function(x) (2*x)/(1+x)) # Spearman Brown correction

