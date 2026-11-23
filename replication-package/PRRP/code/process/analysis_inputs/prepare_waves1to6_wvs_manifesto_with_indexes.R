library(tidyverse)
library(haven)
library(data.table)
library(Hmisc)
source("code/util/commonVars.R")
library(lfe)
source("code/process/analysis_inputs/index_functions.R")
WVS <- read_dta(file.path(datasets_path, "WVS", "wvs_clean_1to6_waves.dta"))

# Add wave numbers
WVS <- WVS %>%
  left_join(waveLabelsDF, by = c("BB_Wave_num"="waveNum"), keep = F) %>% 
  as.data.table()

# Load data at the manifesto*year level for merging with voters
# If running analysis on ALL waves (1-6), ensure correct data file:
# Load data at the manifesto*year level for merging with voters
MANIFESTO = readRDS(manifestoModelData1to6) %>% 
  mutate(traditional_index = per603 - per604,
         new_culture_index = per601 + per608 - per501 - per503 - per602 -per607 - per705) %>% 
  select(Y_manifesto_key, manifesto_country_name, BB_Wave_num,
         rile, economy_index, society_index, traditional_index, new_culture_index)

# Standardize manifesto
MANIFESTO <- MANIFESTO %>% 
  group_by(BB_Wave_num, manifesto_country_name) %>% 
  mutate(
    economy_indexS = as.numeric(scale(economy_index)),
    society_indexS = as.numeric(scale(society_index)),
    traditional_indexS = as.numeric(scale(traditional_index)),
    new_culture_indexS = as.numeric(scale(new_culture_index))
  )

DATA <- merge(WVS, MANIFESTO, by = c("Y_manifesto_key", "BB_Wave_num"))

DATA = as.data.table(DATA)

# # XXX - Remove countries with too many missing vars in waves 1-3
countries_to_remove <- c("Switzerland", "Lithuania", "Estonia", "Poland", "Norway")

# Remove rows where BB_Country matches any country in the list
DATA <- DATA %>% dplyr::filter(!BB_Country %in% countries_to_remove)

DATA = as.data.table(DATA)

# *************************************************************************
#  CREATE INDEXES ----
# *************************************************************************

# plug cleanData into DATA, to edit DATA while keeping a cleanData unchanged for later

# Get all the names of opinion columns in our merged dataset
opinions = grep("X_op_", names(DATA), value=TRUE)

economyOpinions <- economyOpinions[economyOpinions %in% opinions]
cultureOpinions <- cultureOpinions[cultureOpinions %in% opinions]

economyOpinionsS = paste0(economyOpinions, "S")
cultureOpinionsS = paste0(cultureOpinions, "S")

for (opinion in c(economyOpinions, cultureOpinions)) {
  DATA[, eval(paste0(opinion, "S")) := corpcor::wt.scale(get(opinion), w = BB_Weight_CW), by = "BB_Wave_num"]
}



# .Index based on regressions predicting left right scale ----
# .........................................................................

# Regressions with country fixed effects to construct economic and cultural index
economyCoef <- felm(data = WVS, as.formula(paste0("X_general_self_political_scale ~ BB_Int_Wave +", paste(economyOpinions, collapse = " + "), " | BB_Country")), weights = WVS$BB_Weight_CW)
cultureCoef <- felm(data = WVS, as.formula(paste0("X_general_self_political_scale ~ BB_Int_Wave +", paste(cultureOpinions, collapse = " + "), " | BB_Country")), weights = WVS$BB_Weight_CW)

# Use the coefficients to construct the indexes
DATA[, X_econ_weighted_index := rowSums(sapply(economyOpinions, function(x) get(x)*economyCoef$coefficients[x,]))]
DATA[, X_culture_weighted_index := rowSums(sapply(cultureOpinions, function(x) get(x)*cultureCoef$coefficients[x,]))]

# Standardize index
DATA[, X_econ_weighted_index := corpcor::wt.scale(X_econ_weighted_index, w = BB_Weight_CW), by="BB_Wave_num"]
DATA[, X_culture_weighted_index := corpcor::wt.scale(X_culture_weighted_index, w = BB_Weight_CW), by="BB_Wave_num"]



# .Index based on equal weights to each variable ----
# .........................................................................

# Find if the correlation of each variable in our sets of categories is positive or negative with self reported political position
economyDirections = sapply(economyOpinions, function(x) sign(cor(DATA[[x]], DATA[["X_general_self_political_scale"]], use="complete.obs")))
cultureDirections = sapply(cultureOpinions, function(x) sign(cor(DATA[[x]], DATA[["X_general_self_political_scale"]], use="complete.obs")))

# Standardize each index
DATA[, X_op_economy_index := createStandardIndex(.SD, control=NULL, directions = economyDirections, weights = BB_Weight_CW), .SDcols = economyOpinions, by=c("BB_Wave_num", "BB_Country")]
DATA[, X_op_culture_index := createStandardIndex(.SD, control=NULL, directions = cultureDirections, weights = BB_Weight_CW), .SDcols = cultureOpinions, by=c("BB_Wave_num", "BB_Country")]



# .Index based on PCA ----
# .........................................................................

# Option 1: scaling using internal PCA option (won't take into account survey weights)
#X = as.matrix(DATA %>% select(all_of(economyOpinions)))
#X = scale(X) #centres the vriables and scales them to unit variance
#pca_stats = prcomp(x = X, center=TRUE) #from the stats package
#pca_svd = svd(X) # the svd of X
#pca_svd$v
#pca_stats$rotation


# Option 2 that we'll use, not scaling but using variabels standardized with weights
X = as.matrix(DATA %>% select(all_of(economyOpinionsS)))
econ_pca = prcomp(x = X, center=FALSE) #from the stats package
pca_svd = svd(X) 

# the loadings of cvs and prcomp are the same
pca_svd$v
econ_pca$rotation

DATA[, X_econ_pca := econ_pca$x[,"PC1"]]


if (cor(DATA$X_econ_pca, DATA$X_op_income_equality_indexS, use="complete.obs") < 0) {
  print("reversing sign")
  DATA[, X_econ_pca := -X_econ_pca]
}

stopifnot(sign(cor(DATA$X_econ_pca, DATA$X_op_income_equality_indexS, use="complete.obs"))==
            sign(cor(DATA$X_econ_pca, DATA$X_op_GovtVsPersonalRespS, use="complete.obs")))



culture_pca <- prcomp(DATA %>% select(all_of(cultureOpinionsS)), center = FALSE)
DATA[, X_culture_pca := culture_pca$x[,"PC1"]]

# Sign is arbitrary, change so higher is more conservative
# X_op_immigrant_jobs_index - jobs prioritize native (cons)
# X_op_Pride_Nationality - proud of naion (cons)
# X_op_neighbors_diff_race - don't want diff race (cons)

if (cor(DATA$X_culture_pca, DATA$X_op_immigrant_jobs_index, use="complete.obs") < 0) {
  print("reversing sign")
  DATA[, X_culture_pca := -X_culture_pca]
}

stopifnot(sign(cor(DATA$X_culture_pca, DATA$X_op_immigrant_jobs_index, use="complete.obs"))==
            sign(cor(DATA$X_culture_pca, DATA$X_op_Pride_Nationality, use="complete.obs")))


DATA[, X_econ_pcaS := corpcor::wt.scale(X_econ_pca, w = BB_Weight_CW), by = c("BB_Wave_num", "BB_Country")]
DATA[, X_culture_pcaS := corpcor::wt.scale(X_culture_pca, w = BB_Weight_CW), by = c("BB_Wave_num", "BB_Country")]

saveRDS(DATA, file.path(cleandata_path, "manifesto", "waves1to6_wvs_manifesto_with_indexes.rds"))
