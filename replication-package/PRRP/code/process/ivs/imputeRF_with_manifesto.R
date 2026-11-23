library(data.table)
library(randomForest)
library(dplyr)
rm(list=ls())
source("code/util/commonVars.R")
source("code/process/paths.R")
# File takes about 8 minutes
print(timestamp())

data <- as.data.table(haven::read_dta(paste0(intermediate_data_path, "/temp/for_impute_ivs_with_manifesto.dta"))) %>%
  mutate(Y_manifesto_key = ifelse(is.na(Y_manifesto_key), 999, Y_manifesto_key))
currentData = data[0]

set.seed(222)

# Testing 1 2 3
# wave = 6
# country = "Germany"
#loop over waves 
for (wave in sort(data[, unique(BB_Wave_num)])) {
  print(paste0("Wave: ", wave))
  for (country in data[BB_Wave_num==wave, unique(BB_Country)]) {
    print(paste0("Country: ", country))
    
    #wave=1; country="Germany"
    wave_country = data[BB_Country==country & BB_Wave_num==wave]
    
    predictors = wave_country %>% select(starts_with("X_"))
    predictors = as.data.frame(predictors)
    allNA = sapply(predictors, function(x) all(is.na(x)))
    predictors = predictors[allNA==FALSE]
    
    # Imputation - have about 88 country-waves
    # Germany wave 1 153 sec; ntree=150 -> 80 sec; ntree=150 + iter=3 ->45; ntree=75 + iter=2 ->14
    Y_manifesto_key = as.factor(wave_country$Y_manifesto_key)
    X.tmp <- rfImpute(predictors, Y_manifesto_key, ntree=75, iter = 2)
    #X.tmp <- rfImpute(predictors, Y_manifesto_key, ntree=5, iter = 1)
    
    X.tmp = as.data.table(cbind(X.tmp, id=wave_country$id))
    currentData = rbindlist(list(currentData, X.tmp), use.names = TRUE, fill = TRUE)
  }
}


keepData = data.table::copy(currentData)
keepData = currentData %>% select(id, starts_with("X_")) %>% select(-ends_with("_unimp"))

haven::write_dta(keepData, paste0(intermediate_data_path, "/temp/after_rf_impute_ivs_with_manifesto.dta"))

print(timestamp())  
