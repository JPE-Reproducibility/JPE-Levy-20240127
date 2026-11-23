
rm(list=ls())


CONTINUE_PREVIOUS_RUN  = 0


library(nleqslv)
library(future.apply)

#load the function
source("code/util/commonVars.R")
source("code/model/func_nuclear_and_pgd.R")
source("code/model/leave_country_out.R")

#load data
source("code/model/clean_data_model.R")

#get lambda and initial parameters 
wave.globals = list()
filepath = paste0(model_results_path, "/model_wave_")

#loop over waves 4 and 6 
for (g in c(4,6)){
  #load model
  mod = readRDS(paste0(filepath,g,".Rds"))
  #load 
  wave.globals[[g]] = list(LMBD = mod$lmbd,
                           INIT.PHI = mod$mod$phi,
                           INIT.INTERCEPT =  mod$mod$intercept,
                           Z.country = sapply(mod$Z.os, function(s) substr(s,1,nchar(s)-1)))
                           
}
#saveRDS(wave.globals,paste0(model_results_path, "/loo/wave_globals.Rds"))
prepare.run.one.country.loo <- function(wave, outside) {
  
  # Training: All other countries 
  X.trn = X[X.all$BB_Wave_num == wave & X.all$BB_Country != outside,]
  Z.trn = Z[Z.all$BB_Wave_num == wave & Z.all$BB_Country != outside,]
  
  y.trn = X.all[X.all$BB_Wave_num== wave & X.all$BB_Country != outside,"Y_manifesto_key"]
  
  # Standardize X
  mx = apply(X[X.all$BB_Wave_num == wave,], 2, mean)
  sdx = apply(X[X.all$BB_Wave_num == wave,], 2, sd)
  
  X.trn = t(((t(X.trn)-mx)/sdx))

  # Standardize Z
  mz = apply(Z[Z.all$BB_Wave_num == wave,], 2, mean)
  sdz = apply(Z[Z.all$BB_Wave_num == wave,], 2, sd)
  Z.trn = t(((t(Z.trn)-mz)/sdz))

  # Set Y matrix
  Y.trn = matrix(0,nrow(X.trn),nrow(Z.trn))
  colnames(Y.trn) = sapply(rownames(Z.trn),function(s) substr(s,1,nchar(s)-1))
  for (i in 1:length(y.trn)){
    Y.trn[i,as.character(y.trn[i])] = 1
  }

  # Names of countries
  X.group.trn = X.all$BB_Country[X.all$BB_Wave_num == wave & X.all$BB_Country != outside]

  Z.group.trn = Z.all$BB_Country[Z.all$BB_Wave_num == wave & Z.all$BB_Country != outside]
  
  X.wave = paste0(X.all$BB_Country[X.all$BB_Wave_num == wave & X.all$BB_Country != outside], 
                  X.all$BB_Wave[X.all$BB_Wave_num == wave & X.all$BB_Country != outside])
  
  #weights 
  #define weights
  W. = X.all$BB_Weight[X.all$BB_Wave_num == wave & X.all$BB_Country != outside]
  
  N = length(W.)
  n = length(unique(X.wave))
  wave.weight = aggregate(W., list(X.wave),FUN = sum)
  ww = wave.weight[,2]
  names(ww) = wave.weight[,1]
  W.wave = ww[X.wave] 
  W. = W./W.wave*N/n
  #normalize to equal size in every country 
  n = length(unique(X.group.trn))
  country.weight = aggregate(W., list(X.group.trn),FUN = sum)
  cw = country.weight[,2]
  names(cw) = country.weight[,1]
  W.country = cw[X.group.trn] 
  W.trn = W./W.country*N/n
  
  #take intercept only of countries left in
  init.intercept = wave.globals[[wave]]$INIT.INTERCEPT[wave.globals[[wave]]$Z.country != outside]
  
  # Run one wave*country
  res = estimate.model(X.trn, Z.trn, Y.trn, X.group.trn,Z.group.trn,
                        wave.globals[[wave]]$LMBD,wave.globals[[wave]]$INIT.PHI,init.intercept,wave = wave,sample.weight = W.trn,
                        conv.crit=conv.crit.model, conv.crit2=conv.crit2.model,
                        lag=lag.model, max.iter=max.iter.model)
  fsls = list(X = X.trn, Z= Z.trn, mod = res, lmbd = wave.globals[[wave]]$LMBD, X.os = X.group.trn, Z.os = Z.group.trn, Y = Y.trn, W = W.trn)

  # Save dataset
  saveRDS(fsls,paste0(paste0(model_results_path, "/loo/loo_wave_"), wave, "county_", outside, ".Rds"))
  
  #gc()
  #return; 
}



avilCores = availableCores()
plan(multisession, workers = estimationWorkers)

waveCountries = data.frame()
#outsideCountries = c("France", "Germany")
outsideCountries = unique(X.all$BB_Country)

for (country in outsideCountries) {
  
  for (wave in c(4,6)) {
    if (sum(X.all$BB_Wave_num== wave & X.all$BB_Country==country)==0) {
      next
    }
    
    waveCountry = list()
    waveCountry$wave = wave
    waveCountry$country = country
    
    waveCountries = rbind(waveCountries, waveCountry)
  }
}


if (CONTINUE_PREVIOUS_RUN) {
  library(data.table)
  completed = as.data.table(list.files(paste0(model_results_path, "/loo")))
  setnames(completed, "V1", "fileName")
  completed[, wave := gsub(".*wave_(.*)county.*", "\\1", fileName)]
  completed[, country := gsub(".*_(.*)\\.Rds", "\\1", fileName)]
  waveCountries = as.data.table(merge(waveCountries, completed, by = c("wave", "country"), all.x=TRUE))
  waveCountries = waveCountries[is.na(fileName)]
}


system.time(future_mapply(prepare.run.one.country.loo, waveCountries$wave, waveCountries$country, future.seed=TRUE))

#system.time(future_lapply(waveCountries, prepare.run.one.country.loo)








# *************************************************************************
#  RUN SEQUENTIALY  ----
# *************************************************************************

if (0) {
  #cntrs = c("Germany","France","Austria","UK","Sweden")
  cntrs = unique(X.all$BB_Country)
  
  X. = X[X.all$BB_Wave_num == 6 & X.all$BB_Country %in% cntrs,]
  Z. = Z[Z.all$BB_Wave_num == 6 & Z.all$BB_Country %in% cntrs,]
  y = X.all[X.all$BB_Wave_num== 6 & X.all$BB_Country %in% cntrs,"Y_manifesto_key"]
  Y. = matrix(0,nrow(X.),nrow(Z.))
  colnames(Y.) = sapply(rownames(Z.),function(s) substr(s,1,nchar(s)-1))
  for (i in 1:length(y)){
    Y.[i,as.character(y[i])] = 1
  }
  X.group. = X.all$BB_Country[X.all$BB_Wave_num == 6 & X.all$BB_Country %in% cntrs]
  Z.group. = Z.all$BB_Country[Z.all$BB_Wave_num == 6 & Z.all$BB_Country %in% cntrs]
  
  fsl = first.stage.loo(X.,Z.,Y.,X.group.,Z.group., 
                        conv.crit = 10,conv.crit2 = 1,lag = 10,
                        max.iter =1000, s = 0.1,cv.conv.rat = 10,
                        standardize = T,
                        accelerate = F, wave=6)
  
  saveRDS(fsl, paste0(model_results_path, "/loo/fsl.rds"))
}
