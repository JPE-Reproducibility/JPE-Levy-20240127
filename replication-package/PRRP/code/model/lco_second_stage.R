#estimate betas (second stage estimation) for leave-country-out
#Oren Danieli
#January 2023 

rm(list=ls())

#load the function
source("code/util/commonVars.R")
source("code/model/func_nuclear_and_pgd.R")

#load data
source("code/model/clean_data_model.R")

#list of countries
outsideCountries = unique(X.all$BB_Country)

filepath = paste0(model_results_path, "/loo/loo_wave_")
grps = c(4,6)
#dimension of second stage estimation
k = 5 
for (outside in outsideCountries){
  X.list = lapply(grps, function(g) {readRDS(paste0(filepath,g,"county_", outside,".Rds"))$X})
  Z.list = lapply(grps, function(g) {readRDS(paste0(filepath,g,"county_", outside,".Rds"))$Z})
  
  mod.list = lapply(grps, function(g) {readRDS(paste0(filepath,g,"county_", outside,".Rds"))$mod})
  phi.list = lapply(mod.list, function(m) {m$phi})
  phi = phi.list[[length(phi.list)]]
  W.list = lapply(grps, function(g) {readRDS(paste0(filepath,g,"county_", outside,".Rds"))$W})
  #account for standardization (maybe needs to be done within the function)
  #after this, phi can multiply X and Z themselves (not standardized)
  #X was standardized with the weighted SD in estimation (wt.scale), Z with the unweighted SD (scale)
  phi.list = lapply(1:length(grps), function(g) {
    diag(inv.wt.sd(X.list[[g]],W.list[[g]]))%*%
      phi.list[[g]]%*%
      diag(apply(Z.list[[g]],2,sd)^-1)
  })
  Z.os.list = lapply(grps, function(g) {readRDS(paste0(filepath,g,"county_", outside,".Rds"))$Z.os})

  Y.list = lapply(grps, function(g) {readRDS(paste0(filepath,g,"county_", outside,".Rds"))$Y})
  for (i in 1:2){
    colnames(Y.list[[i]]) = rownames(Z.list[[i]])
  }

  FE.list = lapply(mod.list,function(m) {m$intercept})
  #demean all Xs by the global mean 
  mean.X.wave = lapply(1:length(grps),function(g) t(apply(X.list[[g]],2,mean)))
  mean.X.global = (Reduce("+",mean.X.wave)/length(grps))[1,]

  
  #change to get the intercept without standardization 
  FE.list = lapply(1:2, function(g) {t(FE.list[[g]] - 
                                                      (mean.X.wave[[g]]-mean.X.global) %*%
                                                      phi.list[[g]] %*% t((Z.list[[g]])))})
  
  #party.id.list 
  Z.all$nms = paste0(Z.all$Y_manifesto_key)
  key = Z.all[,c("nms","Y_manifesto_party_id")]
  key = key[!duplicated(key),]
  rownames(key) = key$nms
  party.id.list = lapply(Z.list, function(z){
    #  v = as.integer(key[rownames(z),"Y_manifesto_party_id"])
    v = as.character(key[substr(rownames(z),1,nchar(rownames(z))-1),
                         "Y_manifesto_party_id"])
    names(v) = rownames(z)
    v})
  
  #verify no NA
  for (i in 1:2){
    stopifnot(!any(is.na(party.id.list[[i]])))
  }
  
  
  #drop wave number for os list
  Z.country = lapply(Z.os.list,function(d) sapply(d,function(s) substr(s,1,nchar(s)-1)))
  
  wgt.list = lapply(1:length(Y.list),function(i){t(apply(Y.list[[i]]*W.list[[i]],2,sum))}) #need to verify this works with the name
  #FE.dec = Z.coeff(Z.list,Z.group.list,party.id.list,wgt.list,phi.list,FE.list,k)
  FE.dec = Z.coeff(Z.list,Z.country,party.id.list,wgt.list,phi,FE.list,k)
  saveRDS(FE.dec,paste0(paste0(model_results_path, "/loo/loo_second_sage_county_"), outside, ".Rds"))
}
