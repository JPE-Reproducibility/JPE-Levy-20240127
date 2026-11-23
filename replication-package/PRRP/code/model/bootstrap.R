#bootstrap estimation to get CIs
#Oren 
#February 2023


library(future.apply)

rm(list = ls())
source("code/util/commonVars.R")
source("code/model/func_nuclear_and_pgd.R")

intermediate_bs_dir <- function(seed) {
  file.path(bootstrap_path, "BS", as.character(seed))
}

#load data
old_input_option <- getOption("populist.inputs.only",FALSE)
options(populist.inputs.only=TRUE)
tryCatch(source("code/model/clean_data_model.R"),
         finally=options(populist.inputs.only=old_input_option))
source("code/model/bootstrap_checks.R")
bootstrap_check_baseline(model_results_path,list(X=X,Z=Z,X.all=X.all,Z.all=Z.all))
bootstrap_template <- readRDS(file.path(decomposition_path,"dec_4_6.Rds"))
bs_root <- file.path(bootstrap_path,"BS")
dir.create(bs_root,recursive=TRUE,showWarnings=FALSE)
bootstrap_timing <- function(seed,stage,started,status="complete") {
  write(paste(format(Sys.time(),tz="UTC",usetz=TRUE),"seed",seed,stage,
              "seconds",round(proc.time()[["elapsed"]]-started,3),"pid",Sys.getpid(),status),
        file=file.path(Sys.getenv("POPULIST_LOG_DIR"),paste0("bootstrap-",seed,".log")),append=TRUE)
}
#wave number - This should be set differently for each wave; g = 6
#seed - for randomization 

#draw bootstrap from each survey x country and generate ewights such that all countries are equal weights and within countries two surveys are same weight
BSweights = function(orig.weights, waves, countries, seed){
  set.seed(seed)
  #vector of wave and country
  wave.cnt = paste0(waves,countries)
  all.sets = unique(wave.cnt)
  bs.draw = c()
  #loop over all combinations of wave and countries 
  for (s in all.sets){
    #find all observations from that set
    idx = which(wave.cnt == s)
    #draw the same number of observations from this set 
    bs.draw = c(bs.draw, sample(idx,length(idx),T))
  }
  #generate weights. create a table of number of times each observation was selected. To include zeros for observations that were not drawn I include them at first and then subtract one. 
  obs.count = table(c(1:length(orig.weights),bs.draw))-1
  #multiply by oriiginal weights and return
  W. = as.vector(obs.count*orig.weights)
  #normalize to equal size in every country 
  N = length(W.)
  n = length(unique(waves))
  wave.weight = aggregate(W., list(waves),FUN = sum)
  ww = wave.weight[,2]
  names(ww) = wave.weight[,1]
  W.wave = ww[waves] 
  W. = W./W.wave*N/n
  #normalize to equal size in every country 
  n = length(unique(countries))
  country.weight = aggregate(W., list(countries),FUN = sum)
  cw = country.weight[,2]
  names(cw) = country.weight[,1]
  W.country = cw[countries] 
  W. = W./W.country*N/n
}

estimateWaveBS <- function(g,seed) {
  started <- proc.time()[["elapsed"]]
  
  print("***estimating wave***")
  print(g)
  
  X. = X[X.all$BB_Wave_num == g,]
  Z. = Z[Z.all$BB_Wave_num == g,]
  X.all.g = X.all[X.all$BB_Wave_num == g,]
  Z.all.g = Z.all[Z.all$BB_Wave_num == g,]
  
  #define outcome
  y = paste0(X.all.g$Y_manifesto_key,X.all.g$BB_Wave_num)
  Y = matrix(0, nrow(X.),nrow(Z.))
  colnames(Y) = row.names(Z.)
  for (i in 1:length(y)){
    Y[i,y[i]] = 1
  }
  
  stopifnot(all(apply(Y,1,sum)==1))
  
  #define option set
  X.os = paste0(X.all.g$BB_Country,X.all.g$BB_Wave_num)
  Z.os = paste0(Z.all.g$BB_Country,Z.all.g$BB_Wave_num)
  
  X.wave = paste0(X.all.g$BB_Country,X.all.g$BB_Wave)
  
  #define weights
  W. = X.all$BB_Weight_CW[X.all$BB_Wave_num == g]
  W. = BSweights(W.,X.wave,X.os,seed)
  
  set.seed(2023)
  # cv.res <- cv.lambda(X., Z., Y, X.os, Z.os, wave=g,
  #                     n.values = n.values.lambda, conv.crit = conv.crit.lambda, conv.crit2 = conv.crit2.lambda,
  #                     lag = lag.lambda, max.iter = max.iter.lambda, sample.weight = W., bootstrapSeed=seed, file.name = "logs/cvLambda.log")
  
  # lmbd = cv.res$lmbd[which.max(unlist(cv.res$scores))]
  
  # Use the lambda and starting values from the pre-estimated full-sample model.
  base_model <- readRDS(paste0(paste0(model_results_path, "/model_wave_"), g, ".Rds"))

  lmbd <- base_model$lmbd
  phi_start <- base_model$mod$phi
  intercept_start <- base_model$mod$intercept

  #estimate model fully 
  
  
  #improve perceision of the model on the full data 
  # res <- estimate.model(X.,Z.,Y,X.os,Z.os,lmbd, cv.res$phi[[as.character(lmbd)]], cv.res$intercept[[as.character(lmbd)]], 
  #                       sample.weight = W., wave=g,
  #                       conv.crit = conv.crit.model, conv.crit2 = conv.crit2.model, lag = lag.model, max.iter = max.iter.model,
  #                       bootstrapSeed=seed)
  res <- estimate.model(
    X., Z., Y, X.os, Z.os,
    lmbd,
    phi_start,
    intercept_start,
    sample.weight = W.,
    wave = g,
    conv.crit = conv.crit.model,
    #conv.crit2 = conv.crit2.model * 10,
    conv.crit2 = conv.crit2.model,
    lag = lag.model,
    max.iter = max.iter.model,
    bootstrapSeed = seed
  )

  
  full.res = list(X = X., Z= Z., mod = res, lmbd = lmbd, X.os = X.os, Z.os = Z.os, Y = Y, W = W.)
  saveRDS(full.res, file.path(intermediate_bs_dir(seed), paste0("model_wave_", g, ".Rds")))
  bootstrap_check_fit(full.res,paste("seed",seed,"wave",g))
  bootstrap_timing(seed,paste("wave",g),started)
  #strength of dimensions
  # plot(svd(res$phi)$d,pch = 19,main = g)
  
  logging(text = paste0("Finished wave", g))
}  


decomposeBS = function(seed){
  started <- proc.time()[["elapsed"]]
  grps <- 4:6
  print("decomposition")
  filepath = file.path(intermediate_bs_dir(seed), "model_wave_")
  X.list = lapply(grps, function(g) {readRDS(paste0(filepath,g,".Rds"))$X})
  Z.list = lapply(grps, function(g) {readRDS(paste0(filepath,g,".Rds"))$Z})
  
  mod.list = lapply(grps, function(g) {readRDS(paste0(filepath,g,".Rds"))$mod})
  phi.list = lapply(mod.list, function(m) {m$phi})
  phi = phi.list[[length(phi.list)]]
  W.list = lapply(grps, function(g) {readRDS(paste0(filepath,g,".Rds"))$W})
  #account for standardization (maybe needs to be done within the function)
  #after this, phi can multiply X and Z themselves (not standardized)
  #X was standardized with the weighted SD in estimation (wt.scale), Z with the unweighted SD (scale)
  phi.list = lapply(grps-min(grps)+1, function(g) {
    diag(inv.wt.sd(X.list[[g]],W.list[[g]]))%*%
      phi.list[[g]]%*%
      diag(apply(Z.list[[g]],2,sd)^-1)
  })
  X.os.list = lapply(grps, function(g) {readRDS(paste0(filepath,g,".Rds"))$X.os})
  Z.os.list = lapply(grps, function(g) {readRDS(paste0(filepath,g,".Rds"))$Z.os})
  y.list = lapply(grps, function(g) {paste0(X.all[X.all$BB_Wave_num== g,"Y_manifesto_key"],X.all[X.all$BB_Wave_num== g,"BB_Wave_num"])})
  
  Y.list = lapply(grps-min(grps)+1, function(g) {
    res = matrix(0, nrow(X.list[[g]]),nrow(Z.list[[g]]))
    colnames(res) = row.names(Z.list[[g]])
    res}
  )
  for (g in grps-min(grps)+1){
    for (i in 1:length(y.list[[g]])){
      Y.list[[g]][i,y.list[[g]][i]] = 1
    }
    stopifnot(all(apply(Y.list[[g]],1,sum)==1))
    
  }

  FE.list = lapply(mod.list,function(m) {m$intercept})
  #demean all Xs by the global mean 
  mean.X.wave = lapply(grps-min(grps)+1,function(g) t(apply(X.list[[g]],2,function(x) weighted.mean(x,W.list[[g]]))))
  mean.X.global = (Reduce("+",mean.X.wave)/length(grps))[1,]
  X.list = lapply(grps-min(grps)+1,function(g) t(t(X.list[[g]])-mean.X.global))
  
  #change to get the intercept without standardization 
  FE.list = lapply(grps-min(grps)+1, function(g) {t(FE.list[[g]] - 
                                                      (mean.X.wave[[g]]-mean.X.global) %*%
                                                      phi.list[[g]] %*% t((Z.list[[g]])))})
  
  #saveRDS(FE.list,paste0(model_results_path, "/preciseFromServer/deltas.Rds"))
  
  
  
  #full.Z$nms = paste0(full.Z$Y_manifesto_key,full.Z$BB_Wave_num)
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
  for (i in grps-min(grps)+1){
    stopifnot(!any(is.na(party.id.list[[i]])))
  }
  
  
  #drop wave number for os list
  X.country = lapply(X.os.list,function(d) sapply(d,function(s) substr(s,1,nchar(s)-1)))
  Z.country = lapply(Z.os.list,function(d) sapply(d,function(s) substr(s,1,nchar(s)-1)))

  party.dat = unique(X.all[,c("Y_manifesto_party_id","BB_Country","Y_FR_Pop")])
  rownames(party.dat) = party.dat[,1]
  party.dat = party.dat[,-1]
  dec4.6 =  decomp.do.all(X.list,Z.list,Y.list,phi.list,FE.list,party.id.list,party.dat,X.country,Z.country,phi,1,3,bar = -1,
                          W.list = W.list, dec.order = c("resid", "Z","X","param"),
                          return.res = T)
  print(dec4.6)
  saveRDS(dec4.6, file.path(intermediate_bs_dir(seed), "dec_4_6.Rds"))
  bootstrap_timing(seed,"decomposition",started)
  
  
}

BS = function(seed){
  grps = 4:6
  for (g in grps){
    if (!dir.exists(intermediate_bs_dir(seed))) {
      dir.create(intermediate_bs_dir(seed), recursive = TRUE)
    }
    gc()
    estimateWaveBS(g,seed)
  }
}



# Baseline bootstrap only; each seed fits waves 4--6 sequentially without CV.
plan(multisession, workers=bootstrapWorkers, gc=TRUE)
cat("Bootstrap workers:",future::nbrOfWorkers(),"; draws:",bootstrapDraws,"; precision:",PRECISE_ESTIMATION,"\n")
set.seed(2023)
started <- proc.time()[["elapsed"]]
print(system.time(future_lapply(seq_len(bootstrapDraws),function(x) BS(seed=x),future.seed=TRUE)))
cat("Bootstrap fitting complete; seconds:",proc.time()[["elapsed"]]-started,"\n")
gc()
started <- proc.time()[["elapsed"]]
print(system.time(future_lapply(seq_len(bootstrapDraws),function(x) decomposeBS(seed=x),future.seed=TRUE)))
cat("Bootstrap decomposition complete; seconds:",proc.time()[["elapsed"]]-started,"\n")
plan(sequential)
bootstrap_check_collection(bs_root,seq_len(bootstrapDraws),bootstrap_template,include_fits=TRUE)
