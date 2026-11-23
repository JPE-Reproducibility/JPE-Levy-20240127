#Leave Country Out
#estimate model and then decompose when the model is estimated when leaving one country outside 
#library(rootSolve)
library(nleqslv)
#' First Stage Model Estimation - Leave Country Out
#' 
#' This unction estimates the first stage using all countries except one, leaving out each country at a time.
#'
#' @param X Voters data
#' @param Z party data 
#' @param Y Outcome matrix 0 for not chosen, 1 for chosen 
#' @param X.group unique value for each voter option set (country and wave)
#' @param Z.group unique value for each party option set
#' @param conv.crit 
#' @param conv.crit2 
#' @param lag 
#' @param max.iter 
#' @param s 
#' @param standardize 
#' @param accelerate 
#' @param wave 
#'
#' @return list of Phi and delta, lambda (penalty) for each country left out. name is the country left out 
#' @export
#'
#' @examples
first.stage.loo = function(X,Z,Y,X.group,Z.group, 
                            
                            conv.crit = .01,conv.crit2 = .1,lag = 10,
                            max.iter =1000, s = 0.1,cv.conv.rat = 10,
                            standardize = T,
                            accelerate = F, wave=NULL){
  X = as.matrix(X)
  Z = as.matrix(Z)
  #standardize
  if (standardize){
    X = scale(X)
    consts = apply(Z,2,function(z) length(unique(z))==1)
    Z = scale(Z)
    Z[,consts] = 0
  } 
  #get unique values of the option sets
  country.list = unique(X.group)
  #prepare lists for results (Phi and deltas)
  phi.res = list()
  intercept.res = list()
  lambdas = list()
  #loop over option sets (countries) 
  for (cnt in country.list){
    print(cnt)
    ##prepare data
    #mark people to be dropped
    drop = X.group == cnt
    #Redefine X, X.group, Y
    X.in = X[!drop,]
    X.out = X[drop,]
    X.group.in = X.group[!drop]
    X.group.out = X.group[drop]
    Y.in = Y[!drop,]
    Y.out = Y[drop,]
    #mark parties to drop
    par.drop = Z.group == cnt
    #redfine Z, Z.group
    Z.in = Z[!par.drop,]
    Z.out = Z[par.drop,]
    Z.group.in = Z.group[!par.drop]
    Y.in = Y.in[,!par.drop]
    Y.out = Y.out[,par.drop]
    #get optimal lambda 
    #cvl = cv.loo(X.in,Z.in, Y.in,X.group.in, Z.group.in,
    #              X.out,Z.out,Y.out,X.group.out,n.values = 20,
    #             conv.crit = 20,conv.crit2 = 2, lag = 10 #convergence parameters are a bit arbitrary but they seem to be sufficiently good when testing   
    #             )
    res = first.stage.loo.single(X.in, Z.in, Y.in, X.group.in, Z.group.in, 
                                 X.out,Z.out,Y.out,X.group.out, wave,cnt)
    
  
    
    
    #add results to list
    phi.res[[cnt]] = res$phi
    intercept.res[[cnt]] = res$intercept
    lambdas[[cnt]] = res$lmbd
  }
  #return 
  list(phi.res = phi.res, intercept.res = intercept.res, lambda.list = lambdas)
  
  
}



first.stage.loo.single.old = function(X.in, Z.in, Y.in, X.group.in, Z.group.in, 
                                  X.out,Z.out,Y.out,X.group.out, 
                                  wave = NULL, cnt = NULL){
  cvl = cv.loo(X.in, Z.in, Y.in, X.group.in, Z.group.in, 
               X.out,Z.out,Y.out,X.group.out,
               wave = wave, cnt = cnt, n.values = n.values.lambda,
               conv.crit = conv.crit.lambda, conv.crit2 = conv.crit2.lambda, lag = lag.lambda, file.name = file.path(Sys.getenv("POPULIST_LOG_DIR"), "looLambda.log"))
  
  lmbd = cvl[["lambda"]]
  phi = cvl[["phi"]]
  intercept = cvl[["intercept"]]
  #estimate model using previous function
  #res = estimate.model(X.in,Z.in,Y.in,X.group.in,Z.group.in,
  #                     lmbd,phi,intercept,
  #                     conv.crit = conv.crit,conv.crit2 = conv.crit2,lag = lag,max.iter = max.iter,
  #                     standardize = F,accelerate = F)
  res = estimate.model(X.in, Z.in, Y.in ,X.group.in, Z.group.in, lmbd,phi, intercept,
                       wave = wave, cnt = cnt, 
                       conv.crit = conv.crit.model, conv.crit2 = conv.crit2.model, lag = lag.model, max.iter = max.iter.model,
                       standardize = F,accelerate = F)
  res$lambda = lmbd
  res
  
}
#' Cross Validation - Leave Country Out
#' 
#' Finds lambda that best predicts this country 
#'
#' @param X.trn X for used countries 
#' @param Z.trn Z for used countries
#' @param Y.trn Y for used countries
#' @param X.group.trn voters options set for used countries
#' @param Z.group.trn parties option sets for used countries
#' @param X.tst X for countries left out
#' @param Z.tst Z for countries left out
#' @param Y.tst Y for countries left ouy
#' @param eps 
#' @param fold 
#' @param n.values 
#' @param conv.crit 
#' @param conv.crit2 
#' @param lag 
#' @param max.iter 
#' @param wave 
#' @param seed 
#'
#' @return lambda (scalar)
#' @export
#'
#' @examples
cv.loo  = function(X.trn,Z.trn,Y.trn,X.group.trn,Z.group,
                   X.tst,Z.tst,Y.tst,X.group.tst,
                #   Z.outside.indicator,
                   eps=.001, n.values = 100,
                              conv.crit = 0.1, conv.crit2 = .01, lag = 10, max.iter = 1000,
                   plot = F, assume.concave = T,
                              wave=NULL, cnt=NULL, file.name = NULL){
  X.trn = as.matrix(X.trn)
  X.tst = as.matrix(X.tst)
  
  Z.trn = as.matrix(Z.trn)
  Z.tst = as.matrix(Z.tst)
  
  #define min and max lambda 
  max.lambda = max.lmbd(X.trn,Z.trn,Y.trn,X.group.trn,Z.group)
  min.lambda = max.lambda*eps
  #intitate phi to zero
  phi = matrix(0,ncol(X.trn),ncol(Z.trn))
  #initiate intercept to log total votes 
  intercept = init.intercept(Y.trn,Z.group)
  #generate lambda values to loop over
  lmbd.sq = exp(seq(log(max.lambda),log(min.lambda),length.out = n.values))
  #loop over lambda values
  count = 0
  scores = c()
  phi.list = c()
  intercept.list = c()
  for (lmbd in lmbd.sq){
    count = count + 1
    #stop looking if preformance is in decline for 3 times in a row
    n = length(scores)
    if (assume.concave & n>3){
      if(scores[n]<scores[n-1] & scores[n-1]<scores[n-2] & scores[n-2]<scores[n-3] ){
      break
    }}
    print("****************************************")
    print(lmbd)
    print("****************************************")
    
    if (!is.null(file.name)){
      write(paste0("country ", cnt, " wave ", wave, ", lambda ", lmbd, " ", scales::percent(count/n.values, accuracy = 0.1), " ", 
                   timestamp(prefix = "", suffix = "")), file.name,append = T)
    }
    
    #estaimte model for training 
    res = estimate.model(X.trn,Z.trn,Y.trn,X.group.trn,Z.group,
                         lmbd,phi,intercept,
                         conv.crit = conv.crit,conv.crit2 = conv.crit2,lag = lag,max.iter = max.iter,
                         standardize = F,accelerate = F)
    #update parameters so that next run will be faster
    phi = res$phi
    intercept = res$intercept
    
    #save results (not sure if we need it)
    phi.list[[as.character(lmbd)]] = phi
    intercept.list[[as.character(lmbd)]] = intercept
    
    
    #get delta for the test group 
    delta = predict.delta(X.tst,Z.tst,Y.tst,phi)
    
    #predict the model on the test 
    scores = c(scores,entropy.score(phi,delta,X.tst,Z.tst,rep(1,nrow(X.tst)),rep(1,nrow(Z.tst)),Y.tst))
    print("**********")
    print(scores)
    stopifnot(!is.na(scores[length(scores)])) #should be commented out, just for debugging
  }
  #return minimum lambda 

  if (plot){
    plot(lmbd.sq,scores,log = "x")
  } 
  lmbd = lmbd.sq[which.max(scores)]
  list(lambda = lmbd, phi = phi.list[[as.character(lmbd)]],intercept = intercept.list[[as.character(lmbd)]])
}

#' Simulate Model for One Group
#' 
#' Get Vote Shares when all voters and parties are in one group
#'
#' @param X voter data
#' @param Phi Phi matrix 
#' @param Z Voter data 
#' @param delta party fixed effects 
#'
#' @return total voting for each party
#' @export
#'
#' @examples
simulate.one.group = function(X,Phi,Z,delta,W=1){
  #get utility 
  U = t(t(X%*%Phi%*%t(Z)) + delta)
  #exponentiate
  P = exp(U)
  #normalize rows to sum to 1
  P = P/apply(P,1,sum)
  #aggregate accounting for weights
  apply(P*W,2,sum)
}  
  
#' Predict Delta
#' 
#' Function that get Phi (probably calculated for a different population) and calculates lambda to get Vote Shares right
#'
#' @param X voter data
#' @param Z party data
#' @param Y voting matrix outcome
#' @param Phi paramter matrix
#'
#' @return vector of delta, one for each party 
#' @export
#'
#' @examples
predict.delta = function(X,Z,Y,Phi,W = 1){
  #define equation to be solved 
  #fixing last delta to zero (DOF)
  f = function(d){ 
    delta = c(d,0)
    (simulate.one.group(X,Phi,Z,delta,W)-apply(Y*W,2,sum))[1:(length(d))]
    }
  #solve equation (equalize share of votes to predicted share of votes)
  #mr = multiroot(f,rep(0,nrow(Z)-1))
  mr = nleqslv(rep(0,nrow(Z)-1),f,control = list(maxit = 10000,
                                                 stepmax = 1)) #this generates smaller steps in the quest for solutions. makes things slower but maybe more stable
  if (mr$termcd >=3){
    print("couldn't find deltas. trying again, faster")
    print(mr)
    mr = nleqslv(rep(0,nrow(Z)-1),f,control = list(maxit = 10000) )
    print("faster version:")
    print(mr)
    
  }
 # stopifnot(mr$estim.precis < 0.1)
  stopifnot(mr$termcd <3)
  res =   c(mr[[1]],0)
  names(res) = rownames(Z)
  res

}

transpwose.list = function(list.of.lists){
  xs = names(list.of.lists)
  if (is.null(xs)){
    xs = 1:length(list.of.lists)
  }
  ys = unique(unlist(lapply(list.of.lists,names)))
#  print(ys)
  res = list()
  for (y in ys){
    res[[y]] = list()
    for (x in xs){
      res[[y]][[x]] = list.of.lists[[x]][[y]]
    }
  }
  res
}
  
#' Second Stage estimation of leave country out
#' 
#' Estimate beta when each country is out. Note that all lists should have the same country names!
#'
#' @param delta.lists list of lists. for each country, list of waves with the delta calculated excluding this party
#' @param Z.list standard list of party data by wave. party should have the same name in all waves
#' @param Z.os.list party country (option set) in each set. should have the same names as the names in phi.list and delta.list 
#' @param k number of dimensions to consider 
#' @param phi.lists list lists. for each country left out, list of waves
#'
#' @return list of beta for each country 
#' @export
#'
#' @examples
second.stage.loo = function(phi.lists, delta.lists, Z.list, Z.os.list, w.list, k = 5, standardize = T){
  if (standardize){
    Z.list = lapply(Z.list, function(Z) scale(Z))
  } 
  
  #get list of countries (form phi names)
  all.countries = unique(names(phi.lists))
  #init final results 
  beta.list = list()
  #loop over countries 
  for (cnt in all.countries){
    print(cnt)
    ##create a data set of delta and parties
    #take the top k component from Phi
    V.list = lapply(phi.lists[[cnt]], function (phi) if(is.null(phi)){
      NULL
      } else {
        v = svd(phi)$v[,1:k]
        k. = min(k, sum(svd(phi)$d>10^-16))
        if (k.<k){
          v[,(k.+1):k] = 0
        }
        v
      })
    nms = names(phi.lists[[cnt]])
    if (is.null(nms)){
      nms = (1:length(phi.lists[[cnt]]))[!unlist(lapply(phi.lists[[cnt]],is.null))]
      
    }
    #calculate the transformation on Z for the k components 
    ZV.list = list()
    for (nm in nms){
      ZV.list[[nm]] = as.matrix(Z.list[[nm]])%*%V.list[[nm]]
    }
    #link with country and wave number and party identifier
    dat.list = list()
    for (i in nms){
      d = cbind(data.frame(ZV.list[[i]]),paste0(Z.os.list[[i]],i),i,rownames(Z.list[[i]]))
      colnames(d) = c(paste0("z.proj",1:k),"option.set","Wave","party.id")
      rownames(d) = rownames(Z.list[[i]])
      d$wgt = w.list[[i]][d$party.id]
      dat.list[[i]] = d}
    #merge with deltas(NA for the missing country)
    for (i in nms){
      dat.list[[i]]$delta = delta.lists[[cnt]][[i]][dat.list[[i]]$party.id]
    }
    #merge all data
    #final data is delta, wave and the k components 
    full.Z = do.call("rbind",dat.list)
    #run regression of Z interacted with time  with party fixed effects 
    form = as.formula(paste0("delta~(",paste0("z.proj",1:k,collapse = "+"),")*factor(Wave)|party.id+option.set"))
    felm.res = felm(form,full.Z,weights = full.Z$wgt)
    #print("printing fixed effects regression results")
    #print(summary(felm.res))
    
    #parse results - get the coefficient on each z at each time 
    coef = felm.res$coef[,1]
    #create matrix of coefficients on V (k x Wave num)
    coef.matrix = list()
    coef.matrix[[nms[1]]] = coef[paste0("z.proj",1:k)]
    for (j in nms[-1]){
      coef.matrix[[j]] = coef.matrix[[nms[1]]] + coef[paste0("z.proj",1:k,":factor(Wave)",j)]
    }
    
    #multiply each V[,1:k] by the estiamted coefficients for that wave
    coef.list = list()
    for (i in nms){
      coef.list[[i]] = V.list[[i]] %*% replace(coef.matrix[[i]],is.na(coef.matrix[[i]]),0)
      }
    #bind to one matrix 
    final.coef = do.call(cbind,coef.list)
    rownames(final.coef) = colnames(Z.list[[1]])
    colnames(final.coef) = nms
    beta.list[[cnt]] = final.coef
  #return everything 
  }
  
  return(beta.list)
}

#get beta using phi from the overall population 

second.stage.loo.v2 = function(phi.lists, delta.lists, Z.list, Z.os.list, w.list, k = 5, standardize = T){
  if (standardize){
    Z.list = lapply(Z.list, function(Z) scale(Z))
  } 
  
  #get list of countries (form phi names)
  all.countries = unique(names(delta.lists))
  #init final results 
  beta.list = list()
  #loop over countries 
  for (cnt in all.countries){
    print(cnt)
    ##create a data set of delta and parties
    #take the top k component from Phi
    V.list = lapply(phi.lists, function (phi) svd(phi)$v[,1:k])
    nms = (1:length(phi.lists))[!sapply(delta.lists[[cnt]],is.null)]
    #calculate the transformation on Z for the k components 
    ZV.list = list()
    for (nm in nms){
      ZV.list[[nm]] = as.matrix(Z.list[[nm]])%*%V.list[[nm]]
    }
    #link with country and wave number and party identifier
    dat.list = list()
    for (i in nms){
      d = cbind(data.frame(ZV.list[[i]]),paste0(Z.os.list[[i]],i),i,rownames(Z.list[[i]]))
      colnames(d) = c(paste0("z.proj",1:k),"option.set","Wave","party.id")
      rownames(d) = rownames(Z.list[[i]])
      d$wgt = w.list[[i]][d$party.id]
      dat.list[[i]] = d}
    #merge with deltas(NA for the missing country)
    for (i in nms){
      dat.list[[i]]$delta = delta.lists[[cnt]][[i]][dat.list[[i]]$party.id]
    }
    #merge all data
    #final data is delta, wave and the k components 
    full.Z = do.call("rbind",dat.list)
    #run regression of Z interacted with time  with party fixed effects 
    form = as.formula(paste0("delta~(",paste0("z.proj",1:k,collapse = "+"),")*factor(Wave)|party.id+option.set"))
    felm.res = felm(form,full.Z,weights = full.Z$wgt)
    #print("printing fixed effects regression results")
    #print(summary(felm.res))
    
    #parse results - get the coefficient on each z at each time 
    coef = felm.res$coef[,1]
    #create matrix of coefficients on V (k x Wave num)
    coef.matrix = list()
    coef.matrix[[nms[1]]] = coef[paste0("z.proj",1:k)]
    for (j in nms[-1]){
      coef.matrix[[j]] = coef.matrix[[nms[1]]] + coef[paste0("z.proj",1:k,":factor(Wave)",j)]
    }
    
    #multiply each V[,1:k] by the estiamted coefficients for that wave
    coef.list = list()
    for (i in nms){
      coef.list[[i]] = V.list[[i]] %*% replace(coef.matrix[[i]],is.na(coef.matrix[[i]]),0)
    }
    #bind to one matrix 
    final.coef = do.call(cbind,coef.list)
    rownames(final.coef) = colnames(Z.list[[1]])
    colnames(final.coef) = nms
    beta.list[[cnt]] = final.coef
    #return everything 
  }
  
  return(beta.list)
}

#' Leave-Country-Out Decomposition
#' 
#' Run a counterfactual for the development in party shares. Do this using leave-country-out
#'
#' @param X.list List of voter data
#' @param Z.list List of party data
#' @param Y.list List of voting decisions
#' @param phi.list.list List of Phi lists. For each wave, the phi calculated for all countries except one
#' @param beta.list.list List of beta lists for leave one out
#' @param party.id.list 
#' @param X.group.list list of voter group (country)
#' @param Z.group.list list of party group (country)
#' @param X the waves for voters. * means all. To fix the same wave, repeat the same wave times the number of waves
#' @param Z the waves for parties 
#' @param param the waves for the parameters
#' @param resid the wave for residuals 
#' @param Z.fix 
#'
#' @return table with simulated shares for each party and wave 
#' @export
#'
#' @examples
decomposition.loo = function(X.list, Z.list, Y.list, phi.list.list, beta.list.list,party.id.list,
                         X.group.list, Z.group.list, W.list = NULL,
                         X = "*", Z = "*", param = "*", resid = "*") {
  #turn into matrices
  X.list = lapply(X.list, as.matrix)
  Z.list = lapply(Z.list, as.matrix)
  
  #name of waves
  nms = names(X.list)
  if (is.null(nms)){
    nms = 1:length(X.list)
  }
  
  #name of parties
  parties = unique(unlist(party.id.list))
  #set wave names for each component
  if (X == "*"){
    X = nms
  } else {
    X = rep(X,length(nms))
  }
  if (Z == "*"){
    Z = nms
  } else {
    Z = rep(Z,length(nms))
  }
  if (param == "*"){
    param = nms
  } else {
    param = rep(param,length(nms))
  }
  if (resid == "*"){
    resid = nms
  } else {
    resid = rep(resid,length(nms))
  }
  
  res = data.frame(row.names = parties)
  all.countries = unique(unlist(X.group.list))
  #loop over countries
  for (cnt in all.countries){
    ##calculate deltas
    vel.list = list()
    Z.only.cnt = list()
    pil = list()
    #loop over waves waves
    for (i in nms){
      #skip if country does not participate in this wave
      if (sum(X.group.list[[i]] == cnt)>0){
      
      #prepare data for this country in wave 
      X. = X.list[[i]][X.group.list[[i]] == cnt,]
      Z. = Z.list[[i]][Z.group.list[[i]] == cnt,]
      Z.only.cnt[[i]] = Z.
      Y = Y.list[[i]][X.group.list[[i]] == cnt, Z.group.list[[i]] == cnt]
      W. = ifelse(is.null(W.list),1,W.list[[i]])
      delta = predict.delta(X.,Z.,Y,phi.list.list[[cnt]][[i]],W.)
      #residuals 
      vel.list[[i]] = get.residual.lco(delta,beta.list.list[[cnt]][[i]],Z.)
      pil[[i]] = party.id.list[[i]][Z.group.list[[i]] == cnt]
      }}
      #expand data to include all parties even when they don't run
      EP = expand.parties(Z.only.cnt,vel.list,
                          lapply(nms, function(i) {if(!is.null(Z.only.cnt[[i]])){
                            rep(1,nrow(Z.only.cnt[[i]]))
                          }})
                            ,
                          pil)
      Z.new = EP[[1]]
      vel.new = EP[[2]]
   #   Z.group = EP[[3]]
      
    
    ##Start with counterfactuals for this country
    #loop over waves
    for (i in nms){
      #skip if country does not participate in this wave
      if (sum(X.group.list[[i]] == cnt)>0){
      #predict voters
      X. = X.list[[X[i]]][X.group.list[[X[i]]] == cnt,]
      Z. = Z.new[[Z[i]]]
      shrs = simulate.shares(X.,
                             Z.,
                             phi.list.list[[cnt]][[param[i]]],
                             beta.list.list[[cnt]][[param[i]]], 
                             vel.new[[resid[i]]],
                             rep(1,nrow(X.)),
                             rep(1, nrow(Z.)))
      
      #divide by total votes
      res[names(shrs),i] = shrs/nrow(X.)
      } else {
        res[names(shrs),i] = NA
    }}
  }
  #return everything
  res
  
}

decomp.do.all.loo = function(X.list, Z.list, Y.list, phi.list.list, beta.list.list,party.id.list, party.dat,
                         X.group.list, Z.group.list, strt, final = 6, bar = .02, k = 5,yrs = NULL, W.list = NULL,
                         dec.order = c("resid","Z", "X","param"), standardize = T){
  if (standardize){
    X.list = lapply(X.list, function(X) scale(X))
    Z.list = lapply(Z.list, function(Z) scale(Z))
  } 
  if (is.null(W.list)){
    
  }
  #fit model
  model.fit = decomposition.loo(X.list,Z.list,Y.list,
                                phi.list.list,beta.list.list,party.id.list,
                                X.group.list, Z.group.list,W.list)
  
  #first component 
  c1 = decomposition.loo(X.list,Z.list,Y.list,
                     phi.list.list,beta.list.list,party.id.list,
                     X.group.list, Z.group.list, W.list,
                     X = ifelse(dec.order[1] == "X", "*", strt), 
                     param = ifelse(dec.order[1] == "param", "*", strt), 
                     Z =  ifelse(dec.order[1] == "Z", "*", strt),
                     resid = ifelse(dec.order[1] == "resid", "*", strt))
  colnames(c1) = paste0("w",1:ncol(c1),".1")
  
  c2 = decomposition.loo(X.list,Z.list,Y.list,
                     phi.list.list,beta.list.list,party.id.list,
                     X.group.list, Z.group.list, W.list,
                     X = ifelse(dec.order[1] == "X", "*", ifelse(dec.order[2] == "X", "*", strt)), 
                     param = ifelse(dec.order[1] == "param", "*", ifelse(dec.order[2] == "param", "*", strt)),
                     Z =  ifelse(dec.order[1] == "Z", "*", ifelse(dec.order[2] == "Z", "*", strt)),
                     resid = ifelse(dec.order[1] == "resid", "*", ifelse(dec.order[2] == "resid", "*", strt)))
  
  colnames(c2) = paste0("w",1:ncol(c2),".2")
  c3 = decomposition.loo(X.list,Z.list,Y.list,
                     phi.list.list,beta.list.list,party.id.list,
                     X.group.list, Z.group.list, W.list,
                     X = ifelse(dec.order[4] != "X", "*", strt), 
                     param = ifelse(dec.order[4] != "param", "*", strt), 
                     Z =  ifelse(dec.order[4] != "Z", "*", strt),
                     resid = ifelse(dec.order[4] != "resid", "*", strt))
  colnames(c3) = paste0("w",1:ncol(c3),".3")
  
  
  c4 = model.fit
  #decomposition(X.list,Z.list,Y.list,
  #                 phi.list,FE.list,party.id.list,
  #                X.country,Z.country, 
  #                X = ifelse(dec.order[3] == "X", "*", final), 
  #               param = ifelse(dec.order[3] == "param", "*", final), 
  #              Z =  ifelse(dec.order[3] == "Z", "*", final))  
  colnames(c4) = paste0("w",1:ncol(c4),".4")
  
  
  
  wvs = ncol(c1)
  #organize in one table
  dat = merge(c1,c2,by = 0)
  rownames(dat) = dat$Row.names
  dat = dat[,-1]
  dat =  merge(dat,c3,by = 0)
  rownames(dat) = dat$Row.names
  dat = dat[,-1]
  dat =  merge(dat,c4,by = 0)
  rownames(dat) = dat$Row.names
  dat = dat[,-1]
  
  dat =  merge(dat,party.dat,by = 0, all.x = T)
  rownames(dat) = dat$Row.names
  dat = dat[,-1]
  
  
  #aggregate by country 
  nc = ncol(dat)
  #only populist
  dat.f = dat[dat[,nc]==1,]
  cnt = aggregate(dat.f[,1:(nc-2)],list(dat.f[,nc-1]), function(x) sum(x,na.rm = T))
  rownames(cnt) = cnt[,1]
  cnt = cnt[,-1]
  #is this contry in the wave
  cnt.w = do.call(rbind,lapply(rownames(cnt),function(cou){unlist(lapply(X.group.list,function(wv) cou %in% wv))}))
  rownames(cnt.w) = rownames(cnt)
  cnt.w = cbind(cnt.w,cnt.w,cnt.w,cnt.w)
  cnt.norm = replace(cnt,!cnt.w,NA)
  
  
  #initial support 
  init = cnt.norm[,1]
  
  #look at difference from start
  
  cnt.norm[1:wvs]=cnt.norm[1:wvs]-cnt[,strt]
  cnt.norm[,(wvs+1):(2*wvs)]=cnt.norm[,(wvs+1):(2*wvs)]-cnt.norm[,wvs+strt]
  cnt.norm[,(2*wvs+1):(3*wvs)]=cnt.norm[,(2*wvs+1):(3*wvs)]-cnt.norm[,2*wvs+strt]
  cnt.norm[,(3*wvs+1):(4*wvs)]=cnt.norm[,(3*wvs+1):(4*wvs)]-cnt.norm[,3*wvs+strt]
  
  #loof at difference between components
  cnt.norm[,(3*wvs+1):(4*wvs)] = cnt.norm[,(3*wvs+1):(4*wvs)]-cnt.norm[,(2*wvs+1):(3*wvs)]
  cnt.norm[,(2*wvs+1):(3*wvs)] = cnt.norm[,(2*wvs+1):(3*wvs)]-cnt.norm[,(wvs+1):(2*wvs)]
  cnt.norm[,(wvs+1):(2*wvs)] = cnt.norm[,(wvs+1):(2*wvs)]-cnt.norm[1:wvs]
  
  #Throw away countries with non populism trend
  tot.delta = apply(cnt.norm[,seq(wvs,4*wvs,wvs)],1,sum)
  cnt.rise = cnt.norm/(init+tot.delta)
  #replace NA with avgs
  if (wvs>2){
    for (i in 2:(wvs-1)){
      for(j in 0:3)
      {
        cnt.rise[,i+wvs*j] = replace(cnt.rise[,i+wvs*j], is.na(cnt.rise[,i+wvs*j]),((cnt.rise[,i+wvs*j-1]+cnt.rise[,i+wvs*j+1])/2)[is.na(cnt.rise[,i+wvs*j])])
      }
    }
  }
  total = apply(cnt.rise,2,function(x) mean(x,na.rm = T))
  #normalize components to sum to 1
  norm.fact = sum(total[seq(wvs,4*wvs,wvs)])
  total = total/norm.fact
  cnt.norm = rbind(cnt.norm, total)
  rownames(cnt.norm)[nrow(cnt.norm)] = "total"
  cnt.norm
  
  }
  
  
#' Get Residual Leave Country Out
#' 
#' Use deltas for each party and substract the observed part (beta*Z) to get the residual 
#'
#' @param delta.list list of deltas that should be computed with the function predict.delta
#' @param beta.list list of betas, estimated with second stage
#' @param Z.list Z for each wave 
#'
#' @return list of party residuals for each wave
#' @export
#'
#' @examples
get.residual.lco = function(delta,beta,Z){
  #subtract delta - beta*Z
  delta - Z%*%beta
}
