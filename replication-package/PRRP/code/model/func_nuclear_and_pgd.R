# *************************************************************************
# Description ----
# *************************************************************************
# This code has functions (and only functions) for
# 1. Estimation - first stage
# 2. Estimation - second stage
# 3. Decomposition
# 4. Generating the weight distribution

# *************************************************************************
# SETUP ----
# *************************************************************************
library(npmr)
library(mclogit)
library(lfe)
library(reshape2)
library(ggplot2)
library(corpcor)


# *************************************************************************
# Estimation
# *************************************************************************

#' Inverse weighted SD of each column of X
#'
#' estimate.model standardizes X with wt.scale(X, sample.weight), i.e. by the weighted SD.
#' Use this (not the unweighted sd) when converting the estimated Phi back to raw X units.
#' A column with zero weighted SD gets 0, because wt.scale sets that column to 0 during estimation.
inv.wt.sd <- function(X, W) {
  sc <- attr(wt.scale(X, W), "scaled:scale")
  ifelse(sc > 0, 1 / sc, 0)
}

#' Estimate Model
#'
#' Main function to the first stage estimation of the model.
#'
#' This function gets data from a wave and estimate the parameters Phi and delta using penalized MLE which is estimated with proximal gradient descent.
#' It starts from the initial parameter and in each iteration tries to improve the target function. It compares the performance to the performance "lag" iterations before.
#'
#' @param X Voter data matrix
#' @param Z Party data matrix
#' @param Y A matrix for voter-party combination with an indicator if voter voted for this party
#' @param X.group vector of voter option set (typically country/wave)
#' @param Z.group vector of party option set (typically country/wave)
#' @param lambda penalization parameter.
#' @param phi.t initial value for Phi
#' @param intercept.t initial value for delta
#' @param NP.vec currently not used (for non participation)
#' @param eta.t currently not used (for non participation)
#' @param conv.crit convergence criteria 1: improvement in objective function (penalized likelihood)
#' @param conv.crit2 not used. Kept so existing calls still work (see the note at the main loop)
#' @param lag number of iterations used to test if convergence was reached
#' @param max.iter Maximum number of iteration allowed
#' @param s step size in proximal gradient descent
#' @param standardize if True, function will standardize both X and Z
#' @param accelerate not used
#' @param wave wave number. for printing
#' @param cnt
#' @param sample.weight sampling weights
#'
#' @return   list(phi = phi.t1, intercept = intercept.t1, eta = eta.t1, obj = obj.t1,s = s, conv = rtn.rsn)
#' @param phi estimated matrix
#' @param intercept estimated delta
#' @param eta not used (for non participation)
#' @param obj
#' @param s step size used
#' @param conv reason for stop. 1 - all good. 2. maximum iteration reached. not so good. 3. stopped becuase improved paramters were actually worse. shouldn't happen. I'd be worried if I were you.
#' @export
#'
#' @examples
estimate.model <- function(X, Z, Y, X.group, Z.group, lambda,
                           phi.t = NULL, intercept.t = NULL,
                           NP.vec = NULL, eta.t = NULL,
                           conv.crit = .01, conv.crit2 = .1, lag = 10,
                           max.iter = 1000, s = 0.1,
                           standardize = T,
                           accelerate = F, wave = NULL, cnt = NULL,
                           sample.weight = 1, bootstrapSeed = NULL) {
  # validate that every party has voters
  stopifnot(all(apply(Y, 2, sum) > 0))

  # Standardize X and Z
  if (standardize) {
    # standardize X using sampling weights (mean 0, SD 1)
    X <- wt.scale(X, sample.weight)
    ## @Oren, do we need this?
    print(summary(X[, 7]))
    # standardize Z (un-weighted)
    Z <- scale(Z)
    ## @Oren, do we need the following two lines?
    print(summary(Z[, 7]))
    print(sd(Z[, 8]))
  }

  # Verify lambda is positive
  stopifnot(lambda >= 0)

  # Initiate parameters to zero
  if (is.null(phi.t)) {
    phi.t <- matrix(0, ncol(X), ncol(Z))
  }
  if (is.null(intercept.t)) {
    intercept.t <- init.intercept(Y)
  }

  # create matrix of non participation by group
  if (!is.null(NP.vec)) {
    all.os <- unique(Z.group)
    NPY <- expand.matrix(NP.vec, X.group, all.os)
  } else {
    NPY <- NULL
  }
  if (is.null(eta.t) & !is.null(NP.vec)) {
    eta.t <- matrix(0, ncol(X), length(all.os))
    colnames(eta.t) <- all.os
  }
  # initiate convergence criteria to Inf
  conv <- Inf
  # set a counter for number of iterations to 0
  cntr <- 0
  # return reason
  rtn.rsn <- 1
  # initial probabilities
  SM <- simulate.model(X, Z, phi.t, intercept.t, X.group, Z.group, eta.t)
  # a matrix of log probability for every voter to vote for every party
  logP <- SM[[1]]
  P <- exp(logP)
  # non participation
  logNPP <- SM[[2]]
  if (is.null(logNPP)) {
    NPP <- NULL
  } else {
    NPP <- exp(logNPP)
  }
  # calculate initial value of penalized likelihood
  obj.t <- penalized.likelihood(phi.t, Y, logP, lambda, NPY, logNPP, sample.weight)
  # save past results to test overall convergence
  obj.vec <- obj.t
  # loop until the convergence criterion is met or maximum iterations reached.
  # A second criterion on the change in Phi was removed: c(phi.list, phi.t1) split each
  # Phi into single numbers, so it compared unrelated entries of an early Phi and almost
  # never held a fit back, while the growing list made long fits much slower.
  while (conv > conv.crit & max.iter > cntr) {
    print(conv)
    # print iteration number
    if (cntr %% 50 == 0 & !is.null(wave)) {
      print(paste0("iteration number ", cntr))
      write(
        paste0(
          ifelse(!is.null(cnt), paste0("country ", cnt, " "), ""),
          ifelse(!is.null(bootstrapSeed), paste0("seed ", bootstrapSeed, " "), ""),
          "wave ", wave, " iter ", cntr, " ", timestamp(prefix = "", suffix = "")
        ),
        file = file.path(Sys.getenv("POPULIST_LOG_DIR"), "estimateModel.log"), append = TRUE
      )
    }
    # get new parameters using PGD
    new.p <- improved.param(X, Z, Y, phi.t, intercept.t,
      X.group, Z.group, s,
      lambda, obj.t, P,
      eta.t = eta.t, NPY = NPY, NPP.t = NPP, sample.weight = sample.weight
    )
    # update parameters if objective function increased
    if (new.p$imp > 0) {
      phi.t1 <- new.p$phi.t1
      intercept.t1 <- new.p$intercept.t1
      eta.t1 <- new.p$eta.t1
      print(new.p$obj.t1)
      obj.t1 <- new.p$obj.t1
      obj.vec <- c(obj.vec, new.p$obj.t1)
      # take last s used time 2
      ## @ Oren, I did not understand this
      s <- new.p$s * 2
      # check convergence on objective function
      # compare to the results "lag" iterations before, so no point to start before lag iterations
      if (cntr > lag) {
        # convergence criteria: improvement in objective function (penalized likelihood)
        conv <- obj.vec[[cntr]] - obj.vec[[cntr - lag]]
      }
    } else {
      # if new parameter are better stop and don't update for new values
      conv <- 0
      rtn.rsn <- 3
      phi.t1 <- phi.t
      intercept.t1 <- intercept.t
      eta.t1 <- eta.t
      obj.t1 <- obj.t
    }

    # not used
    ## @Oren do we want to delete this acceleration option? + what is 3?
    if (accelerate) {
      phi.t <- phi.t1 + cntr / (cntr + 3) * (phi.t1 - phi.t)
    } else {
      # update parameter
      phi.t <- phi.t1
    }
    intercept.t <- intercept.t1
    eta.t <- eta.t1
    obj.t <- obj.t1
    P <- new.p$P.t
    NPP <- new.p$NPP.t
    cntr <- cntr + 1
  }
  if (max.iter == cntr) {
    rtn.rsn == 2
  }
  # Bootstrap-only diagnostics; numerical stopping logic and return values are unchanged.
  if (!is.null(bootstrapSeed)) {
    write(paste("seed",bootstrapSeed,"wave",wave,"iterations",cntr,
                "criterion1",conv,"stopping_code",rtn.rsn,
                "iteration_limit",cntr==max.iter),
          file=file.path(Sys.getenv("POPULIST_LOG_DIR"),paste0("bootstrap-",bootstrapSeed,".log")),append=TRUE)
    if (cntr==max.iter) warning("Bootstrap seed ",bootstrapSeed," wave ",wave," reached iteration limit.")
    if (rtn.rsn!=1) warning("Bootstrap seed ",bootstrapSeed," wave ",wave," stopping code ",rtn.rsn)
  }
  # return new parameters, objective function,
  list(phi = phi.t1, intercept = intercept.t1, eta = eta.t1, obj = obj.t1, s = s, conv = rtn.rsn)
}

#' Simulate Model
#'
#' Simulate the probability to voter for every party given voter and party characteristics and parameters
#'
#' This function simulates our model for a given parameters and data. It will be used to both estimate the model, and create counterfactuals
#' It allows for voters and parties to have "groups" (e.g. a country, or year) such that a voter can only vote for a party from their group
#'
#' @param X Voters data matrix
#' @param Z Party data matrix where rows are parties
#' @param phi Model parameters
#' @param X.group categorical variable for the category (country/year) of each voter
#' @param Z.group categorical variable for the category (country/year) of each party
#'
#' @param intercept vector in the length of Z rows of additional party utility (delta)
#' @param eta matrix of coefficients for non participation. Rows are categories country/year. Columns are corresponding X variables.
#'
#' @return matrix of log probability for each person to vote for each party
#' @export
#'
#' @examples
simulate.model <- function(X, Z, phi, intercept, X.group, Z.group, eta = NULL) {
  # define all groups
  all.os <- unique(Z.group)
  # define final output, probability to vote for each party and non participation
  logP <- matrix(-Inf, nrow(X), nrow(Z))
  # non participation - not used.
  if (!is.null(eta)) {
    logNPP <- matrix(-Inf, nrow(X), length(all.os))
    stopifnot(!is.null(colnames(eta)))
    colnames(logNPP) <- colnames(eta)
  } else {
    logNPP <- NULL
  }
  # loop over groups
  for (os in all.os) {
    # get utility matrix U using X*\Phi*Z
    U <- X[X.group == os, ] %*% phi %*% t(Z[Z.group == os, ])
    # add intercept
    U <- t(t(U) + intercept[Z.group == os])
    # add non participation
    if (!is.null(eta)) {
      NPU <- X[X.group == os, ] %*% eta[, os]
    }
    # get probabilities using logP = U + const
    # first exponent
    # get the constant
    if (is.null(eta)) {
      Tot.U <- U
    } else {
      Tot.U <- cbind(U, NPU)
    }
    # subtract the max to get utility around 0 (this shouldn't matter but helps with doing the exponent on smaller numbers)
    mx <- apply(Tot.U, 1, max)
    Tot.U <- Tot.U - mx
    # constant is the sum of each row.
    cnst <- apply(exp(Tot.U), 1, sum)
    # divide by the constant to set all rows sum to 1 and put in final output (equivalently, subtract log of the constant to get log probs)
    logP[X.group == os, Z.group == os] <- U - log(cnst) - mx
    if (!is.null(eta)) {
      logNPP[X.group == os, colnames(eta) == os] <- NPU - log(cnst) - mx
    }
  }
  list(logP = logP, logNPP = logNPP)
}



#' Find next parameter value
#'
#' Find new parameter that improve the objective function
#'
#' #this function updates the parameters using PGD. If it fails, it tries a smaller step
#'
#' @param X voters data
#' @param Z party data
#' @param Y matrix for every voter and party where Y_ij = 1 iff voter i voted for party j
#' @param phi.t current value of parameter phi
#' @param intercept.t current value of intercept
#' @param X.group categorical variable for the category (country/year) of each voter
#' @param Z.group categorical variable for the category (country/year) of each party
#' @param s step size
#' @param lambda penalty size
#' @param max.iter total iterations allowed (for eaching improved parameters)
#' @param p.t probabilities to vote for each party from previous time (P matrix)
#'
#' @return
#' @param phi.t1 new value of phi
#' @param intercept.t1 new value of the intercept
#' @param obj.t1 value for objective function
#' @param s used value of s
#' @param imp improvement in function
#'
#' @examples
improved.param <- function(X, Z, Y,
                           phi.t, intercept.t,
                           X.group, Z.group,
                           s, lambda,
                           obj.t, p.t, max.iter = 40,
                           eta.t = NULL, NPY = NULL, NPP.t = NULL,
                           sample.weight = 1) {
  # initiate improvement to a negative value
  imp <- -1
  # loop until improvement/max.iter
  while (imp < 0 & max.iter > 0) {
    # update parameters
    new.p <- update.param(X, Z, p.t, Y, phi.t, intercept.t, s, lambda, eta.t, NPY, NPP.t, sample.weights = sample.weight)
    phi.t1 <- new.p$phi.t1
    intercept.t1 <- new.p$intercept.t1
    eta.t1 <- new.p$eta.t1
    # calculate next probability
    SM <- simulate.model(X, Z, phi.t1, intercept.t1, X.group, Z.group, eta.t1)
    logP <- SM[[1]]
    p.t <- exp(logP)
    logNPP <- SM[[2]]
    if (!is.null(logNPP)) {
      NPP.t <- exp(logNPP)
    }
    # calculate objective function
    obj.t1 <- penalized.likelihood(phi.t1, Y, logP, lambda, NPY, logNPP, sample.weight)
    # define improvement
    imp <- obj.t1 - obj.t
    # reduce number of interactions left
    max.iter <- max.iter - 1
    # divide s by 2 to get smaller step size
    s <- s / 2
  }
  # return new parameters estimated, new objective value, reason for stopping loop
  list(
    phi.t1 = phi.t1, intercept.t1 = intercept.t1, P.t = p.t,
    obj.t1 = obj.t1, s = s * 2,
    imp = obj.t1 - obj.t, rem.iter = max.iter,
    eta.t1 = eta.t1, NPP.t = NPP.t
  )
}



#' Likelihood
#'
#' Calculates the likelihood of of observing a vote given specific data and parameters, penalizes with nuclear norm
#'
#' @param lambda penalty factor for norm
#' @param phi parameter
#' @param Y matrix for every voter and party where Y_ij = 1 iff voter i voted for party j
#' @param logP matrix for every voter and party where Y_ij = probability
#' @param sample.weight sample weights
#'
#' @return the total likelihood + penalty
#' @export
#'
#' @examples
penalized.likelihood <- function(phi, Y, logP, lambda, NPY = NULL, logNPP = NULL, sample.weight = 1) {
  # calculate likelihood by summing log probabilities of chosen parties
  realized.p <- logP * Y
  # replace NaN (0*-Inf) with 0
  realized.p[is.nan(realized.p)] <- 0
  # same for non participation
  if (!is.null(NPY)) {
    realized.npp <- logNPP * NPY[, colnames(logNPP)]
    realized.npp[is.nan(realized.npp)] <- 0
  } else {
    realized.npp <- 0
  }
  # the log likelihood. Sum of the log probability of chosen parties, times the weights
  ll <- sum(realized.p * sample.weight) + sum(realized.npp * sample.weight)
  # get penalty
  pen <- nuclear(phi)
  # final results is the likelihood, penalized
  res <- ll - lambda * pen
  res
}


#' Update Parameters
#'
#' Return the next parameters given a value of P,s
#'
#' @param X voter data
#' @param Z Party data
#' @param eta.t last value of eta (not used)
#' @param NPY matrix of indicators for every voters who didnot vote and their group (not used)
#' @param NPP probability of every voter not vote, for each group (not used)
#' @param P probability of every voter to vote for every part
#' @param Y matrix of indicators for every voters and the party they chose
#' @param phi.t last value of phi
#' @param intercept.t last value of the intercept
#' @param s step size
#' @param lambda penalty on nuclear norm
#' @param sample.weights samping weights
#'
#' @return
#' @param phi.t1 new value of phi
#' @param intercept.t1 new value of the intercept
#'
#'
#' @export
#'
#' @examples
update.param <- function(X, Z, P, Y, phi.t, intercept.t, s, lambda, eta.t = NULL, NPY = NULL, NPP = NULL, sample.weights = NULL) {
  # define the matrix Y-P and multiply by weights
  YmP <- Y - P
  if (!is.null(sample.weights)) {
    YmP <- YmP * sample.weights
  }
  # update intercept based on the FOC and regular gradient descent
  intercept.t1 <- intercept.t + s * apply(YmP, 2, sum)
  # update phi baesd on the FOC using PGD
  phi.t1 <- prox.nn(phi.t + s * t(X) %*% (YmP) %*% Z, s * lambda)
  # update eta (not used)
  if (!is.null(eta.t)) {
    eta.t1 <- eta.t + s * t(X) %*% (NPY - NPP)
  } else {
    eta.t1 <- NULL
  }
  # return
  list(phi.t1 = phi.t1, intercept.t1 = intercept.t1, eta.t1 = eta.t1)
}

#' Prox Nuclear Norm
#'
#' Proximal map for nuclear norm
#'
#' This function calculates the proximity operator on a matrix using the nuclear norm.
#' It performs soft-thresholding to its eigen values using an SVD
#'
#' @param mat Choose a matrix. Any matrix
#' @param lambda how much to reduce from eigen-values
#'
#' @return matrix with the same dimensions
#' @export
#'
#' @examples
prox.nn <- function(mat, lambda) {
  # svd
  mat.svd <- s.v.d.2(mat)
  # soft threshold the diagonal
  mat.svd$d <- sign(mat.svd$d) * pmax(abs(mat.svd$d) - lambda, 0)
  # return
  mat.svd$u %*% diag(mat.svd$d) %*% t(mat.svd$v)
}


#' SVD
#'
#' This function wraps the R singular value decomposition such that it won't have errors
#'
#' The function tries to use svd using the regular R function. If it fails, it changes the matrix a little bit until it works. If correction is too big, it gives up.
#'
#' @param mat
#' @param correction
#'
#' @return svd results
#' @export
#'
#' @examples
s.v.d.2 <- function(mat, correction = 10^-19) {
  if (correction > 10^-12) {
    stop("svd failed and correction did not work.")
  }
  tryCatch(svd(mat), error = function(cond) {
    s.v.d.2(mat + correction, correction * 2)
  })
}

#' Initiate Intercept
#'
#' Find the optimal intercept when phi is the 0 matrix
#'
#' optimal intercept when phi is zero is log number of voters for each party up to a constant per group.
#' Constant is calculated based on number of non participants
#'
#' @param Y
#' @param NPY
#' @param eta
#'
#' @return
#' @export
#'
#' @examples
init.intercept <- function(Y, Z.group = NULL, group.cnst = NULL) {
  # initiate based on Y
  intercept <- log(apply(Y, 2, sum))
  if (!is.null(group.cnst)) {
    # calculate voting utility in each group based on Gumbel distribution
    tmp <- aggregate(intercept, list(Z.group), mean.gumbel)
    tot.U <- tmp[, 2]
    names(tot.U) <- tmp[, 1]
    # calculate difference between estimated and default constant
    dif <- -group.cnst - tot.U[names(group.cnst)]
    # fix constant to get to group constant
    intercept <- intercept + dif[Z.group]
  }
  # replace -Inf with a Bayesian correction of 1 observation
  infs <- intercept == -Inf
  intercept[infs] <- 0
  if (!is.null(group.cnst)) {
    intercept[infs] <- dif[Z.group[infs]]
  }
  # return
  intercept
}


#' Gumbel Expectation
#'
#' Calculates expected value of a Gumbel variable that is the maximum of a vector of Gumbel with expectation x
#'
#'
#'
#' @param x vector of expectations
#'
#' @return value
#' @export
#'
#' @examples
mean.gumbel <- function(x) {
  log(sum(exp(x)))
}


#' Cross Validation Choice of Lambda
#'
#' Test estimation performance in prediction on a test group to select the best value for lambda
#'
#' This function loops over potential values for lambda, estimate the model for each of them using a training set, and tests the likelihood of predicting in the test group
#' @param X voters data matrix
#' @param Z party data matrix
#' @param Y outcome matrix such that Y_{i,j} = 1 iff i voted for j and0 otherwise
#' @param X.group option set for voter (country/wave)
#' @param Z.group options set for party (country/wave)
#' @param eps minimal lambda considered, as share of maximal lambda (default is 1/1000)
#' @param fold fold for cross-validation. if 10, 9/10 will be train and 1/10 will be test.
#' @param n.values number of lambda values to check (default is 100)
#' @param conv.crit convergence criteria 1 for model estimation (minimal improvement of objective function)
#' @param conv.crit2 convergence criteria 2 for model estimation (minimal change in Phi required)
#' @param lag compare beween this iteration and an iteration that came "lag" before
#' @param max.iter maximum number of iterations to consider in estimation the model for each lambda
#' @param file.name for writing logs
#' @param sample.weight sample weights
#' @param NP.vec non participation (not used)
#' @param wave for writing logs
#' @param seed seed for randomization (for splitting the data)
#'
#' @return
#' @param lmbd sequence of lambdas that were tried
#' @param scores vector of likelihood for each lambda (name is the value of lambda)
#' @param phi list of phi for each lambda (can then be used to inititate actual estimation)
#' @param intercept list of estimated intercepts for each lambda
#' @param tst indices for values used as test
#' @export
#'
#' @examples

cv.lambda <- function(X, Z, Y, X.group, Z.group, eps = .001, fold = 10, n.values = 100,
                       conv.crit = 0.1, conv.crit2 = .01, lag = 10, max.iter = 10000,
                       file.name = NULL,
                       sample.weight = NULL,
                       NP.vec = NULL, wave = NULL, seed = 1234, bootstrapSeed = NULL,
                       warm_start = FALSE) {
  if (is.null(sample.weight)) sample.weight <- rep(1, nrow(X))
  stopifnot(abs(mean(sample.weight) - 1) < .01)
  X <- wt.scale(X, sample.weight)
  Z <- scale(Z)
  set.seed(seed)
  
  smp <- sample(1:nrow(X), floor(nrow(X) / 10))
  
  if (!is.null(NP.vec)) {
    NPY <- expand.matrix(NP.vec, X.group, unique(Z.group))
    eta <- init.eta(X[-smp, ], X.group[-smp], NPY[-smp, ])
    eta.cnst <- eta[[2]]
    eta <- eta[[1]]
  } else {
    NPY <- NULL
    eta <- NULL
    eta.cnst <- NULL
  }
  
  max.lambda <- max.lmbd(X[-smp, ], Z, Y[-smp, ], X.group[-smp], Z.group, eta)
  min.lambda <- max.lambda * eps
  phi <- matrix(0, ncol(X), ncol(Z))
  intercept <- init.intercept(Y[-smp, ], Z.group, eta.cnst)
  lmbd.sq <- exp(seq(log(max.lambda), log(min.lambda), length.out = n.values))
  
  # stash initial values for cold starts
  phi_init <- phi
  intercept_init <- intercept
  eta_init <- eta
  
  phi.list <- list()
  intercept.list <- list()
  eta.list <- list()
  score.list <- list()
  
  count <- 0
  for (lmbd in lmbd.sq) {
    count <- count + 1
    if (!is.null(file.name)) {
      write(paste0(
        ifelse(!is.null(bootstrapSeed), paste0("seed ", bootstrapSeed, " "), ""),
        "wave ", wave, ", lambda ", lmbd, " ", scales::percent(count / n.values, accuracy = 0.1), " ",
        timestamp(prefix = "", suffix = "")
      ), file.name, append = TRUE)
    }
    
    # reset starting point if not warm-starting
    if (!warm_start) {
      phi <- phi_init
      intercept <- intercept_init
      eta <- eta_init
    }
    
    res <- estimate.model(
      X[-smp, ], Z, Y[-smp, ], X.group[-smp], Z.group,
      lmbd, phi, intercept,
      conv.crit = conv.crit, conv.crit2 = conv.crit2, lag = lag, max.iter = max.iter,
      NP.vec = if (!is.null(NP.vec)) NP.vec[-smp] else NULL, eta.t = eta,
      standardize = FALSE, accelerate = FALSE, sample.weight = sample.weight[-smp]
    )
    
    phi.list[[as.character(lmbd)]] <- res$phi
    intercept.list[[as.character(lmbd)]] <- res$intercept
    eta.list[[as.character(lmbd)]] <- res$eta
    
    ent <- entropy.score(
      res$phi, res$intercept,
      X[smp, ], Z,
      X.group[smp], Z.group, Y[smp, ], eta,
      sample.weight = sample.weight[smp]
    )
    score.list[[as.character(lmbd)]] <- ent
    
    if (warm_start) {
      phi <- res$phi
      intercept <- res$intercept
      eta <- res$eta
    }
    
  }
  
  list(
    lmbd = lmbd.sq, scores = score.list,
    phi = phi.list, intercept = intercept.list, eta = eta.list,
    tst = smp
  )
}


#' Cross-validation split into independent per-lambda fits
#'
#' Same computations as cv.lambda with cold starts and no NP.vec, so each lambda can
#' be fitted on a separate worker: setup once, fit each index, then collect.
cv.lambda.setup <- function(X, Z, Y, X.group, Z.group, eps = .001, n.values = 100,
                            sample.weight = NULL, wave = NULL, seed = 1234) {
  if (is.null(sample.weight)) sample.weight <- rep(1, nrow(X))
  stopifnot(abs(mean(sample.weight) - 1) < .01)
  X <- wt.scale(X, sample.weight)
  Z <- scale(Z)
  set.seed(seed)
  smp <- sample(1:nrow(X), floor(nrow(X) / 10))
  max.lambda <- max.lmbd(X[-smp, ], Z, Y[-smp, ], X.group[-smp], Z.group, NULL)
  min.lambda <- max.lambda * eps
  list(X = X, Z = Z, Y = Y, X.group = X.group, Z.group = Z.group, sample.weight = sample.weight,
       smp = smp, wave = wave, n.values = n.values,
       lmbd = exp(seq(log(max.lambda), log(min.lambda), length.out = n.values)),
       phi = matrix(0, ncol(X), ncol(Z)), intercept = init.intercept(Y[-smp, ], Z.group, NULL))
}

cv.lambda.fit <- function(s, i, conv.crit = 0.1, conv.crit2 = .01, lag = 10, max.iter = 10000,
                          file.name = NULL) {
  lmbd <- s$lmbd[i]
  smp <- s$smp
  if (!is.null(file.name)) {
    write(paste0(
      "wave ", s$wave, ", lambda ", lmbd, " ", scales::percent(i / s$n.values, accuracy = 0.1), " ",
      timestamp(prefix = "", suffix = "")
    ), file.name, append = TRUE)
  }
  res <- estimate.model(
    s$X[-smp, ], s$Z, s$Y[-smp, ], s$X.group[-smp], s$Z.group,
    lmbd, s$phi, s$intercept,
    conv.crit = conv.crit, conv.crit2 = conv.crit2, lag = lag, max.iter = max.iter,
    NP.vec = NULL, eta.t = NULL,
    standardize = FALSE, accelerate = FALSE, sample.weight = s$sample.weight[-smp]
  )
  list(phi = res$phi, intercept = res$intercept, eta = res$eta,
       score = entropy.score(
         res$phi, res$intercept,
         s$X[smp, ], s$Z,
         s$X.group[smp], s$Z.group, s$Y[smp, ], NULL,
         sample.weight = s$sample.weight[smp]
       ))
}

# fits[[i]] is the cv.lambda.fit result for lambda index i; returns the cv.lambda result.
cv.lambda.collect <- function(s, fits) {
  phi.list <- list()
  intercept.list <- list()
  eta.list <- list()
  score.list <- list()
  for (i in seq_along(s$lmbd)) {
    key <- as.character(s$lmbd[i])
    phi.list[[key]] <- fits[[i]]$phi
    intercept.list[[key]] <- fits[[i]]$intercept
    eta.list[[key]] <- fits[[i]]$eta
    score.list[[key]] <- fits[[i]]$score
  }
  list(
    lmbd = s$lmbd, scores = score.list,
    phi = phi.list, intercept = intercept.list, eta = eta.list,
    tst = s$smp
  )
}


#' Expand Matrix - NOT USED
#'
#' interacts a vector with categories, and generate dummy variables for each category
#'
#' Typically used to set non participation matrix by group
#'
#' @param main.vec binaty vector whether you are included
#' @param group.vec categorical vector
#' @param group.names name of the groups
#'
#' @return matrix which is 1 is main vec is 1 and the category of the colum fits
#' @export
#'
#' @examples
expand.matrix <- function(main.vec, group.vec, group.names) {
  NPY <- matrix(0, length(main.vec), length(group.names))
  colnames(NPY) <- group.names
  for (i in 1:length(main.vec)) {
    NPY[i, as.character(group.vec[i])] <- main.vec[i]
  }
  NPY
}



#' Max Lambda
#'
#' Finds the maximal lambda, above which phi is always 0
#'
#' The maximal lambda is the spectral norm (highest eigenvalue) of the gradient when all(phi==0).In this case PGD will never want to change
#'
#' @param X voter matrix
#' @param Z party matrix
#' @param Y outcome matrix where Y_{i,j} = 1 means i voted j and 0 otherwise
#' @param X.group voters option set
#' @param Z.group party options set
#' @param eta0 currently not used (for non-participation)
#' @param group.cnst add group constant to the intercept. does not matter for estimation so not sure why we needed that
#'
#' @return maximum value of lambda
#' @export
#'
#' @examples
max.lmbd <- function(X, Z, Y, X.group, Z.group, eta0 = NULL, group.cnst = NULL) {
  # define intercept as log total Y up to a group constant
  intercept <- init.intercept(Y, Z.group, group.cnst)
  # define phi as 0
  phi <- matrix(0, ncol(X), ncol(Z))
  # assign probabilities based on vote share in each group using simulate model
  P <- exp(simulate.model(X, Z, phi, intercept, X.group, Z.group, eta0)[[1]])
  # get gradient of phi when phi is 0
  grad <- t(X) %*% (Y - P) %*% Z
  # set lambda.max to the spectral norm (highest eigen value)
  abs(s.v.d.2(grad)$d[1])
}

#' Entropy Score
#'
#' Calculate score on new data to assess performance
#'
#' Using parameters and new data, calculate score as sum of log probabilities. Used in cross validation to see fit to new data.
#'
#' @param phi parameter matrix
#' @param intercept intercept parameter (constant for each party)
#' @param new.X voter data
#' @param new.Z party data
#' @param new.X.group voter option sets
#' @param new.Z.group party option sets
#' @param new.Y matrix of who voted for whom
#' @param eta not used (for non-participation)
#' @param new.NPY not used (for non-participation)
#' @param sample.weight sample weights
#'
#' @return scalar of total likelihood (weighted with sample weights )
#' @export
#'
#' @examples
entropy.score <- function(phi, intercept, new.X, new.Z, new.X.group, new.Z.group, new.Y, eta = NULL, new.NPY = NULL, sample.weight = 1) {
  # simulate logP - log probability of each voter to vote to each party
  SM <- simulate.model(new.X, new.Z, phi, intercept, new.X.group, new.Z.group, eta)
  logP <- SM[[1]]
  logNPP <- SM[[2]]
  # sum total log prob and return
  realized.p <- logP * new.Y
  realized.p[is.nan(realized.p)] <- 0
  if (!is.null(new.NPY)) {
    realized.npp <- logNPP * new.NPY[, colnames(logNPP)]
    realized.npp[is.nan(realized.npp)] <- 0
  } else {
    realized.npp <- 0
  }
  sum(realized.p * sample.weight) + sum(realized.npp * sample.weight)
}


#' Initiate Eta - NOT USED
#'
#' Find initial value for eta parameter when phi is zero
#'
#' Use logistic regressions by group to get the optimal coefficients on non participation in each group
#'
#' @param X Voter data
#' @param X.group vector of group for each voter
#' @param NPY indicator equals 1 if someone didnt vote
#'
#' @return eta matrix when ncol(X) == nrow(eta) and ncol(eta) == length(unique(X.group)) - number of groups
#' vector of constants
#' @export
#'
#' @examples
init.eta <- function(X, X.group, NPY) {
  # get all group values
  all.os <- unique(X.group)
  # initiate eta
  eta <- matrix(NA, ncol(X), length(all.os))
  cnst <- rep(NA, length(all.os))
  colnames(eta) <- all.os
  names(cnst) <- all.os
  # loop of all group values
  for (os in all.os) {
    print(os)
    # create unified data
    dat <- data.frame(cbind(X, NPY[, os]))
    colnames(dat)[ncol(dat)] <- "NPY"

    # run logit on subset
    glm.res <- glm(NPY ~ ., "binomial", dat, subset = X.group == os)
    # save results
    eta[, os] <- glm.res$coef[colnames(dat)[-ncol(dat)]]
    cnst[os] <- glm.res$coef[1]
  }
  # replace missing with 0 (these are cases of collinearities)
  eta[is.na(eta)] <- 0
  # return
  list(eta = eta, cnst = cnst)
}

# *************************************************************************
# Estimation (Second stage)
# *************************************************************************

#' Second Stage Estimation
#'
#' Estimate the "beta" coefficients in second stage. The coefficients on the party positions in a regression of party positions (Z) on the party average utility (delta).
#'
#' This function estimates the beta coefficients. It's using a simple OLS so it's pretty fast. That's why typically we run it several times, whenever we want to run the decomposition or get the weights.
#' It weights parties based on their mean number of votes when votes are avialble (waves the counrty participated). In case of more than one wave, it assumes that coefficients change linearly through time.
#' Reduce the dimension of Z to "k" based on the matrix Phi (typically the paramter matrix from first stage).
#' The actual OLS is ran on the party average positions, and the party positions trend (change in case of only 2 waves). Then this is turned back to the dimension of Z.
#' If quad = T, allow for a quadratic function in Z. The quadratic function is the square of the k dimensions. So if V is the matrix that reduces the dimensino of Z, estimate \delta = \beta * (V*Z) + \gamma * (V*Z)^2 where ^2 means take the square of the vector (horrible notation, this is more formally written with matrices)
#'
#' @param Z.list List of party positions by wave.
#' @param Z.os.list List of parties' option sets (countries) by wave
#' @param party.id.list a list of dictionaries from the party name at each wave to a time-consistent name
#' @param w.list weights. should be the number (total weights) of voter for each party.
#' @param phi matrix that we use to reduce the dimension of Z. Typically the estimated Phi from one of the waves.
#' @param FE.list list of the outcome (delta), estimated in the first stage
#' @param k number of dimensions to reduce to.
#' @param quad if True, use a quadratic model
#'
#' @return   list(beta = final.coef,FE = eta,eps = resid.list, quad = matrix.list)
#' @param beta matrix of the coefficients on Z for each wave
#' @param FE party fixed effects (residual which is constant through time)
#' @param eps residuals (residual which is changing through time)
#' @param quad a list of matrices that represent that coefficientss on the interaction of all Zs in case of estimating a quadratic model.

#' @export
#'
#' @examples
Z.coeff <- function(Z.list, Z.os.list, party.id.list, w.list, phi, FE.list, k = 5, quad = F) {
  # reduce dimension of Z based on the matrix phi.
  # take the first k components from the "V" compoonent in an SVD of phi.
  ## Prepare low dimension matrix
  # get V (coefficients on Z from svd decomposition of phi)
  V <- s.v.d.2(phi)$v
  # multiply by inverse SD (since Z is first standardized before multiplied by phi, this is like multiplying by Phi*A when A is the diagonal matrix of inverse SD.)
  isd.V <- diag(apply(Z.list[[length(Z.list)]], 2, function(x) 1 / sd(x))) %*% V
  # multiple (standardized) z with V and take first 5 components
  ZV.list <- lapply(1:length(Z.list), function(i) {
    (as.matrix(Z.list[[i]]) %*% isd.V)[, 1:k]
  })
  # add z^2 (square of the previous k components)
  if (quad) {
    ZV.list <- lapply(ZV.list, function(zvl) cbind(zvl, zvl^2))
    # increaes k to 2k for to include the quadratics
    k <- 2 * k
  }

  # for each wave construct wide party data (variable for each component)
  merged.list <- lapply(1:length(ZV.list), function(i) {
    nms <- rownames(ZV.list[[i]])
    if (!is.null(dim(w.list[[i]]))) {
      w.list[[i]] <- w.list[[i]][1, ]
    }
    res <- cbind(ZV.list[[i]], party.id.list[[i]][nms], FE.list[[i]], w.list[[i]][nms])
    res <- data.frame(res)
    # data includes:
    # the k components (linear transformations of Z)
    colnames(res)[1:k] <- paste0("comp", 1:k)
    # consistent party name
    colnames(res)[k + 1] <- "party.id"
    # delta
    colnames(res)[k + 2] <- "FE"
    # total wgt of voters for this party in this wave
    colnames(res)[k + 3] <- "wgt"
    # wave number
    res$Wave <- i
    # country
    res$country <- Z.os.list[[i]]
    # option set (country and wave nubmer)
    res$option.set <- paste0(Z.os.list[[i]], ".", i)
    res
  })
  # bind all wave to one data set (each party appears once for each wave)
  full.Z <- do.call("rbind", merged.list)
  # verifyweight are numeric
  full.Z$wgt <- as.numeric(full.Z$wgt)
  # change weights to have the same total weights in each option set
  # aggregegate weights by option set
  aggr <- aggregate(full.Z$wgt, list(full.Z$option.set), FUN = function(x) sum(x, na.rm = T))
  aggr.vec <- aggr[, 2]
  names(aggr.vec) <- aggr[, 1]
  # divide by total weights in wave and multiply by 1000. Now weights sum to 1000 in each (country x wave)
  full.Z$wgt <- full.Z$wgt / aggr.vec[full.Z$option.set] * 1000

  # add party id where missing
  mis.pid <- is.na(full.Z$party.id)
  full.Z$party.id[mis.pid] <- paste0("mpid", 1:sum(mis.pid))

  # generate wide data, row for each party
  wide.Z <- reshape(full.Z, idvar = c("party.id", "country"), timevar = "Wave", direction = "wide")

  ## create joint weights per party for all waves. weights are based on total vote share for parties across all waves.
  # sum weights across all waves. This is almost the weights we want but now countries in more waves have more weights.
  wide.Z$wgt.tot <- apply(wide.Z[, paste0("wgt", ".", 1:length(Z.list))], 1, function(x) sum(x, na.rm = T))
  # reweight such that each country have equal weight
  # aggregate total weight per country (should be #of waves * 1000)
  country.wgt <- aggregate(wide.Z$wgt.tot, list(wide.Z$country), FUN = sum)
  country.wgt.vec <- country.wgt[, 2]
  names(country.wgt.vec) <- country.wgt[, 1]
  # define variable for total country weight (similar to Stata's egen total)
  wide.Z$cnt.wgt <- country.wgt.vec[wide.Z$country]
  # divide by the total weight. Now all countries should aggregate to the same value (1)
  wide.Z$wgt.tot <- wide.Z$wgt.tot / wide.Z$cnt.wgt

  # Calculate trends in the outcome variable delta. This will be used as the ouctome
  # This is the change in delta across time, which we are trying to explain. Regress delta on wave number and take the slope
  # if there are only 2 waves (most countries) this is just the difference across times.
  # practically this is done by regressing the FE on party.id interacted with wave, using option.set fixed effect (the option.set fixed effect is necessary since adding a constant to all deltas in an option set is meaningless)
  # then take the coefficient on the trend
  trends <- suppressWarnings(felm(FE ~ party.id * Wave | option.set + party.id, full.Z, weights = full.Z$wgt)$coef)
  # one party is left out by default in the regression. let's find out who that is!
  nms <- paste0("party.id", wide.Z$party.id, ":Wave")
  dflt <- which(!(nms %in% rownames(trends)))
  trends <- trends[, 1]
  # add the missing party with a NA value (don't worry, we're going to fill it up soon!)
  trends[nms[dflt]] <- NaN
  # add the change in deltas to the wide data set
  wide.Z$trends <- trends[nms]
  # fill in all dropped out parties  - parties with two non missing but no trend (one for each country due to collinearity)
  wide.Z$trends[is.na(wide.Z$trends) & apply(!is.na(wide.Z[, paste0("FE.", 1:length(FE.list))]), 1, sum) > 1] <- 0
  # demean by country (FWL)
  wide.Z[!is.na(wide.Z[, "trends"]), "trends"] <- lm(trends ~ country, wide.Z, weights = wide.Z$wgt.tot)$res


  # generate Z averages and trends
  # loop over the k Z linear combinations
  for (cn in paste0("comp", 1:k)) {
    # calcualte averages
    wide.Z[, paste0(cn, "_avg")] <- apply(wide.Z[, paste0(cn, ".", 1:length(Z.list))], 1, function(x) mean(as.numeric(x), na.rm = T))
    # demean by country (outcome is also demeaned - FWL)
    wide.Z[!is.na(wide.Z[, paste0(cn, "_avg")]), paste0(cn, "_avg")] <- lm(as.formula(paste0(cn, "_avg~country")), wide.Z, weights = wide.Z$wgt.tot)$res

    # trends
    z.trends <- suppressWarnings(felm(as.formula(paste0(cn, "~party.id*Wave|option.set+party.id")), full.Z, weights = full.Z$wgt)$coef)
    dflt <- which(!(nms %in% rownames(z.trends)))
    z.trends <- z.trends[, 1]
    z.trends[nms[dflt]] <- NaN
    wide.Z[, paste0(cn, "_delta")] <- z.trends[nms]
    # fill in dropped out categories - has less than two missing but no trend
    wide.Z[
      is.na(wide.Z[, paste0(cn, "_delta")]) &
        apply(!is.na(wide.Z[, paste0(cn, ".", 1:length(Z.list))]), 1, sum) > 1,
      paste0(cn, "_delta")
    ] <- 0
    # demean
    wide.Z[!is.na(wide.Z[, paste0(cn, "_delta")]), paste0(cn, "_delta")] <- lm(as.formula(paste0(cn, "_delta~country")), wide.Z, weights = wide.Z$wgt.tot)$res
  }
  ## run OLS (finally!)
  # outcome variable (the trends in delta)
  y. <- wide.Z[, "trends"]
  # independent vars - all the linear components of Z (trends and averages)
  x. <- wide.Z[, grep("^comp[0-9]*_", colnames(wide.Z))]
  # weights
  w <- wide.Z$wgt.tot
  # look only at complete cases (parties that exist in more than one wave and so can be in our data)
  cc <- complete.cases(x., y.)
  ols.mod <- lm(y. ~ ., x., cc, w)
  print(summary(ols.mod))

  # get coefficients from the OLS
  ols.coef <- ols.mod$coefficients
  # coefficient on the average variables. these equal the delta in coefficients beta
  avg.coef <- ols.coef[paste0("comp", 1:k, "_avg")]
  # coefficients on the delta. these equal the average coeffieints across periods
  delta.coef <- ols.coef[paste0("comp", 1:k, "_delta")]
  ## Translate coefficients on the averages and delta to coefficients on the components themselve (k coefficients for each wave)
  coef.matrix <- matrix(NA, k, length(Z.list))

  # just some linear algebra to get the coefficint on the first beta
  # note that "avg.coef" are the coef on the average z. These are the delta of the coefficients (see paper). Also vice versa
  coef.matrix[, 1] <- -0.5 * avg.coef + delta.coef
  for (j in range(2:length(Z.list))) {
    # the rest of the betas are linear trends based on the coefficient on the average zs
    coef.matrix[, j] <- coef.matrix[, j - 1] + avg.coef
  }
  # move from coefficients on k dimensions to coefficients on z
  # NOTE - coefficients yielded are for non standardized Z
  # multiply each V[,1:k] by the estiamted coefficients for that wave
  # (the k lineasr combinations are Z % isd.V, and the linear combinration is Z%*% is.V %*% coef.matrx, so isd.V %*% coef.matrix are the coefficients on Z)
  if (quad) {
    k <- k / 2 # return to original k
  }
  coef.list <- lapply(1:length(Z.list), function(i) {
    isd.V[, 1:k] %*% coef.matrix[1:k, i]
  })
  # bind to one matrix
  final.coef <- do.call(cbind, coef.list)
  rownames(final.coef) <- colnames(Z.list[[1]])

  # get the matrix of coefcients on Z^tZ (Z^2)
  # these are the isd.V matrix to reduce to k, times the coefficient on each of the k components (using a diagonal matrix) times the transpose of isd.V
  # This is equivalent to Sigma_j d_j * (Z %*$isd.V[,j])^2 where d_j is the diagonal components of the diagonal matrix
  matrix.list <- NULL
  if (quad) {
    # turn coefficients into matrices for coefficients on z^2
    matrix.list <- lapply(1:length(Z.list), function(i) {
      isd.V[, 1:k] %*% diag(coef.matrix[(k + 1):(2 * k), i]) %*% t(isd.V[, 1:k])
    })
  }

  ## calculate the residuals given beta
  ## this will be needed for the decomposition
  ## this is simply (\delta - \beta*z)
  # loop over waves
  for (i in 1:length(FE.list)) {
    # to use the original Z, need to make sure we connect each party with right data
    # build a key from the constant name to the name in each wave
    pil <- party.id.list[[i]]
    inv.pil <- names(pil)
    names(inv.pil) <- pil
    # parties that participate in this wave
    par.wave <- wide.Z$party.id %in% party.id.list[[i]]
    # calculate residual for this wave which is:
    wide.Z[par.wave, paste0("res", i)] <-
      # the outcome (\delta) minus
      as.numeric(wide.Z[par.wave, paste0("FE.", i)]) -
      # the predicted value z*\beta
      as.matrix(Z.list[[i]])[inv.pil[wide.Z$party.id[par.wave]], ] %*% final.coef[, i]
    # if quadratic, substract also the quadratic terms
    if (quad) {
      wide.Z[par.wave, paste0("res", i)] <- wide.Z[par.wave, paste0("res", i)] -
        # calculate the quatratic form of each party z'Az is like looking at the diagonal of Z'AZ
        diag(as.matrix(Z.list[[i]])[inv.pil[wide.Z$party.id[par.wave]], ] %*% matrix.list[[i]] %*% t(as.matrix(Z.list[[i]])[inv.pil[wide.Z$party.id[par.wave]], ]))
    }
  }

  ## get fixed effect (eta) - the average of the residuals
  wide.Z$eta <- apply(wide.Z[, paste0("res", 1:length(FE.list))], 1, function(x) mean(as.numeric(x), na.rm = T))
  eta <- wide.Z$eta
  names(eta) <- wide.Z$party.id
  # matrix of residuals (without the "fixed effect)
  for (i in 1:length(FE.list)) {
    wide.Z[, paste0("demean.res", i)] <- wide.Z[, paste0("res", i)] - wide.Z[, "eta"]
  }
  rownames(wide.Z) <- wide.Z$party.id
  resid.list <- lapply(1:length(FE.list), function(i) wide.Z[party.id.list[[i]], paste0("demean.res", i)])

  list(beta = final.coef, FE = eta, eps = resid.list, quad = matrix.list)
}



# *************************************************************************
# Decomposition
# *************************************************************************



#' Decomposition do all
#'
#' This function wraps all the decomposition procedure
#'
#' Gets all data and estimated parameters and create counterfactual support under different scenarios. It also estimates the second stage while doing it.
#' Return the aggregate increase support in each wave under all scenarios.
#' Also the aggregated support where aggregation is done based on weighting on the inverse amount of support at the end (last wave)
#'
#' @param X.list list of voter data
#' @param Z.list list of party data
#' @param Y.list list of voter outcome where $Y_{i,j}$ is an indicator for whether i voted for party j
#' @param phi.list list of phi matrices estimated for each wave
#' @param FE.list list of intercepts estimated for each wave.
#' @param party.id.list list of vectors whose names are the name of the party in each wave and the value is a consistent-over-time name/value for each party.
#' @param party.dat additional information on parties. Last row represents the type of parties we're studying that we'll study the increase of
#' @param X.country list of countries that corresponds to X.list (the country of each voter)
#' @param Z.country list of countries that corresponds to Z.list (the country of each party)
#' @param phi for second stage - the matrix that we'll use for dimension reduction based on SVD. Typically the Phi matrix for last wave
#' @param strt wave to start the decomposition (measure rise relative to)
#' @param final last wave for the decomposition
#' @param bar not used
#' @param k number of dimensions in second stage
#' @param quad indicator. include quadratic terms in second stage?
#' @param yrs for plot
#' @param W.list list of sample weights
#' @param dec.order order to decompose by. vector that should include the following strings: Z, resid, X, param. The first changes first (holding other component to their value at the starting wave)
#' @param return.res return result (the alternative is to just plot them)
#' @param category2 add another category to decompose by (for instance, new entries)
#' @param allowNormStart If True, Normalization for countries that have 0 share at the end, will be based on the starting point
#'
#' @return a table with 4*#waves columns and a row for every country + aggregate. Start wave is fixed to zero. other columns show the counterfactual change when changing only one component, from starting wave until that wave. last row is the average across all counrties, weighted by inverse of total support for PRRP in last wave, normalized to 100%
#' @export
#'
#' @examples
decomp.do.all <- function(X.list, Z.list, Y.list, phi.list, FE.list, party.id.list, party.dat,
                          X.country, Z.country, phi, strt, final = 6, bar = .02, k = 5, quad = F, yrs = NULL, W.list = NULL,
                          dec.order = c("Z", "resid", "X", "param"), return.res = F, category2 = NULL, allowNormStart = F) {
  ## decompose
  ## get counterfactual support for every party by different scenarios
  # fit model (no counterfactuals)
  model.fit <- decomposition(X.list, Z.list, Y.list,
    phi.list, FE.list, party.id.list,
    X.country, Z.country, phi,
    W.list = W.list, k = k, quad = quad
  )

  # first component - counterfactual support when changing only the first component, holding all others constant
  c1 <- decomposition(X.list, Z.list, Y.list,
    phi.list, FE.list, party.id.list,
    X.country, Z.country, phi,
    W.list = W.list, k = k, quad = quad,
    X = ifelse(dec.order[1] == "X", "*", strt),
    param = ifelse(dec.order[1] == "param", "*", strt),
    Z = ifelse(dec.order[1] == "Z", "*", strt),
    resid = ifelse(dec.order[1] == "resid", "*", strt)
  )
  colnames(c1) <- paste0("w", 1:ncol(c1), ".1")

  # second component (change another thing)
  c2 <- decomposition(X.list, Z.list, Y.list,
    phi.list, FE.list, party.id.list,
    X.country, Z.country, phi,
    W.list = W.list, k = k, quad = quad,
    X = ifelse(dec.order[1] == "X", "*", ifelse(dec.order[2] == "X", "*", strt)),
    param = ifelse(dec.order[1] == "param", "*", ifelse(dec.order[2] == "param", "*", strt)),
    Z = ifelse(dec.order[1] == "Z", "*", ifelse(dec.order[2] == "Z", "*", strt)),
    resid = ifelse(dec.order[1] == "resid", "*", ifelse(dec.order[2] == "resid", "*", strt))
  )

  colnames(c2) <- paste0("w", 1:ncol(c2), ".2")

  # third component, three things change with time, only the last is helf fixed
  c3 <- decomposition(X.list, Z.list, Y.list,
    phi.list, FE.list, party.id.list,
    X.country, Z.country, phi,
    W.list = W.list, k = k, quad = quad,
    X = ifelse(dec.order[4] != "X", "*", strt),
    param = ifelse(dec.order[4] != "param", "*", strt),
    Z = ifelse(dec.order[4] != "Z", "*", strt),
    resid = ifelse(dec.order[4] != "resid", "*", strt)
  )
  colnames(c3) <- paste0("w", 1:ncol(c3), ".3")

  # the last component is actually the model fit (everything changes)
  c4 <- model.fit
  colnames(c4) <- paste0("w", 1:ncol(c4), ".4")


  # number of waves
  wvs <- ncol(c1)
  # organize all counterfactulas in one table of parties and support by different scenarios
  # wi.j is the counterfactual support for all party in wave i in counterfactual j
  dat <- merge(c1, c2, by = 0)
  rownames(dat) <- dat$Row.names
  dat <- dat[, -1]
  dat <- merge(dat, c3, by = 0)
  rownames(dat) <- dat$Row.names
  dat <- dat[, -1]
  dat <- merge(dat, c4, by = 0)
  rownames(dat) <- dat$Row.names
  dat <- dat[, -1]
  # merge with party data (e.g. country and PRRP indicator or other category to decompose by)
  dat <- merge(dat, party.dat, by = 0, all.x = T)
  rownames(dat) <- dat$Row.names
  dat <- dat[, -1]


  # aggregate by country
  nc <- ncol(dat)
  # data of only PRRP (or whaterver group decomposed by)
  dat.f <- dat[dat[, nc] == 1, ]
  # sum total support for that party group by country in all counterfactuals
  cnt <- aggregate(dat.f[, 1:(nc - 2)], list(dat.f[, nc - 1]), function(x) sum(x, na.rm = T))
  rownames(cnt) <- cnt[, 1]
  cnt <- cnt[, -1]

  # create data set of is this contry in the wave
  cnt.w <- do.call(rbind, lapply(rownames(cnt), function(cou) {
    unlist(lapply(X.country, function(wv) cou %in% wv))
  }))
  rownames(cnt.w) <- rownames(cnt)
  # concatenate 4 times, for each of the different counterfactuals
  cnt.w <- cbind(cnt.w, cnt.w, cnt.w, cnt.w)

  ## create final data set
  # repalce countries that do not participate in a wave with NA
  cnt.norm <- replace(cnt, !cnt.w, NA)

  # define initial support in first wave
  init <- cnt.norm[, strt]

  # look at difference from start wave in each of the counterfactuals (not sure why we did not just sbtract the baseline from everything but this is also ok)
  cnt.norm[1:wvs] <- cnt.norm[1:wvs] - cnt[, strt]
  cnt.norm[, (wvs + 1):(2 * wvs)] <- cnt.norm[, (wvs + 1):(2 * wvs)] - cnt.norm[, wvs + strt]
  cnt.norm[, (2 * wvs + 1):(3 * wvs)] <- cnt.norm[, (2 * wvs + 1):(3 * wvs)] - cnt.norm[, 2 * wvs + strt]
  cnt.norm[, (3 * wvs + 1):(4 * wvs)] <- cnt.norm[, (3 * wvs + 1):(4 * wvs)] - cnt.norm[, 3 * wvs + strt]

  # look at difference between components
  # currently the table has support given the 4 counterfaculs: change only 1 component, only 2, only 3, all 4
  # to isolate the contribution of each component we subtract the contribution from the prebious counterfactual.
  cnt.norm[, (3 * wvs + 1):(4 * wvs)] <- cnt.norm[, (3 * wvs + 1):(4 * wvs)] - cnt.norm[, (2 * wvs + 1):(3 * wvs)]
  cnt.norm[, (2 * wvs + 1):(3 * wvs)] <- cnt.norm[, (2 * wvs + 1):(3 * wvs)] - cnt.norm[, (wvs + 1):(2 * wvs)]
  cnt.norm[, (wvs + 1):(2 * wvs)] <- cnt.norm[, (wvs + 1):(2 * wvs)] - cnt.norm[1:wvs]

  ## Aggregation
  # calculate the total change in support for that party group (typically PRRP)
  tot.delta <- apply(cnt.norm[, seq(wvs, 4 * wvs, wvs)], 1, sum)

  # define new data set in shares from overall support by the end of the period
  # 1 if the entire support for these groups at the end of the period.
  cnt.rise <- cnt.norm / (init + tot.delta)
  if(allowNormStart){
    end.point = (init + tot.delta)
    dnmtr = ifelse(end.point==0,init, end.point)
    cnt.rise = cnt.norm / dnmtr
  }
  # missing waves
  # replace NA with avgs of pre and post wave
  # if there are only 2 wvs, there's no pre and post.
  if (wvs > 2) {
    for (i in 2:(wvs - 1)) {
      for (j in 0:3)
      {
        cnt.rise[, i + wvs * j] <- replace(cnt.rise[, i + wvs * j], is.na(cnt.rise[, i + wvs * j]), ((cnt.rise[, i + wvs * j - 1] + cnt.rise[, i + wvs * j + 1]) / 2)[is.na(cnt.rise[, i + wvs * j])])
      }
    }
  }
  # get rid of places with a decline to 0 (not included in aggregation) - currently not used as does not happen in any country
  cnt.rise <- replace(cnt.rise, cnt.rise == -Inf, NA)
  # take the mean of the share from final increase
  # equivalent to weight the rise based on the inverse of the share of support
  total <- apply(cnt.rise, 2, function(x) mean(x, na.rm = T))
  # normalize components to sum to 1 in the aggregate
  norm.fact <- sum(total[seq(wvs, 4 * wvs, wvs)])
  total <- total / norm.fact
  # attach the total as another row
  cnt.norm <- rbind(cnt.norm, total)
  rownames(cnt.norm)[nrow(cnt.norm)] <- "total"

  # break results by category. For instance - new entries
  if (!is.null(category2)) {
    # add extra category to party data set
    dat$cat2 <- NA
    dat[rownames(party.dat), "cat2"] <- category2
    # look only at the rise of parties who are 1 based on both categories (the original and the second one. For instance new PRRP)
    category2 <- dat$cat2 * dat[, nc]
    # as before - take a data with only those parties who are both
    dat.f2 <- dat[category2 == 1, ]
    # aggregate by countries
    cnt2 <- aggregate(dat.f2[, 1:(nc - 2)], list(dat.f2[, nc - 1]), function(x) sum(x, na.rm = T))
    rownames(cnt2) <- cnt2[, 1]
    cnt2 <- cnt2[, -1]
    # is this contry in the wave
    cnt.w2 <- do.call(rbind, lapply(rownames(cnt2), function(cou) {
      unlist(lapply(X.country, function(wv) cou %in% wv))
    }))
    rownames(cnt.w2) <- rownames(cnt2)
    cnt.w2 <- cbind(cnt.w2, cnt.w2, cnt.w2, cnt.w2)
    # drop values for countries that did not appear in this wave
    cnt.norm2 <- replace(cnt2, !cnt.w2, NA)

    # look at difference from start
    cnt.norm2[1:wvs] <- cnt.norm2[1:wvs] - cnt2[, strt]
    cnt.norm2[, (wvs + 1):(2 * wvs)] <- cnt.norm2[, (wvs + 1):(2 * wvs)] - cnt.norm2[, wvs + strt]
    cnt.norm2[, (2 * wvs + 1):(3 * wvs)] <- cnt.norm2[, (2 * wvs + 1):(3 * wvs)] - cnt.norm2[, 2 * wvs + strt]
    cnt.norm2[, (3 * wvs + 1):(4 * wvs)] <- cnt.norm2[, (3 * wvs + 1):(4 * wvs)] - cnt.norm2[, 3 * wvs + strt]

    # look at difference between components
    cnt.norm2[, (3 * wvs + 1):(4 * wvs)] <- cnt.norm2[, (3 * wvs + 1):(4 * wvs)] - cnt.norm2[, (2 * wvs + 1):(3 * wvs)]
    cnt.norm2[, (2 * wvs + 1):(3 * wvs)] <- cnt.norm2[, (2 * wvs + 1):(3 * wvs)] - cnt.norm2[, (wvs + 1):(2 * wvs)]
    cnt.norm2[, (wvs + 1):(2 * wvs)] <- cnt.norm2[, (wvs + 1):(2 * wvs)] - cnt.norm2[1:wvs]

    ## aggregate
    # look at ratio from overall far right
    cnt.rise2 <- cnt.rise * 0
    # indices for subset of countries with parties in cat2
    ind.cnt <- sapply(row.names(cnt.norm2), function(n) which(rownames(cnt.rise2) == n))
    cnt.rise2[ind.cnt, ] <- cnt.norm2 / (init + tot.delta)[ind.cnt]
    # replace NA with avgs
    if (wvs > 2) {
      for (i in 2:(wvs - 1)) {
        for (j in 0:3)
        {
          cnt.rise2[, i + wvs * j] <- replace(cnt.rise2[, i + wvs * j], is.na(cnt.rise2[, i + wvs * j]), ((cnt.rise2[, i + wvs * j - 1] + cnt.rise2[, i + wvs * j + 1]) / 2)[is.na(cnt.rise2[, i + wvs * j])])
        }
      }
    }
    # get rid of places of a decline to 0
    cnt.rise2 <- replace(cnt.rise2, cnt.rise2 == -Inf, NA)
    total2 <- apply(cnt.rise2, 2, function(x) mean(x, na.rm = T))
    # divide by overall rise in far right to get share from far right component
    total2 <- total2 / norm.fact
    cnt.norm <- rbind(cnt.norm, total2)
    rownames(cnt.norm)[nrow(cnt.norm)] <- "Category 2"
  }

  # plot results
  if (is.null(yrs)) {
    yrs <- c(1990, 1994, 1999, 2005, 2011, 2016)[strt:final]
  }
  if (return.res) {
    return(cnt.norm)
  } else {
    apply(cnt.norm, 1, function(r) {
      decomp.plot(
        r[strt:final],
        r[(strt:final) + wvs],
        r[(strt:final) + 2 * wvs], yrs, dec.order
      )
    })
  }
}





#' Decomposition
#'
#' Decomposechanges in Vote Share holding some model components fixed
#'
#' Calculate shares for basic three countrerfactual - holding voters/parties/parameters (weights) fixed.Estimates second stage along the way.
#' Produces data frame of counterfactual shares in each waves and plots
#'
#' @param X.list list of voter data sets for each wave
#' @param Z.list list of party data sets for each wave
#' @param phi.list list of phi estimation for each wave
#' @param X.group.list list of voter option sets (usually country) for each wave
#' @param Z.group.list list of partry option sets (usually country) for each wave
#' @param X fixed wave for voters or allow to vary by using "*"
#' @param Z fixed wave for parties (includes vel) or allow to vary by using "*"
#' @param param fixed wave for parameters (beta, phi) or allow to vary by using "*"
#' @param resid fixed wave for residuals or allow to vary by using *
#' @param Y.list list of outcome matrices
#' @param FE.list list of intercepts estimated
#' @param party.id.list list of dictionaries from party name in wave to overall name
#' @param k number of dimensions for second stage
#' @param null.resid not used (can get all residuals to 0)
#' @param return.party.level return party-level decomposition internals in addition to aggregate support shares
#'
#' @return table with simulated voting shares of each party with fixing some components at a constant level and allowing the others to vary
#' @export
#'
#' @examples
decomposition <- function(X.list, Z.list, Y.list, phi.list, FE.list, party.id.list,
                          X.group.list, Z.group.list, phi, W.list = NULL, k = 5, quad = F,
                          X = "*", Z = "*", param = "*", resid = "*", null.resid = F,
                          return.party.level = FALSE) {
  # define sample weights to 1 if they are not given
  if (is.null(W.list)) {
    W.list <- lapply(X.list, function(x) rep(1, nrow(x)))
  }
  # turn into matrices
  X.list <- lapply(X.list, as.matrix)
  Z.list <- lapply(Z.list, as.matrix)

  # name of waves
  nms <- names(X.list)
  if (is.null(nms)) {
    nms <- 1:length(X.list)
  }
  # name of parties
  parties <- unique(unlist(party.id.list))
  # set wave names for each component
  # If * - this means the component changes with time. Otherwise, it is held fixed to the value provuded.
  if (X == "*") {
    X <- nms
  } else {
    X <- rep(X, length(nms))
  }
  if (Z == "*") {
    Z <- nms
  } else {
    Z <- rep(Z, length(nms))
  }
  if (param == "*") {
    param <- nms
  } else {
    param <- rep(param, length(nms))
  }
  if (resid == "*") {
    resid <- nms
  } else {
    resid <- rep(resid, length(nms))
  }
  # create final data
  res <- data.frame(row.names = parties)


  ## decompose FEs
  beta.list <- list()
  # create list of weights for overall support in each party
  wgt.list <- lapply(1:length(Y.list), function(i) {
    t(apply(Y.list[[i]] * W.list[[i]], 2, sum))
  })
  # second stage estimation decomposes the deltas into beta,z, and residual
  FE.dec <- Z.coeff(Z.list, Z.group.list, party.id.list, wgt.list, phi, FE.list, k, quad = quad)
  vel.list <- list()

  # create lists by wave also for beta, and the residual baesd on the estimation results
  for (i in 1:length(nms)) {
    # list of betas
    beta.list[[nms[i]]] <- FE.dec[["beta"]][, i]
    # total residual is the sum of the party fixed effect (constant over waves) and the unexplained part (changes over waves)
    vel.list[[nms[i]]] <- FE.dec[["FE"]][as.character(party.id.list[[nms[i]]])] + FE.dec[["eps"]][[i]]
    # not used but this is a way to only use the party FE and ignore the residuals
    if (null.resid) {
      vel.list[[nms[i]]] <- FE.dec[["FE"]][as.character(party.id.list[[nms[i]]])]
    }
    #
    vel.list[[nms[i]]][is.na(vel.list[[nms[i]]])] <- 0
  }
  # if quad is T, there is also a matrix of coefficients A for z'Az. else this is null
  matrix.list <- FE.dec[["quad"]]
  # expand data to include all parties even when they don't run
  EP <- expand.parties(Z.list, vel.list, Z.group.list, party.id.list)
  # party data including non participants
  Z.new <- EP[[1]]
  # expand residual (-Inf if party did not participate)
  vel.new <- EP[[2]]
  # country for each party. now this is constant across waves since we have a consistent set of countries
  Z.group <- EP[[3]]
  # loop over waves and calculate total support for every party
  # calculate counterfactuals based on what is fixed and what changes
  # res is data of support for each party over the waves
  for (i in 1:length(nms)) {
    if (quad) {
      # it's called simulate shars but it's acutally simulating totals. (just to confuse the enemy)
      shrs <- simulate.shares(X.list[[X[i]]], Z.new[[Z[i]]], phi.list[[param[i]]],
        beta.list[[param[i]]], vel.new[[resid[i]]],
        X.group.list[[X[i]]], Z.group, W.list[[X[i]]],
        mtrx = matrix.list[[param[[i]]]]
      )
    } else {
      shrs <- simulate.shares(
        X.list[[X[i]]], Z.new[[Z[i]]], phi.list[[param[i]]],
        beta.list[[param[i]]], vel.new[[resid[i]]],
        X.group.list[[X[i]]], Z.group, W.list[[X[i]]]
      )
    }

    res[, nms[i]] <- shrs
  }
  # turn totals into shares
  # get overall voters by countries based on weights
  n.wave <- length(Z.list)
  table.list <- lapply(1:n.wave, function(i) {
    tab <- aggregate(W.list[[X[i]]], list(X.group.list[[X[i]]]), FUN = sum)
    res <- tab[, 2]
    names(res) <- tab[, 1]
    res
  })
  # for each wave, create a vector of total voters in the relevant country (similar to stata egen total)
  total.list <- lapply(1:n.wave, function(i) table.list[[i]][Z.group])
  # now divide each voter number by party by the total number of voters
  for (i in 1:length(nms)) {
    res[, nms[i]] <- res[, nms[i]] / total.list[[i]]
  }
  # Return party-level components when downstream residual analyses need them.
  if (return.party.level) {
    return(list(res = res, vel.list = vel.list, vel.new = vel.new, Z.group = Z.group,
                Z.new = Z.new, beta.list = beta.list, X.group.list = X.group.list,
                Z.group.list = Z.group.list, shrs = shrs))
  }
  
  res
}


#' Expand Parties
#'
#' A function that creates a consistent list of parties for each wave and fills up missing party that didn't entry or exit
#'
#' This function creates a consistent data set of parties that does not change across waves. It imputes party positions from nearest wave. Velance is -Inf if party does not particpate
#'
#' @param Z.list list of party position matrices
#' @param vel.list list of residuals for every party
#' @param party.id.list list of dictionaries from party name at the wave levelto a consistent name across waves
#' @param Z.group.list list of countries of each party in each wave
#'
#' @return
#' @param Z.new list of party position matrices for the expanded data set, with consistent parties by consistent order
#' @param vel.new list of residuals for every party
#' @param Z.group one vector of the country for each party based on the expanded list (same for all waves as parties don't switch countries)

#' @export
#'
#' @examples
expand.parties <- function(Z.list, vel.list, Z.group.list, party.id.list) {
  # vector of all the parties
  all.party <- unique(c(unlist(party.id.list)))
  # create a matrix with row for each party in any wave, and coloumns for all party position in the wave with most positions (in practice, all waves have same nubmer of positions, 56)
  nc <- max(unlist(lapply(Z.list, ncol)))
  wnc <- which.max(unlist(lapply(Z.list, ncol)))
  Z.new <- lapply(1:length(Z.list), function(i) {
    res <- (matrix(NA, length(all.party), nc))
    rownames(res) <- all.party
    colnames(res) <- colnames(Z.list[[wnc]])
    res
  })
  # fill in where Z is available
  for (i in 1:length(Z.list)) {
    Z.new[[i]][party.id.list[[i]], ] <- Z.list[[i]][names(party.id.list[[i]]), ]
  }
  # fill up for exits based on previous position
  for (i in 2:length(Z.list)) {
    mis <- apply(Z.new[[i]], 1, function(x) all(is.na(x)))
    Z.new[[i]][mis, ] <- Z.new[[i - 1]][mis, ]
  }
  # fill up for entries based on next position
  for (i in (length(Z.list) - 1):1) {
    mis <- apply(Z.new[[i]], 1, function(x) all(is.na(x)))
    Z.new[[i]][mis, ] <- Z.new[[i + 1]][mis, ]
  }
  # set velance to -Inf. replace with existing value if there is one
  vel.new <- lapply(1:length(Z.list), function(i) {
    res <- rep(-Inf, length(all.party))
    names(res) <- all.party
    res[party.id.list[[i]]] <- vel.list[[i]]
    res
  })
  # find the group for each party based on their new order
  full.group <- data.frame(name = unlist(party.id.list), group = unlist(Z.group.list))
  full.group <- unique(full.group)
  rownames(full.group) <- full.group$name
  Z.group <- full.group[as.character(all.party), "group"]

  list(Z.new, vel.new, Z.group)
}


#' Simulate vote share (actually totals)
#'
#' Simulate vote totals for each party given voters, party positions and model parameters
#'
#' This function simulates the model and aggregates to get Vote totals for each party (does NOT calculate the shares, only total sampling weights that supports each party in expectation)
#' ֿ
#'
#' @param X voter data
#' @param Z party positiobns
#' @param phi matrix of coefficent for voter and party interactions
#' @param beta coefficient only on Z
#' @param velance idiosyncaratic common party shock
#' @param X.group option set for voters (countries/waves)
#' @param Z.group option set for parties (countries/waves)
#'
#' @return vector of shares in the size of nrow(Z) - apparently this return absolute balues and not shares
#' @export
#'
#' @examples
simulate.shares <- function(X, Z, phi, beta, velance, X.group, Z.group, W = 1, standardize = F, mtrx = NULL) {
  # get delta
  zeta <- Z %*% beta + velance
  # add quadratic term to delta if matrix is given (if using quadratic cases)
  if (!is.null(mtrx)) {
    zeta <- zeta + diag(Z %*% mtrx %*% t(Z))
  }
  if (standardize) {
    # standardize before multiplying with phi
    X <- scale(X)
    Z <- scale(Z)
  }
  # simulate the model
  logP <- simulate.model(X, Z, phi, zeta, X.group, Z.group)[[1]]
  P <- exp(logP)
  # aggregate at the party level
  s.hat <- apply(P * W, 2, sum)
  # name by party
  names(s.hat) <- rownames(Z)
  s.hat
}


# *************************************************************************
# Get model weights
# *************************************************************************

#' Get Weights
#'
#' Get the model weights that are places on the party positions by each voter
#'
#' calculate the weights based on w(x_i) = x_i \phi + \beta. It estimates \beta for this (the second stange)
#'
#' @param X.list list of voter attributes
#' @param Z.list list of party positions
#' @param Y.list list of outcomes
#' @param phi.list list of Phi matrices
#' @param FE.list list of fixed effects (\delta)
#' @param party.id.list list of dictionary from party names in wave to consistent coding
#' @param Z.group.list list of parties option sets (parties)
#' @param phi \Phi matrix for the dimension reduction in second stage (typically the \Phi of last wave)
#' @param k number of dimensions in second wave
#' @param W.list sampling weights
#'
#' @return a list of weight vector for each wave
#' @export
#'
#' @examples
get.weights <- function(X.list, Z.list, Y.list, phi.list, FE.list, party.id.list,
                        Z.group.list, phi, k = 5, W.list = NULL) {
  if (is.null(W.list)) {
    W.list <- lapply(X.list, function(X) rep(1, nrow(X)))
  }
  # turn into matrices
  X.list <- lapply(X.list, as.matrix)
  Z.list <- lapply(Z.list, as.matrix)

  # name of waves
  nms <- names(X.list)
  if (is.null(nms)) {
    nms <- 1:length(X.list)
  }
  # calculate vote share for each party
  wgt.list <- lapply(1:length(Y.list), function(i) {
    t(apply(Y.list[[i]] * W.list[[i]], 2, sum))
  })
  # run second stage estimation
  FE.dec <- Z.coeff(Z.list, Z.group.list, party.id.list, wgt.list, phi, FE.list, k)
  beta.list <- list()
  for (i in 1:length(nms)) {
    beta.list[[nms[i]]] <- FE.dec[["beta"]][, i]
  }
  # individual part  - calculate the weights for everyone in the wave
  lapply(nms, function(i) {
    t(t(X.list[[i]] %*% phi.list[[i]]) + beta.list[[i]])
  })
}
