# *************************************************************************
# Description ----
# *************************************************************************

# This file estimates the model for each wave that saves the results which would later be merged

# Output
## paste0(model_results_path, "/model_wave_g") for g= 4,5,6

# *************************************************************************
# SETUP ----
# *************************************************************************
# 

# force single-thread math and sequential futures
#Sys.setenv(
#  OMP_NUM_THREADS = "1",
#  MKL_NUM_THREADS = "1",
#  OPENBLAS_NUM_THREADS = "1"
#)


rm(list = ls())
set.seed(1234)


library(haven)
library(randomForest)
library(dplyr)
library(tidyr)
library(ggrepel)
library(reshape2)
library(future.apply)
library(logging)

# load the function
source("code/model/func_nuclear_and_pgd.R")
source("code/util/commonVars.R")

# load data
source("code/model/clean_data_model.R")
set.seed(1234)
# include weights (country-weights)
INCLUDE.WEIGHTS <- T
# include weights within countries based on sampling weights as well
WITHIN.WEIGHTS <- T

# drop demographics
NO_DEMOGRAPHICS <- file.exists("code/noDemographicsConfig")
WITHCZSK <- file.exists("code/withCZSKConfig")
NOGRIT <- file.exists("code/noGRITConfig")
PLEDGE <- file.exists("code/pledgeConfig")
STYLE <- file.exists("code/styleGoogleConfig")
COHORT <- file.exists("code/cohortConfig")
EASTSOUTH <- file.exists("code/eastSouthConfig")
WESTNORTH <- file.exists("code/westNorthConfig")


default_suffix = ""
if (NO_DEMOGRAPHICS) {
  default_suffix = "_no_dem"
}

if (WITHCZSK) {
  default_suffix <- "_with_CZSK"
}

if (NOGRIT) {
  default_suffix <- "_NoGRIT"
}

if (PLEDGE) {
  default_suffix <- "_pledge"  
}

if (STYLE) {
  default_suffix <- paste0("_style_", style_variant)
}

if (COHORT) {
  default_suffix <- "_cohort"  
}
if (EASTSOUTH) {
  default_suffix <- "_eastSouth"  
}
if (WESTNORTH) {
  default_suffix <- "_westNorth"
}

# *************************************************************************
# Estimate model by wave
# *************************************************************************
# 
# Waves 4-6 are estimated together: every (wave, lambda) cross-validation fit is an
# independent task on the estimationWorkers pool, then each wave's full model is fitted.
# When sourced from estimate_all.R, this file only prepares the variant and queues it, so the
# fits of all variants run together (see run.estimation.jobs).

# Input: Wave number; g = 4,5,6
prepareWave <- function(g) {
  # Pick the relevant wave (g)
  X. <- X[X.all$BB_Wave_num == g, ]
  Z. <- Z[Z.all$BB_Wave_num == g, ]
  X.all.g <- X.all[X.all$BB_Wave_num == g, ]
  Z.all.g <- Z.all[Z.all$BB_Wave_num == g, ]
  # Create a matrix (Y) of voting behavior (row: voters, col: parties)
  y <- paste0(X.all.g$Y_manifesto_key, X.all.g$BB_Wave_num)
  Y <- matrix(0, nrow(X.), nrow(Z.))
  colnames(Y) <- row.names(Z.)
  for (i in 1:length(y)) {
    Y[i, y[i]] <- 1
  }
  # Check if Y is OK (each voters has one party to vote)
  stopifnot(all(apply(Y, 1, sum) == 1))

  # Define option set
  X.os <- paste0(X.all.g$BB_Country, X.all.g$BB_Wave_num)
  Z.os <- paste0(Z.all.g$BB_Country, Z.all.g$BB_Wave_num)
  
  # Define sample weight matrix
  W. <- X.all$BB_Weight_CW[X.all$BB_Wave_num == g]
  
  # Print the sum of W and 
  logging(text = paste0("Wave ", g, ": Sum of weights is ", sum(W.)))
  # Print the differences between sum and the sample size
  logging(text = paste0("Wave ", g, ": Difference between sum of weights and sample size is ", sum(W.) - nrow(X.)))
  # Stop if the sum of the weights are not equal to the sample size
  stopifnot(abs(sum(W.) - nrow(X.)) < 0.01)
  
  # Normalize weights
  #W. <- W. / mean(W.)
  list(g = g, X. = X., Z. = Z., Y = Y, X.os = X.os, Z.os = Z.os, W. = W.)
}

# Input: prepared wave data, its cross-validation result, and the variant's file suffix
finishWave <- function(p, cv.res, suffix) {
  options(populist.log.dir=Sys.getenv("POPULIST_LOG_DIR"))
  g <- p$g
  pdf(file.path(Sys.getenv("POPULIST_LOG_DIR"), paste0("estimation", suffix, "_wave", g, ".pdf")))
  on.exit(dev.off(), add=TRUE)
  lmbd <- cv.res$lmbd[which.max(unlist(cv.res$scores))]

  # plot optimal lambda
  ## @ Oren, do we need this?
  plot(cv.res$lmbd, 
       exp(unlist(cv.res$scores) / nrow(p$X.) * 10), 
       log = "x", 
       pch = 19, 
       ylab = "Average Probability of Prediction", 
       xlab = "lambda",
       main = g)
  abline(v = lmbd, lty = 2)

  
  # Estimate model fully
  ## Improve precision of the model on the full data
  res <- estimate.model(p$X., p$Z., p$Y, p$X.os, p$Z.os, lmbd, cv.res$phi[[as.character(lmbd)]], cv.res$intercept[[as.character(lmbd)]],
    sample.weight = p$W., wave = g,
    conv.crit = conv.crit.model, conv.crit2 = conv.crit2.model, lag = lag.model, max.iter = max.iter.model
  )
  
  # Save the full results
  full.res <- list(X = p$X., Z = p$Z., mod = res, lmbd = lmbd, X.os = p$X.os, Z.os = p$Z.os, Y = p$Y, W = p$W.,
                   cv.tst = cv.res$tst, cv.phi = cv.res$phi[[as.character(lmbd)]])

  saveRDS(full.res, paste0(
    paste0(model_results_path, "/model_wave_"), g,
    ifelse(INCLUDE.WEIGHTS,
      ifelse(WITHIN.WEIGHTS, "", "_country_weights"),
      "_unweighted"), suffix, ".Rds"
  ))
  
  # strength of dimensions
  ## @ Oren, do we need this?
  plot(svd(res$phi)$d, pch = 19, main = g)

  logging(text = paste0("Finished wave", g))
}

# Input: a list of jobs, one per variant: list(suffix, wave.data, cv.setup) for waves 4-6.
# All cross-validation fits of all jobs run as one queue, then all full fits as a second queue.
# The fits are deterministic, so the order of the queue does not change any result.
run.estimation.jobs <- function(jobs) {
  cv.log <- file.path(Sys.getenv("POPULIST_LOG_DIR"), "cvLambda.log")
  # Smallest lambdas converge slowest, so they are queued first.
  cv.tasks <- expand.grid(w = seq_along(jobs[[1]]$cv.setup), j = seq_along(jobs), i = rev(seq_len(n.values.lambda)))
  cv.futures <- lapply(seq_len(nrow(cv.tasks)), function(k) {
    s <- jobs[[cv.tasks$j[k]]]$cv.setup[[cv.tasks$w[k]]]
    i <- cv.tasks$i[k]
    future::future(cv.lambda.fit(s, i, conv.crit = conv.crit.lambda, conv.crit2 = conv.crit2.lambda,
      lag = lag.lambda, max.iter = max.iter.lambda, file.name = cv.log), seed = TRUE)
  })
  cv.fits <- future::value(cv.futures)
  cv.res <- lapply(seq_along(jobs), function(j) lapply(seq_along(jobs[[j]]$cv.setup), function(w) {
    fits <- list()
    for (k in which(cv.tasks$j == j & cv.tasks$w == w)) fits[[cv.tasks$i[k]]] <- cv.fits[[k]]
    cv.lambda.collect(jobs[[j]]$cv.setup[[w]], fits)
  }))
  rm(cv.futures, cv.fits)
  # Waves 6 and 4 take longest, so their full fits are queued first.
  full.tasks <- expand.grid(j = seq_along(jobs), w = c(3, 1, 2))
  full.futures <- lapply(seq_len(nrow(full.tasks)), function(k) {
    j <- full.tasks$j[k]
    w <- full.tasks$w[k]
    p <- jobs[[j]]$wave.data[[w]]
    cv.res.w <- cv.res[[j]][[w]]
    suffix <- jobs[[j]]$suffix
    future::future(finishWave(p, cv.res.w, suffix), seed = TRUE)
  })
  invisible(future::value(full.futures))
}

avilCores <- availableCores()
future::plan(multisession, workers = estimationWorkers)
avilCores = estimationWorkers
logging(text = paste0("Starting file with ", avilCores, " cores available"))
# ONLY SWEDEN: 3:10 minutes 4 cores; # 3:06 minutes only Sweden 1 cores; my new pc 2:30 min; fast server 1:24; fast server 6 cores 22 seconds
# ALL COUNTRIES: 4 cores parallel 6:40 hours. 96 cores (6 parallel):
# ALL countries new desktop 6 hours, precise 32:15 hours

waves <- 4:6
# Prepare each wave and draw its CV holdout on a seeded worker, as before: subsetting there runs
# without tibble loaded, and set.seed() uses the L'Ecuyer-CMRG generator.
prepared <- future::value(lapply(waves, function(g) future::future({
  p <- prepareWave(g)
  list(p = p, s = cv.lambda.setup(p$X., p$Z., p$Y, p$X.os, p$Z.os,
    n.values = n.values.lambda, sample.weight = p$W., wave = p$g))
}, seed = TRUE)))
job <- list(suffix = default_suffix, wave.data = lapply(prepared, `[[`, "p"), cv.setup = lapply(prepared, `[[`, "s"))
rm(prepared)

# Fit now, or leave the fits to estimate_all.R, which runs the queued variants together.
queue <- getOption("populist.estimation.queue")
if (is.null(queue)) {
  system.time(run.estimation.jobs(list(job)))
} else {
  queue$jobs <- c(queue$jobs, list(job))
}

logging(text = "Ending file")
