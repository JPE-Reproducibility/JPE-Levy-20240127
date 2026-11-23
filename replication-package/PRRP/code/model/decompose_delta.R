# *************************************************************************
# Description ----
# *************************************************************************

# This file does decomposition given the model results

# Input
## paste0(model_results_path, "/model_wave_g") for g= 4,5,6

# Output
# Main Outcomes
## paste0(decomposition_path, "/dec_4_6.Rds")
# Robustness
## paste0(decomposition_path, "/dec_4_6_k3.Rds")
## paste0(decomposition_path, "/dec_4_6_k7.Rds")
## paste0(decomposition_path, "/dec_4_6_new.Rds")
## paste0(decomposition_path, "/dec_4_6_quad.Rds")
## paste0(decomposition_path, "/dec_4_6_no_dem.Rds")

# *************************************************************************
# SETUP ----
# *************************************************************************
# 

rm(list = ls())
source("code/util/commonVars.R")
source("code/model/clean_data_model.R")
source("code/model/func_nuclear_and_pgd.R")
outdir <- "output/figures/decomposition/"

# Taking only waves 4,5,6
grps <- 4:6

NO_DEMOGRAPHICS <- file.exists("code/noDemographicsConfig")
WITHCZSK <- file.exists("code/withCZSKConfig")
NOGRIT <- file.exists("code/noGRITConfig")
PLEDGE <- file.exists("code/pledgeConfig")
STYLE <- file.exists("code/styleGoogleConfig")
COHORT <- file.exists("code/cohortConfig")
EASTSOUTH <- file.exists("code/eastSouthConfig")
WESTNORTH <- file.exists("code/westNorthConfig")

filepath <- paste0(model_results_path, "/model_wave_")

filesuffix <- ""
if (NO_DEMOGRAPHICS) {
  filesuffix <- "_no_dem"
} 

if (WITHCZSK) {
  filesuffix <- "_with_CZSK"
}

if (NOGRIT) {
  filesuffix <- "_NoGRIT"
}

if (PLEDGE) {
  filesuffix <- "_pledge"  
}

if (STYLE) {
  filesuffix <- paste0("_style_", style_variant)
}

if (COHORT) {
  filesuffix <- "_cohort"  
}
if (EASTSOUTH) {
  filesuffix <- "_eastSouth"  
}
if (WESTNORTH) {
  filesuffix <- "_westNorth"
}

# *************************************************************************
# Load Data ----
# *************************************************************************
# 

# Load X and Z used in the data
X.list <- lapply(grps, function(g) {
  readRDS(paste0(filepath, g, filesuffix, ".Rds"))$X
})
Z.list <- lapply(grps, function(g) {
  readRDS(paste0(filepath, g, filesuffix, ".Rds"))$Z
})

# Load model estimates
mod.list <- lapply(grps, function(g) {
  readRDS(paste0(filepath, g, filesuffix, ".Rds"))$mod
})
phi.list <- lapply(mod.list, function(m) {
  m$phi
})

# choose the last estimated phi for the dimension reduction in the second stage
phi <- phi.list[[length(phi.list)]]

# sampling weights (also used for the weighted SD of X below)
W.list <- lapply(grps, function(g) {
  readRDS(paste0(filepath, g, filesuffix, ".Rds"))$W
})

# account for standardization. after this, phi can multiply X and Z themselves (not standardized)
# X was standardized with the weighted SD in estimation (wt.scale), Z with the unweighted SD (scale)
phi.list <- lapply(grps - min(grps) + 1, function(g) {
  diag(inv.wt.sd(X.list[[g]], W.list[[g]])) %*%
    phi.list[[g]] %*%
    diag(apply(Z.list[[g]], 2, sd)^-1)
})

# Option Sets
X.os.list <- lapply(grps, function(g) {
  readRDS(paste0(filepath, g, filesuffix, ".Rds"))$X.os
})
Z.os.list <- lapply(grps, function(g) {
  readRDS(paste0(filepath, g, filesuffix, ".Rds"))$Z.os
})

# Other variables
Y.list <- lapply(grps, function(g) {
  readRDS(paste0(filepath, g, filesuffix, ".Rds"))$Y
})
FE.list <- lapply(mod.list, function(m) {
  m$intercept
})

# *************************************************************************
# Clean Data ----
# *************************************************************************
# 

# demean all Xs by the global mean
mean.X.wave <- lapply(grps - min(grps) + 1, function(g) t(apply(X.list[[g]], 2, function(x) weighted.mean(x, W.list[[g]]))))
mean.X.global <- (Reduce("+", mean.X.wave) / length(grps))[1, ]
X.list <- lapply(grps - min(grps) + 1, function(g) t(t(X.list[[g]]) - mean.X.global))

# change to get the intercept without standardization
# the intercept is the utility for the voter with X=0. It was estimated when 0 was the wave average. Change to the average across all waves.
# do this by adding the difference in utility of the wave average and the global average
FE.list <- lapply(grps - min(grps) + 1, function(g) {
  t(FE.list[[g]] -
    (mean.X.wave[[g]] - mean.X.global) %*%
    phi.list[[g]] %*% t((Z.list[[g]])))
})

# Create dictionary from party name in each wave to a consistent name in all waves
Z.all$nms <- paste0(Z.all$Y_manifesto_key)
key <- Z.all[, c("nms", "Y_manifesto_party_id")]
key <- key[!duplicated(key), ]
rownames(key) <- key$nms
party.id.list <- lapply(Z.list, function(z) {
  v <- as.character(key[
    substr(rownames(z), 1, nchar(rownames(z)) - 1),
    "Y_manifesto_party_id"
  ])
  names(v) <- rownames(z)
  v
})

# verify no NA
for (i in grps - min(grps) + 1) {
  stopifnot(!any(is.na(party.id.list[[i]])))
}

# drop wave number for os list
X.country <- lapply(X.os.list, function(d) sapply(d, function(s) substr(s, 1, nchar(s) - 1)))
Z.country <- lapply(Z.os.list, function(d) sapply(d, function(s) substr(s, 1, nchar(s) - 1)))

# data set with party id, country and whether the party is considered PRRP

party.dat <- unique(X.all[, c("Y_manifesto_party_id", "BB_Country", "Y_FR_Pop")])
party.dat <- party.dat |>
  mutate(Y_manifesto_party_id = as.character(Y_manifesto_party_id)) |>
  as.data.frame()
rownames(party.dat) <- party.dat[, 1]
party.dat <- party.dat[, -1]

# *************************************************************************
# Decomposition ----
# *************************************************************************

dec4.6 <- decomp.do.all(X.list, Z.list, Y.list, phi.list, FE.list, party.id.list, party.dat, X.country, Z.country, phi, strt=1, final=3,
  bar = -1,
  W.list = W.list, dec.order = c("resid", "Z", "X", "param"), k = 5,
  return.res = T
)
print(dec4.6)
saveRDS(dec4.6, paste0(paste0(decomposition_path, "/dec_4_6"), filesuffix, ".Rds"))

if (!NO_DEMOGRAPHICS & !WITHCZSK & !NOGRIT & !PLEDGE & !STYLE & !COHORT & !EASTSOUTH & !WESTNORTH) {
  # Smaller k (k=3 instead of k=5)
  dec4.6_k3 <- decomp.do.all(X.list, Z.list, Y.list, phi.list, FE.list, party.id.list, party.dat, X.country, Z.country, phi, strt=1, final=3,
                             bar = -1,
                             W.list = W.list, dec.order = c("resid", "Z", "X", "param"), k = 3,
                             return.res = T
  )
  print(dec4.6_k3)
  saveRDS(dec4.6_k3, paste0(paste0(decomposition_path, "/dec_4_6_k3"), filesuffix, ".Rds"))
}

# We only run the following robustness for main specification
if (!NO_DEMOGRAPHICS & !WITHCZSK & !NOGRIT & !PLEDGE & !STYLE & !COHORT & !EASTSOUTH & !WESTNORTH) {
  # larger k (k=7, instead of k=5)
  dec4.6_k7 <- decomp.do.all(X.list, Z.list, Y.list, phi.list, FE.list, party.id.list, party.dat, X.country, Z.country, phi, strt=1, final=3,
    bar = -1,
    W.list = W.list,
    dec.order = c("resid", "Z", "X", "param"), k = 7,
    return.res = T
  )
  print(dec4.6_k7)
  saveRDS(dec4.6_k7, paste0(paste0(decomposition_path, "/dec_4_6_k7"), filesuffix, ".Rds"))

  # quadratic
  dec4.6_q <- decomp.do.all(X.list, Z.list, Y.list, phi.list, FE.list, party.id.list, party.dat, X.country, Z.country, phi, strt=1, final=3,
    bar = -1,
    W.list = W.list,
    dec.order = c("resid", "Z", "X", "param"), k = 3, quad = T,
    return.res = T
  )
  print(dec4.6_q)
  saveRDS(dec4.6_q, paste0(paste0(decomposition_path, "/dec_4_6_quad"), filesuffix, ".Rds"))


  # run for new entries
  parties.last.wave <- party.id.list[[3]][colnames(Y.list[[3]])]
  parties.first.wave <- party.id.list[[1]][colnames(Y.list[[1]])]
  entries <- parties.last.wave[!(parties.last.wave %in% parties.first.wave)]
  # decomposition for subcategory that explain how much this generates overall
  dec4.6.new <- decomp.do.all(X.list, Z.list, Y.list, phi.list, FE.list, party.id.list, party.dat, X.country, Z.country, phi, strt=1, final=3,
    bar = -1, 
    W.list = W.list, dec.order = c("resid", "Z", "X", "param"),
    return.res = T, category2 = rownames(party.dat) %in% entries
  )
  saveRDS(dec4.6.new[nrow(dec4.6.new), ], paste0(paste0(decomposition_path, "/dec_4_6_new"), filesuffix, ".Rds"))
}

# SAVE EMPTY RESULT FOR PLACEHOLDER FIGURES
if (!file.exists(paste0(decomposition_path, "/dec_4_6_no_dem.Rds"))) {
  dec_empty <- readRDS(file.path(cleandata_path, "metadata", "empty_decomposition.rds"))
  
  saveRDS(dec_empty, paste0(paste0(decomposition_path, "/dec_4_6_no_dem.Rds")))
}

if (!file.exists(paste0(decomposition_path, "/dec_4_6_with_CZSK.Rds"))) {
  dec_empty <- readRDS(file.path(cleandata_path, "metadata", "empty_decomposition.rds"))
  
  saveRDS(dec_empty, paste0(paste0(decomposition_path, "/dec_4_6_with_CZSK.Rds")))
}

if (!file.exists(paste0(decomposition_path, "/dec_4_6_NoGRIT.Rds"))) {
  dec_empty <- readRDS(file.path(cleandata_path, "metadata", "empty_decomposition.rds"))
  
  saveRDS(dec_empty, paste0(paste0(decomposition_path, "/dec_4_6_NoGRIT.Rds")))
}

if (!file.exists(paste0(decomposition_path, "/dec_4_6_pledge.Rds"))) {
  dec_empty <- readRDS(file.path(cleandata_path, "metadata", "empty_decomposition.rds"))
  
  saveRDS(dec_empty, paste0(paste0(decomposition_path, "/dec_4_6_pledge.Rds")))
}

if (!file.exists(paste0(decomposition_path, "/dec_4_6_style_google.Rds"))) {
  dec_empty <- readRDS(file.path(cleandata_path, "metadata", "empty_decomposition.rds"))
  saveRDS(dec_empty, paste0(decomposition_path, "/dec_4_6_style_google.Rds"))
}

if (!file.exists(paste0(decomposition_path, "/dec_4_6_cohort.Rds"))) {
  dec_empty <- readRDS(file.path(cleandata_path, "metadata", "empty_decomposition.rds"))
  
  saveRDS(dec_empty, paste0(paste0(decomposition_path, "/dec_4_6_cohort.Rds")))
}
if (!file.exists(paste0(decomposition_path, "/dec_4_6_eastSouth.Rds"))) {
  dec_empty <- readRDS(file.path(cleandata_path, "metadata", "empty_decomposition.rds"))
  
  saveRDS(dec_empty, paste0(paste0(decomposition_path, "/dec_4_6_eastSouth.Rds")))
}
if (!file.exists(paste0(decomposition_path, "/dec_4_6_westNorth.Rds"))) {
  dec_empty <- readRDS(file.path(cleandata_path, "metadata", "empty_decomposition.rds"))
  
  saveRDS(dec_empty, paste0(paste0(decomposition_path, "/dec_4_6_westNorth.Rds")))
}
