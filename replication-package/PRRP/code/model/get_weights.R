#get the voting weights (AKA Gidron Weights) of each person
#Oren January 2022
#source("nuclear and PGD.R")
#
#Update August 2022 - work only on waves 4-6 with the new second stage estiamtors (for betas)

#output are weights that are consistent with raw (non-standardized) party postions Z

library(tidyr)
library(labelled) # for var_label


rm(list = ls())
source("code/util/commonVars.R")
source("code/model/clean_data_model.R")
source("code/model/func_nuclear_and_pgd.R")


outdir = "output/figures/decomposition/"
grps = 4:6

scope_configs <- list(
  list(scope_name = "all", model_suffix = "", country_subset = NULL, output_suffix = ""),
  list(scope_name = "westNorth", model_suffix = "_westNorth", country_subset = country_westNorth, output_suffix = "_westNorth"),
  list(scope_name = "eastSouth", model_suffix = "_eastSouth", country_subset = country_eastSouth, output_suffix = "_eastSouth")
)

run_weights_for_scope <- function(model_suffix = "", output_suffix = "", country_subset = NULL) {
  scope_idx <- seq_along(grps)

  model_res <- lapply(grps, function(g) {
    file_path <- paste0(paste0(model_results_path, "/model_wave_"), g, model_suffix, ".Rds")
    if (!file.exists(file_path)) {
      stop(paste0("Missing model file: ", file_path))
    }
    readRDS(file_path)
  })

  X.list <- lapply(model_res, function(m) m$X)
  Z.list <- lapply(model_res, function(m) m$Z)
  smp.W.list <- lapply(model_res, function(m) m$W)
  mod.list <- lapply(model_res, function(m) m$mod)
  X.os.list <- lapply(model_res, function(m) m$X.os)
  Z.os.list <- lapply(model_res, function(m) m$Z.os)
  Y.list <- lapply(model_res, function(m) m$Y)

  phi.list <- lapply(mod.list, function(m) m$phi)
  phi <- phi.list[[length(phi.list)]]

  # Account for standardization.
  # X was standardized with the weighted SD in estimation (wt.scale), Z with the unweighted SD (scale).
  phi.list <- lapply(scope_idx, function(i) {
    diag(inv.wt.sd(X.list[[i]], smp.W.list[[i]])) %*%
      phi.list[[i]] %*%
      diag(apply(Z.list[[i]], 2, sd)^-1)
  })

  for (i in scope_idx) {
    stopifnot(all(apply(Y.list[[i]], 1, sum) == 1))
  }

  FE.list <- lapply(mod.list, function(m) m$intercept)

  # Demean all Xs by the global mean across selected waves.
  mean.X.wave <- lapply(scope_idx, function(i) t(apply(X.list[[i]], 2, function(x) weighted.mean(x,smp.W.list[[i]]))))
  mean.X.global <- (Reduce("+", mean.X.wave) / length(grps))[1, ]
  X.list <- lapply(scope_idx, function(i) t(t(X.list[[i]]) - mean.X.global))

  # Change to get the intercept without standardization.
  FE.list <- lapply(scope_idx, function(i) {
    t(FE.list[[i]] -
        (mean.X.wave[[i]] - mean.X.global) %*%
        phi.list[[i]] %*%
        t(Z.list[[i]]))
  })

  Z.all$nms <- paste0(Z.all$Y_manifesto_key)
  key <- Z.all[, c("nms", "Y_manifesto_party_id")]
  key <- key[!duplicated(key), ]
  rownames(key) <- key$nms

  party.id.list <- lapply(scope_idx, function(i) {
    g <- grps[i]
    z <- Z.list[[i]]
    key_id <- substr(rownames(z), 1, nchar(rownames(z)) - nchar(as.character(g)))
    v <- as.character(key[key_id, "Y_manifesto_party_id"])
    names(v) <- rownames(z)
    v
  })

  for (i in scope_idx) {
    stopifnot(!any(is.na(party.id.list[[i]])))
  }

  target_wave <- max(grps)
  target_filter <- X.all$BB_Wave_num == target_wave
  if (!is.null(country_subset)) {
    target_filter <- target_filter & (X.all$BB_Country %in% country_subset)
  }

  X. <- X.all[target_filter, colnames(X.list[[length(X.list)]])]
  X. <- t(t(X.) - mean.X.global)

  Z.country <- lapply(Z.os.list, function(d) sapply(d, function(s) substr(s, 1, nchar(s) - 1)))

  w.list <- get.weights(
    # Take weights for last-wave Xs fixed.
    lapply(scope_idx, function(u) X.),
    Z.list, Y.list, phi.list, FE.list, party.id.list, Z.country, phi, W.list = smp.W.list
  )

  tmp <- readRDS(manifestoModelData)
  NewZ <- readRDS(paste0(cleandata_path, "/manifesto/manifesto_supply.Rds")) %>%
    select(
      Y_manifesto_key,
      gender, education, age, occupation, marital_status, divorce, minority, femShare
    )

  tmp <- inner_join(tmp, NewZ)

  LABELS <- sapply(tmp, function(x) attributes(x)[["label"]])
  for (i in scope_idx) {
    colnames(w.list[[i]]) <- LABELS[colnames(Z.list[[i]])]
  }

  # Save weights data set.
  wgt.dat <- X.all[target_filter, ]
  for (i in scope_idx) {
    tmp <- w.list[[i]]
    colnames(tmp) <- paste0(colnames(tmp), "_", grps[i])
    wgt.dat <- cbind(wgt.dat, tmp)
  }

  ##########################################
  ######### TRANSFORM WEIGHTS AND SAVE THEM
  ##########################################

  raw_data_manifesto <- readRDS(file = paste0(cleandata_path, "/manifesto/manifesto_populist.rds"))
  temp <- raw_data_manifesto %>%
    select(starts_with("per"), Y_manifesto_key) %>%
    select(-c("pervote", "peruncod"))

  temp <- inner_join(temp, NewZ)
  temp <- temp %>%
    select(-Y_manifesto_key)

  temp_label_cw <- as.data.frame(as.matrix(var_label(temp)))
  temp_label_cw <- tibble::rownames_to_column(temp_label_cw, "variable_original")
  temp_label_cw <- temp_label_cw %>%
    mutate(V1 = as.character(unlist(V1))) %>%
    rename(variable_long = V1)

  weight_wave_suffix <- paste0("_(", paste(grps, collapse = "|"), ")$")
  weight_cols <- grep(weight_wave_suffix, names(wgt.dat), value = TRUE)

  weights <- wgt.dat %>%
    select(
      c("BB_Country", "Y_manifesto_party_id", "id", "BB_Id", "Y_FR_Pop", "Y_Far_Right"),
      all_of(weight_cols)
    )

  weights <- pivot_longer(
    weights,
    cols = !c("BB_Country", "Y_manifesto_party_id", "id", "BB_Id", "Y_FR_Pop", "Y_Far_Right"),
    names_to = c("variable_long", "wave"),
    names_pattern = "(.+)_([0-9]+)$",
    values_to = "value"
  )

  # Join by manifesto data to convert variable names to usual CMP names.
  weights <- left_join(weights, temp_label_cw)
  weights <- weights %>%
    select(-variable_long)
  weights <- pivot_wider(
    weights,
    names_from = variable_original,
    values_from = value
  )

  saveRDS(weights, paste0(paste0(model_results_path, "/weights"), output_suffix, ".Rds"))
}

for (cfg in scope_configs) {
  message("Running get_weights scope: ", cfg$scope_name)
  run_weights_for_scope(
    model_suffix = cfg$model_suffix,
    output_suffix = cfg$output_suffix,
    country_subset = cfg$country_subset
  )
}


