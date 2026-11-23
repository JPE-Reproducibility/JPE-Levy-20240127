################################################################################
# DESCRIPTION:
# This script performs a sensitivity analysis on the decomposition order 
# (Path Dependency). It runs the 'decomp.do.all' function for all possible 
# permutations of the components (Z, X, and parameters) to check if the 
# results are sensitive to the order of operations.
# 
# INPUTS:
# - Data objects: X.list, Z.list, Y.list, phi.list, FE.list, party.dat, etc.
# - Functions: Source files for decomposition and plotting logic.
#
# PROCESS:
# 1. Generate 6 permutations of the components (Z, X, Param).
# 2. Iterate through permutations and run the decomposition model.
# 3. Calculate the Shorrocks Index (average result across all paths).
# 4. Generate and save a faceted waterfall plot for all scenarios.
#
# OUTPUTS:
# - RDS: datasets/model/dec_permutate_order.Rds
# - PDF: output/figures/decomposition/DecompOrder_waterfall_plots.pdf
################################################################################

# Clear environment
rm(list = ls())

# ******************************************************************************
# 1. LOAD DEPENDENCIES & LIBRARIES
# ******************************************************************************
source("code/util/commonVars.R")
source("code/model/decompose_delta.R")
source("code/analyze/decomposition/functions_for_plotDecomposition.R")

library(gtools)
library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)

# ******************************************************************************
# 2. SETUP PERMUTATIONS
# ******************************************************************************

# 1=resid, 2=Z, 3=X, 4=param
comp_names <- c("resid", "Z", "X", "param")

# Create all 3! permutations for Z, X, and param
perms_core <- permutations(n = 3, r = 3, v = c(2, 3, 4))
# Prepend 1 (Residual) as it is always the baseline
perms <- cbind(1, perms_core)

results_list <- list()

# ******************************************************************************
# 3. RUN DECOMPOSITION PERMUTATIONS
# ******************************************************************************

for (i in 1:nrow(perms)) {
  
  current_perm <- perms[i, ]
  
  # Run the core decomposition function
  decomp_output <- decomp.do.all(
    X.list, Z.list, Y.list, phi.list, FE.list, party.id.list, party.dat, 
    X.country, Z.country, phi, strt = 1, final = 3,
    bar = -1,
    W.list = W.list, 
    dec.order = comp_names[current_perm], 
    k = 5,
    return.res = TRUE
  )
  
  # Capture the total result row
  total_row <- decomp_output[nrow(decomp_output), ]
  
  # Map results back to the original column order for consistency
  inv_perm <- invertPerm(current_perm)
  index_mapping <- as.vector(sapply((inv_perm - 1) * 3 + 1, function(x) rep(x, 3)) + c(0, 1, 2))
  
  perm_label <- paste(comp_names[current_perm], collapse = ".")
  results_list[[perm_label]] <- total_row[index_mapping]
  colnames(results_list[[perm_label]]) <- paste0(sapply(comp_names, function(x) rep(x, 3)), 1:3)
}

# Bind into final dataframe
results <- bind_rows(results_list)
rownames(results) <- apply(perms[, 2:4], 1, function(x) paste0(comp_names[x], collapse = "."))

# ******************************************************************************
# 4. CALCULATE SHORROCKS INDEX & SAVE
# ******************************************************************************

shorrocks_row <- colMeans(results, na.rm = TRUE)
results["shorrocks", ] <- shorrocks_row

saveRDS(results, paste0(decomposition_path, "/dec_permutate_order.Rds"))

# ******************************************************************************
# 5. DATA PREPARATION FOR PLOTTING
# ******************************************************************************

fullLevels <- c("Z.X.param", "Z.param.X", "X.Z.param", 
                "X.param.Z", "param.Z.X", "param.X.Z",
                "shorrocks")

fullLabels <- c("1) Positions, 2) Attributes, 3) Priorities", 
                "1) Positions, 2) Priorities, 3) Attributes", 
                "1) Attributes, 2) Positions, 3) Priorities", 
                "1) Attributes, 2) Priorities, 3) Positions", 
                "1) Priorities, 2) Positions, 3) Attributes", 
                "1) Priorities, 2) Attributes, 3) Positions",
                "Shorrocks Index")

labelsNoPanels <- gsub(".*: ", "", fullLabels)

plotData_long <- results %>%
  rownames_to_column("type") %>%
  select(type, resid3, Z3, X3, param3) %>%
  rename(
    Residual = resid3,
    Parties = Z3,
    Voter = X3,
    Priorities = param3
  ) %>%
  mutate(country = "total") %>%
  pivot_longer(cols = -c(type, country), names_to = "component", values_to = "value") %>%
  mutate(
    values = round(value, 3),
    typeF = factor(type, levels = fullLevels, labels = fullLabels),
    typeFNoPanel = factor(type, levels = fullLevels, labels = labelsNoPanels),
    labelF = factor(component, levels = unique(component)),
    componentF = labelF,
    country = type,
    waveNum = 0
  )

# ******************************************************************************
# 6. VISUALIZATION
# ******************************************************************************

plotData_final <- prepareDecompositionPlot(plotData_long, adjustHeight = 0.07) %>% 
  ungroup()

decomp_plot <- createCommonPlot(
  plotData_final, 
  basicPlot = FALSE, 
  legendPosition = "null", 
  textSize = 2.5
) + 
  facet_wrap(~typeF, nrow = 4) +
  scale_x_discrete(limits = labels.pattern[1:4]) +
  theme(
    strip.text.x = element_text(face = "bold"), 
    strip.background = element_blank()
  )

