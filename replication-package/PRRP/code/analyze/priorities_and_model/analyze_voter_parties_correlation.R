# *************************************************************************
# File description ----
# *************************************************************************
#
# This file analyzes the relationship between voter opinion indices and party
# position indices, focusing on waves 4 and 6.
#
# Main goals:
#   (1) Merge WVS voter data with manifesto party-position data
#   (2) Construct voter economy/culture indices using weighted PCA and
#       alternative index constructions
#   (3) Plot voter-party opinion-position correlations and export slope tables
#       for all countries and regional subsets
#
# Outputs:
#   - output/figures/voter_party_correlation/voterPartyCorPCA*.pdf
#
# *************************************************************************

# *************************************************************************
# SETUP ----
# *************************************************************************


rm(list=ls())
library(haven)
library(dplyr)
library(ggplot2)
library(data.table)
library(lfe)
library(hutils)
library(xtable)

# Load function
source("code/util/commonVars.R")
source("code/analyze/priorities_and_model/func_plot_weight.R")


# Read WVS data from dropbox:
DATA <- readRDS(file.path(cleandata_path, "manifesto", "wvs_manifesto_with_indexes.rds"))

# *************************************************************************
#  ANALYZE ----
# *************************************************************************

# Function to extract slope from linear model
get_slope <- function(df) {
  # Fit the displayed trend line with the same bin weights used in the plot.
  model <- lm(position ~ opinion, data = df, weights = bin_weight)
  coef(model)[2]
}

build_index_plot_data <- function(data_scope, x_var, quantile_var, y_var, index_label) {
  data_scope %>%
    dplyr::filter(BB_Wave_num %in% c(4, 6)) %>%
    select(
      opinion = all_of(x_var),
      quantile = all_of(quantile_var),
      position = all_of(y_var),
      waveLabels,
      BB_Weight_CW
    ) %>%
    mutate(indexType = index_label)
}

run_voter_party_correlation <- function(data_input, scope_name = "all", country_subset = NULL) {
  DATA_scope <- copy(data_input)

  if (!is.null(country_subset)) {
    DATA_scope <- DATA_scope[BB_Country %in% country_subset]
  }

  # Generate quantiles used for plotting.
  for (x in c("X_econ_pcaS", "X_culture_pcaS")) {
    q <- paste0(x, "Q")
    DATA_scope[, (q) := weighted_ntile(get(x), weights = BB_Weight_CW, n = 20), by = "waveLabels"]
  }

  # Organize data for waves 4 and 6.
  econData <- build_index_plot_data(
    data_scope = DATA_scope,
    x_var = "X_econ_pcaS",
    quantile_var = "X_econ_pcaSQ",
    y_var = "economy_indexS",
    index_label = "Economic Index"
  )
  cultureData <- build_index_plot_data(
    data_scope = DATA_scope,
    x_var = "X_culture_pcaS",
    quantile_var = "X_culture_pcaSQ",
    y_var = "society_indexS",
    index_label = "Culture Index"
  )
  sumForGraphW <- rbindlist(list(econData, cultureData))

  # Compute weighted bin means and retain each bin's total survey weight.
  sumForGraphW <- sumForGraphW[, .(
    opinion = weighted.mean(opinion, BB_Weight_CW, na.rm = TRUE),
    position = weighted.mean(position, BB_Weight_CW, na.rm = TRUE),
    bin_weight = sum(BB_Weight_CW, na.rm = TRUE)
  ),
    by = c("quantile", "waveLabels", "indexType")
  ]


  file_suffix <- if (scope_name == "all") "" else paste0("_", scope_name)

  p <- ggplot(sumForGraphW, aes(x = opinion, y = position, color = waveLabels, shape = waveLabels, linetype = waveLabels, weight = bin_weight)) +
    labs(x = "Voter Opinions", y = "Party Positions") +
    scale_color_manual(values = c("2005-2009" = ParFamColor[2], "2017-2020" = ParFamColor[3])) +
    facet_wrap(~factor(indexType, levels = c("Economic Index", "Culture Index"))) +
    theme_classic(base_size = 10) +
    theme(
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.key.width = unit(2, "cm"),
      legend.text = element_text(size = 10)
    )

  p <- p +
    geom_point(size = 1.2) +
    geom_smooth(method = lm, se = FALSE, formula = y ~ x, alpha = 1, size = 0.5) +
    scale_shape_manual(values = c(2, 19)) +
    scale_linetype_manual(values = c("dashed", "solid"))

  ggsave(
    plot = p,
    filename = paste0("output/figures/voter_party_correlation/voterPartyCorPCA", file_suffix, ".pdf"),
    height = 7 * figureWidth / (1.7 * 8),
    width = 7 * figureWidth / 8,
    units = "cm"
  )


}

analysis_scopes <- list(
  list(scope_name = "all", country_subset = NULL)
)

for (scope_cfg in analysis_scopes) {
  run_voter_party_correlation(
    data_input = DATA,
    scope_name = scope_cfg$scope_name,
    country_subset = scope_cfg$country_subset
  )
}
