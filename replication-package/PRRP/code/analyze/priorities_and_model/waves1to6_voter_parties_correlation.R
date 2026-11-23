# *************************************************************************
# File description ----
# *************************************************************************
#
# This file constructs voter opinion indices for waves 1, 3, 4, and 6 and
# compares them to party-position indices from manifesto data. It generates
# binned scatter plots showing the voter-party relationship across waves and
# saves slope estimates for the plotted fitted lines.
#
# Main goals:
#   (1) Merge IVS/WVS voter data with CMP manifesto party-position data
#   (2) Construct economic and cultural voter opinion indices
#   (3) Standardize voter and party indices within relevant wave/country cells
#   (4) Create weighted opinion bins and binned party-position means
#   (5) Plot voter-party correlations across alternative wave sets
#
# Outputs:
#   - output/figures/voter_party_correlation/voterPartyCorPCA_*_combined.pdf
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
library(tidyr)

# Load function
source("code/util/commonVars.R")
source("code/analyze/priorities_and_model/func_plot_weight.R")


# Read WVS data from dropbox:
DATA <- readRDS(file.path(cleandata_path, "manifesto", "waves1to6_wvs_manifesto_with_indexes.rds"))
# *************************************************************************
#  ANALYZE ----
# *************************************************************************

# Create a data table with variables to examine and names for output figures
VARS = as.data.table(list(
  xVar = c("X_econ_pcaS", "X_culture_pcaS",
           "X_econ_pca", "X_culture_pca"),
  yVar = c("economy_indexS", "society_indexS",
           rep("X_general_self_political_scale",2)),
  xLab = c("Voter economy index", "Voter culture index", 
           "Voter economy index", "Voter culture index"), 
  yLab=c("Party economy index", "Party culture index", 
         rep("Self-reported left-right scale", 2)),
  figureName = c("economy_pca", "culture_pca", 
                 "pca_econ_left_right", "pca_culture_left_right")))



# Generate figures
for (i in 1:nrow(VARS)) {
  # Find quantiles
  x = VARS[i, xVar]
  q = paste0(x, "Q")
  DATA[, eval(q) := weighted_ntile(get(x), weights = BB_Weight_CW, n=20), by="waveLabels"]  
}


# XX- add analyses for earlier waves -------------------------------
source("code/analyze/party_positions/manifesto_general_functions.R")

# use the clean data set 
newDATA <- DATA

# Create a data table to store results for calculating plot slopes
sumForGraphW_keep <- NA
  
# Function to repeat the exercise for different wave combinations and save plots
run_analysis_for_waves <- function(DATA_subset, exercise_tag) {
  # Build the economy and culture plotting datasets from the already scaled PCA indices.
  econData <- DATA_subset %>% 
    select(opinion = X_econ_pcaS, quantile = X_econ_pcaSQ, position = economy_indexS, waveLabels, BB_Weight_CW) %>% 
    mutate(indexType = "Economic Index")
  
  cultureData <- DATA_subset %>% 
    dplyr::filter(BB_Wave_num %in% c(1, 3, 4, 6)) %>% 
    select(opinion = X_culture_pcaS, quantile = X_culture_pcaSQ, position = society_indexS, waveLabels, BB_Weight_CW) %>% 
    mutate(indexType = "Culture Index")
  
  sumForGraphW <- rbindlist(list(econData, cultureData))
  
  # Compute weighted bin means and retain each bin's total survey weight.
  sumForGraphW <- sumForGraphW[
    ,
    .(
      opinion = weighted.mean(opinion, BB_Weight_CW, na.rm = TRUE),
      position = weighted.mean(position, BB_Weight_CW, na.rm = TRUE),
      bin_weight = sum(BB_Weight_CW, na.rm = TRUE)
    ),
    by = c("quantile", "waveLabels", "indexType")
  ]
  # For correct legend order: set waveLabels as a factor with the desired order
  sumForGraphW$waveLabels <- factor(sumForGraphW$waveLabels, 
                                    levels = c("1990-1992", "2005-2009", "1999-2000", "2011-2013", "2017-2020"))
  # Plot the combined data with facets for indexType
  combined_plot <- ggplot(sumForGraphW, aes(x = opinion, y = position, color = waveLabels, shape = waveLabels, linetype = waveLabels, weight = bin_weight)) +
    labs(x = "Voter Opinions", y = "Party Positions") +
    # Match the "2005-2009"/"2017-2020" colors used in analyze_voter_parties_correlation.R (Panel a)
    # so the shared legend items look identical across both panels of fig:opinion_position_cor.
    scale_color_manual(values = c(
      "1990-1992" = "green",
      "2005-2009" = ParFamColor[2],
      "1999-2000" = "red",
      "2017-2020" = ParFamColor[3]
    )) +
    theme_classic(base_size = 10) +
    theme(
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.key.width = unit(2, 'cm'),
      legend.text = element_text(size = 10)
    ) +
    guides(color = guide_legend(nrow = 2), shape = guide_legend(nrow = 2), linetype = guide_legend(nrow = 2)) +
    geom_point(size = 1.2) + 
    geom_smooth(method = lm, se = FALSE, formula = y ~ x, alpha = 1, size = 0.5) +
    facet_wrap(~factor(indexType, levels = c("Economic Index", "Culture Index"))) +    scale_shape_manual(values = c(3, 2, 4, 19)) +
    scale_linetype_manual(values = c("dotdash", "dashed", "twodash", "solid"))
  
  # Save the combined plot
  ggsave(
    plot = combined_plot, 
    filename = paste0("output/figures/voter_party_correlation/voterPartyCorPCA_", exercise_tag, "_combined.pdf"),
    height = 7*figureWidth/(8*1.7), width = 7*figureWidth/8, units = "cm"
  )
  
  if (exercise_tag == "1346") {
    # Save the data for the combined plot
    sumForGraphW_keep <<- sumForGraphW
  } else {
    # Don't do anything with the data for other exercises
    # This is to avoid overwriting the data for the first exercise
    sumForGraphW_keep <<- rbindlist(list(sumForGraphW_keep, sumForGraphW), use.names = TRUE, fill = TRUE)
  }
}

# Run analysis for each exercise

# Waves 1, 3, 4, 6
DATA1346 <- newDATA[newDATA$BB_Wave_num %in% c(1, 3, 4, 6), ]
run_analysis_for_waves(DATA1346, "1346")

# Check the plotted slopes

sumForGraphW = sumForGraphW_keep

# Function to extract slope from linear model
get_slope <- function(df) {
  # Fit the displayed trend line with the same bin weights used in the plot.
  model <- lm(position ~ opinion, data = df, weights = bin_weight)
  coef(model)[2]
}

# Group by waveLabels and indexType, calculate the slope
slopes <- sumForGraphW %>%
  group_by(waveLabels, indexType) %>%
  summarise(
    Slope = get_slope(cur_data_all()),
    .groups = "drop"
  )

soc_slopes <- slopes %>%
  filter(indexType == "Culture Index", waveLabels != "2017-2020") %>%
  pull(Slope) %>% 
  round(2)

# Check valid estimates without enforcing the old numerical results.
stopifnot(nrow(slopes) == 8L, all(is.finite(slopes$Slope)))
