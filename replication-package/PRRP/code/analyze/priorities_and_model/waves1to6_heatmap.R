# *************************************************************************
# File description ----
# *************************************************************************
#
# This file creates comparable heatmaps of changes in voter self-reported
# ideology over multiple wave intervals. It recreates the wave-4-to-wave-6
# heatmap and adds wave-1-to-wave-4 comparisons using the all-wave data.
#
# Main goals:
#   (1) Recreate the wave-4-to-wave-6 heatmap with a common color scale
#   (2) Create a wave-1-to-wave-4 heatmap from the all-wave data
#   (3) Create a wave-4-to-wave-6 heatmap using the all-wave data subset
#   (4) Save individual and combined heatmap outputs
#
# Analysis includes:
#   - Country exclusions for early-wave comparability
#   - Quantile construction for economic and cultural voter indices
#   - Nonparametric surface fitting with npreg::gsm
#   - Prediction-grid differencing across wave pairs
#   - Common color-scale construction across heatmaps
#
# Outputs:
#   - output/figures/heat_map_voters/voters_heatmap_4to6_rescale.pdf
#   - output/figures/heat_map_voters/voters_heatmap_1to4.pdf
#   - output/figures/heat_map_voters/voters_heatmap_46_V2.pdf
#
# *************************************************************************

# Clear the workspace so the script does not depend on previously sourced plots.
rm(list = ls())

# Load plotting, data manipulation, and layout packages.
library(dplyr)
library(ggplot2)
library(patchwork)
library(tidyr)

# Source project constants and shared heatmap helper functions.
source("code/util/commonVars.R")
source("code/analyze/priorities_and_model/waves1to6_func_plot_weight.R")

# Define countries excluded for early-wave comparability.
countries_to_remove <- c("Switzerland", "Lithuania", "Estonia", "Poland", "Norway")

# Load the original wave 4/6 heatmap data.
DATA_46_ORIGINAL <- readRDS(paste0(cleandata_path, "/manifesto/wvs_manifesto_with_indexes.rds")) %>%
  filter(BB_Wave_num %in% c(4, 6))

# Load the all-wave data and remove countries with problematic early-wave coverage.
DATA_ALL_WAVES <- readRDS(paste0(cleandata_path, "/manifesto/waves1to6_wvs_manifesto_with_indexes.rds")) %>%
  filter(!BB_Country %in% countries_to_remove)

# Compute the three distinct heatmap differences before setting the common scale.
DIFF_WAVES_PREDS <- compute_heatmap_difference(DATA_46_ORIGINAL, earlier_wave = 4, later_wave = 6)
DIFF_WAVES_PREDS_16 <- compute_heatmap_difference(DATA_ALL_WAVES, earlier_wave = 1, later_wave = 4)
DIFF_WAVES_PREDS_46 <- compute_heatmap_difference(DATA_ALL_WAVES, earlier_wave = 4, later_wave = 6)

# Use one color scale for all heatmaps so shades are comparable across panels.
all_changes <- c(
  DIFF_WAVES_PREDS$Change,
  DIFF_WAVES_PREDS_16$Change,
  DIFF_WAVES_PREDS_46$Change
)
global_limits <- range(all_changes, na.rm = TRUE)

# Build the original wave 4-to-6 heatmap with annotations and the shared scale.
p1 <- plot_heatmap_difference(
  DIFF_WAVES_PREDS,
  color_limits = global_limits,
  contour_breaks = c(-0.05, 0.05),
  annotations = TRUE
)

# Save the rescaled version of the original wave 4-to-6 heatmap.
ggsave(
  plot = p1,
  "output/figures/heat_map_voters/voters_heatmap_4to6_rescale.pdf",
  width = figureWidth,
  height = figureWidth / 1.33,
  units = "cm",
  dpi = 600
)

# Build the all-wave wave 1-to-4 heatmap with the shared scale.
p2 <- plot_heatmap_difference(
  DIFF_WAVES_PREDS_16,
  color_limits = global_limits,
  contour_breaks = c(-Inf, Inf)
)

# Save the all-wave wave 1-to-4 heatmap.
ggsave(
  plot = p2,
  "output/figures/heat_map_voters/voters_heatmap_1to4.pdf",
  width = figureWidth,
  height = figureWidth / 1.33,
  units = "cm",
  dpi = 600
)

# Build the all-wave wave 4-to-6 heatmap with the shared scale.
p3 <- plot_heatmap_difference(
  DIFF_WAVES_PREDS_46,
  color_limits = global_limits,
  contour_breaks = c(-Inf, Inf)
  )

# Save the all-wave wave 4-to-6 heatmap.
ggsave(
  plot = p3,
  "output/figures/heat_map_voters/voters_heatmap_46_V2.pdf",
  width = figureWidth,
  height = figureWidth / 1.33,
  units = "cm",
  dpi = 600
)
