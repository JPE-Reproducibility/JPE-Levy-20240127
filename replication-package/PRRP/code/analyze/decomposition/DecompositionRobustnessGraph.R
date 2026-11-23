################################################################################
# DESCRIPTION:
# The following script generates Figure 5: Decomposition Robustness, and 
# Figure A.2: Additional Decomposition Robustness for the manuscript.
# 
# It aggregates results from various model specifications to perform 
# comprehensive robustness checks, comparing Bliss-point models, Shorrocks 
# indices, and different K-values against the main decomposition.
# 
# INPUTS:
# - Standard robustness check files (w3 prefix):
#   * original:  datasets/model/dec_4_6.Rds
#   * quad:      datasets/model/dec_4_6_quad.Rds
#   * k3:        datasets/model/dec_4_6_k3.Rds
#   * k7:        datasets/model/dec_4_6_k7.Rds
#   * no_dem:    datasets/model/dec_4_6_no_dem.Rds
#.  * cohort:     datasets/model/dec_4_6_cohort.Rds
#.  * rich: datasets/model/dec_4_6_westNorth.Rds
#.  * poor: datasets/model/dec_4_6_eastSouth.Rds
#   * with_CZSK: datasets/model/dec_4_6_with_CZSK.Rds
#   * NoGRIT:    datasets/model/dec_4_6_NoGRIT.Rds
#   * pledge:    datasets/model/dec_4_6_pledge.Rds
# - Permutation results: datasets/model/dec_permutate_order.Rds
# - Leave-one-out results: datasets/model/loo/decomposition.Rds
# - Plotting functions: code/analyze/decomposition/functions_for_plotDecomposition.R
#
# PROCESS:
# 1. Load and adapt all specified datasets into a unified long-format dataframe.
# 2. Generate faceted waterfall plots for the main text and the appendix.
#
# OUTPUTS:
# - Figure 5: Decomposition Robustness:              output/figures/decomposition/Decomposition_Robustness_Waterfalls.pdf
# - Figure A.2: Additional Decomposition Robustness: output/figures/decomposition/Decomposition_Robustness_Waterfalls_Appendix.pdf
# - Figure for R1: Additional Decomposition Robustness:          output/figures/decomposition/Decomposition_Robustness_Waterfalls_R1.pdf
################################################################################

# Clear environment
# Clear environment
rm(list = ls())

# ******************************************************************************
# 1. LOAD DEPENDENCIES & SETUP
# ******************************************************************************
library(tidyverse)
library(readr)
library(scales)
library(stringr)

source("code/util/commonVars.R")
source("code/analyze/decomposition/functions_for_plotDecomposition.R")

outdir <- "output/figures/decomposition/"

# Refactored adaptation function: added 'prefix' argument for flexibility
data_adaption <- function(data_input, prefix = "w3") {
  p_adapted <- data_input %>% 
    select(starts_with(prefix)) %>% 
    filter(row.names(.) == "total") %>%
    rename_all(~str_replace_all(., paste0(prefix, "."), ""))
  return(p_adapted)
}

# ******************************************************************************
# 2. PREPARE PLOTTING DATA
# ******************************************************************************
# --- A. Standard robust check files (w3 prefix) ---
# Mapping: list(type_name = "file_path")
files_map <- list(
  original  = paste0(decomposition_path, "/dec_4_6.Rds"),
  quad      = paste0(decomposition_path, "/dec_4_6_quad.Rds"),
  k3        = paste0(decomposition_path, "/dec_4_6_k3.Rds"),
  k7        = paste0(decomposition_path, "/dec_4_6_k7.Rds"),
  no_dem    = paste0(decomposition_path, "/dec_4_6_no_dem.Rds"),
  cohort    = paste0(decomposition_path, "/dec_4_6_cohort.Rds"),
  poor = paste0(decomposition_path, "/dec_4_6_eastSouth.Rds"),
  rich = paste0(decomposition_path, "/dec_4_6_westNorth.Rds"),
  with_CZSK = paste0(decomposition_path, "/dec_4_6_with_CZSK.Rds"),
  NoGRIT    = paste0(decomposition_path, "/dec_4_6_NoGRIT.Rds"),
  pledge    = paste0(decomposition_path, "/dec_4_6_pledge.Rds"),
  style     = paste0(decomposition_path, "/dec_4_6_style_google.Rds")
)

temp = readRDS(paste0(decomposition_path, "/dec_4_6_eastSouth.Rds"))

# Load and adapt files in a loop to reduce redundancy
adapted_list <- lapply(names(files_map), function(name) {
  read_rds(files_map[[name]]) %>% 
    data_adaption() %>% 
    mutate(type = name)
})

# --- B. Handle Special Case: Permutation/Shorrocks ---
dec_perm <- read_rds(paste0(decomposition_path, "/dec_permutate_order.Rds"))
colnames(dec_perm) <- c("w1.1", "w2.1", "w3.1", "w1.2", "w2.2", "w3.2", 
                        "w1.3", "w2.3", "w3.3", "w1.4", "w2.4", "w3.4")
rownames(dec_perm)[rownames(dec_perm) == "shorrocks"] <- "total"

dec_perm_adapted <- dec_perm %>% 
  data_adaption(prefix = "w3") %>% 
  mutate(type = "perm")

# --- C. Handle Special Case: Leave Country Out (w2 prefix) ---
leave_country_out_dec <- read_rds(paste0(decomposition_path, "/loo/decomposition.Rds"))
leave_country_out_adapted <- leave_country_out_dec %>% 
  data_adaption(prefix = "w2") %>% 
  mutate(type = "leave country out")

# --- D. Merge All ---
merged_data_all <- bind_rows(adapted_list) %>%
  bind_rows(dec_perm_adapted, leave_country_out_adapted) %>%
  rename(
    "Priorities" = `4`,
    "Voter"      = `3`,
    "Residual"   = `1`,
    "Parties"    = `2`
  )

# Helper function to pivot and factor data
prepare_plot_df <- function(data, selected_types, levels, labels) {
  labels_no_panels <- gsub(".*: ", "", labels)
  
  df_long <- data %>%
    filter(type %in% selected_types) %>%
    mutate(country = "total") %>%
    pivot_longer(cols = -c(type, country), names_to = "component", values_to = "value") %>%
    mutate(
      values = round(value, 3),
      typeF = factor(type, levels = levels, labels = labels),
      typeFNoPanel = factor(type, levels = levels, labels = labels_no_panels),
      labelF = factor(component, levels = unique(component)),
      componentF = labelF,
      country = type,
      waveNum = 0
    )
  return(df_long)
}

# --- Main Figure Setup ---
main_types <- c("perm", "leave country out", "quad", "cohort", "style", "pledge", "rich", "poor")
main_labels <- c("Panel A: Shorrocks Index", "Panel B: Leave Country Out",
                 "Panel C: Bliss-Point Model", "Panel D: Cohort", "Panel E: With Party Rhetoric",
                 "Panel F: Pledges", "Panel G: Higher Income Countries", "Panel H: Lower Income Countries")

plot_main_long <- prepare_plot_df(merged_data_all, main_types, main_types, main_labels)

# --- Appendix Figure Setup ---
appd_types <- c("original", "no_dem", "k3", "k7", "with_CZSK", "NoGRIT")
appd_labels <- c("Panel A: Main Decomposition", "Panel B: No Demographics", "Panel C: K = 3",
                 "Panel D: K = 7", "Panel E: With Czech Republic and Slovakia", "Panel F: Without Greece and Italy")

plot_appd_long <- prepare_plot_df(merged_data_all, appd_types, appd_types, appd_labels)

# ******************************************************************************
# 3. GENERATE & SAVE WATERFALL PLOTS
# ******************************************************************************

# --- Main Robustness Plot ---
plot_main_final <- prepareDecompositionPlot(plot_main_long, adjustHeight = 0.07) %>% ungroup()

createCommonPlot(plot_main_final[plot_main_final$values != 0, ], basicPlot = FALSE, legendPosition = "null", textSize = 2.5) + 
  facet_wrap(~typeF, nrow = 4) +
  scale_x_discrete(limits = labels.pattern[1:4]) +
  theme(strip.text.x = element_text(face = "bold"), strip.background = element_blank())

ggsave("output/figures/decomposition/Decomposition_Robustness_Waterfalls.pdf", width = figureWidth * 0.9, height = 17, units = "cm")

# --- Appendix Robustness Plot ---
plot_appd_final <- prepareDecompositionPlot(plot_appd_long, adjustHeight = 0.07) %>% ungroup()

createCommonPlot(plot_appd_final[plot_appd_final$values != 0, ], basicPlot = FALSE, legendPosition = "null", textSize = 2.5) + 
  facet_wrap(~typeF, nrow = 4) +
  scale_x_discrete(limits = labels.pattern[1:4]) +
  theme(strip.text.x = element_text(face = "bold"), strip.background = element_blank())

ggsave("output/figures/decomposition/Decomposition_Robustness_Waterfalls_Appendix.pdf", width = figureWidth * 0.9, height = 17, units = "cm")
