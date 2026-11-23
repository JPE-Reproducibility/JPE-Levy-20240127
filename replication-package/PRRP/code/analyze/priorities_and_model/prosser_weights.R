# *************************************************************************
# File description ----
# *************************************************************************
#
# This file compares wave-4 vs wave-6 distributions of model weight indices
# under two alternative manifesto-index definitions (Prosser; Bakker-Hobolt).
#
# Main goals:
#   (1) Construct economic and cultural index weights from alternative
#       manifesto-category mappings
#   (2) Plot index-weight distributions across waves 4 and 6
#   (3) Export CSV crosswalks comparing manifesto baseline categories to the
#       Prosser and Bakker-Hobolt definitions
#
# Outputs:
#   - output/figures/model_weight/alternative_cmp_measures/prosser_weight_distr.pdf
#   - output/figures/model_weight/alternative_cmp_measures/bak_hob_weight_distr.pdf
#   - Dropbox raw/Manifesto/econ_index&prosser.csv
#   - Dropbox raw/Manifesto/soc_index&prosser.csv
#
# *************************************************************************
library(haven)
library(ggplot2)
library(dplyr)
library(tidyr)
library(conflicted)
conflict_prefer("summarize", "dplyr")
conflict_prefer("filter", "dplyr")

rm(list=ls())

# Import functions
source("code/util/commonVars.R")
source("code/analyze/priorities_and_model/func_plot_weight.R")

# Output directory:
outdir = paste0("output/figures/model_weight/alternative_cmp_measures")

# WVS data
raw_data_wvs = read_dta(file = pathWVS_Data)
raw_data_wvs <- raw_data_wvs %>%
  left_join(waveLabelsDF, by = c("BB_Wave_num"="waveNum"))

wvs_weights <- raw_data_wvs %>%
  select(BB_Id, BB_Weight_CW) %>%
  distinct()

# Weight data
position_weights = readRDS(file = paste0(model_results_path, "/weights.Rds"))

data_to_plot <- function(econ_vars, soc_vars, negative_vars){
  # Keep only needed identifier and index-component variables
  general_vars <- c("BB_Country", "Y_manifesto_party_id", "Y_FR_Pop", "BB_Id", "wave")

  # Missing variable when focusing only on East Europe
  for (var in c("per415", "per505", "per602")) {
    if(! var %in% names(position_weights)){
      position_weights[[var]]=0
    }
  }

  data <- position_weights %>%
    select(all_of(c(general_vars, econ_vars, soc_vars))) %>%
    left_join(wvs_weights, by = "BB_Id") %>%
    mutate(across(all_of(negative_vars), function(x) x * (-1))) %>%
    mutate(
      econ_index = rowSums(select(., all_of(econ_vars))),
      soc_index = rowSums(select(., all_of(soc_vars)))
    )

  if (any(is.na(data$BB_Weight_CW))) {
    stop("Missing BB_Weight_CW after joining WVS weights by BB_Id")
  }

  data = pivot_longer(
    data,
    cols =! c("BB_Country", "Y_manifesto_party_id", "Y_FR_Pop", "BB_Id", "wave", "BB_Weight_CW"),
    names_to = "variable_original",
    values_to = "value"
  )

  num_vars_econ_index <- length(econ_vars)
  num_vars_soc_index <- length(soc_vars)

  ###########################################
  # Standardization
  ###########################################

  data = data %>%
    mutate(
      value = if_else(variable_original == "econ_index", value / num_vars_econ_index, value),
      value = if_else(variable_original == "soc_index", value / num_vars_soc_index, value)
    )

  # Fix names
  data = data %>%
    mutate(
      variable = if_else(variable_original == "econ_index", econIndexName, NA_character_),
      variable = if_else(variable_original == "soc_index", societyIndexName, variable)
    )

  ################################################
  # Append variable names to plot
  ################################################

  # Prepare cross-walk for weight data
  cw_manfiesto_labels_forWeight=manifesto_labels %>%
    rename(variable_original=variable_name)

  # Merge cross-walk
  data = left_join(data, cw_manfiesto_labels_forWeight)

  # Rename to "variable", which is the name to plot
  data = data %>%
    mutate(variable=if_else(is.na(variable), title, variable))

  return(data)
}

# Source for the indices below (prosser 2014) : https://www.sciencedirect.com/science/article/pii/S0261379414000626
# pages 90 and 98-99
econ_prosser <-  c("per401", "per407" , "per414" , "per505" , "per507" , "per702",
                   "per403" , "per411" , "per412" , "per413" , "per503" , "per504" , "per506" , "per701")

soc_prosser <- c("per109" , "per302" , "per305" , "per601" , "per608",
                 "per105", "per106" , "per107" , "per201" , "per202" , "per301" , "per416" , "per501" , "per502" , "per602" , "per607" , "per704" , "per705", "per706")

negative_vars_prosser <- c("per403" , "per411" , "per412" , "per413" , "per503" , "per504" , "per506" , "per701", # Econ
                           "per105", "per106" , "per107" , "per201" , "per202" , "per301" , "per416" , "per501" , "per502" , "per602" , "per607" , "per704" , "per705", "per706")

econ_bak_hob = c("per401" , "per402" , "per407" , "per410" , "per414" , "per505" , "per507" ,"per702",
                 "per403" , "per404" , "per405" , "per406" , "per412" , "per413" , "per409" , "per415" , "per503" , "per504" , "per506" , "per701")

soc_bak_hob <- c("per305" , "per601" , "per603" , "per605" , "per606" , "per608",
                 "per201" , "per202" ,"per416" , "per501" , "per502" , "per602" , "per604" , "per607" , "per705" , "per706")

negative_vars_bak_hob <- c("per403" , "per404" , "per405" , "per406" , "per412" , "per413" , "per409" , "per415" , "per503" , "per504" , "per506" , "per701", # Econ
                           "per201" , "per202" ,"per416" , "per501" , "per502" , "per602" , "per604" , "per607" , "per705" , "per706")

plot_labs <- raw_data_wvs %>%
  filter(BB_Wave_num %in% c(4, 6)) %>%
  arrange(BB_Wave_num) %>%
  select(waveLabels) %>%
  distinct() %>%
  pull()

plot_weights_by_scheme <- function(df, soc_label, labels) {
  plot_distribution_wave_allpar_dashedline(
    df_parfam = df %>%
      filter(wave %in% c(4, 6)) %>%
      mutate(variable = ifelse(
        variable == "Economic Index",
        "Economic Index Weights",
        ifelse(variable == "Cultural Index", soc_label, variable)
      )),
    labels = labels,
    list_var = c("Economic Index Weights", soc_label),
    differentLines = TRUE
  )
}

# Plot Prosser
PROSSER <- data_to_plot(
  econ_vars = econ_prosser,
  soc_vars = soc_prosser,
  negative_vars = negative_vars_prosser
)

p_prosser <- plot_weights_by_scheme(
  df = PROSSER,
  soc_label = "Social Liberal-Conservative\n Index Weights",
  labels = plot_labs
)

plotTheme = theme(
  strip.background = element_blank(),
  legend.position = "bottom",
  strip.text.x = element_text(face = "bold")
)

p_prosser  <-  p_prosser +
  scale_x_continuous() +
  scale_y_continuous() +
  plotTheme +
  theme(legend.title = element_blank(), legend.key.width = unit(40, "points"))

p_prosser

ggSaveSlide("prosser_weight_distr.pdf",
            plot = p_prosser,
            path = outdir, height = figureHeightSlide*1.15)

# Plot Bakker and Hobolt
BAK_HOB <- data_to_plot(
  econ_vars = econ_bak_hob,
  soc_vars = soc_bak_hob,
  negative_vars = negative_vars_bak_hob
)

p_bak_hob <- plot_weights_by_scheme(
  df = BAK_HOB,
  soc_label = "Libertarian-Authoritarian\n Index Weights",
  labels = plot_labs
)

p_bak_hob  <-  p_bak_hob +
  scale_x_continuous() +
  scale_y_continuous() +
  plotTheme +
  theme(legend.title = element_blank(), legend.key.width = unit(40, "points"))

p_bak_hob

ggSaveSlide("bak_hob_weight_distr.pdf",
            plot = p_bak_hob,
            path = outdir, height = figureHeightSlide*1.15)
