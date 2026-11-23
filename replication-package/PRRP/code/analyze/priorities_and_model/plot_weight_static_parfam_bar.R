# Commented out code adding older white man index and 
# non-establishment index. 
# If we decide to add them back in, 
# we can simply uncomment these sections and re-run the code.

###############################################
# This file plots weights
###############################################
library(tidyverse)
library(haven)
library(ggplot2)
library(dplyr)
library(Hmisc)
library(conflicted)
library(stringr)
library(savetexvalue)

conflict_prefer("summarize", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("if_else", "dplyr")

rm(list=ls())

# Color code
ParFamColor <- c("#999999",  "#5673e9","#E69F00", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# Import functions
source("code/util/commonVars.R")
source("code/analyze/priorities_and_model/func_plot_weight.R")
source("code/analyze/party_positions/manifesto_general_functions.R")

rrpVotersName = "Radical Right\nVoters"

# Output directory:
outdir = "output/figures/model_weight/"

# Keep labels for the added demographic-style indices consistent across plots.
owmIndexName = "Older White Men Index"
nonesIndexName = "Non-establishment Index"

# WVS data
raw_data_wvs <- read_dta(file = pathWVS_Data)
raw_data_wvs <- raw_data_wvs %>%
  left_join(waveLabelsDF, by = c("BB_Wave_num"="waveNum"))

# Keep respondent weights for each model-weight row generated from that respondent.
wvs_weights <- raw_data_wvs %>%
  select(BB_Id, BB_Weight_CW) %>%
  distinct()

# Manifesto data
raw_data_manifesto=readRDS(manifestoModelData)

# Weight data
raw_data = readRDS(file = paste0(model_results_path, "/weights.Rds"))

raw_data  = raw_data %>%
  mutate(econ_index = rowSums(across(any_of(positive_econ_vars))) - rowSums(across(any_of(negative_econ_vars))),
         soc_index = rowSums(across(any_of(positive_soc_vars))) - rowSums(across(any_of(negative_soc_vars))),
         trad_index = per603 - per604,
         soc_no_trad_index = rowSums(across(any_of(positive_soc_vars))) - rowSums(across(any_of(negative_soc_vars))) - trad_index,
         hawk_dove_index = rowSums(across(any_of(positive_hawk_dove_vars))) - rowSums(across(any_of(negative_hawk_dove_vars))),
         hawk_dove_expanded_index = rowSums(across(any_of(positive_hawk_dove_expanded_vars))) - 
            rowSums(across(any_of(negative_hawk_dove_expanded_vars))),
         neg_index = rowSums(across(any_of(negativity_vars))),
         eu_index = rowSums(across(any_of(positive_eu_vars))) - rowSums(across(any_of(negative_eu_vars))),
         gov_index = rowSums(across(any_of(gov_vars))),
        #  owm_index = gender + age - minority,
        #  nones_index = -(education + marital_status + occupation - divorce)
        ) %>% 
  na.omit()

# Cut unnecessary data
raw_data = raw_data %>% select(c("BB_Country", "Y_manifesto_party_id", "Y_FR_Pop", "BB_Id",
                                  "wave", "econ_index", "soc_index", "trad_index", "soc_no_trad_index", 
                                  "hawk_dove_index", "hawk_dove_expanded_index",
                                  "neg_index", "eu_index", "gov_index",
                                  starts_with("per"))) 
# raw_data = raw_data %>% select(c("BB_Country", "Y_manifesto_party_id", "Y_FR_Pop", "BB_Id",
#                                   "wave", "econ_index", "soc_index", "trad_index", "soc_no_trad_index", 
#                                   "hawk_dove_index", "hawk_dove_expanded_index",
#                                   "neg_index", "eu_index", "gov_index", "owm_index", "nones_index",
#                                   starts_with("per"))) 

# Add WVS respondent weights before reshaping the model weights.
raw_data <- raw_data %>%
  left_join(wvs_weights, by = "BB_Id")

# Make data to long
raw_data=pivot_longer(raw_data,
                      cols=!c("BB_Country", "Y_manifesto_party_id", "Y_FR_Pop", "BB_Id", "wave", "BB_Weight_CW"),
                      names_to="variable_original",
                      values_to = "value")

###########################################
# Append Party Info
###########################################

# Create cross-walk from manifesto party id to parfam
cw_party_parfam=make_cw_parfam(raw_data_manifesto)

# Now merge with original data
df_parfam=left_join(raw_data,cw_party_parfam)

df_parfam=df_parfam%>%
  select(BB_Country,BB_Id,Y_manifesto_party_id,Y_FR_Pop,parfam,wave,variable_original,value,BB_Weight_CW)

df_parfam <- df_parfam %>% 
  add_parfam() %>% 
  filter(!is.na(BBF_parfam))

###########################################
# Standardization
###########################################

# Append STD dev
# First Get manifesto data std dev with index
df_manifesto = create_manifesto_std_all(raw_data_manifesto)

# # Build manifesto-side standard deviations for the added demographic-style indices.
# df_manifesto_demo = raw_data_manifesto %>%
#   select(Y_manifesto_key) %>%
#   inner_join(
#     readRDS(paste0(cleandata_path, "/manifesto/manifesto_supply.Rds")) %>%
#       select(Y_manifesto_key, gender, education, age, occupation, marital_status, divorce, minority),
#     by = "Y_manifesto_key"
#   ) %>%
#   mutate(
#     owm_index = gender + age - minority,
#     nones_index = -(education + marital_status + occupation - divorce)
#   ) %>%
#   summarise(across(c(owm_index, nones_index), ~ sd(.x, na.rm = TRUE))) %>%
#   pivot_longer(cols = everything(), names_to = "variable_original", values_to = "sd")

# # Append the new index standard deviations to the existing manifesto scale table.
# df_manifesto = bind_rows(df_manifesto, df_manifesto_demo)

# Then Merge them
df_parfam = left_join(df_parfam, df_manifesto)
df_parfam = left_join(df_parfam, cw_party_parfam)

# Recover levels of each variable (multiply by sd)
# In weights.rds, everything is standardized

num_econ_index_vars <- length(c(negative_econ_vars, positive_econ_vars))
num_soc_index_vars <- length(c(negative_soc_vars, positive_soc_vars))
num_soc_no_trad_index_vars <- num_soc_index_vars - 2
num_trad_index_vars <-2
num_hawk_dove_index_vars <- length(c(negative_hawk_dove_vars, positive_hawk_dove_vars))
num_hawk_dove_expanded_index_vars <- length(c(negative_hawk_dove_expanded_vars,
                                              positive_hawk_dove_expanded_vars))
num_neg_index_vars <- length(c(negativity_vars))
num_eu_index_vars<- length(c(negative_eu_vars, positive_eu_vars))
num_gov_index_vars <- length(c(gov_vars))

df_parfam <- df_parfam %>%
  mutate(value = value * sd) %>%
  mutate(value = if_else(variable_original == "econ_index", value / num_econ_index_vars, value),
         value = if_else(variable_original == "soc_index", value / num_soc_index_vars, value),
         value = if_else(variable_original == "trad_index", value / num_trad_index_vars, value),
         value = if_else(variable_original == "soc_no_trad_index", value / num_soc_no_trad_index_vars, value),
         value = if_else(variable_original == "hawk_dove_index", value / num_hawk_dove_index_vars, value),
         value = if_else(variable_original == "hawk_dove_expanded_index",
                         value / num_hawk_dove_expanded_index_vars, value),
         value = if_else(variable_original == "neg_index", value / num_neg_index_vars, value),
         value = if_else(variable_original == "eu_index", value / num_eu_index_vars, value),
         value = if_else(variable_original == "gov_index", value / num_gov_index_vars, value))

# Fix names
df_parfam=df_parfam %>%
  mutate(variable = if_else(variable_original == "econ_index", econIndexName, NA_character_),
         variable = if_else(variable_original == "soc_index", societyIndexName, variable),
         variable = if_else(variable_original == "trad_index", tradIndexName, variable),
         variable = if_else(variable_original == "soc_no_trad_index", societynotradIndexName, variable),
         variable = if_else(variable_original == "hawk_dove_index", hawkdoveIndexName, variable),
         variable = if_else(variable_original == "hawk_dove_expanded_index", hawkdoveexpandedIndexName, variable),
         variable = if_else(variable_original == "neg_index", negIndexName, variable),
         variable = if_else(variable_original == "eu_index", euIndexName, variable),
         variable = if_else(variable_original == "gov_index", govIndexName, variable),
        #  variable = if_else(variable_original == "owm_index", owmIndexName, variable),
        #  variable = if_else(variable_original == "nones_index", nonesIndexName, variable)
  )
################################################
# Append variable names to plot
################################################
# Prepare cross-walk for manifesto
cw_manfiesto_labels_forManifesto=manifesto_labels%>%
  rename(variable=variable_name)
# Prepare cross-walk for weight data
cw_manfiesto_labels_forWeight=manifesto_labels%>%
  rename(variable_original=variable_name)

# Merge cross-walk
df_parfam=left_join(df_parfam,
                    cw_manfiesto_labels_forWeight)

# Rename to "variable", which is the name to plot
df_parfam=df_parfam%>%
  mutate(variable=if_else(is.na(variable),
                          title,
                          variable))

################################################
# Pick variables to plot in bar plots
################################################
list_index=c(econIndexName,societyIndexName)

# Create list of variables which has highest variance across parfams/total variance, weighted by pervote
list_highvarwgrvar_weighted_sd=create_varlist_highvar_weighted_sd(df_parfam,6)

# Create list of variables based on manifesto
list_mostmention_manifesto = create_varlist_mostmention(raw_data_manifesto, cw_manfiesto_labels_forManifesto)

# Create list of index and variables which has highest variance across parfams/total variance
list_index_and_highbtwgrvar_weighted_sd = append(list_index,list_highvarwgrvar_weighted_sd)

# # Add the demographic-style indices shown in the phi demo plot to the static parfam figure.
# list_index_and_highbtwgrvar_weighted_sd_demo = append(
#   list_index_and_highbtwgrvar_weighted_sd,
#   c(owmIndexName, nonesIndexName)
# )



# Based on manifeto data, pick top 10 diff variables
current_fr_var = "Y_FR_Pop"
prrpVotersName = "Populist Right\nVoters"
list_index_small = c(econIndexName, societyIndexName)

# Load manifesto data
raw_data_manifesto_full = readRDS(file = paste0(cleandata_path, "/manifesto/manifesto_populist.rds"))

raw_data_manifesto_full = raw_data_manifesto_full %>%
  mutate(manifesto_country_name = if_else(manifesto_country_name=="United States", "USA", manifesto_country_name),
         manifesto_country_name = if_else(manifesto_country_name=="United Kingdom", "UK", manifesto_country_name))

#countries_list = country_dict[[1]]
raw_data_manifesto_full = raw_data_manifesto_full %>%
  filter(manifesto_country_name %in% unique(raw_data_wvs$BB_Country))


raw_data_manifesto_full = raw_data_manifesto_full %>%
  select(manifestoYear, manifesto_country_name, Y_FR_Pop, 
         parfam, all_of(starts_with("per")), economy_index, society_index) %>%
  select(-peruncod) %>%
  mutate(wave = if_else(manifestoYear>=2005 & manifestoYear<=2009, 4, NA_real_),
         wave = if_else(manifestoYear>=2017 & manifestoYear<=2020, 6, wave)) 

# Pick top variables to plot
raw_data_manifesto_full_nonindex = raw_data_manifesto_full %>%
  mutate(economy_index = NA, society_index = NA)


list_top_diff_temp = readRDS(paste0(cleandata_path, "/manifesto/top_distinct_vars.rds"))
df_list_top_diff = as.data.frame(list_top_diff_temp)
df_list_top_diff = df_list_top_diff %>%
  rename(variable_original=list_top_diff_temp)
df_list_top_diff=left_join(df_list_top_diff,
                           cw_manfiesto_labels_forWeight)

list_top_diff=df_list_top_diff$title

# Then merge with index
list_index_and_topdiff_manifesto=append(list_index,list_top_diff)


####################################
# Create df for wave 4 and wave 6
####################################
df_parfam_wave4 = df_parfam%>%
  filter(wave==4) %>%
  select(-wave)

df_parfam_wave6=df_parfam%>%
  filter(wave==6)%>%
  select(-wave) 
####################################
# Static plots for wave 6
####################################
p_weight_static_mostmention <- plot_errbar_parfam_static(df_parfam_wave6, list_mostmention_manifesto)

p_weight_static_index_and_highbtwgrvar_weighted_sd <- plot_errbar_parfam_static(df_parfam_wave6,list_index_and_highbtwgrvar_weighted_sd)
# plot_errbar_parfam_static(df_parfam_wave6,list_index_and_highbtwgrvar_weighted_sd_demo)
ggsave("weight_static_index_and_highbtwgrvar_weighted_sd.pdf",
       plot = p_weight_static_index_and_highbtwgrvar_weighted_sd,
       path = outdir, width=figureWidth, height=18, unit="cm")
# ggsave("weight_static_index_and_highbtwgrvar_weighted_sd.pdf", path = outdir, width=figureWidth, height=22, unit="cm")

p_weight_static_index_standardized <- plot_errbar_parfam_static(df_parfam_wave6,list_index)




##Chainging the labels 
df_parfam_wave6_changed_labels <- df_parfam_wave6 %>% mutate(
  variable = ifelse(variable == "Economic Index","Economic Index Weights",
                    ifelse(variable == "Cultural Index","Cultural Index Weights", variable )))

new_index_labels <- c("Economic Index Weights","Cultural Index Weights")


p_weight_static_index_standardized_new_labels <- plot_errbar_parfam_static(df_parfam_wave6_changed_labels,new_index_labels)


################################################################################
# Change from wave 4 to wave 6
################################################################################
plotTheme = theme(strip.background = element_blank(),
                  legend.position = "bottom",
                  strip.text.x = element_text(face = "bold"))


plot_labs <- raw_data_wvs %>%
  filter(BB_Wave_num %in% c(4, 6)) %>%
  arrange(BB_Wave_num) %>%
  select(waveLabels) %>%
  distinct() %>%
  pull()

read_and_prepare_weight_data <- function(filesuffix = "") {
  weights_file <- ifelse(filesuffix == "",
                         paste0(model_results_path, "/weights.Rds"),
                         paste0(paste0(model_results_path, "/weights"), filesuffix, ".Rds"))
  if (!file.exists(weights_file)) {
    stop(paste("Missing weights file:", weights_file))
  }

  raw_data_loop = readRDS(file = weights_file)

  raw_data_loop = raw_data_loop %>%
    mutate(econ_index = rowSums(across(any_of(positive_econ_vars))) - rowSums(across(any_of(negative_econ_vars))),
           soc_index = rowSums(across(any_of(positive_soc_vars))) - rowSums(across(any_of(negative_soc_vars))),
           trad_index = per603 - per604,
           soc_no_trad_index = rowSums(across(any_of(positive_soc_vars))) - rowSums(across(any_of(negative_soc_vars))) - trad_index,
           hawk_dove_index = rowSums(across(any_of(positive_hawk_dove_vars))) - rowSums(across(any_of(negative_hawk_dove_vars))),
           hawk_dove_expanded_index = rowSums(across(any_of(positive_hawk_dove_expanded_vars))) - 
             rowSums(across(any_of(negative_hawk_dove_expanded_vars))),
           neg_index = rowSums(across(any_of(negativity_vars))),
           eu_index = rowSums(across(any_of(positive_eu_vars))) - rowSums(across(any_of(negative_eu_vars))),
           gov_index = rowSums(across(any_of(gov_vars)))) %>% 
    na.omit()

  raw_data_loop = raw_data_loop %>%
    select(c("BB_Country", "Y_manifesto_party_id", "Y_FR_Pop", "BB_Id",
             "wave", "econ_index", "soc_index", "trad_index", "soc_no_trad_index", 
             "hawk_dove_index", "hawk_dove_expanded_index",
             "neg_index", "eu_index", "gov_index", starts_with("per")))

  raw_data_loop = raw_data_loop %>%
    left_join(wvs_weights, by = "BB_Id")

  raw_data_loop = pivot_longer(raw_data_loop,
                               cols=!c("BB_Country", "Y_manifesto_party_id", "Y_FR_Pop", "BB_Id", "wave", "BB_Weight_CW"),
                               names_to="variable_original",
                               values_to = "value")

  df_parfam_loop = left_join(raw_data_loop,cw_party_parfam)

  df_parfam_loop = df_parfam_loop %>%
    select(BB_Country,BB_Id,Y_manifesto_party_id,Y_FR_Pop,parfam,wave,variable_original,value,BB_Weight_CW)

  df_parfam_loop = df_parfam_loop %>% 
    add_parfam() %>% 
    filter(!is.na(BBF_parfam))

  df_parfam_loop = left_join(df_parfam_loop, df_manifesto)
  df_parfam_loop = left_join(df_parfam_loop, cw_party_parfam)

  df_parfam_loop = df_parfam_loop %>%
    mutate(value = value * sd) %>%
    mutate(value = if_else(variable_original == "econ_index", value / num_econ_index_vars, value),
           value = if_else(variable_original == "soc_index", value / num_soc_index_vars, value),
           value = if_else(variable_original == "trad_index", value / num_trad_index_vars, value),
           value = if_else(variable_original == "soc_no_trad_index", value / num_soc_no_trad_index_vars, value),
           value = if_else(variable_original == "hawk_dove_index", value / num_hawk_dove_index_vars, value),
           value = if_else(variable_original == "hawk_dove_expanded_index",
                           value / num_hawk_dove_expanded_index_vars, value),
           value = if_else(variable_original == "neg_index", value / num_neg_index_vars, value),
           value = if_else(variable_original == "eu_index", value / num_eu_index_vars, value),
           value = if_else(variable_original == "gov_index", value / num_gov_index_vars, value))

  df_parfam_loop = df_parfam_loop %>%
    mutate(variable = if_else(variable_original == "econ_index", econIndexName, NA_character_),
           variable = if_else(variable_original == "soc_index", societyIndexName, variable),
           variable = if_else(variable_original == "trad_index", tradIndexName, variable),
           variable = if_else(variable_original == "soc_no_trad_index", societynotradIndexName, variable),
           variable = if_else(variable_original == "hawk_dove_index", hawkdoveIndexName, variable),
           variable = if_else(variable_original == "hawk_dove_expanded_index", hawkdoveexpandedIndexName, variable),
           variable = if_else(variable_original == "neg_index", negIndexName, variable),
           variable = if_else(variable_original == "eu_index", euIndexName, variable),
           variable = if_else(variable_original == "gov_index", govIndexName, variable))

  df_parfam_loop = left_join(df_parfam_loop,
                             cw_manfiesto_labels_forWeight)

  df_parfam_loop = df_parfam_loop %>%
    mutate(variable = if_else(is.na(variable),
                              title,
                              variable))

  df_parfam_loop
}

for (filesuffix in "") {
  df_parfam_for_plot <- if (filesuffix == "") df_parfam else read_and_prepare_weight_data(filesuffix)

  plotEconSoc_46 = plot_distribution_wave_allpar_dashedline(raw_data_manifesto, 
                                                            labels = plot_labs,
                                                            df_parfam = df_parfam_for_plot %>%
                                                              filter(wave %in% c(4, 6)) %>%
                                                              mutate(variable =ifelse(variable == "Economic Index","Economic Index Weights",
                                                                                      ifelse(variable == "Cultural Index","Cultural Index Weights", variable )) ),
                                                            list_var = c("Economic Index Weights","Cultural Index Weights"),
                                                            differentLines = TRUE)


  p = plotEconSoc_46 + 
    scale_y_continuous(limits = c(0,1.1), breaks = seq(0,1,0.25)) +
    scale_x_continuous(breaks = seq(-2,2,1),
                       labels = c(-2, -1, "0\nIndifferent", 1, 2),
                       limits = c(-2, 2))+
    plotTheme +
    theme(legend.title = element_blank(), legend.key.width = unit(40, "points")) 


  print(p)
  ggsave(paste0("weight_distr_index_allpar_standardized_econ_soc_46", filesuffix, ".pdf"), 
         plot = p,
         path = outdir, width=figureWidth, height=12, unit="cm")


  p_econ_slide <- p + scale_alpha_manual(values = c(1, 0.3), guide="none")
}

plotEconSocHAWKDOVE_46 = plot_distribution_wave_allpar_dashedline_threeindex(raw_data_manifesto, 
                                                                             labels = plot_labs,
                                                                             df_parfam = df_parfam %>%
                                                                               filter(wave %in% c(4, 6)) %>%
                                                                               mutate(variable = ifelse(variable == "Economic Index", "Economic Index Weights",
                                                                                                        ifelse(variable == "Cultural Index", "Cultural Index Weights",
                                                                                                               ifelse(variable == "Hawk-Dove Index", "Hawk-Dove Index Weights", variable)))),
                                                                             list_var = c("Economic Index Weights","Cultural Index Weights", "Hawk-Dove Index Weights"),
                                                                             differentLines = TRUE)


p_3 = plotEconSocHAWKDOVE_46 + 
  scale_y_continuous(breaks = seq(0,4,1)) +
  scale_x_continuous(breaks = seq(-2,2,1),
                     labels = c(-2, -1, "0\nIndifferent", 1, 2),
                     limits = c(-2, 2))+
  plotTheme +
  theme(legend.title = element_blank(), legend.key.width = unit(40, "points")) 

print(p_3)

plotEconSocHAWKDOVE_ex_46 = plot_distribution_wave_allpar_dashedline_threeindex(raw_data_manifesto, 
                                                                                labels = plot_labs,
                                                                                df_parfam = df_parfam %>%
                                                                                  filter(wave %in% c(4, 6)) %>%
                                                                                  mutate(variable = ifelse(variable == "Economic Index", "Economic Index Weights",
                                                                                                           ifelse(variable == "Cultural Index", "Cultural Index Weights",
                                                                                                                  ifelse(variable == "Expanded Hawk-Dove Index", "Expanded Hawk-Dove Index Weights", variable)))),
                                                                                list_var = c("Economic Index Weights","Cultural Index Weights", "Expanded Hawk-Dove Index Weights"),
                                                                                differentLines = TRUE)

p_3_ex = plotEconSocHAWKDOVE_ex_46 + 
  scale_y_continuous(breaks = seq(0,4,1)) +
  scale_x_continuous(breaks = seq(-2,2,1),
                     labels = c(-2, -1, "0\nIndifferent", 1, 2),
                     limits = c(-2, 2))+
  plotTheme +
  theme(legend.title = element_blank(), legend.key.width = unit(40, "points")) 

print(p_3_ex)
plotEconSocNeg_46 = plot_distribution_wave_allpar_dashedline_threeindex(raw_data_manifesto, 
                                                                        labels = plot_labs,
                                                                        df_parfam = df_parfam %>%
                                                                          filter(wave %in% c(4, 6)) %>%
                                                                          mutate(variable = ifelse(variable == "Economic Index", "Economic Index Weights",
                                                                                                   ifelse(variable == "Cultural Index", "Cultural Index Weights",
                                                                                                          ifelse(variable == "Negativity Index", "Negativity Index Weights", variable)))),
                                                                        list_var = c("Economic Index Weights","Cultural Index Weights", "Negativity Index Weights"),
                                                                        differentLines = TRUE)

p_3_neg = plotEconSocNeg_46 + 
  scale_y_continuous(breaks = seq(0,4,1)) +
  scale_x_continuous(breaks = seq(-2,2,1),
                     labels = c(-2, -1, "0\nIndifferent", 1, 2),
                     limits = c(-2, 2))+
  plotTheme +
  theme(legend.title = element_blank(), legend.key.width = unit(40, "points")) 

print(p_3_neg)
plotEconSocEU_46 = plot_distribution_wave_allpar_dashedline_threeindex(raw_data_manifesto, 
                                                                       labels = plot_labs,
                                                                       df_parfam = df_parfam %>%
                                                                         filter(wave %in% c(4, 6)) %>%
                                                                         mutate(variable = ifelse(variable == "Economic Index", "Economic Index Weights",
                                                                                                  ifelse(variable == "Cultural Index", "Cultural Index Weights",
                                                                                                         ifelse(variable == "European Integration Index", "European Integration Index Weights", variable)))),
                                                                       list_var = c("Economic Index Weights","Cultural Index Weights", "European Integration Index Weights"),
                                                                       differentLines = TRUE)

p_3_eu = plotEconSocEU_46 + 
  scale_y_continuous(breaks = seq(0,2,1)) +
  scale_x_continuous(breaks = seq(-2,2,1),
                     labels = c(-2, -1, "0\nIndifferent", 1, 2),
                     limits = c(-2, 2))+
  plotTheme +
  theme(legend.title = element_blank(), legend.key.width = unit(40, "points")) 

print(p_3_eu)
plotEconSocGov_46 = plot_distribution_wave_allpar_dashedline_threeindex(raw_data_manifesto, 
                                                                        labels = plot_labs,
                                                                        df_parfam = df_parfam %>%
                                                                          filter(wave %in% c(4, 6)) %>%
                                                                          mutate(variable = ifelse(variable == "Economic Index", "Economic Index Weights",
                                                                                                   ifelse(variable == "Cultural Index", "Cultural Index Weights",
                                                                                                          ifelse(variable == "Government Efficiency Index", "Government Efficiency Index Weights", variable)))),
                                                                        list_var = c("Economic Index Weights","Cultural Index Weights", "Government Efficiency Index Weights"),
                                                                        differentLines = TRUE)

p_3_gov = plotEconSocGov_46 + 
  scale_y_continuous(breaks = seq(0,2,1)) +
  scale_x_continuous(breaks = seq(-2,2,1),
                     labels = c(-2, -1, "0\nIndifferent", 1, 2),
                     limits = c(-2, 2))+
  plotTheme +
  theme(legend.title = element_blank(), legend.key.width = unit(40, "points")) 

print(p_3_gov)
plotExindex_46 = plot_distribution_wave_allpar_dashedline_threeindex(raw_data_manifesto, 
                                                                     labels = plot_labs,
                                                                     df_parfam = df_parfam %>%
                                                                       filter(wave %in% c(4, 6)) %>%
                                                                       mutate(variable = ifelse(variable == "Expanded Hawk-Dove Index", "Expanded Hawk-Dove Index Weights",
                                                                                                ifelse(variable == "European Integration Index", "European Integration Index Weights",
                                                                                                       ifelse(variable == "Government Efficiency Index", "Government Efficiency Index Weights", variable)))),
                                                                     list_var = c("Expanded Hawk-Dove Index Weights","European Integration Index Weights", "Government Efficiency Index Weights"),
                                                                     differentLines = TRUE)

p_3_ex_index = plotExindex_46 + 
  scale_y_continuous(breaks = seq(0,4,1)) +
  scale_x_continuous(breaks = seq(-2,2,1),
                     labels = c(-2, -1, "0\nIndifferent", 1, 2),
                     limits = c(-2, 2))+
  plotTheme +
  theme(legend.title = element_blank(), legend.key.width = unit(40, "points")) 

print(p_3_ex_index)
ggsave("weight_distr_index_allpar_standardized_ex_index_46.pdf", 
       plot = p_3_ex_index,
       path = outdir, width=figureWidth, height=12, unit="cm")


# Analysis by East and West
df_parfam_east <- df_parfam %>% 
  filter(BB_Country %in% country_east)

plotEconSoc_46_east = plot_distribution_wave_allpar_dashedline(raw_data_manifesto, 
                                                          labels = plot_labs,
                                                          df_parfam = df_parfam_east %>%
                                                            filter(wave %in% c(4, 6)) %>%
                                                            mutate(variable =ifelse(variable == "Economic Index","Economic Index Weights",
                                                                                    ifelse(variable == "Cultural Index","Cultural Index Weights", variable )) ),
                                                          list_var = c("Economic Index Weights","Cultural Index Weights"),
                                                          differentLines = TRUE)


p_east = plotEconSoc_46_east + 
  scale_y_continuous(limits = c(0,1.1), breaks = seq(0,1,0.25)) +
  scale_x_continuous(breaks = seq(-2,2,1),
                     labels = c(-2, -1, "0\nIndifferent", 1, 2),
                     limits = c(-2, 2))+
  plotTheme +
  theme(legend.title = element_blank(), legend.key.width = unit(40, "points")) 


print(p_east)

df_parfam_west <- df_parfam %>% 
  filter(BB_Country %in% country_west)

plotEconSoc_46_west = plot_distribution_wave_allpar_dashedline(raw_data_manifesto, 
                                                               labels = plot_labs,
                                                               df_parfam = df_parfam_west %>%
                                                                 filter(wave %in% c(4, 6)) %>%
                                                                 mutate(variable =ifelse(variable == "Economic Index","Economic Index Weights",
                                                                                         ifelse(variable == "Cultural Index","Cultural Index Weights", variable )) ),
                                                               list_var = c("Economic Index Weights","Cultural Index Weights"),
                                                               differentLines = TRUE)


p_west = plotEconSoc_46_west + 
  scale_y_continuous(limits = c(0,1.1), breaks = seq(0,1,0.25)) +
  scale_x_continuous(breaks = seq(-2,2,1),
                     labels = c(-2, -1, "0\nIndifferent", 1, 2),
                     limits = c(-2, 2))+
  plotTheme +
  theme(legend.title = element_blank(), legend.key.width = unit(40, "points")) 


print(p_west)

plotTrad_46 = plot_distribution_wave_allpar_dashedline(raw_data_manifesto, 
                                                          labels = plot_labs,
                                                          df_parfam = df_parfam %>%
                                                            filter(wave %in% c(4, 6)) %>%
                                                            mutate(variable =ifelse(variable == "Cultural Index without Traditional Morality", "Cultural Index without Traditional Morality Weights",
                                                                                    ifelse(variable == "Traditional Morality Index","Traditional Morality Index Weights", variable )) ),
                                                          list_var = c("Cultural Index without Traditional Morality Weights","Traditional Morality Index Weights"),
                                                          differentLines = TRUE)


p_trad = plotTrad_46 + 
  scale_y_continuous(limits = c(0,1.1), breaks = seq(0,1,0.25)) +
  scale_x_continuous(breaks = seq(-2,2,1),
                     labels = c(-2, -1, "0\nIndifferent", 1, 2),
                     limits = c(-2, 2))+
  plotTheme +
  theme(legend.title = element_blank(), legend.key.width = unit(40, "points")) 


print(p_trad)


# Wave 4 weights distributions for slides ---------------------------------

plot_labs <- raw_data_wvs %>%
  filter(BB_Wave_num == 4) %>%
  arrange(BB_Wave_num) %>%
  select(waveLabels) %>%
  distinct() %>%
  pull()


plotEconSoc_4 <- plot_distribution_wave_allpar_dashedline(
  raw_data_manifesto, 
  labels = plot_labs, 
  df_parfam = df_parfam %>% filter(wave == 4) %>%
    mutate(variable = ifelse(variable == "Economic Index",
                             "Economic Index Weights",
                             ifelse(variable == "Cultural Index",
                                    "Cultural Index Weights", variable )) ),
  list_var = c("Economic Index Weights","Cultural Index Weights"), 
  differentLines = FALSE)


p_econ_soc_4_slide <- plotEconSoc_4 + 
  scale_y_continuous(limits = c(0,1.1), breaks = seq(0,1,0.25)) + 
  scale_x_continuous(breaks = seq(-2,2,1), 
                     labels = c(-2, -1, "0\nIndifferent", 1, 2), 
                     limits = c(-2, 2)) +
  plotTheme + 
  theme(legend.title = element_blank(), legend.key.width = unit(40, "points")) +
  guides(alpha = "none", linetype="none")


#------------------------------------------------------------------------------
# Plot relative importance of econ to culture index
#------------------------------------------------------------------------------
df_econ=df_parfam%>%
  filter(wave==4|wave==6)%>%
  filter(variable_original=="econ_index")%>%
  select(BB_Id,BB_Country,wave,value)%>%
  rename(value_econ=value)

df_soc=df_parfam%>%
  filter(wave==4|wave==6)%>%
  filter(variable_original=="soc_index")%>%
  select(BB_Id,BB_Country,wave,value)%>%
  rename(value_soc=value)

df_econ_soc=left_join(df_econ,df_soc) 

df_econ_soc=df_econ_soc%>%
  mutate(abs_ratio_econ_soc=(abs(value_soc)/abs(value_econ)))

plot_labs <- raw_data_wvs %>%
  filter(BB_Wave_num == 4 | BB_Wave_num==6) %>%
  arrange(BB_Wave_num) %>%
  select(waveLabels) %>%
  distinct() %>%
  pull()

# Create a df for plotting the aggregate of all countries and each one of them individually 
df_econ_soc2 <- df_econ_soc %>% 
  mutate(BB_Country = "Aggregate", BB_Id = NA)

df_econ_soc2 <- bind_rows(df_econ_soc2 , df_econ_soc)

# Create df for analysis by East and West
df_econ_soc_east <- df_econ_soc %>% 
  filter(BB_Country %in% country_east) %>% 
  mutate(BB_Country = "East", BB_Id = NA)

df_econ_soc_west <- df_econ_soc %>% 
  filter(BB_Country %in% country_west) %>% 
  mutate(BB_Country = "West", BB_Id = NA)

df_econ_soc_ew <- bind_rows(df_econ_soc_east, df_econ_soc_west)

wave_country_year <- raw_data_wvs %>% 
  distinct(BB_Country, BB_Year, BB_Wave_num) %>% 
  filter(BB_Wave_num==4|BB_Wave_num==6) %>% 
  group_by(BB_Country, BB_Wave_num) %>% 
  summarise(max_year = max(BB_Year), min_year = min(BB_Year)) %>% 
  ungroup() %>% 
  mutate(years = ifelse(max_year != min_year, paste0(min_year, "-", max_year), as.character(max_year))) %>% 
  group_by(BB_Country) %>% 
  summarise(years = str_c(years, collapse=", "), .groups = 'drop') %>% 
  mutate(country_years = paste0(BB_Country, ": ", years)) %>% 
  select(BB_Country, country_years)


df_econ_soc2 <- left_join(df_econ_soc2, wave_country_year) %>% 
  mutate(country_years = ifelse(BB_Country == "Aggregate", "Aggregate", country_years)) 

# Ratio of absolute values
## By country
p = ggplot(df_econ_soc2, aes(abs_ratio_econ_soc, color = as.factor(wave), linetype = as.factor(wave))) +
  stat_density(geom = "line", position = "identity", size = 0.5) +
  geom_vline(xintercept = 1) +
  scale_color_manual(values = ParFamColor[c(3,4,6) %in% unique(df_parfam$wave)], labels = plot_labs) +
  scale_alpha_manual(values = c(1, 1), guide = 'none') +
  scale_linetype_manual(values = c("solid", "dashed"), labels = plot_labs) +
  labs(x="Weights on culture in absolute value / weights on economics in absolute value") +
  theme_classic(base_size = 9.25)+
  facet_wrap(~BB_Country, nrow=6)+
  scale_x_continuous(trans="log", 
                     breaks = c(0.1, 1, 10),
                     limits = c(0.01, 100)) +
  annotate("rect", xmin=1, xmax=100, ymin=-Inf, ymax=Inf ,alpha=0.4, fill="gray")+
  theme(legend.title = element_blank(), legend.position = "bottom",axis.title.y = element_blank(),legend.key.width = unit(32, "points"), 
        legend.margin = ggplot2::margin(-5,0,-5,0), plot.margin = ggplot2::margin(5,5,0,5))

p_relative_weights_culture_econ <- p +
  annotate("text", x = 20, y = 0.44, label = "Prioritize\nCulture", size = 2.5, ) + 
  annotate("text", x = 0.05, y = 0.44, label = "Prioritize\nEconomics", size = 2.5) 





# By East and West
p = ggplot(df_econ_soc_ew, aes(abs_ratio_econ_soc, color = as.factor(wave), linetype = as.factor(wave))) +
  stat_density(geom = "line", position = "identity", size = 0.5) +
  geom_vline(xintercept = 1) +
  scale_color_manual(values = ParFamColor[c(3,4,6) %in% unique(df_parfam$wave)], labels = plot_labs) +
  scale_alpha_manual(values = c(1, 1), guide = 'none') +
  scale_linetype_manual(values = c("solid", "dashed"), labels = plot_labs) +
  labs(x="Weights on culture in absolute value / weights on economics in absolute value") +
  theme_classic(base_size = 9.25)+
  facet_wrap(~BB_Country, nrow=1)+
  scale_x_continuous(trans="log", 
                     breaks = c(0.1, 1, 10),
                     limits = c(0.01, 100)) +
  annotate("rect", xmin=1, xmax=100, ymin=-Inf, ymax=Inf ,alpha=0.4, fill="gray")+
  theme(legend.title = element_blank(), legend.position = "bottom",axis.title.y = element_blank(),legend.key.width = unit(32, "points"), 
        legend.margin = ggplot2::margin(-5,0,-5,0), plot.margin = ggplot2::margin(5,5,0,5))

p_relative_weights_culture_econ_ew <- p +
  annotate("text", x = 20, y = 0.44, label = "Prioritize\nCulture", size = 2.5, ) + 
  annotate("text", x = 0.05, y = 0.44, label = "Prioritize\nEconomics", size = 2.5)
