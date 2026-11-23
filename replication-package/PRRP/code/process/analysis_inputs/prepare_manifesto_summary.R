library(tidyverse)
library(haven)
library(data.table)
library(Hmisc)
source("code/util/commonVars.R")
WAVE_4_YEARS <- 2005:2009
WAVE_6_YEARS <- 2017:2020
raw_data_manifesto <- readRDS(paste0(cleandata_path, "/manifesto/manifesto_populist.rds"))

# List of countries appearing in the IVS/WVS data
countries_list <- read_dta(pathWVS_Data) %>%
  pull(BB_Country) %>%
  unique()

# *************************************************************************
# DATA CLEANING AND PREPARATION ----
# *************************************************************************

# Select relevant variables, restrict to IVS/WVS countries,
# construct manifesto waves, and compute within-country wave weights
df <- raw_data_manifesto %>%
  filter(manifesto_country_name %in% countries_list) %>%
  select(
    manifestoYear,
    manifesto_country_name,
    Y_FR_Pop,
    all_of(starts_with("per")),
    economy_index,
    society_index
  ) %>%
  select(-peruncod) %>%
  mutate(
    # Assign manifesto waves based on year
    wave = case_when(
      manifestoYear %in% WAVE_4_YEARS ~ 4,
      manifestoYear %in% WAVE_6_YEARS ~ 6,
      TRUE ~ NA_real_
    )
  ) %>%
  # Keep only observations belonging to waves of interest
  # and with valid vote shares (required for weighting)
  filter(!is.na(wave), !is.na(pervote)) %>%
  # Construct weights so that each country × wave sums to 1,
  # ensuring equal weight across countries regardless of
  # number of parties or elections
  group_by(manifesto_country_name, wave) %>%
  mutate(weight = pervote / sum(pervote)) %>%
  ungroup() %>%
  select(-c(pervote, manifestoYear))

# *************************************************************************
# VARIABLE STANDARDIZATION (SDs) ----
# *************************************************************************

# Compute weighted standard deviations for all CMP position variables.
# These SDs are later used to standardize differences between PRRP and non-PRRP.
df_std <- df %>%
  summarise(
    across(
      starts_with("per"),
      ~ sqrt(Hmisc::wtd.var(.x, weights = weight, na.rm = TRUE))
    )
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable_name",
    values_to = "std"
  )

# *************************************************************************
# COMPUTE GROUP MEANS ----
# *************************************************************************

# Step 1: Compute weighted means within each country × wave
# Step 2: Average across countries within each wave
# This ensures that each country contributes equally to wave-level means
df <- df %>%
  pivot_longer(
    cols = c(starts_with("per"), ends_with("_index")),
    names_to = "variable"
  ) %>%
  group_by(Y_FR_Pop, wave, manifesto_country_name, variable) %>%
  summarise(
    value = weighted.mean(value, weight, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(Y_FR_Pop, wave, variable) %>%
  summarise(
    value = mean(value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(variable_name = variable)

# *************************************************************************
# IDENTIFY MOST DISTINCTIVE CMP VARIABLES ----
# *************************************************************************

# Exclude indices, keep variables with at least 1% support in either group,
# standardize differences, and select the 10 most distinctive positions
df_pick <- df %>%
  filter(!grepl("index", variable_name)) %>%
  group_by(Y_FR_Pop, variable_name) %>%
  summarise(value = mean(value), .groups = "drop") %>%
  pivot_wider(names_from = Y_FR_Pop, names_prefix = "PRRP_") %>%
  filter(PRRP_0 >= 1 | PRRP_1 >= 1) %>%
  left_join(df_std, by = "variable_name") %>%
  mutate(
    abs_diff = abs(PRRP_1 - PRRP_0),
    std_abs_diff = abs_diff / std
  ) %>%
  slice_max(std_abs_diff, n = 10) %>%
  select(variable_name)

saveRDS(pull(df_pick, variable_name), file.path(cleandata_path, "manifesto", "top_distinct_vars.rds"))
saveRDS(list(df=df, df_std=df_std, df_pick=df_pick), file.path(cleandata_path, "manifesto", "descriptive_summary.rds"))
