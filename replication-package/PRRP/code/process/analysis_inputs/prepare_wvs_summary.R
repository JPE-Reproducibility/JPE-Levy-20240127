library(tidyverse)
library(haven)
library(data.table)
library(Hmisc)
source("code/util/commonVars.R")
set.seed(2022)
wvs_raw <- read_dta(pathWVS_Data)

# *************************************************************************
# Data cleaning and prep ----
# *************************************************************************
#
# Select variables, limit to waves 4 and 6
outcome_prep_df <- wvs_raw %>%
  mutate(Y_FR_Pop = as.factor(Y_FR_Pop)) %>%
  select(Y_FR_Pop, BB_Country, BB_Wave_num, BB_Weight_CW, starts_with("X_op")) %>%
  filter(BB_Wave_num == 4 | BB_Wave_num == 6)


# *************************************************************************
# Get Top 10 op using diff in std. dev. ----
# *************************************************************************

# calculate SDs for each variables so we can compare differences
outcome_prep_df_std <- outcome_prep_df %>%
  summarise(across(
    starts_with("X_op"),
    ~ sqrt(Hmisc::wtd.var(.x, weights = BB_Weight_CW, na.rm = TRUE)))) %>%
  pivot_longer(cols = everything(), names_to = "variable_name", values_to = "std")

# Calculate averages for each variable in each group (PRRP/non-PRRP) and wave
# first weighted average in each country*wave then average in each wave.
# Results in a for rows for each variable (PRRP*wave) where each wave is
# weighted equally for both groups together (same number of countries in each wave
# and each country*wave with a total weight of 1)
outcome_prep_df <- outcome_prep_df %>%
  pivot_longer(
    cols = c(starts_with("X_op")),
    names_to = "variable"
  ) %>%
  group_by(Y_FR_Pop, BB_Wave_num, BB_Country, variable) %>%
  summarise(value = weighted.mean(value, BB_Weight_CW, na.rm = T)) %>%
  ungroup() %>%
  group_by(Y_FR_Pop, BB_Wave_num, variable) %>%
  summarise(value = mean(value, na.rm = T)) %>%
  ungroup() %>%
  rename(variable_name = variable)

# *************************************************************************
# Pick most distinct variables ----
# *************************************************************************

# standardize the variables using df_std, calculate avg among PRRP and others
# (each wave weighted equally), pick 5 variables with largest absolute difference
top_5_outcome <- outcome_prep_df %>%
  group_by(Y_FR_Pop, variable_name) %>%
  summarise(value = mean(value)) %>%
  pivot_wider(names_from = Y_FR_Pop, names_prefix = "PRRP_") %>%
  left_join(outcome_prep_df_std) %>%
  mutate(
    abs_diff = abs(PRRP_1 - PRRP_0),
    std_abs_diff = abs_diff / std) %>%
  slice_max(std_abs_diff, n = 5) %>%
  pull(variable_name)


top_5_names <- names_for_graph %>%
  filter(name %in% top_5_outcome) %>%
  pull(plotName)


# *************************************************************************
# create demographic tables ----
# *************************************************************************

dem_df <- wvs_raw %>%
  filter(BB_Wave_num %in% c(4, 6)) %>%
  select(
    Y_Outcome = Y_FR_Pop, BB_Wave_num, BB_Weight_CW, X_dem_age, X_dem_male,
    X_dem_education, X_dem_log_town_size, X_dem_employed, X_general_self_political_scale
  ) %>%
  # create dummy for college education
  mutate(
    X_college = if_else(X_dem_education >= 14, 1, 0),
    X_urban = X_dem_log_town_size > log(100000),
    X_emp = if_else(X_dem_employed == 2, 1, 0),
  ) %>%
  select(-X_dem_employed, -X_dem_education, -X_general_self_political_scale, -X_dem_log_town_size,) %>%
  pivot_longer(starts_with("X_"), names_to = "variable", values_to = "value") %>%
  mutate(Y_Outcome = if_else(Y_Outcome == 1, "RRP", "Other")) %>%
  group_by(BB_Wave_num, Y_Outcome, variable) %>%
  # calculate means for PRRP/Other in each wave - weighted by BB_Weight_CW
  summarise(value = weighted.mean(value, BB_Weight_CW, na.rm = T)) %>%
  pivot_wider(names_from = c(Y_Outcome, BB_Wave_num), values_from = value) %>%
  select(variable, RRP_4, Other_4, RRP_6, Other_6) %>%
  mutate(variable = case_when(
    variable == "X_college" ~ "College education",
    variable == "X_dem_age" ~ "Age",
    variable == "X_dem_log_town_size" ~ "Town size (log)",
    variable == "X_dem_male" ~ "Male",
    variable == "X_urban" ~ "Urban",
    variable == "X_emp" ~ "Full-time employment"
  )) %>%
  arrange(nchar(variable)) %>% 
  column_to_rownames("variable")


# Top 5 PRRP (only weighted) ----------------------------------------------

op_df_rrp_5 <- wvs_raw %>%
  filter(BB_Wave_num %in% c(4, 6)) %>%
  select(Y_Outcome = Y_FR_Pop, BB_Wave_num, BB_Weight_CW, all_of(top_5_outcome)) %>% 
  mutate(across(all_of(top_5_outcome), ~ (.x - weighted.mean(.x, w = BB_Weight_CW, na.rm = T)) /
                  sqrt(Hmisc::wtd.var(.x, w = BB_Weight_CW)))) %>%  
  pivot_longer(starts_with("X_"), names_to = "variable", values_to = "value") %>%
  mutate(Y_Outcome = if_else(Y_Outcome == 1, "RRP", "Other")) %>%
  group_by(BB_Wave_num, Y_Outcome, variable) %>%
  summarise(value = mean(value, na.rm = T)) %>%
  pivot_wider(names_from = c(Y_Outcome, BB_Wave_num), values_from = value) %>%
  left_join(names_for_graph, by = c("variable" = "name")) %>%
  mutate(variable = factor(variable, levels = top_5_outcome)) %>%
  arrange(variable) %>%
  select(plotName, RRP_4, Other_4, RRP_6, Other_6) %>%
  mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
  column_to_rownames("plotName")

# *************************************************************************
# Outputs ----
# *************************************************************************

# Top 5 table ------------------------------------------------------------

op_df_rrp_5 <- op_df_rrp_5 %>% filter(rownames(op_df_rrp_5) %in% top_5_names)

wvs_tab <- rbind(dem_df, op_df_rrp_5)

saveRDS(wvs_tab, file.path(cleandata_path, "ivs", "wvs_top5_vars.rds"))
