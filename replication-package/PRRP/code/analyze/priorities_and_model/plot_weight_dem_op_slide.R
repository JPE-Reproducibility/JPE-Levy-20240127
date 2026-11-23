# *************************************************************************
# File description ----
# *************************************************************************
#
# This file analyzes how model weights (for economic and cultural indices)
# vary across voter demographic characteristics. It combines WVS respondent
# data with estimated model weights to examine heterogeneous preference 
# structures across demographic groups.
#
# Main goals:
#   (1) Characterize weight distributions for economic and cultural indices
#       by demographic groups (age, education, income, union membership, etc.)
#   (2) Test for statistically significant differences in weights across
#       demographic categories
#   (3) Identify demographic drivers of heterogeneity in index weights
#   (4) Compare potential PRRP supporters to other voters
#
# Analysis includes:
#   - Demographic categorization (sex, age, employment, union, education, 
#     location, income, PRRP support, political orientation)
#   - Distribution plots of weights across demographic groups
#   - Error bars showing median and interquartile ranges
#   - Regression estimates with heteroskedasticity-robust standard errors
#   - Change in log-ratio of indices (social vs economic salience)
#
# Outputs:
#
# Figures (slides):
#   - Weight distributions by demographic group (multiple PDFs)
#   - Error bar plots with 25th-75th percentile ranges
#
# Tables:
#   - Regression results for weight differences across demographics
#   - Individual demographic effects and joint specification
#
# *************************************************************************

# Plot weights by demographics
## Static
library(ggplot2)
library(dplyr)
library(tidyr)
library(haven)
library(tidyverse)
library(grid)
library(gtable)
library(conflicted)
library(data.table)
library(kableExtra)
library(sandwich)
library(lmtest)
library(broom)


rm(list = ls())

conflict_prefer("filter", "dplyr")
conflict_prefer("margin", "ggplot2")
conflict_prefer("wtd.quantile", "Hmisc")
conflict_prefer("coalesce", "dplyr")
conflict_prefer("tibble", "tibble")

source("code/util/commonVars.R")


data <- readRDS(file.path(cleandata_path, "ivs", "prrp_profiles.rds"))$data

df_wvs_pop_score=data%>%
  select("BB_Id","outcome_score","outcome_score_std")

source("code/analyze/priorities_and_model/func_plot_weight.R")

# output directory:
outdir <- "output/figures/model_weight/dist_by_group/"
ParFamColor <- c("#999999", "#5673e9", "#E69F00", "#009E73", "#F0E442",
               "#0072B2", "#D55E00", "#CC79A7")

# List index
list_index <- c(econIndexName, societyIndexName)

# Load raw data
raw_data_wvs <- read_dta(pathWVS_Data)
raw_data_manifesto <- readRDS(file = paste0(cleandata_path, "/manifesto/manifesto_populist.rds"))

# Prepare weight data
df_weight <- prepare_weight_data(raw_data_manifesto, raw_data_wvs)

# Prepare wvs data
df_wvs <- prepapre_wvs_data(raw_data_wvs)

# Merge Weights and Demographics
df <- left_join(df_weight, df_wvs, by = c("id"))
# Merge them with populist score
df <- left_join(df, df_wvs_pop_score, by = c("BB_Id"))

# Add respondent weights before creating weighted median PRRP score groups.
df <- df %>%
  left_join(
    raw_data_wvs %>%
      select(BB_Id, BB_Weight_CW) %>%
      distinct(),
    by = "BB_Id"
  )

df <- df %>% mutate(unmatch_country = if_else(BB_Country.x != BB_Country.y, 1, 0))

# Check if id-country match is right
stopifnot(max(df$unmatch_country, na.rm = T) == 0)

df <- df %>%
  rename(BB_Country = BB_Country.x) %>%
  select(-BB_Country.y) %>%
  rename(wave = wave.x) %>%
  select(-wave.y)
df <- na.omit(df)

# *************************************************************************
# Create group variables ----
# *************************************************************************

# Create demographic groups
selected_columns <- c("X_dem_rel_services", "X_op_believe_hell",
                      "X_op_god_import", "X_op_religion_import",
                      "X_dem_religious_org_member", "X_op_confidence_church",
                      "X_op_child_faith_import", "X_dem_rel_no_rel", "X_op_god")

data_pca <- df[, selected_columns] %>%
  mutate(X_dem_rel_no_rel = 1- X_dem_rel_no_rel)
pca_result <- prcomp(data_pca, center = TRUE, scale. = FALSE)
PC1 <- pca_result$x[, 1]


df <- add_F_vars(df) %>% mutate(PC1 = -PC1)

# Create left-right groups by global percentiles
df <- df %>%
  group_by(wave) %>%
  mutate(
    pc50_selfpol = as.numeric(wtd.quantile(X_general_self_political_scale, probs = 0.5)),
    pc33_selfpol = as.numeric(wtd.quantile(X_general_self_political_scale, probs = 0.33)),
    pc66_selfpol = as.numeric(wtd.quantile(X_general_self_political_scale, probs = 0.66))) %>%
  ungroup() %>%
  mutate(
    F_self_pol3_all = if_else(
      X_general_self_political_scale >= pc66_selfpol, "Right-wing", NA_character_),
    F_self_pol3_all = if_else(
      X_general_self_political_scale < pc66_selfpol &
        X_general_self_political_scale >= pc33_selfpol, "Center", F_self_pol3_all),
    F_self_pol3_all = if_else(
      X_general_self_political_scale < pc33_selfpol, "Left-wing", F_self_pol3_all),
    F_self_pol3_all = factor(F_self_pol3_all,
                             levels = c("Left-wing", "Center", "Right-wing"), ordered = T),
    F_self_pol2_all = if_else(
      X_general_self_political_scale < pc50_selfpol, "Left-wing", "Right-wing"))

# Create left-right groups by opinion index
df <- df %>% 
  mutate(F_op = if_else(opinionIndex <= 0, "Left-wing opinions", "Right-wing opinions"),
         F_op = factor(F_op, levels = c("Left-wing opinions","Right-wing opinions"), ordered = T))

# Create left-right groups by self_scale directly (Right wing: >=6, left wing: <4, Center: else)
df <- df %>% 
  mutate(
    F_self_pol3_points = if_else(
      X_general_self_political_scale >= 6, "Right-wing", NA_character_),
    F_self_pol3_points = if_else(
      X_general_self_political_scale < 6 &
        X_general_self_political_scale >= 4, "Center", F_self_pol3_points),
    F_self_pol3_points = if_else(
      X_general_self_political_scale < 4, "Left-wing", F_self_pol3_points),
    F_self_pol3_points = factor(F_self_pol3_points,
                                levels = c("Left-wing", "Center", "Right-wing"), ordered = T)
  )
# Create left-right groups by country percentiles
df <- df %>% 
  group_by(BB_Country, wave) %>% 
  mutate(
    pc50_selfpol_country = as.numeric(quantile(X_general_self_political_scale, probs = 0.5)),
    pc33_selfpol_country = as.numeric(quantile(X_general_self_political_scale, probs = 0.33)),
    pc66_selfpol_country = as.numeric(quantile(X_general_self_political_scale, probs = 0.66))
  ) %>%
  ungroup() %>%
  mutate(
    F_self_pol3_country = if_else(
      X_general_self_political_scale >= pc66_selfpol_country, "Right-wing", NA_character_),
    F_self_pol3_country = if_else(
      X_general_self_political_scale < pc66_selfpol_country & 
        X_general_self_political_scale >= pc33_selfpol_country, "Center", F_self_pol3_country),
    F_self_pol3_country = if_else(
      X_general_self_political_scale < pc33_selfpol_country, "Left-wing", F_self_pol3_country),
    F_self_pol3_country = factor(F_self_pol3_country, 
                                 levels = c("Left-wing", "Center", "Right-wing"), ordered = T),
    F_self_pol2_country = if_else(
      X_general_self_political_scale < pc50_selfpol_country, "Left-wing", "Right-wing"))



# Filter and select variables
df <- df %>%
  filter(if_all(starts_with("F_"), ~!is.na(.x))) %>% 
  select(BB_Id, BB_Weight_CW, wave, BB_Country, variable_original, variable, value, starts_with("F"))
  
  
# *************************************************************************
# Plot ----
# *************************************************************************

group_vars <- df %>% 
  select(starts_with("F_")) %>% 
  colnames()


########################################################################
#Plot error bars - Chetty style
########################################################################

df_for_aggregate <- df %>% select(wave,variable,value,
                                  F_sex, F_PRRPscore_std_right, F_age, F_emp, F_union, F_edu, F_bigtown, F_income) %>% 
  filter(wave != 5)
try_df_long <- df_for_aggregate %>% pivot_longer(cols = -c(wave, variable, value),
                                                 names_to = "type",
                                                 values_to = "category")

aggregated_df <- aggregate(value~wave + variable  + category + type, data =try_df_long, FUN = median)

aggregated_df_median <- aggregated_df %>% rename("median" = value)
aggregated_df_75pc <- aggregate(value~wave + variable  + category + type, data =try_df_long, function(x) quantile(x, probs =0.75,na.rm=T))
aggregated_df_75pc <- aggregated_df_75pc %>% rename("pc75" = value)

aggregated_df_25pc <- aggregate(value~wave + variable  + category + type, data =try_df_long, function(x) quantile(x, probs =0.25,na.rm=T))
aggregated_df_25pc <- aggregated_df_25pc %>% rename("pc25" = value)


aggregated_final_df <-  left_join(aggregated_df_median, aggregated_df_75pc) %>%
  left_join(., aggregated_df_25pc) 

prrpPosName = "Above-Median\nPRRP Score"
prrpNegName = "Below-Median\nPRRP Score"

aggregated_final_df <- aggregated_final_df %>% 
  mutate(
    # Use the above/below-median PRRP score labels created by add_F_vars().
    category = ifelse(category == "Above Median PRRP Score", prrpPosName, category),
    category = ifelse(category == "Below Median PRRP Score", prrpNegName, category),
    category = ifelse(category == "Not a Union Member", "Not a\nUnion Member", category)
  ) %>% 
  filter(type != "F_emp")

levels = c("College", "Did Not Complete College",
           "Above Median Income", "Below Median Income", 
           "Male", "Female", 
           "18-50", "51+",
           "Urban", "Rural",
           "Union Member", "Not a\nUnion Member",
           # "Not Attending Religious Services", "Attending Religious Services",
           prrpPosName, prrpNegName)

aggregated_final_df <- aggregated_final_df %>% 
  mutate(var1 = case_when(category == prrpNegName ~ 1,
                          category == prrpPosName ~ 2,
                          # category == "Not Attending Religious Services" ~ 3,
                          # category == "Attending Religious Services" ~ 4,
                          category == "Not a\nUnion Member" ~ 5,
                          category == "Union Member" ~ 6,
                          category == "Rural" ~ 7,
                          category == "Urban" ~ 8,
                          category == "51+" ~ 9,
                          category == "18-50" ~ 10,
                          category == "Female" ~ 11,
                          category == "Male" ~ 12,
                          category == "Below Median Income" ~ 13,
                          category == "Above Median Income" ~ 14,
                          category == "Did Not Complete College" ~ 15,
                          category == "College" ~ 16)) %>% 
  mutate(across(var1, factor))

# *************************************************************************
#  REGRESSION ----
# *************************************************************************

df_plot <- df %>%
  select(BB_Id, wave, variable_original, value, F_edu, F_age, F_sex, F_bigtown, F_union, F_PRRPscore_std_right, F_income, BB_Weight_CW) %>%
  filter(wave %in% c(4, 6)) %>%
  pivot_wider(names_from = variable_original, values_from = value, values_fn = mean) %>%
  mutate(log_ratio = log(abs(soc_index / econ_index))) %>%
  group_by(BB_Id) %>%
  filter(all(c(4, 6) %in% wave)) %>%
  summarise(
    value = log_ratio[wave == 6][1] - log_ratio[wave == 4][1],
    F_edu = coalesce(F_edu[wave == 6][1], F_edu[wave == 4][1]),
    F_age = coalesce(F_age[wave == 6][1], F_age[wave == 4][1]),
    F_sex = coalesce(F_sex[wave == 6][1], F_sex[wave == 4][1]),
    F_bigtown = coalesce(F_bigtown[wave == 6][1], F_bigtown[wave == 4][1]),
    F_union = coalesce(F_union[wave == 6][1], F_union[wave == 4][1]),
    F_PRRPscore_std_right = coalesce(F_PRRPscore_std_right[wave == 6][1], F_PRRPscore_std_right[wave == 4][1]),
    F_income = coalesce(F_income[wave == 6][1], F_income[wave == 4][1]),
    BB_Weight_CW = coalesce(BB_Weight_CW[wave == 6][1], BB_Weight_CW[wave == 4][1]),
    .groups = "drop"
  ) %>%
  mutate(
    D_No_College = ifelse(F_edu == "Did Not Complete College", 1, 0),
    D_College = ifelse(F_edu == "College", 1, 0),
    D_18_50 = ifelse(F_age == "18-50", 1, 0),
    D_51 = ifelse(F_age == "51+", 1, 0),
    D_Male = ifelse(F_sex == "Male", 1, 0),
    D_Female = ifelse(F_sex == "Female", 1, 0),
    D_Rural = ifelse(F_bigtown == "Rural", 1, 0),
    D_Urban = ifelse(F_bigtown == "Urban", 1, 0),
    D_Not_Union = ifelse(F_union == "Not a Union Member", 1, 0),
    D_Union = ifelse(F_union == "Union Member", 1, 0),
    # Split respondents by the weighted median PRRP score from add_F_vars().
    D_Above_PRRP_Score = ifelse(F_PRRPscore_std_right == "Above Median PRRP Score", 1, 0),
    D_Below_PRRP_Score = ifelse(F_PRRPscore_std_right == "Below Median PRRP Score", 1, 0),
    D_Above_Median = ifelse(F_income == "Above Median Income", 1, 0),
    D_Below_Median = ifelse(F_income == "Below Median Income", 1, 0)
  )

model_wave <- lm(value ~ 1, data = df_plot, weights = BB_Weight_CW)
vcov_wave <- vcovHC(model_wave, type = "HC1")

model_edu <- lm(value ~ 0 + D_No_College + D_College, data = df_plot, weights = BB_Weight_CW)
vcov_edu <- vcovHC(model_edu, type = "HC1")

model_age <- lm(value ~ 0 + D_18_50 + D_51, data = df_plot, weights = BB_Weight_CW)
vcov_age <- vcovHC(model_age, type = "HC1")

model_sex <- lm(value ~ 0 + D_Female + D_Male, data = df_plot, weights = BB_Weight_CW)
vcov_sex <- vcovHC(model_sex, type = "HC1")

model_town <- lm(value ~ 0 + D_Rural + D_Urban, data = df_plot, weights = BB_Weight_CW)
vcov_town <- vcovHC(model_town, type = "HC1")

model_union <- lm(value ~ 0 + D_Not_Union + D_Union, data = df_plot, weights = BB_Weight_CW)
vcov_union <- vcovHC(model_union, type = "HC1")

model_prrp <- lm(value ~ 0 + D_Below_PRRP_Score + D_Above_PRRP_Score, data = df_plot, weights = BB_Weight_CW)
vcov_prrp <- vcovHC(model_prrp, type = "HC1")

model_income <- lm(value ~ 0 + D_Below_Median + D_Above_Median, data = df_plot, weights = BB_Weight_CW)
vcov_income <- vcovHC(model_income, type = "HC1")

resolve_term_name <- function(coef_names, term) {
  if (term %in% coef_names) {
    return(term)
  }
  if (grepl(":", term, fixed = TRUE)) {
    term_split <- strsplit(term, ":", fixed = TRUE)[[1]]
    alt_term <- paste(rev(term_split), collapse = ":")
    if (alt_term %in% coef_names) {
      return(alt_term)
    }
  }
  stop(paste0("Term not found in model coefficients: ", term))
}

# *************************************************************************
#  REGRESSION TABLE (HETEROGENEITY, CHANGE ON DEMOGRAPHICS) ----
# *************************************************************************

table_model_edu <- lm(value ~ D_College, data = df_plot, weights = BB_Weight_CW)
table_model_income <- lm(value ~ D_Above_Median, data = df_plot, weights = BB_Weight_CW)
table_model_age <- lm(value ~ D_51, data = df_plot, weights = BB_Weight_CW)
table_model_sex <- lm(value ~ D_Male, data = df_plot, weights = BB_Weight_CW)
table_model_union <- lm(value ~ D_Union, data = df_plot, weights = BB_Weight_CW)
table_model_town <- lm(value ~ D_Urban, data = df_plot, weights = BB_Weight_CW)
table_model_prrp <- lm(value ~ D_Above_PRRP_Score, data = df_plot, weights = BB_Weight_CW)
table_model_all <- lm(
  value ~ D_College +
    D_Above_Median +
    D_51 +
    D_Male +
    D_Union +
    D_Urban +
    D_Above_PRRP_Score,
  data = df_plot,
  weights = BB_Weight_CW
)

vcov_table_edu <- vcovHC(table_model_edu, type = "HC1")
vcov_table_income <- vcovHC(table_model_income, type = "HC1")
vcov_table_age <- vcovHC(table_model_age, type = "HC1")
vcov_table_sex <- vcovHC(table_model_sex, type = "HC1")
vcov_table_union <- vcovHC(table_model_union, type = "HC1")
vcov_table_town <- vcovHC(table_model_town, type = "HC1")
vcov_table_prrp <- vcovHC(table_model_prrp, type = "HC1")
vcov_table_all <- vcovHC(table_model_all, type = "HC1")

table_row_order <- c(
  "(Intercept)",
  "College",
  "Above Median Income",
  "Age 51+",
  "Male",
  "Union Member",
  "Urban",
  "Above Median PRRP Score"
)

table_col_specs <- list(
  "(1)" = list(
    model = model_wave,
    vcov = vcov_wave,
    terms = c(
      "(Intercept)" = "(Intercept)"
    )
  ),
  "(2)" = list(
    model = table_model_edu,
    vcov = vcov_table_edu,
    terms = c(
      "(Intercept)" = "(Intercept)",
      "College" = "D_College"
    )
  ),
  "(3)" = list(
    model = table_model_income,
    vcov = vcov_table_income,
    terms = c(
      "(Intercept)" = "(Intercept)",
      "Above Median Income" = "D_Above_Median"
    )
  ),
  "(4)" = list(
    model = table_model_age,
    vcov = vcov_table_age,
    terms = c(
      "(Intercept)" = "(Intercept)",
      "Age 51+" = "D_51"
    )
  ),
  "(5)" = list(
    model = table_model_sex,
    vcov = vcov_table_sex,
    terms = c(
      "(Intercept)" = "(Intercept)",
      "Male" = "D_Male"
    )
  ),
  "(6)" = list(
    model = table_model_union,
    vcov = vcov_table_union,
    terms = c(
      "(Intercept)" = "(Intercept)",
      "Union Member" = "D_Union"
    )
  ),
  "(7)" = list(
    model = table_model_town,
    vcov = vcov_table_town,
    terms = c(
      "(Intercept)" = "(Intercept)",
      "Urban" = "D_Urban"
    )
  ),
  "(8)" = list(
    model = table_model_prrp,
    vcov = vcov_table_prrp,
    terms = c(
      "(Intercept)" = "(Intercept)",
      "Above Median PRRP Score" = "D_Above_PRRP_Score"
    )
  ),
  "(9)" = list(
    model = table_model_all,
    vcov = vcov_table_all,
    terms = c(
      "(Intercept)" = "(Intercept)",
      "College" = "D_College",
      "Above Median Income" = "D_Above_Median",
      "Age 51+" = "D_51",
      "Male" = "D_Male",
      "Union Member" = "D_Union",
      "Urban" = "D_Urban",
      "Above Median PRRP Score" = "D_Above_PRRP_Score"
    )
  )
)

to_sig_stars <- function(p) {
  ifelse(
    is.na(p), "",
    ifelse(p < 0.01, "***",
           ifelse(p < 0.05, "**",
                  ifelse(p < 0.1, "*", "")))
  )
}

build_table_column <- function(model, vcov_mat, term_map, row_order) {
  coef_out <- setNames(rep("", length(row_order)), row_order)
  se_out <- setNames(rep("", length(row_order)), row_order)
  coef_names <- names(coef(model))

  for (row_name in names(term_map)) {
    term_name <- resolve_term_name(coef_names, term_map[[row_name]])
    estimate <- unname(coef(model)[term_name])
    std_error <- sqrt(vcov_mat[term_name, term_name])
    z_value <- ifelse(std_error > 0, estimate / std_error, NA_real_)
    p_value <- ifelse(std_error > 0, 2 * pnorm(-abs(z_value)), NA_real_)

    coef_out[row_name] <- paste0(sprintf("%.3f", estimate), to_sig_stars(p_value))
    se_out[row_name] <- paste0("(", sprintf("%.3f", std_error), ")")
  }

  unlist(lapply(row_order, function(lbl) c(coef_out[lbl], se_out[lbl])), use.names = FALSE)
}

table_term_col <- unlist(lapply(table_row_order, function(lbl) c(lbl, "")), use.names = FALSE)

reg_table_df <- tibble(Term = table_term_col)
for (col_name in names(table_col_specs)) {
  spec <- table_col_specs[[col_name]]
  reg_table_df[[col_name]] <- build_table_column(
    model = spec$model,
    vcov_mat = spec$vcov,
    term_map = spec$terms,
    row_order = table_row_order
  )
}

table_outdir <- file.path("output", "tables", "model_weight")
if (!dir.exists(table_outdir)) {
  dir.create(table_outdir, recursive = TRUE)
}

hetero_reg_table <- kbl(
  reg_table_df,
  format = "latex",
  booktabs = TRUE,
  align = c("l", rep("c", length(table_col_specs))),
  col.names = c("", names(table_col_specs)),
  linesep = ""
) %>%
  row_spec(0, bold = TRUE, extra_latex_after = "\\midrule")


tidy_single_coef <- function(model, vcov_mat, term, label, group) {
  coef_names <- names(coef(model))
  resolved_term <- resolve_term_name(coef_names, term)

  estimate <- unname(coef(model)[resolved_term])
  std_error <- sqrt(vcov_mat[resolved_term, resolved_term])

  data.frame(
    term = label,
    estimate = estimate,
    std.error = std_error,
    group = group
  ) %>%
    mutate(
      conf.low = estimate - 1.96 * std.error,
      conf.high = estimate + 1.96 * std.error
    )
}

tidy_coef_diff <- function(model, vcov_mat, term_high, term_low, label, group) {
  coef_names <- names(coef(model))
  resolved_high <- resolve_term_name(coef_names, term_high)
  resolved_low <- resolve_term_name(coef_names, term_low)

  estimate <- unname(coef(model)[resolved_high] - coef(model)[resolved_low])
  var_diff <- vcov_mat[resolved_high, resolved_high] +
    vcov_mat[resolved_low, resolved_low] -
    2 * vcov_mat[resolved_high, resolved_low]
  std_error <- sqrt(pmax(var_diff, 0))
  z_value <- ifelse(std_error > 0, estimate / std_error, NA_real_)
  p_value <- ifelse(std_error > 0, 2 * pnorm(-abs(z_value)), NA_real_)

  data.frame(
    term = label,
    estimate = estimate,
    std.error = std_error,
    group = group,
    p.value = p_value
  ) %>%
    mutate(
      conf.low = estimate - 1.96 * std.error,
      conf.high = estimate + 1.96 * std.error
    )
}

group_order <- c(
  "Wave",
  "Age", 
  "Town Size", 
  "Education", 
  "Income", 
  "Union Membership", 
  "Sex", 
  # "Religiosity",
  "PRRP Score")

group_colors <- c(
  "Wave" = "#F26D5B",
  "Age" = "#10B3B8",
  "Town Size" = "#B16BE8",
  "Education" = "#D39A00",
  "Income" = "#7FB800",
  "Union Membership" = "#1D9BF0",
  "Sex" = "#08BE67",
  "PRRP Score" = "#F15BC1"
)

# Keep the heterogeneity regression plot in the same subgroup order as the paired-bar figures.
term_breaks <- c(
  "Full Sample",
  "Age 51+", "Age 18-50",
  "Male", "Female",
  "No College", "College",
  "Below-Median Income", "Above-Median Income",
  "Rural", "Urban",
  "Union", "Not Union",
  "Above-Median PRRP Score", "Below-Median PRRP Score"
)

term_levels <- rev(c(
  "Full Sample", "1",
  "Age 51+", "Age 18-50", "2",
  "Male", "Female", "3",
  "No College", "College", "4",
  "Below-Median Income", "Above-Median Income", "5",
  "Rural", "Urban", "6",
  "Union", "Not Union", "7",
  "Above-Median PRRP Score", "Below-Median PRRP Score"
))

# Extract results from each model
results_plot <- bind_rows(
  tidy_single_coef(model_wave, vcov_wave, "(Intercept)", "Full Sample", "Wave"),
  tidy_single_coef(model_age, vcov_age, "D_18_50", "Age 18-50", "Age"),
  tidy_single_coef(model_age, vcov_age, "D_51", "Age 51+", "Age"),
  tidy_single_coef(model_sex, vcov_sex, "D_Male", "Male", "Sex"),
  tidy_single_coef(model_sex, vcov_sex, "D_Female", "Female", "Sex"),
  tidy_single_coef(model_town, vcov_town, "D_Rural", "Rural", "Town Size"),
  tidy_single_coef(model_town, vcov_town, "D_Urban", "Urban", "Town Size"),
  tidy_single_coef(model_edu, vcov_edu, "D_College", "College", "Education"),
  tidy_single_coef(model_edu, vcov_edu, "D_No_College", "No College", "Education"),
  tidy_single_coef(model_income, vcov_income, "D_Below_Median", "Below-Median Income", "Income"),
  tidy_single_coef(model_income, vcov_income, "D_Above_Median", "Above-Median Income", "Income"),
  tidy_single_coef(model_union, vcov_union, "D_Not_Union", "Not Union", "Union Membership"),
  tidy_single_coef(model_union, vcov_union, "D_Union", "Union", "Union Membership"),
  tidy_single_coef(model_prrp, vcov_prrp, "D_Above_PRRP_Score", "Above-Median PRRP Score", "PRRP Score"),
  tidy_single_coef(model_prrp, vcov_prrp, "D_Below_PRRP_Score", "Below-Median PRRP Score", "PRRP Score")
) %>% 
  mutate(group = factor(group, levels = group_order)) %>% 
  mutate(term = factor(term, levels = term_levels))

delta_results <- bind_rows(
  tidy_coef_diff(model_sex, vcov_sex, "D_Male", "D_Female", "Male", "Sex"),
  tidy_coef_diff(model_age, vcov_age, "D_51", "D_18_50", "Age 51+", "Age"),
  # Anchor each p-value to the first label shown in its pair.
  tidy_coef_diff(model_town, vcov_town, "D_Urban", "D_Rural", "Rural", "Town Size"),
  tidy_coef_diff(model_edu, vcov_edu, "D_College", "D_No_College", "No College", "Education"),
  tidy_coef_diff(model_income, vcov_income, "D_Above_Median", "D_Below_Median", "Below-Median Income", "Income"),
  tidy_coef_diff(model_union, vcov_union, "D_Union", "D_Not_Union", "Union", "Union Membership"),
  tidy_coef_diff(model_prrp, vcov_prrp, "D_Above_PRRP_Score", "D_Below_PRRP_Score", "Above-Median PRRP Score", "PRRP Score")
) %>%
  mutate(
    term = factor(term, levels = levels(results_plot$term)),
    group = factor(group, levels = group_order),
    p_value_label = if_else(
      is.na(p.value),
      "NA",
      if_else(p.value < 0.001, "<0.001", sprintf("%.3f", p.value))
    ),
    p_label = if_else(
      p.value < 0.001, 
      "p < 0.001", sprintf("p = %.3f", p.value))
  )

p_label_x <- max(results_plot$conf.high, na.rm = TRUE) + 0.13

separator_positions <- match(as.character(1:7), levels(results_plot$term))
separator_positions <- separator_positions[!is.na(separator_positions)]

# Create a plot
p_base <- ggplot(results_plot, aes(x = term, y = estimate, ymin = conf.low, ymax = conf.high, color = group)) +
  geom_vline(
    xintercept = separator_positions,
    linetype = "longdash",
    color = "gray60",
    linewidth = 0.4
  ) +
  geom_pointrange(position = position_dodge(width = 0.8),size = 0.35) +
  geom_text(
    data = delta_results,
    aes(x = term, y = p_label_x, label = p_label, color = group),
    inherit.aes = FALSE,
    size = 2.35,
    hjust = 0
  ) +
  coord_flip() +
  scale_x_discrete(labels = term_breaks, breaks = term_breaks, drop = FALSE) +
  scale_y_continuous(
    limits = c(-0.2, 0.8), 
    breaks = seq(-0.2, 0.6, by = 0.2)) +
  scale_color_manual(values = group_colors, drop = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray", size = 1) + 
  theme_classic() +
  labs(
    x = "",
    y = "Change in Log Ratio of Cultural vs Economic Absolute Weights 2005–2020",
    color = "Group") +
  theme(axis.title.x = element_text(size = 10))+
  guides(color = "none")

ggsave(p_base, filename = "hetero_prior_change.pdf", path = outdir, width=figureWidth, height=12, unit="cm")

# *************************************************************************
#  CREATE WIDE PLOT WITH 2 FACETS LINES ----
# *************************************************************************

aggData = aggregated_final_df %>%  
  mutate(category = factor(category, levels = rev(levels))) %>% 
  group_by(type)

aggData$type <- factor(aggData$type, levels = c("F_edu",
                                                "F_income",
                                                "F_sex",
                                                "F_age",
                                                "F_union",
                                                "F_bigtown",
                                                # "F_rel",
                                                "F_PRRPscore_std_right"))

aggData %>% 
  ggplot(aes(y = category, colour = rev(wave)))+
  facet_grid(type  ~  factor(variable, 
                             levels = c("Economic Index", "Cultural Index"),
                             labels = c("Panel A: Economic Index","Panel B: Cultural Index")) , scales = "free_y")+
  geom_boxplot(aes(xlower=pc25,xupper=pc75, xmiddle=median, x=median, xmin=pc25, xmax=pc75),
               outlier.shape = NA, width=0.2,stat = "identity",
               position = position_dodge(width = 0.5))+
  geom_point(aes(x = median, y = category), position =position_dodge(width = 0.5),
             size = 1.5)+
  ylab("") +
  scale_colour_manual(name="Wave", limits = factor(c(6,4)), 
                      labels = c("2005-2009","2017-2020"), values=c("#5673e9","#E69F00"))+
  geom_vline(xintercept = 0, linetype = "twodash")  +
  guides(color=guide_legend(direction='vertical'))+
  theme_bw()+
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        plot.margin = margin(1,1,1,1),
        legend.margin = margin(0,0,0,0),
        legend.spacing.y = unit(0, 'cm'),
        panel.grid = element_blank(),
        strip.text.y = element_blank(),
        legend.key.size = unit(0.5, 'cm'),
        legend.key.height = unit(0.8, 'cm')) +
  xlab("Voting Weights (p25, median, p75)") +
  xlim(min(aggData$pc25) - 0.05, 1.05)
