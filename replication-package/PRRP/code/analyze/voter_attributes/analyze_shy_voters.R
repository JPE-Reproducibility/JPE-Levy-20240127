# *************************************************************************
# Description ----
# *************************************************************************
# This script generates:
# Table A.6: Actual and Predicted Support by Party Family over Time
#
# This table compares actual and predicted vote shares by party family
# across two time periods: 2005–2009 (Panel A) and 2017–2020 (Panel B).
#
# Actual support (%) is calculated based on respondents who expressed
# support for a specific political party.
#
# Predicted support (%) is estimated for respondents who did not provide
# a party preference, using a multinomial LASSO model trained on non-shy
# voters and a set of attitudinal and demographic variables.
#
# The table reports average party family vote shares across countries
# within each period.

# Input files:
# - pathWVS_Data (defined in commonVars.R)
# - datasets/manifesto/manifesto_populist.rds

# Output file:
# - output/tables/shy_voters.tex
# *************************************************************************

# *************************************************************************
# SETUP ----
# *************************************************************************

rm(list = ls())

# Packages
library(caret)
library(glmnet)
library(haven)
library(dplyr)
library(tidyr)
library(kableExtra)

# Paths and shared objects
source("code/util/commonVars.R")
source("code/analyze/party_positions/manifesto_general_functions.R")

# *************************************************************************
# LOAD DATA ----
# *************************************************************************

# Intermediate IVS data (used for missingness filtering)
inter_data <- read_dta(file.path(cleandata_path, "ivs", "full_clean_ivs.dta"))

# Main analysis dataset (used for prediction)
analysis_data <- read_dta(
  file.path(cleandata_path, "ivs", "ivs_selected_countries_imputed_with_manifesto.dta")
)

# Final WVS dataset (used for observed vote shares)
final_data <- read_dta(pathWVS_Data)

# *************************************************************************
# FUNCTIONS ----
# *************************************************************************

# Flag shy voters based on party response variables
flag_shy_voters <- function(df) {
  df %>%
    mutate(
      shy_voters = case_when(
        BB_Party_org %in% c(
          "No answer", "Don't vote", "Don't know",
          "I would cast a blank ballot, white vote",
          "I would cast a blank ballot; White vote ",
          "I would not vote"
        ) ~ 1,
        TRUE ~ 0
      ),
      shy_voters = if_else(
        !BB_Party %in% c(
          "No answer", "Don't vote", "Don't know",
          "I would cast a blank ballot, white vote",
          "I would cast a blank ballot; White vote ",
          "I would not vote"
        ),
        0,
        shy_voters
      )
    )
}

# Exclude respondents with excessive missing opinions
exclude_high_missing <- function(df, removed_ids) {
  df %>% filter(!BB_Id %in% removed_ids)
}

# *************************************************************************
# PROCESS DATA ----
# *************************************************************************

# Apply shy voter flag
analysis_data <- flag_shy_voters(analysis_data)
final_data    <- flag_shy_voters(final_data)

# Compute missing opinion shares
inter_data <- inter_data %>%
  mutate(
    missing_opinions = rowSums(across(starts_with("X_op"), is.na)),
    missing_opinions_share = round(missing_opinions * 100 / 130, 2)
  )

inter_data <- inter_data %>%
  mutate(
    missing_questions = rowSums(across(matches("^X_op|^X_dem"), is.na)),
    missing_questions_share = round(missing_opinions * 100 / 166, 2)
  )

# Respondents to be excluded due to excessive missingness
inter_data_removed <- inter_data %>%
  filter(missing_opinions_share >= 50)

# Manifesto data
manifesto_populist <- readRDS(paste0(cleandata_path, "/manifesto/manifesto_populist.rds")) %>%
  mutate(party_date = paste0(Y_manifesto_party_id, "_", date)) %>%
  distinct(Y_manifesto_party_id, .keep_all = TRUE) %>%
  select(Y_manifesto_party_id, parfam, Y_FR_Pop) %>%
  add_parfam() %>%
  select(Y_manifesto_party_id, BBF_parfam)

# *************************************************************************
# PREPARE ANALYSIS SAMPLE ----
# *************************************************************************

analysis_data <- exclude_high_missing(
  analysis_data,
  inter_data_removed$BB_Id
)

set.seed(123)

list_results <- list()
list_pred_counts <- list()

list_country <- analysis_data %>%
  filter(BB_Wave_num == 6) %>%
  pull(BB_Country) %>%
  unique() %>%
  .[! . %in% c("US", "Czech Republic", "Slovakia")]

# *************************************************************************
# MAIN LOOP ----
# *************************************************************************

for (wave in c(4, 6)) {
  for (country in list_country) {
    
    message(country, " ", wave)
    
    df_wave <- analysis_data %>%
      filter(BB_Wave_num == wave)
    
    df_country <- df_wave %>%
      filter(BB_Country == country)
    
    # Shrink data: keep parties with at least 10 voters
    df_country <- df_country %>%
      group_by(Y_manifesto_party_id) %>%
      mutate(count_voter = n()) %>%
      ungroup() %>%
      filter(count_voter >= 10) %>%
      select(-X_general_self_political_scale) %>%
      select(
        BB_Weight_CW, BB_Id, BB_Country, BB_Wave_num,
        Y_manifesto_party_id, shy_voters,
        matches("^X_op|^X_dem")
      )
    
    predictor_columns <- grep("^X_", names(df_country), value = TRUE)
    
    # Training data: non-shy voters, parties with sufficient support
    df_train <- df_country %>%
      filter(
        shy_voters == 0,
        !is.na(Y_manifesto_party_id)
      ) %>%
      group_by(Y_manifesto_party_id) %>%
      filter(n() >= 10) %>%
      ungroup()
    
    df_pred <- df_country %>%
      filter(shy_voters == 1)
    
    if (nrow(df_pred) <= 20) {
      message("Skipping ", country, ", wave ", wave, ": too few observations")
      next
    }
    
    X_train <- as.matrix(df_train[, predictor_columns])
    X_pred  <- as.matrix(df_pred[, predictor_columns])
    Y       <- as.factor(df_train$Y_manifesto_party_id)
    ID      <- df_pred$BB_Id
    
    # Multinomial LASSO with cross-validation
    cvfit <- cv.glmnet(
      x = X_train,
      y = Y,
      nfolds = 10,
      family = "multinomial",
      weights = df_train$BB_Weight_CW
    )

    pred <- predict(
      cvfit,
      newx = X_pred,
      s = "lambda.min",
      type = "response",
    )
    
    pred_df <- as.data.frame(pred) %>%
      mutate(BB_Id = ID) %>%
      pivot_longer(
        cols = -BB_Id,
        names_to = "pred_party_id",
        values_to = "probability"
      ) %>%
      mutate(
        pred_party_id = gsub("\\..*", "", pred_party_id),
        pred_party_id = as.integer(pred_party_id),
        BB_Country = country,
        wave = wave
      ) %>%
      left_join(df_pred %>% select(BB_Id, BB_Weight_CW), by = "BB_Id") %>%
      group_by(pred_party_id, BB_Country, wave) %>%
      summarise(vote_share = Hmisc::wtd.mean(probability, BB_Weight_CW, na.rm = TRUE), .groups = "drop") %>%
      ungroup()
    
    list_results[[paste(country, wave, sep = "_")]] <- pred_df
    list_pred_counts[[paste(country, wave, sep = "_")]] <- tibble(
      BB_Country = country,
      wave = wave,
      predicted_n = nrow(df_pred)
    )
  }
}

# *************************************************************************
# AGGREGATION AND TABLE ----
# *************************************************************************

df_results <- bind_rows(list_results)
pred_counts <- bind_rows(list_pred_counts)
included_country_waves <- pred_counts %>%
  distinct(BB_Country, wave)

party_families <- c(
  "PRRP", "Center-Right", "Center-Left",
  "Socialist", "Green", NA_character_
)

all_party_families <- df_results %>%
  distinct(BB_Country, wave) %>%
  crossing(BBF_parfam = party_families) %>%
  filter(!BB_Country %in% setdiff(list_country, unique(df_results$BB_Country)))

df_results <- df_results %>%
  left_join(manifesto_populist, by = c("pred_party_id" = "Y_manifesto_party_id")) %>%
  full_join(all_party_families, by = c("BB_Country", "BBF_parfam", "wave")) %>%
  mutate(
    vote_share = ifelse(is.na(vote_share), 0, vote_share)
  )

df_real <- final_data %>%
  filter(shy_voters == 0, BB_Wave_num %in% c(4, 6)) %>%
  semi_join(included_country_waves, by = c("BB_Country", "BB_Wave_num" = "wave")) %>%
  left_join(manifesto_populist, by = "Y_manifesto_party_id") %>%
  select(BBF_parfam, BB_Country, BB_Wave_num, BB_Weight_CW)

panel_counts <- final_data %>%
  filter(shy_voters == 0, BB_Wave_num %in% c(4, 6)) %>%
  transmute(BB_Country, wave = BB_Wave_num) %>%
  semi_join(included_country_waves, by = c("BB_Country", "wave")) %>%
  count(wave, name = "actual_n") %>%
  full_join(
    pred_counts %>%
      group_by(wave) %>%
      summarise(predicted_n = sum(predicted_n), .groups = "drop"),
    by = "wave"
  ) %>%
  complete(wave = c(4, 6), fill = list(actual_n = 0, predicted_n = 0)) %>%
  arrange(wave)

mean_party_shares <- df_results %>%
  group_by(BBF_parfam, BB_Country, wave) %>%
  summarise(total_share = sum(vote_share), .groups = "drop") %>%
  ungroup() %>%
  group_by(BBF_parfam, wave) %>%
  summarise(mean_share_pred = mean(total_share), .groups = "drop")

mean_party_shares_real <- df_real %>%
  rename(wave = BB_Wave_num) %>%
  filter(wave != 5) %>%
  # Sum IVS respondent weights within each country-wave party family.
  group_by(BB_Country, BBF_parfam, wave) %>%
  summarise(party_weight = sum(BB_Weight_CW, na.rm = TRUE), .groups = "drop") %>%
  ungroup() %>%
  # Convert weighted party-family totals into within-country support shares.
  group_by(BB_Country, wave) %>%
  mutate(country_weight = sum(party_weight, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(vote_share = party_weight / country_weight) %>%
  select(-party_weight, -country_weight) %>%
  full_join(all_party_families, by = c("BB_Country", "BBF_parfam", "wave")) %>%
  mutate(vote_share = ifelse(is.na(vote_share), 0, vote_share)) %>%
  # Average country-level weighted shares with equal country weights.
  group_by(BBF_parfam, wave) %>%
  summarise(mean_share_real = mean(vote_share, na.rm = TRUE), .groups = "drop")

BBF_order <- c(
  "PRRP", "Center-Right", "Center-Left",
  "Socialist", "Green", "Other"
)

format_panel_n <- function(x) {
  format(x, big.mark = ",", scientific = FALSE, trim = TRUE)
}

tab_main <- mean_party_shares_real %>%
  inner_join(mean_party_shares, by = c("BBF_parfam", "wave")) %>%
  mutate(BBF_parfam = ifelse(is.na(BBF_parfam), "Other", BBF_parfam)) %>%
  mutate(BBF_parfam = factor(BBF_parfam, levels = BBF_order)) %>%
  mutate(
    mean_share_real = formatC(mean_share_real * 100, digits = 2, format = "f"),
    mean_share_pred = formatC(mean_share_pred * 100, digits = 2, format = "f")
  ) %>%
  arrange(wave, BBF_parfam) %>%
  transmute(
    wave,
    row_label = as.character(BBF_parfam),
    actual_support = mean_share_real,
    predicted_support = mean_share_pred
  )

tab_n <- panel_counts %>%
  transmute(
    wave,
    row_label = "$N$",
    actual_support = format_panel_n(actual_n),
    predicted_support = format_panel_n(predicted_n)
  )

tab <- bind_rows(
  tab_main %>% filter(wave == 4),
  tab_n %>% filter(wave == 4),
  tab_main %>% filter(wave == 6),
  tab_n %>% filter(wave == 6)
) %>%
  select(-wave) %>%
  rename(
    ` ` = row_label,
    `Actual Support (\\%)` = actual_support,
    `Predicted Support among Non-respondents` = predicted_support
  )

tab <- kbl(
  tab,
  col.names = c(
    " ",
    "Actual Support (\\%)",
    linebreak("Predicted Support among\nNon-respondents", align = "c")
  ),
  booktabs = TRUE,
  linesep = "",
  format = "latex",
  escape = FALSE,
  align = c("l", "c", "c")
) %>%
  row_spec(
    0,
    extra_latex_after = "\\midrule \\multicolumn{3}{l}{Panel A: 2005--2009} \\\\"
  ) %>%
  row_spec(7, hline_after = TRUE) %>%
  row_spec(
    7,
    extra_latex_after = "\\midrule \\multicolumn{3}{l}{Panel B: 2017--2020} \\\\"
  ) %>%
  gsub("\\\\midrule\\\\\\s*\\\\", "\\\\midrule", .)

save_kable(tab, "output/tables/shy_voters.tex")
