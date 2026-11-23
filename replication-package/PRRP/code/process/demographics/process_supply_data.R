# *************************************************************************
# Description ----
# *************************************************************************

# Combine all supply side manifesto data

# *************************************************************************
# SETUP ----
# *************************************************************************

rm(list = ls())

library(dplyr)
library(tidyr)
library(purrr)
library(haven)
library(randomForest)
library(missForest)
library(labelled)
library(data.table)
library(savetexvalue)

# paths
source("code/util/commonVars.R")
source("code/process/paths.R")

# load data

# supply side manifesto data
style <- readRDS(paste0(cleandata_path, "/manifesto/manifesto_style.rds"))
insult <- readRDS(file.path(authorcreated_path, "Supply", "insultscores", paste0("insult_scores_", insult_variant, ".rds")))
leader <- readRDS(paste0(cleandata_path, "/leaders/party_leaders.rds"))
female_share <- readRDS(paste0(cleandata_path, "/ivs/ivs_femaleShare.RDS"))

# manifesto party data
final_data <- read_dta(pathWVS_Data_with_CZSK)
manifesto_populist <- readRDS(paste0(cleandata_path, "/manifesto/manifesto_populist.rds")) %>%
  filter(Y_manifesto_key %in% final_data$Y_manifesto_key) %>% 
  mutate(party_date = paste0(Y_manifesto_party_id, "_", date))

raw_data_manifesto <- readRDS(paste0(cleandata_path, "/manifesto/manifesto_populist.rds")) %>%
  mutate(party_date = paste0(Y_manifesto_party_id, "_", date))

# output directory:
outdir <- paste0(cleandata_path, "/manifesto/")

# *************************************************************************
# PROCESS ----
# *************************************************************************
extract_data <- function(country_name, party_name, df) {
  if (!"insult_score" %in% names(df)) {
    df <- df %>% mutate(insult_score = NA) 
  }
  
  df %>%
    mutate(
      country = country_name,
      party_id = substr(party_name, 1, 5), 
      date = substr(party_name, 7, 12) 
    )
}

insult_agg <- map_df(names(insult), function(country) {
  map_df(names(insult[[country]]), function(party) {
    extract_data(country, party, insult[[country]][[party]])
  })
}) %>% 
  mutate(party_date = paste0(party_id, "_", date)) %>% 
  group_by(party_date) %>%
  filter(insult_score >= quantile(insult_score, 0.95, na.rm = TRUE)) %>%
  summarise(top_insult_score = mean(insult_score, na.rm = TRUE))


# process style data
selected_columns <- c("mattr", "total_words", "average_words_clause")
data_pca <- style[, selected_columns] %>% 
  mutate(
    total_words = scale(total_words),
    average_words_clause = scale(average_words_clause))

pca_result <- prcomp(data_pca, center = TRUE, scale. = FALSE)
PC1 <- pca_result$x[, 1]

style <- style %>%
  mutate(PC1 = -PC1) %>%  # multiply by -1 to make higher value more complicated 
  select(party_date, total_words, average_words_clause, mattr, PC1)

# process leader data
set.seed(222)

leader <- leader %>%
  group_by(Y_manifesto_key) %>%
  mutate(
    main_leader = case_when(
      is.na(main_leader) ~ as.integer(row_number() == sample(1:n(), 1)),  # randomly choose a leader from dual-leadership
      TRUE ~ as.integer(main_leader))) %>%
  ungroup() %>% 
  filter(main_leader == 1) %>% 
  select(Y_manifesto_key, gender, education, age, occupation, marital_status, divorce, minority) %>% 
  mutate(divorce = ifelse(is.na(divorce) & !is.na(marital_status), 0L, as.integer(divorce)))

# process female shares
female_share <- female_share %>% 
  distinct(Y_manifesto_key, .keep_all = TRUE) %>% 
  select(Y_manifesto_key, femShare)

# combine data
df <- style %>% 
  left_join(insult_agg) %>% 
  left_join(raw_data_manifesto %>% 
              select(Y_manifesto_key, party_date)) %>% 
  left_join(leader, by = "Y_manifesto_key") %>% 
  left_join(female_share)

# imputation
set.seed(222)

missing_parties <- manifesto_populist %>%
  filter(!party_date %in% df$party_date) %>%
  mutate(top_insult_score = NA,
         total_words = NA,
         average_words_clause = NA,
         mattr = NA,
         PC1 = NA)

df_impute <- df %>% 
  inner_join(manifesto_populist %>% 
               select(-Y_manifesto_key), by="party_date") 

df_impute <- bind_rows(df_impute, missing_parties)

# Keep raw leader demographics so imputed values can be counted after imputation.
leader_demographic_vars <- c(
  "gender", "education", "age",
  "marital_status", "divorce", "minority",
  "femShare"
)

leader_demographics_before_imputation <- df_impute %>%
  select(Y_manifesto_key, all_of(leader_demographic_vars)) %>%
  distinct(Y_manifesto_key, .keep_all = TRUE)

predictors <- df_impute %>%
  select(starts_with("per"), 
         gender, education, age, occupation, marital_status, divorce, minority, femShare) %>%
  select(-pervote, -peruncod) %>% 
  as.data.frame()

predictors_extra <- df_impute %>%
  select(starts_with("per"), top_insult_score, total_words, average_words_clause, mattr, PC1,
         gender, education, age, occupation, marital_status, divorce, minority, femShare) %>%
  select(-pervote, -peruncod) %>% 
  as.data.frame()

imputed_data <- missForest(predictors, ntree = 75, maxiter = 2)$ximp %>% 
  cbind(Y_manifesto_key = df_impute$Y_manifesto_key) 

imputed_data_extra <- missForest(predictors_extra, ntree = 75, maxiter = 2)$ximp  %>% 
  select(top_insult_score, total_words, average_words_clause, mattr, PC1) %>% 
  cbind(Y_manifesto_key = df_impute$Y_manifesto_key)

imputed_data_all <- imputed_data %>% 
  inner_join(imputed_data_extra)

# save final data
df <- df %>% 
  left_join(raw_data_manifesto %>% 
              select(starts_with("per"), Y_manifesto_key) %>% 
              select(-pervote, -peruncod), by = "Y_manifesto_key") %>% 
  select(-party_date) %>% 
  select(Y_manifesto_key, starts_with("per"), top_insult_score,
         total_words, average_words_clause, mattr, PC1) %>% 
  filter(!Y_manifesto_key %in% imputed_data_all$Y_manifesto_key) %>% 
  bind_rows(imputed_data_all) %>% 
  left_join(raw_data_manifesto %>% 
              select(manifesto_country_name, date, Y_manifesto_party_id, manifesto_party_name, Y_manifesto_key)) %>% 
  select(manifesto_country_name, date, Y_manifesto_party_id, manifesto_party_name, Y_manifesto_key,
         starts_with("per"), top_insult_score, total_words, average_words_clause, mattr, PC1,
         gender, education, age, occupation, marital_status, divorce, minority, 
         femShare) %>% 
  set_variable_labels(
    top_insult_score = "Top insult score",
    total_words = "Total words",
    average_words_clause = "Average words per clause",
    mattr = "MATTR",
    PC1 = "First principal component of complexity",
    gender = "Gender",
    education = "Education level",
    age = "Age",
    occupation = "Occupation",
    marital_status = "Marital status",
    divorce = "Number of divorces",
    minority = "Minority status",
    femShare = "Share of female candidates")

saveRDS(df, manifesto_supply_variant_path)
if (is.null(style_variant)) {
  # Every reader of the unsuffixed file uses demographics only.
  # Rhetoric/text measures remain in manifesto_supply_google.Rds.
  saveRDS(df %>% select(-top_insult_score, -total_words, -average_words_clause, -mattr, -PC1),
          paste0(outdir, "manifesto_supply.Rds"))
}

# Compare the saved supply data to the pre-imputation leader demographics.
manifesto_supply <- readRDS(manifesto_supply_variant_path) %>%
  select(Y_manifesto_key, all_of(leader_demographic_vars)) %>%
  distinct(Y_manifesto_key, .keep_all = TRUE)

leader_imputation_comparison <- leader_demographics_before_imputation %>%
  inner_join(
    manifesto_supply,
    by = "Y_manifesto_key",
    suffix = c("_before", "_after")
  )

# Count leaders with any or all used demographics filled by imputation.
leader_imputation_comparison$imputed_demographics <- rowSums(
  is.na(leader_imputation_comparison[paste0(leader_demographic_vars, "_before")]) &
    !is.na(leader_imputation_comparison[paste0(leader_demographic_vars, "_after")])
)

leader_imputation_stats <- leader_imputation_comparison %>%
  summarise(
    impute_any_n = sum(imputed_demographics > 0),
    impute_all_n = sum(imputed_demographics == length(leader_demographic_vars)),
    impute_over_1 = sum(imputed_demographics > 1),
    impute_any_share = impute_any_n / n(),
    impute_all_share = impute_all_n / n(),
    impute_over_1_share = impute_over_1 / n(),
    .groups = "drop"
  )

if (is.null(style_variant)) saveRDS(leader_imputation_stats, file.path(cleandata_path, "leaders", "imputation_summary.rds"))
