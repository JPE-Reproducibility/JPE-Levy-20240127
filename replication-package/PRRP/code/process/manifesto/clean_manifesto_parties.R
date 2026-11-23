# *************************************************************************
# SETUP ----
# *************************************************************************

rm(list = ls())

# libraries
library(tidyverse)
library(haven)
library(data.table)
library(conflicted)

# paths
source("code/util/commonVars.R")

PLEDGE <- file.exists("code/pledgeConfig")
STYLE <- file.exists("code/styleGoogleConfig")

data_manifesto_populist <- readRDS(paste0(cleandata_path, "/manifesto/manifesto_populist.rds"))

if (PLEDGE) {
  data_manifesto_populist <- data_manifesto_populist %>% 
    mutate(party_date = paste0(Y_manifesto_party_id, "_", date))
  
  pledges <- readRDS(paste0(cleandata_path, "/manifesto/pledges.rds"))
  
  labels <- sapply(data_manifesto_populist, function(col) attr(col, "label"), simplify = FALSE)
  
  for (col in colnames(pledges)) {
    if (startsWith(col, "per") && col %in% names(labels)) {
      attr(pledges[[col]], "label") <- labels[[col]]
    }
  }
  
  data_manifesto_populist <- data_manifesto_populist %>% 
    select(-starts_with("per"), pervote) 
  
  data_manifesto_populist <- pledges %>% 
    inner_join(data_manifesto_populist) %>% 
    select(-party_date)
}

if (STYLE) {
  NewZs <- readRDS(manifesto_supply_variant_path) %>%
    select(Y_manifesto_key, top_insult_score, total_words, average_words_clause, mattr,
           gender, education, age, occupation, marital_status, divorce, minority, femShare)
} else{
  NewZs <- readRDS(paste0(cleandata_path, "/manifesto/manifesto_supply.Rds")) %>%
    select(Y_manifesto_key, gender, education, age, occupation, marital_status, divorce, minority, femShare)
}

# Data without CZSK
final_data <- read_dta(pathWVS_Data) %>% 
  distinct(Y_manifesto_key, .keep_all = T) %>% 
  select(BB_Country, Y_Party_Vote_Long, BB_Wave_num, Y_manifesto_key)

data_manifesto <- data_manifesto_populist %>% 
  inner_join(final_data) %>% 
  inner_join(NewZs)

saveRDS(data_manifesto, file = manifestoModelData)

# Write the standalone CZSK variant only during baseline preparation.
if (!STYLE && !PLEDGE) {
  # Data with CZSK
  final_data <- read_dta(pathWVS_Data_with_CZSK) %>%
    distinct(Y_manifesto_key, .keep_all = T) %>%
    select(BB_Country, Y_Party_Vote_Long, BB_Wave_num, Y_manifesto_key)

  data_manifesto <- data_manifesto_populist %>%
    inner_join(final_data) %>%
    inner_join(NewZs)

  saveRDS(data_manifesto, file = manifestoModelData_withCZSK)
}

# Style robustness uses waves 4--6 only; leave the baseline 1--6 file untouched.
if (!STYLE && !PLEDGE) {
# The following data contains CZSK anyway
final_data_1to6 <- read_dta(file.path(datasets_path, "WVS", "wvs_clean_1to6_waves.dta")) %>%
  distinct(Y_manifesto_key, .keep_all = T) %>% 
  select(BB_Country, Y_Party_Vote_Long, BB_Wave_num, Y_manifesto_key)

data_manifesto_1to6 <- data_manifesto_populist %>% 
  inner_join(final_data_1to6)

saveRDS(data_manifesto_1to6, file = paste0(cleandata_path, "/manifesto/manifesto_parties_1to6.rds"))
}
