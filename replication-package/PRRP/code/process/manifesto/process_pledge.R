# *************************************************************************
# Description ----
# *************************************************************************

# Process pledges data

# *************************************************************************
# SETUP ----
# *************************************************************************

rm(list = ls())

library(dplyr)
library(tidyr)
library(stringr)
library(haven)
library(data.table) 
library(kableExtra)
library(missForest)

# paths
source("code/util/commonVars.R")
source("code/analyze/party_positions/manifesto_general_functions.R")
source("code/process/manifesto/load_pledge_data.R")

Z <- readRDS(manifestoModelData) %>% 
  mutate(party_date = paste0(Y_manifesto_party_id, "_", date)) %>% 
  select(starts_with("per"), BB_Wave_num, party_date, 
         gender, education, age, occupation, marital_status, divorce, minority, femShare) %>% 
  select(-pervote) %>% 
  rename_with(~ paste0(., "_raw"), .cols = -c(party_date,BB_Wave_num))

# Use the local classified input or automatically combine public labels and corpus codes.
raw_pledges <- load_pledge_data(DB.folder) %>%
  mutate(party_date = paste0(party_id, "_", date))

pledge_share <- raw_pledges %>% 
  mutate(is_pledge = ifelse(content == 1, 1, 0)) %>% 
  summarise(share_pledge = mean(is_pledge, na.rm = TRUE)) %>% 
  pull(share_pledge)

pledges <- raw_pledges %>% 
  mutate(is_pledge = ifelse(content == 1, 1, 0)) %>% 
  mutate(cmp_code = str_extract(as.character(cmp_code), "^[^.]+")) %>% 
  filter(nchar(cmp_code) == 3) %>% 
  mutate(cmp_code = ifelse(cmp_code == "000", "peruncod", paste0("per", cmp_code))) %>%
  group_by(party_date) %>%
  mutate(total_pledges = sum(is_pledge)) %>%
  group_by(party_date, cmp_code) %>%
  summarize(is_pledge = sum(is_pledge, na.rm = TRUE), .groups = "drop") %>%
  arrange(cmp_code) %>%
  pivot_wider(
    names_from = cmp_code,
    values_from = is_pledge) %>% 
  replace(is.na(.), 0) %>% 
  mutate(total_pledges = rowSums(across(starts_with("per")))) %>% 
  mutate(across(starts_with("per"), ~ round(. *100/ total_pledges, 3))) %>%
  arrange(party_date) %>% 
  select(-total_pledges)


all_data <- Z %>% 
  left_join(pledges) %>% 
  mutate(across(everything(), ~ ifelse(is.nan(.), NA, .)))

set.seed(12345)
list_results=list()

for (wave in c(4, 5, 6)) {
  df <- all_data %>% 
    filter(BB_Wave_num == wave)
  
  predictors <- df %>%
    select(starts_with("per"), ends_with("raw")) %>%
    as.data.frame()
  
  imputed_data <- missForest(predictors, ntree = 75, maxiter = 2)$ximp
  
  imputed_data <- imputed_data / rowSums(imputed_data[ , !grepl("_raw$", colnames(imputed_data))]) *100
  
  imputed_data <- imputed_data %>% 
    cbind(party_date = df$party_date)
  
  list_results[[as.character(wave)]] <- imputed_data
}

pledges <- bind_rows(list_results) %>% 
  select(-ends_with("_raw"))

saveRDS(pledges, paste0(cleandata_path, "/manifesto/pledges.rds"))
