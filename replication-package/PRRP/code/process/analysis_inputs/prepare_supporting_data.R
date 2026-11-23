library(tidyverse)
library(haven)
library(data.table)
library(Hmisc)
source("code/util/commonVars.R")
source("code/process/paths.R")

source("code/process/analysis_inputs/leader_functions.R")
source_data <- data.table::fread(file.path(authorcreated_path, "MissingManifestos", "manifesto_texts.csv")) %>%
  mutate(party_id_year=as.character(party_id_year)) %>% select(party_id_year, source, type)
saveRDS(source_data, file.path(cleandata_path, "manifesto", "sources.rds"))
wvs <- read_dta(pathWVS_Data) %>% distinct(Y_manifesto_party_id, BB_Year, BB_Wave_num) %>%
  mutate(manifestoYear=as.numeric(paste0(Y_manifesto_party_id, BB_Year))) %>% rename(manifestoKey=Y_manifesto_party_id)
leaders <- readRDS(file.path(authorcreated_path, "Supply", "partyleaders", "party_leaders.rds")) %>%
  select(country, party_name, year, party_id, leader) %>%
  mutate(manifestoYear=as.numeric(paste0(party_id, year))) %>% rename(manifestoKey=party_id)
saveRDS(prepare_new_leader_flags(wvs, leaders), file.path(cleandata_path, "leaders", "leader_changes.rds"))
corpora <- readRDS(file.path(authorcreated_path, "Supply", "manifestotexts", "manifesto_texts.rds"))
texts <- lapply(corpora, function(country) lapply(country, function(x) dplyr::select(x, original_text)))
saveRDS(texts, file.path(cleandata_path, "manifesto", "rhetoric_texts.rds"))
for (variant in "google") {
  scores <- readRDS(file.path(authorcreated_path, "Supply", "insultscores", paste0("insult_scores_", variant, ".rds")))
  scores <- lapply(scores, function(country) lapply(country, function(x) dplyr::select(x, any_of("insult_score"))))
  dir.create(file.path(cleandata_path, "manifesto", "insultscores"), showWarnings=FALSE)
  saveRDS(scores, file.path(cleandata_path, "manifesto", "insultscores", paste0("insult_scores_", variant, ".rds")))
}
germany <- read_dta(file.path(raw_path, "not_share", "GESIS", "Politbarometer_ZA2391_v16-1-0.dta")) %>%
  filter(intyear >= 2005, intyear <= 2019) %>% select(intyear, v33, dwght, pwght)
dir.create(file.path(cleandata_path, "Germany"), showWarnings=FALSE)
saveRDS(germany, file.path(cleandata_path, "Germany", "politbarometer.rds"))
