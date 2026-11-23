# *************************************************************************
# SETUP ----
# *************************************************************************

rm(list = ls())

# libraries
library(tidyverse)
library(haven)
library(kableExtra)
library(data.table)
library(conflicted)

# paths
source("code/util/commonVars.R")
source("code/process/paths.R")
source("code/analyze/party_positions/manifesto_general_functions.R")

conflicts_prefer(dplyr::if_else)

# datasets
# Manifesto clean is identical to manifesto but without old codes and irrelevant positions, after renaming vars and with Y_manifesto_key
manifesto_data  <- read_dta(file.path(cleandata_path, "manifesto", "manifesto.dta"))
wvs_data <- read_dta(pathWVS_Data)
populist_data <- read_dta(paste0(intermediate_data_path, "/popuList/PopuList_clean.dta"))

# *************************************************************************
# Use WVS for parties already identified
# *************************************************************************

# Choose only relevant wvs variables, remove parties that don't match with manifesto
wvs_reduced <- wvs_data %>%
  select(Y_Party_Vote_Long, Y_manifesto_party_id, Y_FR_Pop, Y_Populist, Y_Far_Right) %>%
  distinct() %>%
  dplyr::filter(!is.na(Y_manifesto_party_id))

# Check for different parties matched to the same manifesto id
dup_ids <- wvs_reduced %>%
  dplyr::filter(duplicated(Y_manifesto_party_id)) %>%
  pull(Y_manifesto_party_id)
dup_wvs <- wvs_reduced %>%
  dplyr::filter(Y_manifesto_party_id %in% dup_ids) %>%
  arrange(Y_manifesto_party_id)
# Make sure duplicated IDs don't affect populist classification
dup_wvs <- dup_wvs %>%
  group_by(Y_manifesto_party_id) %>%
  summarise(
    prrp_classifications = n_distinct(Y_FR_Pop),
    pop_classifications = n_distinct(Y_Populist)
  )

# Duplicated IDs indeed don't affect populist classification, drop Y_party_name_long

wvs_reduced <- wvs_reduced %>%
  select(-Y_Party_Vote_Long) %>%
  distinct()

# Merge WVS classifications into manifesto
manifesto_wvs <- left_join(manifesto_data, wvs_reduced, by = "Y_manifesto_party_id")

# Don't touch matched parties
manifesto_wvs_matched <- manifesto_wvs %>%
  dplyr::filter(!is.na(Y_FR_Pop))

# How much is matched?
n_parties_matched <- length(unique(manifesto_wvs_matched$Y_manifesto_party_id))
n_parties_total <- length(unique(manifesto_wvs$Y_manifesto_party_id))
prop_matched_parties <- n_parties_matched / n_parties_total
n_obs_matched <- nrow(manifesto_wvs_matched)
n_obs_total <- nrow(manifesto_wvs)
prop_matched_obs <- n_obs_matched / n_obs_total

# Separate unmatched parties
manifesto_wvs_unmatched <- manifesto_wvs %>%
  dplyr::filter(is.na(Y_FR_Pop))
unmatched_parties <- manifesto_wvs_unmatched %>%
  select(manifesto_country_name, manifesto_party_name, Y_manifesto_party_id) %>%
  distinct()

# *************************************************************************
# Use populist for parties not matched
# *************************************************************************

# Choose only relevant Populist variables
populist_data <- populist_data %>%
  rename(Y_manifesto_party_id = manifesto_id) %>% 
  rename(Y_Populist = temp_populist, Y_Far_Right = temp_farright) %>%
  select(Y_manifesto_party_id, Y_Populist, Y_Far_Right, parlgov_id, country_name, party_name_english) %>% 
  mutate(Y_FR_Pop = if_else(Y_Populist == 1 & Y_Far_Right == 1, 1, 0))

# Merge Populist classifications to unmatched parts of manifesto
manifesto_populist <- manifesto_wvs_unmatched %>%
  select(-Y_FR_Pop, -Y_Populist, -Y_Far_Right) %>%
  left_join(populist_data, by = "Y_manifesto_party_id") %>%
  select(-country_name, -party_name_english)

# Don't touch matched parties
manifesto_populist_matched <- manifesto_populist %>%
  dplyr::filter(!is.na(Y_FR_Pop))

# How much is matched (comparing to the whole manifesto dataset)?
n_parties_matched_populist <- length(unique(manifesto_populist_matched$Y_manifesto_party_id))
prop_matched_parties_populist <- n_parties_matched_populist / n_parties_total
n_obs_matched_populist <- nrow(manifesto_populist_matched)
prop_matched_obs_populist <- n_obs_matched_populist / n_obs_total

# How much is matched (comparing to the part of manifesto unmatched with wvs)?
prop_matched_parties_populist_no_wvs <- n_parties_matched_populist /
  (n_parties_total - n_parties_matched)
prop_matched_obs_populist_no_wvs <- n_obs_matched_populist /
  (n_obs_total - n_obs_matched)

# Separate unmatched parties
manifesto_populist_unmatched <- manifesto_populist %>%
  dplyr::filter(is.na(Y_FR_Pop))
unmatched_parties_populist <- manifesto_populist_unmatched %>%
  select(manifesto_country_name, manifesto_party_name, Y_manifesto_party_id) %>%
  distinct()

# *************************************************************************
# For unmatched parties use names in Populist data only
# *************************************************************************
# Crate a list of parties in the manifesto data, which are not matched with the PopuList data

temp_manifesto_populist_unmatched <- manifesto_populist_unmatched %>%
  group_by(manifesto_country_name, manifesto_party_name) %>%
  summarise(Count = n())

# Create a list of parties in the PopuList data
temp_list_populist <- populist_data %>%
  rename(manifesto_party_name = party_name_english) %>%
  rename(manifesto_country_name = country_name)

# Create a list of parties, which can be matched by the names of parties out of those not matched by the ID
list_match_byname <-
  left_join(temp_manifesto_populist_unmatched, temp_list_populist,
    by = c("manifesto_country_name", "manifesto_party_name"))
list_match_byname <- list_match_byname %>%
  dplyr::filter(Y_FR_Pop == 1)
list_match_byname <- list_match_byname %>%
  group_by(manifesto_country_name, manifesto_party_name, parlgov_id) %>%
  summarise(Count = n())

# To the extracted unmatched parties, add parlgov_id IN THE PopuList DATA of parties,
# to the manifesto data.
## Comment: Why parlgov_id? We have parlgov_id not-missing for the unmatched
# parties in populist data

manifesto_populist_unmatched <- manifesto_populist_unmatched %>%
  select(all_of(colnames(manifesto_data))) %>%
  mutate(parlgov_id = 0) %>%
  # Enough party remains from previous code - it is not found in this code due to
  # The different way its name is written in the two datasets
  mutate(
    parlgov_id = if_else(manifesto_country_name == "Portugal" &
      manifesto_party_name == "Enough", 2789, parlgov_id)
  )
for (i in 1:nrow(list_match_byname)) {
  manifesto_populist_unmatched <- manifesto_populist_unmatched %>%
    mutate(
      parlgov_id =
        if_else(manifesto_country_name == list_match_byname$manifesto_country_name[i] &
          manifesto_party_name == list_match_byname$manifesto_party_name[i],
        list_match_byname$parlgov_id[i], parlgov_id
        )
    )
}
# Merge using parlgov_id
manifesto_populist_unmatched <-
  left_join(manifesto_populist_unmatched, populist_data, by = "parlgov_id")

# Impute 0 for NA for populist, Y_Far_Right
manifesto_populist_unmatched <- manifesto_populist_unmatched %>%
  mutate(Y_Populist = if_else(is.na(Y_Populist), 0, Y_Populist)) %>%
  mutate(Y_Far_Right = if_else(is.na(Y_Far_Right), 0, Y_Far_Right)) %>% 
  mutate(Y_FR_Pop = if_else(is.na(Y_FR_Pop), 0, Y_FR_Pop)) 

# Prep to append matched and unmatched data
manifesto_populist_unmatched <- manifesto_populist_unmatched %>%
  select(-Y_manifesto_party_id.y, -country_name, -party_name_english) %>%
  rename(Y_manifesto_party_id = Y_manifesto_party_id.x) %>%
  select(colnames(manifesto_populist_matched))

# *************************************************************************
# Append all data
# *************************************************************************

# Append matched and unmatched data populist
data_manifesto_populist <- rbind(manifesto_populist_matched, manifesto_populist_unmatched) %>%
  select(-parlgov_id)

# manually classify populists in AU and NZ
# "Pauline Hanson's One Nation" in Australia and "New Zealand First" in New Zealand.
# Now i do not see One Nation in AU data in the manifesto
temp_list_manifesto_AU_NZ <- manifesto_data %>%
  dplyr::filter(manifesto_country_name == "Australia" | manifesto_country_name == "New Zealand")
data_manifesto_populist <- data_manifesto_populist %>%
  mutate(
    Y_Populist =
      if_else(manifesto_party_name == "New Zealand First Party", 1, Y_Populist)
  )

# Append with data matched to wvs
data_manifesto_populist <- data_manifesto_populist %>%
  select(colnames(manifesto_wvs_matched)) %>%
  rbind(manifesto_wvs_matched)

# Check if Fidesz is correctedly classified as populist
check <- data_manifesto_populist %>% dplyr::filter(Y_manifesto_party_id == 86421)

# Construct three variables need to be analyzed
data_manifesto_populist <- data_manifesto_populist %>%
  add_econsoc_index() %>%
  rename(economy_index = econ_index, society_index = soc_index) %>%
  mutate(euro_skp_index = per110 - per108)

# Check categorization of populist and Y_Far_Right
manifesto_wvs_matched %>%
  group_by(Y_Populist, Y_Far_Right) %>%
  summarise(n = n())
manifesto_populist_matched %>%
  group_by(Y_Populist,, Y_Far_Right)%>%
  summarise(n = n())
manifesto_populist_unmatched %>%
  group_by(Y_Populist,, Y_Far_Right)%>%
  summarise(n = n())
data_manifesto_populist %>%
  group_by(Y_Populist, Y_Far_Right)%>%
  summarise(n = n())

# Unused population column omitted.

# Save data file to be analyzed
saveRDS(data_manifesto_populist, file = paste0(cleandata_path, "/manifesto/manifesto_populist.rds"))
