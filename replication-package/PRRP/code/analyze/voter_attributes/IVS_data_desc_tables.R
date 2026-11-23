# *************************************************************************
# Description ----
# *************************************************************************
# Process and clean Integrated Values Survey (IVS) data matched with 
# Comparative Manifestos Project (CMP) data.
# 
# Inputs:
# - ivs_selected_countries_with_manifesto.dta: Raw IVS voter data
# - pathWVS_Data: Global variable path for IVS final dataset
# - ivs_to_manifesto.dta: Mapping file between IVS and Manifesto keys
# - commonVars.R: Global project variables and paths
#
# Outputs:
# - Table A.1: IVS Data (dis_stat_table.tex): Descriptive stats of final IVS data by wave
# - Table A.2: IVS Data Matched with CMP (data_filtering.tex): Summary of data matching and filtering process
# - values.tex: LaTeX macros for matched observation percentages
# *************************************************************************

# *************************************************************************
# SETUP ----
# *************************************************************************

# Load core libraries
library(tidyverse)
library(haven)
library(scales)
library(kableExtra)
library(savetexvalue)

# Clear environment
rm(list = ls())

# Load external project variables
source("code/util/commonVars.R")

# Define output directory
out_dir_descriptive <- file.path(github_path, "output", "tables", "descriptive_stat")

# *************************************************************************
# Table Table A.1: IVS Data
# *************************************************************************

# Read and process IVS data for the descriptive table
ivs_final_summary <- read_dta(pathWVS_Data) %>%
  mutate(
    # Identify matched Populist Radical Right Parties (PRRP)
    matched_prrp = if_else(Y_FR_Pop == 1 & !is.na(Y_manifesto_key), 1, 0),
    
    # Standardize integrated IVS wave labels for the LaTeX table display.
    BB_Int_Wave = case_when(
      BB_Int_Wave == "Int Wave 4 (2005-2009)" ~ "2005--2009",
      BB_Int_Wave == "Int Wave 5 (2010-2014)" ~ "2011--2013",
      BB_Int_Wave == "Int Wave 6 (2017-2021)" ~ "2017--2020"
    )
  ) %>%
  # Aggregate metrics per wave
  group_by(BB_Int_Wave) %>%
  summarise(
    countries     = n_distinct(BB_Country),
    parties       = n_distinct(Y_Party_Vote_Long),
    n_prrp        = n_distinct(Y_Party_Vote_Long[matched_prrp == 1]),
    observations  = comma(n()),
    support_share = round(weighted.mean(matched_prrp, BB_Weight_CW, na.rm = TRUE), 2),
    .groups = "drop"
  )

# Define LaTeX table headers
col_names_a2 <- c("IVS Wave", "Countries", "Parties", "PRRP", "Observations", "PRRP \n Support Share")

# Generate and save Table A.1
tab_final_data <- kbl(ivs_final_summary,
                      booktabs = TRUE, 
                      align = "c",
                      format = "latex", 
                      escape = FALSE, 
                      linesep = "",
                      col.names = linebreak(col_names_a2, align = "c"))

save_kable(tab_final_data, file.path(out_dir_descriptive, "dis_stat_table.tex"), float = FALSE)

# Save the total number of IVS observations used in Table A.1
save_tex_value(
  values    = sum(parse_number(ivs_final_summary$observations)),
  names     = "IVSObvsNum",
  file_name = "output/values/values",
  path      = github_path,
  digits    = 0
)

# *************************************************************************
# Data Filtering and Matching Analysis ----
# *************************************************************************

# Get list of unique countries from final data
countries_list <- unique(read_dta(pathWVS_Data)$BB_Country)

# 1. Prepare Party-level data
df_parties <- read_dta(file.path(cleandata_path, "ivs", "ivs_to_manifesto.dta")) %>%
  filter(BB_Country %in% countries_list) %>%
  select(-c(countryPartyWave, manifesto_party_name, manifesto_country_name,
            partyYearManifesto, manifestoYear, manifestoWave, minDistParty)) %>%
  # Flag if the match is to the closest available election
  mutate(match_closest = if_else(distance == minDistElection, 1, 0))

# 2. Prepare Voter-level data
df_voter_raw <- read_dta(file.path(datasets_path, "ivs", "ivs_selected_countries_with_manifesto.dta")) %>%
  filter(BB_Country %in% countries_list)

# 3. Identify and drop parties with insufficient sample size (N < 10)
tab_matched_dropped <- df_voter_raw %>%
  group_by(Y_manifesto_key, BB_Wave_num) %>%
  filter(n() < 10, BB_Wave_num >= 4) %>%
  ungroup() %>%
  select(BB_Id) %>%
  mutate(drop_too_few = TRUE)

matched_dropped_count <- nrow(tab_matched_dropped)

# Merge drop-flag back to main voter data
df_voter <- df_voter_raw %>%
  left_join(tab_matched_dropped, by = "BB_Id") %>%
  mutate(drop_too_few = replace_na(drop_too_few, FALSE))

# 4. Calculate matching and filtering statistics
df_match_stats <- df_voter %>%
  # Explicit join to ensure all columns (like Y_manifesto_party_id) are present
  left_join(df_parties) %>% 
  filter(BB_Wave_num > 3) %>%
  summarise(
    respondents      = n(),
    answered_party   = sum(!is.na(BB_Party) & Y_No_Party_Vote == 0, na.rm = TRUE),
    matched          = sum(!is.na(Y_manifesto_key), na.rm = TRUE),
    matched_min_10   = sum(!is.na(Y_manifesto_key) & !drop_too_few, na.rm = TRUE),
    matched_closest  = sum(match_closest == 1, na.rm = TRUE),
    prrp_sup         = sum(Y_FR_Pop == 1, na.rm = TRUE),
    prrp_sup_matched = sum(Y_FR_Pop == 1 & !is.na(Y_manifesto_key), na.rm = TRUE),
    prrp_sup_min_10  = sum(Y_FR_Pop == 1 & !drop_too_few, na.rm = TRUE),

    # Using column names directly as they appear in the data
    unique_parties_matched        = n_distinct(Y_manifesto_party_id[!is.na(Y_manifesto_key)], na.rm = TRUE),
    unique_parties_matched_min_10 = n_distinct(Y_manifesto_party_id[!is.na(Y_manifesto_key) & !drop_too_few], na.rm = TRUE),
    unique_parties_with_manifesto = n_distinct(Y_manifesto_party_id, na.rm = TRUE),
    unique_parties_unmatched      = n_distinct(Y_Party_Vote_Long[is.na(Y_manifesto_party_id) & Y_Party_Vote_Long != "."], na.rm = TRUE),
    unique_prrp_matched           = n_distinct(Y_manifesto_party_id[!is.na(Y_manifesto_key) & Y_FR_Pop == 1], na.rm = TRUE),
    unique_prrp_min_10            = n_distinct(Y_manifesto_party_id[!is.na(Y_manifesto_key) & Y_FR_Pop == 1 & !drop_too_few], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    matched_share         = matched / answered_party,
    matched_closest_share = matched_closest / answered_party,
    unique_parties_total  = unique_parties_with_manifesto + unique_parties_unmatched
  )

# *******************************************************************************
# Save External Values and Filtering Table (Table A.2: IVS Data Matched with CMP)
# *******************************************************************************

# Save matching percentages as TeX macros
save_tex_value(
  values    = c(df_match_stats$matched_share, df_match_stats$matched_closest_share),
  names     = c("matchedPct", "matchedClosestPct"),
  percent   = TRUE,
  file_name = "output/values/values",
  path      = github_path
)

# Save count of dropped observations due to small sample size
save_tex_value(
  values    = matched_dropped_count, 
  names     = "matchedDropped", 
  file_name = "output/values/values",
  path      = github_path, 
  digits    = 0
)

# Construct Table A.2 (Data Filtering)
data_filtering_tab <- tribble(
  ~n_parties, ~n_prrp, ~obs, ~prrp_sup,
  NA,                                     NA,                                df_match_stats$respondents,     NA,
  df_match_stats$unique_parties_total,    NA,                                df_match_stats$answered_party,  NA,
  df_match_stats$unique_parties_matched,        df_match_stats$unique_prrp_matched, df_match_stats$matched,        df_match_stats$prrp_sup_matched,
  df_match_stats$unique_parties_matched_min_10, df_match_stats$unique_prrp_min_10,  df_match_stats$matched_min_10, df_match_stats$prrp_sup_min_10
) %>%
  # Format values for LaTeX (commas and handle NAs)
  mutate(across(everything(), ~ as.character(comma(.x))),
         across(everything(), ~ if_else(is.na(.x) | .x == "NA", ".", .x))) %>%
  as.data.frame()

# Set row and column names for Table A.4
rownames(data_filtering_tab) <- c("1) All data",
                                  "2) Respondents supporting a party",
                                  "3) Respondents matched with CMP within 5 years",
                                  "4) Final sample (parties with at least 10 supporters)")

col_names_a4 <- c('Unique \nParties', 'Unique \nPRRP', 'Observations', 'PRRP \nSupporters')

# Save Table A.2
tab_filtering_output <- kbl(data_filtering_tab,
                            booktabs = TRUE,
                            align = "c",
                            format = "latex",
                            escape = FALSE,
                            linesep = "",
                            col.names = linebreak(col_names_a4, align = "c"))

tab_filtering_latex <- sub("^\\n", "", as.character(tab_filtering_output))
writeLines(
  c("", "\\resizebox{\\textwidth}{!}{", tab_filtering_latex, "}"),
  file.path(out_dir_descriptive, "data_filtering.tex")
)
