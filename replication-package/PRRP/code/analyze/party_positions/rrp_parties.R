# *************************************************************************
# File description ----
# *************************************************************************
#
# This file creates a summary table listing all Populist Radical Right Parties (PRRP)
# included in the analysis, organized by country.
#
# The table is constructed by:
#   - Identifying all parties with Y_FR_Pop == 1 (PRRP classification)
#   - Grouping parties by country
#   - Creating a comprehensive list for each country
#
# Outputs:
#
# Table A.3: PRRP Parties by Country
#   - Lists all PRRP parties included in the analysis
#   - Organized by country
#   - Used in appendix for documentation
#
#
# Clear the R workspace
rm(list=ls())

# *************************************************************************
# SETUP ----
# *************************************************************************
library(haven)
library(tidyverse)
library(kableExtra)
source("code/util/commonVars.R")
source("code/analyze/party_positions/manifesto_general_functions.R")


WVS <- read_dta(file = pathWVS_Data)

# *************************************************************************
# CLASSIFY TO FAMILIES AND SELECT PARTIES FOR GRAPH ----
# *************************************************************************

TABLE <- WVS %>%
  filter(Y_FR_Pop == 1 & !Y_No_Party_Vote) %>%
  distinct(BB_Country, Y_Party_Vote_Long) %>% 
  mutate(Y_Party_Vote_Long = gsub(".*: ", "", Y_Party_Vote_Long)) %>% 
  mutate(
    Y_Party_Vote_Long = if_else(
      Y_Party_Vote_Long == "New Slovenian Christian Peoples Party",
      "New Slovenian Christian People's Party",
      Y_Party_Vote_Long
    )
  ) %>%
  mutate(rrp_party = Y_Party_Vote_Long) %>% 
  select(BB_Country, rrp_party) %>% 
  group_by(BB_Country) %>% 
  summarise(rrp_party = str_c(rrp_party, collapse=", "), .groups = 'drop')


tab_final <- kable(TABLE, 
                   booktabs = T, align = c("l"), "latex", escape = FALSE, linesep = "",
                   col.names = linebreak(c("Country", "Party"), align = "c",)) %>%
  column_spec(2, width = "30em")


save_kable(tab_final, paste0(github_path,"output/tables/descriptive_stat/prrp_parties.tex"))
