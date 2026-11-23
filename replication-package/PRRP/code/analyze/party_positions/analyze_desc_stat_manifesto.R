# *************************************************************************
# File description ----
# *************************************************************************
#
# This file creates descriptive statistics tables combining
# Comparative Manifesto Project (CMP) data and Integrated Value Survey (IVS/WVS).
#
# The main goal is to compare average positions of PRRP and non-PRRP parties
# across two manifesto waves (Wave 4 and Wave 6) on:
#   (1) Economic and cultural position indices
#   (2) The most distinctive manifesto positions
#   (3) Selected IVS/WVS opinion items
#
# Outputs:
#
# Table 1: CMP and IVS Descriptive Statistics
#   - Panel A: Comparative Manifesto Project
#       * CMP position indices (net shares)
#       * Top 5 most distinctive CMP positions
#   - Panel B: Integrated Value Survey
#       * Selected demographics
#       * Top 5 most distinctive opinions
#
# Additional outputs:
#   - RDS file with top 10 most distinctive CMP variables
#   - Slide-only LaTeX table with CMP indices + top 5 distinctive positions
#
#
# Clear the R workspace
rm(list = ls())

# *************************************************************************
# GLOBAL OPTIONS ----
# *************************************************************************

REFEREE_ONLY <- FALSE

# Define manifesto waves explicitly for transparency and reproducibility
WAVE_4_YEARS <- 2005:2009
WAVE_6_YEARS <- 2017:2020

# *************************************************************************
# SETUP ----
# *************************************************************************

library(dplyr)
library(haven)
library(tidyr)
library(tibble)
library(Hmisc)
library(kableExtra)

source("code/util/commonVars.R")

# Output directory for tables

# *************************************************************************
# LOAD DATA ----
# *************************************************************************

# CMP manifesto data (already restricted to populist-relevant parties)
raw_data_manifesto <- readRDS(paste0(cleandata_path, "/manifesto/manifesto_populist.rds"))

# List of countries appearing in the IVS/WVS data
countries_list <- read_dta(pathWVS_Data) %>%
  pull(BB_Country) %>%
  unique()

list2env(readRDS(file.path(cleandata_path, "manifesto/descriptive_summary.rds")), envir = environment())
# *************************************************************************
# PREPARE TABLE DATA ----
# *************************************************************************

df <- df %>%
  mutate(
    wave = as.factor(wave),
    Y_FR_Pop = as.factor(Y_FR_Pop)
  ) %>%
  unite("wave_outcome", wave:Y_FR_Pop, sep = "-") %>%
  mutate(
    wave_outcome = case_when(
      wave_outcome == "4-0" ~ "Wave_4_PRRP_not",
      wave_outcome == "4-1" ~ "Wave_4_PRRP",
      wave_outcome == "6-0" ~ "Wave_6_PRRP_not",
      wave_outcome == "6-1" ~ "Wave_6_PRRP",
      TRUE ~ wave_outcome
    )
  ) %>%
  # Attach descriptive labels to CMP variables
  left_join(manifesto_labels, by = "variable_name") %>%
  mutate(
    title = if_else(variable_name == "economy_index", econIndexName, title),
    title = if_else(variable_name == "society_index", societyIndexName, title),
    value = round(value, 1)
  ) %>%
  pivot_wider(
    id_cols = c(title, variable_name),
    names_from = wave_outcome,
    values_from = value,
    names_sort = TRUE
  )

# Subsets for table construction
df_index  <- filter(df, grepl("Index", title))
df_top_10 <- left_join(df_pick, df, by = "variable_name")
df_top_5  <- slice_head(df_top_10, n = 5)

# *************************************************************************
# TABLE 1: CMP AND IVS DESCRIPTIVE STATISTICS ----
# *************************************************************************

# Column names for LaTeX tables
colNames <- c("PRRP", "Other\nParties", "PRRP", "Other\nParties")

# CMP component (keep exactly as in original logic)
manifesto_top5 <- rbind(df_index, df_top_5) %>% 
  select(-variable_name) %>% 
  column_to_rownames("title")

# Leadership demographics component for the main descriptive table
# and the referee-response summary table. Keep this single source of
# truth so shared rows have identical labels and values.
leadership_demographics_table1_levels <- c(
  # "Male Share",
  "Years of Education",
  "Number of Divorces",
  "Share of Female Legislators"
)

leadership_demographics_levels <- c(
  "Male Share",
  leadership_demographics_table1_levels,
  "Age",
  "Occupation Wage (log)",
  "Married Share",
  "Minority Share",
  "Share of Female Legislators"
) %>% unique(fromLast = TRUE)

leadership_demographics_all <- readRDS(paste0(cleandata_path, "/manifesto/manifesto_supply.Rds")) %>%
  select(
    Y_manifesto_key,
    gender,
    education,
    age,
    occupation,
    marital_status,
    divorce,
    minority,
    femShare
  ) %>%
  distinct(Y_manifesto_key, .keep_all = TRUE) %>%
  inner_join(
    raw_data_manifesto %>%
      filter(manifesto_country_name %in% countries_list) %>%
      select(
        Y_manifesto_key,
        manifesto_country_name,
        manifestoYear,
        Y_FR_Pop,
        pervote
      ),
    by = "Y_manifesto_key"
  ) %>%
  mutate(
    wave = case_when(
      manifestoYear %in% WAVE_4_YEARS ~ 4,
      manifestoYear %in% WAVE_6_YEARS ~ 6,
      TRUE ~ NA_real_
    ),
    `Male Share` = gender,
    `Years of Education` = education,
    `Number of Divorces` = divorce,
    Age = age,
    `Occupation Wage (log)` = occupation,
    `Married Share` = marital_status,
    `Minority Share` = minority,
    `Share of Female Legislators` = femShare / 100
  ) %>%
  filter(!is.na(wave), !is.na(pervote)) %>%
  group_by(manifesto_country_name, wave) %>%
  mutate(weight = pervote / sum(pervote)) %>%
  ungroup() %>%
  pivot_longer(
    cols = all_of(leadership_demographics_levels),
    names_to = "title",
    values_to = "value"
  ) %>%
  group_by(Y_FR_Pop, wave, manifesto_country_name, title) %>%
  summarise(value = weighted.mean(value, weight, na.rm = TRUE), .groups = "drop") %>%
  group_by(Y_FR_Pop, wave, title) %>%
  summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    wave = as.factor(wave),
    Y_FR_Pop = as.factor(Y_FR_Pop)
  ) %>%
  unite("wave_outcome", wave:Y_FR_Pop, sep = "-") %>%
  mutate(
    wave_outcome = case_when(
      wave_outcome == "4-0" ~ "Wave_4_PRRP_not",
      wave_outcome == "4-1" ~ "Wave_4_PRRP",
      wave_outcome == "6-0" ~ "Wave_6_PRRP_not",
      wave_outcome == "6-1" ~ "Wave_6_PRRP",
      TRUE ~ wave_outcome
    )
  ) %>%
  mutate(value = round(value, 2)) %>%
  pivot_wider(
    id_cols = title,
    names_from = wave_outcome,
    values_from = value,
    names_sort = TRUE
  ) %>%
  mutate(
    title = factor(
      title,
      levels = leadership_demographics_levels
    )
  ) %>%
  arrange(title) %>%
  mutate(title = as.character(title))

leadership_demographics <- leadership_demographics_all %>%
  filter(title %in% leadership_demographics_table1_levels) %>%
  column_to_rownames("title")

tab_leader_demographics <- leadership_demographics_all %>%
  kbl(
    digits = 2,
    booktabs = TRUE,
    linesep = "",
    format = "latex",
    escape = FALSE,
    col.names = linebreak(c("", colNames)),
    align = c("l", rep("c", 4))
  ) %>%
  add_header_above(c("", "2005--2009" = 2, "2017--2020" = 2)) %>%
  gsub("\\\\midrule\\\\\\s*\\\\", "\\\\midrule", .)


# IVS/WVS component
wvs_top5 <- readRDS(paste0(cleandata_path, "/ivs/wvs_top5_vars.rds"))

# Harmonize row labels
current_row_names <- rownames(wvs_top5)
new_row_names <- gsub(
  x = current_row_names,
  pattern = "Don't want immigrant",
  replacement = "Do not want immigrant"
)
rownames(wvs_top5) <- new_row_names

# Harmonize column names for binding (must match CMP naming)
wvs_top5 <- wvs_top5 %>% 
  rename(
    Wave_4_PRRP     = RRP_4,
    Wave_4_PRRP_not = Other_4,
    Wave_6_PRRP     = RRP_6,
    Wave_6_PRRP_not = Other_6
  )


top5_manifesto_wvs <- bind_rows(manifesto_top5, leadership_demographics, wvs_top5) %>% 
  kbl(
    booktabs = TRUE,
    align = "c",
    digits = 2,
    format = "latex",
    escape = FALSE,
    linesep = c("", "", "\\addlinespace", rep("", 13)),
    col.names = linebreak(colNames)
  ) %>%
  add_header_above(c("", "2005--2009" = 2, "2017--2020" = 2)) %>%
  pack_rows("Panel A: Party Positions (CMP) and Demographics", 1, 10,
            latex_align = "c", hline_before = TRUE, hline_after = TRUE) %>% 
  pack_rows("CMP Position Indices (Net Share in %)", 1, 2) %>% 
  pack_rows("Most Distinctive Positions (Share in %)", 3, 7) %>% 
  pack_rows("Distinctive Leadership Demographics", 8, 10) %>%
  pack_rows("Panel B: Integrated Values Survey", 11, 20,
            latex_align = "c", hline_before = TRUE, hline_after = TRUE) %>% 
  pack_rows("Demographics", 11, 15) %>%
  pack_rows("Most Distinctive Opinions (SD from Mean)", 16, 20)

save_kable(top5_manifesto_wvs, "output/tables/top5_manifesto_wvs.tex")
