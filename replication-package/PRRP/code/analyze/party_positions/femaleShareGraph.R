# *************************************************************************
# File description ----
# *************************************************************************
#
# This file analyzes demographic trends in party leadership by party family
# over time, using manifesto and party leader data. It examines how the
# composition of party leaderships has changed across election periods.
#
# Main goals:
#   (1) Track trends in party-family leader demographics:
#       - Share of male legislators
#       - Probability of having older white male leaders
#       - Probability of having non-establishment leaders
#   (2) Compare East/South vs West/North aggregates
#   (3) Measure within-party cumulative demographic changes
#
# Analysis includes:
#   - Vote-share-weighted party-family averages per election and country
#   - Five-year moving averages and country-averaged aggregates
#   - Separate aggregates for Eastern/Southern and Western/Northern Europe
#   - Demographic composition of party leadership across time periods
#
# Outputs:
#
# Figures (paper):
#   - Share of Male Legislators Over Time by Party Family
#   - Demographic Composition Over Time (3-panel: male share, older white male,
#     non-establishment leaders)
#   - Regional variants (all countries, East/South, West/North)
#
# *************************************************************************
# SETUP ----
# *************************************************************************

rm(list = ls())

library(haven)
library(dplyr)
library(tidyr)
library(ggplot2)
source("code/util/commonVars.R")
finalData <- readRDS(file.path(cleandata_path, "ivs", "ivs_femaleShare.RDS"))
source("code/analyze/party_positions/manifesto_general_functions.R")

# load data
raw_data_manifesto <- readRDS(paste0(cleandata_path, "/manifesto/manifesto_populist.rds"))

median_wage <- read_dta(file.path(datasets_path, "IPUMS", "wage_occ_2023.dta")) %>%
  select(occupation = occ, ln_wage) %>% 
  summarise(median_ln_wage = median(ln_wage, na.rm = TRUE)) %>%
  pull(median_ln_wage)

leaders_final <- readRDS(paste0(cleandata_path, "/leaders/party_leaders.rds")) %>%
  select(Y_manifesto_key, gender, education, age, occupation,
         marital_status, divorce, minority, main_leader) %>%   
  mutate(occupation = ifelse(occupation >= median_wage, 1, 0)) 

# Aggregate to the party level:
# for every party&year combination, 
# keep the female share value, party family, and year in original data source
df_party_country <- finalData %>%
  mutate(femShare = 100 - femShare) %>% 
  select(femShare, originFemaleYear, Y_manifesto_key) %>%
  distinct(Y_manifesto_key, .keep_all = TRUE) %>% 
  inner_join(leaders_final) %>% 
  mutate(
    older_white_male = if_else(
      age >= 50 & gender == 1 & minority == 0, 1, 0
    ),
    nonesIndex = if_else(
      education >= 15 & marital_status == 1 & divorce == 0 & occupation == 1, 0, 1))

df_party_country <- df_party_country %>% 
  inner_join(raw_data_manifesto, by = "Y_manifesto_key") %>% 
  select(manifesto_country_name, date, manifestoYear, Y_manifesto_party_id,
         Y_FR_Pop, parfam, pervote, femShare, originFemaleYear, older_white_male, nonesIndex) %>%
  mutate(BBF_Country = factor(manifesto_country_name)) %>%
  add_parfam() %>% 
  dplyr::filter(!is.na(pervote)) %>% 
  group_by(BBF_Country, date) %>% 
  mutate(pervote = pervote / sum(pervote)) %>% 
  ungroup() %>% 
  rename(vote_share = pervote) %>%
  filter(!is.na(BBF_parfam)) %>% 
  group_by(BBF_Country, date) %>% 
  mutate(vote_share = vote_share / sum(vote_share)) %>% 
  ungroup() %>% 
  group_by(BBF_Country, date, BBF_parfam, manifestoYear) %>%
  summarise(across(c(femShare, older_white_male, nonesIndex),
                   ~ weighted.mean(.x, vote_share, na.rm = TRUE))) %>%
  ungroup()

# calculate 5-years moving average in each country*parfam
# Add all years (including non-election years)
calculate_moving_average <- function(df_aggregate){
  df_aggregate <- expand_grid(
    manifestoYear = unique(df_aggregate$manifestoYear),
    BBF_parfam = unique(df_aggregate$BBF_parfam),
    BBF_Country = unique(df_aggregate$BBF_Country)) %>%
    left_join(df_aggregate) %>%
    select(!date) %>%
    group_by(BBF_Country, BBF_parfam) %>%
    arrange(manifestoYear) %>%
    # moving average
    mutate(across(c(femShare, older_white_male, nonesIndex),
                  ~ rolling_avg_pop(var = .x, years = manifestoYear,
                                    year_start = 1980,
                                    year_end = 2021, window = 5))) %>% 
    # drop duplicate years (2 election cycles would create 2 identical rows)
    distinct() %>% 
    # Take average across countries and make long
    group_by(manifestoYear, BBF_parfam) %>%
    summarise(across(c(femShare, older_white_male, nonesIndex), ~ mean(.x, na.rm = TRUE))) %>%
    ungroup() %>%
    # Prepare names of variables
    pivot_longer(cols = !c("manifestoYear", "BBF_parfam"),
                 names_to = "variable_original", values_to = "value") %>%
    rename(variable = variable_original) %>% 
    mutate(year = as.numeric(substr(as.character(manifestoYear), 1, 4)))
}

prepare_scope_data <- function(df_party_country, country_subset = NULL) {
  df_scope <- df_party_country
  if (!is.null(country_subset)) {
    df_scope <- df_scope %>%
      filter(as.character(BBF_Country) %in% country_subset)
  }

  calculate_moving_average(df_scope) %>%
    filter(year >= 2005 & year <= 2021)
}

df_aggregate <- prepare_scope_data(df_party_country)

# plot --------------------------------------------------------------------
# theme and graphics
theme_plot_common <- theme(strip.background = element_blank(),
                           strip.text.x = element_text(face = "bold"))
theme_paper <-  theme_classic(base_size = 12) +
  theme_plot_common
theme_slides <- theme_classic(base_size = 9) + 
  theme(legend.position = "bottom") +
  theme_plot_common

party_colors <- c("#D55E00","#E69F00","#CC79A7" , "#0072B2", "#009E73")
party_lines <- c("solid","dashed","dotted","longdash","dotdash")
party_shapes <-  c(15, 16, 17, 3, 8)

add_plot_details <- function(plot) {
  plot +
    geom_line() +
    geom_point() +
    scale_color_manual(name = "", values = party_colors) +
    scale_linetype_manual(name = "", values = party_lines) +
    scale_shape_manual(name = "", values = party_shapes)+
    xlab("Year")
}

p <- ggplot(df_aggregate %>% 
              filter(variable == "femShare"), 
            aes(x = manifestoYear, y = value, linetype = BBF_parfam, 
                color = BBF_parfam, shape = BBF_parfam)) +
  ylab("Share of Male Legislators") +
  xlab("")+
  scale_x_continuous(limits = c(2005, 2018), breaks = c(2005, 2010, 2015, 2018)) +
  labs(color = NULL, shape = NULL, linetype = NULL) 

p<- p %>% 
  add_plot_details()  +
  theme_paper + 
  theme(legend.key.size = unit(1.5, "cm"))

save_demo_weighted_all <- function(df_scope, file_suffix = "") {
  df_plot <- df_scope %>%
    mutate(
      variable = recode(variable,
                        femShare = "Share of Male Legislators",
                        older_white_male = "Probability of Having an Older White Man Leader",
                        nonesIndex = "Probability of Having a Non-establishment Leader"
      ),
      variable = factor(variable, levels = c(
        "Share of Male Legislators",
        "Probability of Having an Older White Man Leader",
        "Probability of Having a Non-establishment Leader"
      ))
    )

  p_all <- ggplot(df_plot,
                  aes(x = manifestoYear, y = value, linetype = BBF_parfam,
                      color = BBF_parfam, shape = BBF_parfam)) +
    ylab(" ") +
    xlab("") +
    scale_x_continuous(limits = c(2005, 2018), breaks = c(2005, 2010, 2015, 2018)) +
    labs(color = NULL, shape = NULL, linetype = NULL) +
    facet_wrap(~variable, nrow = 3, scales = "free")

  p_all <- p_all %>%
    add_plot_details() +
    theme_paper +
    theme(legend.key.size = unit(1.5, "cm"))

  ggsave(
    plot = p_all,
    filename = paste0("output/figures/femaleShare/demo_weighted_all", file_suffix, ".pdf"),
    width = figureWidth,
    height = 18,
    unit = "cm"
  )
}

plot_scopes <- list(
  list(file_suffix = "", country_subset = NULL)
)

for (scope_cfg in plot_scopes) {
  df_scope <- prepare_scope_data(df_party_country, scope_cfg$country_subset)
  save_demo_weighted_all(df_scope, scope_cfg$file_suffix)
}
