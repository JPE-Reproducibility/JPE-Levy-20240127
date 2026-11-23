# *************************************************************************
# Description ----
# *************************************************************************

# Clean table for party leader codebook and summary statistics

# *************************************************************************
# SETUP ----
# *************************************************************************
rm(list = ls())

library(dplyr)
library(tidyr)
library(kableExtra)
library(tibble)
library(purrr)
library(haven)

# paths
source("code/util/commonVars.R")

# *************************************************************************
# Data prep ----
# *************************************************************************

# load data
leaders_final <- readRDS(paste0(cleandata_path, "/leaders/party_leaders.rds")) %>%
  select(Y_manifesto_key, gender, education, age, occupation, marital_status, 
         divorce, minority, criminal_record, main_leader)

# process leader data
set.seed(222)

leaders_final <- leaders_final %>%
  group_by(Y_manifesto_key) %>%
  mutate(
    main_leader = case_when(
      is.na(main_leader) ~ as.integer(row_number() == sample(1:n(), 1)),  # randomly choose a leader from dual-leadership
      TRUE ~ as.integer(main_leader))) %>%
  ungroup() %>% 
  filter(main_leader == 1) %>% 
  select(-main_leader)

style <- readRDS(paste0(cleandata_path, "/manifesto/manifesto_style.rds"))
insult <- readRDS(insult_scores_path)
female_share <- readRDS(paste0(cleandata_path, "/ivs/ivs_femaleShare.RDS"))

final_data <- read_dta(pathWVS_Data)

manifesto_populist <- readRDS(paste0(cleandata_path, "/manifesto/manifesto_populist.rds")) %>%
  mutate(party_date = paste0(Y_manifesto_party_id, "_", date)) %>% 
  filter(Y_manifesto_key %in% final_data$Y_manifesto_key) %>% 
  select(Y_manifesto_key, Y_FR_Pop, manifestoYear, party_date) %>% 
  mutate(wave = case_when(
    manifestoYear >= 2005 & manifestoYear <= 2009 ~ 4,
    manifestoYear >= 2017 & manifestoYear <= 2020 ~ 6,
    TRUE ~ NA_real_))

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

female_share <- female_share %>% 
  distinct(Y_manifesto_key, .keep_all = TRUE) %>% 
  select(Y_manifesto_key, femShare)

# process data
df <- leaders_final %>% 
  left_join(manifesto_populist %>% 
              select(Y_manifesto_key, party_date, wave, Y_FR_Pop)) %>% 
  left_join(style) %>% 
  left_join(female_share) %>% 
  left_join(insult_agg) %>% 
  select(gender, education, age, occupation, marital_status, 
         divorce, minority, femShare, 
         top_insult_score, total_words, average_words_clause, mattr = mattr_raw) %>% 
  colnames() %>% 
  tibble(Variable = .) %>% 
  mutate(Description = NA, `Coding and notes` = NA)

df  <- df  %>% 
  mutate(
    Description = case_when(
      Variable == "gender" ~ "Party leader is a man",
      Variable == "education" ~ "Years of schooling of party leader by the date of election",
      Variable == "age" ~ "Age of party leader by the date of election",
      Variable == "occupation" ~ "Natural logarithm of average weekly wage in the original occupation of party leader before entering politics",
      Variable == "marital_status" ~ "Party leader is married",
      Variable == "divorce" ~ "Number of divorces of party leader by the date of election",
      Variable == "minority" ~ "An indicator for whether the party leader is immigrant, non-Christian, non-white, or non-heterosexual",
      Variable == "femShare" ~ "Proportion of women in parliament",
      Variable == "top_insult_score" ~ "Average insult score of the top 5\\% most insulting clauses in the manifesto",
      Variable == "total_words" ~ "Total number of words in the manifesto",
      Variable == "average_words_clause" ~ "Average number of words per clause in the manifesto",
      Variable == "mattr" ~ "Lexical diversity of the manifesto measured by moving-average type-token ratio"),
    `Coding and notes` = case_when(
      Variable == "gender" ~ "1 = Man, 0 = Woman",
      Variable == "education" ~ "6 - Elementary School or equivalent, 8 - Middle School or equivalent, 12 - High School or equivalent, 15 - Bachelor or equivalent, 17 - Master or equivalent, 20 - PhD or equivalent",
      Variable == "age" ~ "",
      Variable == "occupation" ~ "Natural logarithm of average weekly wage based on 2023 American Community Survey",
      Variable == "marital_status" ~ "1 = Married, 0 = Single",
      Variable == "children" ~ "",
      Variable == "divorce" ~ "",
      Variable == "minority" ~ "1 = Minority, 0 = All other leaders",
      Variable == "femShare" ~ "",
      Variable == "top_insult_score" ~ "Score between 0 and 1 calculated from the top 5\\% most insulting clauses. A score of 0.9 means that 90 out of 100 respondents consider a clause insulting",
      Variable == "total_words" ~ "",
      Variable == "average_words_clause" ~ "",
      Variable == "mattr" ~ ""),
    Variable = case_when(
      Variable == "gender" ~ "Man",
      Variable == "education" ~ "Years of schooling",
      Variable == "age" ~ "Age",
      Variable == "occupation" ~ "Occupation wage (log)",
      Variable == "marital_status" ~ "Married",
      Variable == "divorce" ~ "Number of divorces",
      Variable == "minority" ~ "Minority",
      Variable == "femShare" ~ "Share of women legislators",
      Variable == "top_insult_score" ~ "Insult score",
      Variable == "total_words" ~ "Total words (1000s)",
      Variable == "average_words_clause" ~ "Average words per clause",
      Variable == "mattr" ~ "Lexical diversity"))

df  %>% kable(format = "latex", booktabs = TRUE, longtable = TRUE, escape = F,
                  col.names = c("Variable", "Description", "Coding and notes"),
              caption = "Leadership Demographics and Manifesto Rhetoric \\label{tab:leader_code_book}") %>%
  kable_styling(latex_options = c("striped", "hold_position"), font_size = 8) %>%
  column_spec(1, width = "3cm") %>%
  column_spec(2, width = "7cm") %>%
  column_spec(3, width = "5cm") %>%
  gsub("\\\\addlinespace\\s*", "", .) %>% 
  cat(file = paste0("output/tables/party_leaders_code.tex"))

# *************************************************************************
# Manifesto rhetoric sum stats ----
# *************************************************************************

sum_stats_supply <- style %>% 
  left_join(insult_agg) %>% 
  left_join(manifesto_populist %>% 
              select(Y_manifesto_key, party_date, wave, Y_FR_Pop)) %>% 
  left_join(female_share) %>% 
  filter(Y_manifesto_key %in% final_data$Y_manifesto_key) %>% 
  filter(!is.na(wave)) %>% 
  select(wave, Y_FR_Pop, top_insult_score, total_words, average_words_clause, mattr = mattr_raw, femShare) %>% 
  mutate(total_words = total_words/1000) %>% 
  rename(
    `Insult score` = top_insult_score,
    `Total words (1000s)` = total_words,
    `Average words per clause` = average_words_clause,
    `Lexical diversity` = mattr,
    `Share of female legislators` = femShare) %>% 
  group_by(wave, Y_FR_Pop) %>%
  summarise(across(c(`Insult score`, `Total words (1000s)`, `Average words per clause`, `Lexical diversity`, `Share of female legislators`),
                   ~ mean(., na.rm = TRUE),
                   .names = "{.col}"),
            .groups = "drop") %>%
  pivot_longer(-c(wave, Y_FR_Pop), names_to = "Variable", values_to = "Mean") %>%
  unite("Group", wave, Y_FR_Pop, sep = "_", remove = T) %>%
  pivot_wider(names_from = Group, values_from = Mean) %>% 
  select(Variable, `4_1`, `4_0`, `6_1`, `6_0`)

colNames <- c("", "PRRP", "Other\nParties", "PRRP", "Other\nParties")

tab_rhetoric <- kbl(
  sum_stats_supply %>% slice(1:4),
  digits = 2,
  booktabs = T,
  linesep = "",
  "latex",
  escape = FALSE,
  col.names = linebreak(colNames),
  align = c("l", rep("c", 4))
) %>% 
  add_header_above(c("", "2005--2009" = 2, "2017--2020" = 2)) %>% 
  # row_spec(0, extra_latex_after = "\\midrule \\multicolumn{5}{l}{Manifesto Rhetoric} \\\\") %>% 
  gsub("\\\\midrule\\\\\\s*\\\\", "\\\\midrule", .)

save_kable(tab_rhetoric, "output/tables/manifesto_rhetoric_sum_stats.tex")

# Report imputation summaries prepared with the clean leader data.
leader_imputation_stats <- readRDS(file.path(cleandata_path, "leaders", "imputation_summary.rds"))
# Save leader demographic imputation counts and shares as TeX macros.
if (is.null(style_variant)) {
save_tex_value(
  values    = c(leader_imputation_stats$impute_any_n, leader_imputation_stats$impute_all_n, leader_imputation_stats$impute_over_1),
  names     = c("leaderDemoImputedAnyNum", "leaderDemoImputedAllNum", "leaderDemoImputedOverOneNum"),
  file_name = "output/values/values",
  path      = github_path,
  digits    = 0
)

save_tex_value(
  values    = leader_imputation_stats$impute_over_1_share,
  names     = "leaderDemoImputedOverOnePct",
  percent   = TRUE,
  file_name = "output/values/values",
  path      = github_path
)
}
