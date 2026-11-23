############################################################
# Robustness check: Internal consistency of rhetoric measures
# Demean by country averages, then correlate odd vs. even
############################################################
rm(list = ls())

library(dplyr)
library(purrr)
library(tidyr)
library(stringr)
library(quanteda)
library(knitr)
library(kableExtra)

# paths
source("code/util/commonVars.R")
source("code/analyze/party_positions/manifesto_general_functions.R")

############################################################
# 1. TOXICITY (INSULT SCORE)
############################################################

# Load sentence-level insult scores
insult <- readRDS(insult_scores_path)

# Convert nested list (country -> party_date) into one long data frame
tox_long <- map_df(names(insult), function(country) {
  map_df(names(insult[[country]]), function(party_date) {
    insult[[country]][[party_date]] %>%
      mutate(
        country = country,
        party_date = party_date
      )
  })
})

# Split each manifesto into 10 sections based on text order
tox_long <- tox_long %>%
  group_by(party_date) %>%
  mutate(
    text_order = row_number(),
    section = ntile(text_order, 10),
    parity = ifelse(section %% 2 == 0, "even", "odd")
  ) %>%
  ungroup()

# Aggregate toxicity at the manifesto level (odd / even), keeping country
tox_corr_country <- tox_long %>%
  group_by(country, party_date, parity) %>%
  summarise(
    toxicity = mean(insult_score, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = parity, values_from = toxicity)

# Demean odd and even toxicity by country averages
tox_corr_country_dm <- tox_corr_country %>%
  group_by(country) %>%
  mutate(
    odd_dm  = odd  - mean(odd,  na.rm = TRUE),
    even_dm = even - mean(even, na.rm = TRUE)
  ) %>%
  ungroup()

# Correlation using demeaned values
cor_toxicity_dm <- cor(
  tox_corr_country_dm$odd_dm,
  tox_corr_country_dm$even_dm,
  use = "complete.obs"
)

############################################################
# 2. TOXICITY – TOP 5% MOST INSULTING SENTENCES
############################################################

tox_corr_top5_country <- tox_long %>%
  group_by(country, party_date, parity) %>%
  filter(insult_score >= quantile(insult_score, 0.95, na.rm = TRUE)) %>%
  summarise(
    toxicity = mean(insult_score, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = parity, values_from = toxicity)

tox_corr_top5_country_dm <- tox_corr_top5_country %>%
  group_by(country) %>%
  mutate(
    odd_dm  = odd  - mean(odd,  na.rm = TRUE),
    even_dm = even - mean(even, na.rm = TRUE)
  ) %>%
  ungroup()

cor_toxicity_top5_dm <- cor(
  tox_corr_top5_country_dm$odd_dm,
  tox_corr_top5_country_dm$even_dm,
  use = "complete.obs"
)

############################################################
# 3. AVERAGE WORDS PER CLAUSE
############################################################

# Load clause-level manifesto texts
corpora <- readRDS(file.path(cleandata_path, "manifesto", "rhetoric_texts.rds"))

# Convert nested list (country -> party_date) into one long data frame
clause_long <- map_df(names(corpora), function(country) {
  map_df(names(corpora[[country]]), function(party_date) {
    corpora[[country]][[party_date]] %>%
      mutate(
        country = country,
        party_date = party_date
      )
  })
})

# Count number of words per clause
clause_long <- clause_long %>%
  filter(!is.na(original_text)) %>%
  mutate(
    words_per_clause = str_count(original_text, "\\S+")
  )

# Split each manifesto into 10 sections
clause_long <- clause_long %>%
  group_by(party_date) %>%
  mutate(
    text_order = row_number(),
    section = ntile(text_order, 10),
    parity = ifelse(section %% 2 == 0, "even", "odd")
  ) %>%
  ungroup()

# Aggregate average words per clause, keeping country
words_corr_country <- clause_long %>%
  group_by(country, party_date, parity) %>%
  summarise(
    avg_words_clause = mean(words_per_clause, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = parity, values_from = avg_words_clause)

# Demean by country averages
words_corr_country_dm <- words_corr_country %>%
  group_by(country) %>%
  mutate(
    odd_dm  = odd  - mean(odd,  na.rm = TRUE),
    even_dm = even - mean(even, na.rm = TRUE)
  ) %>%
  ungroup()

cor_avg_words_dm <- cor(
  words_corr_country_dm$odd_dm,
  words_corr_country_dm$even_dm,
  use = "complete.obs"
)

############################################################
# 4. LEXICAL DIVERSITY (MATTR)
############################################################

# Collapse text separately for odd and even sections
mattr_parity <- clause_long %>%
  group_by(country, party_date, parity) %>%
  summarise(
    text = paste(original_text, collapse = " "),
    .groups = "drop"
  )

# Tokenize once using the shared cleanup before applying rolling-window MATTR.
mattr_tokens <- tokens(
  mattr_parity$text,
  remove_numbers = TRUE,
  remove_punct = TRUE,
  remove_symbols = TRUE,
  remove_url = TRUE
) %>%
  tokens_tolower()

# Compute MATTR with a rolling window, preserving row order.
mattr_parity$mattr <- vapply(
  mattr_tokens,
  calculate_mattr,
  numeric(1),
  window = 200L
)

# Reshape MATTR to odd vs. even manifesto halves.
mattr_parity <- mattr_parity %>%
  select(country, party_date, parity, mattr) %>%
  pivot_wider(names_from = parity, values_from = mattr)

# Demean MATTR by country averages
mattr_corr_country_dm <- mattr_parity %>%
  group_by(country) %>%
  mutate(
    odd_dm  = odd  - mean(odd,  na.rm = TRUE),
    even_dm = even - mean(even, na.rm = TRUE)
  ) %>%
  ungroup()

cor_mattr_dm <- cor(
  mattr_corr_country_dm$odd_dm,
  mattr_corr_country_dm$even_dm,
  use = "complete.obs"
)

############################################################
# Summary of demeaned correlations
############################################################

results_demeaned <- tibble(
  Measure = c(
    "Insult score",
    "Toxicity Top 5% (insult score)",
    "Average words per clause",
    "Lexical diversity"
  ),
  Odd_Even_Correlation = c(
    cor_toxicity_dm,
    cor_toxicity_top5_dm,
    cor_avg_words_dm,
    cor_mattr_dm
  )
)

results_demeaned

############################################################
# Generate and save LaTeX TABULAR ONLY (no table environment)
############################################################

# Define output path
out_dir   <- "output/tables/manifesto/"
file_name <- "manifesto_rhetoric_consistency.tex"
full_path <- file.path(out_dir, file_name)

# Select and rename measures for the table
table_data <- results_demeaned %>%
  filter(Measure == "Toxicity Top 5% (insult score)") %>%   # keep top 5% toxicity
  bind_rows(
    results_demeaned %>%
      filter(Measure %in% c("Average words per clause", "Lexical diversity"))
  ) %>%
  mutate(
    Measure = c(
      "Insult score",
      "Average Words per Clause",
      "Lexical Diversity"
    )
  )

# Create LaTeX tabular (no caption, no label, no table)
tab_tex <- table_data %>%
  knitr::kable(
    format    = "latex",
    booktabs = TRUE,
    digits   = 3,
    col.names = c("Rhetorical Measure", "Split-Half Correlation"), # ($r$)
    escape   = FALSE,
    align    = c("l", "c")
  ) %>%
  as.character()

# Ensure the tabular format matches the Overleaf style: \begin{tabular}[t]{lc}
tab_tex <- tab_tex %>%
  stringr::str_replace("^\\\\begin\\{tabular\\}\\{", "\\\\begin{tabular}[t]{") %>%
  stringr::str_replace("\\{l c\\}", "{lc}")

# Write the tabular to a .tex file
cat(tab_tex, file = full_path)

############################################################
# Sanity checks and diagnostics for the DEMEANED analysis
############################################################

# 1. Check consistency of manifesto counts across all demeaned datasets
# All should match the number of unique manifestos in your raw data
nrow(tox_corr_country_dm)
nrow(tox_corr_top5_country_dm)
nrow(words_corr_country_dm)
nrow(mattr_corr_country_dm)

# 2. Check for NAs introduced during demeaning or aggregation
# High NA counts in 'odd_dm' or 'even_dm' might indicate countries with only 1 manifesto
cat("Missing values in demeaned toxicity (mean):", sum(is.na(tox_corr_country_dm$odd_dm)), "\n")
cat("Missing values in demeaned toxicity (top 5%):", sum(is.na(tox_corr_top5_country_dm$odd_dm)), "\n")
cat("Missing values in demeaned words per clause:", sum(is.na(words_corr_country_dm$odd_dm)), "\n")
cat("Missing values in demeaned MATTR:", sum(is.na(mattr_corr_country_dm$odd_dm)), "\n")

# 3. VERIFY DEMEANING: Mathematical property check
# The mean of demeaned values within each country MUST be zero (or very close to it due to float precision)
verify_zero <- mattr_corr_country_dm %>%
  group_by(country) %>%
  summarise(
    mean_odd = mean(odd_dm, na.rm = TRUE),
    mean_even = mean(even_dm, na.rm = TRUE)
  ) %>%
  summarise(
    avg_of_means_odd = mean(mean_odd, na.rm = TRUE),
    max_abs_deviation = max(abs(mean_odd), na.rm = TRUE)
  )

print("Verification: Do country-level means equal zero?")
print(verify_zero) # max_abs_deviation should be a tiny number (e.g., 1e-16)

# 4. Inspect distributions of demeaned values
# These should be centered around 0 and show the spread within countries
summary(tox_corr_country_dm$odd_dm)
summary(tox_corr_top5_country_dm$odd_dm)
summary(words_corr_country_dm$odd_dm)
summary(mattr_corr_country_dm$odd_dm)

# 5. Check for 'Single-Observation' countries
# Countries with only one manifesto will have demeaned values of 0, 
# which can artificially deflate/influence correlations.
country_counts <- tox_corr_country_dm %>%
  group_by(country) %>%
  count()

print(paste("Number of countries with only one manifesto:", sum(country_counts$n == 1)))

# 6. Robustness: Correlation for long manifestos only (Demeaned)
# We use the same 'eligible' list (manifestos >= 500 clauses) 
# to see if internal consistency holds for long texts after demeaning.
eligible <- clause_long %>% 
  group_by(party_date) %>% 
  count() %>% 
  filter(n >= 500) %>% 
  pull(party_date)

cor_words_long_dm <- cor(
  words_corr_country_dm$odd_dm[words_corr_country_dm$party_date %in% eligible],
  words_corr_country_dm$even_dm[words_corr_country_dm$party_date %in% eligible],
  use = "complete.obs"
)

cat("Demeaned Word Count Correlation (Long manifestos only):", round(cor_words_long_dm, 4), "\n")
