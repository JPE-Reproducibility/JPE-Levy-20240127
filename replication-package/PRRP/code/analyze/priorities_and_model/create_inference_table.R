# ******************************************************************************
# Script Name: Inference Table Generation
# Description: This script loads decomposition models and bootstrap results, 
#              processes them, and generates a formatted LaTeX table:
#              Table A.4: Decomposition of the Rise in PRRP Support by Country
#
# Inputs:
#   1. datasets/model/dec_4_6.Rds       (Original decomposition data)
#   2. Bootstrap decompositions in output/modeldata/bootstrap/BS
#   3. code/util/commonVars.R                (Common variables/functions like 'percent')
#
# Outputs:
#   1. output/tables/InferenceTable.tex (Final LaTeX table, "Table A.4: Decomposition of the Rise in PRRP Support by Country")
# ******************************************************************************


library(tidyverse)
library(readr)
library(kableExtra)

rm(list=ls())

# Load common variables and settings
source("code/util/config.R")
source(file.path("code", "util", "commonVars.R"))
outdir <- file.path(github_path, "output", "tables")
options(scipen = 999) 

# ******************************************************************************
# 1. Process Main Decomposition Data
# ******************************************************************************
original_decomposition <- readRDS(file.path(decomposition_path, "dec_4_6.Rds"))

# Clean and rename columns
original_decomposition <- original_decomposition %>% 
  rename_all(~str_replace_all(., "w1\\.", "4_Wave_")) %>% 
  rename_all(~str_replace_all(., "w2\\.", "5_Wave_")) %>% 
  rename_all(~str_replace_all(., "w3\\.", "6_Wave_")) %>% 
  rename_all(~str_replace_all(., "_2", "_Parties")) %>% 
  rename_all(~str_replace_all(., "_1", "_Residauls")) %>% 
  rename_all(~str_replace_all(., "_3", "_Voters")) %>% 
  rename_all(~str_replace_all(., "_4", "_Priorities")) %>% 
  rename_all(~str_replace_all(., "4_Wave", "Wave_4")) %>% 
  rename_all(~str_replace_all(., "5_Wave", "Wave_5")) %>% 
  rename_all(~str_replace_all(., "6_Wave", "Wave_6")) %>% 
  select(-contains("_5_")) %>% 
  rownames_to_column("Country") %>% 
  select(-contains("_4_")) %>% 
  rename_all(~str_replace_all(., "Wave_6_", "")) %>% 
  rename(
    "Party Characteristics"  = Parties,
    "Voter Attributes" = Voters,
    "Voter Priorities" = Priorities
  )

# Calculate Row Sums before rounding, so Agg is the exact total (the Total row sums to 1)
original_decomposition$Agg <- rowSums(original_decomposition[, 2:5])
original_decomposition <- original_decomposition %>%
  mutate(across(-Country, .fns = ~round(.x, 3)))

# Handle 'Total' row formatting
original_decomposition[17, 1] <- "ZTotal"

original_decomposition <- original_decomposition %>%
  mutate(
    `Party Characteristics`  = ifelse(Country == "Total", percent(`Party Characteristics`), `Party Characteristics`),
    Residauls          = ifelse(Country == "Total", percent(Residauls), Residauls), # Typo preserved
    `Voter Attributes` = ifelse(Country == "Total", percent(`Voter Attributes`), `Voter Attributes`),
    `Voter Priorities` = ifelse(Country == "Total", percent(`Voter Priorities`), `Voter Priorities`),
    Agg                = ifelse(Country == "Total", percent(Agg, 1), Agg)
  ) 

# ******************************************************************************
# 2. Process Bootstrap Standard Errors (SE)
# ******************************************************************************

bs.folder <- paste0(bootstrap_path, "/BS/")

# Load the bootstrap summary from the selected folder.
source("code/model/bootstrap_analysis.R")
se_df <- bootstrap_summary(bs.folder)
se_df <- se_df %>% rownames_to_column("var")

se_df <- se_df %>% 
  pivot_longer(
    cols = -var,
    names_to = "Country",
    values_to = "values"
  ) %>% 
  pivot_wider(
    names_from = var,
    values_from = values
  )

se_df <- se_df %>% 
  select(Country, ends_with(".sd")) %>%
  mutate(across(is.numeric, .fns = ~round(.x, 3))) %>% 
  mutate(Agg = "") %>% 
  mutate(across(is.numeric, .fns = ~paste0("(", .x, ")"))) %>% 
  rename(
    "Residauls"        = residual.sd, # Typo preserved
    "Party Characteristics"  = Z.sd,
    "Voter Attributes" = X.sd,
    "Voter Priorities" = param.sd
  )

# Set 'Total' row name
se_df[nrow(se_df), 1] <- "ZTotal"

# ******************************************************************************
# 3. Combine and Format Table
# ******************************************************************************

# Convert numerics to character with 3 decimal places
original_decomposition_chr <- original_decomposition %>%
  mutate(across(where(is.numeric), ~sprintf("%.3f", .x)))

dec_4_to_6 <- bind_rows(original_decomposition_chr, se_df)

# Reorder columns: Residuals | Parties | Voters | Priorities
dec_4_to_6 <- dec_4_to_6[, c(1, 5, 4, 3, 2, 6)]

# Function to handle repeated row labels
collapse_rows_df <- function(df, variable){
  group_var <- enquo(variable)
  
  df %>%
    group_by(!! group_var) %>%
    mutate(groupRow = 1:n()) %>%
    ungroup() %>%
    mutate(!!quo_name(group_var) := ifelse(groupRow == 1, as.character(!! group_var), "")) %>%
    select(-c(groupRow))
}

dec_4_to_6 <- dec_4_to_6 %>% 
  arrange(Country) %>% 
  collapse_rows_df(Country) %>% 
  mutate(Country = ifelse(Country == "ZTotal", "Total (0-1 scale)", Country))

# ******************************************************************************
# 4. Generate and Save LaTeX Table
# ******************************************************************************
dec_4_to_6_table <- kbl(
  dec_4_to_6, 
  format = "latex", 
  digits = 4,
  col.names = c("", " Voter Priorities", "Voter Attributes", "Party Characteristics", "Residuals", "Agg."),
  align = "c", 
  booktabs = TRUE, 
  linesep = c('', '\\addlinespace')
) %>% 
  row_spec(0, bold = T, extra_css = "border-bottom: 1px solid;") %>%
  row_spec(nrow(dec_4_to_6) - 2, hline_after = T) %>% 
  row_spec(nrow(dec_4_to_6) - 2, hline_after = T)

save_kable(dec_4_to_6_table, file = file.path(outdir, "InferenceTable.tex"))

# Insert column numbers (1)-(5) above the column titles, matching the
# Columns (1)-(4)/(5) references in the table's notes in main.tex.
table_path <- file.path(outdir, "InferenceTable.tex")
table_lines <- readLines(table_path)
toprule_line <- grep("^\\\\toprule", table_lines)
number_row <- " & (1) & (2) & (3) & (4) & (5)\\\\"
table_lines <- append(table_lines, number_row, after = toprule_line)
writeLines(table_lines, table_path)
