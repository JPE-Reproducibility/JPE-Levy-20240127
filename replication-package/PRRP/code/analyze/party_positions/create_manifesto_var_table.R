# *************************************************************************
# Description ----
# *************************************************************************
# This file creates LaTeX tables describing CMP (Manifesto) variables and
# their inclusion in different ideological indices.
#
# Outputs:
# (1) Table B.1: CMP Indices (paper):
#     - All major (three-digit) CMP variables
#     - Short descriptions from the CMP codebook
#     - Signs in the CMP economic and society indices
#     - Signs in alternative indices (Bakker & Hobolt; Prosser)
#     - Expanded Hawk-Dove, EU, and Government efficiency indices
#
# (2) Slides' tables:
#     - Economic index variables
#     - Social (culture) index variables
#
# *************************************************************************
# SETUP ----
# *************************************************************************

rm(list = ls())

library(dplyr)
library(kableExtra)


# Inputs:
# - Index definitions (hawk-dove, EU, government): code/util/commonVars.R
# - Alternative indices (Prosser 2014; Bakker & Hobolt 2013):
#   code/manifesto/alternative_indices.R

source("code/util/commonVars.R")

# *************************************************************************
# Data prep ----
# *************************************************************************

# Read raw description from CMP codebook
manifestoVariables <- readRDS(file.path(cleandata_path, "metadata", "manifesto_indices.rds"))

# Keep only main CMP categories (exclude sub-categories and code 0) and retain fields needed for index definition and table output
manifestoShort <- manifestoVariables %>%
  dplyr::filter(type == "main" & code != 0) %>%
  select(
    title, variable_name, rile_index, description_md,
    PRRP_index, economy_index, society_index
  )

# Sanity check: each CMP variable should appear only once
stopifnot(!any(duplicated(manifestoShort$variable_name)))

# -------------------------------------------------------------------------
# Create short description
# -------------------------------------------------------------------------
# Normalize Unicode punctuation before the final ASCII conversion.

manifestoShort <- manifestoShort %>% 
  mutate(
    shortDescription = gsub("[\u2018\u2019]", "'", description_md),
    shortDescription = gsub("\\..*", "", shortDescription),
    shortDescription = gsub("\\n", " ", shortDescription),
    shortDescription = gsub("[\u201c\u201d]", '"', shortDescription),
    shortDescription = gsub("[\u2013\u2014]", "-", shortDescription)
  ) %>% 
  # Remove parenthesis
  mutate(
    shortDescription = gsub(" \\(.*\\)", "", shortDescription),
    shortDescription = gsub(" \\(.*", "", shortDescription)
  ) %>% 
  # Remove reference to specific code
  mutate(
    shortDescription = gsub("101", "Foreign Special Relationships", shortDescription),
    shortDescription = gsub(" ' ", " - ", shortDescription),
    shortDescription = iconv(shortDescription, "UTF-8", "ASCII", sub = "")
  )

# -------------------------------------------------------------------------
# Add additional indices
# -------------------------------------------------------------------------

manifestoShort <- manifestoShort %>% 
  mutate(
    hawk_dove_ex_index = case_when(
      variable_name %in% positive_hawk_dove_expanded_vars ~ "+",
      variable_name %in% negative_hawk_dove_expanded_vars ~ "-",
      TRUE ~ ""
    ),
    eu_index = case_when(
      variable_name %in% positive_eu_vars ~ "+",
      variable_name %in% negative_eu_vars ~ "-",
      TRUE ~ ""
    ),
    gov_index = case_when(
      variable_name %in% gov_vars ~ "+",
      TRUE ~ ""
    )
  )

# -------------------------------------------------------------------------
# Read alternative indices
# -------------------------------------------------------------------------
source("code/process/manifesto/alternative_indices.R")

manifestoShort <- manifestoShort %>%
  mutate(
    # Economic alternative indices
    econ_prosser = assign_index_sign(variable_name, econ_prosser_positive, econ_prosser_negative),
    econ_bak_hob = assign_index_sign(variable_name, econ_bak_hob_positive, econ_bak_hob_negative),
    
    # Social alternative indices
    soc_prosser  = assign_index_sign(variable_name, soc_prosser_positive,  soc_prosser_negative),
    soc_bak_hob  = assign_index_sign(variable_name, soc_bak_hob_positive,  soc_bak_hob_negative)
  )

# *************************************************************************
# Create the tables ----
# *************************************************************************

# -------------------------------------------------------------------------
# Table B.1 – Paper
# -------------------------------------------------------------------------

manifesto_table_latex <- manifestoShort %>%
  select(
    title, shortDescription,
    economy_index, society_index, eu_index,
    ends_with(c("bak_hob", "prosser")),
    hawk_dove_ex_index, gov_index
  ) %>%
  kable(
    format = "latex",
    booktabs = TRUE,
    longtable = TRUE,
    escape = FALSE,
    caption = "CMP Indices \\label{tab:manifesto_vars}",
    col.names = c(
      "Variable", "Description (from the CMP codebook)",
      "Econ.", "Soc.", "EU",
      "Econ.", "Lib.-Auth.",
      "Econ.", "Soc.",
      linebreak("Hawk-Dove\n(expanded)", align = "l"), "Gov."
    )
  ) %>%
  column_spec(1, width = "9em") %>%
  column_spec(2, width = "14em") %>%
  kable_styling(latex_options = c("striped", "hold_position"), font_size = 8) %>%
  add_header_above(
    c(
      " ", " ",
      "CMP" = 3,
      "Bakker and\nHobolt (2013)" = 2,
      "Prosser\n(2014)" = 2,
      "Kiratli\n(2021)" = 1,
      " "
    )
  ) %>%
  kable_styling(latex_options = "repeat_header")

# Tighten the inter-column padding (scoped inside the table's \begingroup group)
# so the wide table fits within the portrait text block.
manifesto_table_latex <- sub(
  "\\\\selectfont",
  "\\\\selectfont\n\\\\setlength{\\\\tabcolsep}{3pt}",
  manifesto_table_latex
)

cat(manifesto_table_latex, file = "output/tables/manifesto/manifestoVariablesExtended.tex")
