# *************************************************************************
# Description ----
# *************************************************************************
# This file creates a LaTeX table of IVS variables with:
# - Variable (name)
# - Description
# - Coding and notes
# - Index (economic / cultural classification)
#
# Notes: This table presents all IVS variables included in our data. The third column provides information on
# how variables were processed. The fourth columns show which variables are part of the economic and culture
# opinions indices used in Figure 8 and Appendix Figure A.9.
#
# Output:
#   Table B.3: IVS Variables
#   output/tables/ivsVariables.tex

# *************************************************************************
# SETUP ----
# *************************************************************************

rm(list=ls())
library(dplyr)
library(kableExtra)

source("code/util/commonVars.R")


# *************************************************************************
# Data prep ----
# *************************************************************************
      
# Read the data and select relevant variables

wvsVariables <- readRDS(file.path(cleandata_path, "metadata", "wvs_variables.rds"))
WVS <- haven::read_dta(pathWVS_Data)

wvsVariables <- wvsVariables %>%
  rename(
    topic          = Question.Topic,
    name           = Variable.Name,
    nameForPlot    = Plot.Name,
    questionNum    = Question.num,
    dataType       = Type.of.data,
    question       = Question,
    codingAndNotes = Question.coding.and.note,
    wave1          = Int_Wave_1__1989_1993_,
    wave2          = Int_Wave_2__1994_1998_,
    wave3          = Int_Wave_3__1999_2004_,
    wave4          = Int_Wave_4__2005_2009_,
    wave5          = Int_Wave_5__2010_2014_,
    wave6          = Int_Wave_6__2017_2021_
  )

wvsVariables <- wvsVariables %>% 
  dplyr::filter(name %in% names(WVS))

# Check that each row corresponds to a unique existing variable
stopifnot(
  nrow(wvsVariables) == length(unique(wvsVariables$name))
)

# Tag variables by index membership (economic vs. cultural), based on predefined lists
wvsVariables <- wvsVariables %>% 
  mutate(
    Index = if_else(
      name %in% economyOpinions, 
      "Econ.", 
      if_else(name %in% cultureOpinions, "Cult.", "")
    )
  )

# Fix chars
wvsVariables$question <- iconv(wvsVariables$question, "latin1", "ASCII", sub = "")
wvsVariables <- wvsVariables %>% mutate(topic = ifelse(topic == "", NA, topic))

# Check that there are no NA values in the question column
stopifnot(sum(is.na(wvsVariables$question)) == 0)

# Re-code "Same as above" as "" (allow a trailing period and/or whitespace;
# keep variants that carry an extra note, e.g. "Same as above. For the US")
same_as_above <- c("Same as above", "Same as above.")
wvsVariables <- wvsVariables %>%
  mutate(
    question       = ifelse(trimws(question) %in% same_as_above, "", question),
    codingAndNotes = ifelse(trimws(codingAndNotes) %in% same_as_above, "", codingAndNotes)
  )

# Remove general 
wvsVariables <- wvsVariables %>% 
  dplyr::filter(topic != "General") %>% 
  arrange(topic)

printWvs <- wvsVariables %>% 
  dplyr::filter(nameForPlot != "") %>% 
  select(topic, nameForPlot, question, codingAndNotes, Index) 

# Create a row index
printWvs <- printWvs %>% mutate(rowNum = 1:n())

# For each topic, create min/max row indices (used later for pack_rows in the LaTeX table)
for (t in unique(wvsVariables$topic)) {
  assign(
    paste0("min", t),
    printWvs %>% dplyr::filter(topic == t) %>% { min(.$rowNum) }
  )
  assign(
    paste0("max", t),
    printWvs %>% dplyr::filter(topic == t) %>% { max(.$rowNum) }
  )
}

# *************************************************************************
# LaTeX table ----
# *************************************************************************

printTable <- printWvs %>% 
  select(nameForPlot, question, codingAndNotes, Index) %>% 
  kable(
    format = "latex",
    booktabs = TRUE,
    longtable = TRUE,
    escape = TRUE,
    caption = "IVS Variables \\label{tab:ivs_vars}",
    col.names = c("Variable", "Description", "Coding and notes", "Index")
  ) %>%
  {gsub(
    "\\\\textbackslash\\{\\}citep\\\\\\{([[:alnum:]]*)\\\\\\}",
    "\\\\citep\\{\\1\\}",
    .
  )} %>% 
  column_spec(1, width = "3cm") %>%
  column_spec(2, width = "7cm") %>%
  column_spec(3, width = "5cm") %>%
  column_spec(4, width = "1cm")

if ("Demographics" %in% unique(wvsVariables$topic)) {
  printTable <- printTable %>% 
    pack_rows(
      "Demographics",
      minDemographics,
      maxDemographics,
      italic = TRUE,
      indent = FALSE
    )
}

printTable %>%
  pack_rows(
    "Opinions",
    minOpinions,
    maxOpinions,
    italic = TRUE,
    indent = FALSE
  ) %>%
  kable_styling(font_size = 8) %>% 
  kable_styling(latex_options = c("striped", "hold_position")) %>% 
  kable_styling(latex_options = "repeat_header") %>%
  cat(file = "output/tables/ivsVariables.tex")
