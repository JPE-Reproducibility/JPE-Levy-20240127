# *************************************************************************
# File description ----
# *************************************************************************
#
# This file creates summary statistics tables for the World Values Survey (WVS) data.
#
# The main goal is to generate descriptive statistics that provide insights into the WVS data, including:
#   - Summary statistics for various demographic and opinion items.
#
# Outputs:
#
# - In-text values describing the WVS sample
#

# Clear the R workspace
rm(list = ls())

# *************************************************************************
# SETUP ----
# *************************************************************************
library(ggplot2)
library(dplyr)
library(haven)
library(tidyverse)
library(Hmisc)
library(kableExtra)
library(conflicted)
library(savetexvalue)
conflict_prefer("summarize", "dplyr")
conflict_prefer("filter", "dplyr")

rm(list = ls())
set.seed(2022)

source("code/util/commonVars.R")

# data
wvs_raw <- read_dta(pathWVS_Data)

# values about data set size ----------------------------------------------

countries_num <- length(unique(wvs_raw$BB_Country))

dem_and_op_num <- wvs_raw %>%
  select(starts_with("X_dem"), starts_with("X_op"), starts_with("X_b")) %>%
  ncol()

# Age or Cohorts
dem_and_op_num = dem_and_op_num - 1

save_tex_value(
  values = c(countries_num, dem_and_op_num),
  names = c("countriesNum", "demOpNum"),
  file_name = "output/values/values.tex",
  digits = 0
)
