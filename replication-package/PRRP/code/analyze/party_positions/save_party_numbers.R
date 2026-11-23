# *************************************************************************
# Description ----
# *************************************************************************
# This script computes scalar summary values for the manifesto dataset
# and writes them as LaTeX macros to be used later in the manuscript.
#
# Inputs:
# - pathWVS_Data (from code/util/commonVars.R): Stata file containing Y_manifesto_key
# - datasets/manifesto/manifesto_populist.rds
#
# Outputs:
# - output/values/manifestoValues.tex
#
# Notes:
# - This script does NOT generate tables.
# - It only produces scalar values for later inclusion in LaTeX.
# - save_tex_value() appends multiple macros to the same .tex file.
# *************************************************************************

# *************************************************************************
# SETUP ----
# *************************************************************************

rm(list = ls())

library(dplyr)
library(savetexvalue)
library(data.table)
library(haven)

# paths
source("code/util/commonVars.R")
source("code/process/manifesto/alternative_indices.R")

manifesto_rds_path      <- paste0(cleandata_path, "/manifesto/manifesto_populist.rds")
missing_source_path     <- file.path(cleandata_path, "manifesto", "sources.rds")
manifesto_codebook_path <- file.path(cleandata_path, "metadata", "manifesto_indices.rds")
outdir_value            <- "output/values/"
tex_out_path            <- file.path(outdir_value, "manifestoValues.tex")

if (!dir.exists(outdir_value)) {
  dir.create(outdir_value, recursive = TRUE)
}

# *************************************************************************
# LOAD DATA ----
# *************************************************************************

final_data <- read_dta(pathWVS_Data)

manifesto_populist <- readRDS(manifesto_rds_path) %>%
  filter(Y_manifesto_key %in% final_data$Y_manifesto_key) %>%
  mutate(party_id_year = paste(Y_manifesto_party_id, manifestoYear, sep = "_"))

manifesto_source <- readRDS(missing_source_path) %>%
  mutate(party_id_year = as.character(party_id_year)) %>%
  select(party_id_year, source, type)

source_key_counts <- manifesto_source %>%
  count(party_id_year)

bad_final_source_keys <- manifesto_populist %>%
  select(Y_manifesto_key, party_id_year) %>%
  left_join(source_key_counts, by = "party_id_year") %>%
  filter(is.na(n) | n != 1)

stopifnot(nrow(bad_final_source_keys) == 0)

manifesto_populist <- manifesto_populist %>%
  left_join(manifesto_source, by = "party_id_year")

missing_source_keys <- manifesto_populist %>%
  filter(is.na(source) | is.na(type))

stopifnot(nrow(missing_source_keys) == 0)

# *************************************************************************
# COMPUTE COUNTS ----
# *************************************************************************

# total number of manifesto records in the final sample
finalparty <- nrow(manifesto_populist)


manifesto_source_final <- manifesto_populist %>%
  select(Y_manifesto_key, party_id_year, source, type)

non_cmp_text_source_final <- manifesto_source_final %>%
  filter(!(source == "CMP" & type %in% c("Annotated Text", "Text")))

missing_by_source <- non_cmp_text_source_final %>%
  count(source)

get_source_n <- function(src) {
  missing_by_source %>%
    filter(source == src) %>%
    pull(n) %>%
    { ifelse(length(.) == 0, 0, .) }
}

capparty     <- get_source_n("CAP")
vaultparty   <- get_source_n("ManifestoVault")
polidocparty <- get_source_n("Polidoc")

cmpparty <- sum(manifesto_source_final$source == "CMP" &
                  manifesto_source_final$type == "Annotated Text")
cmptext <- sum(manifesto_source_final$source == "CMP" &
                 manifesto_source_final$type == "Text")
missingparty <- nrow(non_cmp_text_source_final)

# *************************************************************************
# COMPUTE PERCINDEX ----
# *************************************************************************

excluded_vars <- c("per203", "per204", "per408", "per703")

# rowSums() is used without na.rm = TRUE.
# Consequently, only manifestos with complete coverage of all 'per*'
# variables (excluding pervote) are retained for the computation below..

manifesto_populist <- manifesto_populist %>%
  mutate(
    sum_per = rowSums(select(., starts_with("per"), -pervote)),
    sum_excluded_vars = rowSums(select(., all_of(excluded_vars)), na.rm = TRUE)
  ) %>%
  filter(!is.na(sum_per))

avg_sum_excluded_vars <- mean(manifesto_populist$sum_excluded_vars, na.rm = TRUE)

percindex <- (100 - avg_sum_excluded_vars) / 100

# *************************************************************************
# COMPUTE MANIFESTO INDEX COVERAGE ----
# *************************************************************************

manifesto_codebook <- readRDS(manifesto_codebook_path)

manifesto_position_vars <- manifesto_codebook %>%
  filter(type == "main" & code != 0) %>%
  pull(variable_name)

stopifnot(!any(duplicated(manifesto_position_vars)))

econ_culture_index_vars <- unique(c(
  positive_econ_vars, negative_econ_vars,
  positive_soc_vars, negative_soc_vars,
  econ_prosser_positive, econ_prosser_negative,
  soc_prosser_positive, soc_prosser_negative,
  econ_bak_hob_positive, econ_bak_hob_negative,
  soc_bak_hob_positive, soc_bak_hob_negative
))

additional_index_vars <- unique(c(
  positive_eu_vars, negative_eu_vars,
  positive_hawk_dove_expanded_vars, negative_hawk_dove_expanded_vars,
  gov_vars
))

all_index_vars <- unique(c(econ_culture_index_vars, additional_index_vars))

stopifnot(length(setdiff(all_index_vars, manifesto_position_vars)) == 0)


# *************************************************************************
# WRITE LaTeX VALUES ----
# *************************************************************************

tex_values <- list(
  finalparty   = finalparty,
  cmpparty     = cmpparty,
  missingparty = missingparty,
  capparty     = capparty,
  vaultparty   = vaultparty,
  polidocparty = polidocparty,
  cmptext      = cmptext
)

purrr::iwalk(
  tex_values,
  ~ save_tex_value(
    values    = .x,
    names     = .y,
    digits    = 0,
    file_name = tex_out_path
  )
)

save_tex_value(
  values    = percindex,
  names     = "percindex",
  file_name = tex_out_path,
  percent  = TRUE
)
