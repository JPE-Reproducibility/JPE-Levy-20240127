# Supplied labels become final metadata inputs for all downstream stages.
metadata <- file.path("data/cleandata", "metadata")
dir.create(metadata, recursive = TRUE, showWarnings = FALSE)
for (item in list(
  c("data/rawdata/authorcreated/WVS/variable_list.csv", "wvs_variables.csv"),
  c("data/rawdata/downloaded/not_share/Manifesto/codebook_categories_MPDS2021a.csv", "manifesto_categories.csv"),
  c("data/rawdata/authorcreated/Manifesto/codebook_categories_MPDS2021a_indices.csv", "manifesto_indices.csv"))) {
  source_encoding <- if (grepl("WVS/variable_list.csv$", item[1])) "windows-1252" else "UTF-8"
  x <- read.csv(item[1], fileEncoding = source_encoding, encoding = "UTF-8", stringsAsFactors = FALSE)
  saveRDS(x, file.path(metadata, sub("\\.csv$", ".rds", item[2])))
}
saveRDS(readRDS("data/rawdata/authorcreated/model/dec_4_6_empty.rds"),
        file.path(metadata, "empty_decomposition.rds"))
rm(metadata, item, x, source_encoding)
