# Write a CSV copy next to each Stata (.dta) file: the JPE data policy requires a
# non-proprietary copy of every dataset in the package. Values are written as stored;
# variable and value labels remain in the .dta files and the codebooks listed in the README.
# masterFile.R calls this for data/cleandata after preprocessing. The two shared raw .dta
# files were converted once with write_csv_copies("data/rawdata/downloaded/public").
write_csv_copies <- function(dirs) {
  files <- unlist(lapply(dirs, function(d) list.files(d, pattern = "\\.dta$", recursive = TRUE, full.names = TRUE)))
  for (f in files) {
    d <- haven::zap_formats(haven::zap_labels(haven::read_dta(f)))
    data.table::fwrite(d, sub("\\.dta$", ".csv", f))
  }
  invisible(files)
}
