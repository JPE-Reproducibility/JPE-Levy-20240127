# *************************************************************************
# Load pledge classifications ----
# *************************************************************************

# Use the original input locally, or recover CMP codes from the prepared corpus
# and the public classification export without rerunning the language model.
load_pledge_data <- function(data_root) {
  original_path <- file.path(data_root, "data/rawdata/authorcreated/Pledges/ClassifiedManifestos.csv")
  if (file.exists(original_path)) {
    classifications <- data.table::fread(original_path)
    required <- c("country", "party_id", "date", "cmp_code", "content")
    if (!all(required %in% names(classifications)) || nrow(classifications) == 0L) {
      stop("Pledge classifications must contain country, party_id, date, cmp_code and content.")
    }
    invalid <- is.na(classifications$content) |
      !as.character(classifications$content) %in% c("0", "1")
    if (any(invalid)) {
      stop("Pledge classifications contain ", sum(invalid),
        " invalid or missing labels. Repair the supplied raw classifications before processing.")
    }
    if (anyNA(classifications[, c("country", "party_id", "date"), with = FALSE])) {
      stop("Pledge classifications contain missing document identifiers.")
    }
    classifications$content <- as.integer(classifications$content)
    return(classifications)
  }

  stop("Missing supplied pledge classifications: ", original_path)
}
