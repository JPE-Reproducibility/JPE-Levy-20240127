# Normalize labels read from the IVS variable list.
normalize_plot_label <- function(names) {
  gsub("\\s+", " ", trimws(names))
}

# Add direction of the effect as a character
add_sign <- function(names, signs){
  clean_names <- normalize_plot_label(names)
  sgn_symbol <- ifelse(signs > 0, " (+)", ifelse(signs < 0, " (-)", " (\u00B1)"))
  var_names <- paste0(clean_names, sgn_symbol)
  return(var_names)
}

# Re-express negatively correlated labels so larger values correspond to
# positions that are more associated with PRRP support.
flip_label_toward_prrp <- function(label) {
  clean_label <- normalize_plot_label(label)

  if (grepl("^Confidence in ", clean_label)) {
    return(sub("^Confidence in ", "Lack of confidence in ", clean_label))
  }

  if (grepl("^Trust ", clean_label)) {
    return(paste0("Lack of trust in ", tolower(sub("^Trust ", "", clean_label))))
  }

  if (grepl(" justifiable$", clean_label)) {
    return(sub(" justifiable$", " not justifiable", clean_label))
  }

  clean_label
}

align_label_with_prrp <- function(names, signs) {
  clean_names <- normalize_plot_label(names)
  neg_idx <- !is.na(signs) & signs < 0

  if (any(neg_idx)) {
    clean_names[neg_idx] <- vapply(
      clean_names[neg_idx],
      flip_label_toward_prrp,
      FUN.VALUE = character(1)
    )
  }

  clean_names
}

#this function validate whether a variable appears between t1 and t2
appear = function(var, year, t1, t2, thresh = 0.9){
  prop_miss_before = mean(is.na(var[year <= t1]))
  prop_miss_after = mean(is.na(var[year >= t2]))
  return(prop_miss_before < thresh & prop_miss_after < thresh)
}

