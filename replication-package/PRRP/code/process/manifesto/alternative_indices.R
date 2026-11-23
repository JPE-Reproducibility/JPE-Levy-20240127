# *************************************************************************
# Alternative CMP indices (Prosser 2014; Bakker & Hobolt 2013)
# *************************************************************************
# This file defines alternative ideological indices based on CMP variables.
#
# IMPORTANT:
# - These indices are manually defined conceptual classifications.
# - They were previously stored in CSV files, but since they are not
#   empirical data, they are defined here directly in code.
# - This script defines objects and helper functions only.
# - It does NOT read data and does NOT write output.
# *************************************************************************


# ========================================================================
# Economic index
# ========================================================================

# -------------------------------
# Prosser (2014) – Economic index
# -------------------------------
# Reference: Prosser, C. (2014). "Building policy scales from manifesto data".
# Electoral Studies, 35, 88-101.
# See Table 5 ("Final component selection..."), Page 99.
# -------------------------------

econ_prosser_positive <- c(
  "per401", "per407", "per414", "per505", "per507", "per702"
)

econ_prosser_negative <- c(
  "per403", "per411", "per412", "per413",
  "per503", "per504", "per506", "per701"
)

# ----------------------------------------
# Bakker & Hobolt (2013) – Economic index
# ----------------------------------------
# Reference: Bakker, R., & Hobolt, S. B. (2013). "Measuring party positions".
# In G. Evans & N. D. De Graaf (Eds.), Political Choice Matters. Oxford University Press.
# See Table 2.4 ("Bakker-Hobolt's modified CMP measures"), Page 38.
# ----------------------------------------

econ_bak_hob_positive <- c(
  "per401", "per402", "per407", "per410",
  "per414", "per505", "per507", "per702"
)

econ_bak_hob_negative <- c(
  "per403", "per404", "per405", "per406", "per409",
  "per412", "per413", "per415",
  "per503", "per504", "per506", "per701"
)


# ========================================================================
# Social (culture / libertarian–authoritarian) index
# ========================================================================

# -------------------------------
# Prosser (2014) – Social index
# -------------------------------
# Reference: Prosser, C. (2014). "Building policy scales from manifesto data".
# Electoral Studies, 35, 88-101.
# See Table 5 ("Final component selection..."), Page 99.
# -------------------------------

soc_prosser_positive <- c(
  "per109", "per302", "per305", "per601", "per608"
)

soc_prosser_negative <- c(
  "per105", "per106", "per107",
  "per201", "per202", "per301", "per416",
  "per501", "per502",
  "per602", "per607",
  "per704", "per705", "per706"
)

# ----------------------------------------
# Bakker & Hobolt (2013) – Social index
# ----------------------------------------
# Reference: Bakker, R., & Hobolt, S. B. (2013). "Measuring party positions".
# In G. Evans & N. D. De Graaf (Eds.), Political Choice Matters. Oxford University Press.
# See Table 2.4 ("Bakker-Hobolt's modified CMP measures"), Page 38.
# ----------------------------------------

soc_bak_hob_positive <- c(
  "per305", "per601", "per603", "per605", "per606", "per608"
)

soc_bak_hob_negative <- c(
  "per201", "per202", "per416",
  "per501", "per502",
  "per602", "per604", "per607",
  "per705", "per706"
)


# ========================================================================
# Helper function
# ========================================================================
# Assigns "+", "-", or "" to a CMP variable depending on index membership.

assign_index_sign <- function(variable_name, positive_vars, negative_vars) {
  dplyr::case_when(
    variable_name %in% positive_vars ~ "+",
    variable_name %in% negative_vars ~ "-",
    TRUE ~ ""
  )
}


# ========================================================================
# Sanity checks
# ========================================================================
# Ensure that no variable appears with both positive and negative signs
# within the same index definition.

stopifnot(length(intersect(econ_prosser_positive, econ_prosser_negative)) == 0)
stopifnot(length(intersect(econ_bak_hob_positive, econ_bak_hob_negative)) == 0)
stopifnot(length(intersect(soc_prosser_positive, soc_prosser_negative)) == 0)
stopifnot(length(intersect(soc_bak_hob_positive, soc_bak_hob_negative)) == 0)