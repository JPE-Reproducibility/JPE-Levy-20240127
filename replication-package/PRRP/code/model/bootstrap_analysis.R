bootstrap_summary <- function(bs.folder) {
source("code/model/bootstrap_checks.R")
bootstrap_check_collection(bs.folder,seq_len(bootstrapDraws))
all.runs <- file.path(bs.folder,as.character(seq_len(bootstrapDraws)))
dec.results <- lapply(all.runs,function(run) readRDS(file.path(run,"dec_4_6.Rds")))
names(dec.results) <- all.runs

# *************************************************************************
# HELPER FUNCTIONS ----
# *************************************************************************

# Function to calculate SD and Quantiles (5%, 95%)
calc_bootstrap_stats <- function(x) {
  return(c(sd(x), quantile(x, c(0.05, 0.95))))
}

# *************************************************************************
# DATA PROCESSING & STATISTICS ----
# *************************************************************************

# Get list of countries from the first result
cntrs <- rownames(dec.results[[1]])

# Define columns to analyze: 
# Indices 3, 6, 9, 12 correspond to variables: residual, Z, X, param
target_cols_indices <- seq(3, 12, 3) 

# Calculate stats for each country across all runs
bs.res <- lapply(cntrs, function(cnt) { 
  
  # Step 1: Combine rows for the specific country from all runs into one matrix
  all.cnt <- do.call("rbind", lapply(dec.results, function(x) x[cnt, ]))
  
  # Step 2: Apply the stats function to the target columns
  res <- apply(all.cnt[, target_cols_indices], 2, calc_bootstrap_stats)
  
  # Flatten the result to a vector
  as.vector(res)
})

# *************************************************************************
# FORMAT RESULTS ----
# *************************************************************************

bs.res <- data.frame(bs.res)
colnames(bs.res) <- cntrs

# Construct row names: repeats variable names and appends stat suffix
# Format: variable.stat (e.g., residual.sd, residual.5, residual.95)
var_names <- c("residual", "Z", "X", "param")
stat_suffixes <- c(".sd", 5, 95)

rownames(bs.res) <- paste0(
  as.vector(sapply(var_names, function(n) rep(n, 3))),
  stat_suffixes
)


bs.res
}
