# *************************************************************************
# Description ----
# *************************************************************************

# General functions for manifesto cleaning and handling

# *************************************************************************
# Functions ----
# *************************************************************************

# This function takes a manifesto dataset and adds econ and soc index
# definition of the indices available at https://manifesto-project.wzb.eu/information/documents/visualizations
add_econsoc_index <- function(df) {
  
  # Missing variable when focusing only on east europe
  for (var in c("per415", "per505", "per602")) {
    if(! var %in% names(df)){
      df[[var]]=0
    }
  }
  
  df$econ_index <- Reduce(`+`, df[positive_econ_vars]) - Reduce(`+`, df[negative_econ_vars])
  df$soc_index <- Reduce(`+`, df[positive_soc_vars]) - Reduce(`+`, df[negative_soc_vars])
  df
}

# This function takes a manifesto dataset and adds negativity index
# this index sums up all the negative positions
add_neg_index <- function(df) {
  
  # Missing variable when focusing only on east europe
  for (var in c("per415", "per505", "per602")) {
    if(! var %in% names(df)){
      df[[var]]=0
    }
  }
  
  df %>%
    mutate(neg_index = per102 + per105 + per109 + per110 + per204 +
                          per407 + per602 + per604 + per608 + per702)
}

add_sal <- function(df) {
  
  # Missing variable when focusing only on east europe
  for (var in c("per415", "per505", "per602")) {
    if(! var %in% names(df)){
      df[[var]]=0
    }
  }
  
  df %>%
    mutate(econ_sal = per401 + per402 + per407 + per410 + per414 + per505 + per507 + per702 +
             per403 + per404 + per405 + per406 + per409 + per412 + per413 + per415 + per504 + per506 + per701,
           soc_sal = per601 + per603 + per605 + per608 + per704 +
             per201 + per202 + per503 + per602 + per604 + per607,
           fr_sal = per101 + per103 + per105 + per106 + per107 + per108 +
             per102 + per104 + per109 + per110)
}

# This function takes a manifesto dataset 
# as the indication for PRRP) and adds party families (if a party is PRRP it is prioritized over parfam)

add_parfam <- function(df){
  df %>%
    mutate(BBF_parfam = case_when(Y_FR_Pop == 1 ~ "PRRP",
                                  # 30 = Social democratic parties
                                  parfam == 30 ~ "Center-Left", 
                                  # 40 = Liberal parties
                                  # 50 =  Christian democratic parties 
                                  # 60 = Conservative parties
                                  (parfam == 40 | parfam == 50 | parfam == 60) ~ "Center-Right", 
                                  # 10 = Ecological parties
                                  parfam == 10 ~ "Green",
                                  # 20 =  Socialist or other left parties
                                  parfam == 20 ~ "Socialist",
                                  # 70 = Nationalist parties
                                  # 80 =  Agrarian parties
                                  # 90 =  Agrarian parties
                                  # 95 = Special issue parties
                                  # 98 =  Electoral alliances of diverse origin without dominant party
                                  # 999 = Missing
                                  parfam > 60 ~ NA_character_,
                                  TRUE ~ NA_character_)) %>% 
    mutate(BBF_parfam = factor(BBF_parfam, 
                             levels = c("PRRP", "Center-Right", "Center-Left","Socialist","Green")))
}


# This function computes the rolling average for a variable that has NAs in non-election years
# and is ordered in increasing years order. The function is needed since we may have more
# than 1 election cycle in the same year and would thus want to look on 6 rows for example
# in a five years period.

rolling_avg_pop <- function(var, years, window = 5, year_start = 1980, year_end = 2021) {
  # years should be a vector of the length of var containing all years in the range
  # (twice if there were two election cycles in the same year, once if there was one or none).
  # window should be an odd number so we can take symmetric number of years below and above it
  # year_start and year_end make sure that all windows cover periods that exist in the data
  stopifnot(length(var) == length(years) & window <= length(years))
  rolling_avg_year_t <- function(t) {
    half_win <- floor(window / 2) # the number of years strictly below and strictly above t in our window
    if (t - half_win >= year_start & t + half_win <= year_end) { # make sure window is in range
      if (all(is.na(var[years <= t + half_win & years >= t - half_win]))) {
        NA
      } # check for elections in window
      else {
        mean(var[years <= t + half_win & years >= t - half_win], na.rm = T) 
      }
    } else {
      NA
    }
  }
  sapply(years, rolling_avg_year_t)
}

add_alpha <- function(plot) {
  plot + 
    scale_alpha_manual(name = "", values = c(1.2, 0.1)) +
    guides(alpha = "none")
}

# Compute moving-average type-token ratio using a rolling token window.
calculate_mattr <- function(x, window = 200L) {
  n_tokens <- length(x)
  if (n_tokens == 0L) return(NA_real_)
  
  # Match quanteda behavior for short documents by using the available length.
  window <- min(as.integer(window), n_tokens)
  n_windows <- n_tokens - window + 1L
  
  # Initialize token counts for the first window.
  counts <- new.env(hash = TRUE, parent = emptyenv())
  unique_count <- 0L
  
  # Count unique tokens in the first rolling window.
  for (i in seq_len(window)) {
    token <- x[[i]]
    old_count <- counts[[token]]
    
    if (is.null(old_count)) {
      counts[[token]] <- 1L
      unique_count <- unique_count + 1L
    } else {
      counts[[token]] <- old_count + 1L
    }
  }
  
  # Add the first window's type-token ratio.
  mattr_sum <- unique_count / window
  
  # Slide the window and update only the entering and leaving token counts.
  if (n_windows > 1L) {
    for (start in 2L:n_windows) {
      outgoing <- x[[start - 1L]]
      outgoing_count <- counts[[outgoing]] - 1L
      
      if (outgoing_count == 0L) {
        rm(list = outgoing, envir = counts)
        unique_count <- unique_count - 1L
      } else {
        counts[[outgoing]] <- outgoing_count
      }
      
      incoming <- x[[start + window - 1L]]
      incoming_count <- counts[[incoming]]
      
      if (is.null(incoming_count)) {
        counts[[incoming]] <- 1L
        unique_count <- unique_count + 1L
      } else {
        counts[[incoming]] <- incoming_count + 1L
      }
      
      mattr_sum <- mattr_sum + unique_count / window
    }
  }
  
  # Return the average type-token ratio across all rolling windows.
  mattr_sum / n_windows
}
