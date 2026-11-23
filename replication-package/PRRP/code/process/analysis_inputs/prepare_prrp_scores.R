library(tidyverse)
library(haven)
library(data.table)
library(Hmisc)
source("code/util/commonVars.R")
library(glmnet)
source("code/process/analysis_inputs/label_functions.R")
conflicted::conflict_prefer("wtd.var", "Hmisc")
set.seed(2022)
### Choose outcome variable for lasso ----
# if "RRP" - use Y_Far_Right, if "PRRP" - use Y_FR_Pop,
# Since there is almost complete overlap, we use Y_Far_Right but refer to it as PRRP
outcome <- "PRRP"
outcome_var <- "Y_FR_Pop"

weighted_suf <- "_weighted.pdf"
ranking_artifact <- file.path(cleandata_path, "ivs", "prrp_score_covariate_ranking.rds")

# read data from dropbox:
data <- read_dta(file = pathWVS_Data)

data <- data %>%
  left_join(waveLabelsDF,
    by = c("BB_Wave_num" = "waveNum"),
    keep = F
  )

# Choose data, last wave, no general or cabinet vars
data_last_year <- data %>%
  filter(BB_Wave_num == 6) %>%
  dplyr::select(-starts_with("X_general")) %>%
  filter(!Y_No_Party_Vote)

df_for_reg <- data_last_year %>%
  dplyr::select(
    starts_with("X_op"),
    c("BB_Country", "Y_FR_Pop", "Y_Far_Right", "BB_Weight_CW")
  )

# prepare data for lasso: opinion variables plus unpenalized country FE
X_names <- df_for_reg %>%
  dplyr::select(starts_with("X_op")) %>%
  colnames()

stopifnot(all(grepl("^X_op", X_names)))

form <- paste0(X_names, collapse = "+")
form <- paste0("~", form, "+ BB_Country")

X_for_lasso <- model.matrix(as.formula(form), data = df_for_reg)[, -1]

stopifnot(all(grepl("^X_op|^BB_Country", colnames(X_for_lasso))))

# Penalty = 0 country is always included in the model
penalty <- rep(1, ncol(X_for_lasso))
penalty[startsWith(colnames(X_for_lasso), "BB_Country")] <- 0

# Run the model
lasso <- cv.glmnet(
  x = X_for_lasso,
  y = pull(df_for_reg, outcome_var),
  weights = pull(df_for_reg, BB_Weight_CW),
  penalty.factor = penalty
)

# Preserve the original post-fit random state for subsequent presentation.
post_lasso_seed <- .Random.seed

# Pick the best model
beta <- coef(lasso, s = "lambda.min")
beta <- beta[rownames(beta) != "(Intercept)", ]

# The score excludes country FE and uses only selected opinion coefficients.
beta <- beta[!startsWith(names(beta), "BB_Country")]
beta <- beta[beta != 0]
stopifnot(length(beta) > 0)
stopifnot(all(grepl("^X_op", names(beta))))

# calculate for all years
rel_X <- data %>%
  dplyr::select(names(beta)) %>%
  as.matrix()

data <- data %>%
  add_count(BB_Int_Wave, BB_Country, name = "countWaveCountry")

data_east <- data %>% 
  filter(BB_Country %in% country_east) %>% 
  mutate(BB_Country = "East", BB_Id = NA)

data_west <- data %>% 
  filter(BB_Country %in% country_west) %>% 
  mutate(BB_Country = "West", BB_Id = NA)

data_ew <- bind_rows(data_east, data_west)

rel_X_east <- data_east %>%
  dplyr::select(names(beta)[beta != 0]) %>%
  as.matrix()

rel_X_west <- data_west %>%
  dplyr::select(names(beta)[beta != 0]) %>%
  as.matrix()

rel_X_ew <- data_ew %>%
  dplyr::select(names(beta)[beta != 0]) %>%
  as.matrix()

# Calculate the values for the most PRRP possible voter and the least PRRP possible voter
x_max <- apply(rel_X, MARGIN = 2, FUN = max)
x_min <- apply(rel_X, MARGIN = 2, FUN = min)

# The ceiling would have the maximal value when the coefficient is positive and minimal when it is negative
prrp_ceil_x <- (beta > 0) * x_max + (beta < 0) * x_min
prrp_floor_x <- (beta < 0) * x_max + (beta > 0) * x_min

# repeat by country
x_country <- data.frame(rel_X, BB_Country = data$BB_Country) %>%
  group_by(BB_Country) %>%
  summarise(across(everything(), list(MMmax = max, MMmin = min))) %>%
  pivot_longer(
    cols = (starts_with("X")), names_to = c("var", "stat"),
    names_sep = "_MM"
  ) %>%
  pivot_wider(names_from = var, values_from = value)


x_country_max <- x_country %>%
  filter(stat == "max") %>%
  select(!c(BB_Country, stat)) %>%
  as.matrix()

x_country_min <- x_country %>%
  filter(stat == "min") %>%
  select(!c(BB_Country, stat)) %>%
  as.matrix()

beta_mat_country <- matrix(rep(beta, nrow(x_country_max)),
  nrow = nrow(x_country_max), byrow = T
)

prrp_ceil_x_country <- x_country_max * (beta_mat_country > 0) + x_country_min * (beta_mat_country < 0)
prrp_floor_x_country <- x_country_max * (beta_mat_country < 0) + x_country_min * (beta_mat_country > 0)
rownames(prrp_ceil_x_country) <- row.names(prrp_floor_x_country) <- unique(data$BB_Country)

# Repeat by wave
x_wave <- data.frame(rel_X, BB_Wave_num = data$BB_Wave_num) %>%
  group_by(BB_Wave_num) %>%
  summarise(across(everything(), list(MMmax = max, MMmin = min))) %>%
  pivot_longer(
    cols = (starts_with("X")), names_to = c("var", "stat"),
    names_sep = "_MM"
  ) %>%
  pivot_wider(names_from = var, values_from = value)


x_wave_max <- x_wave %>%
  filter(stat == "max") %>%
  select(!c(BB_Wave_num, stat)) %>%
  as.matrix()

x_wave_min <- x_wave %>%
  filter(stat == "min") %>%
  select(!c(BB_Wave_num, stat)) %>%
  as.matrix()

beta_mat_wave <- matrix(rep(beta, nrow(x_wave_max)),
  nrow = nrow(x_wave_max), byrow = T
)

prrp_ceil_x_wave <- x_wave_max * (beta_mat_wave > 0) + x_wave_min * (beta_mat_wave < 0)
prrp_floor_x_wave <- x_wave_max * (beta_mat_wave < 0) + x_wave_min * (beta_mat_wave > 0)

# Repeat by east and west
x_ew <- data.frame(rel_X_ew, BB_Country = data_ew$BB_Country) %>%
  group_by(BB_Country) %>%
  summarise(across(everything(), list(MMmax = max, MMmin = min))) %>%
  pivot_longer(
    cols = (starts_with("X")), names_to = c("var", "stat"),
    names_sep = "_MM"
  ) %>%
  pivot_wider(names_from = var, values_from = value)

x_ew_max <- x_ew %>%
  filter(stat == "max") %>%
  select(!c(BB_Country, stat)) %>%
  as.matrix()

x_ew_min <- x_ew %>%
  filter(stat == "min") %>%
  select(!c(BB_Country, stat)) %>%
  as.matrix()

beta_mat_ew <- matrix(rep(beta, nrow(x_ew_max)),
                           nrow = nrow(x_ew_max), byrow = T
)

prrp_ceil_x_ew <- x_ew_max * (beta_mat_ew > 0) + x_ew_min * (beta_mat_ew < 0)
prrp_floor_x_ew <- x_ew_max * (beta_mat_ew < 0) + x_ew_min * (beta_mat_ew > 0)
rownames(prrp_ceil_x_ew) <- row.names(prrp_floor_x_ew) <- unique(data_ew$BB_Country)




# Create a version of the data with outcome_score standardized separately by country
# using wave 4 mean and sd for later use
data_country <- data %>%
  mutate(outcome_score = rel_X %*% beta %>% as.numeric()) %>%
  group_by(BB_Country, BB_Wave_num) %>%
  mutate(
    sd_wave_4 = if_else(BB_Wave_num == 4, sqrt(wtd.var(outcome_score, w = BB_Weight_CW)), -Inf),
    mean_wave_4 = if_else(BB_Wave_num == 4, weighted.mean(outcome_score, w = BB_Weight_CW), -Inf)
  ) %>%
  ungroup() %>%
  group_by(BB_Country) %>%
  mutate(
    sd_wave_4 = max(sd_wave_4),
    mean_wave_4 = max(mean_wave_4),
    outcome_score_std = (outcome_score - mean_wave_4) / sd_wave_4
  ) %>%
  ungroup()



# Create main version of the data with outcome_score standardized for all obs together
data <- data %>%
  mutate(outcome_score = rel_X %*% beta %>% as.numeric())

data_east <- data_east %>%
  mutate(outcome_score_east = rel_X_east %*% beta %>% as.numeric())

data_west <- data_west %>%
  mutate(outcome_score_west = rel_X_west %*% beta %>% as.numeric())

# get standardization parameters for ceiling and floor voters
mean_outcome_score <- data %>%
  summarise(mean_outcome_score = weighted.mean(outcome_score, w = BB_Weight_CW)) %>%
  pull(mean_outcome_score)

sd_outcome_score <- data %>%
  summarise(mean_outcome_score = sqrt(wtd.var(outcome_score, w = BB_Weight_CW))) %>%
  pull(mean_outcome_score)

# standardize outcome_score
data <- data %>%
  mutate(outcome_score_stdNoWt = scale(outcome_score) %>% as.numeric()) %>%
  mutate(outcome_score_std = (outcome_score - weighted.mean(outcome_score, w = BB_Weight_CW)) /
    sqrt(wtd.var(outcome_score, w = BB_Weight_CW)))

data_east <- data_east %>%
  mutate(outcome_score_stdNoWt_east = scale(outcome_score_east) %>% as.numeric()) %>%
  mutate(outcome_score_std_east = (outcome_score_east - weighted.mean(outcome_score_east, w = BB_Weight_CW)) /
           sqrt(wtd.var(outcome_score_east, w = BB_Weight_CW)))

data_west <- data_west %>%
  mutate(outcome_score_stdNoWt_west = scale(outcome_score_west) %>% as.numeric()) %>%
  mutate(outcome_score_std_west = (outcome_score_west - weighted.mean(outcome_score_west, w = BB_Weight_CW)) /
           sqrt(wtd.var(outcome_score_west, w = BB_Weight_CW)))

outcome_score_ceil <- prrp_ceil_x %*% beta
outcome_score_floor <- prrp_floor_x %*% beta
outcome_score_std_ceil <- (outcome_score_ceil - mean_outcome_score) / sd_outcome_score
outcome_score_std_floor <- (outcome_score_floor - mean_outcome_score) / sd_outcome_score

ceil_floor_dat <- data.frame(
  ceil = prrp_ceil_x_country %*% beta,
  floor = prrp_floor_x_country %*% beta
) %>%
  mutate(across(everything(), ~ (.x - mean_outcome_score) / sd_outcome_score)) %>%
  rownames_to_column("BB_Country")

ceil_floor_dat_wave <- data.frame(
  ceil = prrp_ceil_x_wave %*% beta,
  floor = prrp_floor_x_wave %*% beta
) %>%
  mutate(across(everything(), ~ (.x - mean_outcome_score) / sd_outcome_score),
    BB_Wave_num = 4:6
  ) %>%
  left_join(waveLabelsDF, by = c("BB_Wave_num" = "waveNum")) %>%
  filter(BB_Wave_num != 5)

ceil_floor_dat_ew <- data.frame(
  ceil = prrp_ceil_x_ew %*% beta,
  floor = prrp_floor_x_ew %*% beta
) %>%
  mutate(across(everything(), ~ (.x - mean_outcome_score) / sd_outcome_score)) %>%
  rownames_to_column("BB_Country")

names_for_graph <- names_for_graph %>%
  add_row(name = "outcome_score", plotName = "PRRP Score") %>%
  add_row(name = "outcome_score_std", plotName = "PRRP Score (in std. dev.)")

######################################
# Figure A.4: Covariates and PRRP correlation
######################################


# calculate correlation using the last year
X <- data_last_year %>%
  select(starts_with("X_"))

# which variables have zero variation
var_X <- apply(X, 2, var, na.rm = T)


stopifnot(!is.na(var_X) & var_X > 0)


pop_score_last_year <- data %>%
  filter(BB_Wave_num == 6) %>%
  dplyr::select(-starts_with("X_general")) %>%
  filter(!Y_No_Party_Vote) %>%
  pull(outcome_score_std)

pop_score_last_year_east <- data_east %>%
  filter(BB_Wave_num == 6) %>%
  dplyr::select(-starts_with("X_general")) %>%
  filter(!Y_No_Party_Vote) %>%
  pull(outcome_score_std_east)

pop_score_last_year_west <- data_west  %>%
  filter(BB_Wave_num == 6) %>%
  dplyr::select(-starts_with("X_general")) %>%
  filter(!Y_No_Party_Vote) %>%
  pull(outcome_score_std_west)

# Calculate weighted correlations
res <- data.frame()

for (var in colnames(X)) {
  
  X_i <-  X[var]
  cor_i <- cov.wt(as.matrix(cbind(X_i, pop_score_last_year)), wt = data_last_year$BB_Weight_CW, cor = T)$cor[1,2]
  res <- rbind(res, cbind.data.frame(variable = var, cor = cor_i))
}

# Check which variables can be plotted over time relative to wave 4.
trend_eligibility <- tibble(variable = colnames(X)) %>%
  mutate(
    has_wave_4 = map_lgl(variable, ~ any(data$BB_Wave_num == 4 & !is.na(data[[.x]]))),
    has_wave_6 = map_lgl(variable, ~ any(data$BB_Wave_num == 6 & !is.na(data[[.x]]))),
    wave_4_sd = map_dbl(variable, ~ sd(data[[.x]][data$BB_Wave_num == 4], na.rm = TRUE)),
    is_trend_eligible = has_wave_4 & has_wave_6 & is.finite(wave_4_sd) & wave_4_sd > 0
  )

# merge with plot names and add metadata used by other figures
res_full <- res %>%
  left_join(names_for_graph, by = c("variable" = "name")) %>%
  left_join(trend_eligibility, by = "variable") %>%
  mutate(
    plot_name_raw = if_else(is.na(plotName) | plotName == "", variable, plotName),
    plot_name_raw = normalize_plot_label(plot_name_raw),
    cor_sign = sign(cor),
    cor_abs = abs(cor),
    plot_name_signed = add_sign(plot_name_raw, cor_sign),
    display_label = align_label_with_prrp(plot_name_raw, cor_sign),
    is_opinion = grepl("^X_op", variable),
    is_cultural_opinion = variable %in% cultureOpinions
  ) %>%
  arrange(desc(cor_abs), variable) %>%
  mutate(rank = row_number())

data_eastSouth <- data %>%
  filter(BB_Country %in% country_eastSouth) %>%
  mutate(BB_Country = "EastSouth", BB_Id = NA) %>%
  mutate(outcome_score_std_eastSouth = (outcome_score - weighted.mean(outcome_score, w = BB_Weight_CW)) /
           sqrt(wtd.var(outcome_score, w = BB_Weight_CW)))

data_westNorth <- data %>%
  filter(BB_Country %in% country_westNorth) %>%
  mutate(BB_Country = "WestNorth", BB_Id = NA) %>%
  mutate(outcome_score_std_westNorth = (outcome_score - weighted.mean(outcome_score, w = BB_Weight_CW)) /
           sqrt(wtd.var(outcome_score, w = BB_Weight_CW)))

data$waveFactor <- factor(data$BB_Wave_num, levels = 4:6, labels = sort(unique(data$BB_Int_Wave)))
data_east$waveFactor <- factor(data_east$BB_Wave_num, levels = 4:6, labels = sort(unique(data_east$BB_Int_Wave)))
data_west$waveFactor <- factor(data_west$BB_Wave_num, levels = 4:6, labels = sort(unique(data_west$BB_Int_Wave)))
data_eastSouth$waveFactor <- factor(data_eastSouth$BB_Wave_num, levels = 4:6, labels = sort(unique(data_eastSouth$BB_Int_Wave)))
data_westNorth$waveFactor <- factor(data_westNorth$BB_Wave_num, levels = 4:6, labels = sort(unique(data_westNorth$BB_Int_Wave)))


populistWave <-  data %>%
  group_by(BB_Wave_num, BB_Country, waveLabels) %>%
  summarise(m = weighted.mean(outcome_score_std, w = BB_Weight_CW, na.rm = T)) %>%
  dplyr::select(BB_Wave_num, BB_Country, waveLabels, m) %>%
  group_by(BB_Wave_num, waveLabels) %>%
  summarise(m = mean(m)) %>%
  pivot_longer(cols = "m", names_to = "type", values_to = "value")

populistWave_east <-  data_east %>%
  group_by(BB_Wave_num, BB_Country, waveLabels) %>%
  summarise(m = weighted.mean(outcome_score_std_east, w = BB_Weight_CW, na.rm = T)) %>%
  dplyr::select(BB_Wave_num, BB_Country, waveLabels, m) %>%
  group_by(BB_Wave_num, waveLabels) %>%
  summarise(m = mean(m)) %>%
  pivot_longer(cols = "m", names_to = "type", values_to = "value")

populistWave_west <-  data_west %>%
  group_by(BB_Wave_num, BB_Country, waveLabels) %>%
  summarise(m = weighted.mean(outcome_score_std_west, w = BB_Weight_CW, na.rm = T)) %>%
  dplyr::select(BB_Wave_num, BB_Country, waveLabels, m) %>%
  group_by(BB_Wave_num, waveLabels) %>%
  summarise(m = mean(m)) %>%
  pivot_longer(cols = "m", names_to = "type", values_to = "value")

populistWave_eastSouth <-  data_eastSouth %>%
  group_by(BB_Wave_num, BB_Country, waveLabels) %>%
  summarise(m = weighted.mean(outcome_score_std_eastSouth, w = BB_Weight_CW, na.rm = T)) %>%
  dplyr::select(BB_Wave_num, BB_Country, waveLabels, m) %>%
  group_by(BB_Wave_num, waveLabels) %>%
  summarise(m = mean(m)) %>%
  pivot_longer(cols = "m", names_to = "type", values_to = "value")

populistWave_westNorth <-  data_westNorth %>%
  group_by(BB_Wave_num, BB_Country, waveLabels) %>%
  summarise(m = weighted.mean(outcome_score_std_westNorth, w = BB_Weight_CW, na.rm = T)) %>%
  dplyr::select(BB_Wave_num, BB_Country, waveLabels, m) %>%
  group_by(BB_Wave_num, waveLabels) %>%
  summarise(m = mean(m)) %>%
  pivot_longer(cols = "m", names_to = "type", values_to = "value")

# Avg. + Countries - standardized together demeaned by ech country wave 4
populistWaveAll <- populistWave %>%
  rename(av = value) %>%
  select(-type)

populistWaveAll_east <- populistWave_east %>%
  rename(av = value) %>%
  select(-type)

populistWaveAll_west <- populistWave_west %>%
  rename(av = value) %>%
  select(-type)

populistWaveAll_eastSouth <- populistWave_eastSouth %>%
  rename(av = value) %>%
  select(-type)

populistWaveAll_westNorth <- populistWave_westNorth %>%
  rename(av = value) %>%
  select(-type)


# Summary stats by country and wave
populistCountryWave <- data %>%
  group_by(BB_Country, BB_Int_Wave, BB_Wave_num, waveFactor, waveLabels) %>%
  summarise(
    av = weighted.mean(outcome_score_std, w = BB_Weight_CW, na.rm = T),
    v =  wtd.var(outcome_score_std, w = BB_Weight_CW, na.rm = T),
    n = n()
  )

populistWaveAllCountries <- populistCountryWave %>%
  ungroup() %>%
  bind_rows(populistWaveAll) %>%
  select(BB_Country, all_of(colnames(populistWaveAll))) %>%
  mutate(BB_Country = if_else(is.na(BB_Country), "Average", BB_Country)) %>%
  mutate(color = if_else(BB_Country == "Average", 2, 1)) %>%
  group_by(BB_Country) %>%
  mutate(
    mean_wave_4 = if_else(BB_Wave_num == 4, av, -Inf),
    mean_wave_4 = max(mean_wave_4),
    av = av - mean_wave_4
  ) %>%
  arrange(color, BB_Wave_num)

populistCountryWave_east <- data_east %>%
  group_by(BB_Country, BB_Int_Wave, BB_Wave_num, waveFactor, waveLabels) %>%
  summarise(
    av = weighted.mean(outcome_score_std_east, w = BB_Weight_CW, na.rm = T),
    v =  wtd.var(outcome_score_std_east, w = BB_Weight_CW, na.rm = T),
    n = n()
  )

populistWaveAllCountries_east <- populistCountryWave_east %>%
  ungroup() %>%
  bind_rows(populistWaveAll_east) %>%
  select(BB_Country, all_of(colnames(populistWaveAll_east))) %>%
  mutate(BB_Country = if_else(is.na(BB_Country), "Average", BB_Country)) %>%
  mutate(color = if_else(BB_Country == "Average", 2, 1)) %>%
  group_by(BB_Country) %>%
  mutate(
    mean_wave_4 = if_else(BB_Wave_num == 4, av, -Inf),
    mean_wave_4 = max(mean_wave_4),
    av = av - mean_wave_4
  ) %>%
  arrange(color, BB_Wave_num)

populistWaveAllCountries_east <- populistWaveAllCountries_east %>% 
  filter(BB_Country == "Average") %>% 
  bind_rows(populistWaveAllCountries %>% 
              filter(BB_Country %in% country_east)) %>%
  arrange(color, BB_Wave_num)

populistCountryWave_west <- data_west %>%
  group_by(BB_Country, BB_Int_Wave, BB_Wave_num, waveFactor, waveLabels) %>%
  summarise(
    av = weighted.mean(outcome_score_std_west, w = BB_Weight_CW, na.rm = T),
    v =  wtd.var(outcome_score_std_west, w = BB_Weight_CW, na.rm = T),
    n = n()
  )

populistWaveAllCountries_west <- populistCountryWave_west %>%
  ungroup() %>%
  bind_rows(populistWaveAll_west) %>%
  select(BB_Country, all_of(colnames(populistWaveAll_west))) %>%
  mutate(BB_Country = if_else(is.na(BB_Country), "Average", BB_Country)) %>%
  mutate(color = if_else(BB_Country == "Average", 2, 1)) %>%
  group_by(BB_Country) %>%
  mutate(
    mean_wave_4 = if_else(BB_Wave_num == 4, av, -Inf),
    mean_wave_4 = max(mean_wave_4),
    av = av - mean_wave_4
  ) %>%
  arrange(color, BB_Wave_num)

populistWaveAllCountries_west <- populistWaveAllCountries_west %>% 
  filter(BB_Country == "Average") %>% 
  bind_rows(populistWaveAllCountries %>% 
              filter(BB_Country %in% country_west)) %>% 
  arrange(color, BB_Wave_num)

populistCountryWave_eastSouth <- data_eastSouth %>%
  group_by(BB_Country, BB_Int_Wave, BB_Wave_num, waveFactor, waveLabels) %>%
  summarise(
    av = weighted.mean(outcome_score_std_eastSouth, w = BB_Weight_CW, na.rm = T),
    v =  wtd.var(outcome_score_std_eastSouth, w = BB_Weight_CW, na.rm = T),
    n = n()
  )

populistWaveAllCountries_eastSouth <- populistCountryWave_eastSouth %>%
  ungroup() %>%
  bind_rows(populistWaveAll_eastSouth) %>%
  select(BB_Country, all_of(colnames(populistWaveAll_eastSouth))) %>%
  mutate(BB_Country = if_else(is.na(BB_Country), "Average", BB_Country)) %>%
  mutate(color = if_else(BB_Country == "Average", 2, 1)) %>%
  group_by(BB_Country) %>%
  mutate(
    mean_wave_4 = if_else(BB_Wave_num == 4, av, -Inf),
    mean_wave_4 = max(mean_wave_4),
    av = av - mean_wave_4
  ) %>%
  arrange(color, BB_Wave_num)

populistWaveAllCountries_eastSouth <- populistWaveAllCountries_eastSouth %>%
  filter(BB_Country == "Average") %>%
  bind_rows(populistWaveAllCountries %>%
              filter(BB_Country %in% country_eastSouth)) %>%
  arrange(color, BB_Wave_num)

populistCountryWave_westNorth <- data_westNorth %>%
  group_by(BB_Country, BB_Int_Wave, BB_Wave_num, waveFactor, waveLabels) %>%
  summarise(
    av = weighted.mean(outcome_score_std_westNorth, w = BB_Weight_CW, na.rm = T),
    v =  wtd.var(outcome_score_std_westNorth, w = BB_Weight_CW, na.rm = T),
    n = n()
  )

populistWaveAllCountries_westNorth <- populistCountryWave_westNorth %>%
  ungroup() %>%
  bind_rows(populistWaveAll_westNorth) %>%
  select(BB_Country, all_of(colnames(populistWaveAll_westNorth))) %>%
  mutate(BB_Country = if_else(is.na(BB_Country), "Average", BB_Country)) %>%
  mutate(color = if_else(BB_Country == "Average", 2, 1)) %>%
  group_by(BB_Country) %>%
  mutate(
    mean_wave_4 = if_else(BB_Wave_num == 4, av, -Inf),
    mean_wave_4 = max(mean_wave_4),
    av = av - mean_wave_4
  ) %>%
  arrange(color, BB_Wave_num)

populistWaveAllCountries_westNorth <- populistWaveAllCountries_westNorth %>%
  filter(BB_Country == "Average") %>%
  bind_rows(populistWaveAllCountries %>%
              filter(BB_Country %in% country_westNorth)) %>%
  arrange(color, BB_Wave_num)


# Average value for all countries
populistWaveAllCountries_average <- populistWaveAllCountries %>% filter(BB_Country == "Average")

write_rds(res_full, ranking_artifact)
write_rds(populistWaveAllCountries, paste0(cleandata_path, "/ivs/prrp_score.rds"))
profiles <- list(data=data, data_east=data_east, data_west=data_west, ceil_floor_dat=ceil_floor_dat, ceil_floor_dat_wave=ceil_floor_dat_wave, ceil_floor_dat_ew=ceil_floor_dat_ew, res_full=res_full, populistWave=populistWave, populistWaveAllCountries=populistWaveAllCountries, populistWaveAllCountries_east=populistWaveAllCountries_east, populistWaveAllCountries_west=populistWaveAllCountries_west, populistWaveAllCountries_eastSouth=populistWaveAllCountries_eastSouth, populistWaveAllCountries_westNorth=populistWaveAllCountries_westNorth, populistWaveAllCountries_average=populistWaveAllCountries_average, names_for_graph=names_for_graph)
attr(profiles, "post_lasso_seed") <- post_lasso_seed
saveRDS(profiles, file.path(cleandata_path, "ivs", "prrp_profiles.rds"))
