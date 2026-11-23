#Heterogeneity in Priorities


#Step 1: Calculate the probability in support for PRRP for every individual in wave 6 

# *************************************************************************
# SETUP ----
# *************************************************************************
# 

rm(list = ls())
source("code/util/commonVars.R")
data <- readRDS(file.path(cleandata_path, "ivs", "prrp_profiles.rds"))$data
source("code/model/clean_data_model.R")
source("code/model/func_nuclear_and_pgd.R")
source("code/analyze/priorities_and_model/func_plot_weight.R")
# Keep PRRP scores from wave 6 only so the subgroup averages match the wave-6 decomposition sample.
df_wvs_pop_score=data%>%
  filter(BB_Wave_num == 6) %>%
  select("BB_Id","outcome_score","outcome_score_std")

outdir <- "output/figures/decomposition/"

# Taking only waves 4,5,6
grps <- 4:6


filepath <- paste0(model_results_path, "/model_wave_")

filesuffix <- "" #Can change if for some reason want to look at a different model

# *************************************************************************
# Load Data ----
# *************************************************************************
# 

# Load X and Z used in the data
X.list <- lapply(grps, function(g) {
  readRDS(paste0(filepath, g, filesuffix, ".Rds"))$X
})
Z.list <- lapply(grps, function(g) {
  readRDS(paste0(filepath, g, filesuffix, ".Rds"))$Z
})

# Load model estimates
mod.list <- lapply(grps, function(g) {
  readRDS(paste0(filepath, g, filesuffix, ".Rds"))$mod
})
phi.list <- lapply(mod.list, function(m) {
  m$phi
})

# choose the last estimated phi for the dimension reduction in the second stage
phi <- phi.list[[length(phi.list)]]

# sampling weights (also used for the weighted SD of X below)
W.list <- lapply(grps, function(g) {
  readRDS(paste0(filepath, g, filesuffix, ".Rds"))$W
})

# account for standardization. after this, phi can multiply X and Z themselves (not standardized)
# X was standardized with the weighted SD in estimation (wt.scale), Z with the unweighted SD (scale)
phi.list <- lapply(grps - min(grps) + 1, function(g) {
  diag(inv.wt.sd(X.list[[g]], W.list[[g]])) %*%
    phi.list[[g]] %*%
    diag(apply(Z.list[[g]], 2, sd)^-1)
})

# Option Sets
X.os.list <- lapply(grps, function(g) {
  readRDS(paste0(filepath, g, filesuffix, ".Rds"))$X.os
})
Z.os.list <- lapply(grps, function(g) {
  readRDS(paste0(filepath, g, filesuffix, ".Rds"))$Z.os
})

# Other variables
Y.list <- lapply(grps, function(g) {
  readRDS(paste0(filepath, g, filesuffix, ".Rds"))$Y
})
FE.list <- lapply(mod.list, function(m) {
  m$intercept
})

# *************************************************************************
# Clean Data ----
# *************************************************************************
# 

# demean all Xs by the global mean
mean.X.wave <- lapply(grps - min(grps) + 1, function(g) t(apply(X.list[[g]], 2, function(x) weighted.mean(x, W.list[[g]]))))
mean.X.global <- (Reduce("+", mean.X.wave) / length(grps))[1, ]
X.list <- lapply(grps - min(grps) + 1, function(g) t(t(X.list[[g]]) - mean.X.global))

# change to get the intercept without standardization
# the intercept is the utility for the voter with X=0. It was estimated when 0 was the wave average. Change to the average across all waves.
# do this by adding the difference in utility of the wave average and the global average
FE.list <- lapply(grps - min(grps) + 1, function(g) {
  t(FE.list[[g]] -
      (mean.X.wave[[g]] - mean.X.global) %*%
      phi.list[[g]] %*% t((Z.list[[g]])))
})

# Create dictionary from party name in each wave to a consistent name in all waves
Z.all$nms <- paste0(Z.all$Y_manifesto_key)
key <- Z.all[, c("nms", "Y_manifesto_party_id")]
key <- key[!duplicated(key), ]
rownames(key) <- key$nms
party.id.list <- lapply(Z.list, function(z) {
  v <- as.character(key[
    substr(rownames(z), 1, nchar(rownames(z)) - 1),
    "Y_manifesto_party_id"
  ])
  names(v) <- rownames(z)
  v
})

# verify no NA
for (i in grps - min(grps) + 1) {
  stopifnot(!any(is.na(party.id.list[[i]])))
}

# drop wave number for os list
X.country <- lapply(X.os.list, function(d) sapply(d, function(s) substr(s, 1, nchar(s) - 1)))
Z.country <- lapply(Z.os.list, function(d) sapply(d, function(s) substr(s, 1, nchar(s) - 1)))

# data set with party id, country and whether the party is considered PRRP

manifesto_populist <- readRDS(paste0(cleandata_path, "/manifesto/manifesto_populist.rds")) %>%
  distinct(Y_manifesto_party_id, .keep_all = TRUE) %>% 
  select(Y_manifesto_party_id, parfam)

X.all <- X.all %>% 
  left_join(manifesto_populist) 

party.dat <- unique(X.all[, c("Y_manifesto_party_id", "BB_Country", "Y_FR_Pop")])
party.dat <- party.dat |>
  mutate(Y_manifesto_party_id = as.character(Y_manifesto_party_id)) |>
  as.data.frame()
rownames(party.dat) <- party.dat[, 1]
party.dat <- party.dat[, -1]


# *************************************************************************
# Decomposition ----
# *************************************************************************
k = 5
quad = F

#Counterfactuals 

# turn into matrices
X.list <- lapply(X.list, as.matrix)
Z.list <- lapply(Z.list, as.matrix)

# name of waves
nms <- 1:length(X.list)
# name of parties
parties <- unique(unlist(party.id.list))
# set wave names for each component


# create final data
res <- data.frame(row.names = parties)


## decompose FEs
beta.list <- list()
# create list of weights for overall support in each party
wgt.list <- lapply(1:length(Y.list), function(i) {
  t(apply(Y.list[[i]] * W.list[[i]], 2, sum))
})
# second stage estimation decomposes the deltas into beta,z, and residual
FE.dec <- Z.coeff(Z.list, Z.country, party.id.list, wgt.list, phi, FE.list, k, quad = quad)
vel.list <- list()

# create lists by wave also for beta, and the residual baesd on the estimation results
for (i in 1:length(nms)) {
  # list of betas
  beta.list[[nms[i]]] <- FE.dec[["beta"]][, i]
  # total residual is the sum of the party fixed effect (constant over waves) and the unexplained part (changes over waves)
  vel.list[[nms[i]]] <- FE.dec[["FE"]][as.character(party.id.list[[nms[i]]])] + FE.dec[["eps"]][[i]]
  #
  vel.list[[nms[i]]][is.na(vel.list[[nms[i]]])] <- 0
}
# expand data to include all parties even when they don't run
EP <- expand.parties(Z.list, vel.list, Z.country, party.id.list)
# party data including non participants
Z.new <- EP[[1]]
# expand residual (-Inf if party did not participate)
vel.new <- EP[[2]]
# country for each party. now this is constant across waves since we have a consistent set of countries
Z.group <- EP[[3]]
# loop over waves and calculate total support for every party
# calculate counterfactuals based on what is fixed and what changes
# res is data of support for each party over the waves
X = X.list[[3]]
X.group = X.country[[3]]
Z = Z.new[[3]]
velance = vel.new[[3]]
PRRP.ind = party.dat[rownames(Z),2]
PRRP.ind = which(PRRP.ind==1)

X.final = X.all[X.all$BB_Wave_num==6,]

for (i in c(1,3)) {
  beta = beta.list[[i]]
  phi = phi.list[[i]]
  
  #simulate model 
  # get delta
  zeta <- Z %*% beta + velance
  # simulate the model
  logP <- simulate.model(X, Z, phi, zeta, X.group, Z.group)[[1]]
  P <- exp(logP)
  #PRRP vote: 
  PRRP.support = rowSums(P[,PRRP.ind])
  X.final[,paste0("Prob.",i)] = PRRP.support
}
  
#At this point X.final includes the probability to vote for PRRP in each counterfactual (Prob.1 and Prob.3). 
# We now aggregate by subgroups using the same wave-6 sample as the decomposition.
X.final = left_join(X.final, df_wvs_pop_score, by = c("BB_Id"))
X.final = add_F_vars(X.final)

# Build the subgroup indicators used in both the support and PRRP-score summaries.
X.final$did_not_complete_college = X.final$F_edu == "Did Not Complete College"
X.final$complete_college = X.final$F_edu == "College"
X.final$below_median_income = X.final$F_income == "Below Median Income"
X.final$above_median_income = X.final$F_income == "Above Median Income"
X.final$age_18_50 = X.final$F_age == "18-50"
X.final$age_50_plus = X.final$F_age == "51+"
X.final$not_union_member = X.final$F_union == "Not a Union Member"
X.final$union_member = X.final$F_union == "Union Member"
X.final$rural = X.final$F_bigtown == "Rural"
X.final$urban = X.final$F_bigtown == "Urban"
X.final$above_median_prrp_score = X.final$F_PRRPscore_std_right == "Above Median PRRP Score"
X.final$below_median_prrp_score = X.final$F_PRRPscore_std_right == "Below Median PRRP Score"
X.final$female_indicator = 1 - X.final$X_dem_male
X.final$full_population = 1
X.final$rise_prrp = X.final$Prob.3 - X.final$Prob.1

# Map output names to the subgroup indicators once and reuse them below.
subgroup_vars <- c(
  Full.pop = "full_population",
  male = "X_dem_male",
  female = "female_indicator",
  age_50_plus = "age_50_plus",
  age_18_50 = "age_18_50",
  no_college = "did_not_complete_college",
  college = "complete_college",
  below_median_income = "below_median_income",
  above_median_income = "above_median_income",
  union_member = "union_member",
  not_union_member = "not_union_member",
  rural = "rural",
  urban = "urban",
  above_median_prrp_score = "above_median_prrp_score",
  below_median_prrp_score = "below_median_prrp_score"
)

# Calculate a country-level weighted mean for any value within any subgroup.
calc_country_subgroup_mean <- function(data, value_var, indicator_var = "full_population") {
  data %>%
    group_by(BB_Country) %>%
    summarise(
      subgroup_weight = sum(BB_Weight_CW * .data[[indicator_var]], na.rm = TRUE),
      value = sum(.data[[value_var]] * BB_Weight_CW * .data[[indicator_var]], na.rm = TRUE) /
        subgroup_weight,
      .groups = "drop"
    ) %>%
    mutate(value = if_else(subgroup_weight > 0, value, NA_real_)) %>%
    select(BB_Country, value)
}

# Start with actual PRRP support and add the counterfactual rise for each subgroup.
# Compute country-level support and store it as the baseline weight.
sup <- calc_country_subgroup_mean(X.final, "Prob.3") %>%
  rename(weight = value)

# Invert the weight values.
sup$weight <- ifelse(sup$weight > 0, 1 / sup$weight, 0)

for (group_name in names(subgroup_vars)) {
  subgroup_summary <- calc_country_subgroup_mean(X.final, "rise_prrp", subgroup_vars[[group_name]])
  sup <- left_join(sup, subgroup_summary %>% rename(!!group_name := value), by = "BB_Country")
}

# Step 3: Take the weighted average by country and compare each subgroup to the full population.
demographic_vars <- colnames(sup)[-(1:2)]  # All subgroup columns from 'Full.pop' onwards
weighted_avg <- data.frame(variable = demographic_vars)

# Calculate inverse-prrp-support-country-weighted subgroup averages for the counterfactual rise.
weighted_avg$weighted_mean <- sapply(demographic_vars, function(var) {
  weighted.mean(sup[[var]], sup$weight, na.rm = TRUE)
})

# Compare to weighted average of Full.pop
full_pop_avg <- weighted_avg$weighted_mean[1]

# Express each subgroup relative to the full-population average.
weighted_avg$diff_from_full_pop <- weighted_avg$weighted_mean / full_pop_avg

# Calculate each subgroup's average PRRP score using the standard IVS/country weights.
weighted_avg$avg_prrp_score <- sapply(demographic_vars, function(var) {
  indicator_var <- subgroup_vars[[var]]
  weighted.mean(
    X.final$outcome_score_std,
    w = X.final$BB_Weight_CW * X.final[[indicator_var]],
    na.rm = TRUE
  )
})

print(weighted_avg)

# Step 4: Plot diff_from_full_pop in paired bars with gaps between pairs
pair_vars <- list(
  c("age_50_plus", "age_18_50"),
  c("male", "female"),
  c("no_college", "college"),
  c("below_median_income", "above_median_income"),
  c("rural", "urban"),
  c("union_member", "not_union_member"),
  c("above_median_prrp_score", "below_median_prrp_score")
)

category_labels <- c(
  age_50_plus = "Age 51+",
  age_18_50 = "Age 18-50",
  male = "Male",
  female = "Female",
  no_college = "No College",
  college = "College",
  below_median_income = "Below-Median Income",
  above_median_income = "Above-Median Income",
  rural = "Rural",
  urban = "Urban",
  union_member = "Union",
  not_union_member = "Not Union",
  above_median_prrp_score = "Above-Median PRRP Score",
  below_median_prrp_score = "Below-Median PRRP Score"
)

plot_df <- do.call(rbind, lapply(seq_along(pair_vars), function(i) {
  vars <- pair_vars[[i]]
  pair_names <- c("Age", "Gender", "College", "Income", "Rural", "Union", "PRRP Score")
  data.frame(
    variable = vars,
    pair_id = i,
    pair_name = pair_names[i],
    within_pair = seq_along(vars),
    stringsAsFactors = FALSE
  )
}))

plot_df <- left_join(plot_df, weighted_avg, by = "variable")

# x positions: each pair uses two adjacent bars, then one empty slot as a gap
plot_df$x <- (plot_df$pair_id - 1) * 3 + plot_df$within_pair
plot_df$category_label <- category_labels[plot_df$variable]

# Match colors used in hetero_prior_change.pdf (ggplot default hue palette by group)
pair_colors <- c(
  "Age" = "#00BFC4",
  "Gender" = "#00BE67",
  "College" = "#CD9600",
  "Income" = "#7CAE00",
  "Rural" = "#C77CFF",
  "Union" = "#00A9FF",
  "PRRP Score" = "#FF61CC"
)

bar_plot <- ggplot(plot_df, aes(x = x, y = diff_from_full_pop, fill = pair_name)) +
  geom_col(width = 0.8, color = "black") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
  scale_fill_manual(values = pair_colors) +
  scale_x_reverse(breaks = plot_df$x, labels = plot_df$category_label) +
  coord_flip() +
  labs(
    x = "",
    y = "Counterfactual Rise in PRRP Support (Relative to Full Population)"
  ) +
  theme_bw() +
  theme(
    axis.text.y = element_text(hjust = 1),
    legend.position = "none"
  )

print(bar_plot)

# Reuse the same layout to show subgroup average PRRP scores.
prrp_score_plot <- ggplot(plot_df, aes(x = x, y = avg_prrp_score, fill = pair_name)) +
  geom_col(width = 0.8, color = "black") +
  scale_fill_manual(values = pair_colors) +
  scale_x_reverse(breaks = plot_df$x, labels = plot_df$category_label) +
  coord_flip() +
  labs(
    x = "",
    y = "Average PRRP Score"
  ) +
  theme_bw() +
  theme(
    axis.text.y = element_text(hjust = 1),
    legend.position = "none"
  )

print(prrp_score_plot)

ggsave(
  filename = paste0(outdir, "weighted_avg_diff_from_full_pop_bar.pdf"),
  plot = bar_plot,
  device = "pdf",
  width = figureWidth,
  height = 15,
  units = "cm"
)

# Save the subgroup PRRP score figure next to the existing decomposition plot.
ggsave(
  filename = paste0(outdir, "demo_prrp_score_bar.pdf"),
  plot = prrp_score_plot,
  device = "pdf",
  width = figureWidth,
  height = 15,
  units = "cm"
)
