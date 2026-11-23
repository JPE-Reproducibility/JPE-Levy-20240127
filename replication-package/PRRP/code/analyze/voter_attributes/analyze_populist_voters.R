###
# Are populist parties' voters similar across countries?
# investigate using the last year
###

library(tidyverse)
library(haven)
library(ggplot2)
library(conflicted)
library(Hmisc)
library(ggrepel)
library(savetexvalue)
conflict_prefer("summarize", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("coalesce", "dplyr")
conflict_prefer("wtd.var", "Hmisc")
conflict_prefer("if_else", "dplyr")

set.seed(2022)


# *************************************************************************
# SETUP ----
# *************************************************************************

rm(list = ls())

# Load functions
source("code/util/commonVars.R")
source("code/analyze/voter_attributes/corFunctions.R")
source("code/analyze/party_positions/manifesto_general_functions.R")
outdir <- paste0(github_path, "output/figures/WVS/populist_voters/")

outcome <- "PRRP"
outcome_var <- "Y_FR_Pop"
weighted_suf <- "_weighted.pdf"
prrp_profiles <- readRDS(file.path(cleandata_path, "ivs/prrp_profiles.rds"))
if (is.null(attr(prrp_profiles, "post_lasso_seed"))) stop("Prepare PRRP inputs with the current preprocessing code first.")
assign(".Random.seed", attr(prrp_profiles, "post_lasso_seed"), envir=.GlobalEnv)
list2env(prrp_profiles, envir=environment())
# take only n most correlated
res_top20 <- res_full %>%
  slice_head(n = 20)


xlabel <- paste0("Correlation with ", outcome, " Score")

plot <- ggplot(res_top20, aes(y = reorder(plot_name_signed, cor_abs), x = cor_abs)) +
  geom_point() +
  ylab("") +
  xlab(xlabel)

# Used in overleaf
ggsave(
  plot = plot + theme_classic(base_size = 12),
  paste0(outdir, outcome, "_cor", weighted_suf), width = figureWidth, height = 0.9 * figureWidth, units = "cm"
)







######################################
# Figure A.6 - Voters’ PRRP Score By Country
######################################

# Plot the entire distribution at wave 4 and wave 6 (density)
plot <- data %>%
  filter(BB_Wave_num != 5) %>%
  ggplot(aes(x = outcome_score_std)) +
  scale_color_manual(name = element_blank(), values = rev(c("#E69F00", "#0072B2"))) +
  scale_linetype_manual(name = element_blank(), values = rev(c("solid", "31"))) +
  theme_classic() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), name = "Density") +
  labs(x = "PRRP Score, Standard Deviations")

# Paper version
ggsave(
  plot = plot + theme(legend.position = "bottom") + facet_wrap(~BB_Country) +
    stat_density(aes(color = as.factor(waveLabels), linetype = as.factor(waveLabels), weight = BB_Weight_CW),
                 geom = "line", position = "identity", size = 0.75) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    geom_vline(data = ceil_floor_dat, aes(xintercept = ceil), linetype = "dotted", color = "black") +
    geom_vline(data = ceil_floor_dat, aes(xintercept = floor), linetype = "dotted", color = "black"),
  filename = paste0(outdir, outcome, "_score_dist_density_country_ceil", weighted_suf),
  width = figureWidth, height = figureWidth, units = "cm"
)

# Slides version


# same graph just for 4 countries
countries_list <- c("Germany", "Hungary", "UK", "Italy")

plot <- data %>%
  filter(BB_Wave_num != 5 & BB_Country %in% countries_list) %>%
  ggplot(aes(x = outcome_score_std)) +
  scale_color_manual(name = element_blank(), values = rev(c("#E69F00", "#0072B2"))) +
  scale_linetype_manual(name = element_blank(), values = rev(c("solid", "31"))) +
  theme_classic() +
  labs(x = "PRRP Score (in std. dev. units)") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), name = "Density")

# analysis by east and west
data_ew <- bind_rows(data_east, data_west) %>% 
  mutate(outcome_score = coalesce(outcome_score_east, outcome_score_west),
         outcome_score_stdNoWt = coalesce(outcome_score_stdNoWt_east, outcome_score_stdNoWt_west),
         outcome_score_std = coalesce(outcome_score_std_east, outcome_score_std_west)) %>% 
  select(-c(outcome_score_std_east, outcome_score_std_west,
            outcome_score_stdNoWt_east, outcome_score_stdNoWt_west,
            outcome_score_east, outcome_score_west))

plot_ew <- data_ew %>%
  filter(BB_Wave_num != 5) %>%
  ggplot(aes(x = outcome_score_std)) +
  scale_color_manual(name = element_blank(), values = rev(c("#E69F00", "#0072B2"))) +
  scale_linetype_manual(name = element_blank(), values = rev(c("solid", "31"))) +
  theme_classic() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), name = "Density") +
  labs(x = "PRRP Score, Standard Deviations")+ 
  theme(legend.position = "bottom") + facet_wrap(~BB_Country) +
  stat_density(aes(color = as.factor(waveLabels), linetype = as.factor(waveLabels), weight = BB_Weight_CW),
               geom = "line", position = "identity", size = 0.75) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(data = ceil_floor_dat_ew, aes(xintercept = ceil), linetype = "dotted", color = "black") +
  geom_vline(data = ceil_floor_dat_ew, aes(xintercept = floor), linetype = "dotted", color = "black")

# *************************************************************************
#  PREPARE DATA FOR PLOTS ----
# *************************************************************************

# Aggregate all countries --------------------------------------------------

############################################################
# Check if PRRP score changes correlate with PRRP Support
############################################################

## Prepare PRRP score change
cty_PRRP_score_chage <- populistWaveAllCountries %>%
  filter(BB_Wave_num == 6)

## Prepare PRRP vote share
# read data
raw_data <- readRDS(file = paste0(cleandata_path, "/manifesto/manifesto_populist.rds"))

# read functions
source("code/analyze/party_positions/manifesto_general_functions.R")



# Limit sample and variables
df <- raw_data %>%
  dplyr::filter(manifestoYear >= 1980) %>%
  mutate(manifesto_country_name =
           if_else(manifesto_country_name == "United Kingdom", "UK", manifesto_country_name)) %>%
  select(manifestoYear, date, manifesto_country_name, pervote, Y_FR_Pop)

# Add dummy for prrp with at least 1%, adjust pervote to make the sum of pervote in the data 100%
df <- df %>%
  mutate(prrp_great_1 = pervote >= 1 & Y_FR_Pop == 1) %>% 
  group_by(date, manifesto_country_name) %>%
  mutate(adj_pervote = pervote / sum(pervote, na.rm = T)) %>%
  ungroup() 

# For every election calculate vote share for PRRP, number of PRRP with pervote>=1%
# and indicator for existence of at least one such party
df_prrp <- df %>%
  group_by(date, manifesto_country_name, manifestoYear) %>%
  summarize(adj_pervote = sum(adj_pervote * Y_FR_Pop, na.rm = T)) %>%
  ungroup()

# Add non-election years
df_prrp <- expand.grid(manifestoYear = 1980:2021,
                       manifesto_country_name = unique(data$BB_Country)) %>% 
  left_join(df_prrp) %>% 
  select(!date)

# Compute 5-years moving avg within each country
df_prrp <- df_prrp %>%
  arrange(manifestoYear) %>%
  group_by(manifesto_country_name) %>%
  mutate(across(c(adj_pervote), ~ rolling_avg_pop(var = .x, years = manifestoYear))) %>%
  ungroup() %>% 
  # drop duplicate years (2 election cycles would create 2 identical rows)
  distinct()

# Compute Changes from 2005 to 2017
cty_prrp_change <- df_prrp %>%
  mutate(year=manifestoYear)%>%
  filter(year == 2005 | year == 2017) %>%
  mutate(pervote_2017_temp = if_else(year == 2017, adj_pervote, NA_real_)) %>%
  group_by(manifesto_country_name) %>%
  mutate(prrp_vote_share_2017 = mean(pervote_2017_temp, na.rm = T)) %>%
  ungroup() %>%
  mutate(change_prrp_vote_share = prrp_vote_share_2017 - adj_pervote) %>%
  filter(year == 2005) %>%
  rename(
    BB_Country = manifesto_country_name,
    prrp_vote_share_2005 = adj_pervote
  ) %>%
  select(c("BB_Country", "change_prrp_vote_share", "prrp_vote_share_2005", "prrp_vote_share_2017"))

## Combine them and run regressions, plot scatter
df_cty_prrp_vote_score <- left_join(cty_prrp_change, cty_PRRP_score_chage)


df_cty_prrp_vote_score_nohun= df_cty_prrp_vote_score%>%
  filter(BB_Country!="Hungary")

score_corr_support = ggplot(df_cty_prrp_vote_score, aes(x = av, y = change_prrp_vote_share)) +
  geom_point() +
  theme_classic() +
  theme_classic(base_size = 10) +
  xlab("Changes in PRRP Score, Standard Deviations") +
  ylab("Changes in PRRP Voting Share") +
  geom_text_repel(aes(label = BB_Country),
    size = 3
  )
# geom_smooth(method=lm, se=FALSE)
ggsave(paste0(outdir, outcome, "_score_corr_support", weighted_suf), 
       score_corr_support, width = figureWidth, height = 0.9 * figureWidth, units = "cm")





diffScore <- populistWave %>%
  filter(type == "m" & BB_Wave_num == 6) %>%
  pull(value) -
  populistWave %>%
  filter(type == "m" & BB_Wave_num == 4) %>%
  pull(value)

diffActualPop <- data %>%
  filter(.data[[outcome_var]] == 1) %>%
  {
    weighted.mean(.$outcome_score_std, w = .$BB_Weight_CW)
  } -
  data %>%
  filter(.data[[outcome_var]] == 0) %>%
  {
    weighted.mean(.$outcome_score_std, w = .$BB_Weight_CW)
  }

### Compare difference in score to
save_tex_value(
  values = diffScore / diffActualPop,
  names = "trendScoreShareOfDiffUnweighted",
  file_name = "output/values/values",
  percent = TRUE
)

############################################################
# PRRP score / vote by Age / Cohort
############################################################

# Build a reusable profile plot by binned x-variable (age or cohort) for waves 4 and 6.
plot_profile_by_x <- function(df, x_var, x_label, y_var, y_label, file_name, y_labels = waiver()) {
  # Prepare wave-specific binned summaries: mean x within bin and weighted mean of outcome.
  p <- df %>%
    filter(BB_Wave_num %in% c(4, 6)) %>%
    group_by(BB_Wave_num) %>%
    mutate(
      x_bin = ntile(.data[[x_var]], 25)
    ) %>%
    ungroup() %>%
    group_by(BB_Wave_num, x_bin) %>%
    summarise(
      mean_x = mean(.data[[x_var]], na.rm = TRUE),
      y_value = weighted.mean(.data[[y_var]], BB_Weight_CW, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    ggplot(
      aes(
        x = mean_x,
        y = y_value,
        color = factor(BB_Wave_num),
        shape = factor(BB_Wave_num),
        linetype = factor(BB_Wave_num)
      )
    ) +
    geom_point(size = 2) +
    geom_smooth(method = "loess", se = FALSE) +
    theme_classic() +
    scale_y_continuous(name = y_label, labels = y_labels) +
    scale_color_manual(
      values = c("4" = "#0072B2", "6" = "#E69F00"),
      labels = c("2005-2009", "2017-2020")
    ) +
    scale_shape_manual(
      values = c(16, 2),
      labels = c("2005-2009", "2017-2020")
    ) +
    scale_linetype_manual(
      values = c("solid", "dashed"),
      labels = c("2005-2009", "2017-2020")
    ) +
    labs(x = x_label, color = "Wave", shape = "Wave", linetype = "Wave") +
    theme(legend.position = "bottom") +
    guides(
      color = guide_legend(title = NULL),
      shape = guide_legend(title = NULL),
      linetype = guide_legend(title = NULL)
    )

  # Save the figure with standard project dimensions.
  ggsave(
    p,
    filename = paste0(outdir, file_name),
    width = figureWidth,
    height = figureWidth,
    units = "cm"
  )
}
