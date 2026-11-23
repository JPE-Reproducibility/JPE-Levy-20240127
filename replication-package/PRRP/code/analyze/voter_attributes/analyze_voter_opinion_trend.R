# *************************************************************************
# File description ----
# *************************************************************************
#
# This file analyzes trends in voter opinions across waves and creates
# visualizations showing how public attitudes have evolved over time.
#
# The main goal is to examine changes in populist-relevant opinions including:
#   (1) Immigration attitudes (neighbors, jobs prioritization)
#   (2) Institutional confidence (EU, UN, justice system)
#   (3) Social attitudes (abortion, homosexuality)
#   (4) Social trust (trust in other people)
#   (5) PRRP voter scores over time
#
# The analysis is performed across multiple subgroups:
#   - Overall population (all countries)
#   - Eastern Europe countries
#   - Western Europe countries
#   - Individual countries
#
# Outputs:
#
# Figures:
#   - Line plots showing opinion trends across waves
#   - Country-level and aggregate visualizations
#   - Regional comparisons (East vs West)
#   - Appendix figure of 20 most-changed opinions
#
# TeX Values:
#   - Last wave PRRP score (average across countries)
#
#
# Clear the R workspace
rm(list=ls())

# *************************************************************************
# SETUP ----
# *************************************************************************

library(haven) # For dta import
library(tidyverse)
library(savetexvalue)
source("code/util/commonVars.R")
#conflict_prefer("filter", "dplyr")
conflict_prefer("wtd.mean", "Hmisc")
conflict_prefer("wtd.var", "Hmisc")
REFEREE_ONLY <- TRUE

# Load function
source("code/analyze/voter_attributes/corFunctions.R")

#read data from dropbox:
data <- read_dta(file = pathWVS_Data)

#output directory:
outdir = paste0(github_path, "output/figures/WVS/opinions_trend/") 
ranking_artifact = paste0(cleandata_path, "/ivs/prrp_score_covariate_ranking.rds")


# *************************************************************************
# OPINIONS THAT CHANGED THE MOST ----     
# *************************************************************************

# focus only on opinion variables
X_op = data %>%
  select(starts_with(paste0("X_op"))) 

#normalize so coefficients will be comparable
normalize <- function(x, w = rep(1, length(x))){
  (x - Hmisc::wtd.mean(x, w, na.rm = T)) / sqrt(Hmisc::wtd.var(x, w, na.rm = T))
}

X_op = apply(X_op, 2, normalize)


gdat = data.frame(var = colnames(X_op), trend = NA, se = NA)

year = data %>% pull(BB_Year)
country = data %>% pull(BB_Country) %>% as.factor()

for(i in 1:ncol(X_op)){
  y = X_op[,i, drop=TRUE]
  reg = lm(y ~ year + country, weights = data$BB_Weight_CW)
  res = summary(reg)$coefficients
  gdat$trend[i] = res["year", "Estimate"]
  gdat$se[i] = res["year", "Std. Error"]
}


# Merge with plot names
gdat = gdat %>%
  left_join(names_for_graph, by = c("var" = "name"))

# Add sign , make sure all variables have a corresponding plot name
stopifnot(any(!is.na(gdat$plotName)))

gdat = gdat %>%
  mutate(plotName = add_sign(plotName, sign(trend)))


# Switch to absolute terms, add CI and order

# t distribution quartile
t_quartile <- qt(0.975, nrow(X_op))

gdat = gdat %>%
  mutate(trend = abs(trend),
         lower_ci = trend - t_quartile*se,
         upper_ci = trend + t_quartile*se,
         plotName = factor(plotName, 
                           levels = plotName[order(trend)], 
                           ordered = T)) %>% 
  arrange(desc(trend))

# We have around 80 variables, so we want to use the first 20
gdat_small = gdat[1:20,]

#Create appendix figure showing the 20 opinions that changed the most
# Figure limits:
minx = min(gdat$lower_ci)
maxx = max(gdat$upper_ci)

currentPlot = ggplot(gdat_small, aes(y = plotName)) + 
  geom_pointrange(aes(x = trend, xmin = lower_ci, xmax = upper_ci), size=0.25) +
  xlab("Average Yearly Changes in Opinions (in SD units)") + ylab("") +
  ggtitle("") +
  geom_vline(xintercept = 0, color = "red") +
  xlim(c(minx, maxx))

ggsave(plot = currentPlot + theme_classic(base_size = 12),
     "opinions_average_trend_1st_half.pdf", path=outdir, width = figureWidth, height = figureWidth, units="cm")



# *************************************************************************
# PREPARE DATA FOR 22 COUNTRIES + AVERAGE PLOT ----     
# *************************************************************************

# Figure 9 / fig:populist_score_agg
# Select the top opinion variables most associated with the PRRP score.
prrp_score_ranking <- read_rds(ranking_artifact)

eligible_opinions <- prrp_score_ranking %>%
  filter(is_opinion, is_trend_eligible) %>%
  arrange(rank, variable) %>%
  distinct(variable, .keep_all = TRUE)

stopifnot(nrow(eligible_opinions) >= 8)

selected_issue_metadata <- eligible_opinions %>%
  slice_head(n = 8) %>%
  transmute(
    variable,
    issue = display_label,
    cor_sign,
    rank
  )

stopifnot(n_distinct(selected_issue_metadata$issue) == 8)

selected_opinion_variables <- selected_issue_metadata$variable
negative_opinion_variables <- selected_issue_metadata %>%
  filter(cor_sign < 0) %>%
  pull(variable)

figure_issue_order <- c("PRRP score", selected_issue_metadata$issue)
panel_rows <- 3
paper_plot_height <- figureWidth

flip_toward_prrp <- function(x) {
  if (all(is.na(x))) {
    return(x)
  }

  max(x, na.rm = TRUE) - x
}

df_ops <- data %>%
  select(BB_Country, BB_Int_Wave, BB_Wave_num, BB_Weight_CW, all_of(selected_opinion_variables)) %>%
  mutate(across(all_of(negative_opinion_variables), flip_toward_prrp))


# Summarize mean and sd for each wave
df_ops <- zap_formats(df_ops)

df_ops <- df_ops %>%
  pivot_longer(cols = all_of(selected_opinion_variables), names_to ="variable", values_to = "value") %>%
  ungroup() %>%
  group_by(BB_Country, BB_Int_Wave, BB_Wave_num, variable) %>%
  summarise(
    mean = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    wtd_mean = weighted.mean(value, BB_Weight_CW, na.rm = TRUE),
    wtd_sd = sqrt(Hmisc::wtd.var(value, weights = BB_Weight_CW, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  left_join(selected_issue_metadata %>% select(variable, issue), by = "variable") %>%
  group_by(BB_Country, variable, issue) %>%
  arrange(BB_Wave_num) %>%
  mutate(
    mean_wave_4 = mean[match(4, BB_Wave_num)],
    sd_wave_4 = sd[match(4, BB_Wave_num)],
    wtd_mean_wave_4 = wtd_mean[match(4, BB_Wave_num)],
    wtd_sd_wave_4 = wtd_sd[match(4, BB_Wave_num)],
    value = (mean - mean_wave_4) / sd_wave_4,
    wtd_value = (wtd_mean - wtd_mean_wave_4) / wtd_sd_wave_4
  ) %>%
  ungroup() %>%
  filter(
    !is.na(sd_wave_4), sd_wave_4 > 0,
    !is.na(wtd_sd_wave_4), wtd_sd_wave_4 > 0,
    is.finite(value), is.finite(wtd_value)
  ) %>%
  select(BB_Country, BB_Int_Wave, BB_Wave_num, issue, value, wtd_value) %>%
  mutate(issue = factor(issue, levels = selected_issue_metadata$issue))

stopifnot(n_distinct(df_ops$issue) == 8)


# Create an aggregated d.f
df_agg <- df_ops %>%
  group_by(BB_Int_Wave, issue)%>%
  summarise(value = mean(value), wtd_value = mean(wtd_value))%>%
  ungroup()

#add average to 22 countries data

add_average <- function(df_agg){ 
  df <- df_ops %>%
  bind_rows(df_agg) %>%
  mutate(BB_Country = if_else(is.na(BB_Country), "Average" ,BB_Country)) %>% 
  group_by(BB_Country,issue) %>%
  arrange(BB_Int_Wave) %>%
  mutate(color = if_else(BB_Country == "Average", 2, 1))%>%
  arrange(color) %>%
  ungroup() %>%
  mutate(waveNum = as.numeric(substr(BB_Int_Wave, 10, 10))) %>%
  mutate(index = waveNum - 3) %>%
  filter(!(BB_Country == "Average" & waveNum == 5)) %>%
  left_join(waveLabelsDF, keep = FALSE) %>% 
  select(BB_Country, issue, wtd_value, color, index, waveLabels, waveNum)
  
  return(df)
}

fig_dat <- add_average(df_agg)
# Figure 9 / fig:populist_score_agg outputs
prrp_score <- read_rds(paste0(cleandata_path, "/ivs/prrp_score.rds"))

# Select relevant cols
select_rel_cols <- function(fig_dat, prrp_score, issue_order){
fig_dat <- fig_dat %>% 
  select(BB_Country, issue, wtd_value, color, index, waveLabels)

prrp_score_small <- prrp_score %>% 
  ungroup() %>% 
  mutate(issue = "Voters' PRRP Score Over Time") %>% 
  rename(wtd_value = av, waveNum = BB_Wave_num) %>% 
  mutate(waveNum = as.numeric(waveNum)) %>%
  filter(!(BB_Country %in% c("Average", "EastSouth Aggregate", "WestNorth Aggregate") & waveNum == 5)) %>%
  left_join(unique(fig_dat[ , c("waveLabels", "index")]), by = "waveLabels", multiple = "all") %>% 
  select(BB_Country, issue, wtd_value, color, index, waveLabels) %>% 
  distinct()

PopOpinionsOverTime <- bind_rows(fig_dat, prrp_score_small)

PopOpinionsOverTime <- PopOpinionsOverTime %>%
  mutate(issue = if_else(issue == "Voters' PRRP Score Over Time", "PRRP score", issue))

PopOpinionsOverTime <- PopOpinionsOverTime %>% 
  filter(issue %in% issue_order)

PopOpinionsOverTime$issue <- factor(PopOpinionsOverTime$issue, levels = issue_order)

return(PopOpinionsOverTime)
}

PopOpinionsOverTime <- select_rel_cols(fig_dat, prrp_score, figure_issue_order)
stopifnot(n_distinct(PopOpinionsOverTime$issue) == length(figure_issue_order))
theme <-  theme(axis.text.x = element_text(angle = 90), legend.position = c(0.75, 0.1),
                strip.background = element_blank(), strip.text.x = element_text(face = "bold"))

themeBottom_fixed <- theme + theme(
  legend.position = "bottom", 
  legend.box.spacing = unit(0.75, "cm"),
  legend.background = element_rect(fill = alpha("white", 0)),
  plot.margin = ggplot2::margin(t = 10, r = 10, b = 5, l = 10)
)

# Populism over time - paper
populismOverTime <- function(DT) {
  ggplot(DT, aes(x = index, y = wtd_value, color = as.factor(color),
             alpha = as.factor(color), linetype = BB_Country,
             size=as.factor(color)))+
    geom_line()+
    geom_point(size = 1)+
    scale_linetype_manual(name = "", values = rep("solid",length(unique(fig_dat$BB_Country))), guide = "none") +
    scale_size_manual(name ="", breaks=c(2, 1), values = c(1, 0.25), labels=c("Aggregate", "Individual Countries"))+
    scale_color_manual(name ="", breaks=c(2, 1), values = c("navy", "grey"), labels=c("Aggregate", "Individual Countries"))+
    scale_alpha_manual(name ="", breaks=c(2, 1), values = c(1, 0.7), labels=c( "Aggregate", "Individual Countries"))+
    facet_wrap(~issue, scales = "fixed",labeller = label_wrap_gen(width=25), nrow = panel_rows )+
    theme_classic()+
    scale_x_continuous(name = element_blank(), breaks = 1:3,
                       labels = unique(fig_dat$waveLabels))+
    labs(col = element_blank(), y = "Standard Deviations")
}

# Original full-sample version (single aggregate line)
pop_over_time_plot <- populismOverTime(PopOpinionsOverTime)
ggsave(plot = pop_over_time_plot + themeBottom_fixed,
       filename = "output/figures/WVS/populist_voters/PRRP_pop_opinion_over_time.pdf",
       width = figureWidth, height = paper_plot_height, units = "cm")

# A value for overleaf, last wave prrp score
lastWavePrrpScore <- PopOpinionsOverTime %>% 
  filter(issue == "PRRP score" & BB_Country == "Average") %>%
  distinct() %>%
  filter(index == 3) %>%
  pull(wtd_value)


save_tex_value(values = lastWavePrrpScore,
               names = "lastWavePrrpScore",
               file_name = "output/values/values.tex",
               digits = 2)
