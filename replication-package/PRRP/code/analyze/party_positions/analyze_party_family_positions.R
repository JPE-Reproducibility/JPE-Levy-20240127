# *************************************************************************
# File description ----
# *************************************************************************
#
# This file analyzes trends in party positions by party family over time,
# using Manifesto Project data. It examines both aggregate trends across
# countries and within-party changes across election periods.
#
# Main goals:
#   (1) Track trends in party-family positions on key indices:
#       - CMP Economic Index, CMP Cultural Index, CMP Negativity Index
#       - Salience indices (Economic, Cultural, Foreign Affairs)
#       - Econ–Soc salience ratio
#   (2) Compare East vs West aggregates
#   (3) Highlight the most distinctive PRRP issue positions
#   (4) Summarize grouped salience trends (econ/social, positive/negative)
#   (5) Measure within-party cumulative changes in 5-year periods
#
# Analysis includes:
#   - Vote-share-weighted party-family averages per election and country
#   - Five-year moving averages and country-averaged aggregates
#   - Separate aggregates for Eastern and Western Europe
#   - Identification and plotting of top PRRP-distinctive items
#   - Within-party changes aggregated to party-family and cumulatively summed
#
# Outputs:
#
# Figures (paper/slides):
#   - Changes in Party Positions Over Time (economic vs cultural indices)
#   - Changes in Most Distinctive Party Positions Over Time
#   - Within-Party Position Changes (cumulative, 5-year periods)
#   - Grouped salience trends and salience ratio
#   - East vs West versions and slide-highlighted variants
#
# TeX Values:
#   - Average vote share excluded due to missing party-family classification
#   - Complementary included vote share
#
# *************************************************************************
# SETUP ----
# *************************************************************************
#
# See trends of the selected variables in manifesto data
library(ggplot2)
library(ggh4x)
library(haven)
library(dplyr)
library(tidyr)
library(patchwork)
library(savetexvalue)

rm(list = ls())

# Load function
source("code/util/commonVars.R")
source("code/analyze/party_positions/manifesto_general_functions.R")


econIndexName <- "CMP Economic Index"
societyIndexName <- "CMP Cultural Index"
negIndexName <- "CMP Negativity Index"
econSalName <- "CMP Economic Salience Index"
societySalName <- "CMP Cultural Salience Index"
foreignSalName <- "CMP Foreign Affair Salience Index"
ratioSalName <- "CMP Econ-Soc Salience Index Ratio"

# output directory:
outdir <- "output/figures/manifesto_party_pos_change"

# read data
raw_data_manifesto <- readRDS(paste0(cleandata_path, "/manifesto/manifesto_populist.rds"))
country_list <- read_dta(file = pathWVS_Data) %>%
  pull(BB_Country) %>%
  unique()

# *************************************************************************
# Changes in aggregate party family positions ----
# *************************************************************************

# For each position/index we first calculate avg value for each
# parfam*country*election, then calculate a five year moving avg
# for each parfam*country and take the avg of these values across
# countries. We show the changes in the econ/social indices in one figure
# and most distinct prrp/others positions in another figure.

## Prepare the aggregate data ----

# Select and mutate variables
df <- raw_data_manifesto %>%
  filter(manifesto_country_name %in% country_list) %>%
  select(manifesto_country_name, date, manifestoYear, Y_manifesto_party_id,
         Y_FR_Pop, parfam, starts_with("per"), pervote) %>%
  select(-peruncod) %>%
  add_econsoc_index() %>%
  add_neg_index() %>% 
  add_sal() %>% 
  mutate(ratio_sal = soc_sal/(econ_sal + soc_sal)) %>% 
  mutate(BBF_Country = factor(manifesto_country_name)) %>%
  add_parfam() %>% 
  # make pervote sum to 100%
  filter(!is.na(pervote)) %>% 
  group_by(BBF_Country, date) %>% 
  mutate(pervote = pervote / sum(pervote)) %>% 
  ungroup()

# calculate avg vote share for parties excluded because they have no BBF_parfam (only from 2001)
vote_share_excluded <- df %>% 
  filter(is.na(BBF_parfam) & manifestoYear >= 2001) %>% 
  group_by(BBF_Country, date) %>% 
  summarise(pervote = sum(pervote)) %>% 
  ungroup() %>% 
  group_by(BBF_Country) %>% 
  summarise(pervote = mean(pervote)) %>% 
  ungroup() %>% 
  summarise(pervote = mean(pervote)) %>% 
  pull(pervote) 

save_tex_value(1-vote_share_excluded, "haveParfamVoteShare", file_name = "manifestoValues.tex",
               path = "output/values", percent = T) 

# exclude parties with no BBF_parfam
df <- df %>%
  filter(!is.na(BBF_parfam))

# BY Parfam, Take group-mean in each election
df_aggregate <- df %>%
  rename(vote_share = pervote) %>%
  # remove parties with no manifesto coded (if one of the positions is NA so are the others)
  filter(!is.na(per101)) %>%
  group_by(BBF_Country, date, BBF_parfam, manifestoYear) %>%
  summarise(across(c(matches(".*(index|sal)$"), starts_with("per")),
                   ~ weighted.mean(.x, vote_share, na.rm = TRUE))) %>%
  ungroup()


# analysis by east and west
df_aggregate_east <- df %>%
  rename(vote_share = pervote) %>%
  filter(BBF_Country %in% country_east) %>% 
  filter(!is.na(per101)) %>%
  group_by(BBF_Country, date, BBF_parfam, manifestoYear) %>%
  summarise(across(c(matches(".*(index|sal)$"), starts_with("per")),
                   ~ weighted.mean(.x, vote_share, na.rm = TRUE))) %>%
  ungroup()

df_aggregate_west <- df %>%
  rename(vote_share = pervote) %>%
  filter(BBF_Country %in% country_west) %>% 
  filter(!is.na(per101)) %>%
  group_by(BBF_Country, date, BBF_parfam, manifestoYear) %>%
  summarise(across(c(matches(".*(index|sal)$"), starts_with("per")),
                   ~ weighted.mean(.x, vote_share, na.rm = TRUE))) %>%
  ungroup()

df_aggregate_eastSouth <- df %>%
  rename(vote_share = pervote) %>%
  filter(BBF_Country %in% country_eastSouth) %>%
  filter(!is.na(per101)) %>%
  group_by(BBF_Country, date, BBF_parfam, manifestoYear) %>%
  summarise(across(c(matches(".*(index|sal)$"), starts_with("per")),
                   ~ weighted.mean(.x, vote_share, na.rm = TRUE))) %>%
  ungroup()

df_aggregate_westNorth <- df %>%
  rename(vote_share = pervote) %>%
  filter(BBF_Country %in% country_westNorth) %>%
  filter(!is.na(per101)) %>%
  group_by(BBF_Country, date, BBF_parfam, manifestoYear) %>%
  summarise(across(c(matches(".*(index|sal)$"), starts_with("per")),
                   ~ weighted.mean(.x, vote_share, na.rm = TRUE))) %>%
  ungroup()

# calculate 5-years moving average in each country*parfam
# Add all years (including non-election years)
calculate_moving_average <- function(df_aggregate){
df_aggregate <- expand_grid(
  manifestoYear = unique(df_aggregate$manifestoYear),
  BBF_parfam = unique(df_aggregate$BBF_parfam),
  BBF_Country = unique(df_aggregate$BBF_Country)) %>%
  left_join(df_aggregate) %>%
  select(!date) %>%
  group_by(BBF_Country, BBF_parfam) %>%
  arrange(manifestoYear) %>%
  # moving average
  mutate(across(c(matches(".*(index|sal)$"), starts_with("per")),
                ~ rolling_avg_pop(var = .x, years = manifestoYear,
                                  year_start = 1980,
                                  year_end = 2021, window = 5))) %>% 
  # drop duplicate years (2 election cycles would create 2 identical rows)
  distinct() %>% 
  # Take average across countries and make long
  group_by(manifestoYear, BBF_parfam) %>%
  summarise(across(c(matches(".*(index|sal)$"), starts_with("per")), ~ mean(.x, na.rm = TRUE))) %>%
  ungroup() %>%
  # Prepare names of variables
  pivot_longer(cols = !c("manifestoYear", "BBF_parfam"),
               names_to = "variable_original", values_to = "value") %>%
  left_join(manifesto_labels, by = c("variable_original" = "variable_name"), keep = F) %>%
  rename(variable = title) %>%
  mutate(variable = if_else(is.na(variable), variable_original, variable)) %>%
  mutate(variable = if_else(variable_original == "econ_index", econIndexName, variable),
         variable = if_else(variable_original == "soc_index", societyIndexName, variable),
         variable = if_else(variable_original == "neg_index", negIndexName, variable),
         variable = if_else(variable_original == "econ_sal", econSalName, variable),
         variable = if_else(variable_original == "soc_sal", societySalName, variable),
         variable = if_else(variable_original == "fr_sal", foreignSalName, variable),
         variable = if_else(variable_original == "ratio_sal", ratioSalName, variable)) %>%
  mutate(year = as.numeric(substr(as.character(manifestoYear), 1, 4)))
}

df_aggregate_sal <- calculate_moving_average(df_aggregate) %>% # expand years for salience analysis
  filter(year >= 1990 & year <= 2021)
df_aggregate <- calculate_moving_average(df_aggregate) %>%
  filter(year >= 2005 & year <= 2021)
df_aggregate_east <- calculate_moving_average(df_aggregate_east) %>%
  filter(year >= 2005 & year <= 2021)
df_aggregate_west <- calculate_moving_average(df_aggregate_west) %>%
  filter(year >= 2005 & year <= 2021)
df_aggregate_eastSouth <- calculate_moving_average(df_aggregate_eastSouth) %>%
  filter(year >= 2005 & year <= 2021)
df_aggregate_westNorth <- calculate_moving_average(df_aggregate_westNorth) %>%
  filter(year >= 2005 & year <= 2021)

## Plot aggregate parfam trends ----

# theme and graphics
theme_plot_common <- theme(strip.background = element_blank(),
                           strip.text.x = element_text(face = "bold"))
theme_paper <-  theme_classic(base_size = 12) +
  theme_plot_common
party_colors <- c("#D55E00","#E69F00","#CC79A7" , "#0072B2", "#009E73")
party_lines <- c("solid","dashed","dotted","longdash","dotdash")
party_shapes <-  c(15, 16, 17, 3, 8)
add_plot_details <- function(plot) {
  plot +
    geom_line() +
    geom_point() +
    scale_color_manual(name = "", values = party_colors) +
    scale_linetype_manual(name = "", values = party_lines) +
    scale_shape_manual(name = "", values = party_shapes)+
    xlab("Year")
}

# Keep only the total economics and culture salience series.
df_aggregate_sal_broad <- df_aggregate_sal %>%
  filter(variable_original %in% c("econ_sal", "soc_sal")) %>%
  mutate(variable_category = case_when(
    variable_original == "econ_sal" ~ "Economics",
    variable_original == "soc_sal" ~ "Culture"
  )) %>%
  select(year, variable_category, value) %>%
  group_by(year, variable_category) %>%
  summarise(value = sum(value, na.rm = TRUE)/5, .groups = "drop") %>%
  mutate(variable_category = factor(variable_category, levels = c("Economics", "Culture"))) %>%
  mutate(value = ifelse(year == 2021, NA, value))

# Plot total economics versus culture salience over time.
p_sal_broad <- ggplot(df_aggregate_sal_broad, aes(x = year, y = value,
              color = variable_category,
              linetype = variable_category,
              group = variable_category)) +
  geom_line(linewidth = 1) +
  labs(x = "Year", y = "Percent of Manifesto") +
  scale_color_manual(
  values = c("Economics" = "#984EA3", "Culture" = "#377EB8"),
  breaks = c("Economics", "Culture")
  ) +
  scale_linetype_manual(
  values = c("Economics" = "solid", "Culture" = "dashed"),
  breaks = c("Economics", "Culture")
  ) +
  guides(color = guide_legend(title = NULL,
                override.aes = list(
                  linetype = c("solid", "dashed"),
                  color = c("#984EA3", "#377EB8"),
                  linewidth = c(1, 1)
                )),
     linetype = "none") +
  scale_x_continuous(expand = expansion(mult = c(0.01, 0.02))) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.04))) +
  theme_classic(base_size = 11.2) +
  theme(legend.position = "top",
        legend.direction = "horizontal",
        legend.justification = "center",
        legend.key.width = unit(2.2, "cm"),
        legend.key.height = unit(0.35, "cm"),
        legend.spacing.x = unit(0.55, "cm"),
        legend.margin = ggplot2::margin(0, 0, -2, 0),
        legend.text = element_text(size = 12.8),
        axis.title = element_text(size = 11.2),
        axis.text = element_text(size = 10, color = "black"),
        axis.line = element_line(linewidth = 0.9),
        axis.ticks = element_line(linewidth = 0.8),
        axis.ticks.length = unit(0.14, "cm"),
        plot.margin = ggplot2::margin(2, 4, 2, 4))

# Save the collapsed economics-versus-culture salience figure.
ggsave(plot = p_sal_broad,
       "salience_grouped_broad.pdf",
       path = outdir, width = figureWidth * 0.8, height = 10.5 * 0.8,
       units = "cm")

### plot indices + highlighted versions for slides ----

process_base_indices <- function(df_aggregate){
  df_aggregate %>% 
  filter(variable %in% c(econIndexName, societyIndexName)) %>% 
  mutate(variable = factor(variable, levels = c(econIndexName, societyIndexName)),
         cult_all = if_else(variable == societyIndexName,
                            "high", "low"),
         cult_prrp = if_else(variable == societyIndexName & BBF_parfam == "PRRP",
                             "high", "low"),
         cult_non_prrp = if_else(variable == societyIndexName & BBF_parfam != "PRRP",
                                 "high", "low"),
         shift_left = if_else((variable == societyIndexName & BBF_parfam == "Socialist") |
                                (variable == econIndexName & BBF_parfam %in% c("Center-Left", "Green")),
                              "high", "low" ),
         prrp = if_else(BBF_parfam == "PRRP",
                        "high", "low"),
         mainstream = if_else(BBF_parfam %in% c("Center-Left", "Center-Right"),
                              "high", "low"),
         econ_mainstream = if_else(variable == econIndexName &
                                     BBF_parfam %in% c("Center-Left", "Center-Right"), 
                                   "high", "low"))
}

df_indices <- process_base_indices(df_aggregate)
df_indices_east <- process_base_indices(df_aggregate_east)
df_indices_west <- process_base_indices(df_aggregate_west)
df_indices_eastSouth <- process_base_indices(df_aggregate_eastSouth)
df_indices_westNorth <- process_base_indices(df_aggregate_westNorth)

df_indices_add_neg <-  df_aggregate %>% 
  filter(variable %in% c(econIndexName, societyIndexName, negIndexName)) %>% 
  mutate(variable = factor(variable, levels = c(econIndexName, societyIndexName, negIndexName)),
         cult_all = if_else(variable == societyIndexName,
                            "high", "low"),
         cult_prrp = if_else(variable == societyIndexName & BBF_parfam == "PRRP",
                             "high", "low"),
         cult_non_prrp = if_else(variable == societyIndexName & BBF_parfam != "PRRP",
                                 "high", "low"),
         shift_left = if_else((variable == societyIndexName & BBF_parfam == "Socialist") |
                                (variable == econIndexName & BBF_parfam %in% c("Center-Left", "Green")),
                              "high", "low" ),
         prrp = if_else(BBF_parfam == "PRRP",
                        "high", "low"),
         mainstream = if_else(BBF_parfam %in% c("Center-Left", "Center-Right"),
                              "high", "low"),
         econ_mainstream = if_else(variable == econIndexName &
                                     BBF_parfam %in% c("Center-Left", "Center-Right"), 
                                   "high", "low"))

df_sals <-  df_aggregate_sal %>% 
  filter(variable %in% c(econSalName, societySalName)) %>% 
  mutate(variable = factor(variable, levels = c(econSalName, societySalName)),
         cult_all = if_else(variable == societySalName,
                            "high", "low"),
         cult_prrp = if_else(variable == societySalName & BBF_parfam == "PRRP",
                             "high", "low"),
         cult_non_prrp = if_else(variable == societySalName & BBF_parfam != "PRRP",
                                 "high", "low"),
         shift_left = if_else((variable == societySalName & BBF_parfam == "Socialist") |
                                (variable == econSalName & BBF_parfam %in% c("Center-Left", "Green")),
                              "high", "low" ),
         prrp = if_else(BBF_parfam == "PRRP",
                        "high", "low"),
         mainstream = if_else(BBF_parfam %in% c("Center-Left", "Center-Right"),
                              "high", "low"),
         econ_mainstream = if_else(variable == econSalName &
                                     BBF_parfam %in% c("Center-Left", "Center-Right"), 
                                   "high", "low"))

df_ratio <-  df_aggregate_sal %>% 
  filter(variable %in% ratioSalName)

indices_plot_base <-
  ggplot(df_indices, 
         aes(x = year,y = value, linetype = BBF_parfam, color = BBF_parfam, shape = BBF_parfam)) +
  ylab("Net Percent of Manifesto") +
  scale_x_continuous(limits = c(2005, 2018), breaks = c(2005, 2010, 2015, 2018)) 

indices_plot_base <- indices_plot_base %>% 
  add_plot_details()

indices_plot_base_east <-
  ggplot(df_indices_east, 
         aes(x = year,y = value, linetype = BBF_parfam, color = BBF_parfam, shape = BBF_parfam)) +
  ylab("Net Percent of Manifesto") +
  scale_x_continuous(limits = c(2005, 2018), breaks = c(2005, 2010, 2015, 2018)) 

indices_plot_base_east <- indices_plot_base_east %>% 
  add_plot_details()

indices_plot_base_west <-
  ggplot(df_indices_west, 
         aes(x = year,y = value, linetype = BBF_parfam, color = BBF_parfam, shape = BBF_parfam)) +
  ylab("Net Percent of Manifesto") +
  scale_x_continuous(limits = c(2005, 2018), breaks = c(2005, 2010, 2015, 2018)) 

indices_plot_base_west <- indices_plot_base_west %>% 
  add_plot_details()

indices_plot_base_eastSouth <-
  ggplot(df_indices_eastSouth,
         aes(x = year,y = value, linetype = BBF_parfam, color = BBF_parfam, shape = BBF_parfam)) +
  ylab("Net Percent of Manifesto") +
  scale_x_continuous(limits = c(2005, 2018), breaks = c(2005, 2010, 2015, 2018))

indices_plot_base_eastSouth <- indices_plot_base_eastSouth %>%
  add_plot_details()

indices_plot_base_westNorth <-
  ggplot(df_indices_westNorth,
         aes(x = year,y = value, linetype = BBF_parfam, color = BBF_parfam, shape = BBF_parfam)) +
  ylab("Net Percent of Manifesto") +
  scale_x_continuous(limits = c(2005, 2018), breaks = c(2005, 2010, 2015, 2018))

indices_plot_base_westNorth <- indices_plot_base_westNorth %>%
  add_plot_details()

indices_plot_base_add_neg <-
  ggplot(df_indices_add_neg, 
         aes(x = year,y = value, linetype = BBF_parfam, color = BBF_parfam, shape = BBF_parfam)) +
  ylab("Net Percent of Manifesto") +
  scale_x_continuous(limits = c(2005, 2018), breaks = c(2005, 2010, 2015, 2018)) 

indices_plot_base_add_neg <- indices_plot_base_add_neg %>% 
  add_plot_details()

sals_plot_base <-
  ggplot(df_sals, 
         aes(x = year,y = value, linetype = BBF_parfam, color = BBF_parfam, shape = BBF_parfam)) +
  ylab("Net Percent of Manifesto") +
  scale_x_continuous(limits = c(1990, 2018)) 

sals_plot_base <- sals_plot_base %>% 
  add_plot_details()

ratio_plot_base <-
  ggplot(df_ratio, 
         aes(x = year,y = value, linetype = BBF_parfam, color = BBF_parfam, shape = BBF_parfam)) +
  ylab("Ratio of Salience") +
  scale_x_continuous(limits = c(1990, 2018)) 

ratio_plot_base <- ratio_plot_base %>% 
  add_plot_details()

#### indices for paper ----
indices_plot_paper <- indices_plot_base +
  facet_wrap(~variable, nrow = 2, scales = "free") +
  theme_paper + 
  theme(legend.key.size = unit(1.5, "cm"))

scale_inidividual_facet_y_axes = function(plot, ylims) {
  init_scales_orig = plot$facet$init_scales
  
  init_scales_new = function(...) {
    r = init_scales_orig(...)
    y = r$y
    if(is.null(y)) return(r)
    for (i in seq(1, length(y))) {
      ylim = ylims[[i]]
      if(!is.null(ylim)) {
        y[[i]]$limits = ylim
      }
    }
    r$y = y
    return(r)
  }
  
  plot$facet$init_scales = init_scales_new
  
  return(plot)
}
ylims = list(c(-37, -3), c(-22, 25))

indices_plot_paper_east <- indices_plot_base_east +
  facet_wrap(~variable, nrow = 2, scales = "free") +
  theme_paper + 
  theme(legend.key.size = unit(1.5, "cm"))

indices_plot_paper_east <- scale_inidividual_facet_y_axes(indices_plot_paper_east, ylims = ylims)

indices_plot_paper_west <- indices_plot_base_west +
  facet_wrap(~variable, nrow = 2, scales = "free") +
  theme_paper + 
  theme(legend.key.size = unit(1.5, "cm"))

indices_plot_paper_west <- scale_inidividual_facet_y_axes(indices_plot_paper_west, ylims = ylims)

indices_plot_paper_eastSouth <- indices_plot_base_eastSouth +
  facet_wrap(~variable, nrow = 2, scales = "free") +
  theme_paper +
  theme(legend.key.size = unit(1.5, "cm"))

indices_plot_paper_eastSouth <- scale_inidividual_facet_y_axes(indices_plot_paper_eastSouth, ylims = ylims)

indices_plot_paper_westNorth <- indices_plot_base_westNorth +
  facet_wrap(~variable, nrow = 2, scales = "free") +
  theme_paper +
  theme(legend.key.size = unit(1.5, "cm"))

indices_plot_paper_westNorth <- scale_inidividual_facet_y_axes(indices_plot_paper_westNorth, ylims = ylims)

# Combine poor and rich country panels with a shared legend.
indices_plot_paper_poor_rich <- (
  indices_plot_paper_westNorth +
    labs(title = "Higher-Income Countries") +
    theme(legend.position = "none")
) + (
  indices_plot_paper_eastSouth +
    labs(title = "Lower-Income Countries", y = NULL)
) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom",
        legend.key.size = unit(1.5, "cm"),
        plot.title = element_text(hjust = 0.5))


indices_plot_paper_add_neg <- indices_plot_base_add_neg +
  facet_wrap(~variable, nrow = 3, scales = "free") +
  theme_paper + 
  theme(legend.key.size = unit(1.5, "cm"))

sals_plot_paper <- sals_plot_base +
  facet_wrap(~variable, nrow = 2, scales = "free_x")+
  theme_paper + 
  theme(legend.key.size = unit(1.5, "cm"))

ratio_plot_paper <- ratio_plot_base + 
  theme_paper + 
  theme(legend.key.size = unit(1.5, "cm"))

ggsave(plot = indices_plot_paper,
       "index_aggregate.pdf",
       path = outdir, width = figureWidth, height = 12, unit = "cm")

### Plot six positions that are most distinct ----
# The variables are already sorted in decreasing order of absolute std diff
# (see analyze_desc_stat_manifesto.R)
list_most_distinct_PRRP <- readRDS(paste0(cleandata_path, "/manifesto/top_distinct_vars.rds"))[1:6]

# create labels for variable
fct_labels <- 
  data.frame(variable_original = list_most_distinct_PRRP) %>% 
  left_join(df_aggregate) %>% 
  select(variable_original, variable) %>% 
  distinct()

df_elems <- df_aggregate %>% 
  filter(variable_original %in% list_most_distinct_PRRP) %>% 
  mutate(variableF = factor(variable_original,
                            levels = fct_labels$variable_original,
                            labels = fct_labels$variable))

ymax_elems <- max(df_elems$value,na.rm=T)
ymin_elems <- min(df_elems$value,na.rm=T)

elems_plot <-
  ggplot(df_elems,
         aes(x = year,y = value, linetype = BBF_parfam,
             color = BBF_parfam, shape = BBF_parfam)) +
  facet_wrap(~ variableF, ncol = 2, scales = "free",
             labeller = labeller(variableF = label_wrap_gen(20)))+
  ylab("Percent of Manifesto") +
  ylim(ymin_elems, ymax_elems) 

elems_plot <- elems_plot %>% 
  add_plot_details() +
  theme_paper +
  theme(legend.key.size = unit(1.5, "cm"))

ggsave(plot = elems_plot,
       "top_distinct_aggregate.pdf",
       path = outdir, width = figureWidth, height = 20, unit = "cm")

### Plot positions about cultural index ----

# create labels for positive society index
pos_soc_labels <- 
  data.frame(variable_original = positive_soc_vars) %>% 
  left_join(df_aggregate) %>% 
  select(variable_original, variable) %>% 
  distinct()

pos_soc_elems <- df_aggregate %>% 
  filter(variable_original %in% positive_soc_vars) %>% 
  mutate(variableF = factor(variable_original,
                            levels = pos_soc_labels$variable_original,
                            labels = pos_soc_labels$variable))

pos_soc_elems_plot <-
  ggplot(pos_soc_elems,
         aes(x = year,y = value, linetype = BBF_parfam,
             color = BBF_parfam, shape = BBF_parfam)) +
  facet_wrap(~ variableF, ncol = 2, scales = "free",
             labeller = labeller(variableF = label_wrap_gen(20)))+
  ylab("Percent of Manifesto") 

pos_soc_elems_plot <- pos_soc_elems_plot %>% 
  add_plot_details() +
  theme_paper +
  theme(legend.key.size = unit(1.5, "cm"))


# create labels for negative society index
neg_soc_labels <- 
  data.frame(variable_original = negative_soc_vars) %>% 
  left_join(df_aggregate) %>% 
  select(variable_original, variable) %>% 
  distinct()

neg_soc_elems <- df_aggregate %>% 
  filter(variable_original %in% negative_soc_vars) %>% 
  mutate(variableF = factor(variable_original,
                            levels = neg_soc_labels$variable_original,
                            labels = neg_soc_labels$variable))
neg_soc_elems_plot <-
  ggplot(neg_soc_elems,
         aes(x = year,y = value, linetype = BBF_parfam,
             color = BBF_parfam, shape = BBF_parfam)) +
  facet_wrap(~ variableF, ncol = 2, scales = "free",
             labeller = labeller(variableF = label_wrap_gen(20)))+
  ylab("Percent of Manifesto") 

neg_soc_elems_plot <- neg_soc_elems_plot %>% 
  add_plot_details() +
  theme_paper +
  theme(legend.key.size = unit(1.5, "cm"))



# *************************************************************************
# Within party position trends ----
# *************************************************************************
# For each party separately we group observations in 5 year periods
# (2001-2005 etc). We calculate the mean value for the two indices and
# vote share in each 5-year period and then the change from one period 
# to the next.In each period*parfam*country we calculate a weighted avg
# of the change from previous period using previous period mean vote share
# as weights. For each period*parfam we avg the changes giving each country 
# equal weight. We then plot the cumulative changes (the change in each
# period t is only based on parties that existed in both t and t-1.

## prepare within party change data ----
## create 5-year periods and calculate avg vote share and indices by party
calculate_5year_averages <- function(df, start_year, end_year, initial_year) {
  df %>%
    mutate(year5 = if_else(manifestoYear %% 5 != 0, manifestoYear - manifestoYear %% 5, manifestoYear)) %>%
    filter(year5 >= start_year & year5 <= end_year) %>%
    group_by(year5, BBF_Country, Y_manifesto_party_id, BBF_parfam) %>%
    summarise(across(matches(".*(index|sal)$") | "pervote", ~ mean(.x, na.rm = TRUE))) %>%
    ungroup() %>%
    # Compute changes at party level
    arrange(year5, Y_manifesto_party_id) %>%
    group_by(Y_manifesto_party_id) %>%
    rename(vote_share = pervote) %>%
    mutate(
      lag.vote_share = if_else((year5 - lag(year5) == 5), lag(vote_share, 1), 0),
      across(matches(".*(index|sal)$"), ~if_else((year5 - lag(year5) == 5), .x - lag(.x, 1), NA_real_))
    ) %>%
    ungroup() %>%
    # Take weighted average at parfam-country level then parfam level (global)
    group_by(year5, BBF_Country, BBF_parfam) %>%
    summarise(across(matches(".*(index|sal)$"), ~ weighted.mean(.x, lag.vote_share, na.rm = TRUE))) %>%
    ungroup() %>%
    group_by(year5, BBF_parfam) %>%
    summarise(across(matches(".*(index|sal)$"), ~ mean(.x, na.rm = TRUE))) %>%
    ungroup() %>%
    # calculate the cumulative changes
    mutate(across(matches(".*(index|sal)$"), ~if_else(year5 == initial_year, 0, .x))) %>%
    group_by(BBF_parfam) %>%
    arrange(year5) %>%
    mutate(across(matches(".*(index|sal)$"), ~ cumsum(.x))) %>%
    ungroup()
}

# Prepare to plot
pivot_for_plotting <- function(df, index_names) {
  df %>%
    pivot_longer(cols = matches(".*(index|sal)$"), names_to = "variable_original", values_to = "value") %>%
    mutate(variable = case_when(
      variable_original == "econ_index" ~ index_names$econIndexName,
      variable_original == "soc_index"  ~ index_names$societyIndexName,
      variable_original == "neg_index"  ~ index_names$negIndexName,
      variable_original == "econ_sal"   ~ index_names$econSalName,
      variable_original == "soc_sal"    ~ index_names$societySalName,
      variable_original == "fr_sal"    ~ index_names$foreignSalName,
      variable_original == "ratio_sal"  ~ index_names$ratioSalName,
      TRUE ~ NA
    ))
}


df_within <- calculate_5year_averages(df, 2005, 2019, 2005)

df_within <- pivot_for_plotting (df_within, list(
  econIndexName = econIndexName,
  societyIndexName = societyIndexName,
  negIndexName = negIndexName,
  econSalName = econSalName,
  societySalName = societySalName,
  foreignSalName = foreignSalName,
  ratioSalName = ratioSalName
))


df_within_sal <- calculate_5year_averages(df, 1990, 2019, 1990)

df_within_sal <- pivot_for_plotting(df_within_sal, list(
  econIndexName = econIndexName,
  societyIndexName = societyIndexName,
  negIndexName = negIndexName,
  econSalName = econSalName,
  societySalName = societySalName,
  foreignSalName = foreignSalName,
  ratioSalName = ratioSalName
))

## Plot within party change ----

df_indices_within <- df_within %>%
  filter(variable_original %in% c("econ_index", "soc_index")) %>%
  mutate(variable = factor(variable, levels = c(econIndexName, societyIndexName)))


indices_within_plot <-
  ggplot(df_indices_within, 
         aes(x = year5, y = value, linetype = BBF_parfam, color = BBF_parfam, shape = BBF_parfam)) +
  ylab("Cumulative change in net percent of manifesto") +
  facet_wrap(~variable, nrow = 2, scales = "free") +
  scale_x_continuous(breaks = seq(2005, 2019, by = 5),
                     labels = paste0(seq(2005, 2015, by = 5), "-", seq(2009, 2019, by = 5)))

indices_within_plot <- add_plot_details(indices_within_plot) +
  theme_paper +
  theme(legend.key.width = unit(1.5, "cm")) +
  xlab("")

ggsave(plot = indices_within_plot,
       "index_parfam_within.pdf", path = outdir, width=figureWidth, height=12, unit="cm") 

df_indices_within_sal <- df_within_sal %>%
  filter(variable_original %in% c("econ_sal", "soc_sal")) %>%
  mutate(variable = factor(variable, levels = c(econSalName, societySalName)))


indices_within_plot_sal <-
  ggplot(df_indices_within_sal, 
         aes(x = year5, y = value, linetype = BBF_parfam, color = BBF_parfam, shape = BBF_parfam)) +
  ylab("Cumulative change in net percent of manifesto") +
  facet_wrap(~variable, nrow = 2, scales = "free") +
  scale_x_continuous(breaks = seq(1990, 2019, by = 5),
                     labels = paste0(seq(1990, 2015, by = 5), "-", seq(1994, 2019, by = 5)))

indices_within_plot_sal <- add_plot_details(indices_within_plot_sal) +
  theme_paper +
  theme(legend.key.width = unit(1.5, "cm")) +
  xlab("")
