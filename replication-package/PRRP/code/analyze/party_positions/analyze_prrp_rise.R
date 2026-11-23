# *************************************************************************
# Description ----
# *************************************************************************
# This script produces figures documenting the rise of Populist Radical Right
# Parties (PRRP) using manifesto data, with complementary robustness checks
# based on the World Values Survey (WVS).
#
# Outputs:
#
# Paper Figures:
# - Figure 1: Support for Populist Radical Right Parties over Time
#   (file: fr_vote_share_global.pdf)
#
# Appendix Figures:
# - Figure A.20: Average Number of PRRP by Country
#   (file: fr_num_global.pdf)
#
# Additional manuscript-related outputs:
# - Regional variants of the average number of PRRP by country:
#   * Western/North Europe (file: fr_num_rich.pdf)
#   * Eastern/South Europe (file: fr_num_poor.pdf)
#   * East–West comparison (file: fr_num_rich_poor.pdf)
#
# - Share of countries with at least one PRRP over time
#   (file: share_countries_w_fr_populists.pdf)
#
# Slide-only:
# - Share of countries with far-right populist parties (slide version)
#   (file: share_countries_w_fr_populists_slide.pdf)
#
# - Correlation between PRRP support measured in manifesto data and WVS:
#   * Change from Wave 4 to Wave 6
#   * Levels in Wave 4
#   * Levels in Wave 6
#   (displayed as on-screen robustness plots)
#
# Tex Values:
# - Correlation between changes in PRRP support measured in manifesto data
#   and WVS (Wave 4 to Wave 6)
#   (file: output/values/values.tex, value name: prrpSupChangeCor)

# *************************************************************************
# SETUP ----
# *************************************************************************

rm(list = ls())


library(tidyverse)
library(haven)
library(scales)
library(savetexvalue)
library(ggrepel)

source("code/util/commonVars.R")

outdir <- "output/figures/manifesto_populist_rise/"

raw_data <- readRDS(paste0(cleandata_path, "/manifesto/manifesto_populist.rds"))
raw_data_wvs <- read_dta(pathWVS_Data)

source("code/analyze/party_positions/manifesto_general_functions.R")

countries_list <- raw_data_wvs %>%
  pull(BB_Country) %>%
  unique()

# *************************************************************************
# Data prep ---- Manifesto
# *************************************************************************

df <- raw_data %>%
  filter(manifestoYear >= 1980) %>%
  mutate(
    manifesto_country_name =
      if_else(manifesto_country_name == "United Kingdom", "UK", manifesto_country_name)
  ) %>%
  filter(manifesto_country_name %in% countries_list) %>%
  select(manifestoYear, date, manifesto_country_name, pervote, Y_FR_Pop)

df <- df %>%
  mutate(prrp_great_1 = pervote >= 1 & Y_FR_Pop == 1) %>%
  group_by(date, manifesto_country_name) %>%
  mutate(pervote = pervote / sum(pervote, na.rm = TRUE)) %>%
  ungroup()

df_poor <- df %>% filter(manifesto_country_name %in% country_eastSouth)
df_rich <- df %>% filter(manifesto_country_name %in% country_westNorth)

prrp_share <- function(df, countries_list) {
  
  df_prrp <- df %>%
    group_by(date, manifesto_country_name, manifestoYear) %>%
    summarise(
      count = sum(prrp_great_1),
      dum_exist = count >= 1,
      pervote = sum(pervote * Y_FR_Pop, na.rm = TRUE),
      .groups = "drop"
    )
  
  df_prrp <- expand.grid(
    manifestoYear = 1980:2021,
    manifesto_country_name = countries_list
  ) %>%
    left_join(df_prrp) %>%
    select(-date)
  
  df_prrp <- df_prrp %>%
    arrange(manifestoYear) %>%
    group_by(manifesto_country_name) %>%
    mutate(across(
      c(count, dum_exist, pervote),
      ~ rolling_avg_pop(var = .x, years = manifestoYear)
    )) %>%
    ungroup() %>%
    distinct() %>%
    group_by(manifestoYear) %>%
    summarise(across(c(count, dum_exist, pervote), ~ mean(.x, na.rm = TRUE)))
  
  return(df_prrp)
}

df_prrp <- prrp_share(df, countries_list)
df_prrp_poor <- prrp_share(df_poor, country_eastSouth)
df_prrp_rich <- prrp_share(df_rich, country_westNorth)

df_prrp_rich_poor <- bind_rows(
  df_prrp_poor %>% mutate(region = "Lower income countries"),
  df_prrp_rich %>% mutate(region = "Higher income countries")
)

build_manifesto_country_year <- function(df_in) {
  df_in %>%
    rename(BB_Country = manifesto_country_name) %>%
    group_by(BB_Country, manifestoYear) %>%
    summarise(
      count = sum(prrp_great_1),
      dum_exist = count >= 1,
      pervote = sum(pervote * Y_FR_Pop, na.rm = TRUE),
      .groups = "drop"
    )
}

# *************************************************************************
# Data prep ---- WVS
# *************************************************************************

# (a) Compute weighted PRRP support share in WVS by country and wave (waves 4 and 6).
df_wvs <- raw_data_wvs %>%
  filter(BB_Wave_num %in% c(4, 6)) %>%
  group_by(BB_Country, BB_Wave_num) %>%
  summarise(pervote_wvs = weighted.mean(Y_FR_Pop, BB_Weight_CW, na.rm = TRUE), .groups = "drop")

# (b) Compute manifesto PRRP support share by country and year with no moving average.
# The denominator is total observed party vote share in manifesto data.
df_manifesto_share <- raw_data %>%
  filter(manifestoYear >= 1980) %>%
  mutate(
    manifesto_country_name = if_else(manifesto_country_name == "United Kingdom", "UK", manifesto_country_name)
  ) %>%
  filter(manifesto_country_name %in% countries_list) %>%
  group_by(manifesto_country_name, manifestoYear) %>%
  summarise(
    pervote_manifesto = sum(Y_FR_Pop * pervote, na.rm = TRUE) / sum(pervote, na.rm = TRUE),
    .groups = "drop"
  )

# Keep a unique manifesto lookup by key for the WVS-to-manifesto merge.
manifesto_key_lookup <- raw_data %>%
  mutate(
    manifesto_country_name = if_else(manifesto_country_name == "United Kingdom", "UK", manifesto_country_name)
  ) %>%
  select(Y_manifesto_key, manifesto_country_name, date, manifestoYear) %>%
  distinct()

# (c) Map each country-wave to the most common matched election
wave_manifesto_map <- raw_data_wvs %>%
  filter(BB_Wave_num %in% c(4, 6)) %>%
  select(BB_Country, BB_Wave_num, Y_manifesto_key) %>%
  left_join(manifesto_key_lookup, by = "Y_manifesto_key") %>%
  filter(!is.na(manifesto_country_name), !is.na(date), !is.na(manifestoYear)) %>%
  count(BB_Country, BB_Wave_num, manifesto_country_name, date, manifestoYear) %>%
  group_by(BB_Country, BB_Wave_num) %>%
  filter(n == max(n)) %>%
  ungroup() %>%
  select(BB_Country, BB_Wave_num, manifesto_country_name, manifestoYear)

# Merge WVS support with the matched manifesto election for each country-wave.
df_wvs_manifesto_long <- df_wvs %>%
  left_join(wave_manifesto_map, by = c("BB_Country", "BB_Wave_num")) %>%
  left_join(
    df_manifesto_share %>% rename(BB_Country = manifesto_country_name),
    by = c("BB_Country", "manifestoYear")
  )

# Reshape to wave-specific columns and compute wave-to-wave changes.
df_wvs_manifesto <- df_wvs_manifesto_long %>%
  select(BB_Country, BB_Wave_num, pervote_wvs, pervote_manifesto) %>%
  pivot_wider(
    names_from = BB_Wave_num,
    values_from = c(pervote_wvs, pervote_manifesto)
  ) %>%
  mutate(
    change_sh_prrp_wvs = pervote_wvs_6 - pervote_wvs_4,
    change_sh_prrp_manifesto = pervote_manifesto_6 - pervote_manifesto_4
  )

# (d) Compute correlations in levels by wave.
cor_wave4 <- cor(
  df_wvs_manifesto$pervote_wvs_4,
  df_wvs_manifesto$pervote_manifesto_4,
  use = "complete.obs"
)
cor_wave6 <- cor(
  df_wvs_manifesto$pervote_wvs_6,
  df_wvs_manifesto$pervote_manifesto_6,
  use = "complete.obs"
)

# (e) Compute the correlation in changes between waves 4 and 6.
cor_share_prrp <- cor(
  df_wvs_manifesto$change_sh_prrp_manifesto,
  df_wvs_manifesto$change_sh_prrp_wvs,
  use = "complete.obs"
)

# Keep yearly manifesto series (with moving averages) for time-series figures below.
df_manifesto <- build_manifesto_country_year(df)

df_manifesto <- expand.grid(
  manifestoYear = 1980:2021,
  BB_Country = countries_list
) %>%
  left_join(df_manifesto) %>%
  arrange(manifestoYear) %>%
  group_by(BB_Country) %>%
  mutate(across(
    c(count, dum_exist, pervote),
    ~ rolling_avg_pop(var = .x, years = manifestoYear)
  )) %>%
  ungroup() %>%
  distinct() %>%
  select(BB_Country, manifestoYear, pervote) %>%
  complete(BB_Country, manifestoYear) %>%
  mutate(pervote = if_else(is.na(pervote), 0, pervote)) %>%
  rename(
    BB_Year = manifestoYear,
    pervote_manifesto = pervote
  )

# *************************************************************************
# Plots ----
# *************************************************************************

create_base_plot <- function(df, var) {
  ggplot(df, aes(x = manifestoYear, y = {{ var }})) +
    geom_line(size = 1.5) +
    theme_classic() +
    xlab("Year")
}

annotate_waves <- function(plot, y, label_size = 3) {
  plot +
    geom_rect(aes(xmin = 2017, xmax = 2020, ymin = -Inf, ymax = Inf),
              fill = "gray", alpha = 0.01) +
    geom_rect(aes(xmin = 2011, xmax = 2013, ymin = -Inf, ymax = Inf),
              fill = "gray", alpha = 0.01) +
    geom_rect(aes(xmin = 2005, xmax = 2009, ymin = -Inf, ymax = Inf),
              fill = "gray", alpha = 0.01) +
    annotate("text", x = 2007, y = y[1], label = "2005–2009\nsurvey wave",
             size = label_size, angle = 90) +
    annotate("text", x = 2012, y = y[2], label = "2011–2013\nsurvey wave",
             size = label_size, angle = 90) +
    annotate("text", x = 2018.5, y = y[3], label = "2017–2020\nsurvey wave",
             size = label_size, angle = 90) +
    geom_line(size = 1.5)
}

plot_support_scatter <- function(df, x, y, x_lab, y_lab) {
  ggplot(df, aes(x = {{ x }}, y = {{ y }})) +
    geom_point(size = 1.5) +
    geom_label_repel(
      aes(label = BB_Country),
      box.padding = 0.35,
      point.padding = 0.5,
      segment.color = "grey50"
    ) +
    geom_abline(intercept = 0, slope = 1, size = 0.5) +
    theme_classic() +
    labs(
      title = "PRRP Vote Share in WVS and Manifesto Data",
      x = x_lab,
      y = y_lab
    )
}

# *************************************************************************
# Robustness / Slide-only plots ----
# *************************************************************************

plot_support_scatter(
  df = df_wvs_manifesto,
  x = change_sh_prrp_wvs,
  y = change_sh_prrp_manifesto,
  x_lab = "WVS (Wave 4 to Wave 6)",
  y_lab = "Manifesto (Wave 4 to Wave 6)"
)

plot_support_scatter(
  df = df_wvs_manifesto,
  x = pervote_wvs_4,
  y = pervote_manifesto_4,
  x_lab = "WVS (Wave 4)",
  y_lab = "Manifesto (Wave 4)"
)

plot_support_scatter(
  df = df_wvs_manifesto,
  x = pervote_wvs_6,
  y = pervote_manifesto_6,
  x_lab = "WVS (Wave 6)",
  y_lab = "Manifesto (Wave 6)"
)


# ==================================================
# Paper & Appendix figures
# ==================================================

y_number <- c(0.57, 0.57, 0.57)
y_vote_share <- rep(0.05, 3)

# --------------------------------------------------
# Figure A.20: Average Number of PRRP by Country
# --------------------------------------------------

p_number <- create_base_plot(df_prrp, count) +
  scale_y_continuous(limits = c(0.5, 1.30)) +
  scale_x_continuous(limits = c(2000, 2020), breaks = seq(2000, 2020, by = 5)) +
  ylab("Number of Populist Radical Right Parties")

p_number <- annotate_waves(p_number, y = y_number)

ggsave(
  p_number,
  filename = "fr_num_global.pdf",
  path = outdir,
  width = figureWidth,
  height = figureWidth * 0.55,
  units = "cm"
)

# --------------------------------------------------
# Share of countries with PRRP
# --------------------------------------------------

# --------------------------------------------------
# Share of countries with PRRP
# --------------------------------------------------

# --------------------------------------------------
# Figure 1: Support for Populist Radical Right Parties over Time
# --------------------------------------------------

p_vote_share <- create_base_plot(df_prrp, pervote) +
  ylab("PRRP Vote Share") +
  scale_x_continuous(limits = c(1990, 2020), breaks = seq(1990, 2020, by = 5)) +
  scale_y_continuous(
    breaks = seq(0, 0.2, by = 0.05),
    labels = percent_format(accuracy = 1)
  ) +
  # Larger fonts so the in-figure text is close to the figure-notes size on the
  # page (figure is included at 0.65\textwidth, so it is scaled down by ~0.64).
  theme_classic(base_size = 16)



p_vote_share_annotate <- annotate_waves(p_vote_share, y = y_vote_share, label_size = 4.5)

ggsave(
  p_vote_share_annotate,
  filename = "fr_vote_share_global.pdf",
  path = outdir,
  width = figureWidth,
  height = figureWidth * 0.6,
  units = "cm"
)

# ==================================================
# Save Tex value: correlation between WVS and Manifesto change (Wave 4 -> 6)
# ==================================================

# Save the precomputed change-correlation value from the WVS/manifesto merge block.
save_tex_value(
  values = c(cor_share_prrp, cor_wave6),
  names = c("prrpSupChangeCor", "prrpSupportCor"),
  file_name = "output/values/values.tex",
  accuracy = 0.1
)
