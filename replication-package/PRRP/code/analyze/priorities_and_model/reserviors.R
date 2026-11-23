# *************************************************************************
# File description ----
# *************************************************************************
#
# This file simulates counterfactual support for selected PRRP parties across
# European and U.S. voters using the estimated wave-6 voting model.
#
# Main goals:
#   (1) Load the estimated wave-6 model and voter data, including U.S. voters
#   (2) Apply the same standardization used in model estimation
#   (3) Simulate party-choice probabilities under German and French choice sets
#   (4) Aggregate predicted support by country using survey/country weights
#   (5) Map counterfactual support across countries
#
# Outputs:
#   - datasets/model/reservior.csv
#   - output/figures/counterfactual_maps/france_counterfactual_*.pdf/png
#   - output/figures/counterfactual_maps/germ_counterfactual_*.pdf/png
#
# *************************************************************************

library(ggplot2)
library(dplyr)
library(maps)
library(sf)
library(rworldmap)
library(tidyr)
library(tibble)
library(conflicted)

# Prefer dplyr verbs when map packages expose overlapping function names.
conflict_prefer("filter", "dplyr")

# Clear the workspace before loading model and map inputs.
rm(list = ls())

# Source project paths, model functions, and cleaned model data objects.
source("code/util/commonVars.R")
source("code/model/func_nuclear_and_pgd.R")
source("code/model/clean_data_model.R")

outdir <- "output/figures/counterfactual_maps/"

filepath = paste0(model_results_path, "/model_wave_")

#load data
res6 = readRDS(paste0(filepath,6,".Rds"))


#X = res6$X
US = readRDS(file.path(cleandata_path, "WVS", "us_voters.rds"))
US.weights = US$BB_Weight_CW
X = res6$X


Z = res6$Z
mod = res6$mod
phi = mod$phi
FE = mod$intercept

X.os = res6$X.os
Z.os = res6$Z.os


# Match the model-estimation transformation:
# X is weighted-standardized with IVS sampling weights, while Z is unweighted.
US = US[,colnames(X)]
X = corpcor::wt.scale(X, w = res6$W)
X.center = attr(X, "scaled:center")
X.scale = attr(X, "scaled:scale")
US = sweep(as.matrix(US), 2, X.center, "-")
US = sweep(US, 2, X.scale, "/")
US[,"X_op_confidence_EU"] = 0
X = rbind(X, US)
X.os = c(X.os,rep("USA", nrow(US)))
Z = scale(Z)

# Define the counterfactual party systems and target PRRP parties.
target_parties <- tibble::tribble(
  ~support_col, ~target_country, ~party_id,
  "germ", "Germany6", "19806",
  "fra", "France6", "13446"
)

# Simulate target-party support when every voter faces one target-country party system.
simulate_counterfactual_party <- function(target_country, party_id) {
  pred <- exp(simulate.model(X, Z, phi, FE, rep(target_country, nrow(X)), Z.os)$logP)
  colnames(pred) <- rownames(Z)
  pred[, party_id]
}

# Bind simulated target-party support to each voter's observed country and weight.
support_list <- setNames(
  lapply(seq_len(nrow(target_parties)), function(i) {
    simulate_counterfactual_party(target_parties$target_country[i], target_parties$party_id[i])
  }),
  target_parties$support_col
)

# Add each voter's observed country and analysis weight to the simulated support.
res <- as.data.frame(support_list) %>%
  mutate(country = X.os,
         weight = c(res6$W, US.weights))

#get aggregate voters by country using IVS sampling weights
final.res = res %>%
  group_by(country) %>%
  summarise(
    germ = weighted.mean(germ, weight, na.rm = TRUE),
    fra = weighted.mean(fra, weight, na.rm = TRUE),
    .groups = "drop"
  )
final.res %>% arrange(germ)
final.res %>% arrange(fra)
cor(final.res$germ, final.res$fra)
write.csv(final.res,paste0(model_results_path, "/reservior.csv"), row.names = FALSE)

#Read reservoir data
reservoir_df <- read.csv(paste0(model_results_path, "/reservior.csv"), header = T)
reservoir_df <- reservoir_df %>%
  mutate(country = gsub("6", "", country, fixed = T)) %>%
  select(-any_of("X"))

europe_countries <- unique(c(reservoir_df$country,"Andorra","Belgium", "Bulgaria", "Croatia", "Romania", "kosovo","Ukraine", "Czech Republic",
                             "Belarus","Ireland","Monaco", "Serbia", "Moldova", "Bosnia and Herzegovina", "Albania",
                             "North Macedonia", "Latvia", "Montenegro", "Luxemburg", "Malta", "Lichtenstein"))
#Merge with map data
map_df <- map_data("world", region = europe_countries) %>%
  left_join(reservoir_df, by = c("region" = "country")) %>%
  dplyr::filter(!subregion %in% c("Alaska", "Hawaii", "Svalbard"))

map_sf <- st_make_valid(st_as_sf(getMap(resolution = "li")))

# Get country centroids and name the coordinate columns immediately.
centroids <- st_centroid(map_sf) %>%
  st_coordinates() %>%
  as.data.frame() %>%
  transmute(region = map_sf$ADMIN, x = X, y = Y) %>%
  mutate(region = dplyr::if_else(region == "United Kingdom", "UK", region),
         region = dplyr::if_else(region == "United States of America", "USA", region)) %>%
  filter(region %in% reservoir_df$country)
# Get abbreviations

abb_df <- iso3166 %>%
  select(ISOname, a3) %>%
  mutate(ISOname = if_else(ISOname == "United Kingdom of Great Britain and Northern Ireland",
                           "UK", ISOname),
         ISOname = if_else(ISOname == "United States", "USA", ISOname))
#Merge them to map data, create labels
map_df <- map_df %>%
  mutate(
    germ = 100 * germ,
    fra = 100 * fra,
    germ_lab = round(germ , digits = 1),
    fra_lab = round(fra , digits = 1)) %>%
  left_join(centroids, by = "region") %>%
  left_join(abb_df, by = c("region" = "ISOname")) %>%
  mutate(germ_lab = if_else(a3 %in% c("SVN",  "SVK", "AUT"), paste0(a3, ": ", germ_lab, "%"), paste0(a3, "\n", germ_lab, "%")),
         fra_lab = if_else(a3 %in% c("SVN",  "SVK", "AUT"), paste0(a3, ": ", fra_lab, "%"), paste0(a3, "\n", fra_lab, "%")))

# Get coordinates for non-US countries
coord_no_US <- map_df %>%
  filter(region != "USA") %>%
  summarise(min_lat = min(lat), max_lat = max(lat), min_long = min(long)-10, max_long = max(long)) %>%
  pivot_longer(cols = everything())
lat_no_US <- coord_no_US$value[1:2]
long_no_US <- coord_no_US$value[3:4]

# Get coordinates for US
coord_US <- map_df %>%
  filter(region == "USA") %>%
  summarise(min_lat = min(lat), max_lat = max(lat), min_long = min(long) , max_long = max(long)) %>%
  pivot_longer(cols = everything())
lat_US <- coord_US$value[1:2]
long_US <- coord_US$value[3:4]
US_lat_long_ratio <- (lat_US[2]-lat_US[1])/(long_US[2]-long_US[1])

US_annotate_xmin <- long_no_US[1]
US_annotate_xmax <- US_annotate_xmin + (long_no_US[2]-long_no_US[1])*0.3
US_annotate_ymin <- lat_no_US[1]
US_annotate_ymax <- US_annotate_ymin + (long_no_US[2]-long_no_US[1])*0.3*US_lat_long_ratio

map_df <- map_df %>%
  mutate(x = if_else(region == "USA", mean(long_US), x),
         y = if_else(region == "USA", mean(lat_US), y),
         only_fra = if_else(region == "France", fra, NA_real_),
         only_germ = if_else(region == "Germany", germ, NA_real_))

# common functions for plotting
create_base_plot <- function(data, fill_col, labels, label_col, base_size = 5, legend_size = 8, key_size, text_size) {
  ggplot(data, aes(x = long, y = lat, group = group)) +
    geom_polygon(aes(fill = !!sym(fill_col)), color = "white") +
    theme_void(base_size = base_size) +
    scale_fill_viridis_c(
      option = "F", begin = 0.5, direction = -1, alpha = 0.8,
      na.value = "gray90", name = "Predicted\nSupport",
      breaks = seq(0, 35, by = 5), labels = seq(0, 35, by = 5), limits = c(0, 35)) +
    theme(
      legend.position = c(0.1, 0.6),
      legend.key.size = grid::unit(key_size, "cm"),
      legend.title = element_text(size = legend_size - 2),
      legend.text = element_text(size = legend_size - 1)) +
    annotate(
      geom = "text", label = labels[[label_col]], x = labels$x, y = labels$y,
      size = text_size, fontface = "bold", lineheight = 0.85, hjust = 0.5)
}

add_us <- function(plot, us_plot, xmin, xmax, ymin, ymax) {
  plot +
    annotation_custom(grob = ggplotGrob(us_plot), xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax) +
    geom_rect(
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      alpha = 0, color = "black", linetype = "dotted")
}


# Save all map variants for one target country.
save_counterfactual_maps <- function(prefix, fill_col, label_col) {
  labels <- map_df %>%
    select(x, y, all_of(label_col)) %>%
    distinct() %>%
    mutate(y = if_else(.data[[label_col]] == "HRV\n0.4%", y - 1.3, y))

  p_main <- create_base_plot(map_df, fill_col, labels, label_col, key_size = 0.6, text_size = 2)

  p_no_US <- p_main + xlim(long_no_US) + ylim(lat_no_US)
  p_US <- p_main + xlim(long_US) + ylim(lat_US) + theme(legend.position = "none")

  p_tot <- add_us(p_no_US, p_US, US_annotate_xmin, US_annotate_xmax, US_annotate_ymin, US_annotate_ymax)

  ggsave(plot = p_tot, filename = paste0(outdir, prefix, "_counterfactual_with_US.pdf"),
         dpi = 72, useDingbats = TRUE, width = 19, height = 10.5, units = "cm")

}

# Save France and Germany counterfactual maps with the existing filenames.
save_counterfactual_maps("france", "fra", "fra_lab")
save_counterfactual_maps("germ", "germ", "germ_lab")
