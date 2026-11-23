# *************************************************************************
# File description ----
# *************************************************************************
#
# This file visualizes the largest wave-6 phi coefficients for four indices:
# economic, cultural, older-white-men, and non-establishment.
#
# Main goals:
#   (1) Build index-specific phi loadings from style-model estimates
#   (2) Keep the top coefficients (by absolute value) for each index
#   (3) Export comparison plots for all indices and for grouped subsets
#
# Outputs:
#   - output/figures/phis/phi_newZs_pos.pdf
#   - output/figures/phis/phi_newZs_demo.pdf
#
# *************************************************************************

library(ggplot2)
library(dplyr)
library(tidyr)
library(tibble)

rm(list = ls())
source("code/util/commonVars.R")
source("code/analyze/party_positions/manifesto_general_functions.R")

grps = 4:6
filepath = paste0(model_results_path, "/model_wave_")

X.list = lapply(grps, function(g) {readRDS(paste0(filepath,g,"_style_google.Rds"))$X})
Z.list = lapply(grps, function(g) {readRDS(paste0(filepath,g,"_style_google.Rds"))$Z})

mod.list = lapply(grps, function(g) {readRDS(paste0(filepath,g,"_style_google.Rds"))$mod})
phi.list = lapply(mod.list, function(m) {m$phi})

phi = as.data.frame(phi.list[[length(phi.list)]])

rownames(phi) = colnames(X.list[[length(X.list)]])
colnames(phi) = colnames(Z.list[[length(Z.list)]])
phi = phi %>%
  add_econsoc_index() %>% 
  mutate(owm_index = gender+age-minority,
         nones_index = -(education+marital_status+occupation-divorce)) %>% 
  select(econ_index, soc_index, owm_index, nones_index) %>% 
  arrange(across(everything(), ~desc(abs(.))))

phi = rownames_to_column(phi,var="X_var")
phi_long = pivot_longer(phi,cols = !X_var,names_to = "Z_var")

names_for_graph$plotName[names_for_graph$plotName=="Confidence in environmental protection mvt"] <- "Confidence in environmental protection movement"

get_top_phi <- function(phi_long, index_name, top_n = 10) {
  phi_long %>%
    dplyr::filter(Z_var == index_name) %>%
    rename(name = X_var) %>%
    left_join(names_for_graph) %>%
    mutate(value = as.numeric(value)) %>%
    dplyr::filter(!is.na(plotName)) %>%
    slice_max(abs(value), n = top_n)
}

prepare_phi_plot_data <- function(df, facet_levels) {
  df %>%
    mutate(
      newName = if_else(value > 0, plotName, paste0(plotName, " (-) ")),
      newValue = abs(value),
      facet_name = factor(facet_name, levels = facet_levels)
    ) %>%
    group_by(facet_name) %>%
    arrange(newValue, .by_group = TRUE) %>%
    ungroup() %>%
    mutate(newNameOrder = paste(newName, facet_name, sep = "___")) %>%
    mutate(newNameOrder = factor(newNameOrder, levels = newNameOrder))
}

make_phi_plot <- function(df) {
  ggplot(df, aes(x = newNameOrder, y = newValue)) +
    theme_classic() +
    theme(axis.title = element_blank()) +
    geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) +
    geom_point(size = 2) +
    coord_flip() +
    facet_wrap(~facet_name, scales = "free_y") +
    scale_x_discrete(labels = function(x) stringr::str_wrap(gsub("___.*$", "", x), width = 35)) +
    theme(strip.background = element_blank(),
          strip.text.x = element_text(face = "bold"))
}

index_labels <- c(
  econ_index = "Economic Index",
  soc_index = "Cultural Index",
  owm_index = "Older White Men Index",
  nones_index = "Non-establishment Index"
)

phi_by_index <- lapply(names(index_labels), function(index_name) {
  temp <- get_top_phi(phi_long, index_name)
  temp$facet_name <- index_labels[[index_name]]
  temp
})
names(phi_by_index) <- names(index_labels)

phi_pos <- prepare_phi_plot_data(
  bind_rows(phi_by_index[c("econ_index", "soc_index")]),
  c("Economic Index", "Cultural Index")
)

phi_demo <- prepare_phi_plot_data(
  bind_rows(phi_by_index[c("owm_index", "nones_index")]),
  c("Older White Men Index", "Non-establishment Index")
)

phi_all <- prepare_phi_plot_data(
  bind_rows(phi_by_index),
  unname(index_labels)
)

ggsave("output/figures/phis/phi_newZs_pos.pdf", plot = make_phi_plot(phi_pos), width = 20, height = 12, units = "cm")

ggsave("output/figures/phis/phi_newZs_demo.pdf", plot = make_phi_plot(phi_demo), width = 20, height = 12, units = "cm")
