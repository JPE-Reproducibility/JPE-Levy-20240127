# *************************************************************************
# File description ----
# *************************************************************************
#
# This file creates a map visualization of populist radical right party (PRRP)
# support across European countries in Wave 6.
#
# The main goal is to visualize geographic patterns of PRRP support by:
#   (1) Calculating weighted average PRRP support from survey data (Wave 6)
#   (2) Merging with actual election results from Manifesto data
#   (3) Creating choropleth maps showing support intensity across regions
#   (4) Displaying both survey-based and electoral support measures
#
# The analysis includes:
#   - Survey-based PRRP support (weighted means by country)
#   - Electoral support shares from the closest available election
#   - Correlation between survey and electoral measures
#
# Outputs:
#
# Figures:
#   - Main choropleth map with survey support and election results
#   - Clean version for presentations (without election results)
#   - Slide version with adjusted formatting
#
# TeX Values:
#   - Correlation between survey and electoral PRRP support
#
#
# *************************************************************************
# SETUP ----
# *************************************************************************
library(ggplot2)
library(dplyr)
library(sf)
library(rworldmap)
library(haven)
library(tibble)
library(savetexvalue)

rm(list = ls())

source("code/util/commonVars.R")
outdir <- paste0(github_path,"output/figures/WVS/populist_support_maps/")

# *************************************************************************
# Read and prep WVS data ----
# *************************************************************************

wvs_df <- read_dta(pathWVS_Data)


# Compute Wave 6 survey support and keep the country-specific survey year.
prrp_support <- wvs_df %>%
  filter(BB_Wave_num == 6) %>%
  group_by(BB_Country) %>%
  summarise(
    prrp_support = weighted.mean(Y_FR_Pop, BB_Weight_CW),
    survey_year = median(BB_Year, na.rm = TRUE),
    .groups = "drop"
  )


## Adding the Manifesto data to present the actual rrp_support for RRP.
manifesto_df_raw <- readRDS(paste0(cleandata_path, "/manifesto/manifesto_populist.rds"))


# Compute PRRP vote share for each country-election year.
# Note - can update this with newer manifesto data later
manifesto_df_wrangled <- manifesto_df_raw %>%
  select(manifesto_country_name, pervote, manifestoYear, Y_FR_Pop, Y_manifesto_party_id) %>%
  mutate(manifesto_country_name = if_else(manifesto_country_name == "United Kingdom", "UK", manifesto_country_name)) %>%
  filter(manifestoYear %in% c(2000:2020)) %>%
  group_by(manifesto_country_name, manifestoYear) %>%
  summarise(
    voting_support_share_RRP = sum(Y_FR_Pop * pervote, na.rm = T)/sum(pervote, na.rm = TRUE),
    .groups = "drop"
  )

# Select the election closest to each country's Wave 6 survey year.
manifesto_df_wrangled_by_closest_year <- prrp_support %>%
  select(BB_Country, survey_year) %>%
  left_join(manifesto_df_wrangled, by = c("BB_Country" = "manifesto_country_name")) %>%
  filter(!is.na(survey_year), !is.na(manifestoYear)) %>%
  mutate(year_distance = abs(manifestoYear - survey_year)) %>%
  group_by(BB_Country) %>%
  arrange(year_distance, desc(manifestoYear), .by_group = TRUE) %>%
  slice(1) %>%
  ungroup() %>%
  select(BB_Country, manifestoYear, voting_support_share_RRP)

## Adding rrp_support_share_RRP_sum to rrp_support
prrp_support <- left_join(prrp_support, manifesto_df_wrangled_by_closest_year,
                          by = "BB_Country") %>%
  select(-manifestoYear, -survey_year)

# *************************************************************************
# Create map data ----
# *************************************************************************

europe_countries <- c(prrp_support$BB_Country,
                      "Andorra", "Belgium", "Bulgaria", "Croatia",
                      "Romania", "kosovo", "Ukraine", "Czech Republic",
                      "Belarus", "Ireland", "Monaco", "Serbia", "Moldova",
                      "Bosnia and Herzegovina", "Albania", "North Macedonia",
                      "Latvia", "Montenegro", "Luxemburg", "Malta",
                      "Lichtenstein", "Slovakia")
#Merge with map data
map_df <- map_data("world", region = europe_countries) %>%
  left_join(prrp_support, by = c("region" = "BB_Country")) %>%
  filter(!subregion %in% c("Alaska", "Hawaii", "Svalbard"))

map_sf <- st_make_valid(st_as_sf(getMap(resolution = "li")))

#Get centroids of countries for annotations
centroids <- st_centroid(map_sf) %>%
  st_coordinates() %>%
  as.data.frame() %>%
  mutate(region = map_sf$ADMIN) %>% 
  mutate(region = if_else(region == "United Kingdom", "UK", region),
         region = if_else(region == "United States of America", "USA", region)) %>%
  filter(region %in% prrp_support$BB_Country)

# Get abbreviations from the data in the rworldmap package,
# replace the names of some countries so we can join the datasets
abb_df <- countryExData %>%
  select(Country, ISO3V10) %>%
  mutate(Country = if_else(Country == "United Kingdom", "UK", Country)) %>%
  mutate(Country = if_else(Country == "United States", "USA", Country)) %>% 
  mutate(Country = if_else(Country == "Czech Rep.", "Czech Republic", Country))

#Merge them to map data, create labels
map_df <- map_df %>%
  mutate(
         prrp_support = 100 * prrp_support,
         prrp_support_share = 100 * voting_support_share_RRP,
         prrp_support_lab = round(prrp_support , digits = 1),
         prrp_support_share_lab = round(prrp_support_share, digits = 1)) %>%
  left_join(centroids, by = "region") %>%
  left_join(abb_df, by = c("region" = "Country")) %>%
  mutate(prrp_support_lab = if_else(ISO3V10 %in% c("SVN",  "SVK", "AUT"), 
                                    paste0(ISO3V10, ": ", prrp_support_lab, "%"), 
                                    paste0(ISO3V10, "\n", prrp_support_lab, "%"))) %>%
  mutate(prrp_support_share_lab = if_else(ISO3V10 %in% c("SVN",  "SVK", "AUT"), 
                                          paste0("\n","\n", "(", prrp_support_share_lab, "%",")"),
                                          paste0("\n","\n", "\n","(", prrp_support_share_lab, "%",")"))) %>% 
  rename(x = X, y = Y)


# *************************************************************************
# Plot ----
# *************************************************************************

# Adjust Croatia's labels manually
# Labels - Average support for prrp from 2017 - 2020
avg_supp_lab <-  map_df %>%
  select(prrp_support_lab, x, y) %>%
  distinct() %>%
  mutate(y = if_else(prrp_support_lab == "HRV\n0.4%", y-0.6, y))

# Labels - Share of rrp support in the closest election
last_elect_share_lab <- map_df %>%
  select(prrp_support_share_lab, x, y) %>%
  distinct() %>%
  mutate(y = if_else(prrp_support_share_lab == "HRV\n0.4%", y-0.6, y))

create_base_plot <- function(map_df, avg_supp_lab, last_elect_share_lab,
                             size_avg, size_share, is_clean = FALSE,
                             legend_title_size = 8, legend_text_size = 7) {
  plot <- ggplot(map_df, aes(x = long, y = lat, group = group)) +
    geom_polygon(aes(fill = prrp_support), color = "white") +
    theme_void() +
    scale_fill_viridis_c(option = "F", begin = 0.5, direction = -1, alpha = 0.8,
                         na.value = "gray90", name = "PRRP\nSupport", breaks = 20 * (0:4),
                         labels = 20 * 0:4, limits = c(0,80)) +
    theme(legend.position = c(0.1, 0.5), legend.key.size = unit(0.6, "cm"),
          legend.title = element_text(size = legend_title_size),
          legend.text = element_text(size = legend_text_size))
  
  plot_with_annotations <- plot + 
    annotate(geom = "text", label = avg_supp_lab$prrp_support_lab, x = avg_supp_lab$x,
             y = avg_supp_lab$y, size = size_avg, hjust = 0.5, fontface = "bold", lineheight = 0.85)
  
  if (!is_clean) {
    plot_with_annotations <- plot_with_annotations +
      annotate(geom = "text", label = last_elect_share_lab$prrp_support_share_lab, x = last_elect_share_lab$x,
               y = last_elect_share_lab$y, size = size_share, hjust = 0.5, fontface = "bold", 
               lineheight = 1, color = "grey39")
  }
  
  
  return(plot_with_annotations)
}

# Paper figure: enlarge the labels and legend so the in-figure text is closer to
# the figure-notes size on the page. The map is dense, so the country labels are
# kept a touch below full notes size to avoid overlap.
plot_base <- create_base_plot(map_df, avg_supp_lab, last_elect_share_lab,
                              size_avg = 2.8, size_share = 2.1, is_clean = TRUE,
                              legend_title_size = 9, legend_text_size = 11)

output_targets <- strsplit(Sys.getenv("PRRP_MAP_OUTPUTS", unset = "all"),
                           ",", fixed = TRUE)[[1]]
output_targets <- trimws(output_targets)

should_save <- function(target) {
  "all" %in% output_targets || target %in% output_targets
}

if (should_save("paper")) {
  ggsave(filename = paste0(outdir, "prrp_support_map_wave6.pdf"), plot = plot_base,
         width = 19, height = 10.5, units = "cm", dpi = 72)
}
