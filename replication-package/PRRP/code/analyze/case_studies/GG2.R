# Load required libraries
library(haven)      # For reading .dta files
library(dplyr)      # For data manipulation
library(ggplot2)    # For plotting
library(forcats)    # For factor reordering
library(openxlsx)  # For writing Excel files

source("code/util/commonVars.R")

# Output path
output_path <- "output/figures/case_studies/Germany/"

# Read the dataset
data <- readRDS(file.path(cleandata_path, "Germany", "politbarometer.rds"))

### Response Codes to Labels 

description_dict <- list(
  `-99` = "No answer", # verified
  `-98` = "Don’t know", # verified
  `-94` = "Not collected", # verified
  `1` = "Pension provision, pension issues", # verified
  `5` = "Prices, inflation, cost of living", # verified
  `7` = "Wages and income, wage negotiations", # verified
  `10` = "Taxes, tax reform, fees", # verified
  `13` = "Economic situation, growth, recession", # verified
  `14` = "Unemployment, jobs", # verified
  `15` = "Environment, climate change, renewable energy", # verified
  `19` = "family / children / youth", # verified
  `20` = "Healthcare/Health policy/Long-term care insurance", # verified
  `23` = "Violence, extremism (left/right), demonstrations, Antifa", # verified
  `27` = "Party/political dissatisfaction/politician affairs", # verified
  `30` = "School & Education", # verified
  `54` = "Immigration, integration, asylum, refugees", # verified
  `55` = "Foreign policy, general", # verified, different countries and EU issues
  `56` = "National debt", # verified
  `59` = "Federal elections outcome", # verified
  `66` = "Other problems", # verified
  `77` = "social divide/ justice/ rich-poor", # verified
  `78` = "Morality, values", # verified
  `117` = "International banking crisis", # verified
  `134` = "Viruses & Pandemics" # verified
)

# Combined stacked bar graph

## colors
issue_colors <- c(
  # --- Economics (Reds/Oranges - #D53E4F) ---
  "Economic situation, growth, recession"                    = "#D53E4F",
  "Unemployment, jobs"                                       = "#F46D43",
  "Prices, inflation, cost of living"                        = "#FDAE61",
  "family / children / youth"                                = "#FEE08B",
  "Wages and income, wage negotiations"                      = "#E6F598",
  "Taxes, tax reform, fees"                                  = "#ABDDA4",
  "Rents, housing market"                                    = "#FF9800",

  # --- Culture (ex. immigration) (Blue - #1F78B4) ---
  "Pension provision, pension issues"                        = "#66C2A5",
  "Healthcare/Health policy/Long-term care insurance"        = "#3288BD",
  "Violence, extremism (left/right), demonstrations, Antifa" = "#5E4FA2",
  "social divide/ justice/ rich-poor"                        = "#9E0142",
  "Right-wing extremism, neo-Nazis"                          = "#B71C1C",
  "International banking crisis"                             = "#4CAF50",
  "Gender-neutral language"                                  = "#9C27B0",
  "Morality, values"                                         = "#FFC107",

  # --- Immigration (Light Blue - #A6CEE3) ---
  "Immigration, integration, asylum, refugees"               = "#A6CEE3",

  # --- Foreign Policy (Red - #E31A1C) ---
  "Foreign policy, general"                                  = "#1F78B4",
  "Other problems"                                           = "#B2DF8A",
  "National debt"                                            = "#33A02C",
  "Federal elections outcome"                                = "#FB9A99",
  "Viruses & Pandemics"                                      = "#880E4F",

  # --- Government & Politics (Purples/Pinks - #FF7F00) ---
  "Party/political dissatisfaction/politician affairs"       = "#E31A1C",
  "School & Education"                                       = "#FDBF6F",

  # --- Environment (Yellow/Green - #4CAF50) ---
  "Environment, climate change, renewable energy"            = "#FF7F00",

  # --- Other (Grey - #CAB2D6) ---
  "Other"                                                    = "#CAB2D6"
)

sorted_issue_names <- names(issue_colors)

issue_category_colors <- c(
  "Immigration"             = "#AA0000",  # Red
  "Culture (ex. immigration)" = "#FF0000",  # Light Red
  "Environment"             = "#4CAF50",  # Green
  "COVID"                   = "#880E4F",  # Purple
  "Other"                   = "#CAB2D6",  # Grey
  "Government & Politics"   = "#FF7F00",  # Orange (base)
  "Foreign Policy"          = "#FFAA1C",  # Yellow
  "Economics"       = "#1F78B4"   # Blue
)

sorted_category_names <- names(issue_category_colors)

# Mapping from specific issue to broader category
issue_to_category_map <- c(
  # --- Economics ---
  "Economic situation, growth, recession"                    = "Economics",
  "Unemployment, jobs"                                       = "Economics",
  "Prices, inflation, cost of living"                        = "Economics",
  "Wages and income, wage negotiations"                      = "Economics",
  "Taxes, tax reform, fees"                                  = "Economics",
  "Pension provision, pension issues"                        = "Economics",
  "social divide/ justice/ rich-poor"                        = "Economics",
  "National debt"                                            = "Economics",
  "International banking crisis"                             = "Economics",
  # --- Culture (ex. immigration) ---
  "family / children / youth"                                = "Culture (ex. immigration)",
  "Healthcare/Health policy/Long-term care insurance"        = "Other",
  "Violence, extremism (left/right), demonstrations, Antifa" = "Culture (ex. immigration)",
  "School & Education"                                       = "Culture (ex. immigration)",
  
  # --- Immigration ---
  "Immigration, integration, asylum, refugees"               = "Immigration",
  
  # --- Foreign Policy / Other ---
  "Foreign policy, general"                                  = "Other",
  
  # --- Government & Politics / Other ---
  "Federal elections outcome"                                = "Other",
  "Party/political dissatisfaction/politician affairs"       = "Other",
  
  # --- Environment (Culture (ex. immigration)) ---
  "Environment, climate change, renewable energy"            = "Culture (ex. immigration)",

  # Covid
  "Viruses & Pandemics"                                      = "Other",
  
  # --- Other / Other ---
  "Other problems"                                           = "Other",
  "Other"                                                    = "Other"
)

# Mutate data

processed_df <- data %>%
  filter(intyear <= 2019, intyear >= 2005) %>%
  select(intyear, v33, dwght, pwght) %>%
  rename(`Most important issue` = v33) %>%
  mutate(final_weight = dwght * pwght) %>%
  filter(
    !(!!sym("Most important issue") %in% c(-99, -98, -94)),
    !is.na(!!sym("Most important issue"))
  ) %>%
  mutate(
    issue_num = as.numeric(`Most important issue`),
    german_label = as.character(as_factor(`Most important issue`))
  ) %>%
  rowwise() %>%
  mutate(
    english_label = if (as.character(issue_num) %in% names(description_dict)) {
      description_dict[[as.character(issue_num)]]
    } else {
      german_label
    }
  ) %>%
  ungroup()

# Summarize and calculate top answers per year
top_answers <- processed_df %>%
  group_by(english_label) %>%
  summarise(
    w_n = sum(final_weight, na.rm=TRUE),  # weighted count
    issue_num = dplyr::first(issue_num),      # issue number
    .groups = "drop"
  ) %>%
  arrange(desc(w_n)) %>%
  filter(w_n > 4600) # Hack to get top 20 answers across all years

df_for_stack_bar_plot <- processed_df %>%
  mutate(
    issue_factor = factor(english_label, levels = sorted_issue_names),
    issue_category = ifelse(!is.na(issue_factor), issue_to_category_map[english_label], "Other"),
    issue_category_factor = factor(issue_category, levels = sorted_category_names)
  )

df_for_stack_bar_plot$issue_category_factor[is.na(df_for_stack_bar_plot$issue_category_factor)] <- "Other"

df_for_stack_bar_plot <- df_for_stack_bar_plot %>%
  group_by(intyear, issue_category_factor) %>%
  summarise(
    w_n = sum(final_weight, na.rm=TRUE),
    .groups = "drop"
  ) %>%
  group_by(intyear) %>%
  mutate(
    percent = w_n / sum(w_n) * 100
  )

# Plot stacked bar chart
gg1 <- ggplot(df_for_stack_bar_plot, aes(x = factor(intyear), y = percent, fill = issue_category_factor)) +
  geom_bar(stat = "identity", position = "stack", color = "black", linewidth = 0.3) +
  labs(x = "", y = "", fill = "") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_classic(base_size = 12) +
  scale_fill_manual(
    values = issue_category_colors,
    labels = function(x) sub(" \\(ex\\. immigration\\)", "\n(ex. immigration)", x)
  ) +
  theme(
    legend.position = "bottom",
    legend.box.spacing = unit(0, "pt"),
    legend.box.margin = ggplot2::margin(0, 0, 0, 0),
    legend.margin = ggplot2::margin(0, 0, 0, 0),
    legend.key.size = unit(0.9, "lines"),
    legend.spacing.x = unit(0, "pt"),
    plot.margin = ggplot2::margin(l = 0),
    axis.title.y = element_blank()
  ) +
  guides(fill = guide_legend(nrow = 1))

ggsave(plot = gg1,
       "stacked_bar_plot_categorized.pdf", path = output_path, width = 16.9, height = 9.5, units = "cm", dpi = 300)
