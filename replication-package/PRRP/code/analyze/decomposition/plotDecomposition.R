# *************************************************************************
# File description ----
# *************************************************************************
#
# This file creates decomposition analysis visualizations and summary statistics
# that break down changes in PRRP support across
# waves into component contributions.
#
# The main goal is to decompose vote share changes into:
#   (1) Residual component (unexplained variation)
#   (2) Party positioning changes
#   (3) Voter attribute changes
#   (4) Priority/issue salience changes
#
# The analysis is performed across multiple subgroups:
#   - Overall population (main decomposition)
#   - Birth cohort analysis (cohort decomposition)
#   - New party entries (entry decomposition)
#
# Outputs:
#
# Figures:
#   - Waterfall plots showing decomposition breakdown
#   - Country-level decomposition plots
#   - Progressive/gradual revelation plots (by component)
#   - Aggregate and subgroup-specific visualizations
#
# TeX Values:
#   - Percentage contribution of each component (all time)
#   - Entry residual rise (2017-2020)
#   - Green party and cohort priority contributions
#   - Cohort attribute contributions
#
#
# Clear the R workspace
rm(list=ls())

# *************************************************************************
# SETUP ----
# *************************************************************************

library(haven)
library(tidyr)
library(dplyr)
library(savetexvalue)

# Load function
source("code/util/commonVars.R")
source("code/analyze/decomposition/functions_for_plotDecomposition.R")

# Main decomposition
DEC_4_6 <- readRDS(paste0(decomposition_path, "/dec_4_6.Rds"))

# Decomposition for cohort
DEC_4_6_COHORT <- readRDS(paste0(decomposition_path, "/dec_4_6_cohort.Rds"))

# Decomposition with new entries: Before period is the waves (1-3 means waves 4-6). After period only one values matter .1 is the new party residual. 
DEC_4_6_NEW <- readRDS(paste0(decomposition_path, "/dec_4_6_new.Rds"))
rownames(DEC_4_6_NEW) = "entryData"

# Add new entries to main dataset temporarily for processing
DEC_4_6 = rbind(DEC_4_6, DEC_4_6_NEW)

# *************************************************************************
#  PROCESS ----
# *************************************************************************

processData <- function(DEC_4_6) {
  # Format is Wave.Component. Changing from waves 1-3 to waves 4-6.
  stopifnot(sum(colnames(DEC_4_6) != c("w1.1", "w2.1", "w3.1", "w1.2", "w2.2", "w3.2", "w1.3", "w2.3", "w3.3", "w1.4", "w2.4", "w3.4"))==0)
  
  colnames(DEC_4_6) = paste0("w", as.numeric(gsub("w([1-3])\\.[1-4]", "\\1", colnames(DEC_4_6))) + 3, 
                             gsub("w[1-3](\\.[1-4])", "\\1", colnames(DEC_4_6)))
  
  # Adding component details
  names(DEC_4_6) <- gsub("\\.4", "\\.Priorities", names(DEC_4_6))
  names(DEC_4_6) <- gsub("\\.3", "\\.Voter", names(DEC_4_6))
  names(DEC_4_6) <- gsub("\\.2", "\\.Parties", names(DEC_4_6))
  names(DEC_4_6) <- gsub("\\.1", "\\.Residual", names(DEC_4_6))
  
  # Adding country to data frame
  DEC_4_6$country = rownames(DEC_4_6)
  
  # Create a table at the country-wave-component level with difference in each component over time
  DEC_4_6_LONG <- DEC_4_6 %>% 
    processDecomposition() %>% 
    filter(!is.na(value)) 
  
  # Add total component
  DEC_4_6_LONG = rbind(DEC_4_6_LONG,
                       DEC_4_6_LONG %>% 
                         group_by(waveNum, country) %>% 
                         mutate(sum = sum(value)) %>% 
                         select(-component, -value) %>% 
                         distinct() %>% 
                         mutate(component="Total") %>% 
                         select(waveNum, country, waveLabels, component, value=sum) )
  
  # Split  decomposition with entry from general decomposition
  DEC_4_6_GENERAL <- DEC_4_6_LONG %>% 
    filter(country != "entryData")
  
  # Rescale to 100%
  rescaleValue_46 = 1 / DEC_4_6_GENERAL %>% 
    filter(country=="total" & component == "Total") %>% 
    pull(value) %>% sum()
  
  DEC_4_6_GENERAL <- DEC_4_6_GENERAL %>% 
    mutate(value = ifelse(country == "total", value*rescaleValue_46, value))
  
  # Remove first wave which is always zero
  DEC_4_6_GENERAL <- DEC_4_6_GENERAL %>% 
    filter(waveNum != min(DEC_4_6_GENERAL$waveNum) | component == "Total") 
  
  # Add labels
  DEC_4_6_GENERAL = addComponentLabels(DEC_4_6_GENERAL)
  
  # Prepare for plot, add variable to find if country has data for wave 5 (needed for country figures)
  ALL_DATA_4 <- prepareDecompositionPlot(DEC_4_6_GENERAL) %>% group_by(country) %>% 
    mutate(haveWave5 = sum(waveNum==5)>0) %>% 
    ungroup()
  
  # Data for aggregate plot
  TOTAL_4 <- ALL_DATA_4 %>% 
    filter(country=="total")
  
  return(list(ALL_DATA_4 = ALL_DATA_4, TOTAL_4 = TOTAL_4, DEC_4_6_LONG = DEC_4_6_LONG))
}

DEC_4_6 <- processData(DEC_4_6)
DEC_4_6_LONG <- DEC_4_6$DEC_4_6_LONG
ALL_DATA_4 <- DEC_4_6$ALL_DATA_4
TOTAL_4 <- DEC_4_6$TOTAL_4

DEC_4_6_COHORT <- processData(DEC_4_6_COHORT)
ALL_DATA_4_COHORT <- DEC_4_6_COHORT$ALL_DATA_4
TOTAL_4_COHORT <- DEC_4_6_COHORT$TOTAL_4

# .process new entries plot ----
# .........................................................................

# In this dataset the residual component is the residual of new parties 
NEW_PARTY_RES <- DEC_4_6_LONG %>% 
  filter(country=="entryData" & component=="Residual") %>%
  mutate(component = "New Parties Res")
  
# Create template for spltting residual (no values here, just the table rows)
EXISTING_RES = DEC_4_6_LONG %>% filter(country=="total" & component=="Residual") %>% 
  mutate(component="Existing Parties Res", value = ifelse(waveNum==4, 0, NA))

# Add existing party residual, take total residual from the main decomposition and substract the new party residual 
EXISTING_RES = EXISTING_RES %>% arrange(waveNum) %>% 
  mutate(value = DEC_4_6_LONG %>%
  filter(country=="total" & component=="Residual") %>%
  arrange(waveNum) %>%
  pull(value) - NEW_PARTY_RES %>%
  arrange(waveNum) %>%
  pull(value))

# For everything besides new and existing residual figure is the same
other_components <- DEC_4_6_LONG %>%
  filter(
    country == "total" &
      component %in% c("Voter", "Priorities", "Parties", "Total")
  )

DEC_4_6_ENTRY <- rbind(NEW_PARTY_RES, EXISTING_RES) %>%
  rbind(other_components)

# Remove first wave which is always zero
DEC_4_6_ENTRY <- DEC_4_6_ENTRY %>% mutate(country="entryData") %>%
  filter(waveNum != min(DEC_4_6_ENTRY$waveNum) | component == "Total") 

# Add labels and prepare for plot
DEC_4_6_ENTRY = addComponentLabels(DEC_4_6_ENTRY, componentNum = 5)

TOTAL_4_ENTRY <- prepareDecompositionPlot(DEC_4_6_ENTRY)


# *************************************************************************
#  SAVE VALUES ----
# *************************************************************************
save_decomposition_value <- function(data, comp, name, accuracy = 0.1) {
  # Sum the unrounded pieces; save_tex_value rounds once (summing rounded pieces can be off by 0.1)
  value <- data %>%
    filter(component == comp) %>%
    summarise(total = sum(value)) %>%
    pull(total)
  
  save_tex_value(
    values = value,
    names = name,
    file_name = "output/values/modelValues.tex",
    percent = TRUE,
    accuracy = accuracy
  )
}


# Save component contributions
for (comp in c("Residual", "Parties", "Voter", "Priorities")) {
  save_decomposition_value(TOTAL_4, comp, paste0("per", comp, "AllTime"))
}

save_decomposition_value(TOTAL_4_COHORT, "Voter", "cohortAttributeNum")

# *************************************************************************
# PLOTS ----
# *************************************************************************

# .Aggregated ----
# .........................................................................
# Save the manuscript figures.
plot_list <- list(
  list(data = TOTAL_4, file = "output/figures/decomposition/waterfall_total_46.pdf"),
  list(data = TOTAL_4_ENTRY, file = "output/figures/decomposition/waterfall_entry_46.pdf")
)
for (plot_info in plot_list) {
  ggsave(plot = createCommonPlot(plot_info$data), 
         filename = plot_info$file, width = figureWidth, height = 12, units = "cm")
  
}
