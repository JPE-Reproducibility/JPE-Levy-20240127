# ***********************************
# Functions for Decompositions Graphs
# ***********************************

library(tidyverse)

# **************************
# Data adaptations functions
# **************************

# Turn the decomposition data into one organized table with waveNum and label, country, component, and value
processDecomposition <- function(DATA) {
  
  # Converting everything to differences
  DATA <- DATA %>% rename(w5.ResidualOrig = w5.Residual, w5.PartiesOrig = w5.Parties, w5.VoterOrig = w5.Voter, w5.PrioritiesOrig = w5.Priorities,
                          w6.ResidualOrig = w6.Residual, w6.PartiesOrig = w6.Parties, w6.VoterOrig = w6.Voter, w6.PrioritiesOrig = w6.Priorities) %>% 
    mutate(w5.Residual = w5.ResidualOrig - w4.Residual, 
           w5.Parties = w5.PartiesOrig - w4.Parties, 
           w5.Voter = w5.VoterOrig - w4.Voter, 
           w5.Priorities = w5.PrioritiesOrig - w4.Priorities,
           w6.Residual = w6.ResidualOrig - ifelse(is.na(w5.ResidualOrig), w4.Residual, w5.ResidualOrig),
           w6.Parties = w6.PartiesOrig - ifelse(is.na(w5.PartiesOrig), w4.Parties, w5.PartiesOrig),
           w6.Voter = w6.VoterOrig - ifelse(is.na(w5.VoterOrig), w4.Voter, w5.VoterOrig),
           w6.Priorities = w6.PrioritiesOrig - ifelse(is.na(w5.PrioritiesOrig), w4.Priorities, w5.PrioritiesOrig)) %>% 
    select(-w5.ResidualOrig, -w5.PartiesOrig, -w5.VoterOrig, -w5.PrioritiesOrig, -w6.ResidualOrig, -w6.PartiesOrig, -w6.VoterOrig, -w6.PrioritiesOrig)
  
  # One row for each wave*component, and separate columns for wave and component
  DATA_LONG = pivot_longer(DATA, cols = starts_with("w"), names_to = "variable")
  
  DATA_LONG <- extract(DATA_LONG, variable, into = c("wave", "component"), regex = "(w[1-9])\\.(.*)")
  
  # Add wave labels
  DATA_LONG$waveNum = as.numeric(substr(DATA_LONG$wave, 2, 3))
  DATA_LONG <- merge(DATA_LONG, waveLabelsDF, by="waveNum", all.x=TRUE)
  DATA_LONG <- DATA_LONG %>% select(-wave)
  
  return (DATA_LONG)
}


# Add x-axis labels
addComponentLabels <- function(DT, componentNum=4) {
  minYear = ifelse(min(DT$waveNum)==3, "1999", "2005")
  
  componentLevels = c("Residual", "Parties", "Voter", "Priorities", "Total")
  levelsNoTotal = componentLevels[1:4]
  if (componentNum==5) {
    componentLevels = c("Existing Parties Res", "New Parties Res", "Parties", "Voter", "Priorities", "Total")
    levelsNoTotal = componentLevels[1:5]
  }
  
  # Create component facts and label for component*year. For Total component start from the first year
  DT <- DT %>% mutate(componentF = factor(component, levels = componentLevels)) %>%
    arrange(country, waveNum, componentF) %>% 
    mutate(label = paste0(waveLabels, ", ", component)) %>%
    mutate(label = ifelse(component=="Total", gsub("(.*)(\\-.*)", paste0(minYear, "\\2"), label), label)) %>%
    arrange(waveNum) 
  
  DT$labelF = factor(DT$label, levels = unique(DT$label))

  return(DT)
}


# Add location of values
prepareDecompositionPlot <- function (DATA, adjustHeight = 0.025) {
  
  DATA <- DATA %>% mutate(value = ifelse(component=="Total", -value, value))
  adjustHeight = adjustHeight
  
  DATA <- DATA %>% group_by(country) %>% 
    arrange(country, waveNum, componentF) %>% # Need arrange since using lag in next row
    mutate(
      ymin = ifelse(component=="Total", 0, cumsum(ifelse(component=="Total", 0, value))),
      ymax = dplyr::lag(cumsum(ifelse(component=="Total", 0, value)), default = 0),
      xmin = c(head(labelF, -1), NA),
      xmax = c(tail(labelF, -1), NA),
      lineheight = ifelse(component=="Total", ymax, ymin),
      Impact = ifelse(component=="Total", "Total", ifelse(value > 0, "Increase", "Decrease")),
      shortLabel = ifelse(component=="Total" | value==0, NA, 
                          paste0(ifelse(Impact=="Increase", "+", ""), scales::percent(value, accuracy = 1))),
      labelPositionY = ifelse(Impact=="Increase", pmax(ymin, ymax) +adjustHeight, pmin(ymin, ymax)-adjustHeight),
      labelPositionFlexY = ifelse(Impact=="Increase", pmax(ymin, ymax) +max(value/10), pmin(ymin, ymax)-max(value)/15))
  
  return(DATA)
}

# *****************************
# Graphs functions and settings
# *****************************

w = 0.5

color.pattern=c("Residual" = "#FFFFFF", "Parties" = "#CC79A7", "Voter" = "#E69F00", "Priorities"="#0072B2", "Total" = "#000000")
alpha.pattern=c("Residual" = 0.0, "Parties" = 0.2, "Voter" = 0.8, "Priorities"=0.9, "Total" = 1)
labels.pattern=c("Residual" = "Residual", "Parties" = "Party\ncharacteristics", "Voter" = "Voter\nattributes", "Priorities"="Voter\npriorities", "Total" = "Total")



plotTheme = theme(panel.grid = element_blank(), legend.title = element_blank(), axis.title = element_blank(), legend.direction = "horizontal") 


# Need component to be a factor for the legend, need labelF for order of all components
createCommonPlot <- function(DT, baseSize = 10, textSize = 3, textFont = "plain", rectSize = GeomSegment$default_aes$size, 
                             rectColor = "black", basicPlot = FALSE, legendPosition = c(0.42, 0.95)) {
  
  fillValues = color.pattern
  fillLabels = labels.pattern
  
  if (length(unique(DT$component))==6) {
    fillValues = c("Existing Parties Res" = "#FFFFFF", "New Parties Res" = "Darkgreen", color.pattern[2:5])
    fillLabels = c("Existing Parties Res" = "Existing\nparties\nresidual", "New Parties Res" = "Party\nentry\nresidual", labels.pattern[2:5])
  }
  
  plot = ggplot(DT) +
    labs(y = "Share of Change", x = "Component") + 
    geom_rect(aes(xmin = as.integer(labelF) - w/2, xmax = as.integer(labelF) + w/2, # These are the rectangles
                  ymin = ymin, ymax = ymax,
                  fill = componentF, alpha=componentF), colour = rectColor, size = rectSize) +
    geom_segment(aes(x = xmin, xend = xmax, y = lineheight, yend = lineheight), colour = rectColor, size = rectSize) + # The lines between them
    scale_x_discrete(limits = ifelse(DT$component=="Total", DT$waveLabels, "")) +
    scale_fill_manual(values = fillValues, labels = fillLabels) +
    scale_alpha_manual(values = alpha.pattern, guide = 'none') +
    theme_classic(base_size = baseSize) + plotTheme +
    theme(legend.position = legendPosition) 
  
  # For country plot don't need the following
  if(basicPlot==FALSE) {
    plot = plot +
      scale_y_continuous(limits = c(-0.075, 1.1), breaks = c(0, 0.25, 0.5, 0.75, 1), expand = c(0,0), labels = scales::percent) +
      geom_text(aes(label=shortLabel, y=labelPositionY, x=as.integer(labelF)), nudge_x = -0.1, size = textSize, fontface = textFont) +
      guides(fill = guide_legend(nrow = 1))
  }
  
  if (length(unique(DT$component))==6) {  
    plot = plot +
      scale_y_continuous(limits = c(-0.25, 1.1), breaks = c(-0.25, 0, 0.25, 0.5, 0.75, 1), expand = c(0,0), labels = scales::percent) +
      geom_text(aes(label=shortLabel, y=labelPositionY, x=as.integer(labelF)), nudge_x = -0.1, size = textSize, fontface = textFont) +
      guides(fill = guide_legend(nrow = 1))
  }
  return(plot)
}
