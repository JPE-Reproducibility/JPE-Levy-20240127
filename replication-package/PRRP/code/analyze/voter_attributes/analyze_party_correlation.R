# *************************************************************************
# File description ----
# *************************************************************************
#
# This file analyzes the correlation between party voters across countries
# to determine whether populist party voters share similar characteristics
# internationally.
#
# The main goal is to examine voter similarity patterns by:
#   (1) Predicting party vote probabilities using voter attributes
#   (2) Calculating correlations between predicted probabilities
#   (3) Computing average correlations within and across party families
#   (4) Visualizing correlation patterns in a heat map
#
# The analysis focuses on Wave 6 data and includes:
#   - PRRP (Populist Radical Right Parties)
#   - Other Populist parties
#   - Traditional party families (Center-Right, Center-Left, Socialist, Green)
#
# Outputs:
#
# Figures:
#   - Correlation heat map showing voter similarities across parties (Figure A.16 + Slide)
#
# TeX Values:
#   - PRRP-to-PRRP average correlation coefficient
#
#
# *************************************************************************
# SETUP ----
# *************************************************************************
rm(list=ls())
library(haven)
library(dplyr)
library(ggplot2)
library(conflicted)
library(ggcorrplot)
library(savetexvalue)
library(forcats)

conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("arrange", "dplyr")
conflict_prefer("fct_relevel", "forcats")

# Load function
source("code/util/commonVars.R")
source("code/analyze/voter_attributes/corFunctions.R")
source("code/analyze/party_positions/manifesto_general_functions.R")

# Read data from dropbox:
WVS <- read_dta(file = pathWVS_Data)
manifesto = read_dta(file.path(cleandata_path, "manifesto", "manifesto.dta"))

# Add "parfam"
WVS = manifesto %>%
  select(Y_manifesto_key, parfam) %>%
  right_join(WVS, by = "Y_manifesto_key") 

# Use just wave 6 data
WAVE6 = WVS %>%
  filter(BB_Wave_num==6) %>%
  select(-starts_with("X_general")) %>% 
  filter(!Y_No_Party_Vote)


# Add short country names
WAVE6 = WAVE6 %>%
  left_join(country_dict, by = "BB_Country")

if(any(is.na(WAVE6$BB_Country_abbrev))){
  warning("some abbreviations are missing. update country_dict")
}



# *************************************************************************
# CLASSIFY TO FAMILIES AND SELECT PARTIES FOR GRAPH ----
# *************************************************************************
WAVE6 <- WAVE6 %>%  add_count(Y_Party_Vote, name="countVote") 
WAVE6 <- add_parfam(WAVE6)
WAVE6 <- WAVE6 %>% mutate(party_classification = 
                             case_when(
                               Y_FR_Pop == 1 ~ "PRRP",
                               Y_Populist == 1 ~ "Other Populist",
                               TRUE ~ as.character(BBF_parfam))) 



# Remove duplicates
sel_part_for_graph <- WAVE6 %>%
  filter(!is.na(party_classification)) %>%
  distinct(BB_Country, Y_Party_Vote, party_classification)



# *************************************************************************
# PREPARE DATA FOR PREDICTION ----
# *************************************************************************

# Generate a dummy variable for each party
party_dummies = WAVE6 %>%
  mutate(Y_Party_Vote = as.factor(Y_Party_Vote)) %>%
  select(Y_Party_Vote)

# Set up a matrix
party_dummies = model.matrix(~-1 +., party_dummies)
colnames(party_dummies) = gsub(x = colnames(party_dummies), pattern = "Y_Party_Vote", "")
party_dummies = as_tibble(party_dummies)
party_dummies = party_dummies %>%
  select_at(c(sel_part_for_graph$Y_Party_Vote))

colnames(party_dummies) = paste0("Y_Party_Vote_", colnames(party_dummies))
colnames(party_dummies) = gsub("-", "", colnames(party_dummies))

# A data frame with all the X_ variables
X_all <-  WAVE6 %>%
  select(starts_with("X_"))



# *************************************************************************
# PREDICT ----
# *************************************************************************

#this function returns X with ncol(Y) columns of probabilities.
#Each one calculated using a logit model that predicts voting for this party
#using the relevant data (voters from the relevant country)
predict_pop_vote = function(X,          #df with Xs 
                            Y,          #df or matrix with dummy for each pop party
                            country_id){ #country vector with length == nrow(X) == nrow(Y)
                            
  countries = unique(country_id)
  p_mat = data.frame(row_num = 1:nrow(X))
  
  #we'll save the coefficients of each model:
  betas = data.frame(variable = colnames(X))
  
  # For each country separately predict the prob of voting for each party
  for(country in countries){
    
    relevant_pop_part = Y %>%
      select(ends_with(country)) %>%
      colnames()
    
    for(party in relevant_pop_part){
      tmp = cbind(X, y = Y[,party])
      pred_mod = do.call("lm", args = list(formula = as.formula(paste0(party, " ~.")),
                                           data = tmp,
                                           subset = country_id == country))
      
      tmp = matrix(summary(pred_mod)$coefficients[,1], ncol = 1, dimnames = list(NULL, party))
      tmp = as.data.frame(tmp)
      tmp$variable = rownames(summary(pred_mod)$coefficients)
      
      
      #predict for the all sample
      p = predict(pred_mod, X, type = "response")
      p_mat = p_mat %>%
        mutate(!!paste0("p_voting_", party) := p) 
    }
  }
  return(p_mat)
}


predictedVote = predict_pop_vote(X = X_all, Y = party_dummies, country_id = WAVE6$BB_Country_abbrev)
X_with_pred = cbind(X_all, predictedVote)
rm(predictedVote)

# Correlation matrix
# Arguments:
# df for which we want to produce cor mat
# Country vector that define the domain
cor_with_limited_domain = function(X, country_id) 
{
  
  #prep output
  n = ncol(X)
  parties = colnames(X)
  output = matrix(nrow = n, ncol = n, dimnames = list(parties, parties))
  diag(output) = rep(NA, n)
  #gen combination of every two parties
  comb_mat = combn(parties, 2)
  for(i in 1:ncol(comb_mat)){
    comb = comb_mat[,i]
    #restrict the domain to include only the relevant countries
    rel_countries = substr(comb,start = nchar(comb) -2 , stop = nchar(comb) )
    rel_countries = gsub(x = rel_countries, pattern = "_", replacement = "") #for UK
    tmp = X %>% 
      filter(country_id %in% rel_countries) %>%
      select_at(comb)
    output[comb[1], comb[2]] = cor(tmp)[1, 2]
    output[comb[2], comb[1]] = cor(tmp)[1, 2]
  }
  return(output)
}

lim_domain_cor <- cor_with_limited_domain(X = X_with_pred %>% select(starts_with("p_vot")), 
                                          country_id = WAVE6$BB_Country_abbrev)

# Adjust the names of the columns and rows
colnames(lim_domain_cor) = substr(colnames(lim_domain_cor), 23, nchar(colnames(lim_domain_cor)))
rownames(lim_domain_cor) = substr(rownames(lim_domain_cor), 23, nchar(rownames(lim_domain_cor)))
assign(paste0(lim_domain_cor, "_", "all"), lim_domain_cor)



# A vector of countries
cor_countries <-  gsub(".*_", "", rownames(lim_domain_cor))



# *************************************************************************
# PLOT ----
# *************************************************************************


#remove parties we don't need:
parties = sel_part_for_graph$Y_Party_Vote
lim_domain_cor = lim_domain_cor[rownames(lim_domain_cor) %in% parties, colnames(lim_domain_cor) %in% parties]
n = ncol(lim_domain_cor)
sel_part_for_graph = sel_part_for_graph %>%
  filter(Y_Party_Vote %in% colnames(lim_domain_cor))

#order parties according to their classification and country
sel_part_for_graph = sel_part_for_graph %>%
  mutate(country = substr(Y_Party_Vote, 
                          start =  nchar(sel_part_for_graph$Y_Party_Vote) -2,
                          stop =  nchar(sel_part_for_graph$Y_Party_Vote)),
         country = ifelse(country == "_UK", "UK", country),
         #order party_classification as we want
         party_classification = fct_relevel(party_classification, 
                                                     "PRRP",
                                                     "Other Populist",
                                                     "Center-Right", 
                                                     "Center-Left",
                                                     "Socialist",
                                                     "Green")) %>%
  mutate(Y_Party_Vote = gsub(Y_Party_Vote, pattern = "_",replacement =  ", ")) %>% 
  arrange(party_classification, desc(country), desc(Y_Party_Vote))


# Replace "_" with comma in the party names
rowLabels <- rownames(lim_domain_cor)
colLabels <- colnames(lim_domain_cor)
rownames(lim_domain_cor) <- gsub(rowLabels, pattern = "_",replacement =  ", ")
colnames(lim_domain_cor) <- gsub(colLabels, pattern = "_",replacement =  ", ")

lim_domain_cor = lim_domain_cor[rev(sel_part_for_graph$Y_Party_Vote), sel_part_for_graph$Y_Party_Vote]

# Don't account for parties within the same country
col_country <- gsub("^[^,]+, ", "", colnames(lim_domain_cor))
row_country <- gsub("^[^,]+, ", "", rownames(lim_domain_cor))
for(i in 1:ncol(lim_domain_cor)){
  for(j in 1:nrow(lim_domain_cor)){
    if(col_country[i] == row_country[j]){
      lim_domain_cor[j, i] = NA
    }
  }
}


#calc average correlation in each cell
categories = unique(sel_part_for_graph$party_classification)
categories = as.character(categories)
cat_comb = t(combn(categories, 2))
colnames(cat_comb) = c("g1", "g2")
for(cat in categories){
  cat_comb = rbind(cat_comb, rep(cat,2))
}
cat_comb = data.frame(cat_comb)
cat_comb$avg_cor = NA
for(i in 1:nrow(cat_comb)){
  row_parties = sel_part_for_graph %>%
    filter(party_classification == cat_comb$g1[i]) %>%
    pull(Y_Party_Vote)
  col_parties = sel_part_for_graph %>%
    filter(party_classification == cat_comb$g2[i]) %>%
    pull(Y_Party_Vote)
  corrs = as.numeric(lim_domain_cor[row_parties, col_parties])
  corrs = corrs[corrs != 1] #we dont count the diagonal
  if (any(!is.na(corrs)))
    cat_comb$avg_cor[i] = mean(corrs, na.rm=TRUE)
  cat_comb$med_cor[i] = median(corrs, na.rm=TRUE)
  
}

# Nullify the upper triangular
for(i in 1:ncol(lim_domain_cor)){
  for(j in 1: nrow(lim_domain_cor)){
    if(i >= j){
      lim_domain_cor[n-j + 1,i] = 0
    }
  }
}

# Now, change the correlations between parties within the same country from NA to zero so they are plotted in gray in the plot
for(i in 1:ncol(lim_domain_cor)){
  for(j in 1: nrow(lim_domain_cor)){
    if(col_country[i] == rowLabels[j]){
      lim_domain_cor[i, j] = NA
    }
  }
}



cor_plot <- function(cormat, class_party, sumStat = "average", label_size ){
 
  # Calculate how many parties in each category (to know where to draw the lines)
  num_parties_in_cat = as.numeric(table(class_party$party_classification))
  
  # Party labels for the plot
  class_labels = c("PRRP", "Other\nPop.", "Center-Right", 
                   "Center-Left", "Socialist", "Green")
  num_cat = length(class_labels)
  # Position of the lines in the plot
  lines_position = cumsum(num_parties_in_cat)
  class_pos_x = 0.5 + (c(0, lines_position[-num_cat]) +  lines_position)/ 2
  class_pos_y = rep(n + 3.2, num_cat)
  # We don't need the last line
  lines_position = lines_position[-num_cat]
  
  #add positions to avg correlation (they should be presented on the upper diagonal)
  xpos = c(n + 0.5, n + 0.5 - lines_position, 0.5)
  xpos = (xpos[-1] + xpos[-length(xpos)]) / 2
  xpos = data.frame(x = xpos, x.name = levels(class_party$party_classification))
  ypos = c(0.5, 0.5 + lines_position, n + 0.5)
  ypos = (ypos[-1] + ypos[-length(ypos)]) / 2
  ypos = data.frame(y = ypos, y.name = levels(class_party$party_classification))
  position = merge(xpos,ypos)
  #take only combinations from the upper triangle
  position = position %>%
    filter(x > n + 0.5 - y)
  cat_comb = cat_comb %>%
    left_join(position, by = c("g1" = "x.name", "g2" = "y.name"))
  if (sumStat=="median") {
    cat_comb$sum_cor = cat_comb$med_cor
  } else if (sumStat=="average") {
    cat_comb$sum_cor = cat_comb$avg_cor
  }  else {
    stop("no summary stat")
  }
  cat_comb$sum_cor = round(cat_comb$sum_cor, 2)
  
  g = ggcorrplot(cormat, ggtheme = theme_gray, outline.color = "white", legend.title = "Correlation") +
    geom_hline(yintercept = 0.5 + lines_position) + 
    geom_vline(xintercept = n + 0.5 - lines_position) +
    annotate("text", x = n + 1 - rev(class_pos_x), y = class_pos_y+1,
             label = rev(class_labels), size = label_size) +
    annotate("text", x = class_pos_y + 1, y = class_pos_x, label = class_labels,
             size = label_size, angle = 270, vjust = "center") +
    annotate("text", x = cat_comb$x, y = cat_comb$y, label = as.character(cat_comb$sum_cor), size = label_size, angle = -90) +
    scale_y_discrete(expand = expansion(add = c(0, 8.5))) +
    scale_x_discrete(expand = expansion(add = c(0, 8.5))) +
    coord_fixed(ratio = 1, clip = 'off') 
  return (g)
}




# For the paper
theme = theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.text.x = element_text(size= 2.4, hjust = 0, vjust = 0.5 ,angle = -90),
              axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank(),
              axis.title = element_blank(),
              legend.position = "left",
              legend.title = element_text(angle = -90),
              legend.text = element_text(angle = -90),
              plot.margin=unit(c(0,2,0,2), units = "mm"))

# Paper version
corPlot = cor_plot(lim_domain_cor, sel_part_for_graph, sumStat = "average", label_size = 2.5)

ggsave(plot = corPlot + theme_gray(base_size = 8) + theme,
       "output/figures/WVS/voter_correlations/correlations_average_diff.pdf",
       width = figureWidth * 0.8, height = figureWidth * 0.8, units = "cm", dpi = 600)
