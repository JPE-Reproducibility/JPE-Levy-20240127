source("renv/activate.R")

library(dplyr)
library(conflicted)


conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::lag)
conflicts_prefer(dplyr::summarize)
conflicts_prefer(dplyr::if_else)


source("code/util/config.R")
source("code/util/log.R")
DB.folder <- paste0(normalizePath(getwd(), winslash = "/"), "/")
github_path <- DB.folder
dropbox_path <- DB.folder
datasets_path <- file.path(DB.folder, "data/cleandata")
cleandata_path <- datasets_path
model_results_path <- file.path(DB.folder, "output", "modeldata", "results")
decomposition_path <- file.path(DB.folder, "output", "modeldata", "decomposition")
bootstrap_path <- file.path(DB.folder, "output", "modeldata", "bootstrap")

style_variant <- NULL
if (file.exists("code/styleGoogleConfig")) style_variant <- "google"
insult_variant <- if (is.null(style_variant)) "google" else style_variant
insult_scores_path <- file.path(cleandata_path, "manifesto", "insultscores",
                               paste0("insult_scores_", insult_variant, ".rds"))
manifesto_supply_variant_path <- file.path(cleandata_path, "manifesto",
                                         paste0("manifesto_supply_", insult_variant, ".Rds"))
style_suffix <- if (is.null(style_variant)) "" else paste0("_style_", style_variant)
if (file.exists("code/pledgeConfig")) style_suffix <- "_pledge"

#source("code/util/log.R")
logging()

pathWVS_Data = file.path(datasets_path, "WVS", "wvs_clean_4to6_waves.dta")
pathWVS_Data_no_Dem = file.path(datasets_path, "WVS", "wvs_clean_4to6_waves_no_dem.dta")
pathWVS_Data_with_CZSK = file.path(datasets_path, "WVS", "wvs_clean_4to6_waves_with_CZSK.dta")
pathWVS_Data_NoGRIT = file.path(datasets_path, "WVS", "wvs_clean_4to6_waves_NoGRIT.dta")
pathWVS_Data_EastSouth = file.path(datasets_path, "WVS", "wvs_clean_4to6_waves_eastsouth.dta")
pathWVS_Data_WestNorth = file.path(datasets_path, "WVS", "wvs_clean_4to6_waves_westnorth.dta")
# pathWVS_Data = file.path(datasets_path, "WVS", "wvs_clean_4to6_waves_18c.dta")

manifestoModelData = file.path(cleandata_path, "manifesto", paste0("manifesto_parties", style_suffix, ".rds"))
manifestoModelData_withCZSK = file.path(cleandata_path, "manifesto", paste0("manifesto_parties_withCZSK", style_suffix, ".rds"))
manifestoModelData1to6 = file.path(cleandata_path, "manifesto", "manifesto_parties_1to6.rds")
# manifestoDupData = paste0(cleandata_path, "/manifesto/manifesto_clean_dup_years_18c.dta")


### FOR MASTER FILE
#gen a dictionary for country abbreviation
country_dict = data.frame(BB_Country = c("Austria","Bulgaria","Croatia","Denmark","Estonia","Finland","France",
                                         "Germany",  "Hungary", "Iceland", "Italy", "Netherlands", "Norway", "Poland",
                                         "Portugal", "Spain","Sweden", "Switzerland", "UK","Greece","USA",
                                         "Czech Republic", "Lithuania", "Slovakia", "Slovenia" ),
                          BB_Country_abbrev = c("AUS","BGR", "HRV", "DNK", "EST", "FIN", "FRA", 
                                                "GER", "HUN", "ISL", "ITA", "NLD", "NOR", "POL", 
                                                "POR", "SPA", "SWE", "CHE", "UK", "GRC", "USA",
                                                "CZE", "LTU", "SVK", "SVN"))

country_east <- c("Estonia", "Hungary","Poland", "Slovenia", "Lithuania")
country_west <- c("Austria","Denmark", "Finland","France","Germany","Greece",
                  "Italy", "Netherlands", "Norway","Sweden", "Switzerland", "UK",
                  "Spain", "Portugal", "Iceland")
country_eastSouth <- c("Estonia", "Hungary","Poland", "Slovenia", "Lithuania", "Greece", "Italy", "Spain", "Portugal")
country_westNorth <- c("Austria","Denmark", "Finland","France","Germany","Iceland", "Netherlands", "Norway","Sweden", "Switzerland","UK")

# New categories that match CMP  
economyOpinions = c("X_op_income_equality_index", "X_op_GovtVsPersonalResp", "X_op_Competition_Markets",
                    "X_op_confidence_unions", "X_op_PrivateVsState")

# CMP econ but excluded from the above:
# X_op_redistribution_democratic, X_op_aid_unemploy

cultureOpinions = c("X_op_immigrant_jobs_index", "X_op_Pride_Nationality",
                    "X_op_neighbors_diff_race",
                    "X_op_neighbors_immigrants", "X_op_neighbors_homo", "X_op_preK_work_mom",
                    "X_op_confidence_press", "X_op_confidence_church",
                    "X_op_just_homosex", "X_op_just_abortion", "X_op_just_divorce", "X_op_just_suicide", "X_op_just_prostitution",
                    "X_op_men_jobs_priority", "X_op_confidence_army",
                    "X_op_army_rule", "X_op_democracy",
                    "X_op_Strong_Leader_Good","X_op_child_faith_import",
                    "X_op_neighbors_drinkers", "X_op_neighbors_drugs")

# CMP culture but excluded from the above:
# "X_op_trust_otherNat", "X_op_men_pol_leaders", "X_op_boy_uni", "X_op_men_business",
# "X_op_free_elections", "X_op_army_takeover", "X_op_civil_rights_protect", "X_op_women_dem_rights",
# "X_op_import_live_democracy", "X_op_just_euthanasia"




# Economic ,Social and Other Index -----------------------------------------------

positive_econ_vars <- c("per401", "per402", "per407", "per410", "per414", "per505", "per507", "per702")
negative_econ_vars <- c("per403", "per404", "per405", "per406", "per409", "per412", "per413", "per415", "per504", "per506", "per701")

positive_soc_vars <- c("per601", "per603", "per605", "per608", "per704")
negative_soc_vars <- c("per201", "per202", "per503", "per602", "per604", "per607")

positive_hawk_dove_vars <- c("per109")
negative_hawk_dove_vars <- c("per101", "per103", "per105", "per106", "per107", "per108")


positive_hawk_dove_expanded_vars <- c("per102", "per104", "per109", "per110")
negative_hawk_dove_expanded_vars <- c("per101", "per103", "per105", "per106", "per107", "per108")

negativity_vars <- c("per102", "per105", "per109", "per110", "per204", "per407", "per602", "per604", "per608", "per702")

positive_eu_vars <- "per110"
negative_eu_vars <- "per108"

gov_vars <- c("per303", "per304")

# ****************** OLD categories **********************************************
# Create 4 categories for all of our opnion variables
# economyOpinions = c("X_op_income_equality_index", "X_op_GovtVsPersonalResp", "X_op_Competition_Markets",
#                     "X_op_confidence_unions", "X_op_PrivateVsState")
# 
# cultureOpinions = c("X_op_respect_for_authority", "X_op_immigrant_jobs_index", "X_op_Strong_Leader_Good", "X_op_Pride_Nationality", 
#                     "X_op_neighbors_drinkers", "X_op_neighbors_drugs", "X_op_neighbors_diff_race", 
#                     "X_op_neighbors_immigrants", "X_op_neighbors_homo", "X_op_preK_work_mom",
#                     "X_op_confidence_press", "X_op_confidence_EU", "X_op_confidence_church", "X_op_confidence_army", 
#                     "X_op_just_homosex", "X_op_just_abortion", "X_op_just_divorce", "X_op_just_suicide",
#                     "X_op_men_jobs_priority", "X_op_just_prostitution",
#                     "X_op_child_work_import", "X_op_child_indep_import", "X_op_child_resp_import",
#                     "X_op_child_imag_import", "X_op_child_tol_import", "X_op_child_thrift_import",
#                     "X_op_child_persev_import", "X_op_child_faith_import", "X_op_child_unself_import",
#                     "X_op_child_obed_import") 

# Remove cabinet vars, moved to cleaning code
#cabinet.vars = paste0("X_op_",c("satis_polsystem","confidence_in_govt","country_democracy",
#                                "worries_job","worries_child","confidence_parliament"))

names_for_graph <- if (file.exists(file.path(cleandata_path, "metadata", "wvs_variables.rds")))
  readRDS(file.path(cleandata_path, "metadata", "wvs_variables.rds"))[2:3] else data.frame(name=character(), plotName=character())
names(names_for_graph)[1:2]= c("name", "plotName")


waveLabels = factor(c(1, 3, 4, 5, 6), levels = c(1,3,4,5,6), labels = c("1990-1992","1999-2000", "2005-2009", "2011-2013", "2017-2020"))
waveLabelsDF = data.frame(waveNum = c(1,3,4,5,6), waveLabels = c("1990-1992","1999-2000", "2005-2009", "2011-2013", "2017-2020"))

manifesto_labels <- if (file.exists(file.path(cleandata_path, "metadata", "manifesto_categories.rds")))
  readRDS(file.path(cleandata_path, "metadata", "manifesto_categories.rds")) else data.frame(variable_name=character(), title=character())
manifesto_labels <- as.data.frame(manifesto_labels) %>%
  dplyr::select(variable_name, title)

# Manifesto indicies
econIndexName = "Economic Index"
societyIndexName = "Cultural Index"
tradIndexName = "Traditional Morality Index"
societynotradIndexName = "Cultural Index without Traditional Morality"
hawkdoveIndexName = "Hawk-Dove Index"
euIndexName = "European Integration Index"
negIndexName = "Negativity Index"
hawkdoveexpandedIndexName = "Expanded Hawk-Dove Index"
prrpIndexName = "Populist Right Party Positions Index"
rileIndexName = "RILE Party Positions Index"
govIndexName = "Government Efficiency Index"

# *************************************************************************
# FIGURES ----
# *************************************************************************

ParFamColor <- c("#999999", "#5673e9","#E69F00", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#figureWidth = 22 - 2.5*2
figureWidth = 16.9
figureWidthSlide = 14.1
figureHeightSlide = 7

ggSaveSlide <- function(filename, plot=last_plot(), path=NULL, width=figureWidthSlide, height = figureHeightSlide, units="cm") {
  ggsave(filename=filename, plot=plot, path=path, width=width, height=height, units=units, dpi=600)
}


# *************************************************************************
# FUNCTIONS ----
# *************************************************************************

systemExit <- function(x) {
  returnValue <- system(x)
  if (returnValue!=0) {
    stop(paste0("Error " , returnValue, " ", x))
  }
} 

# *************************************************************************
# PRECISION ----
# *************************************************************************

# PRECISE_ESTIMATION is supplied by code/util/config.R.

if (PRECISE_ESTIMATION==-1) { # Values for quick tests
  n.values.lambda = 5  # With this value one country*wave < 5 min
  conv.crit.lambda = 20
  conv.crit2.lambda = 2
  lag.lambda = 10
  max.iter.lambda = 10000
  
  conv.crit.model = 100 # < 4 min
  conv.crit2.model = 0.1
  lag.model = 100
  max.iter.model = 50000
} else if (PRECISE_ESTIMATION==0) { # Imprecise
  n.values.lambda = 20
  conv.crit.lambda = 20
  conv.crit2.lambda = 2
  lag.lambda = 10
  max.iter.lambda = 10000
  
  conv.crit.model = 20
  conv.crit2.model = 0.1
  lag.model = 100
  max.iter.model = 50000
} else if (PRECISE_ESTIMATION==0.5) { # Default
  n.values.lambda = 50
  conv.crit.lambda = 0.05
  conv.crit2.lambda = 0.1
  lag.lambda = 5
  max.iter.lambda = 10000
  
  conv.crit.model = 1
  conv.crit2.model = 0.01
  lag.model = 100
  max.iter.model = 50000
}

