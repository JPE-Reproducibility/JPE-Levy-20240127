# Master file for all output for the paper

rm(list=ls())

source("renv/activate.R")
library(tidyverse)

#paths
source("code/util/commonVars.R")

# *******************
# tables ----
# *******************

source("code/analyze/voter_attributes/create_wvs_var_table.R")
source("code/analyze/voter_attributes/analyze_desc_stats_wvs.R")
source("code/analyze/party_positions/create_party_entry_table.R")

source("code/analyze/party_positions/create_manifesto_var_table.R")
source("code/analyze/party_positions/analyze_desc_stat_manifesto.R")

source("code/analyze/voter_attributes/IVS_data_desc_tables.R")

## Demographics Zs
source("code/analyze/party_positions/create_party_leader_var_table.R")

## Legitimacy
source("code/analyze/voter_attributes/analyze_shy_voters.R")

# ************************
# figures and values ----
# ************************

## Voter PRRP scores used by later analyses
source("code/analyze/voter_attributes/analyze_populist_voters.R")

## Manifesto descriptive figures and values
source("code/analyze/party_positions/analyze_prrp_rise.R")
source("code/analyze/party_positions/rrp_parties.R")
source("code/analyze/party_positions/save_party_numbers.R")
source("code/analyze/party_positions/check_manifesto_rhetoric_consistency_demean.R")
## Decomposition ----

#post bootstrap analysis
source("code/analyze/decomposition/plotDecomposition.R")

if (runAdvancedDecomposition) {
  source("code/analyze/decomposition/permutate order.R")
  source("code/analyze/priorities_and_model/create_inference_table.R")
  source("code/analyze/decomposition/DecompositionRobustnessGraph.R")
}

## Voters figures ----
source("code/analyze/voter_attributes/analyze_voter_opinion_trend.R")
source("code/analyze/voter_attributes/analyze_party_correlation.R")
source("code/analyze/voter_attributes/map_populist_support.R")

## Party Positions ----
source("code/analyze/party_positions/analyze_party_family_positions.R")
source("code/analyze/party_positions/femaleShareGraph.R")

## Analyze weights ----
source("code/analyze/priorities_and_model/plot_weight_static_parfam_bar.R")
source("code/analyze/priorities_and_model/plot_weight_dem_op_slide.R")
source("code/analyze/priorities_and_model/plot_phi_with_new_Zs.R")
source("code/analyze/priorities_and_model/prosser_weights.R")
source("code/analyze/priorities_and_model/hetero_priorities_prrp.R")

# Reduced form evdience for priorities
source("code/analyze/priorities_and_model/analyze_voter_parties_correlation.R")

## Counterfactual Maps ----
source("code/analyze/priorities_and_model/reserviors.R")

## Expand Waves
source("code/analyze/priorities_and_model/waves1to6_voter_parties_correlation.R")
source("code/analyze/priorities_and_model/waves1to6_heatmap.R")  

# Counterfactual support for new parties ----
source("code/analyze/decomposition/smallPartyResidual.R")

# Residual ----
source("code/analyze/priorities_and_model/newLeaderResidual.R")

# Germany case study in the paper appendix
source("code/analyze/case_studies/GG2.R")
