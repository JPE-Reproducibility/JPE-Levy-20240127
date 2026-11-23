#script for leave-country-out exercise 


#find phi and delta for when each country is left out
source("code/model/run_loo.R")

#second stage LCO -
source("code/model/lco_second_stage.R")

#decompositions
source("code/model/decomposition_lco.R")