// Master do-file for data preparation, test 


cd "$POPULISM_ROOT"

clear all
capture log close

log using "$REPLICATION_LOGS/prepare_data_master_1.txt", text replace 


/******** COMMON CODE ************************/

/* Import data*/ 
display "Importing data $S_DATE $S_TIME"
include "code/import/import_manifesto.do"
include "code/import/import_partyfacts.do"
include "code/import/import_PopuList.do"
include "code/import/import_parlgov.do"

display "Integrating WVS+EVS $S_DATE $S_TIME"


/* Merge EVS WVS based on official IVS code*/
include "code/process/merge/EVS_WVS_Merge.do"


/* Process basic BB vars, outcome vars */
include "code/process/ivs/ivs_process.do"

/* Harmonize political parties across EVS/WVS, create the variables Y_Party_Vote, Y_Party_Vote_Long, and outcome variables for each party*/
include "code/process/ivs/clean_political_parties.do"

/* Add Party fact*/
include "code/process/merge/merge_IVS_Partyfact.do" 

/* Add manifesto (only adds keys and green party)*/
include "code/process/merge/merge_IVS_Manifesto.do" 

/* Add PopuList data*/
include "code/process/merge/merge_IVS_PopuList.do"

/* Add Parlgov data*/
include "code/process/merge/merge_IVS_Parlgov.do"

/* Add CHES data*/
// Unused CHES identifier omitted.

/* Clean dataset and keep subset of variables*/
display "Cleaning variables $S_DATE $S_TIME"

/* Clean dataset and keep subset of variables*/
include "code/process/ivs/clean_demographics.do"
include "code/process/ivs/clean_behavior.do"
include "code/process/ivs/clean_opinions.do"

include "code/process/ivs/keep_subset_variables_countries.do"
include "code/process/ivs/impute_missing_survey_waves.do"

/* Legitimacy*/
include "code/process/ivs/impute_missing_survey_waves_with_manifesto.do"

capture log close
