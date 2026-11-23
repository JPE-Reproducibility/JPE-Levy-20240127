// Master do-file for data preparation, test 


cd "$POPULISM_ROOT"

clear all

capture log close
log using "$REPLICATION_LOGS/prepare_data_master_2.txt", text replace 

include "code/process/ivs/add_imputedRF.do"
include "code/process/ivs/add_imputedRF_with_manifesto.do"
include "code/process/ivs/save_specific_waves.do"

clear all
set more off

log close