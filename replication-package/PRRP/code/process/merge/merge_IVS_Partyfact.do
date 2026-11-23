/* This file merges the WVS data with partyfact based on data in WVS and on manual data
After running this file almost every voters who support a party should have a partyfact_id*/


use "$POPULISM_DROPBOX/data/intdata/temp/ivs_politics_clean.dta", replace

/**********************************
******* Define WVS_PartyFact_ID********************
**********************************/

/* Save correct wvs partyfact_id*/
gen WVS_PartyFact_ID_temp = E179WVS 
gen WVS_2nd_PartyFact_ID_temp = E180WVS 
gen WVS_Least_PartyFact_ID_temp = E182

/* Below 10 not real parties, setting missing parties to 0 for max operations later*/
replace WVS_PartyFact_ID_temp = 0 if E179WVS<10 | missing(E179WVS)
replace WVS_2nd_PartyFact_ID_temp = 0 if E180WVS<10 | missing(E180WVS)
replace WVS_Least_PartyFact_ID_temp = 0 if E182<10 | missing(E182)

gen use_Party_for_agg = Y_Party_Vote_Long if Country_Cleaned == 1 
replace use_Party_for_agg = BB_Party if Country_Cleaned != 1
egen WVS_PartyFact_ID = max(WVS_PartyFact_ID_temp) , by(use_Party_for_agg)

gen use_2nd_Party_for_agg = Y_2nd_Party_Vote_Long if Country_Cleaned == 1 
replace use_2nd_Party_for_agg = BB_2nd_Party if Country_Cleaned != 1
egen WVS_2nd_PartyFact_ID = max(WVS_2nd_PartyFact_ID_temp) , by(use_2nd_Party_for_agg)

gen use_Least_Party_for_agg = Y_Least_Party_Vote_Long if Country_Cleaned == 1 
replace use_Least_Party_for_agg = BB_Least_Party if Country_Cleaned != 1
egen WVS_Least_PartyFact_ID = max(WVS_Least_PartyFact_ID_temp) , by(use_Least_Party_for_agg)


/* Add partyfact_external_id based on wvs_partyfact_id*/
merge m:1 WVS_PartyFact_ID using "$POPULISM_DROPBOX/data/intdata/partyfacts/ivs_partyfact_auto.dta", keep(master match) keepusing(partyfacts_id) gen(matched_IVS_PartyFact)
rename partyfacts_id partyfacts_id_auto
merge m:1 WVS_2nd_PartyFact_ID using "$POPULISM_DROPBOX/data/intdata/partyfacts/ivs_2nd_partyfact_auto.dta", keep(master match) keepusing(partyfacts_2nd_id) gen(matched_2nd_IVS_PartyFact)
rename partyfacts_2nd_id partyfacts_2nd_id_auto
merge m:1 WVS_Least_PartyFact_ID using "$POPULISM_DROPBOX/data/intdata/partyfacts/ivs_Least_partyfact_auto.dta", keep(master match) keepusing(partyfacts_Least_id) gen(matched_Least_IVS_PartyFact)
rename partyfacts_Least_id partyfacts_Least_id_auto

save "$POPULISM_DROPBOX/data/intdata/temp/ivs_temp_with_partyfact_key.dta", replace


/******************************************
********* PARTIES THAT ARE NOT MATCHED
********************************************/

keep if missing(partyfacts_id_auto)
quietly bysort Y_Party_Vote_Long: gen dup = cond(_N==1,0,_n)

drop if dup > 1
drop if Y_Party_Vote_Long == "."

gen haveWVSKey = WVS_PartyFact_ID!=0 & matched_IVS_PartyFact==1

keep Y_Party_Vote_Long haveWVSKey

/* Create table to manually match the unmatched parties */

/* Add partyfact_external_id based on manual list we created */
use "$POPULISM_DROPBOX/data/intdata/temp/ivs_temp_with_partyfact_key.dta", clear

merge m:1 Y_Party_Vote_Long using "$POPULISM_DROPBOX/data/intdata/partyfacts/ivs_partyfact_manual.dta", keep(master match) keepusing(partyfacts_id_manual) nogenerate
merge m:1 Y_2nd_Party_Vote_Long using "$POPULISM_DROPBOX/data/intdata/partyfacts/ivs_2nd_partyfact_manual.dta", keep(master match) keepusing(partyfacts_2nd_id_manual) nogenerate
merge m:1 Y_Least_Party_Vote_Long using "$POPULISM_DROPBOX/data/intdata/partyfacts/ivs_Least_partyfact_manual.dta", keep(master match) keepusing(partyfacts_Least_id_manual) nogenerate

/* Use both datasets to determine partyfact_external_id*/
gen Y_partyfacts_id = partyfacts_id_auto if !missing(partyfacts_id_auto)
replace Y_partyfacts_id = partyfacts_id_manual if missing(partyfacts_id_auto) & !missing(partyfacts_id_manual)

gen Y_2nd_partyfacts_id = partyfacts_2nd_id_auto if !missing(partyfacts_2nd_id_auto)
replace Y_2nd_partyfacts_id = partyfacts_2nd_id_manual if missing(partyfacts_2nd_id_auto) & !missing(partyfacts_2nd_id_manual)

gen Y_Least_partyfacts_id = partyfacts_Least_id_auto if !missing(partyfacts_Least_id_auto)
replace Y_Least_partyfacts_id = partyfacts_Least_id_manual if missing(partyfacts_Least_id_auto) & !missing(partyfacts_Least_id_manual)

save "$POPULISM_DROPBOX/data/intdata/temp/ivs_clean_with_partyfact.dta", replace


tab Y_Party_Vote_Long if missing(Y_partyfacts_id)
tab Y_2nd_Party_Vote_Long if missing(Y_2nd_partyfacts_id)
tab Y_Least_Party_Vote_Long if missing(Y_Least_partyfacts_id)

/* Note some parties still missing. 
  For example Greece course of freedom does not appear in partyfacts (only written once but with popular unity and its id, and that's a different party)*/

  
  
/******************************************
********* TEST HOW MANY OBSERVATIONS MERGE
********************************************/

gen havePartyFact = !missing(Y_partyfacts_id)
tab BB_Country havePartyFact if ! Y_No_Party_Vote
