clear
use "$POPULISM_DROPBOX/data/intdata/popuList/PopuList_clean.dta", replace

drop if missing(Y_partyfacts_id)
quietly bysort Y_partyfacts_id: gen dup = cond(_N==1,0,_n)
assert dup<=1
save "$POPULISM_DROPBOX/data/intdata/popuList/PopuList_temp.dta", replace

use "$POPULISM_DROPBOX/data/intdata/temp/ivs_with_manifesto_key.dta", clear

/**********************
******* Merge with PopuList *******
**********************/

merge m:1 Y_partyfacts_id using "$POPULISM_DROPBOX/data/intdata/popuList/PopuList_temp.dta", keep(match master) nogen

/**********************
******* Create Populism Variables *******
**********************/

/* PopuList covers all European countries besides Malta. Don't know if parties in other countries are populist*/
replace temp_populist = 0 if missing(temp_populist) & BB_countryInESM & BB_Country!="Malta"
replace temp_farright = 0 if missing(temp_farright) & BB_countryInESM & BB_Country!="Malta"
replace temp_farleft = 0 if missing(temp_farleft) & BB_countryInESM & BB_Country!="Malta"

bysort Y_Party_Vote: egen Y_Populist = max(temp_populist) if BB_countryInESM & BB_Country!="Malta"
replace Y_Populist = 0 if Y_Party_Vote == "." & BB_countryInESM & BB_Country!="Malta"

by Y_Party_Vote: egen Y_Far_Right = max(temp_farright) if BB_countryInESM & BB_Country!="Malta"
replace Y_Far_Right = 0 if Y_Party_Vote == "." & BB_countryInESM & BB_Country!="Malta"

by Y_Party_Vote: egen Y_Far_Left = max(temp_farleft) if BB_countryInESM & BB_Country!="Malta"
replace Y_Far_Left = 0 if Y_Party_Vote == "." & BB_countryInESM & BB_Country!="Malta"

by Y_Party_Vote: egen Y_EuroSkeptic = max(temp_eurosceptic) if BB_countryInESM & BB_Country!="Malta"
replace Y_EuroSkeptic = 0 if Y_Party_Vote == "." & BB_countryInESM & BB_Country!="Malta"

/* Alliance for the Future of Austria is farright until 2008 */
replace Y_Far_Right = 1 if Y_manifesto_party_id==42710 

drop temp_*

save "$POPULISM_DROPBOX/data/intdata/temp/ivs_with_populist.dta", replace
