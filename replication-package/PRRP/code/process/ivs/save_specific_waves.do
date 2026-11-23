/**********************************************************
*************** KEEP SUBSET SPECIFIC WAVES ****************
***********************************************************/

use "$POPULISM_DROPBOX/data/intdata/ivs/ivs_selected_countries_imputed.dta", clear
 
capture gen one=1

/**********************************************************
*************** WEIGHTS ****************
***********************************************************/
capture program drop create_weights 
program define create_weights 
	/* WEIGHTS */
	replace BB_Weight_CW = .


	/* normalize to equal size in every wave (separating EVS and WVS) */
	egen waveWeight = sum(BB_Weight), by(BB_Wave BB_Country BB_Wave_num)
	egen countryWeight = sum(BB_Weight), by(BB_Country BB_Wave_num)
	
	    /* Determine the wave range to use within the for loop */
    summarize BB_Wave_num
    local minWave = r(min)
    local maxWave = r(max)

	forvalues wave_num=`minWave'/`maxWave' {
	    // Calculate the number of distinct combinations of BB_Wave and BB_Country for the current wave number
		distinct BB_Wave BB_Country if BB_Wave_num==`wave_num', joint 
		local uniqueWave = `r(ndistinct)'
		local totalObs = `r(N)'
		display "`uniqueWave' `totalObs'"
		
		// Normalize the sample weight by wave and country for the current wave number
		replace BB_Weight_CW = BB_Sample_Weight / waveWeight * `totalObs' / `uniqueWave' if BB_Wave_num==`wave_num'
		
		// Recalculate the country weight for the current wave number
		distinct BB_Country if BB_Wave_num==`wave_num', joint 
		local uniqueCountry =`r(ndistinct)'
		drop countryWeight 
		egen countryWeight = sum(BB_Weight_CW), by(BB_Country BB_Wave_num)
		replace BB_Weight_CW = BB_Weight_CW / countryWeight * `totalObs' / `uniqueCountry' if BB_Wave_num==`wave_num'
	}

	//list BB_Weight BB_Weight_CW if BB_Id=="6_3000720389" | BB_Id=="4_2760521680"
	drop waveWeight countryWeight
end 

*************** WAVES 1-6 ****************

/* If don't have waves 4-6, keep but replace with zero*/
foreach var of varlist X_* {
    
	// Generate a unique count of non-missing values for each wave
	quiet: gunique BB_Wave_num if !missing(`var'), by(one) gen(uniqueWavesNonMissing)
	
	// Generate a unique count of non-missing values for waves 4 to 6
	quiet: gunique BB_Wave_num if !missing(`var') & 4 <= BB_Wave_num & BB_Wave_num <= 6 ///
  , by(one) gen(uniqueWavesNonMissing4to6)
  
  // Create a flag to identify variables that need to be replaced
	quiet: gen replaceValue = uniqueWavesNonMissing4to6==3 & uniqueWavesNonMissing<6 
	quiet: sum replaceValue 
	if (`r(max) >0') {
		display "replacing `var'"
		foreach wave of numlist 1 2 3 {
			count if !missing(`var') & BB_Wave_num==`wave'
			if (`r(N)'==0) {
				display "replacing `var' wave `wave'"
				replace `var' = 999 if BB_Wave_num==`wave'
			}
		}
	}
	// Create a flag to identify variables that need to be deleted
	quiet: gen deleteValue = uniqueWavesNonMissing4to6 < 3 
	quiet: sum deleteValue 
	if (`r(max) >0') {
		display "deleting `var'"
		drop `var'
	}
	drop replaceValue deleteValue uniqueWavesNonMissing uniqueWavesNonMissing4to6
}


/* Check for missing values, skipping wave 2 since we're not using it and didn't impute missing values in impute_missing_survey_waves.do*/
foreach var of varlist X_* {
	display "`var' `wave'"
	assert !missing(`var') if BB_Wave_num != 2
}

//Check missing variable*wave by country, skipping wave 2 for same reason as above
gunique BB_Country if BB_Wave_num!=2, by(BB_Int_Wave) gen(maxCountryWave)
foreach var of varlist X_* {
	quiet: gunique BB_Country if !missing(`var') & BB_Wave_num!=2, by(BB_Int_Wave) gen(uniqueCountryWave)
	quiet: egen nonMissingWave = sum(!missing(`var')) if BB_Wave_num!=2, by(BB_Int_Wave)
	quiet: gen missingIssue = uniqueCountryWave<maxCountryWave & 0<nonMissingWave
	quiet: sum missingIssue 
	display "checking `var'"
	assert `r(max)'==0
	drop missingIssue nonMissingWave uniqueCountryWave
}

/**********************************************************
*************** DROP OBS ****************
***********************************************************/

/**** DROPPING FEWER THAN 10 VOTES ***/
egen countryWave = group(BB_Country BB_Wave_num)
egen countyPartyCountryWave = count(BB_Country ), by(Y_Party_Vote BB_Country BB_Wave_num)
tab Y_Party_Vote countyPartyCountryWave  if BB_Wave_num==6 & BB_Country=="Spain"
drop if countyPartyCountryWave < 10

gen BB_Sample_Weight = BB_Weight
gen BB_Weight_CW = .

//save temporary data	
save "$POPULISM_DROPBOX/data/intdata/temp/wvs_clean_all_waves.dta", replace

// drop waves 1-3 and save datasets for waves 4 to 6
drop if BB_Wave_num < 4

/**********************************************************
*************** SAVE ****************
***********************************************************/

/* Save data with US */

create_weights
sort BB_Id
save "$POPULISM_DROPBOX/data/intdata/WVS/wvs_clean_4to6_waves_with_CZSK_with_US.dta", replace

drop if inlist(BB_Country, "Czech Republic", "Slovakia")	
create_weights
sort BB_Id
save "$POPULISM_DROPBOX/data/intdata/WVS/wvs_clean_4to6_waves_with_US.dta", replace

drop if inlist(BB_Country, "Greece", "Italy")
create_weights
sort BB_Id
save "$POPULISM_DROPBOX/data/intdata/WVS/wvs_clean_4to6_waves_NoGRIT_with_US.dta", replace

/* Save data with US and without countries with no populism (not sure this is needed but currently used) */

use "$POPULISM_DROPBOX/data/intdata/WVS/wvs_clean_4to6_waves_with_US.dta"

egen havePop = max(Y_Far_Right), by(BB_Country)
drop if havePop==0

create_weights
sort BB_Id


/* Save main data */

use "$POPULISM_DROPBOX/data/intdata/WVS/wvs_clean_4to6_waves_with_CZSK_with_US.dta", replace

drop if BB_Country=="US"

create_weights
sort BB_Id

save "$POPULISM_DROPBOX/data/cleandata/WVS/wvs_clean_4to6_waves_with_CZSK.dta", replace

use "$POPULISM_DROPBOX/data/intdata/WVS/wvs_clean_4to6_waves_with_US.dta"

drop if BB_Country=="US"

create_weights
sort BB_Id

save "$POPULISM_DROPBOX/data/cleandata/WVS/wvs_clean_4to6_waves.dta", replace

egen havePop = max(Y_FR_Pop), by(BB_Country)
drop if havePop==0

/* Save main data without countries with no populism */
create_weights
sort BB_Id

save "$POPULISM_DROPBOX/data/intdata/WVS/wvs_clean_4to6_waves_18c.dta", replace

use "$POPULISM_DROPBOX/data/intdata/WVS/wvs_clean_4to6_waves_NoGRIT_with_US.dta", replace

drop if BB_Country=="US"

create_weights
sort BB_Id

save "$POPULISM_DROPBOX/data/cleandata/WVS/wvs_clean_4to6_waves_NoGRIT.dta", replace

/* Save no demographics*/
use "$POPULISM_DROPBOX/data/intdata/WVS/wvs_clean_4to6_waves_with_US.dta", replace

drop X_dem_*
create_weights
sort BB_Id


drop if BB_Country=="US"
create_weights
sort BB_Id

save "$POPULISM_DROPBOX/data/cleandata/WVS/wvs_clean_4to6_waves_no_dem.dta", replace

use "$POPULISM_DROPBOX/data/intdata/WVS/wvs_clean_4to6_waves_with_US.dta", replace
keep if inlist(BB_Country,"Bulgaria","Czech Republic", "Croatia", "Estonia") | ///
	inlist(BB_Country,"Hungary","Lithuania","Poland") | ///
	inlist(BB_Country,"Romania","Slovakia","Slovenia") | ///
	inlist(BB_Country, "Italy","Spain","Portugal","Greece")
create_weights
sort BB_Id

save "$POPULISM_DROPBOX/data/cleandata/WVS/wvs_clean_4to6_waves_eastsouth.dta", replace

use "$POPULISM_DROPBOX/data/intdata/WVS/wvs_clean_4to6_waves_with_US.dta", replace
drop if inlist(BB_Country,"Bulgaria","Czech Republic", "Croatia", "Estonia") | ///
	inlist(BB_Country,"Hungary","Lithuania","Poland") | ///
	inlist(BB_Country,"Romania","Slovakia","Slovenia") | ///
	inlist(BB_Country, "Italy","Spain","Portugal","Greece","US")
create_weights
sort BB_Id

save "$POPULISM_DROPBOX/data/cleandata/WVS/wvs_clean_4to6_waves_westnorth.dta", replace
	

/******************************************
*************** WAVES 1-6 *****************
******************************************/

// Reload the temporary data before saving waves 4 to 6:
use "$POPULISM_DROPBOX/data/intdata/temp/wvs_clean_all_waves.dta", replace

/**********************************************************
*************** DROP VARIABLES ****************
***********************************************************/

/* Waves 1,3: Drop variables missing in most countries: X_dem_education, X_op_army_rule,
X_op_confidence_environ, X_op_confidence_UN, X_op_democracy, X_op_experts_rule,
X_op_NoWorkLazy, X_op_Strong_Leader_Good, X_op_work_duty, X_op_work_first, X_dem_state_health,
X_op_confidence_big_company, X_op_just_avoid_fare, X_op_just_prostitution, X_op_pol_interest, 
X_op_PrivateVsState*/

drop X_dem_education
drop X_op_army_rule
drop X_op_confidence_environ
drop X_op_confidence_UN
drop X_op_democracy
drop X_op_experts_rule
drop X_op_Strong_Leader_Good
drop X_dem_state_health
drop X_op_confidence_big_company
drop X_op_just_avoid_fare
drop X_op_just_prostitution
drop X_op_pol_interest
drop X_op_PrivateVsState

	
/**********************************************************
*************** SAVE ****************
***********************************************************/

/* Save main data */
drop if BB_Country=="US"
// Drop wave 2 - not in use
drop if BB_Wave_num==2 
	// Drop wave 2 - not in use
	//drop if BB_Wave_num==2 
	
	/* Waves 1,3: Drop Estonia, Lithuania, Switzerland, Greece, Poland, Norway:
	missing too many variables */
drop if (BB_Country=="Estonia" & BB_Wave_num==1) | (BB_Country=="Estonia" & BB_Wave_num==3)
drop if (BB_Country=="Lithuania" & BB_Wave_num==1) | (BB_Country=="Lithuania" & BB_Wave_num==3)
drop if (BB_Country=="Switzerland" & BB_Wave_num==1) | (BB_Country=="Switzerland" & BB_Wave_num==3)
drop if (BB_Country=="Poland" & BB_Wave_num==1) | (BB_Country=="Poland" & BB_Wave_num==3)
drop if (BB_Country=="Norway" & BB_Wave_num==1) | (BB_Country=="Norway" & BB_Wave_num==3)
	
create_weights
sort BB_Id
save "$POPULISM_DROPBOX/data/cleandata/WVS/wvs_clean_1to6_waves.dta", replace
