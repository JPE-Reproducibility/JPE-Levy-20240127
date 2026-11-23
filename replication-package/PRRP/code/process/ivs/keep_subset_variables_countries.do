/* Keep only clean variables and countries*/ 

set rmsg off
use "$POPULISM_DROPBOX/data/intdata/ivs/full_clean_ivs.dta", replace

gen Y_FR_Pop = Y_Far_Right & Y_Populist if !missing(Y_Far_Right, Y_Populist)
replace Y_FR_Pop = . if missing(Y_manifesto_key) | Y_No_Party_Vote
replace Y_Far_Right = . if missing(Y_manifesto_key) | Y_No_Party_Vote



tab BB_Country if Country_Cleaned
tab BB_Country BB_Int_Wave if BB_countryInManifesto & !Country_Cleaned
tab BB_Country BB_Int_Wave if BB_countryInManifesto & !Country_Cleaned & BB_countryAdvancedDem & BB_countryInESM
tab BB_Country BB_Int_Wave if BB_countryInManifesto & !Country_Cleaned & BB_countryInESM


/* Keep only specific countries */
keep if Country_Cleaned

// Drop countries with no wave 6
egen haveWave6 = max(BB_Wave_num==6), by(BB_Country)
drop if haveWave6==0

// Dropping countries where can't match many respondents with closest eleciton
drop if inlist(BB_Country, "Bulgaria", "Croatia", "Romania")


// Drop  Poland Wave 2 & 1, Switzerland 1, missing many value
drop if (BB_Country=="Switzerland" & BB_Wave_num==1) 
drop if (BB_Country=="Poland" & BB_Wave_num==1) | (BB_Country=="Poland" & BB_Wave_num==2)

// dropping year of birth, too similar to wave within age
drop X_dem_year_birth



/* Dropping variables that are always missing*/
foreach var of varlist X_* {
	quiet sum `var'
	if `r(N)'==0 {
		display "var `var' is always missing, dropping"
		drop `var'
	}
}



/* Replace variable that doesn't exist for at least 5 countries with missing, takes a while*/
foreach var of varlist X_* {
	if("`var'"!="X_op_income_equality_index") {
		quiet: gunique BB_Country if !missing(`var'), by(BB_Wave_num) gen(uniqueCountryWaveNonMissing)
		quiet: gunique BB_Country, by(BB_Wave_num)  gen(uniqueCountryWave)	
		quiet: gen replaceValue = uniqueCountryWave - uniqueCountryWaveNonMissing >= 5
		quiet: replace `var' = . if replaceValue==1 
		quiet: sum replaceValue 
		if (`r(max)' >0) {
			display "replacing with missing `var'"
			glevelsof BB_Int_Wave if replaceValue
		}
		drop uniqueCountryWave uniqueCountryWaveNonMissing replaceValue 
	}
}


/* Drop variables still missing more than twice*/
replace X_op_pol_interest = . if BB_Wave_num==3
replace X_op_just_euthanasia = . if BB_Wave_num==5



/* Drop variables that don't exist in first wave*/
foreach var of varlist X_* {
	quiet: count if BB_Wave_num==4 & !missing(`var')
	if (`r(N)'==0) {
		display "dropping `var' missing in wave 4"
		drop `var'
	}
}


/* Drop variables that don't exist in last wave*/
foreach var of varlist X_* {
	quiet: count if BB_Wave_num==6 & !missing(`var')
	if (`r(N)'==0) {
		display "dropping `var' missing in wave 6"
		drop `var'
	}
}

// dropping cabinet vars
drop  X_op_confidence_parliament X_op_confidence_polparties X_op_confidence_in_govt


/* These two variables exist almost exclusively in wave 6*/
//drop X_dem_num_household X_op_immigrant_impact


/* Keep only relevant variables*/
keep X_* BB_* Y_* Unimp_* // P_*


global checkVariationOn 1


/* Program to drop and report variables with no variation*/
capture program drop checkVariation
program define checkVariation
	if ($checkVariationOn) {
		quietly: gunique `1'
		//assert r(ndistinct)>0
		if (r(unique)==1) {
			drop `1'
			display "dropping `1'"
		}
	}
end

/* Dropping all X_ variables with no variation after removing countries*/
foreach var of varlist X_* {
	checkVariation `var'
}

/* Dropping one party using the same manifesto twice, keepping the result in wave 4 only*/
drop if Y_manifesto_key == 1634

save "$POPULISM_DROPBOX/data/intdata/temp/ivs_selected_countries_with_manifesto.dta", replace

// Save the same pre-missing-manifesto dataset for downstream R analysis.
save "$POPULISM_DROPBOX/data/cleandata/ivs/ivs_selected_countries_with_manifesto.dta", replace

/* Dropping missing manifesto*/
drop if missing(Y_manifesto_key)

save "$POPULISM_DROPBOX/data/intdata/temp/ivs_selected_countries.dta", replace
