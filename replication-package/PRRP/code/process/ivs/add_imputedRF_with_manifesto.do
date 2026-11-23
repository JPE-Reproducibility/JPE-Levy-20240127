use "$POPULISM_DROPBOX/data/intdata/temp/for_impute_ivs_with_manifesto.dta", replace
gen missingManifeso = missing(Y_manifesto_key)
tab missingManifeso

//unab allX : X_*
//unab unimp : *_unimp
//local removeX : list allX - unimp
//display "`removeX'"
//drop `removeX'

drop X_*

merge 1:1 id using "$POPULISM_DROPBOX/data/intdata/temp/after_rf_impute_ivs_with_manifesto.dta"

capture gen one=1

drop if BB_Wave_num < 4
/* If don't have waves 4-6, keep but replace with zero*/
foreach var of varlist X_* {
	quiet: gunique BB_Wave_num if !missing(`var'), by(one) gen(uniqueWavesNonMissing)
	quiet: gunique BB_Wave_num if !missing(`var') & 4 <= BB_Wave_num & BB_Wave_num <= 6 ///
  , by(one) gen(uniqueWavesNonMissing4to6)
	quiet: gen replaceValue = uniqueWavesNonMissing4to6==3 & uniqueWavesNonMissing<6 
	quiet: sum replaceValue 
	if (`r(max) >0') {
		display "replacing `var'"
		foreach wave of numlist 1 2 {
			count if !missing(`var') & BB_Wave_num==`wave'
			if (`r(N)'==0) {
				display "replacing `var' wave `wave'"
				replace `var' = 999 if BB_Wave_num==`wave'
			}
		}
	}
	quiet: gen deleteValue = uniqueWavesNonMissing4to6 < 3 
	quiet: sum deleteValue 
	if (`r(max) >0') {
		display "deleting `var'"
		drop `var'
	}
	drop replaceValue deleteValue uniqueWavesNonMissing uniqueWavesNonMissing4to6
}


foreach var of varlist X_* {
	display "`var' `wave'"
	assert !missing(`var') 
}

* Add weights
gen BB_Sample_Weight = BB_Weight
gen BB_Weight_CW = .

capture program drop create_weights 
program define create_weights 
	/* WEIGHTS */
	replace BB_Weight_CW = .


	/* normalize to equal size in every wave (separating EVS and WVS) */
	egen waveWeight = sum(BB_Weight), by(BB_Wave BB_Country BB_Wave_num)
	egen countryWeight = sum(BB_Weight), by(BB_Country BB_Wave_num)

	forvalues wave_num=4/6 {
		distinct BB_Wave BB_Country if BB_Wave_num==`wave_num', joint 
		local uniqueWave = `r(ndistinct)'
		local totalObs = `r(N)'
		display "`uniqueWave' `totalObs'"
		replace BB_Weight_CW = BB_Sample_Weight / waveWeight * `totalObs' / `uniqueWave' if BB_Wave_num==`wave_num'
		
		distinct BB_Country if BB_Wave_num==`wave_num', joint 
		local uniqueCountry =`r(ndistinct)'
		drop countryWeight 
		egen countryWeight = sum(BB_Weight_CW), by(BB_Country BB_Wave_num)
		replace BB_Weight_CW = BB_Weight_CW / countryWeight * `totalObs' / `uniqueCountry' if BB_Wave_num==`wave_num'
	}

	//list BB_Weight BB_Weight_CW if BB_Id=="6_3000720389" | BB_Id=="4_2760521680"
	drop waveWeight countryWeight
end 

create_weights

save "$POPULISM_DROPBOX/data/cleandata/ivs/ivs_selected_countries_imputed_with_manifesto.dta", replace

use "$POPULISM_DROPBOX/data/intdata/temp/for_impute_ivs_with_manifesto.dta", replace

capture gen one=1

drop if BB_Wave_num < 4
/* If don't have waves 4-6, keep but replace with zero*/
foreach var of varlist X_* {
	quiet: gunique BB_Wave_num if !missing(`var'), by(one) gen(uniqueWavesNonMissing)
	quiet: gunique BB_Wave_num if !missing(`var') & 4 <= BB_Wave_num & BB_Wave_num <= 6 ///
  , by(one) gen(uniqueWavesNonMissing4to6)
	quiet: gen replaceValue = uniqueWavesNonMissing4to6==3 & uniqueWavesNonMissing<6 
	quiet: sum replaceValue 
	if (`r(max) >0') {
		display "replacing `var'"
		foreach wave of numlist 1 2 {
			count if !missing(`var') & BB_Wave_num==`wave'
			if (`r(N)'==0) {
				display "replacing `var' wave `wave'"
				replace `var' = 999 if BB_Wave_num==`wave'
			}
		}
	}
	quiet: gen deleteValue = uniqueWavesNonMissing4to6 < 3 
	quiet: sum deleteValue 
	if (`r(max) >0') {
		display "deleting `var'"
		drop `var'
	}
	drop replaceValue deleteValue uniqueWavesNonMissing uniqueWavesNonMissing4to6
}
