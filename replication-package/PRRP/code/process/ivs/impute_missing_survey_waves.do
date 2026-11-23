cd "$POPULISM_ROOT"
use "$POPULISM_DROPBOX/data/intdata/temp/ivs_selected_countries.dta", replace


/*********************************************
********** IMPUTE ****************************
*********************************************/
// Imputing manually for now since do not want this to be common
/* In cases we have a wave before and a wave after the imputation is done like that:
		1) calculate average value for the wave before and the wave after separately
		2) calculate the mean BB_year at the wave before, the wave after and the imputed wave
		3) assume a linear trend in time to predict value at the imputed wave
	In case we only have a wave before or a wave use their mean
*/

/*Germany -  X_dem_other_org_member - use waves 4,6*/
local germany_vars "X_dem_other_org_member X_op_just_cheat_tax"
local country "Germany"
local bef 4
local imp 5
local aft 6
foreach var of local germany_vars {
	sum `var' if BB_Wave_num== `bef' & BB_Country== "`country'"
	local mean_wave_bef = `r(mean)'
	sum `var' if BB_Wave_num== `aft' & BB_Country== "`country'"
	local mean_wave_aft = `r(mean)'
	sum BB_Year if BB_Wave_num==`bef' & BB_Country== "`country'"
	local year_wave_bef = `r(mean)'
	sum BB_Year if BB_Wave_num== `imp' & BB_Country== "`country'"
	local year_wave_imp = `r(mean)'
	sum BB_Year if BB_Wave_num== `aft' & BB_Country== "`country'"
	local year_wave_aft = `r(mean)'
	local impute_value = (`mean_wave_bef'*(`year_wave_aft' - `year_wave_imp') +  `mean_wave_aft'*(`year_wave_imp' - `year_wave_bef')) / (`year_wave_aft' - `year_wave_bef')
	display `impute_value'
	sum `var' if BB_Wave_num== `imp' & BB_Country== "`country'"
	assert `r(N)'==0
	replace `var' = `impute_value' if BB_Wave_num== `imp' & BB_Country== "`country'"
}

/* X_op_confidence_army missing in Iceland Wave 6, use Wave 4 since that is the closest we got*/
local var "X_op_confidence_army"
local country "Iceland"
local bef 4
local imp 6
sum `var' if BB_Wave_num== `bef' & BB_Country== "`country'"
local mean_wave_bef = `r(mean)'
local impute_value = `mean_wave_bef'
display `impute_value'
sum `var' if BB_Wave_num== `imp' & BB_Country== "`country'"
assert `r(N)'==0
replace `var' = `impute_value' if BB_Wave_num== `imp' & BB_Country== "`country'"

/* X_op_just_homosex missing in Italy Wave 4, use Wave 3  and 6 (don't have 5)*/
local var "X_op_just_homosex"
local country "Italy"
local bef 3
local imp 4
local aft 6
sum `var' if BB_Wave_num== `bef' & BB_Country== "`country'"
local mean_wave_bef = `r(mean)'
sum `var' if BB_Wave_num== `aft' & BB_Country== "`country'"
local mean_wave_aft = `r(mean)'
sum BB_Year if BB_Wave_num==`bef' & BB_Country== "`country'"
local year_wave_bef = `r(mean)'
sum BB_Year if BB_Wave_num== `imp' & BB_Country== "`country'"
local year_wave_imp = `r(mean)'
sum BB_Year if BB_Wave_num== `aft' & BB_Country== "`country'"
local year_wave_aft = `r(mean)'
local impute_value = (`mean_wave_bef'*(`year_wave_aft' - `year_wave_imp') +  `mean_wave_aft'*(`year_wave_imp' - `year_wave_bef')) / (`year_wave_aft' - `year_wave_bef')
display `impute_value'
sum `var' if BB_Wave_num== `imp' & BB_Country== "`country'"
assert `r(N)'==0
replace `var' = `impute_value' if BB_Wave_num== `imp' & BB_Country== "`country'"

/* X_op_just_prostitution missing in Spain Wave 5, use Wave 4  and 6 */
local var "X_op_just_prostitution"
local country "Spain"
local bef 4
local imp 5
local aft 6
sum `var' if BB_Wave_num== `bef' & BB_Country== "`country'"
local mean_wave_bef = `r(mean)'
sum `var' if BB_Wave_num== `aft' & BB_Country== "`country'"
local mean_wave_aft = `r(mean)'
sum BB_Year if BB_Wave_num==`bef' & BB_Country== "`country'"
local year_wave_bef = `r(mean)'
sum BB_Year if BB_Wave_num== `imp' & BB_Country== "`country'"
local year_wave_imp = `r(mean)'
sum BB_Year if BB_Wave_num== `aft' & BB_Country== "`country'"
local year_wave_aft = `r(mean)'
local impute_value = (`mean_wave_bef'*(`year_wave_aft' - `year_wave_imp') +  `mean_wave_aft'*(`year_wave_imp' - `year_wave_bef')) / (`year_wave_aft' - `year_wave_bef')
display `impute_value'
sum `var' if BB_Wave_num== `imp' & BB_Country== "`country'"
assert `r(N)'==0
replace `var' = `impute_value' if BB_Wave_num== `imp' & BB_Country== "`country'"

/* X_dem_log_town_size */

/* X_dem_log_town_size - missing wave 4 Iceland, use 3+6 that is the closest we got*/
local var "X_dem_log_town_size"
local country "Iceland"
local bef 3
local imp 4
local aft 6
sum `var' if BB_Wave_num== `bef' & BB_Country== "`country'"
local mean_wave_bef = `r(mean)'
sum `var' if BB_Wave_num== `aft' & BB_Country== "`country'"
local mean_wave_aft = `r(mean)'
sum BB_Year if BB_Wave_num==`bef' & BB_Country== "`country'"
local year_wave_bef = `r(mean)'
sum BB_Year if BB_Wave_num== `imp' & BB_Country== "`country'"
local year_wave_imp = `r(mean)'
sum BB_Year if BB_Wave_num== `aft' & BB_Country== "`country'"
local year_wave_aft = `r(mean)'
local impute_value = (`mean_wave_bef'*(`year_wave_aft' - `year_wave_imp') +  `mean_wave_aft'*(`year_wave_imp' - `year_wave_bef')) / (`year_wave_aft' - `year_wave_bef')
display `impute_value'
sum `var' if BB_Wave_num== `imp' & BB_Country== "`country'"
assert `r(N)'==0
replace `var' = `impute_value' if BB_Wave_num== `imp' & BB_Country== "`country'"

/* X_dem_log_town_size - missing wave 5 Spain, use 4+6 - FIXED no longer missing
local var "X_dem_log_town_size"
local country "Spain"
local bef 4
local imp 5
local aft 6
sum `var' if BB_Wave_num== `bef' & BB_Country== "`country'"
local mean_wave_bef = `r(mean)'
sum `var' if BB_Wave_num== `aft' & BB_Country== "`country'"
local mean_wave_aft = `r(mean)'
sum BB_Year if BB_Wave_num==`bef' & BB_Country== "`country'"
local year_wave_bef = `r(mean)'
sum BB_Year if BB_Wave_num== `imp' & BB_Country== "`country'"
local year_wave_imp = `r(mean)'
sum BB_Year if BB_Wave_num== `aft' & BB_Country== "`country'"
local year_wave_aft = `r(mean)'
local impute_value = (`mean_wave_bef'*(`year_wave_aft' - `year_wave_imp') +  `mean_wave_aft'*(`year_wave_imp' - `year_wave_bef')) / (`year_wave_aft' - `year_wave_bef')
display `impute_value'
sum `var' if BB_Wave_num== `imp' & BB_Country== "`country'"
assert `r(N)'==0
replace `var' = `impute_value' if BB_Wave_num== `imp' & BB_Country== "`country'"*/

/* X_dem_log_town_size - missing wave 6 Netherlands, use 5 */
local var "X_dem_log_town_size"
local country "Netherlands"
local bef 5
local imp 6
sum `var' if BB_Wave_num== `bef' & BB_Country== "`country'"
local mean_wave_bef = `r(mean)'
local impute_value = `mean_wave_bef'
display `impute_value'
sum `var' if BB_Wave_num== `imp' & BB_Country== "`country'"
assert `r(N)'==0
replace `var' = `impute_value' if BB_Wave_num== `imp' & BB_Country== "`country'"

/* X_dem_log_town_size - missing wave 6 UK, use 4 */
local var "X_dem_log_town_size"
local country "UK"
local bef 4
local imp 6
sum `var' if BB_Wave_num== `bef' & BB_Country== "`country'"
local mean_wave_bef = `r(mean)'
local impute_value = `mean_wave_bef'
display `impute_value'
sum `var' if BB_Wave_num== `imp' & BB_Country== "`country'"
assert `r(N)'==0
replace `var' = `impute_value' if BB_Wave_num== `imp' & BB_Country== "`country'"

/* X_dem_Income_Decile  in Portugal missing wave 6- use wave 4 */
local var "X_dem_Income_Decile"
local country "Portugal"
local bef 4
local imp 6
sum `var' if BB_Wave_num== `bef' & BB_Country== "`country'"
local mean_wave_bef = `r(mean)'
local impute_value = `mean_wave_bef'
display `impute_value'
sum `var' if BB_Wave_num== `imp' & BB_Country== "`country'"
assert `r(N)'==0
replace `var' = `impute_value' if BB_Wave_num== `imp' & BB_Country== "`country'"


/* US */

/*  variables missing wave 4 US, use 3+5:
	X_dem_children, X_behav_join_strike, X_op_god, X_op_believe_hell*/
local us_vars "X_dem_children X_behav_join_strike X_op_god X_op_believe_hell"
local country "US"
local bef 3
local imp 4
local aft 5
foreach var of local us_vars {
	sum `var' if BB_Wave_num== `bef' & BB_Country== "`country'"
	local mean_wave_bef = `r(mean)'
	sum `var' if BB_Wave_num== `aft' & BB_Country== "`country'"
	local mean_wave_aft = `r(mean)'
	sum BB_Year if BB_Wave_num==`bef' & BB_Country== "`country'"
	local year_wave_bef = `r(mean)'
	sum BB_Year if BB_Wave_num== `imp' & BB_Country== "`country'"
	local year_wave_imp = `r(mean)'
	sum BB_Year if BB_Wave_num== `aft' & BB_Country== "`country'"
	local year_wave_aft = `r(mean)'
	local impute_value = (`mean_wave_bef'*(`year_wave_aft' - `year_wave_imp') +  `mean_wave_aft'*(`year_wave_imp' - `year_wave_bef')) / (`year_wave_aft' - `year_wave_bef')
	display `impute_value'
	sum `var' if BB_Wave_num== `imp' & BB_Country== "`country'"
	assert `r(N)'==0
	replace `var' = `impute_value' if BB_Wave_num== `imp' & BB_Country== "`country'"
}

/*  variables missing wave 5 US, use 4+6:
	X_dem_log_town_size, X_op_just_prostitution*/
local us_vars "X_dem_log_town_size X_op_just_prostitution"
local country "US"
local bef 4
local imp 5
local aft 6
foreach var of local us_vars {
	sum `var' if BB_Wave_num== `bef' & BB_Country== "`country'"
	local mean_wave_bef = `r(mean)'
	sum `var' if BB_Wave_num== `aft' & BB_Country== "`country'"
	local mean_wave_aft = `r(mean)'
	sum BB_Year if BB_Wave_num==`bef' & BB_Country== "`country'"
	local year_wave_bef = `r(mean)'
	sum BB_Year if BB_Wave_num== `imp' & BB_Country== "`country'"
	local year_wave_imp = `r(mean)'
	sum BB_Year if BB_Wave_num== `aft' & BB_Country== "`country'"
	local year_wave_aft = `r(mean)'
	local impute_value = (`mean_wave_bef'*(`year_wave_aft' - `year_wave_imp') +  `mean_wave_aft'*(`year_wave_imp' - `year_wave_bef')) / (`year_wave_aft' - `year_wave_bef')
	display `impute_value'
	sum `var' if BB_Wave_num== `imp' & BB_Country== "`country'"
	assert `r(N)'==0
	replace `var' = `impute_value' if BB_Wave_num== `imp' & BB_Country== "`country'"
}


/* X_op_preK_work_mom - missing wave 4 in US, use 5 (also missing wave 3 which is imputed using wave 1)*/
local var "X_op_preK_work_mom"
local country "US"
local aft 5
local imp 4
sum `var' if BB_Wave_num== `aft' & BB_Country== "`country'"
local mean_wave_aft = `r(mean)'
local impute_value = `mean_wave_aft'
display `impute_value'
sum `var' if BB_Wave_num== `imp' & BB_Country== "`country'"
assert `r(N)'==0
replace `var' = `impute_value' if BB_Wave_num== `imp' & BB_Country== "`country'"

/* X_op_life_after_death, X_op_believe_heaven - missing wave 4 US, use 3+6 */
local us_vars "X_op_life_after_death X_op_believe_heaven"
local country "US"
local bef 3
local imp 4
local aft 6
foreach var of local us_vars {
	sum `var' if BB_Wave_num== `bef' & BB_Country== "`country'"
	local mean_wave_bef = `r(mean)'
	sum `var' if BB_Wave_num== `aft' & BB_Country== "`country'"
	local mean_wave_aft = `r(mean)'
	sum BB_Year if BB_Wave_num==`bef' & BB_Country== "`country'"
	local year_wave_bef = `r(mean)'
	sum BB_Year if BB_Wave_num== `imp' & BB_Country== "`country'"
	local year_wave_imp = `r(mean)'
	sum BB_Year if BB_Wave_num== `aft' & BB_Country== "`country'"
	local year_wave_aft = `r(mean)'
	local impute_value = (`mean_wave_bef'*(`year_wave_aft' - `year_wave_imp') +  `mean_wave_aft'*(`year_wave_imp' - `year_wave_bef')) / (`year_wave_aft' - `year_wave_bef')
	display `impute_value'
	sum `var' if BB_Wave_num== `imp' & BB_Country== "`country'"
	assert `r(N)'==0
	replace `var' = `impute_value' if BB_Wave_num== `imp' & BB_Country== "`country'"
}


/* X_op_child_manners_import - missing wave 4 US, use wave 1+6 since this is the closest we got */ 
local var "X_op_child_manners_import"
local country "US"
local bef 1
local imp 4
local aft 6
sum `var' if BB_Wave_num== `bef' & BB_Country== "`country'"
local mean_wave_bef = `r(mean)'
sum `var' if BB_Wave_num== `aft' & BB_Country== "`country'"
local mean_wave_aft = `r(mean)'
sum BB_Year if BB_Wave_num==`bef' & BB_Country== "`country'"
local year_wave_bef = `r(mean)'
sum BB_Year if BB_Wave_num== `imp' & BB_Country== "`country'"
local year_wave_imp = `r(mean)'
sum BB_Year if BB_Wave_num== `aft' & BB_Country== "`country'"
local year_wave_aft = `r(mean)'
local impute_value = (`mean_wave_bef'*(`year_wave_aft' - `year_wave_imp') +  `mean_wave_aft'*(`year_wave_imp' - `year_wave_bef')) / (`year_wave_aft' - `year_wave_bef')
display `impute_value'
sum `var' if BB_Wave_num== `imp' & BB_Country== "`country'"
assert `r(N)'==0
replace `var' = `impute_value' if BB_Wave_num== `imp' & BB_Country== "`country'"



/* X_dem_education_completed_age - missing wave 6 Greece, use 4 */
local var "X_dem_education_completed_age"
local country "Greece"
local bef 4
local imp 6
sum `var' if BB_Wave_num== `bef' & BB_Country== "`country'"
local mean_wave_bef = `r(mean)'
local impute_value = `mean_wave_bef'
display `impute_value'
sum `var' if BB_Wave_num== `imp' & BB_Country== "`country'"
assert `r(N)'==0
replace `var' = `impute_value' if BB_Wave_num== `imp' & BB_Country== "`country'"


/* X_dem_education_completed_age - missing wave 6 US, use 5 */
local var "X_dem_education_completed_age"
local country "US"
local bef 5
local imp 6
sum `var' if BB_Wave_num== `bef' & BB_Country== "`country'"
local mean_wave_bef = `r(mean)'
local impute_value = `mean_wave_bef'
display `impute_value'
sum `var' if BB_Wave_num== `imp' & BB_Country== "`country'"
assert `r(N)'==0
replace `var' = `impute_value' if BB_Wave_num== `imp' & BB_Country== "`country'"


/* X_op_NoWorkLazy X_op_preK_work_mom X_op_work_duty X_op_work_first - missing in Austria in Wave 3 but question will be dropped anyway when analyzing all waves*/
/* X_op_men_pol_leaders X_op_boy_uni missing in Switzerland in wave 2 will be dropped anyway when analyzing all waves*/

// Drop wave 2 in UK, missing most values used only for impute values of wave 3 in UK 
// Only drop after imputing waves 1-3
/* drop if BB_Country=="UK" & BB_Wave_num==2 */

/* Romania, Croatia and Bulgaria are not currently in use.*/



/* The following commands are commented since Croatia and Romania were dropped
/* X_dem_education - use wave 3 since education not asked in one*/
local var "X_dem_education"
local country "Croatia"
local aft 3
local imp 2

sum `var' if BB_Wave_num == `aft' & BB_Country == "`country'"
local n_wave_aft = r(N)

if `n_wave_aft' > 0 {
    local mean_wave_aft = r(mean)
    local impute_value = `mean_wave_aft'
    display `impute_value'

    sum `var' if BB_Wave_num == `imp' & BB_Country == "`country'"
    assert r(N) == 0

    replace `var' = `impute_value' if BB_Wave_num == `imp' & BB_Country == "`country'"
} 
else {
    display "No valid observations for `var' in `country' Wave `aft'. Imputation skipped."
}

	/* X_op_child_indep_import and similar Crotia - use waves 2, 4*/
	local croatia_vars "X_op_child_indep_import X_op_child_work_import X_op_child_resp_import X_op_child_imag_import X_op_child_tol_import X_op_child_thrift_import X_op_child_persev_import X_op_child_faith_import X_op_child_unself_import X_op_child_obed_import X_op_child_manners_import"
	local country `"Croatia"'
	local bef 2
	local imp 3
	local aft 4
	foreach var of local croatia_vars  {
		sum `var' if BB_Wave_num== `bef' & BB_Country== "`country'"
		local mean_wave_bef = `r(mean)'
		sum `var' if BB_Wave_num== `aft' & BB_Country== "`country'"
		local mean_wave_aft = `r(mean)'
		sum BB_Year if BB_Wave_num==`bef' & BB_Country== "`country'"
		local year_wave_bef = `r(mean)'
		sum BB_Year if BB_Wave_num== `imp' & BB_Country== "`country'"
		local year_wave_imp = `r(mean)'
		sum BB_Year if BB_Wave_num== `aft' & BB_Country== "`country'"
		local year_wave_aft = `r(mean)'
		local impute_value = (`mean_wave_bef'*(`year_wave_aft' - `year_wave_imp') +  `mean_wave_aft'*(`year_wave_imp' - `year_wave_bef')) / (`year_wave_aft' - `year_wave_bef')
		display `impute_value'
		sum `var' if BB_Wave_num== `imp' & BB_Country== "`country'"
		assert `r(N)'==0
		replace `var' = `impute_value' if BB_Wave_num== `imp' & BB_Country== "`country'"
	}
	
	/*Romania - X_behav_sign_petition and similar- Romania missing wave 1. use waves 2 */
	local romania_vars "X_behav_sign_petition X_behav_join_boycott X_behav_demonstrations X_behav_join_strike X_op_respect_for_authority"
	local country "Romania"
	local aft 2
	local imp 1
	foreach var of local romania_vars  {
		sum `var' if BB_Wave_num== `aft' & BB_Country== "`country'"
		local mean_wave_aft = `r(mean)'
		local impute_value = `mean_wave_aft'
		display `impute_value'
		sum `var' if BB_Wave_num== `imp' & BB_Country== "`country'"
		assert `r(N)'==0
		replace `var' = `impute_value' if BB_Wave_num== `imp' & BB_Country== "`country'"
		
	}
	*/
	
/* X_op_NoWorkLazy X_op_preK_work_mom X_op_work_duty X_op_work_first missing Wave 3 in Austria - 
   using wave 4 and wave 1 when exists. 
   Note these  variables are dropped when keeping only variables that exist in all waves*/
local austria_vars  "X_op_NoWorkLazy X_op_preK_work_mom X_op_work_duty X_op_work_first"
local country "Austria"
local bef 1
local imp 3
local aft 4

foreach var of local austria_vars {
	sum `var' if BB_Wave_num== `bef' & BB_Country== "`country'"
	local n_wave_bef = `r(N)'
	if (`n_wave_bef'!=0)  {
		local mean_wave_bef = `r(mean)'
		sum BB_Year if BB_Wave_num==`bef' & BB_Country== "`country'"
		local year_wave_bef = `r(mean)'
	}
	sum `var' if BB_Wave_num== `aft' & BB_Country== "`country'"
	local mean_wave_aft = `r(mean)'
	sum BB_Year if BB_Wave_num== `imp' & BB_Country== "`country'"
	local year_wave_imp = `r(mean)'
	sum BB_Year if BB_Wave_num== `aft' & BB_Country== "`country'"
	local year_wave_aft = `r(mean)'
	if (`n_wave_bef'==0)  {
		local impute_value = `mean_wave_aft'
	} 
	else{
		local impute_value = (`mean_wave_bef'*(`year_wave_aft' - `year_wave_imp') +  `mean_wave_aft'*(`year_wave_imp' - `year_wave_bef')) / (`year_wave_aft' - `year_wave_bef')
	}	
	display `impute_value'
	sum `var' if BB_Wave_num== `imp' & BB_Country== "`country'"
	assert `r(N)'==0
	replace `var' = `impute_value' if BB_Wave_num== `imp' & BB_Country== "`country'" 
}

/* X_op_confidence_EU Missing in Austria Wave 1, use Wave 3 since that is what we got*/
local var "X_op_confidence_EU"
local country "Austria"
local aft 3
local imp 1
sum `var' if BB_Wave_num== `aft' & BB_Country== "`country'"
local mean_wave_aft = `r(mean)'
local impute_value = `mean_wave_aft'
display `impute_value'
sum `var' if BB_Wave_num== `imp' & BB_Country== "`country'"
assert `r(N)'==0
replace `var' = `impute_value' if BB_Wave_num== `imp' & BB_Country== "`country'"

/* X_op_neighbors_* - missing wave 3 Hungary, use 2+4 */
local country "Hungary"
local bef 2
local imp 3
local aft 4
foreach var of varlist X_op_neighbors_* {
	display "`var'"
	sum `var' if BB_Wave_num== `bef' & BB_Country== "`country'"
	local mean_wave_bef = `r(mean)'
	sum `var' if BB_Wave_num== `aft' & BB_Country== "`country'"
	local mean_wave_aft = `r(mean)'
	sum BB_Year if BB_Wave_num==`bef' & BB_Country== "`country'"
	local year_wave_bef = `r(mean)'
	sum BB_Year if BB_Wave_num== `imp' & BB_Country== "`country'"
	local year_wave_imp = `r(mean)'
	sum BB_Year if BB_Wave_num== `aft' & BB_Country== "`country'"
	local year_wave_aft = `r(mean)'
	local impute_value = (`mean_wave_bef'*(`year_wave_aft' - `year_wave_imp') +  `mean_wave_aft'*(`year_wave_imp' - `year_wave_bef')) / (`year_wave_aft' - `year_wave_bef')
	display `impute_value'
	sum `var' if BB_Wave_num== `imp' & BB_Country== "`country'"
	assert `r(N)'==0
	replace `var' = `impute_value' if BB_Wave_num== `imp' & BB_Country== "`country'"
}

/* X_op_just_abortion - use wave 4 since Denmark doesn't have 2-3*/
local var "X_op_just_abortion"
local country "Denmark"
local aft 4
local imp 1
sum `var' if BB_Wave_num== `aft' & BB_Country== "`country'"
local mean_wave_aft = `r(mean)'
local impute_value = `mean_wave_aft'
display `impute_value'
sum `var' if BB_Wave_num== `imp' & BB_Country== "`country'"
assert `r(N)'==0
replace `var' = `impute_value' if BB_Wave_num== `imp' & BB_Country== "`country'"

/* X_op_just_euthanasia - missing in Germany Wave 2, use Wave 1 and 3 */
local var "X_op_just_euthanasia"
local country "Germany"
local bef 1
local imp 2
local aft 3
sum `var' if BB_Wave_num== `bef' & BB_Country== "`country'"
local mean_wave_bef = `r(mean)'
sum `var' if BB_Wave_num== `aft' & BB_Country== "`country'"
local mean_wave_aft = `r(mean)'
sum BB_Year if BB_Wave_num==`bef' & BB_Country== "`country'"
local year_wave_bef = `r(mean)'
sum BB_Year if BB_Wave_num== `imp' & BB_Country== "`country'"
local year_wave_imp = `r(mean)'
sum BB_Year if BB_Wave_num== `aft' & BB_Country== "`country'"
local year_wave_aft = `r(mean)'
local impute_value = (`mean_wave_bef'*(`year_wave_aft' - `year_wave_imp') +  `mean_wave_aft'*(`year_wave_imp' - `year_wave_bef')) / (`year_wave_aft' - `year_wave_bef')
display `impute_value'
sum `var' if BB_Wave_num== `imp' & BB_Country== "`country'"
assert `r(N)'==0
replace `var' = `impute_value' if BB_Wave_num== `imp' & BB_Country== "`country'"

/* X_general_1_* - missing wave 3 UK, use 2+4 */
local country "UK"
local bef 2
local imp 3
local aft 4
foreach var of varlist X_general_1_* {
	sum `var' if BB_Wave_num== `bef' & BB_Country== "`country'"
	local mean_wave_bef = `r(mean)'
	sum `var' if BB_Wave_num== `aft' & BB_Country== "`country'"
	local mean_wave_aft = `r(mean)'
	sum BB_Year if BB_Wave_num==`bef' & BB_Country== "`country'"
	local year_wave_bef = `r(mean)'
	sum BB_Year if BB_Wave_num== `imp' & BB_Country== "`country'"
	local year_wave_imp = `r(mean)'
	sum BB_Year if BB_Wave_num== `aft' & BB_Country== "`country'"
	local year_wave_aft = `r(mean)'
	local impute_value = (`mean_wave_bef'*(`year_wave_aft' - `year_wave_imp') +  `mean_wave_aft'*(`year_wave_imp' - `year_wave_bef')) / (`year_wave_aft' - `year_wave_bef')
	display `impute_value'
	sum `var' if BB_Wave_num== `imp' & BB_Country== "`country'"
	assert `r(N)'==0
	replace `var' = `impute_value' if BB_Wave_num== `imp' & BB_Country== "`country'"
}

/* X_op_happiness- missing wave 3 UK, use 2+4 */
local var "X_op_happiness"
local country "UK"
local bef 2
local imp 3
local aft 4
sum `var' if BB_Wave_num== `bef' & BB_Country== "`country'"
local mean_wave_bef = `r(mean)'
sum `var' if BB_Wave_num== `aft' & BB_Country== "`country'"
local mean_wave_aft = `r(mean)'
sum BB_Year if BB_Wave_num==`bef' & BB_Country== "`country'"
local year_wave_bef = `r(mean)'
sum BB_Year if BB_Wave_num== `imp' & BB_Country== "`country'"
local year_wave_imp = `r(mean)'
sum BB_Year if BB_Wave_num== `aft' & BB_Country== "`country'"
local year_wave_aft = `r(mean)'
local impute_value = (`mean_wave_bef'*(`year_wave_aft' - `year_wave_imp') +  `mean_wave_aft'*(`year_wave_imp' - `year_wave_bef')) / (`year_wave_aft' - `year_wave_bef')
display `impute_value'
sum `var' if BB_Wave_num== `imp' & BB_Country== "`country'"
assert `r(N)'==0
replace `var' = `impute_value' if BB_Wave_num== `imp' & BB_Country== "`country'"

/* skip imputation for wave 2 - wave not in use
/* X_dem_log_town_size - missing wave 2 Estonia, use 3 that is the closest we got*/
local var "X_dem_log_town_size"
local country "Estonia"
local aft 3
local imp 2
sum `var' if BB_Wave_num== `aft' & BB_Country== "`country'"
local mean_wave_aft = `r(mean)'
local impute_value = `mean_wave_aft'
display `impute_value'
sum `var' if BB_Wave_num== `imp' & BB_Country== "`country'"
assert `r(N)'==0
replace `var' = `impute_value' if BB_Wave_num== `imp' & BB_Country== "`country'"



/* X_dem_log_town_size - missing wave 2 Sweden, use 1+3*/
local var "X_dem_log_town_size"
local country "Sweden"
local bef 1
local imp 2
local aft 3
sum `var' if BB_Wave_num== `bef' & BB_Country== "`country'"
local mean_wave_bef = `r(mean)'
sum `var' if BB_Wave_num== `aft' & BB_Country== "`country'"
local mean_wave_aft = `r(mean)'
sum BB_Year if BB_Wave_num==`bef' & BB_Country== "`country'"
local year_wave_bef = `r(mean)'
sum BB_Year if BB_Wave_num== `imp' & BB_Country== "`country'"
local year_wave_imp = `r(mean)'
sum BB_Year if BB_Wave_num== `aft' & BB_Country== "`country'"
local year_wave_aft = `r(mean)'
local impute_value = (`mean_wave_bef'*(`year_wave_aft' - `year_wave_imp') +  `mean_wave_aft'*(`year_wave_imp' - `year_wave_bef')) / (`year_wave_aft' - `year_wave_bef')
display `impute_value'
sum `var' if BB_Wave_num== `imp' & BB_Country== "`country'"
assert `r(N)'==0
replace `var' = `impute_value' if BB_Wave_num== `imp' & BB_Country== "`country'"

	/* X_dem_log_town_size - missing wave 2 Czech Republic , use 1+3*/
local var "X_dem_log_town_size"
local country "Czech Republic"
local bef 1
local imp 2
local aft 3
sum `var' if BB_Wave_num== `bef' & BB_Country== "`country'"
local mean_wave_bef = `r(mean)'
sum `var' if BB_Wave_num== `aft' & BB_Country== "`country'"
local mean_wave_aft = `r(mean)'
sum BB_Year if BB_Wave_num==`bef' & BB_Country== "`country'"
local year_wave_bef = `r(mean)'
sum BB_Year if BB_Wave_num== `imp' & BB_Country== "`country'"
local year_wave_imp = `r(mean)'
sum BB_Year if BB_Wave_num== `aft' & BB_Country== "`country'"
local year_wave_aft = `r(mean)'
local impute_value = (`mean_wave_bef'*(`year_wave_aft' - `year_wave_imp') +  `mean_wave_aft'*(`year_wave_imp' - `year_wave_bef')) / (`year_wave_aft' - `year_wave_bef')
display `impute_value'
sum `var' if BB_Wave_num== `imp' & BB_Country== "`country'"
assert `r(N)'==0
replace `var' = `impute_value' if BB_Wave_num== `imp' & BB_Country== "`country'"

	/* X_dem_Income_Decile - missing wave 2 Hungary, use 1+3*/
local var "X_dem_Income_Decile"
local country "Hungary"
local bef 1
local imp 2
local aft 3
sum `var' if BB_Wave_num== `bef' & BB_Country== "`country'"
local mean_wave_bef = `r(mean)'
sum `var' if BB_Wave_num== `aft' & BB_Country== "`country'"
local mean_wave_aft = `r(mean)'
sum BB_Year if BB_Wave_num==`bef' & BB_Country== "`country'"
local year_wave_bef = `r(mean)'
sum BB_Year if BB_Wave_num== `imp' & BB_Country== "`country'"
local year_wave_imp = `r(mean)'
sum BB_Year if BB_Wave_num== `aft' & BB_Country== "`country'"
local year_wave_aft = `r(mean)'
local impute_value = (`mean_wave_bef'*(`year_wave_aft' - `year_wave_imp') +  `mean_wave_aft'*(`year_wave_imp' - `year_wave_bef')) / (`year_wave_aft' - `year_wave_bef')
display `impute_value'
sum `var' if BB_Wave_num== `imp' & BB_Country== "`country'"
assert `r(N)'==0
replace `var' = `impute_value' if BB_Wave_num== `imp' & BB_Country== "`country'"	

/* X_dem_Income_Decile - missing wave 2 Slovenia, use 1+3*/
local var "X_dem_Income_Decile"
local country "Slovenia"
local bef 1
local imp 2
local aft 3
sum `var' if BB_Wave_num== `bef' & BB_Country== "`country'"
local mean_wave_bef = `r(mean)'
sum `var' if BB_Wave_num== `aft' & BB_Country== "`country'"
local mean_wave_aft = `r(mean)'
sum BB_Year if BB_Wave_num==`bef' & BB_Country== "`country'"
local year_wave_bef = `r(mean)'
sum BB_Year if BB_Wave_num== `imp' & BB_Country== "`country'"
local year_wave_imp = `r(mean)'
sum BB_Year if BB_Wave_num== `aft' & BB_Country== "`country'"
local year_wave_aft = `r(mean)'
local impute_value = (`mean_wave_bef'*(`year_wave_aft' - `year_wave_imp') +  `mean_wave_aft'*(`year_wave_imp' - `year_wave_bef')) / (`year_wave_aft' - `year_wave_bef')
display `impute_value'
sum `var' if BB_Wave_num== `imp' & BB_Country== "`country'"
assert `r(N)'==0
replace `var' = `impute_value' if BB_Wave_num== `imp' & BB_Country== "`country'"
*/

/* X_dem_log_town_size - missing wave 1 Iceland, use 3 that is the closest we got*/
local var "X_dem_log_town_size"
local country "Iceland"
local aft 3
local imp 1
sum `var' if BB_Wave_num== `aft' & BB_Country== "`country'"
local mean_wave_aft = `r(mean)'
local impute_value = `mean_wave_aft'
display `impute_value'
sum `var' if BB_Wave_num== `imp' & BB_Country== "`country'"
assert `r(N)'==0
replace `var' = `impute_value' if BB_Wave_num== `imp' & BB_Country== "`country'"



/* X_dem_Income_Decile - missing wave 1 Sweden, use 2 */
local var "X_dem_Income_Decile"
local country "Sweden"
local aft 2
local imp 1
sum `var' if BB_Wave_num== `aft' & BB_Country== "`country'"
local mean_wave_aft = `r(mean)'
local impute_value = `mean_wave_aft'
display `impute_value'
sum `var' if BB_Wave_num== `imp' & BB_Country== "`country'"
assert `r(N)'==0
replace `var' = `impute_value' if BB_Wave_num== `imp' & BB_Country== "`country'"



/* X_dem_Income_Decile missing in Portugal Wave 3, use Wave 1  and 4 (don't have 2)*/
local var "X_dem_Income_Decile"
local country "Portugal"
local bef 1
local imp 3
local aft 4
sum `var' if BB_Wave_num== `bef' & BB_Country== "`country'"
local mean_wave_bef = `r(mean)'
sum `var' if BB_Wave_num== `aft' & BB_Country== "`country'"
local mean_wave_aft = `r(mean)'
sum BB_Year if BB_Wave_num==`bef' & BB_Country== "`country'"
local year_wave_bef = `r(mean)'
sum BB_Year if BB_Wave_num== `imp' & BB_Country== "`country'"
local year_wave_imp = `r(mean)'
sum BB_Year if BB_Wave_num== `aft' & BB_Country== "`country'"
local year_wave_aft = `r(mean)'
local impute_value = (`mean_wave_bef'*(`year_wave_aft' - `year_wave_imp') +  `mean_wave_aft'*(`year_wave_imp' - `year_wave_bef')) / (`year_wave_aft' - `year_wave_bef')
display `impute_value'
sum `var' if BB_Wave_num== `imp' & BB_Country== "`country'"
assert `r(N)'==0
replace `var' = `impute_value' if BB_Wave_num== `imp' & BB_Country== "`country'"

/*US - X_op_confidence_courts - missing wave 3 US, use 2+4*/
local var "X_op_confidence_courts"
local country "US"
local bef 2
local imp 3
local aft 4
sum `var' if BB_Wave_num== `bef' & BB_Country== "`country'"
local mean_wave_bef = `r(mean)'
sum `var' if BB_Wave_num== `aft' & BB_Country== "`country'"
local mean_wave_aft = `r(mean)'
sum BB_Year if BB_Wave_num==`bef' & BB_Country== "`country'"
local year_wave_bef = `r(mean)'
sum BB_Year if BB_Wave_num== `imp' & BB_Country== "`country'"
local year_wave_imp = `r(mean)'
sum BB_Year if BB_Wave_num== `aft' & BB_Country== "`country'"
local year_wave_aft = `r(mean)'
local impute_value = (`mean_wave_bef'*(`year_wave_aft' - `year_wave_imp') +  `mean_wave_aft'*(`year_wave_imp' - `year_wave_bef')) / (`year_wave_aft' - `year_wave_bef')
display `impute_value'
sum `var' if BB_Wave_num== `imp' & BB_Country== "`country'"
assert `r(N)'==0
replace `var' = `impute_value' if BB_Wave_num== `imp' & BB_Country== "`country'"

/*X_op_income_equality_index - missing wave 3 for many countries, use 2+4,
In Denmark and Portugal use 1+4 since missing wave 2,
in Greece missing waves 1,2 as well - use only 4*/
local countries "Germany Hungary Latvia Malta Slovakia Sweden"
local var "X_op_income_equality_index"
local bef 2
local imp 3
local aft 4

foreach country of local countries {
    sum `var' if BB_Wave_num== `bef' & BB_Country== "`country'"
    if r(N) > 0 {
        local mean_wave_bef = r(mean)
    }
    
    sum `var' if BB_Wave_num== `aft' & BB_Country== "`country'"
    if r(N) > 0 {
        local mean_wave_aft = r(mean)
    }
    
    sum BB_Year if BB_Wave_num==`bef' & BB_Country== "`country'"
    if r(N) > 0 {
        local year_wave_bef = r(mean)
    }
    
    sum BB_Year if BB_Wave_num== `imp' & BB_Country== "`country'"
    if r(N) > 0 {
        local year_wave_imp = r(mean)
    }
    
    sum BB_Year if BB_Wave_num== `aft' & BB_Country== "`country'"
    if r(N) > 0 {
        local year_wave_aft = r(mean)
    }
    
    local impute_value = ( `mean_wave_bef' * (`year_wave_aft' - `year_wave_imp') + ///
                           `mean_wave_aft' * (`year_wave_imp' - `year_wave_bef') ) / ///
                         (`year_wave_aft' - `year_wave_bef')

    display `impute_value'

    sum `var' if BB_Wave_num== `imp' & BB_Country== "`country'"
    if r(N) > 0 {
        display "Warning: `var' already has values in `country' for wave `imp'."
    } 
	else {
        replace `var' = `impute_value' if BB_Wave_num== `imp' & BB_Country== "`country'"
    }
}



local countries "Denmark Portugal"
local var "X_op_income_equality_index"
local bef 1
local imp 3
local aft 4
foreach country of local countries  {
	sum `var' if BB_Wave_num== `bef' & BB_Country== "`country'"
	local mean_wave_bef = `r(mean)'
	sum `var' if BB_Wave_num== `aft' & BB_Country== "`country'"
	local mean_wave_aft = `r(mean)'
	sum BB_Year if BB_Wave_num==`bef' & BB_Country== "`country'"
	local year_wave_bef = `r(mean)'
	sum BB_Year if BB_Wave_num== `imp' & BB_Country== "`country'"
	local year_wave_imp = `r(mean)'
	sum BB_Year if BB_Wave_num== `aft' & BB_Country== "`country'"
	local year_wave_aft = `r(mean)'
	local impute_value = (`mean_wave_bef'*(`year_wave_aft' - `year_wave_imp') +  `mean_wave_aft'*(`year_wave_imp' - `year_wave_bef')) / (`year_wave_aft' - `year_wave_bef')
	display `impute_value'
	sum `var' if BB_Wave_num== `imp' & BB_Country== "`country'"
	assert `r(N)'==0
	replace `var' = `impute_value' if BB_Wave_num== `imp' & BB_Country== "`country'"
}	
local var "X_op_income_equality_index"
local country "Greece"
local aft 4
local imp 3
sum `var' if BB_Wave_num== `aft' & BB_Country== "`country'"
local mean_wave_aft = `r(mean)'
local impute_value = `mean_wave_aft'
display `impute_value'
sum `var' if BB_Wave_num== `imp' & BB_Country== "`country'"
assert `r(N)'==0
replace `var' = `impute_value' if BB_Wave_num== `imp' & BB_Country== "`country'"

/* X_op_preK_work_mom - missing wave 3  in US, use 1 
(also missing wave 4 which is imputed using wave 5)*/
local var "X_op_preK_work_mom"
local country "US"
local bef 1
local imp 3
sum `var' if BB_Wave_num== `bef' & BB_Country== "`country'"
local mean_wave_bef = `r(mean)'
local impute_value = `mean_wave_bef'
display `impute_value'
sum `var' if BB_Wave_num== `imp' & BB_Country== "`country'"
assert `r(N)'==0
replace `var' = `impute_value' if BB_Wave_num== `imp' & BB_Country== "`country'"

// XX - Add more variables to impute:

	/* X_dem_oc_agricultural_worker - missing wave 1  in Finland, use 2*/
local var "X_dem_oc_agricultural_worker"
local country "Finland"
local bef 2
local imp 1
sum `var' if BB_Wave_num== `bef' & BB_Country== "`country'"
local mean_wave_bef = `r(mean)'
local impute_value = `mean_wave_bef'
display `impute_value'
sum `var' if BB_Wave_num== `imp' & BB_Country== "`country'"
assert `r(N)'==0
replace `var' = `impute_value' if BB_Wave_num== `imp' & BB_Country== "`country'"

		/* X_dem_oc_farm_owner_or_manager - missing wave 1  in Finland, use 2*/
local var "X_dem_oc_farm_owner_or_manager"
local country "Finland"
local bef 2
local imp 1
sum `var' if BB_Wave_num== `bef' & BB_Country== "`country'"
local mean_wave_bef = `r(mean)'
local impute_value = `mean_wave_bef'
display `impute_value'
sum `var' if BB_Wave_num== `imp' & BB_Country== "`country'"
assert `r(N)'==0
replace `var' = `impute_value' if BB_Wave_num== `imp' & BB_Country== "`country'"

			/* X_dem_oc_non_manual_office - missing wave 1  in Finland, use 2*/
local var "X_dem_oc_non_manual_office"
local country "Finland"
local bef 2
local imp 1
sum `var' if BB_Wave_num== `bef' & BB_Country== "`country'"
local mean_wave_bef = `r(mean)'
local impute_value = `mean_wave_bef'
display `impute_value'
sum `var' if BB_Wave_num== `imp' & BB_Country== "`country'"
assert `r(N)'==0
replace `var' = `impute_value' if BB_Wave_num== `imp' & BB_Country== "`country'"

				/* X_dem_oc_other - missing wave 1  in Finland, use 2*/
local var "X_dem_oc_other"
local country "Finland"
local bef 2
local imp 1
sum `var' if BB_Wave_num== `bef' & BB_Country== "`country'"
local mean_wave_bef = `r(mean)'
local impute_value = `mean_wave_bef'
display `impute_value'
sum `var' if BB_Wave_num== `imp' & BB_Country== "`country'"
assert `r(N)'==0
replace `var' = `impute_value' if BB_Wave_num== `imp' & BB_Country== "`country'"

/*/* X_dem_oc_non_manual_office - missing wave 1  in Finland, use 3*/
local var "X_dem_oc_non_manual_office"
local country "Finland"
local bef 2
local imp 1
sum `var' if BB_Wave_num== `bef' & BB_Country== "`country'"
local mean_wave_bef = `r(mean)'
local impute_value = `mean_wave_bef'
display `impute_value'
sum `var' if BB_Wave_num== `imp' & BB_Country== "`country'"
assert `r(N)'==0
replace `var' = `impute_value' if BB_Wave_num== `imp' & BB_Country== "`country'"


/* X_dem_oc_other - missing wave 1  in Finland, use 2*/
local var "X_dem_oc_other"
local country "Finland"
local bef 2
local imp 1
sum `var' if BB_Wave_num== `bef' & BB_Country== "`country'"
local mean_wave_bef = `r(mean)'
local impute_value = `mean_wave_bef'
display `impute_value'
sum `var' if BB_Wave_num== `imp' & BB_Country== "`country'"
assert `r(N)'==0
replace `var' = `impute_value' if BB_Wave_num== `imp' & BB_Country== "`country'"
*/

/* X_dem_oc_professional - missing wave 1  in Finland, use 2*/
local var "X_dem_oc_professional"
local country "Finland"
local bef 2
local imp 1
sum `var' if BB_Wave_num== `bef' & BB_Country== "`country'"
local mean_wave_bef = `r(mean)'
local impute_value = `mean_wave_bef'
display `impute_value'
sum `var' if BB_Wave_num== `imp' & BB_Country== "`country'"
assert `r(N)'==0
replace `var' = `impute_value' if BB_Wave_num== `imp' & BB_Country== "`country'"

/* X_dem_oc_semi_skilled_manual - missing wave 1  in Finland, use 2*/
local var "X_dem_oc_semi_skilled_manual"
local country "Finland"
local bef 2
local imp 1
sum `var' if BB_Wave_num== `bef' & BB_Country== "`country'"
local mean_wave_bef = `r(mean)'
local impute_value = `mean_wave_bef'
display `impute_value'
sum `var' if BB_Wave_num== `imp' & BB_Country== "`country'"
assert `r(N)'==0
replace `var' = `impute_value' if BB_Wave_num== `imp' & BB_Country== "`country'"

/* X_dem_oc_unskilled_manual - missing wave 1  in Finland, use 2*/
local var "X_dem_oc_unskilled_manual"
local country "Finland"
local bef 2
local imp 1
sum `var' if BB_Wave_num== `bef' & BB_Country== "`country'"
local mean_wave_bef = `r(mean)'
local impute_value = `mean_wave_bef'
display `impute_value'
sum `var' if BB_Wave_num== `imp' & BB_Country== "`country'"
assert `r(N)'==0
replace `var' = `impute_value' if BB_Wave_num== `imp' & BB_Country== "`country'"



/*X_op_child_manners_import* - missing wave 3 US, use 2+4 */
local country "US"
local bef 2
local imp 3
local aft 4
foreach var of varlist X_op_child_manners_import {
	display "`var'"
	sum `var' if BB_Wave_num== `bef' & BB_Country== "`country'"
	local mean_wave_bef = `r(mean)'
	sum `var' if BB_Wave_num== `aft' & BB_Country== "`country'"
	local mean_wave_aft = `r(mean)'
	sum BB_Year if BB_Wave_num==`bef' & BB_Country== "`country'"
	local year_wave_bef = `r(mean)'
	sum BB_Year if BB_Wave_num== `imp' & BB_Country== "`country'"
	local year_wave_imp = `r(mean)'
	sum BB_Year if BB_Wave_num== `aft' & BB_Country== "`country'"
	local year_wave_aft = `r(mean)'
	local impute_value = (`mean_wave_bef'*(`year_wave_aft' - `year_wave_imp') +  `mean_wave_aft'*(`year_wave_imp' - `year_wave_bef')) / (`year_wave_aft' - `year_wave_bef')
	display `impute_value'
	sum `var' if BB_Wave_num== `imp' & BB_Country== "`country'"
	assert `r(N)'==0
	replace `var' = `impute_value' if BB_Wave_num== `imp' & BB_Country== "`country'"
}


sort BB_Id
gen id = _n
save "$POPULISM_DROPBOX/data/intdata/temp/for_impute_ivs.dta", replace
