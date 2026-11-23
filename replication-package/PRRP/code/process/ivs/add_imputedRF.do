use "$POPULISM_DROPBOX/data/intdata/temp/for_impute_ivs.dta", replace
gen missingManifeso = missing(Y_manifesto_key)
tab missingManifeso

//unab allX : X_*
//unab unimp : *_unimp
//local removeX : list allX - unimp
//display "`removeX'"
//drop `removeX'

drop X_*

merge 1:1 id using "$POPULISM_DROPBOX/data/intdata/temp/after_rf_impute_ivs.dta"


save "$POPULISM_DROPBOX/data/intdata/ivs/ivs_selected_countries_imputed.dta", replace

