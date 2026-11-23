clear

import delimited "$POPULISM_DROPBOX/data/rawdata/downloaded/public/PopuList/The PopuList 3.0.csv", delimiter (";") clear

/*keep only populist parties*/
//drop if populist==0

//keep party_name_english populist farright farleft eurosceptic partyfacts_id

rename partyfacts_id Y_partyfacts_id
rename populist temp_populist
rename farright temp_farright
rename farleft temp_farleft
rename eurosceptic temp_eurosceptic

/*Two Romanian parties with same partyfacts id and the same parlgov id are present (Social Democratic Party (PSD), Romanian Socialist Democratic Party (PSoDR)) besides name they are the same in every aspect so dropping*/
drop if party_name_short == "PSoDR"

/* In Estonia conservative party has id 110 but this is actually the id of Union of Estonia and not the Estonian Conservative People's Party which replaced it*/
replace Y_partyfacts_id = 4094 if party_name_short=="EKRE"

/*Two Polish parties with same partyfacts id and the same parlgov id are present (Coalition for the Renewal of the Republic â€" Liberty and Hope (KORWiN), Coalition for the Renewal of the Republic â€" Liberty and Hope) besides name they are the same in every aspect so dropping*/
drop if party_name_short == "KORWiN"

/*some parties not with correct partyfacts_id*/

/*Belgium -  Nieuw-Vlaamse Alliantie */
replace Y_partyfacts_id = 36 if Y_partyfacts_id == 756

/*Poland - Congress of the New Right */
replace Y_partyfacts_id = 4713 if party_name_short == "KNP"

/*Slovenia - United Left */
replace Y_partyfacts_id = 4714 if Y_partyfacts_id == 1403

/*Norway - Red Party */
replace Y_partyfacts_id = 3698 if Y_partyfacts_id == 44

/*Switzerland - Solidarity*/
replace Y_partyfacts_id = 8653 if party_name_short == "SOL"

/*Italy - The People of Freedom*/
replace Y_partyfacts_id = 365 if party_name_short == "PDL"

/* Add Manifesto id*/
merge m:1 Y_partyfacts_id using "$POPULISM_DROPBOX/data/intdata/temp/partyfact_manifesto_crosswork.dta"
drop if _merge == 2
drop _merge

ren Y_manifesto_party_id manifesto_id

//quietly bysort Y_partyfacts_id: gen dup = cond(_N==1,0,_n)
//assert dup<=1

/* Not using currently but renaming for consistency*/
replace country_name="UK" if country_name=="United Kingdom"

sort country_name party_name_english

save "$POPULISM_DROPBOX/data/intdata/popuList/PopuList_clean.dta", replace

/* Verify that ESM countries in PopuList*/
tab country_name
gen BB_countryInESM = 0
replace BB_countryInESM = 1 if inlist(country_name, "Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czech Republic", "Denmark", "Estonia")
replace BB_countryInESM = 1 if inlist(country_name, "Finland", "France", "Germany", "Greece", "Hungary", "Ireland", "Italy")
replace BB_countryInESM = 1 if inlist(country_name, "Latvia", "Lithuania", "Luxembourg", "Malta", "Netherlands")
replace BB_countryInESM = 1 if inlist(country_name, "Poland", "Portugal", "Romania", "Slovakia", "Slovenia", "Spain", "Sweden")
replace BB_countryInESM = 1 if inlist(country_name, "Norway", "Iceland", "Switzerland")
tab BB_countryInESM 
