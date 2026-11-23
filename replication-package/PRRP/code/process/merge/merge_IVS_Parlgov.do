use "$POPULISM_DROPBOX/data/intdata/temp/ivs_with_populist.dta", clear


/**********************
******* Merge with parlgov ids *******
**********************/

/*keep if BB_countryInESM == 1

gen BB_id = _n
duplicates report BB_id
assert `r(unique_value)'==_N */


/*finding mistakes by looking at parties that have a party_facts id but were not merged*/


/*Merge WVS with external parlgov to get parlgov ids*/
merge m:1 Y_partyfacts_id using "$POPULISM_DROPBOX/data/intdata/partyfacts/external_parlgov.dta", keep(match master) gen(mergeParlGov)


replace Y_parlgov_id = 1292 if Y_Party_Vote_Long == "FIN: Left Alliance (VAS)"

/*Different parlgov partyfacts id than manifesto. Parlgov technically is the Democratic Union/Left Alliance, but wikipedia says the Democratic Union is the children's wing of the Left Alliance*/
replace Y_parlgov_id = 59 if Y_Party_Vote_Long == "FRA: Liberal Democracy"

/*Similar to the above. When the leader of the party won, he changed its name from Republicans to Liberal Democracy. The parlgov name includes both, but has the same end date as the Liberal Democracy party.*/
replace Y_parlgov_id = 791 if Y_Party_Vote_Long == "DEU: Democratic Socialism Party"

/*NOR*/ // Red party appears as red electoral alliance in parlgov but that party no longer exists
replace Y_parlgov_id = 1638 if Y_Party_Vote_Long == "NOR: Red Party" & BB_Year>=2017

/*CZE*/ // Czech Socialist Party (CSS) change the name and appears as Czech National Social(ist) Party in parlgov 
replace Y_parlgov_id = 1813 if Y_Party_Vote_Long == "CZE: Czech Socialist Party (CSS)" & BB_Year<=1993

/*Rou*/ // Different parlgov partyfacts id than manifesto. Social democratic party (PSD) also named FDSN. This party was in alliance in 2008 under partyfact_id 120 and parlgov remained under that partyfact_id.
replace Y_parlgov_id = 1120 if Y_Party_Vote_Long == "ROU: Social Democratic Party" | Y_Party_Vote_Long == "ROU: Democratic National Salvation Front"

/*Similar to the above, with PDS and Die Linke*/

/*Correcting duplicates*/

/*DEU*/
replace Y_parlgov_id = 1235 if Y_partyfacts_id==2702 & BB_Year>1994

/*HRV*/
replace Y_parlgov_id = 2771 if Y_partyfacts_id==2524 & BB_Year>2003

/*HUN*/
replace Y_parlgov_id = 2659 if Y_partyfacts_id==859 & BB_Year>2014

/*IRL*/
replace Y_parlgov_id = 2328 if Y_partyfacts_id==2735 & BB_Year<2014

/*ISR*/
replace Y_parlgov_id = 1862 if Y_partyfacts_id==3148 & BB_Year>1996

/*ITA*/
replace Y_parlgov_id = 1304 if Y_partyfacts_id==851 & BB_Year<1992

/*JPN*/
replace Y_parlgov_id = 163 if Y_parlgov_id==2750 & BB_Year<2005

/*LTU*/
replace Y_parlgov_id = 1408 if Y_parlgov_id==64 & BB_Year<2004

replace Y_parlgov_id = 983 if Y_partyfacts_id==1357 // party "Lithuanian Liberal Union"(1357) was marge with "Liberal and Centre Union"(1193) in 2004, in parlgov only "Liberal and Centre Union" appears for 1996-2014.



save "$POPULISM_DROPBOX/data/intdata/temp/ivs_clean_with_parlgov.dta", replace
