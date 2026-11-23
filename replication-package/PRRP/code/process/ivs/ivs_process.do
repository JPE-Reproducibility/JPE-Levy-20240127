********* IVS **********

cd "$POPULISM_ROOT"

//run "code/ivs/marge/EVS_WVS_Merge.do"

use "$POPULISM_DROPBOX/data/intdata/ivs/Integrated_values_surveys_1981-2021", replace

// Define waves of sourvey

//EVS Time Series data from Waves 1-5
decode S002EVS, gen (BB_Wave)
replace BB_Wave = "EVS Wave 1 (1981-1984)" if BB_Wave == "1981-1984"
replace BB_Wave = "EVS Wave 2 (1990-1993)" if BB_Wave == "1990-1993"
replace BB_Wave = "EVS Wave 3 (1999-2001)" if BB_Wave == "1999-2001"
replace BB_Wave = "EVS Wave 4 (2008-2010)" if BB_Wave == "2008-2010"
replace BB_Wave = "EVS Wave 5 (2017-2020)" if BB_Wave == "2017-2020"

//WVS Time Series data from Waves 1-7
decode S002, gen (wave_WVS_temp_name)
replace BB_Wave = "WVS Wave 1 (1981-1984)" if wave_WVS_temp_name == "1981-1984"
replace BB_Wave = "WVS Wave 2 (1989-1993)" if wave_WVS_temp_name == "1989-1993"
replace BB_Wave = "WVS Wave 3 (1994-1998)" if wave_WVS_temp_name == "1994-1998"
replace BB_Wave = "WVS Wave 4 (1999-2004)" if wave_WVS_temp_name == "1999-2004"
replace BB_Wave = "WVS Wave 5 (2005-2009)" if wave_WVS_temp_name == "2005-2009"
replace BB_Wave = "WVS Wave 6 (2010-2014)" if wave_WVS_temp_name == "2010-2014"
replace BB_Wave = "WVS Wave 7 (2017-2020)" if wave_WVS_temp_name == "2017-2019"

// Define common country variable
decode S003, gen(BB_Country)

replace BB_Country = trim(BB_Country) // Taking off the space after the countries' names

// Define common party variable for EVS Note: names of parties are different in EVS Wave 5
decode E179, gen(BB_Party) // This variable exist only for waves 2-4 EVS.
decode E180WVS, gen(BB_2nd_Party)
decode E181A, gen(temp_party_evs_1_5)
decode E181, gen(temp_closest_party)
decode E182, gen(BB_Least_Party)

replace BB_Party = temp_party_evs_1_5 if BB_Wave == "EVS Wave 1 (1981-1984)" | BB_Wave == "EVS Wave 5 (2017-2020)"

* Generate original BB_party including missing answers for legitimacy analysis
gen BB_Party_org = BB_Party
replace BB_Party_org = trim(BB_Party)

/* Defining BB_Party as party you would vote for or party closest to when say don't know. For last EVS survey wave the question was only asked when E179 is Don't Know, No answer, Not applicable
For other survey waves it seems to have also been asked in other cases so we have to manually check BB_Party */
replace BB_Party = temp_closest_party if !inlist(temp_closest_party, "Not applicable") & BB_Wave=="EVS Wave 4 (2008-2010)"
replace BB_Party = temp_closest_party if inlist(BB_Party, "Missing; Unknown", "Not applicable", "No answer", "Don´t vote", "I would not vote", "I would cast a blank ballot; white vote", "None", "Other") & !inlist(temp_closest_party, "Not applicable", "Not asked in survey") & BB_Wave!="EVS Wave 4 (2008-2010)"

// A party's name may have a space before or after it.
replace BB_Party = trim(BB_Party) 
replace BB_2nd_Party = trim(BB_2nd_Party)
replace BB_Least_Party = trim(BB_Least_Party)

// Define common party variable for WVS 
gen BB_Wave_WVS = 0 if missing(S002)
replace BB_Wave_WVS = 1 if !missing(S002)

decode E179WVS, gen(temp_party_wvs)
replace BB_Party = temp_party_wvs if BB_Wave_WVS==1
replace BB_Party_org = trim(temp_party_wvs) if BB_Wave_WVS==1

// Creating BB_Date
gen str temp_date = string(S012, "%10.0g")
gen BB_Date = date(temp_date, "YMD")
format BB_Date %td

gen has_date = 1 if !missing(BB_Date)
tab BB_Wave has_date
//277,318 have dates EVS Wave 3-5 WVS Wave 4-7

/********************************
***** CLEAN *********************
********************************/

/* Create BB_Year variable */
gen BB_Year = S020
assert !missing(BB_Year)

/* General integreated wave variables, starting from wave 2*/
gen BB_Int_Wave = "Int Wave 1 (1989-1993)" if BB_Wave == "WVS Wave 2 (1989-1993)" | BB_Wave == "EVS Wave 2 (1990-1993)" 
replace BB_Int_Wave = "Int Wave 2 (1994-1998)" if BB_Wave == "WVS Wave 3 (1994-1998)"
replace BB_Int_Wave = "Int Wave 3 (1999-2004)" if BB_Wave == "WVS Wave 4 (1999-2004)" | BB_Wave == "EVS Wave 3 (1999-2001)"
replace BB_Int_Wave = "Int Wave 4 (2005-2009)" if BB_Wave == "WVS Wave 5 (2005-2009)" | BB_Wave == "EVS Wave 4 (2008-2010)" 
replace BB_Int_Wave = "Int Wave 5 (2010-2014)" if BB_Wave == "WVS Wave 6 (2010-2014)"
replace BB_Int_Wave = "Int Wave 6 (2017-2021)" if BB_Wave == "WVS Wave 7 (2017-2020)" | BB_Wave == "EVS Wave 5 (2017-2020)"
count if BB_Int_Wave=="" & 1989<=BB_Year
assert `r(N)'==0

gen BB_Wave_num = .
replace BB_Wave_num = 1 if BB_Int_Wave == "Int Wave 1 (1989-1993)"
replace BB_Wave_num = 2 if BB_Int_Wave == "Int Wave 2 (1994-1998)" 
replace BB_Wave_num = 3 if BB_Int_Wave == "Int Wave 3 (1999-2004)"
replace BB_Wave_num = 4 if BB_Int_Wave == "Int Wave 4 (2005-2009)" 
replace BB_Wave_num = 5 if BB_Int_Wave == "Int Wave 5 (2010-2014)"
replace BB_Wave_num = 6 if BB_Int_Wave == "Int Wave 6 (2017-2021)"
count if BB_Wave_num ==. & 1989<=BB_Year
assert `r(N)'==0

/* Create BB_Weight variable */
gen BB_Weight = S017
assert !missing(BB_Weight)

/* Set ID */
gen idTemp  = .
recast double idTemp  
replace idTemp  = S007
tostring idTemp, gen(BB_Id) format(%16.0f)

// Some duplicate IDs in different waves
replace BB_Id = string(BB_Wave_num) + "_" + BB_Id
distinct  BB_Id  
//assert _N==`r(ndistinct)' // Still some duplicates, same country different year (Columbia)




/********************************
***** SET COUNTRIES ************
********************************/

replace BB_Country = "UK" if BB_Country =="Great Britain"
replace BB_Country = "US" if BB_Country =="United States"

/* Create indicator for Manifesto countries */
gen BB_countryInManifesto = 0
replace BB_countryInManifesto = 1 if inlist(BB_Country, "Albania", "Armenia", "Australia", "Austria", "Azerbaijan", "Belarus", "Belgium", "Bosnia Herzegovina")
replace BB_countryInManifesto = 1 if inlist(BB_Country, "Denmark", "Estonia", "Finland", "France", "Georgia", "German Democratic Republic")
replace BB_countryInManifesto = 1 if inlist(BB_Country, "Ireland", "Israel", "Italy", "Japan", "Latvia", "Lithuania", "Luxembourg", "Malta", "Mexico")
replace BB_countryInManifesto = 1 if inlist(BB_Country, "New Zealand", "North Macedonia", "Northern Ireland", "Norway", "Poland", "Portugal", "Romania")
replace BB_countryInManifesto = 1 if inlist(BB_Country, "Russia", "Serbia", "Slovakia", "Slovenia", "South Africa", "South Korea", "Spain", "Sri Lanka")
replace BB_countryInManifesto = 1 if inlist(BB_Country, "Bulgaria", "Canada", "Croatia", "Cyprus", "Czech Republic", "Germany", "Greece", "Hungary", "Iceland")
replace BB_countryInManifesto = 1 if inlist(BB_Country, "Moldova", "Montenegro", "Netherlands", "Turkey", "Ukraine", "UK", "US", "Sweden", "Switzerland")

/* Create indicator for European Single Market countries + UK + Iceland?*/
gen BB_countryInESM = 0
replace BB_countryInESM = 1 if inlist(BB_Country, "Austria", "Belgium", "Bulgaria", "Croatia", "Czech Republic", "Denmark", "Estonia")
replace BB_countryInESM = 1 if inlist(BB_Country, "Finland", "France", "Germany", "Greece", "Hungary", "Ireland", "Italy")
replace BB_countryInESM = 1 if inlist(BB_Country, "Latvia", "Lithuania", "Luxembourg", "Malta", "Netherlands")
replace BB_countryInESM = 1 if inlist(BB_Country, "Poland", "Portugal", "Romania", "Slovakia", "Slovenia", "Spain", "Sweden")
replace BB_countryInESM = 1 if inlist(BB_Country, "Norway", "Iceland" , "Switzerland", "UK")
// Cyprus removed for now, since mix of northern and south Cyprus. See https://www.worldvaluessurvey.org/WVSDocumentationWV6.jsp
// Examples North Cyprus parties: National Unity Party. Example South Cyprus: CY: Movement for Social Democracy (KISOS/EDEK) - Social Democrats

/*Create indicator for advanced democracies*/
gen BB_countryAdvancedDem = 0
replace BB_countryAdvancedDem = 1 if inlist(BB_Country, "Australia", "Austria", "Belgium", "Canada", "Denmark", "Finland")
replace BB_countryAdvancedDem = 1 if inlist(BB_Country, "France", "Germany", "Greece", "UK", "Northern Ireland", "Iceland", "Ireland")
replace BB_countryAdvancedDem = 1 if inlist(BB_Country, "Israel", "Italy", "Luxembourg", "Netherlands", "New Zealand")
replace BB_countryAdvancedDem = 1 if inlist(BB_Country, "Norway", "Portugal", "Spain", "Sweden", "Switzerland", "US")

gen Country_Cleaned = inlist(BB_Country, "Germany", "France", "UK", "Italy", "Sweden", "Hungary", "Poland", "Austria", "Spain")
replace Country_Cleaned = 1 if inlist(BB_Country, "Belgium", "Israel", "Bulgaria", "Croatia", "Cyprus", "Czech Republic")
replace Country_Cleaned = 1 if inlist(BB_Country, "Denmark", "Estonia", "Finland", "Greece", "Iceland", "Netherlands", "Norway", "Switzerland")
replace Country_Cleaned = 1 if inlist(BB_Country, "Portugal", "Slovakia", "Slovenia", "Romania", "Lithuania", "US")


/********************************
***** DROP WAVE 1 *********************
********************************/

/* Drop South Korea?*/
drop if BB_countryAdvancedDem==0 & BB_countryInESM==0 

tab BB_Wave has_date
//125,952 have dates EVS Wave3-5 and WVS Wave 4-7

// Drop Wave 1 since doesn't have party question
drop if BB_Wave == "WVS Wave 1 (1981-1984)" || BB_Wave == "EVS Wave 1 (1981-1984)"

/* Verify that Id is unique*/
distinct  BB_Id  
assert _N==`r(ndistinct)'

/********************************
***** DROP Countries with no vote question *********************
********************************/

gen missingVote = missing(BB_Party) | BB_Party=="Not asked in survey"
egen minMissingVote = min(missingVote), by(BB_Country BB_Wave)
tab BB_Country BB_Wave if minMissingVote==1
drop if minMissingVote==1


// save
save "$POPULISM_DROPBOX/data/intdata/ivs/ivs_waves.dta", replace
