/* This file adds manifesto id based on partyfact id, 
in some cases need to first change partyfact_id manually when a party has only one id in WVS but multiple in manifesto
The purpose of manually changing ids is to make sure that each observation that has a manifesto platform gets assigned that platform*
Find potential cases based on missing Y_manifesto_key even though Y_partyfacts_id exists */


use "$POPULISM_DROPBOX/data/intdata/temp/ivs_clean_with_partyfact.dta", clear

/**********************
******* ADD PARTYFACT AND MANIFESTO ID *******
**********************/

/* Modifying party_fact id for German green party - appears as one party in WVS but three parties in Party_fact 
so assigning id by year (no WVS data for before 1990, so only two ids are relevant)
Need to split to two IDs to get relevant manifestos*/
replace Y_partyfacts_id = 865 if Y_Party_Vote == "Green_GER" & BB_Year == 1990

/* Correcting an automatically matched Bulgarian party with the partyfacts_id that matches to manifesto */
replace Y_partyfacts_id = 1793 if Y_Party_Vote == "Attack_BGR"

/* Correcting an automatically matched Slovenian party with the partyfacts_id that matches to manifesto */
replace Y_partyfacts_id = 472 if Y_Party_Vote_Long == "SVN: Slovenian Democratic Party"
replace Y_partyfacts_id = 1773 if Y_Party_Vote_Long == "SVN: Positive Slovenia"

/* Correcting an automatically matched Romanian party with the partyfacts_id that matches to manifesto */
replace Y_partyfacts_id = 276 if Y_Party_Vote_Long == "ROU: Social Democratic Party" 
replace Y_partyfacts_id = 5941 if Y_Party_Vote_Long == "ROU: Social Liberal Coallition"
replace Y_partyfacts_id = 120 if Y_Party_Vote_Long == "ROU: Social Democratic Party" & 2005<=BB_Year & BB_Year<=2008 // In year 2008 the party was running with a coalition named Social Democratic Party + Conservative Party Alliance

replace Y_2nd_partyfacts_id = 865 if Y_2nd_Party_Vote == "Green_GER" & BB_Year == 1990

/* Correcting an automatically matched Bulgarian party with the partyfacts_id that matches to manifesto */
replace Y_2nd_partyfacts_id = 1793 if Y_2nd_Party_Vote == "Attack_BGR"

/* Correcting an automatically matched Slovenian party with the partyfacts_id that matches to manifesto */
replace Y_2nd_partyfacts_id = 472 if Y_2nd_Party_Vote_Long == "SVN: Slovenian Democratic Party"
replace Y_2nd_partyfacts_id = 1773 if Y_2nd_Party_Vote_Long == "SVN: Positive Slovenia"

/* Correcting an automatically matched Romanian party with the partyfacts_id that matches to manifesto */
replace Y_2nd_partyfacts_id = 276 if Y_2nd_Party_Vote_Long == "ROU: Social Democratic Party" 
replace Y_2nd_partyfacts_id = 5941 if Y_2nd_Party_Vote_Long == "ROU: Social Liberal Coallition"
replace Y_2nd_partyfacts_id = 120 if Y_2nd_Party_Vote_Long == "ROU: Social Democratic Party" & 2005<=BB_Year & BB_Year<=2008 // In year 2008 the party was running with a coalition named Social Democratic Party + Conservative Party Alliance

replace Y_partyfacts_id = 865 if Y_Party_Vote == "Green_GER" & BB_Year == 1990

/* Correcting an automatically matched Bulgarian party with the partyfacts_id that matches to manifesto */
replace Y_Least_partyfacts_id = 1793 if Y_Least_Party_Vote == "Attack_BGR"

/* Correcting an automatically matched Slovenian party with the partyfacts_id that matches to manifesto */
replace Y_Least_partyfacts_id = 472 if Y_Least_Party_Vote_Long == "SVN: Slovenian Democratic Party"
replace Y_Least_partyfacts_id = 1773 if Y_Least_Party_Vote_Long == "SVN: Positive Slovenia"

/* Correcting an automatically matched Romanian party with the partyfacts_id that matches to manifesto */
replace Y_Least_partyfacts_id = 276 if Y_Least_Party_Vote_Long == "ROU: Social Democratic Party" 
replace Y_Least_partyfacts_id = 5941 if Y_Least_Party_Vote_Long == "ROU: Social Liberal Coallition"
replace Y_Least_partyfacts_id = 120 if Y_Least_Party_Vote_Long == "ROU: Social Democratic Party" & 2005<=BB_Year & BB_Year<=2008 // In year 2008 the party was running with a coalition named Social Democratic Party + Conservative Party Alliance

merge m:1 Y_partyfacts_id using "$POPULISM_DROPBOX/data/intdata/partyfacts/external_manifesto_ivs.dta", keep(match master) nogen
merge m:1 Y_2nd_partyfacts_id using "$POPULISM_DROPBOX/data/intdata/partyfacts/external_2nd_manifesto_ivs.dta", keep(match master) keepusing(Y_manifesto_2nd_party_id) nogen
merge m:1 Y_Least_partyfacts_id using "$POPULISM_DROPBOX/data/intdata/partyfacts/external_Least_manifesto_ivs.dta", keep(match master) keepusing(Y_manifesto_Least_party_id) nogen

/* In some cases two parties in manifesto matched to one partyfact party. Changing manually the manifesto party for merge*/


//Belgium
replace Y_manifesto_party_id = 21915 if Y_partyfacts_id==1424 & BB_Year>=1999
replace Y_manifesto_party_id = 21425 if Y_partyfacts_id==8259 & BB_Year>=1999

//Bulgaria
replace Y_manifesto_party_id = 80228 if Y_partyfacts_id==1665 & BB_Year<1997
replace Y_manifesto_party_id = 80229 if Y_partyfacts_id==1665 & 1997<=BB_Year & BB_Year<2001
replace Y_manifesto_party_id = 80410 if Y_partyfacts_id==482 & BB_Year<=1999 // This party dosent appear in the dataset but does exist in manifesto  
 
//CZE
replace Y_manifesto_party_id = 82529 if Y_partyfacts_id==676 & BB_Year<1992


//Estonia
replace Y_manifesto_party_id = 83220 if Y_partyfacts_id==1556 & BB_Year<1995
replace Y_manifesto_party_id = 83709 if Y_partyfacts_id==1556 & 1995<=BB_Year & 2000<=BB_Year

//Germany
replace Y_manifesto_party_id = 41222 if Y_partyfacts_id==86 & 2005<=BB_Year


// Greece - one party in party facts but two in manifesto 
replace Y_manifesto_party_id = 34212 if Y_partyfacts_id==301 & 2015<=BB_Year
tab Y_manifesto_party_id  BB_Year if Y_partyfacts_id==301 



/* Check that don't have multiple manifesto for one Y_Party_Vote_Long (currently have just a few, need to decide if these are the same party or not)*/
egen tagY_m_id = tag(Y_Party_Vote_Long Y_manifesto_party_id) if !missing(Y_Party_Vote_Long, Y_manifesto_party_id) & Y_Party_Vote_Long!="."
bysort Y_Party_Vote_Long: egen uniquePartyIdByVote = total(tagY_m_id )
sum uniquePartyIdByVote if !missing(Y_Party_Vote_Long)
assert  `r(max)'==1

egen tagY_m_2nd_id = tag(Y_2nd_Party_Vote_Long Y_manifesto_2nd_party_id) if !missing(Y_2nd_Party_Vote_Long, Y_manifesto_2nd_party_id) & Y_2nd_Party_Vote_Long!="."
bysort Y_2nd_Party_Vote_Long: egen unique2ndPartyIdByVote = total(tagY_m_2nd_id )
sum unique2ndPartyIdByVote if !missing(Y_2nd_Party_Vote_Long)
assert  `r(max)'==1


/* Example for how to investigate
tab Y_manifesto_party_id BB_Year if Y_Party_Vote_Long=="BEL: People's Union*/


// Croatia
replace Y_manifesto_party_id = 81021 if Y_partyfacts_id==214 & 1999<=BB_Year & 2003>BB_Year 
replace Y_manifesto_party_id = 81042 if Y_partyfacts_id==214 & 2003<=BB_Year & 2007>BB_Year
replace Y_manifesto_party_id = 81041 if Y_partyfacts_id==214 & 2007<=BB_Year 

replace Y_manifesto_party_id = 81041 if Y_partyfacts_id==3918 & 2007<=BB_Year & BB_Year<2015
replace Y_manifesto_party_id = 81091 if Y_partyfacts_id==3918 & 2015<=BB_Year & BB_Year<2016
replace Y_manifesto_party_id = 81043 if Y_partyfacts_id==3918 & 2016<=BB_Year 

replace Y_manifesto_party_id = 81220 if Y_partyfacts_id==1475  

// Lithuania
replace Y_manifesto_party_id = 88460 if Y_partyfacts_id==237 & 2008<=BB_Year 
replace Y_manifesto_party_id = 88621 if Y_partyfacts_id==193 & 2008<=BB_Year 

replace Y_manifesto_2nd_party_id = 88460 if Y_2nd_partyfacts_id==237 & 2008<=BB_Year 
replace Y_manifesto_2nd_party_id = 88621 if Y_2nd_partyfacts_id==193 & 2008<=BB_Year 

replace Y_manifesto_Least_party_id = 88460 if Y_Least_partyfacts_id==237 & 2008<=BB_Year 
replace Y_manifesto_Least_party_id = 88621 if Y_Least_partyfacts_id==193 & 2008<=BB_Year 

// Latvia
replace Y_manifesto_party_id = 87421 if Y_partyfacts_id==1719 & 1995<=BB_Year 

// Slovenia
replace Y_manifesto_party_id = 97322 if Y_partyfacts_id==1403 & 2004<=BB_Year
replace Y_manifesto_party_id = 97330 if Y_Party_Vote_Long == "SVN: Slovenian Democratic Party"

replace Y_manifesto_2nd_party_id = 97322 if Y_2nd_partyfacts_id==1403 & 2004<=BB_Year
replace Y_manifesto_2nd_party_id = 97330 if Y_2nd_Party_Vote_Long == "SVN: Slovenian Democratic Party"

replace Y_manifesto_Least_party_id = 97322 if Y_Least_partyfacts_id==1403 & 2004<=BB_Year
replace Y_manifesto_Least_party_id = 97330 if Y_Least_Party_Vote_Long == "SVN: Slovenian Democratic Party"

// These parties don't have manifesto matched to them in PartyFacts (check in the future for a partyfact update) 
replace Y_manifesto_party_id = 97020 if Y_partyfacts_id == 4714
replace Y_manifesto_party_id = 97460 if Y_partyfacts_id == 3114
replace Y_manifesto_party_id = 97461 if Y_partyfacts_id == 3098

replace Y_manifesto_2nd_party_id = 97020 if Y_2nd_partyfacts_id == 4714
replace Y_manifesto_2nd_party_id = 97460 if Y_2nd_partyfacts_id == 3114
replace Y_manifesto_2nd_party_id = 97461 if Y_2nd_partyfacts_id == 3098

replace Y_manifesto_Least_party_id = 97020 if Y_Least_partyfacts_id == 4714
replace Y_manifesto_Least_party_id = 97460 if Y_Least_partyfacts_id == 3114
replace Y_manifesto_Least_party_id = 97461 if Y_Least_partyfacts_id == 3098

// Poland: These parties only exist since 2015 and don't appear in PartyFacts yet 
// (check in the future for a partyfact update)
replace Y_manifesto_party_id = 92720 if Y_partyfacts_id==4631
replace Y_manifesto_party_id = 92450 if Y_partyfacts_id==4630

replace Y_manifesto_2nd_party_id = 92720 if Y_2nd_partyfacts_id==4631
replace Y_manifesto_2nd_party_id = 92450 if Y_2nd_partyfacts_id==4630

replace Y_manifesto_Least_party_id = 92720 if Y_Least_partyfacts_id==4631
replace Y_manifesto_Least_party_id = 92450 if Y_Least_partyfacts_id==4630

// Spain partyfacts_id 2538 was removed due to duplicates fixed manually 
replace Y_manifesto_party_id = 33612 if Y_Party_Vote_Long=="SPA: Asturias Forum (FAC)"
replace Y_manifesto_party_id = 33093 if Y_Party_Vote_Long=="SPA: Compromise"

replace Y_manifesto_2nd_party_id = 33612 if Y_2nd_Party_Vote_Long=="SPA: Asturias Forum (FAC)"
replace Y_manifesto_2nd_party_id = 33093 if Y_2nd_Party_Vote_Long=="SPA: Compromise"

replace Y_manifesto_Least_party_id = 33612 if Y_Least_Party_Vote_Long=="SPA: Asturias Forum (FAC)"
replace Y_manifesto_Least_party_id = 33093 if Y_Least_Party_Vote_Long=="SPA: Compromise"


/* Only one case where survey was asked in two years in same country - Columbia in WVS_Wave_3. Fix manually for now since not using Columbia anyway*/
replace BB_Year=1997 if BB_Year==1998 & BB_Country=="Colombia"

/* Manually add cases that exist in manifesto but not party fact*/
replace Y_manifesto_party_id=32451 if Y_Party_Vote_Long=="ITA: More Europe"
replace Y_manifesto_2nd_party_id=32451 if Y_2nd_Party_Vote_Long=="ITA: More Europe"
replace Y_manifesto_Least_party_id=32451 if Y_Least_Party_Vote_Long=="ITA: More Europe"

/* Party for Freedom and Progress preceded two Belgian parties, one French and one Dutch-speaking. There are 2 votes for the Party for Freedom and Progress 16 years after it dissolves. Looking at the original BB_Party, one was for the French-speaking Reformist Movement and the other for the Dutch-speaking Open Flemish Liberals and Democrats. I put the two votes for Freedom and Progress after its dissolution in their respective succeeded parties */
//tab Y_Party_Vote_Long if manifestoYear==162 

/* Romania- This party dosent appear in the dataset but does exist in manifesto */
replace Y_manifesto_party_id = 93002 if Y_partyfacts_id == 120
replace Y_manifesto_2nd_party_id = 93002 if Y_2nd_partyfacts_id == 120
replace Y_manifesto_Least_party_id = 93002 if Y_Least_partyfacts_id == 120

save "$POPULISM_DROPBOX/data/intdata/temp/ivs_temp.dta", replace




/********************************************************
******* MERGE AT COUNTRY*PARTY*WAVE LEVEL WITH MANIFESTO *******
***************************************************************/

use "$POPULISM_DROPBOX/data/intdata/temp/ivs_temp.dta", clear


/* Check if multiple Y_Party_Vote_Long for same manifesto year -> that would indicate an error (should merge those parties)*/
drop if missing(Y_manifesto_party_id )
egen manifestoWave = group(Y_manifesto_party_id BB_Int_Wave)
quiet: unique Y_Party_Vote_Long,  gen(groupPartiesManifestoWave) by(manifestoWave)
sum groupPartiesManifestoWave
assert `r(max)'==1

/* If assertion is false*/
/*list Y_Party_Vote_Long BB_Int_Wave manifestoWave groupPartiesManifestoWave if !missing(groupPartiesManifestoWave) & groupPartiesManifestoWave>1
list Y_Party_Vote_Long  Y_partyfacts_id Y_manifesto_party_id  BB_Int_Wave BB_Year BB_Party if manifestoWave ==705
list Y_Party_Vote_Long  Y_partyfacts_id  if manifestoYear==668*/


/* In order to merge with Manifesto only need one observation per party*country*wave*/
egen countryPartyWave = group(BB_Country BB_Wave_num Y_manifesto_party_id)



/* Multiple years in wave */
egen countryWave = group(BB_Country BB_Int_Wave)
unique BB_Year, by(countryWave) gen(yearsInWaveTemp)
egen yearsInWave = max(yearsInWaveTemp), by(countryWave)
tab BB_Country BB_Year if yearsInWave>1
tab BB_Country BB_Year if yearsInWave>1 & !Y_No_Party_Vote

/* For each country*wave wave calculate or impute a mean date*/
gen imputed_date = BB_Date
replace imputed_date = date(string(BB_Year)+"0630","YMD")  if missing(imputed_date)
egen date_for_manifesto = mean(imputed_date), by(BB_Country BB_Wave_num)
format date_for_manifesto %td

//set.seed(222)
//generate random = .
//replace random = runiform()
//sort random
//list BB_Country BB_Int_Wave BB_Year date_for_manifesto in 1/10


/* Keep only some variables*/
keep countryPartyWave  BB_Country BB_Wave_num  Y_manifesto_party_id Y_Party_Vote_Long date_for_manifesto 
duplicates drop
drop if missing(countryPartyWave) 

/* Verify that only one manifesto per year, don't need this if using date
egen countManifesto = count(Y_manifesto_party_id), by(BB_Wave_num Y_manifesto_party_id)
count if countManifesto>1
assert r(N)==0
drop countManifesto*/

/* Temporarily rename for merge. Using joinby for cartesian merge for each party we want all manifestos and then will chose the most relevant one */
joinby Y_manifesto_party_id using "$POPULISM_DROPBOX/data/cleandata/manifesto/manifesto.dta", unmatched(master)

/* Drop few observations that have no manifesto details (for example Golden Dawn in last Greek elections)*/ 
drop if missing(per101)



// keep closest date
gen distance = abs(edate-date_for_manifesto)  
egen minDistParty = min(distance), by(BB_Wave_num Y_manifesto_party_id)
egen minDistElection = min(distance), by(BB_Wave_num BB_Country)


tab per101 if Y_Party_Vote_Long=="GRC: Golden Dawn"
tab Y_manifesto_key if Y_Party_Vote_Long=="GRC: Peoples Association / Golden Dawn"

/* First only keep the minimum distance */
drop if minDistParty < distance

/* Drop if closest manifesto 5 years away*/
drop if 5*365<minDistParty




/* Can drop cases where no manifesto in closest election, not dropping for now
Example: party_fact 1108 (Y_Party_Vote_Disagg=="FRA: The Greens") in 1990 does not have a manifesto entry in 1988 while other parties do. 
Therefore we don't match the green even though there is a manifesto entry in 1993 */
/*gen partyExistNoRecent = minDistElection<minDistParty
replace partyDateManifesto = . if partyExistNoRecent */


/* Note there are cases where two survey will use the same manifesto, e.g., Sweden EVS Wave 4 (2008-2010), WVS Wave 6 (2010-2014)*/
//egen partyDateManifesto = group(Y_manifesto_party_id edate), missing


/* Verify that one manifesto per year for match. 
Should not have only manifesto_id more than once in the same wave (initially will have many due to join by)*/	
egen manifestoWave = group(BB_Wave_num Y_manifesto_party_id)
distinct manifestoWave 
assert r(ndistinct) == _N
 
 /* If errors occur: 
egen countManifestoWave = count(manifestoWave ), by(manifestoWave )
tab countManifestoWave
list edate date_for_manifesto BB_Wave_num Y_Party_Vote Y_manifesto_party_id edate if countManifestoWave==2 */

/* In some case two different manifestos tied to same party. For example for the greater Romania Party we use the 2004 manifesto for 2005 and 2018
UK 1998 and 1999 survey use 1997 data
distinct partyYearManifesto
list BB_Country BB_Year manifesto_party_name manifestoYear if manifesto_party_name=="Labour Party" */

drop partyabbrev-id_perm country oecdmember eumember date  _merge 

save "$POPULISM_DROPBOX/data/cleandata/ivs/ivs_to_manifesto.dta", replace



/********************************************************
******* SAVE INFORMATION IN MAIN DATASET *******
***************************************************************/

use "$POPULISM_DROPBOX/data/intdata/temp/ivs_temp.dta", clear

merge m:1 BB_Wave_num Y_manifesto_party_id using "$POPULISM_DROPBOX/data/cleandata/ivs/ivs_to_manifesto.dta", keepusing(Y_manifesto_key) keep(match master) nogen


/* Parties that do NOT exist in manifesto (after checking manually) and can't be matched for our years*/

/* Netherlands*/
gen noManifestoData = 1 if inlist(Y_partyfacts_id, 960, 1533, 1656) // dosent appear in manifesto 

/* Norway */
replace noManifestoData = 1 if inlist(Y_partyfacts_id, 44, 3698, 317, 677, 1079) // dosent appear in manifesto 
replace noManifestoData = 1 if Y_Party_Vote_Long=="NOR: Democrats in Norway" | Y_Party_Vote_Long=="NOR: The Christians" // small parties, no data 

/* Test for Norway */
gen haveManifesto = !missing(Y_manifesto_key)
tab Y_partyfacts_id if BB_Country=="Norway" & !haveManifesto & Y_No_Party_Vote==0 & noManifestoData!=1, missing

/* Czech Republic */
tab Y_partyfacts_id haveManifesto if BB_Country=="Czech Republic" & Y_No_Party_Vote==0 , missing
replace noManifestoData = 1 if Y_partyfacts_id==1554 & BB_Year<2008 // No data in menifesto untill entered the coalition in 2006
replace noManifestoData = 1 if Y_partyfacts_id==905 & (BB_Year<1998 | BB_Year>1999) // No data in menifesto for those years
replace noManifestoData = 1 if Y_partyfacts_id==952 & BB_Year>1991 // No data in menifesto for this year
replace noManifestoData = 1 if Y_partyfacts_id==1657 & BB_Year>1999 // No data in menifesto for this year
replace noManifestoData = 1 if Y_partyfacts_id==2047 & BB_Year<=1991 // No data in menifesto for this year
replace noManifestoData = 1 if Y_partyfacts_id==223 & BB_Year<=1991 // No data in menifesto for this year
replace noManifestoData = 1 if Y_partyfacts_id==2141 & BB_Year<=2008 // No data in menifesto for this year
replace noManifestoData = 1 if inlist(Y_partyfacts_id, 1425, 1171, 1796, 165, 1041, 1127, 1813, 254, 805, 1272) // dosent appear in manifesto

/* Most parties that dosent have partyfacts_id are from 1991 (353/471), in this years there was menifesto data only on 15 parties*/ 
replace noManifestoData = 1 if missing(Y_partyfacts_id) & BB_Country=="Czech Republic" & !haveManifesto & Y_No_Party_Vote==0 & BB_Year<=1991
/* Parties that dont have partyfacts_id after 1991 are small (in the data) and dose not appear in manifesto */
replace noManifestoData = 1 if missing(Y_partyfacts_id) & BB_Country=="Czech Republic" & !haveManifesto & Y_No_Party_Vote==0 & BB_Year>1991

/* Test for Czech Republic */
tab Y_partyfacts_id if BB_Country=="Czech Republic" & !haveManifesto & Y_No_Party_Vote==0 & noManifestoData!=1, missing
count if BB_Country=="Czech Republic" & noManifestoData==1 & Y_No_Party_Vote==0
count if BB_Country=="Czech Republic" & haveManifesto & noManifestoData!=1 & Y_No_Party_Vote==0

/* Poland */
tab Y_partyfacts_id haveManifesto if BB_Country=="Poland" & Y_No_Party_Vote==0 , missing
replace noManifestoData = 1 if inlist(Y_partyfacts_id, 70, 391, 1286, 1328, 4629, 4871) // dosent appear in manifesto 
replace noManifestoData = 1 if Y_partyfacts_id==1104 & BB_Year>=2008 // No data in menifesto for this year
replace noManifestoData = 1 if Y_partyfacts_id==57 & BB_Year>=2017 // No data in menifesto for this year
replace noManifestoData = 1 if Y_partyfacts_id==1768 & BB_Year>=2012 // No data in menifesto for this year
replace noManifestoData = 1 if Y_partyfacts_id==283 & BB_Year>=1997 // No data in menifesto for this year
replace noManifestoData = 1 if Y_partyfacts_id==468 & BB_Year>=1997 // No data in menifesto for this year
replace noManifestoData = 1 if Y_partyfacts_id==1565 & BB_Year<=1990 // No data in menifesto for this year

/* Y_partyfacts_id== 4631 & 4630 votes are from 2017, menifesto has data on this prty and wave' it dosent appear in the excel sheet */

/* Parties that need to be checked: 
gen need_check_Pol= 1 if Y_partyfacts_id==57 | Y_partyfacts_id==767 |Y_partyfacts_id==863 |Y_partyfacts_id==4630 |Y_partyfacts_id==4631
tab Y_partyfacts_id if BB_Country=="Czech Republic" & !haveManifesto & Y_No_Party_Vote==0 & noManifestoData!=1, missing */

/* Test for Poland */
tab Y_partyfacts_id if BB_Country=="Poland" & !haveManifesto & Y_No_Party_Vote==0 & noManifestoData!=1, missing
count if BB_Country=="Poland" & noManifestoData==1 & Y_No_Party_Vote==0 /* & !need_check */
count if BB_Country=="Poland" & haveManifesto & noManifestoData!=1 & Y_No_Party_Vote==0 /* & !need_check */

/* Switzerland */
tab Y_partyfacts_id haveManifesto if BB_Country=="Switzerland" & Y_No_Party_Vote==0 , missing
replace noManifestoData = 1 if inlist(Y_partyfacts_id, 2939, 8184, 8653) // dosent appear in manifesto 
replace noManifestoData = 1 if Y_partyfacts_id==1122 & BB_Year<=1996 // No data in menifesto for this year
replace noManifestoData = 1 if Y_partyfacts_id==1311 & BB_Year>=2007 // No data in menifesto for this year
replace noManifestoData = 1 if inlist(Y_Party_Vote, "AlternativeList_CHE", "ChristlichDemocraticUnion_CHE", "Ecologist_CHE", "Frap_CHE", "Keine_CHE") // dosent appear in manifesto

/* need to check parties Keine_CHE and Ecologist_CHE */

/* Test for Switzerland */
tab Y_partyfacts_id if BB_Country=="Switzerland" & !haveManifesto & Y_No_Party_Vote==0 & noManifestoData!=1, missing
count if BB_Country=="Switzerland" & noManifestoData==1 & Y_No_Party_Vote==0 
count if BB_Country=="Switzerland" & haveManifesto & noManifestoData!=1 & Y_No_Party_Vote==0 

/* Portugal */
tab Y_partyfacts_id haveManifesto if BB_Country=="Portugal" & Y_No_Party_Vote==0 , missing
replace noManifestoData = 1 if inlist(Y_partyfacts_id, 857, 1301, 1480, 2893) // dosent appear in manifesto 
replace noManifestoData = 1 if inlist(Y_partyfacts_id, 3225, 3226, 7357, 8182, 8640) // dosent appear in manifesto- all from 2020

/* Test for Portugal */
tab Y_partyfacts_id if BB_Country=="Portugal" & !haveManifesto & Y_No_Party_Vote==0 & noManifestoData!=1, missing
count if BB_Country=="Portugal" & noManifestoData==1 & Y_No_Party_Vote==0 
count if BB_Country=="Portugal" & haveManifesto & noManifestoData!=1 & Y_No_Party_Vote==0 

/* Slovakia */
tab Y_partyfacts_id if BB_Country=="Slovakia" & !haveManifesto & Y_No_Party_Vote==0 & noManifestoData!=1, missing
replace noManifestoData = 1 if inlist(Y_partyfacts_id, 200, 137, 2917, 217, 1406) // dosent appear in manifesto 
replace noManifestoData = 1 if Y_partyfacts_id==1242 & BB_Year>=2008 // No data in menifesto for this year
replace noManifestoData = 1 if Y_partyfacts_id==340 & BB_Year<=1990 | Y_partyfacts_id==340 & BB_Year>=2008 // No data in menifesto for this year
replace noManifestoData = 1 if Y_partyfacts_id==1569 & BB_Year>=2017 // No data in menifesto for this year
replace noManifestoData = 1 if Y_partyfacts_id==349 & BB_Year>=2017 // No data in menifesto for this year
replace noManifestoData = 1 if Y_partyfacts_id==656 & BB_Year<=1998 // No data in menifesto for this year
replace noManifestoData = 1 if inlist(Y_Party_Vote, "Businessmen_SVK", "Farmers_SVK") // dosent appear in manifesto

/* Test for Slovakia */
tab Y_partyfacts_id if BB_Country=="Slovakia" & !haveManifesto & Y_No_Party_Vote==0 & noManifestoData!=1, missing
count if BB_Country=="Slovakia" & noManifestoData==1 & Y_No_Party_Vote==0 
count if BB_Country=="Slovakia" & haveManifesto & noManifestoData!=1 & Y_No_Party_Vote==0 

/* Slovenia */
tab Y_partyfacts_id haveManifesto if BB_Country=="Slovenia" & Y_No_Party_Vote==0 , missing
replace noManifestoData = 1 if inlist(Y_partyfacts_id, 3098, 4714, 2098, 1188, 881, 1037, 318, 3114) // dosent appear in manifesto 
replace noManifestoData = 1 if Y_partyfacts_id==1479 & BB_Year>=1999 // No data in menifesto for this year
replace noManifestoData = 1 if Y_partyfacts_id==1494 & BB_Year>=1999 // No data in menifesto for this year
replace noManifestoData = 1 if Y_partyfacts_id==764 & BB_Year>=2017 // No data in menifesto for this year
replace noManifestoData = 1 if Y_partyfacts_id==1773 & BB_Year>=2017 // No data in menifesto for this year
replace noManifestoData = 1 if inlist(Y_Party_Vote, "SocialDemocraticRenewal_SVN", "SocialDemocraticUnion_SVN") // dosent appear in manifesto

/* Test for Slovenia */
tab Y_partyfacts_id if BB_Country=="Slovenia" & !haveManifesto & Y_No_Party_Vote==0 & noManifestoData!=1, missing
count if BB_Country=="Slovenia" & noManifestoData==1 & Y_No_Party_Vote==0 
count if BB_Country=="Slovenia" & haveManifesto & noManifestoData!=1 & Y_No_Party_Vote==0 

/* Romania */
tab Y_partyfacts_id haveManifesto if BB_Country=="Romania" & Y_No_Party_Vote==0 , missing
replace noManifestoData = 1 if inlist(Y_partyfacts_id, 296, 645, 967, 1443, 1541, 1579, 1590, 2473) // dosent appear in manifesto 
replace noManifestoData = 1 if inlist(Y_partyfacts_id, 1034, 2534) // dosent appear in manifesto for the relevant years
replace noManifestoData = 1 if Y_partyfacts_id==1217 & BB_Year>=1998 // No data in menifesto for this years	
replace noManifestoData = 1 if Y_partyfacts_id==1305 & BB_Year>=2012 // No data in menifesto for this years	
replace noManifestoData = 1 if Y_partyfacts_id==1373 & BB_Year>=1999 // No data in menifesto for this years	
replace noManifestoData = 1 if Y_partyfacts_id==1715 & BB_Year>=2005 // No data in menifesto for this years	
replace noManifestoData = 1 if Y_partyfacts_id==1750 & BB_Year>=1998 // No data in menifesto for this years	
replace noManifestoData = 1 if Y_partyfacts_id==5662 & BB_Year<=1998 // No data in menifesto for this years	
replace noManifestoData = 1 if inlist(Y_Party_Vote, "AnyParty_ROU", "FreeChange_ROU", "Germans_ROU", "Liberal_ROU", "MovementRomania_ROU", "NationalLiberalYoung_ROU", "RomanianNational_ROU", "RadicalLiberalism_ROU", "Rroma_ROU") // dosent appear in manifesto

/* Test for Romania */
tab Y_partyfacts_id if BB_Country=="Romania" & !haveManifesto & Y_No_Party_Vote==0 & noManifestoData!=1, missing
count if BB_Country=="Romania" & noManifestoData==1 & Y_No_Party_Vote==0 
count if BB_Country=="Romania" & haveManifesto & noManifestoData!=1 & Y_No_Party_Vote==0 

/* Lithuania */
tab Y_partyfacts_id haveManifesto if BB_Country=="Lithuania" & Y_No_Party_Vote==0 , missing
replace noManifestoData = 1 if inlist(Y_partyfacts_id, 51, 105, 168, 330, 612, 776, 1026, 1197, 1407, 1409, 1410, 1747, 5455) // dosent appear in manifesto 
replace noManifestoData = 1 if Y_partyfacts_id==377 & BB_Year>=1999 // No data in menifesto for this years	
replace noManifestoData = 1 if Y_partyfacts_id==753 & BB_Year>=1999 // No data in menifesto for this years	
replace noManifestoData = 1 if Y_partyfacts_id==1357 & BB_Year>=2018 // No data in menifesto for this years- Need to check with Roee about this party.
replace noManifestoData = 1 if Y_partyfacts_id==1800 & BB_Year>=2008 // No data in menifesto for this years- Need to check with Roee about this party.
replace noManifestoData = 1 if inlist(Y_Party_Vote, "CitizensAlliance_LTU", "Independence_LTU", "Industrial_LTU", "Justice_LTU", "LogicOfLife_LTU")
replace noManifestoData = 1 if inlist(Y_Party_Vote, "List_LTU", "MovementElections_LTU", "PeopleParty_LTU", "Republican_LTU", "Socialist_LTU") // dosent appear in manifesto

/* Test for Lithuania */
tab Y_partyfacts_id if BB_Country=="Lithuania" & !haveManifesto & Y_No_Party_Vote==0 & noManifestoData!=1, missing
count if BB_Country=="Lithuania" & noManifestoData==1 & Y_No_Party_Vote==0 
count if BB_Country=="Lithuania" & haveManifesto & noManifestoData!=1 & Y_No_Party_Vote==0 

/* US */
tab Y_partyfacts_id haveManifesto if BB_Country=="US" & Y_No_Party_Vote==0 , missing
replace noManifestoData = 1 if Y_partyfacts_id==3557 // dosent appear in manifesto 
replace noManifestoData = 1 if inlist(Y_Party_Vote, "Green_USA", "Independent_USA", "Libertarian_USA") // dosent appear in manifesto

/* Test for US */
tab Y_partyfacts_id if BB_Country=="US" & !haveManifesto & Y_No_Party_Vote==0 & noManifestoData!=1, missing
count if BB_Country=="US" & noManifestoData==1 & Y_No_Party_Vote==0 
count if BB_Country=="US" & haveManifesto & noManifestoData!=1 & Y_No_Party_Vote==0 


/********************************************************
******* ADD GREEN PARTIES*******
***************************************************************/

merge m:1 Y_manifesto_key using "$POPULISM_DROPBOX/data/cleandata/manifesto/manifesto.dta", keepusing(Y_manifesto_key parfam) keep(match master) nogen

/* Define green party for main model, 10=="eco ecologist"*/
gen Y_Green = parfam==10
drop parfam



save "$POPULISM_DROPBOX/data/intdata/temp/ivs_with_manifesto_key.dta", replace




/********************************************************
******* TESTS *******
***************************************************************/

if (0) {
	/* Test results clean countries */
	use "$POPULISM_DROPBOX/data/intdata/temp/ivs_with_manifesto_key.dta", clear
	

	//Check match manually 
	//keep Y_Party_Vote_Long manifesto_partyname 
	//duplicates drop 


	/* All obs -> asked party -> provided party answer -> have party manifesto -> have party and relevant year manifesto*/
	count
	count if !missing(BB_Party) & BB_Party!="Not asked in survey"
	count if !missing(BB_Party) & Y_No_Party_Vote==0
	count if !missing(Y_manifesto_key) 
	
	// For first seven countries have 31.8k / 37.5k 
	count if Country_Cleaned
	count if !missing(BB_Party) & Y_No_Party_Vote==0 & Country_Cleaned
	count if !missing(Y_manifesto_key)  & Country_Cleaned
	
	gen haveManifesto = !missing(Y_manifesto_key)
	tab BB_Country haveManifesto if Country_Cleaned & Y_No_Party_Vote==0
	
	tab Y_partyfacts_id if BB_Country=="Netherlands" & !haveManifesto & Y_No_Party_Vote==0
	tab Y_Party_Vote_Long if BB_Country=="Netherlands" & !haveManifesto & Y_No_Party_Vote==0
	tab Y_manifesto_key if Y_partyfacts_id==960, missing
	tab BB_Year if Y_partyfacts_id==960, missing
	

	egen countPartyYearWave = count(BB_Wave ), by(BB_Country BB_Year Y_Party_Vote_Long)
	gen Have_Manifesto= !missing(keyManifestoWVS)
	keep BB_Country BB_Year Y_Party_Vote_Long Have_Manifesto countPartyYearWave
	duplicates drop
	drop if Y_Party_Vote_Long=="."
	gsort BB_Country BB_Year -Have_Manifesto -countPartyYearWave 


	/* Test results all countries */
	use "$POPULISM_DROPBOX/data/intdata/temp/ivs_with_manifesto_key.dta", clear

	/* Keeping only WVS because can't match manifesto manually*/
	keep if  BB_Wave_WVS==1
	
	

	/* All obs -> asked party -> provided party answer -> have party manifesto -> have party and relevant year manifesto*/
	count 
	count if !missing(BB_Party) & BB_Party!="Not asked in survey" 
	count if !missing(BB_Party) & Y_No_Party_Vote==0  // Can also use: count if Y_Party_Vote_Disagg!="." & BB_Country=="UK"
	count if !missing(Y_manifesto_key) 

	/*Looking at only Manifesto countries*/
	count if BB_countryInManifesto == 1
	count if !missing(BB_Party) & BB_Party!="Not asked in survey"  & BB_countryInManifesto == 1
	count if !missing(BB_Party) & Y_No_Party_Vote==0 & BB_countryInManifesto == 1
	count if !missing(Y_manifesto_key) & BB_countryInManifesto == 1

	/*Looking at ESM countries*/
	count if BB_countryInESM == 1 & BB_countryInManifesto == 1
	count if !missing(BB_Party) & BB_Party!="Not asked in survey"  & BB_countryInESM == 1 & BB_countryInManifesto == 1
	count if !missing(BB_Party) & Y_No_Party_Vote==0 & BB_countryInESM == 1 & BB_countryInManifesto == 1
	count if !missing(Y_manifesto_key) & BB_countryInESM == 1 & BB_countryInManifesto == 1

	/*Looking at advanced democracies*/
	count if BB_countryAdvancedDem == 1 & BB_countryInManifesto == 1
	count if !missing(BB_Party) & BB_Party!="Not asked in survey"  & BB_countryAdvancedDem == 1 & BB_countryInManifesto == 1
	count if !missing(BB_Party) & Y_No_Party_Vote==0 & BB_countryAdvancedDem == 1 & BB_countryInManifesto == 1
	count if !missing(Y_manifesto_key) & BB_countryAdvancedDem == 1 & BB_countryInManifesto == 1
	
}

