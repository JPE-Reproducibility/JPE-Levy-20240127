
/***************** *********
***MANUAL WVS -> PARTY_FACT *******
****************************/


/* First 
Notes: Greece course of freedom only appears in one dataset (parlgov) combined with another party so not adding*/
import delimited "$POPULISM_DROPBOX/data/rawdata/authorcreated/merge/cleanedPartiesToMatchManually_ivs.csv", clear varnames(1)

capture rename ïy_party_vote_long y_party_vote_long 
rename y_party_vote_long Y_Party_Vote_Long
rename partyfacts_id partyfacts_id_manual
drop if partyfacts_id_manual==.

duplicates report Y_Party_Vote_Long
duplicates list Y_Party_Vote_Long
duplicates drop
duplicates report Y_Party_Vote_Long
assert `r(unique_value)'==_N

save "$POPULISM_DROPBOX/data/intdata/partyfacts/ivs_partyfact_manual.dta", replace



/***************** *********
***IMPORT PARTY_FACT *******
****************************/

/* Create tables: Automatic WVS -> PartyFact (using numericcols(3) to ensure dataset_party_id is a float for the merge)*/
import delimited "$POPULISM_DROPBOX/data/rawdata/downloaded/public/partyfacts/partyfacts-external-parties.csv", numericcols(3) clear
keep if dataset_key == "wvs"
keep dataset_party_id external_id partyfacts_id 
rename dataset_party_id WVS_PartyFact_ID

save "$POPULISM_DROPBOX/data/intdata/partyfacts/ivs_partyfact_auto.dta", replace


/* Create table partyFact -> Manifesto*/
import delimited "$POPULISM_DROPBOX/data/rawdata/downloaded/public/partyfacts/partyfacts-external-parties.csv", numericcols(3) clear

/* Only keep manifesto to match manifesto with partyfact*/
drop if dataset_key != "manifesto"
rename dataset_party_id Y_manifesto_party_id

/* Verify that there is one unqiue identifier for manifesto*/
distinct Y_manifesto_party_id
assert r(ndistinct)==_N

 
 /* Dropping cases where no party fact*/
egen countPartyfact = count(partyfacts_id), by(partyfacts_id)
drop if countPartyfact == 0


/*****************************
**************DUPLICATES *****
******************************/

/* Sometimes one partyfact id has multiple manifesto, for example for FiDez
 list name_english year_first year_last partyfacts_id Y_manifesto_party_id if partyfacts_id==1691
  tab country  if countPartyfact>1
 list name_short year_first year_last partyfacts_id Y_manifesto_party_id if countPartyfact>1 & country=="SVN"
 For this section I also opeened the manifesto raw data to see what exists
  */
 
/* Hungaria: Keeping only relevant party in (others don't appear in manifesto) */
drop if partyfacts_id==1691 & Y_manifesto_party_id!=86421
 
/* Belgium: Two party facts in Keep one and fix second when merging for specific years*/
drop if Y_manifesto_party_id==21915 | Y_manifesto_party_id==21425
 
/* Bulgaria: One party where manifesto_id changes by year*/
drop if Y_manifesto_party_id==80229 | Y_manifesto_party_id==80228
 
/* CZE: One party where manifesto_id changes by year*/
drop if Y_manifesto_party_id==82529
 
/* Germany: One party in where manifesto_id changes by year
Also dropping East Germany party since not analyzing that country */
drop if Y_manifesto_party_id==41222
drop if partyfacts_id==2699
 
/* Spain: Removing both parties in Spain, 2538 is matched to two different parties, will be fixed in merge, 4019 doesn't seem to exist in our data*/
drop if partyfacts_id==2538 | partyfacts_id==4019
  
/* Estonia; One party in where manifesto_id changes by year*/
drop if Y_manifesto_party_id==83220 | Y_manifesto_party_id==83709
 
/* UK: Drop party before 1990*/
drop if Y_manifesto_party_id==52620
 
/* Greece: One party where manifesto_id changes by year*/
drop if Y_manifesto_party_id==34212

/* Croatia: Many parties where manifesto_id changes by year
  Three parties for party 2524 but all 1990 when we don't have Croatia data*/
drop if inlist(Y_manifesto_party_id, 81230, 81042, 81091, 81041, 81043, 81021, 81223)
drop if partyfacts_id==2524

/* Italy: partyfact 2415 (Italians in the World, Labour and Freedom List, and one other list) not in our data, drop*/
drop if partyfacts_id==2415

/* Lithuania two cases where year changed*/
drop if Y_manifesto_party_id==88460 | Y_manifesto_party_id==88621

/* Lux: dropping pre 1990 party */
drop if Y_manifesto_party_id==23112

/* Latvia: one party where year changed*/
drop if Y_manifesto_party_id==87421

/* Slovenia: one party where year changed, other only name changed, doesn't matter*/
drop if name_short=="SDS" |  Y_manifesto_party_id==97322

 /* TODO: Cyprus has two manifestos for partyfacts_id 321, not clear which should be used 
 Skipping all countries that are no in our list */

/* Drop remaining duplicates, besides Cyprus, only countries left are outside our intended sample*/
drop countPartyfact 
egen countPartyfact = count(partyfacts_id), by(partyfacts_id)
tab country if countPartyfact>1
drop if countPartyfact>1
 
rename (partyfacts_id name name_english)  (Y_partyfacts_id partyfact_name partyfact_name_english)

save "$POPULISM_DROPBOX/data/intdata/partyfacts/external_manifesto_ivs.dta", replace

preserve
keep Y_partyfacts_id Y_manifesto_party_id
save "$POPULISM_DROPBOX/data/intdata/temp/partyfact_manifesto_crosswork.dta", replace
restore

ren (Y_partyfacts_id Y_manifesto_party_id) (Y_2nd_partyfacts_id Y_manifesto_2nd_party_id)

save "$POPULISM_DROPBOX/data/intdata/partyfacts/external_2nd_manifesto_ivs.dta", replace

ren (Y_2nd_partyfacts_id Y_manifesto_2nd_party_id) (Y_Least_partyfacts_id Y_manifesto_Least_party_id)

save "$POPULISM_DROPBOX/data/intdata/partyfacts/external_Least_manifesto_ivs.dta", replace

* Save additional data
use "$POPULISM_DROPBOX/data/intdata/partyfacts/ivs_partyfact_manual.dta", clear

ren (Y_Party_Vote_Long partyfacts_id_manual) (Y_2nd_Party_Vote_Long partyfacts_2nd_id_manual)

save "$POPULISM_DROPBOX/data/intdata/partyfacts/ivs_2nd_partyfact_manual.dta", replace

ren (Y_2nd_Party_Vote_Long partyfacts_2nd_id_manual) (Y_Least_Party_Vote_Long partyfacts_Least_id_manual)

save "$POPULISM_DROPBOX/data/intdata/partyfacts/ivs_Least_partyfact_manual.dta", replace

use "$POPULISM_DROPBOX/data/intdata/partyfacts/ivs_partyfact_auto.dta", clear

ren (WVS_PartyFact_ID external_id partyfacts_id) (WVS_2nd_PartyFact_ID external_2nd_id partyfacts_2nd_id)

save "$POPULISM_DROPBOX/data/intdata/partyfacts/ivs_2nd_partyfact_auto.dta", replace

ren (WVS_2nd_PartyFact_ID external_2nd_id partyfacts_2nd_id) (WVS_Least_PartyFact_ID external_Least_id partyfacts_Least_id) 

save "$POPULISM_DROPBOX/data/intdata/partyfacts/ivs_Least_partyfact_auto.dta", replace
