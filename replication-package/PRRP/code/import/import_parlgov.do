clear

/**********************
******* Save Parlgov IDs *******
**********************/

/* Create table partyFact -> Parlgov*/
import delimited "$POPULISM_DROPBOX/data/rawdata/downloaded/public/partyfacts/partyfacts-external-parties.csv", numericcols(3) clear

/* Only keep parlgov to match parlgov with partyfact*/
drop if dataset_key != "parlgov"
rename (dataset_party_id  partyfacts_id name name_english)  (Y_parlgov_id Y_partyfacts_id partyfact_name partyfact_name_english)

/* Verify that there is one unqiue identifier for parlgov */
distinct Y_parlgov_id
assert r(ndistinct)==_N

 /* Dropping cases where no party fact*/
egen countPartyfact = count(Y_partyfacts_id), by(Y_partyfacts_id)
drop if countPartyfact == 0

tab country if countPartyfact >1
list name_short year_first year_last Y_partyfacts_id Y_parlgov_id if countPartyfact>1 & country=="ISR"

/* Germany has multiple parties for Y_partyfacts_id 2702. Keeping 1 now and will fix Y_parlgov_id when merging*/
drop if inlist(Y_parlgov_id, 2703, 1235, 2704)

/* HRV has multiple parties for Y_partyfacts_id 2524. Keeping 1 now and will fix Y_parlgov_id when merging*/
drop if inlist(Y_parlgov_id, 2771)

/* HUN has multiple parties for Y_partyfacts_id 859. Keeping 1 now and will fix Y_parlgov_id when merging*/
drop if inlist(Y_parlgov_id, 2659)

/* France has two parties with the Y_partyfacts_id. Keeping 1 that appears in cabinet dataset*/
drop if inlist(Y_parlgov_id, 2400)

/* GBR has three parties with the Y_partyfacts_id. Keeping 1 that appears in our time period */
drop if inlist(Y_parlgov_id, 1496, 2719)

/* IRL has two parties with the Y_partyfacts_id 2735. Keeping 1 now and will fix 
Y_parlgov_id when merging */
drop if inlist(Y_parlgov_id, 2328)

/* ISR has two parties with the Y_partyfacts_id 3148. Keeping 1 now and will fix Y_parlgov_id when merging */
drop if inlist(Y_parlgov_id, 1862)

/* ITA has two parties with the Y_partyfacts_id 851. Keeping 1 now and will fix Y_parlgov_id when merging */
drop if inlist(Y_parlgov_id, 1304)

/* JPN has two parties with the Y_partyfacts_id 2750. Keeping 1 now and will fix Y_parlgov_id when merging */
drop if inlist(Y_parlgov_id, 163)

/* LTU has two parties with the Y_partyfacts_id 64. Keeping 1 now and will fix Y_parlgov_id when merging */
drop if inlist(Y_parlgov_id, 1408)

/* SWE has two parties with the Y_partyfacts_id 2936. Keeping 1 that appears in our time period */
drop if inlist(Y_parlgov_id, 2406)

/*****************************
**************DUPLICATES *****
******************************/

 /* Drop remaining duplicates, besides Cyprus, only countries left are outside our intended sample*/
 drop countPartyfact 
 egen countPartyfact = count(Y_partyfacts_id), by(Y_partyfacts_id)
 tab country if countPartyfact>1
 tab partyfact_name_english if countPartyfact>1
 drop if countPartyfact>1

save "$POPULISM_DROPBOX/data/intdata/partyfacts/external_parlgov.dta", replace

