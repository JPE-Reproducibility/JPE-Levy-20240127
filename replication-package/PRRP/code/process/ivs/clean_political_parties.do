/* Purpose: generate political party variables. Define several variables
	noPartyVote - a vote for no party is defined
	Y_Party_Vote - the party the respondent support
	Y_Party_Vote_Long - full party name with country
	Note that these variables are different than BB_Party which often has two similar strings for the same party
 */

clear all 

//use "$POPULISM_DROPBOX/data/intdata/wvs/wvs_evs_all_waves.dta", clear

use "$POPULISM_DROPBOX/data/intdata/ivs/ivs_waves", clear

/**********************************
******* TYPICALLY VOTE OUTCOME VARS ***
**********************************/
//Vote in elections: Local level
//Codebook E263
decode E263, gen (temp_vote_local)
gen Y_typically_vote_local = .
replace Y_typically_vote_local = 0 if temp_vote_local == "Never"
replace Y_typically_vote_local = 1 if temp_vote_local == "Usually"
replace Y_typically_vote_local = 2 if temp_vote_local == "Always"

//Vote in elections: National level
//Codebook E264
decode E264, gen (temp_vote_national)
gen Y_typically_vote_national = .
replace Y_typically_vote_national = 0 if temp_vote_national == "Never"
replace Y_typically_vote_national = 1 if temp_vote_national == "Usually"
replace Y_typically_vote_national = 2 if temp_vote_national == "Always"

/**********************************
******* FIND VOTES FOR NO PARTY ***
**********************************/

gen Y_Party_Vote = "."
gen Y_Party_Vote_Long = "."
gen Y_2nd_Party_Vote = "."
gen Y_2nd_Party_Vote_Long = "."
gen Y_Least_Party_Vote = "."
gen Y_Least_Party_Vote_Long = "."

global prefix_BB_Party "_"
global prefix_BB_2nd_Party "_2nd_"
global prefix_BB_Least_Party "_Least_"

/* Ignore the parties do not appear in BB_Party but if BB_Second_Party or BB_Least_Party*/
replace BB_2nd_Party = "Missing: minor party" if BB_2nd_Party == "ESP: Independent Canary Association"|BB_2nd_Party == "ESP: United Extremadura"|BB_2nd_Party == "ESP: Canary Nationalist Party"|BB_2nd_Party == "ESP: Majorera Assembly"|BB_2nd_Party == "ESP: Independent Liberal Group"|BB_2nd_Party == "ESP: Alavesa unity"
replace BB_Least_Party = "Missing: minor party" if BB_Least_Party == "ESP: Independent Canary Association"|BB_Least_Party == "ESP: United Extremadura"|BB_Least_Party == "ESP: Canary Nationalist Party"|BB_Least_Party == "ESP: Majorera Assembly"|BB_Least_Party == "ESP: Independent Liberal Group"|BB_Least_Party == "ESP: Alavesa unity"
replace BB_2nd_Party = "Missing: minor party" if BB_2nd_Party == "BGR: Labor Party"
replace BB_Least_Party = "Missing: minor party" if BB_Least_Party == "BGR: Labor Party"
replace BB_2nd_Party = "Missing: minor party" if BB_2nd_Party == "SVK: The new agrarian party"
replace BB_Least_Party = "Missing: minor party" if BB_Least_Party == "SVK: The new agrarian party"

foreach party in BB_Party BB_2nd_Party BB_Least_Party {
    if "`party'" == "BB_Party" {
        local i "$prefix_BB_Party"
    }
    else if "`party'" == "BB_2nd_Party" {
        local i "$prefix_BB_2nd_Party"
    }
    else if "`party'" == "BB_Least_Party" {
        local i "$prefix_BB_Least_Party"
    }
    
    gen Y_No`i'Party_Vote = inlist(`party', "Don´t know", "I would cast a blank ballot; white vote", "Null vote", "Not applicable", "None", "No answer", "Missing; Unknown", "Don't know", "Other")
    replace Y_No`i'Party_Vote = 1 if inlist(`party', "I would not vote", "No right to vote", "Not asked in survey", "Missing: other", "Missing: minor party")
    replace Y_No`i'Party_Vote = 1 if inlist(`party', "HU: undocumented")
    replace Y_No`i'Party_Vote = 1 if missing(`party')

	/* Use regular expressions to find no party vote*/
    replace Y_No`i'Party_Vote = 1 if regexm(`party', ".*Other.*")
    replace Y_No`i'Party_Vote = 1 if regexm(`party', ".*no other.*")
    replace Y_No`i'Party_Vote = 1 if regexm(lower(`party'), ".*blank.*")
    replace Y_No`i'Party_Vote = 1 if regexm(lower(`party'), ".*know*")
    replace Y_No`i'Party_Vote = 1 if regexm(lower(`party'), ".*vote*")
    replace Y_No`i'Party_Vote = 1 if regexm(lower(`party'), ".*ballot*")
    replace Y_No`i'Party_Vote = 1 if regexm(lower(`party'), ".*spontaneous*")

	/* These parties are included in the search above, fix manually*/
    replace Y_No`i'Party_Vote = 0 if regexm(lower(`party'), ".*duchodci*")
    replace Y_No`i'Party_Vote = 0 if regexm(lower(`party'), ".*free*")

	
    /* Use this code to verify that not missing any additional parties:
    //tab BB_Party if noPartyVote == 1 */
    gen Y`i'Dont_Know = inlist(`party', "Don´t know", "Don't know")
    replace Y`i'Dont_Know = 1 if regexm(lower(`party'), ".*know*") & !regexm(lower(`party'), ".*unknown*")

    gen Y`i'None = inlist(`party', "I would cast a blank ballot; white vote", "Null vote", "None", "I would not vote")
    replace Y`i'None = 1 if regexm(`party', ".*no other.*")
    replace Y`i'None = 1 if regexm(lower(`party'), ".*blank.*")
    replace Y`i'None = 1 if regexm(lower(`party'), ".*vote*")
    replace Y`i'None = 1 if regexm(lower(`party'), ".*ballot*")
    replace Y`i'None = 1 if regexm(lower(`party'), ".*spontaneous*")

    replace Y`i'None = 0 if regexm(lower(`party'), ".*free*")
    replace Y`i'None = 0 if regexm(lower(`party'), ".*duchodci*")
    replace Y`i'None = 0 if regexm(lower(`party'), ".*right*")

    gen Y_Other`i'Party = 1 if inlist(`party', "Other")
    replace Y_Other`i'Party = 1 if regexm(`party', ".*Other.*")

	// Commenting since causes issues in AWS
    //table BB_Party if Y_Dont_Know==1
    //table BB_Party if Y_None==1
    //table BB_Party if Y_Other_Party==1
    //table BB_Party if Y_No_Party_Vote==1
    sum Y_No`i'Party_Vote if Y`i'Dont_Know ==1 | Y`i'None ==1 | Y_Other`i'Party ==1
    assert r(mean)==1
}

//gen Y_Party_Vote_Disagg = "."

/**********************************
******* GERMANY ********************
**********************************/
timer clear 1
timer on 1

foreach party in BB_Party BB_2nd_Party BB_Least_Party {
    if "`party'" == "BB_Party" {
        local i "$prefix_BB_Party"
    }
    else if "`party'" == "BB_2nd_Party" {
        local i "$prefix_BB_2nd_Party"
    }
    else if "`party'" == "BB_Least_Party" {
        local i "$prefix_BB_Least_Party"
    }
    	
// Generating Political Party Dummy Variables for Germany
	replace Y`i'Party_Vote = "CSU_GER" if `party' == "DE: Christian Democratic Party/Christian Social Union" | `party' == "DE: Christlich-Demokratische Union/ Christlich-Soziale Union" | `party' == "DE: Christlich-Demokratische Union/ Christlich-Soziale Union (CDU/CSU) - Christian Democrats" | `party' == "DE: Deutsche Soziale Union (DSU) (linked with CSU)" | `party' == "DEU: Christian Social Union in Bavaria"
	replace Y`i'Party_Vote_Long = "DEU: Christian Democratic Party/Christian Social Union" if Y`i'Party_Vote == "CSU_GER"

	replace Y`i'Party_Vote = "SPD_GER" if `party' == "DE: German Social-Democratic Party" |`party' == "DE: Sozialdemokratische Partei Deutschlands (SPD) - Social D" | `party' == "DE: Sozialdemokratische Partei Deutschlands (SPD) - Social Democrats" | `party' == "DEU: Social Democratic Party"
	replace Y`i'Party_Vote_Long = "DEU: Social Democratic Party" if Y`i'Party_Vote == "SPD_GER"

	/* Using a regular expression because there seems to be an extra character at the end of Free Democratic Party*/
	replace Y`i'Party_Vote = "FDP_GER" if `party' == "DE: Freie Demokratische Partei Deutschlands (FDP) - Free Dem" | `party' == "DE: Freie Demokratische Partei Deutschlands (FDP) - Free Democratic Party" | `party' == "DE: German Liberal Party"  | regexm(`party', ".*DEU: Free Democratic Party.*") // Note that "German Liberal Party" and the "Free Democratic Party" are the same thing.
	replace Y`i'Party_Vote_Long = "DEU: Free Democratic Party" if Y`i'Party_Vote == "FDP_GER"

	// Until 1990 Green considered a different party in Manifesto
	replace Y`i'Party_Vote = "Green_GER" if (`party' == "DE: Die Gruenen (/Buendnis 90)" | `party' == "DE: The Green Party" | `party' == "DEU: Alliance 90/The Greens") & BB_Year>1990
	replace Y`i'Party_Vote_Long = "DEU: The Green Party" if Y`i'Party_Vote == "Green_GER"

	replace Y`i'Party_Vote = "Greens/90_GER" if `party' == "DE: The Green Party" & BB_Year==1990
	replace Y`i'Party_Vote_Long = "DEU: Greens/90_GER" if Y`i'Party_Vote == "Greens/90_GER"

	replace Y`i'Party_Vote = "FreeVoters_GER" if `party' == "DE: Freie Waehler - Free Voters" | `party' == "DEU: Free Voters"
	replace Y`i'Party_Vote_Long = "DEU: Free Voters Party" if Y`i'Party_Vote == "FreeVoters_GER"

	replace Y`i'Party_Vote = "GrayPanthers_GER" if `party' == "DE: Graue Panther - Grey Panthers" | `party' == "DEU: The Greys – Grey Panthers"
	replace Y`i'Party_Vote_Long = "DEU: Gray Panthers" if Y`i'Party_Vote == "GrayPanthers_GER"

	replace Y`i'Party_Vote = "EcoDem_GER" if `party' == "DE: Oekologisch Demokratische Partei (OEDP) - Ecological Dem" | `party' == "DEU: Ecological Democratic Party"
	replace Y`i'Party_Vote_Long = "DEU: Ecological Democratic Party" if Y`i'Party_Vote == "EcoDem_GER"

	replace Y`i'Party_Vote = "Republikaner_GER" if `party' == "DE: Die Republikaner (REP)" | `party' == "DEU: The Republicans"
	replace Y`i'Party_Vote_Long = "DEU: The Republicans" if Y`i'Party_Vote == "Republikaner_GER"

	// Also known as "Tierschutzpartei" in Germany
	replace Y`i'Party_Vote = "AnimalParty_GER" if `party' == "DE: Tierschutzpartei" | `party' == "DEU: Animal Welfare Party"
	replace Y`i'Party_Vote_Long = "DEU: Animal Welfare Party" if Y`i'Party_Vote == "AnimalParty_GER"

	// Also known as "Piraten" Party in Germany 
	replace Y`i'Party_Vote = "Pirates_GER" if `party' == "DE: PIRATEN" | `party' == "DEU: German Pirate Party"
	replace Y`i'Party_Vote_Long = "DEU: Pirate Party" if Y`i'Party_Vote == "Pirates_GER"

	replace Y`i'Party_Vote = "AfD_GER" if `party' == "DE: AfD/Alternative fuer Deutschland" | `party' == "DE: Alternative for Germany" | `party' == "DEU: Alternative for Germany" | regexm(`party', "DEU: Alternative for Germany.*")
	replace Y`i'Party_Vote_Long = "DEU: Alternative for Germany (AfD)" if Y`i'Party_Vote == "AfD_GER"

	// The Left-PDS replaced the party of democratic socialism in 2005 and a new party called the Left completely replaced then in 2007, but still receives a lot of support later. Using the name of the new parties 
	replace Y`i'Party_Vote = "PDS_GER" if BB_Year<2005 & inlist(`party', "DE: Partei des demokratischen Sozialismus (PDS)", "DEU: Democratic Socialism Party")
	replace Y`i'Party_Vote = "Left_PDS_GER" if 2005<BB_Year & BB_Year<2007 & inlist(`party', "DE: Partei des demokratischen Sozialismus (PDS)", "DEU: Democratic Socialism Party")
	replace Y`i'Party_Vote = "Left_GER" if `party' == "DE: The Left" | `party' == "DEU: The Left" | (2007<=BB_Year & inlist(`party', "DE: Partei des demokratischen Sozialismus (PDS)", "DEU: Democratic Socialism Party"))

	replace Y`i'Party_Vote_Long = "DEU: Democratic Socialism Party" if Y`i'Party_Vote == "PDS_GER"
	replace Y`i'Party_Vote_Long = "DEU: The Left. Party of Democratic Socialism" if Y`i'Party_Vote == "Left_PDS_GER"
	replace Y`i'Party_Vote_Long = "DEU: The Left Party" if Y`i'Party_Vote == "Left_GER"

	replace Y`i'Party_Vote = "LibDem_GER" if `party' == "DE: Liberal-Demokratische Partei - Liberal-Democratic Party" // Note - this party dissolved in 1990 and merged into the Free Democratic Party (FDP)
	replace Y`i'Party_Vote_Long = "DEU: Liberal-Democratic Party" if Y`i'Party_Vote == "LibDem_GER"

	replace Y`i'Party_Vote = "NewForum_GER" if `party' == "DE: Neues Forum - New Forum" // This movement merged with Democracy Now and one other party to form Alliance 90, which later became the Green Party (Source: https://en.wikipedia.org/wiki/Alliance_90)
	replace Y`i'Party_Vote_Long = "DEU: New Forum" if Y`i'Party_Vote == "NewForum_GER"

	replace Y`i'Party_Vote = "Drivers_GER" if `party'=="DEU: German Party of Drivers and Citizens' Interests"
	replace Y`i'Party_Vote_Long = "DEU: Party of Drivers and Citizens Interests" if Y`i'Party_Vote == "Drivers_GER"

	replace Y`i'Party_Vote = "Bible_GER" if `party' == "DEU: Party of Bible-abiding Christians"
	replace Y`i'Party_Vote_Long = "DEU: Party of Bible-abiding Christians" if Y`i'Party_Vote == "Bible_GER"

	replace Y`i'Party_Vote = "Pink_GER" if `party' == "DEU: Pink List"
	replace Y`i'Party_Vote_Long = "DEU: Pink List" if Y`i'Party_Vote == "Pink_GER"

	replace Y`i'Party_Vote = "DemNow_GER" if `party' == "DE: Demokratie jetzt - Democracy Now"
	replace Y`i'Party_Vote_Long = "DEU: Democracy Now" if Y`i'Party_Vote == "DemNow_GER"

	replace Y`i'Party_Vote = "DemBreakthrough_GER" if `party' == "DE: Demokratischer Aufbruch - Democratic Breakthrough"
	replace Y`i'Party_Vote_Long = "DEU: Democratic Breakthrough" if Y`i'Party_Vote == "DemBreakthrough_GER"

	replace Y`i'Party_Vote = "Communist_GER" if `party' == "DE: Deutsche Kommunistische Partei (DKP) - Communist Party of Germany"| `party' == "DEU: Communist Party of Germany"
	replace Y`i'Party_Vote_Long = "DEU: Communist Party" if Y`i'Party_Vote == "Communist_GER"

	//NPD and DVU tried to merge in 2010 though Munich court declared it void, they pledged not to compete in elections since 2004
	replace Y`i'Party_Vote = "National_GER" if `party' == "DE: German National Party - NPD" | `party' == "DE: Nationaldemokratische Partei Deutschlands (NPD)" |  `party' == "DEU: National Democratic Party of Germany" 
	replace Y`i'Party_Vote_Long = "DEU: German National Party NPD" if Y`i'Party_Vote == "National_GER"

	replace Y`i'Party_Vote = "People's_GER" if `party' == "DE: German People´s Party - DVU"
	replace Y`i'Party_Vote_Long = "DEU: German People's Party DVU" if Y`i'Party_Vote == "People's_GER"

	replace Y`i'Party_Vote = "REP/NPD/DVU_GER"  if `party' == "DE: Republikaner, NPD oder DVU" | `party' == "DEU: REP, NPD or DVU" | Y`i'Party_Vote == "Republikaner_GER" | Y`i'Party_Vote == "National_GER" | Y`i'Party_Vote == "People's_GER"
	replace Y`i'Party_Vote_Long = "DEU: REP or NPD or DVU" if Y`i'Party_Vote == "REP/NPD/DVU_GER"

	/* Testing Germany */
	/*tab Y`i'Party_Vote_Long if BB_Country=="Germany" & !missing(`party') & noPartyVote==0, missing
	tab `party' if BB_Country=="Germany" & noPartyVote==0 & Y`i'Party_Vote_Long=="."*/

	/* Verfiy that assigned all parties*/
	count if Y`i'Party_Vote_Long=="." & BB_Country=="Germany" & !missing(`party') & Y_No`i'Party_Vote==0
	assert r(N)==0

}

tab BB_Party BB_Year if Y_Party_Vote_Long=="." & BB_Country=="Germany" & !missing(BB_Party) & Y_No_Party_Vote==0
tab BB_2nd_Party BB_Year if Y_2nd_Party_Vote_Long=="." & BB_Country=="Germany" & !missing(BB_2nd_Party) & Y_No_2nd_Party_Vote==0
tab BB_Least_Party BB_Year if Y_Least_Party_Vote_Long=="." & BB_Country=="Germany" & !missing(BB_Least_Party) & Y_No_Least_Party_Vote==0

// Germany takes 9 sec
timer off 1 
timer list 1
display `r(t1)'

/**********************************
******* FRANCE ********************
**********************************/

// Generating Political Party Dummy Variables for France
//Political parties question not asked in WVS waves

gen Y_otherLeftExtreme_FRA = 0

foreach party in BB_Party BB_2nd_Party BB_Least_Party {
    if "`party'" == "BB_Party" {
        local i "$prefix_BB_Party"
    }
    else if "`party'" == "BB_2nd_Party" {
        local i "$prefix_BB_2nd_Party"
    }
    else if "`party'" == "BB_Least_Party" {
        local i "$prefix_BB_Least_Party"
    }
	
	replace Y`i'Party_Vote = "ExtremeLeft_FRA" if BB`i'Party == "FR: Extre-gauche - Ultra Left" | BB`i'Party == "FR: Left Wing Extremist Parties (Revolutionnairy Communist League, Labour Class Combat, Workers Party - PT)" | BB`i'Party == "FR: Left Wing Extremist Parties (New Anticapitalist Party, W" | BB`i'Party == "FR: Left Wing Extremist Parties (New Anticapitalist Party, Workers' Struggle, Independent Workers' Party)"     
	replace Y`i'Party_Vote_Long = "FRA: Left Wing Extremist Parties" if Y`i'Party_Vote == "ExtremeLeft_FRA"

	replace Y`i'Party_Vote = "Unsub_FRA" if BB`i'Party == "FR: Unsubmissive France"
	replace Y`i'Party_Vote_Long = "FRA: Unsubmissive" if Y`i'Party_Vote == "Unsub_FRA"

	replace Y`i'Party_Vote = "Communist_FRA" if BB`i'Party == "FR: Parti Communiste - Communist Party" | BB`i'Party == "FR: Communist Party"
	replace Y`i'Party_Vote_Long = "FRA: Communist" if Y`i'Party_Vote == "Communist_FRA"

	replace Y`i'Party_Vote = "Socialist_FRA" if BB`i'Party == "FR: Parti Socialiste - Socialist Party" | BB`i'Party == " FR: Socialist Party" | regexm(BB`i'Party, ".*FR: Socialist Party.*")
	replace Y`i'Party_Vote_Long = "FRA: Socialist" if Y`i'Party_Vote == "Socialist_FRA"

	replace Y`i'Party_Vote = "Green_FRA" if BB`i'Party == "FR: Les Verts - Green Party" | BB`i'Party == "FR: Europe Ecology – The Greens" | BB`i'Party == "FR: Mouvement Ecologique"
	replace Y`i'Party_Vote_Long = "FRA: The Greens" if Y`i'Party_Vote == "Green_FRA"

	replace Y`i'Party_Vote = "Repub_Onward_FRA" if BB`i'Party == "FR: The Republic Onwards" 
	replace Y`i'Party_Vote_Long = "FRA: Republic Onwards" if Y`i'Party_Vote == "Repub_Onward_FRA"

	replace Y`i'Party_Vote = "Dem_FRA" if BB`i'Party == "FR: Democratic Movement" | BB`i'Party == "FR: UDF" | BB`i'Party == "FR: Union pour la Democratie Francaise (UDF) - Union for the French Democracy" /*This two parties are the same, "Union for French
	Democracy" change the mane to "Democratic Movement" */
	replace Y`i'Party_Vote_Long = "FRA: Democratic Movement" if Y`i'Party_Vote == "Dem_FRA"

	replace Y`i'Party_Vote = "UnionDI_FRA" if BB`i'Party == "FR: Union of Democrats and Independents" // BB`i'Party == "FR: Union pour la Democratie Francais.."
	replace Y`i'Party_Vote_Long = "FRA: Union of Democrats and Independents" if Y`i'Party_Vote == "UnionDI_FRA"

	replace Y`i'Party_Vote = "Act_FRA" if BB`i'Party == "FR: Act – The constructive right"
	replace Y`i'Party_Vote_Long = "FRA: Act - The constructive right" if Y`i'Party_Vote == "Act_FRA"

	replace Y`i'Party_Vote = "Repub_FRA" if BB`i'Party == "FR: The Republicans" | BB`i'Party == "FR: Parti Republicain" | BB`i'Party == "FR: Union for a Popular Mouvement" /*This two prties are the same, "Union for a
	Popular Movement" changed the name to "The Republicans" */
	replace Y`i'Party_Vote_Long = "FRA: Republicans" if Y`i'Party_Vote == "Repub_FRA"

	replace Y`i'Party_Vote = "France_Arise_FRA" if BB`i'Party == "FR: France Arise" 
	replace Y`i'Party_Vote_Long = "FRA: France Arise" if Y`i'Party_Vote == "France_Arise_FRA"

	replace Y`i'Party_Vote = "National_Front_FRA" if BB`i'Party == "FR: Front National" | BB`i'Party ==  "FR: National Front" | BB`i'Party == "FR: Right Wing Extremist Parties (National Front - FN, National Republicain Movement - MNR)" | BB`i'Party == "FR: Le Front National de Bruno Megret - National Front of Bruno Megret" | BB`i'Party == "FR: Le Front National de Jean-Marie le Pen - National Front of Le Pen" // Note - Megret and Le Pen initially were the two heads of the National Front party, but the parties split when they had a bad relationship. Much of the party positions remained the same, however, with Le Pen taking on slightly more extreme right views.
	replace Y`i'Party_Vote_Long = "FRA: National Front" if Y`i'Party_Vote == "National_Front_FRA"

	replace Y`i'Party_Vote = "OtherRight_FRA" if BB`i'Party == "FR: Other Right Wing Extremist Parties (The Patriots, Nation" | BB`i'Party == "FR: Right Wing Extremist Parties (Nat.." |  BB`i'Party == "FR: Other Right Wing Extremist Parties (The Patriots, National Republican Movement)"
	replace Y`i'Party_Vote_Long = "FRA: Other Right Extremist Parties" if Y`i'Party_Vote == "OtherRight_FRA"

	replace Y_otherLeftExtreme_FRA = 1 if BB`i'Party == "FR: Other Left Wing Parties (Radical Leftist Party, Republic" | BB`i'Party == "Other Left Wing Parties (Radical Leftist Party, Republicain's and Citizen's Movement)" | BB`i'Party == "FR: Mouvement des Radiaux de Gauche (MRG) - Left Radical Movement" | BB`i'Party == "FR: Other Left Wing Parties (Radical Leftist Party, Republicain's and Citizen's Movement)" | BB`i'Party == "FR: Other Left Wing Parties (Radical Leftist Party, Republicain's and Citizen's Movement"
	replace Y`i'Party_Vote = "OtherLeftExtreme_FRA" if Y_otherLeftExtreme_FRA == 1
	replace Y`i'Party_Vote_Long = "FRA: Other Left Extremist Parties" if Y_otherLeftExtreme_FRA == 1

	replace Y`i'Party_Vote = "CentreSocDem_FRA" if BB`i'Party == "FR: Centre des Democrates Sociaux (UDF, CDS)" // Note - this party merged with Union for French Democracy to form the Democratic Force in France (Source: https://en.wikipedia.org/wiki/Centre_of_Social_Democrats)
	replace Y`i'Party_Vote_Long = "FRA: Centre des Democrates Sociaux" if Y`i'Party_Vote == "CentreSocDem_FRA"

	replace Y`i'Party_Vote = "LibDemocracy_FRA" if BB`i'Party == "FR: Democratice Liberale - Liberal Democracy" // Note - this party merged with two other parties (not present in survey) to form the Union for a Popular Movement (2002), which later was renamed the Republican Party (2015). (Source: https://en.wikipedia.org/wiki/Union_for_a_Popular_Movement)
	replace Y`i'Party_Vote_Long = "FRA: Liberal Democracy" if Y`i'Party_Vote == "LibDemocracy_FRA"

	replace Y`i'Party_Vote = "HuntFish_FRA"  if BB`i'Party == "FR: Hunting, Fishing, Nature, and Tradition"
	replace Y`i'Party_Vote_Long = "FRA: Hunting, Fishing, Nature, and Tradition" if Y`i'Party_Vote == "HuntFish_FRA" 

	replace Y`i'Party_Vote = "MvmtForFrance_FRA" if BB`i'Party == "FR: Movement for France"
	replace Y`i'Party_Vote_Long = "FRA: Movement for France" if Y`i'Party_Vote == "MvmtForFrance_FRA"

	replace Y`i'Party_Vote = "Centrists_FRA" if BB`i'Party == "FR: New Centrist Party"
	replace Y`i'Party_Vote_Long = "FRA: New Centrist Party" if Y`i'Party_Vote == "Centrists_FRA"

	replace Y`i'Party_Vote = "Assembly_FRA" if BB`i'Party == "FR: Rassemblement Pour la Republique - Assembly for the Republic"
	replace Y`i'Party_Vote_Long = "FRA: Assembly for the Republic" if Y`i'Party_Vote == "Assembly_FRA"

	/* Testing Framce*/
	tab Y`i'Party_Vote_Long if BB_Country=="France" & !missing(BB`i'Party) & Y_No`i'Party_Vote==0, missing
	tab BB`i'Party if BB_Country=="France" & Y_No`i'Party_Vote==0 & Y`i'Party_Vote_Long=="."

	count if Y`i'Party_Vote_Long=="." & BB_Country=="France" & !missing(BB`i'Party) & Y_No`i'Party_Vote==0
	assert r(N)==0

}

tab BB_Party BB_Year if Y_Party_Vote_Long=="." & BB_Country=="France" & !missing(BB_Party) & Y_No_Party_Vote==0
tab BB_2nd_Party BB_Year if Y_2nd_Party_Vote_Long=="." & BB_Country=="France" & !missing(BB_2nd_Party) & Y_No_2nd_Party_Vote==0
tab BB_Least_Party BB_Year if Y_Least_Party_Vote_Long=="." & BB_Country=="France" & !missing(BB_Least_Party) & Y_No_Least_Party_Vote==0

/**********************************
******* Italy ********************
**********************************/

// Generating Political Party Dummy Variables for Italy
//Political parties question not asked in WVS waves

foreach party in BB_Party BB_2nd_Party BB_Least_Party {
    if "`party'" == "BB_Party" {
        local i "$prefix_BB_Party"
    }
    else if "`party'" == "BB_2nd_Party" {
        local i "$prefix_BB_2nd_Party"
    }
    else if "`party'" == "BB_Least_Party" {
        local i "$prefix_BB_Least_Party"
    }
	
	replace Y`i'Party_Vote = "Communist_ITA" if BB`i'Party == "IT: Communist Party" | BB`i'Party == "IT: Partito Comunista Italiano - Party of the Italian Communists"
	replace Y`i'Party_Vote_Long = "ITA: Communist Party" if Y`i'Party_Vote == "Communist_ITA"

	replace Y`i'Party_Vote = "PowerPpl_ITA" if BB`i'Party == "IT: Power to the People!"
	replace Y`i'Party_Vote_Long = "ITA: Power to the People" if Y`i'Party_Vote == "PowerPpl_ITA"

	replace Y`i'Party_Vote = "FreeEqual_ITA" if BB`i'Party == "IT: Free and Equal"
	replace Y`i'Party_Vote_Long = "ITA: Free and Equal" if Y`i'Party_Vote == "FreeEqual_ITA"

	replace Y`i'Party_Vote = "EuropeTogether_ITA" if BB`i'Party == "IT: Italy Europe Together"
	replace Y`i'Party_Vote_Long = "ITA: Italy Europe Together" if Y`i'Party_Vote == "EuropeTogether_ITA"

	replace Y`i'Party_Vote = "Democratic_ITA" if BB`i'Party == "IT: Democratic Party" | BB`i'Party == "IT: Demokratici - Democrats" | BB`i'Party == "IT: Partito Democratico"
	replace Y`i'Party_Vote_Long = "ITA: Democratic Party" if Y`i'Party_Vote == "Democratic_ITA"

	replace Y`i'Party_Vote = "MoreEU_ITA" if BB`i'Party == "IT: More Europe"
	replace Y`i'Party_Vote_Long = "ITA: More Europe" if Y`i'Party_Vote == "MoreEU_ITA"

	replace Y`i'Party_Vote = "FiveStar_ITA" if BB`i'Party == "IT: Five Star Movement"
	replace Y`i'Party_Vote_Long = "ITA: Five Star Movement" if Y`i'Party_Vote == "FiveStar_ITA"

	replace Y`i'Party_Vote = "UsWithItaly-UDC_ITA" if BB`i'Party == "IT: Unione di Centro (UDC-Rosa Bianca)" | BB`i'Party == "IT: Us with Italy - UDC (Union of the Centre)"
	replace Y`i'Party_Vote_Long = "ITA: Us with Italy (Union of the Centre)" if Y`i'Party_Vote == "UsWithItaly-UDC_ITA"

	replace Y`i'Party_Vote = "PplFamilyParty_ITA" if BB`i'Party == "IT: The People of the Family"
	replace Y`i'Party_Vote_Long = "ITA: The People of the Family" if Y`i'Party_Vote == "PplFamilyParty_ITA"

	replace Y`i'Party_Vote = "GoItalyParty_ITA" if BB`i'Party == "IT: FI - Forza Italia" | BB`i'Party == "IT: Go Italy"
	replace Y`i'Party_Vote_Long = "ITA: Go Italy" if Y`i'Party_Vote == "GoItalyParty_ITA"

	replace Y`i'Party_Vote = "League_ITA" if BB`i'Party == "IT: League" | BB`i'Party == "IT: Lega Nord - Northern League" // Same party; Northern League was rebranded to just "The League" in 2018
	replace Y`i'Party_Vote_Long = "ITA: League" if Y`i'Party_Vote == "League_ITA"

	replace Y`i'Party_Vote = "BrothersOfItaly_ITA" if BB`i'Party == "IT: Brothers of Italy with Giorgia Meloni"
	replace Y`i'Party_Vote_Long = "ITA: Brothers of Italy with Giorgia Meloni" if Y`i'Party_Vote == "BrothersOfItaly_ITA"

	replace Y`i'Party_Vote = "Casapound_ITA" if BB`i'Party == "IT: CasaPound Italy"
	replace Y`i'Party_Vote_Long = "ITA: CasaPound Italy" if Y`i'Party_Vote == "Casapound_ITA"

	replace Y`i'Party_Vote = "ItalyToItalians_ITA" if BB`i'Party == "IT: Italy to the Italians"
	replace Y`i'Party_Vote_Long = "ITA: Italy to the Italians" if Y`i'Party_Vote == "ItalyToItalians_ITA"

	replace Y`i'Party_Vote = "NatAlliance_ITA" if BB`i'Party == "IT: AN - National Alliance" | BB`i'Party == "IT: Movimento Sociale-Destra Nazionale" /* This two parties are the same, "Italian
	Social Movement-National Right" changed the name to "National Alliance" */
	replace Y`i'Party_Vote_Long = "ITA: National Alliance" if Y`i'Party_Vote == "NatAlliance_ITA"

	replace Y`i'Party_Vote = "CentreChristDem_ITA" if BB`i'Party == "IT: CCD - Christian Democratic Centre" // I believe this later became the UDC party after merging with United Christian Dems
	replace Y`i'Party_Vote_Long = "ITA: Christian Democratic Centre" if Y`i'Party_Vote == "CentreChristDem_ITA"

	replace Y`i'Party_Vote = "UnitedChristDems_ITA" if BB`i'Party == "IT: CDU - United Christian Democrats"
	replace Y`i'Party_Vote_Long = "ITA: United Christian Democrats" if Y`i'Party_Vote == "UnitedChristDems_ITA"

	replace Y`i'Party_Vote = "DemLeft_ITA" if BB`i'Party == "IT: Dem. Sinistra - Democrats of the Left"
	replace Y`i'Party_Vote_Long = "ITA: Democrats of the Left (Sinistra)" if Y`i'Party_Vote == "DemLeft_ITA"

	replace Y`i'Party_Vote = "DemChristiana_ITA" if BB`i'Party == "IT: Democrazia Cristiana" // Dissolved in 1994, different from other two Christian Democratic parties above
	replace Y`i'Party_Vote_Long = "ITA: Democrazia Cristiana" if Y`i'Party_Vote == "DemChristiana_ITA"

	replace Y`i'Party_Vote = "ProletariatDem_ITA" if BB`i'Party == "IT: Democrazia Proletaria"
	replace Y`i'Party_Vote_Long = "ITA: Democrazia Proletaria" if Y`i'Party_Vote == "ProletariatDem_ITA"

	replace Y`i'Party_Vote = "TricolorFlame_ITA" if BB`i'Party == "IT: Fiamma Tricol - Tricolour Flame"
	replace Y`i'Party_Vote_Long = "ITA: Tricolour Flame" if Y`i'Party_Vote == "TricolorFlame_ITA"

	replace Y`i'Party_Vote = "PplFreedom_ITA" if BB`i'Party == "IT: Il Popolo della Libertà"
	replace Y`i'Party_Vote_Long = "ITA: The People of Freedom" if Y`i'Party_Vote == "PplFreedom_ITA"

	replace Y`i'Party_Vote = "ItalyValues_ITA" if BB`i'Party == "Italia dei Valori" | BB`i'Party == "IT: Italia dei Valori - Lista Di Pietro"
	replace Y`i'Party_Vote_Long = "ITA: Italy of Values" if Y`i'Party_Vote == "ItalyValues_ITA"

	replace Y`i'Party_Vote = "LibDemReform_ITA" if BB`i'Party == "IT: Liberal Democratici Riformisti"
	replace Y`i'Party_Vote_Long = "ITA: Liberal Democratic Reform" if Y`i'Party_Vote == "LibDemReform_ITA"

	replace Y`i'Party_Vote = "Greens_ITA" if BB`i'Party == "IT: Liste Verdi - Federation of the Greens"
	replace Y`i'Party_Vote_Long = "ITA: Federation of the Greens" if Y`i'Party_Vote == "Greens_ITA"

	replace Y`i'Party_Vote = "Forza_ITA" if BB`i'Party == "IT: Forza Nuova (non presente nelle due isola maggiori)" | BB`i'Party == "IT: Forza Nuova (non presente nelle due isole maggiori)"
	replace Y`i'Party_Vote_Long = "ITA: New Force" if Y`i'Party_Vote == "Forza_ITA"

	replace Y`i'Party_Vote = "Bonino-Pannella_ITA" if BB`i'Party == "IT: Lista Bonino-Pannella"
	replace Y`i'Party_Vote_Long = "ITA: Lista Bonino-Pannella" if Y`i'Party_Vote == "Bonino-Pannella_ITA"

	replace Y`i'Party_Vote = "Liberale_ITA" if BB`i'Party == "IT: Partito Liberale Italiano"
	replace Y`i'Party_Vote_Long = "ITA: Partito Liberale Italiano" if Y`i'Party_Vote == "Liberale_ITA"

	replace Y`i'Party_Vote = "Radical_ITA" if BB`i'Party == "IT: Partito Radicali - Radical Party" | BB`i'Party == "IT: RI"
	replace Y`i'Party_Vote_Long = "ITA: Radical Party" if Y`i'Party_Vote == "Radical_ITA"

	replace Y`i'Party_Vote = "Republican_ITA" if BB`i'Party == "IT: Partito Republicano Italiano"
	replace Y`i'Party_Vote_Long = "ITA: Republican Party" if Y`i'Party_Vote == "Republican_ITA"

	replace Y`i'Party_Vote = "Socialist_ITA" if BB`i'Party == "IT: Partito Socialista Italiano"
	replace Y`i'Party_Vote_Long = "ITA: Socialist Party" if Y`i'Party_Vote == "Socialist_ITA"

	replace Y`i'Party_Vote = "Pensioners_ITA" if BB`i'Party == "IT: Partito pensionati"
	replace Y`i'Party_Vote_Long = "ITA: Pensioners Party" if Y`i'Party_Vote == "Pensioners_ITA"

	replace Y`i'Party_Vote = "CivicLorenzin_ITA" if BB`i'Party == "IT: Popular Civic Lorenzin"
	replace Y`i'Party_Vote_Long = "ITA: Popular Civic Lorenzin" if Y`i'Party_Vote == "CivicLorenzin_ITA"

	replace Y`i'Party_Vote = "DemSocialists_ITA" if BB`i'Party == "IT: SDI - Italian Democratic Socialists"
	replace Y`i'Party_Vote_Long = "ITA: Italian Democratic Socialists SDI" if Y`i'Party_Vote == "DemSocialists_ITA"

	replace Y`i'Party_Vote = "Trentino_ITA" if BB`i'Party == "IT: SVP - PATT"
	replace Y`i'Party_Vote_Long = "ITA: Trentino Tyrolean Autonomist Party" if Y`i'Party_Vote == "Trentino_ITA"

	replace Y`i'Party_Vote = "LeftEcology_ITA" if BB`i'Party == "IT: Sinistra e Libertà"
	replace Y`i'Party_Vote_Long = "ITA: Left Ecology Freedom" if Y`i'Party_Vote == "LeftEcology_ITA"

	replace Y`i'Party_Vote = "UDR_ITA" if BB`i'Party == "IT: UDR"
	replace Y`i'Party_Vote_Long = "ITA: Democratic Union for the Republic" if Y`i'Party_Vote == "UDR_ITA"

	replace Y`i'Party_Vote = "PDUP_ITA" if BB`i'Party == "IT: PDUP - Party of Proletarian Unity for Communism"
	replace Y`i'Party_Vote_Long = "ITA: Party of Proletarian Unity for Communism" if Y`i'Party_Vote == "PDUP_ITA"

	replace Y`i'Party_Vote = "Regionaliste_ITA" if BB`i'Party == "IT: Liste regionaliste (per es. Liga Veneta, Lega Lomb)"
	replace Y`i'Party_Vote_Long = "ITA: Liste Regionaliste" if Y`i'Party_Vote == "Regionaliste_ITA"

	replace Y`i'Party_Vote = "EtnicheLocali_ITA" if BB`i'Party == "IT: Liste etniche locali (per es. Sudtiroler-Volkspartei, Par"
	replace Y`i'Party_Vote_Long = "ITA: Liste etniche locai" if Y`i'Party_Vote == "EtnicheLocali_ITA"

	replace Y`i'Party_Vote = "PPI_ITA" if BB`i'Party == "IT: PPI - Italian Peoples Party"
	replace Y`i'Party_Vote_Long = "ITA: Italian people's party" if Y`i'Party_Vote == "PPI_ITA"

	replace Y`i'Party_Vote = "WorkersCommunist_ITA" if BB`i'Party == "IT: Partito Comunista dei Lavoratori (non presente al Sud e nelle isole)"
	replace Y`i'Party_Vote_Long = "ITA: Workers' communist party" if Y`i'Party_Vote == "WorkersCommunist_ITA"

	replace Y`i'Party_Vote = "RefoundCommunist_ITA" if BB`i'Party == "IT: Rifond. Comun. - Communist Refoundation"
	replace Y`i'Party_Vote_Long = "ITA: Communist Refoundation Party" if Y`i'Party_Vote == "RefoundCommunist_ITA"

	replace Y`i'Party_Vote = "Suditroler_ITA" if BB`i'Party == "IT: Südtiroler Volkspartei (minoranza tedesca, con Partito Autonomista Trentino Tirolese)"
	replace Y`i'Party_Vote_Long = "ITA: South Tyrolean People's Party" if Y`i'Party_Vote == "Suditroler_ITA"

	replace Y`i'Party_Vote = "Polo_ITA" if BB`i'Party == "IT: Polo dell'Autonomia"
	replace Y`i'Party_Vote_Long = "ITA: Polo dell Autonomia" if Y`i'Party_Vote == "Polo_ITA"

	/* Testing Italy*/
	tab Y`i'Party_Vote_Long if BB_Country=="Italy" & !missing(BB`i'Party) & Y_No`i'Party_Vote==0, missing
	tab BB`i'Party if BB_Country=="Italy" & Y_No`i'Party_Vote==0 & Y`i'Party_Vote_Long=="."

	count if Y`i'Party_Vote_Long=="." & BB_Country=="Italy" & !missing(BB`i'Party) & Y_No`i'Party_Vote==0
	assert r(N)==0

}

tab BB_Party BB_Year if Y_Party_Vote_Long=="." & BB_Country=="Italy" & !missing(BB_Party) & Y_No_Party_Vote==0
tab BB_2nd_Party BB_Year if Y_2nd_Party_Vote_Long=="." & BB_Country=="Italy" & !missing(BB_2nd_Party) & Y_No_2nd_Party_Vote==0
tab BB_Least_Party BB_Year if Y_Least_Party_Vote_Long=="." & BB_Country=="Italy" & !missing(BB_Least_Party) & Y_No_Least_Party_Vote==0

/**********************************
******* UK ********************
**********************************/

//Generating Political Party Dummies for UK

foreach party in BB_Party BB_2nd_Party BB_Least_Party {
    if "`party'" == "BB_Party" {
        local i "$prefix_BB_Party"
    }
    else if "`party'" == "BB_2nd_Party" {
        local i "$prefix_BB_2nd_Party"
    }
    else if "`party'" == "BB_Least_Party" {
        local i "$prefix_BB_Least_Party"
    }

	replace Y`i'Party_Vote = "Communist_UK" if BB`i'Party == "GB-GBN: Communist Party"
	replace Y`i'Party_Vote_Long = "UK: Communist Party" if Y`i'Party_Vote == "Communist_UK"

	replace Y`i'Party_Vote = "Conserv_UK" if BB`i'Party == "GB-GBN: Conservative" | BB`i'Party == "GB: Conservative" | BB`i'Party == "GBR: Conservative and Unionist Party"
	replace Y`i'Party_Vote_Long = "UK: Conservative and Unionist Party" if Y`i'Party_Vote == "Conserv_UK"

	replace Y`i'Party_Vote = "Green_UK" if BB`i'Party == "GB-GBN: Green Party" | BB`i'Party == "GB: Green Party" | BB`i'Party == "GBR: Green Party"
	replace Y`i'Party_Vote_Long = "UK: Green Party" if Y`i'Party_Vote == "Green_UK"

	replace Y`i'Party_Vote = "Labour_UK" if BB`i'Party == "GB-GBN: Labour" | BB`i'Party == "GB: Labour" | BB`i'Party == "GBR: Labour Party"
	replace Y`i'Party_Vote_Long = "UK: Labour Party" if Y`i'Party_Vote == "Labour_UK"

	replace Y`i'Party_Vote = "Liberal_UK" if BB`i'Party == "GB-GBN: Liberal"
	replace Y`i'Party_Vote_Long = "UK: Liberal" if Y`i'Party_Vote == "Liberal_UK"

	replace Y`i'Party_Vote = "LibDem_UK" if BB`i'Party == "GB-GBN: Liberal Democrat" | BB`i'Party == "GB: Liberal Democrat" | BB`i'Party == "GBR: Liberal Democrats"
	replace Y`i'Party_Vote_Long = "UK: Liberal Democrat" if Y`i'Party_Vote == "LibDem_UK"

	replace Y`i'Party_Vote = "Nationalist_UK" if BB`i'Party == "GB-GBN: Nationalist" | BB`i'Party == "GB: British National Party (BNP)/ National Front"
	replace Y`i'Party_Vote_Long = "UK: Nationalist Front" if Y`i'Party_Vote == "Nationalist_UK"

	replace Y`i'Party_Vote = "PlaidCymru_UK" if BB`i'Party == "GB-GBN: Plaid Cymru" | BB`i'Party == "GB:  Plaid Cymru"
	replace Y`i'Party_Vote_Long = "UK: Plaid Cymru" if Y`i'Party_Vote == "PlaidCymru_UK"

	replace Y`i'Party_Vote = "Ref_UK" if BB`i'Party == "GB-GBN: Referendum Party" | BB`i'Party == "GB: Referendum Party" | BB`i'Party == "GBR: Referendum Party"
	replace Y`i'Party_Vote_Long = "UK: Referendum Party" if Y`i'Party_Vote == "Ref_UK"

	replace Y`i'Party_Vote = "SNP_UK" if BB`i'Party == "GB-GBN: Scottish National Party SNP" | BB`i'Party == "GB:  Scottish National Party" | BB`i'Party == "GBR: Scottish National Party" | BB`i'Party == "GB-GBN: Scottish/Welsh National Party" | BB`i'Party == "GB: Scottish/Welsh National Party" | BB`i'Party == "GB-GBN: Scottish National Party"
	replace Y`i'Party_Vote_Long = "UK: Scottish National Party" if Y`i'Party_Vote == "SNP_UK"

	replace Y`i'Party_Vote = "SocialDem_UK" if BB`i'Party == "GB-GBN: Social Democrat"
	replace Y`i'Party_Vote_Long = "UK: Social Democrat" if Y`i'Party_Vote == "SocialDem_UK"

	replace Y`i'Party_Vote = "UKIP_UK" if BB`i'Party == "GB-GBN: UK Independence Party" | BB`i'Party == "GB: UK Independence Party (UKIP)" | BB`i'Party == "GB-GBN: UK Independence Party (UKIP)"
	replace Y`i'Party_Vote_Long = "UK: UK Independence Party (UKIP)" if Y`i'Party_Vote == "UKIP_UK"

	replace Y`i'Party_Vote = "BNP_UK" if BB`i'Party == "GB-GBN: British National Party (BNP)/ National Front" 
	replace Y`i'Party_Vote_Long = "UK: British National Party" if Y`i'Party_Vote == "BNP_UK"

	/* Testing UK*/
	tab Y`i'Party_Vote_Long if BB_Country=="UK" & !missing(BB`i'Party) & Y_No`i'Party_Vote==0, missing
	tab BB`i'Party if BB_Country=="UK" & Y_No`i'Party_Vote==0 & Y`i'Party_Vote_Long=="."

	count if Y`i'Party_Vote_Long=="." & BB_Country=="UK" & !missing(BB`i'Party) & Y_No`i'Party_Vote==0
	assert r(N)==0

	}

	tab BB_Party BB_Year if Y_Party_Vote_Long=="." & BB_Country=="UK" & !missing(BB_Party) & Y_No_Party_Vote==0
	tab BB_2nd_Party BB_Year if Y_2nd_Party_Vote_Long=="." & BB_Country=="UK" & !missing(BB_2nd_Party) & Y_No_2nd_Party_Vote==0
	tab BB_Least_Party BB_Year if Y_Least_Party_Vote_Long=="." & BB_Country=="UK" & !missing(BB_Least_Party) & Y_No_Least_Party_Vote==0

/**********************************
******* Sweden ********************
**********************************/

	//Generating Political Party Dummies for Sweden

	foreach party in BB_Party BB_2nd_Party BB_Least_Party {
		if "`party'" == "BB_Party" {
			local i "$prefix_BB_Party"
		}
		else if "`party'" == "BB_2nd_Party" {
			local i "$prefix_BB_2nd_Party"
		}
		else if "`party'" == "BB_Least_Party" {
			local i "$prefix_BB_Least_Party"
		}

	replace Y`i'Party_Vote = "Center_SWE" if BB`i'Party =="SE: Center party" | BB`i'Party=="SE: Centerpartiet" | BB`i'Party=="SWE: Center Party"
	replace Y`i'Party_Vote_Long = "SWE: Center party" if Y`i'Party_Vote == "Center_SWE"

	replace Y`i'Party_Vote = "ChristianDems_SWE" if BB`i'Party=="SE: Christian Democratic Party" | BB`i'Party=="SE: Kristdemokraterna" | BB`i'Party=="SWE: The Christian Democrats"
	replace Y`i'Party_Vote_Long = "SWE: Christian Democratic Party" if Y`i'Party_Vote == "ChristianDems_SWE"

	replace Y`i'Party_Vote = "Liberals_SWE"1 if BB`i'Party=="SE: Folkpartiet" | BB`i'Party=="SE: Liberals" | BB`i'Party=="SWE: Fascist People's Party of Swede"
	replace Y`i'Party_Vote_Long = "SWE: Liberals" if Y`i'Party_Vote == "Liberals_SWE"
	//Checked with wave 6 and wave 7 codebooks and there was no Liberal party. It seems the people's party was translated into a neo-nazi Fascist People's Party of Sweden even though that party was long defunct. We are adding the "fascist" votes back to the liberals here

	replace Y`i'Party_Vote = "Left_SWE" if BB`i'Party=="SE: Left wing party" | BB`i'Party=="SE: Vansterpartiet" | BB`i'Party=="SWE: The Left Party"
	replace Y`i'Party_Vote_Long = "SWE: Left Wing Party" if Y`i'Party_Vote == "Left_SWE"

	replace Y`i'Party_Vote = "Green_SWE" if BB`i'Party=="SE: Miljopartiet" | BB`i'Party=="SE: Green party" | BB`i'Party=="SWE: The Green Party"
	replace Y`i'Party_Vote_Long = "SWE: Green Party" if Y`i'Party_Vote == "Green_SWE"

	replace Y`i'Party_Vote = "Moderate_SWE" if BB`i'Party=="SE: Moderata samlingspartiet" | BB`i'Party=="SE: Moderate party" | BB`i'Party=="SWE: Moderate Party"
	replace Y`i'Party_Vote_Long = "SWE: Moderate Party" if Y`i'Party_Vote == "Moderate_SWE"

	replace Y`i'Party_Vote = "SocialDem_SWE" if BB`i'Party=="SE: Social democratic party" | BB`i'Party=="SE: Socialdemokraterna" | BB`i'Party=="SWE: Swedish Social Democratic Party"
	replace Y`i'Party_Vote_Long = "SWE: Social Democratic Party" if Y`i'Party_Vote == "SocialDem_SWE"

	replace Y`i'Party_Vote = "SwedeDems_SWE" if BB`i'Party=="SE: Sweden democrats" | BB`i'Party=="SWE: Sweden Democrats"
	replace Y`i'Party_Vote_Long = "SWE: Sweden Democrats" if Y`i'Party_Vote == "SwedeDems_SWE"

	replace Y`i'Party_Vote = "Feminist_SWE" if BB`i'Party=="SWE: Feminist Initiative"
	replace Y`i'Party_Vote_Long = "SWE: Feminist Initiative" if Y`i'Party_Vote == "Feminist_SWE"

	replace Y`i'Party_Vote = "Pirate_SWE" if BB`i'Party=="SWE: Pirate Party - Piratpartiet"
	replace Y`i'Party_Vote_Long = "SWE: Pirate Party" if Y`i'Party_Vote == "Pirate_SWE"

	/* Testing Sweden*/
	tab Y`i'Party_Vote_Long if BB_Country=="Sweden" & !missing(BB`i'Party) & Y_No`i'Party_Vote==0, missing
	tab BB`i'Party if BB_Country=="Sweden" & Y_No`i'Party_Vote==0 & Y`i'Party_Vote_Long=="."

	count if Y`i'Party_Vote_Long=="." & BB_Country=="Sweden" & !missing(BB`i'Party) & Y_No`i'Party_Vote==0
	assert r(N)==0
}


tab BB_Party BB_Year if Y_Party_Vote_Long=="." & BB_Country=="Sweden" & !missing(BB_Party) & Y_No_Party_Vote==0
tab BB_2nd_Party BB_Year if Y_2nd_Party_Vote_Long=="." & BB_Country=="Sweden" & !missing(BB_2nd_Party) & Y_No_2nd_Party_Vote==0
tab BB_Least_Party BB_Year if Y_Least_Party_Vote_Long=="." & BB_Country=="Sweden" & !missing(BB_Least_Party) & Y_No_Least_Party_Vote==0

/**********************************
******* Hungary ********************
**********************************/

// The Hungarian Socialist Party is one of two successort to the Hungarian Socialist Workers´ Party. The latter received support of 24 people in 1999 even though it didn't exist, however the socialist party also received support in that year which doesn't make much sense. For now, simpliying by merging with party that did receive support. Perhaps should just remove these observation
foreach party in BB_Party BB_2nd_Party BB_Least_Party {
    if "`party'" == "BB_Party" {
        local i "$prefix_BB_Party"
    }
    else if "`party'" == "BB_2nd_Party" {
        local i "$prefix_BB_2nd_Party"
    }
    else if "`party'" == "BB_Least_Party" {
        local i "$prefix_BB_Least_Party"
    }

	tab BB`i'Party if BB_Country=="Hungary" & BB_Year==1999
	replace Y`i'Party_Vote = "Socialist_HUN" if BB`i'Party == "HU: Hungarian Socialist Party (MSZP)" | BB`i'Party== "HU: Magyar Szocialista Part (MSZP) - Hungarian Socialist Party (ex Communists)" | BB`i'Party == "HUN: Hungarian Socialist Party" | BB`i'Party == "HU: Magyar Szocialista Munkaspart (MSZMP) - Hungarian Socialist Workers´ Party"
	replace Y`i'Party_Vote_Long = "HUN: Socialist Party" if Y`i'Party_Vote == "Socialist_HUN"

	replace Y`i'Party_Vote = "ChristianDems_HUN" if BB`i'Party == "HU: Christian Democratic People's Party (KDNP)" | BB`i'Party== "HU: Kereszteny Demokrata Neppart (KDNP) - Christian Democratic People's Party" | BB`i'Party== "HUN: Christian Democratic People's Party" | BB`i'Party == "HU: Christian Democratic People's Party"
	replace Y`i'Party_Vote_Long = "HUN: Christian Democratic People's Party" if Y`i'Party_Vote == "ChristianDems_HUN"

	replace Y`i'Party_Vote = "Jobbik_HUN" if BB`i'Party == "HU: Jobbik Magyarországért Mozgalom - JOBBIK" | BB`i'Party == "HU: Movement for a Better Hungary (Jobbik)" | BB`i'Party == "HUN: Movement for a Better Hungary"
	replace Y`i'Party_Vote_Long = "HUN: Movement for a Better Hungary (Jobbik)" if Y`i'Party_Vote == "Jobbik_HUN"

	replace Y`i'Party_Vote = "Fidesz_HUN" if BB`i'Party == "HU: FIDESZ Magyar Polgari Part - Federation of Young Democrats"  | BB`i'Party == "HU: Fidesz" | BB`i'Party =="HUN: Fidesz – Hungarian Civic Alliance"
	replace Y`i'Party_Vote_Long = "HUN: Fidesz – Hungarian Civic Alliance" if Y`i'Party_Vote == "Fidesz_HUN"

	replace Y`i'Party_Vote = "Center_HUN" if BB`i'Party == "HU: Centrumpárt"
	replace Y`i'Party_Vote_Long = "HUN: Center Party" if Y`i'Party_Vote == "Center_HUN"

	replace Y`i'Party_Vote = "DemCoalition_HUN" if BB`i'Party == "HU: Democratic Coalition (DK)"
	replace Y`i'Party_Vote_Long = "HUN: Democratic Coalition (DK)" if Y`i'Party_Vote == "DemCoalition_HUN"

	replace Y`i'Party_Vote = "Dialogue_HUN" if BB`i'Party == "HU: Dialogue for Hungary (PM)"
	replace Y`i'Party_Vote_Long = "HUN: Dialogue for Hungary (PM)" if Y`i'Party_Vote == "Dialogue_HUN"

	replace Y`i'Party_Vote = "Smallholders_HUN" if BB`i'Party == "HU: Fuggetlen Kisgazda Part (FKGP) - Independent Smallholders Party" | BB`i'Party == "HUN: Independent Smallholders Party"
	replace Y`i'Party_Vote_Long = "HUN: Fuggetlen Kisgazda Part (FKGP) - Independent Smallholders Party" if Y`i'Party_Vote == "Smallholders_HUN"

	replace Y`i'Party_Vote = "Liberal_HUN" if BB`i'Party == "HU: Hungarian Liberal Party (Liberálisok)"
	replace Y`i'Party_Vote_Long = "HUN: Hungarian Liberal Party (Liberálisok)" if Y`i'Party_Vote == "Liberal_HUN"

	replace Y`i'Party_Vote = "TwoTailDog_HUN" if BB`i'Party == "HU: Hungarian Two-tailed Dog Party (MKKP)" 
	replace Y`i'Party_Vote_Long = "HUN: Hungarian Two-tailed Dog Party (MKKP)" if Y`i'Party_Vote == "TwoTailDog_HUN"

	replace Y`i'Party_Vote = "Workers_HUN" if BB`i'Party == "HU: Hungarian Workers' Party (Munkáspárt)" | BB`i'Party == "HU: Magyar Kommunista Munkáspárt Munkáspárt" | BB`i'Party == "HUN: Hungarian Workers' Party"
	replace Y`i'Party_Vote_Long = "HUN: Hungarian Workers' Party (Munkáspárt)" if Y`i'Party_Vote == "Workers_HUN"

	replace Y`i'Party_Vote = "DemForum_HUN" if BB`i'Party == "HU: Magyar Demokrata Forum (MDF) - Hungarian Democratic Forum" | BB`i'Party == "HUN: Hungarian Democratic Forum"
	replace Y`i'Party_Vote_Long = "HUN: Magyar Demokrata Forum (MDF) - Hungarian Democratic Forum" if Y`i'Party_Vote == "DemForum_HUN"

	replace Y`i'Party_Vote = "Justice_HUN" if BB`i'Party == "HU: Magyar Igazsag es Elet Partja (MIEP) - Party of Hungarian Justice and Life" | BB`i'Party ==  "HUN: Party of Hungarian Justice and Life"
	replace Y`i'Party_Vote_Long = "HUN: Magyar Igazsag es Elet Partja (MIEP) - Party of Hungarian Justice and Life" if Y`i'Party_Vote == "Justice_HUN"

	replace Y`i'Party_Vote = "Momentum_HUN" if BB`i'Party == "HU: Momentum Movement (Momentum)"
	replace Y`i'Party_Vote_Long = "HUN: Momentum Movement (Momentum)" if Y`i'Party_Vote == "Momentum_HUN"

	replace Y`i'Party_Vote = "PoliticsDif_HUN" if BB`i'Party == "HU: Politics Can Be Different (LMP)"
	replace Y`i'Party_Vote_Long = "HUN: Politics Can Be Different (LMP)" if Y`i'Party_Vote == "PoliticsDif_HUN"

	replace Y`i'Party_Vote = "FreeDemocrats_HUN" if BB`i'Party == "HU: Szabad Demokratak Szovetsege (SZDSZ) - Aliance of Free Democrats" | BB`i'Party == "HUN: Aliance of Free Democrats"
	replace Y`i'Party_Vote_Long = "HUN: Alliance of Free Democrats" if Y`i'Party_Vote == "FreeDemocrats_HUN"

	replace Y`i'Party_Vote = "NewEra_HUN" if BB`i'Party == "HU: Together – Party for a New Era (Együtt)"
	replace Y`i'Party_Vote_Long = "HUN: Together – Party for a New Era (Együtt)" if Y`i'Party_Vote == "NewEra_HUN"

	replace Y`i'Party_Vote = "Entrepreneurs_HUN" if BB`i'Party == "HUN: Entrepreneurs' Party"
	replace Y`i'Party_Vote_Long = "HUN: Entrepreneurs' Party" if Y`i'Party_Vote == "Entrepreneurs_HUN"

	replace Y`i'Party_Vote = "Green_HUN" if BB`i'Party == "HUN: Green Party of Hungary"
	replace Y`i'Party_Vote_Long = "HUN: Green Party of Hungary" if Y`i'Party_Vote == "Green_HUN"

	replace Y`i'Party_Vote = "DemPeoples_HUN" if BB`i'Party == "HUN: Hungarian Democratic People´s Party"
	replace Y`i'Party_Vote_Long = "HUN: Hungarian Democratic People's Party" if Y`i'Party_Vote == "DemPeoples_HUN"

	replace Y`i'Party_Vote = "SocialDems_HUN" if BB`i'Party == "HUN: Social Democratic Party of Hungary"
	replace Y`i'Party_Vote_Long = "HUN: Social Democratic Party of Hungary" if Y`i'Party_Vote == "SocialDems_HUN"

	/* Testing Hungary*/
	tab Y`i'Party_Vote_Long if BB_Country=="Hungary" & !missing(BB`i'Party) & Y_No`i'Party_Vote==0, missing
	tab BB`i'Party if BB_Country=="Hungary" & Y_No`i'Party_Vote==0 & Y`i'Party_Vote_Long=="."

	count if Y`i'Party_Vote_Long=="." & BB_Country=="Hungary" & !missing(BB`i'Party) & Y_No`i'Party_Vote==0
	assert r(N)==0
}

tab BB_Party BB_Year if Y_Party_Vote_Long=="." & BB_Country=="Hungary" & !missing(BB_Party) & Y_No_Party_Vote==0
tab BB_2nd_Party BB_Year if Y_2nd_Party_Vote_Long=="." & BB_Country=="Hungary" & !missing(BB_2nd_Party) & Y_No_2nd_Party_Vote==0
tab BB_Least_Party BB_Year if Y_Least_Party_Vote_Long=="." & BB_Country=="Hungary" & !missing(BB_Least_Party) & Y_No_Least_Party_Vote==0

/**********************************
******* Poland ********************
**********************************/
foreach party in BB_Party BB_2nd_Party BB_Least_Party {
    if "`party'" == "BB_Party" {
        local i "$prefix_BB_Party"
    }
    else if "`party'" == "BB_2nd_Party" {
        local i "$prefix_BB_2nd_Party"
    }
    else if "`party'" == "BB_Least_Party" {
        local i "$prefix_BB_Least_Party"
    }

	replace Y`i'Party_Vote = "AWSP_POL" if BB`i'Party == "PL: Akcja Wyborcza Solidarnosc (Electoral Action Solidarity)"
	replace Y`i'Party_Vote_Long = "POL: Electoral Action Solidarity (AWSP)" if Y`i'Party_Vote == "AWSP_POL"

	replace Y`i'Party_Vote = "Center_POL" if BB`i'Party == "PL: Center"
	replace Y`i'Party_Vote_Long = "POL: Center" if Y`i'Party_Vote == "Center_POL"

	replace Y`i'Party_Vote = "Retired_POL" if BB`i'Party == "PL: KPEIR - Country Party of Pensioned and Retired Persons" | BB`i'Party == "POL: National Party of Retirees and Pensioners"
	replace Y`i'Party_Vote_Long = "POL: KPEIR - Country Party of Pensioned and Retired Persons" if Y`i'Party_Vote == "Retired_POL"

	replace Y`i'Party_Vote = "KPN_POL" if BB`i'Party == "PL: KPN (Konfederacja Polski Niepodleglej) (nationalistic,irr"
	replace Y`i'Party_Vote_Long = "POL: Confederation of Independent Poland (KPN)" if Y`i'Party_Vote == "KPN_POL"

	replace Y`i'Party_Vote = "Kukiz_POL" if BB`i'Party == "PL: Kukiz'15"
	replace Y`i'Party_Vote_Long = "POL: Kukiz 15" if Y`i'Party_Vote == "Kukiz_POL"

	replace Y`i'Party_Vote = "LPR_POL" if BB`i'Party == "PL: LPR Liga Polskich Rodzin" | BB`i'Party == "POL: The League of Polish Families"
	replace Y`i'Party_Vote_Long = "POL: League of Polish Families (LPR)" if Y`i'Party_Vote == "LPR_POL"

	replace Y`i'Party_Vote = "Law_POL" if BB`i'Party == "PL: Law and Justice" | BB`i'Party == "PL: PiS - Prawo i Sprawiedliwosc" | BB`i'Party == "POL: Law and Justice" 
	replace Y`i'Party_Vote_Long = "POL: Law and Justice (PiS)" if Y`i'Party_Vote == "Law_POL"

	replace Y`i'Party_Vote = "wise_POL" if BB`i'Party == "PL: Well governing, just, wise party"
	replace Y`i'Party_Vote_Long = "POL: Well governing, just, wise party" if Y`i'Party_Vote == "wise_POL"

	replace Y`i'Party_Vote = "Left_POL" if BB`i'Party == "PL: Left-only general formulation"
	replace Y`i'Party_Vote_Long = "POL: Left-only general formulation" if Y`i'Party_Vote == "Left_POL"

	replace Y`i'Party_Vote = "Liberal_POL" if BB`i'Party == "PL: Liberal, true democratic"
	replace Y`i'Party_Vote_Long = "POL: Liberal, true democratic" if Y`i'Party_Vote == "Liberal_POL"

	replace Y`i'Party_Vote = "Liberty_POL" if BB`i'Party == "PL: Liberty (Korwin)"
	replace Y`i'Party_Vote_Long = "POL: Liberty (Korwin)" if Y`i'Party_Vote == "Liberty_POL"

	replace Y`i'Party_Vote = "Modern_POL" if BB`i'Party == "PL: Modern"
	replace Y`i'Party_Vote_Long = "POL: Modern" if Y`i'Party_Vote == "Modern_POL"

	replace Y`i'Party_Vote = "ChristianDems_POL" if BB`i'Party == "PL: Narodowo-Chrzescijansko-Demokratyczny Blok dla Polski" | BB`i'Party == "POL: Polish Christian Democratic Party"
	replace Y`i'Party_Vote_Long = "POL: National Christian Democratic Block for Poland" if Y`i'Party_Vote == "ChristianDems_POL"

	replace Y`i'Party_Vote = "OPZZ_POL" if BB`i'Party == "PL: OPZZ (Ognopolskie Zrzeszenie Zwiazk Zawodowych) (trad"
	replace Y`i'Party_Vote_Long = "POL: All-Poland Alliance of Trade Unions (OPZZ)" if Y`i'Party_Vote == "OPZZ_POL"

	replace Y`i'Party_Vote = "Civic_POL" if BB`i'Party == "PL: PO Platforma Obywatelska" | BB`i'Party == "POL: Civic Platform" | BB`i'Party == "PL: Citizens' Platform"
	replace Y`i'Party_Vote_Long = "POL: Civic Platform (PO)" if Y`i'Party_Vote == "Civic_POL"

	replace Y`i'Party_Vote = "PPS_POL" if BB`i'Party == "PL: PPS (Polska Partia Socjalistyczna), PPS-RD (Polska Partia"
	replace Y`i'Party_Vote_Long = "POL: Polish Socialist Party (PPS)" if Y`i'Party_Vote == "PPS_POL"

	replace Y`i'Party_Vote = "Greens_POL" if BB`i'Party == "PL: PPZ (Polska Partia Zielonych), ruchy ekologiczne"
	replace Y`i'Party_Vote_Long = "POL: Green Party (PPZ)" if Y`i'Party_Vote == "Greens_POL"

	replace Y`i'Party_Vote = "Peoples_POL" if BB`i'Party == "PL: PSL (Polish Peasants Party)" | BB`i'Party == "PL: Polish Peasants' Party" | BB`i'Party == "POL: Peasants' Party" | BB`i'Party == "POL: Polish People's Party"
	replace Y`i'Party_Vote_Long = "POL: Polish People's (Peasants) Party (PSL)" if Y`i'Party_Vote == "Peoples_POL"

	replace Y`i'Party_Vote = "Democratic_POL" if BB`i'Party == "PL: Partia Demokratyczna - Demokraci.pl" | BB`i'Party == "POL: Democratic Party"
	replace Y`i'Party_Vote_Long = "POL: Democratic Party" if Y`i'Party_Vote == "Democratic_POL"

	replace Y`i'Party_Vote = "Womens_POL" if BB`i'Party == "PL: Party of Women - PK"
	replace Y`i'Party_Vote_Long = "POL: Party of Women (PK)" if Y`i'Party_Vote == "Womens_POL"

	replace Y`i'Party_Vote = "Labor_POL" if BB`i'Party == "PL: Polska Partia Pracy" | BB`i'Party == "POL: Labor Party" | BB`i'Party == "POL: The Labour Party"
	replace Y`i'Party_Vote_Long = "POL: Labor Party (SP)" if Y`i'Party_Vote == "Labor_POL"

	replace Y`i'Party_Vote = "Together_POL" if BB`i'Party == "PL: Together Party"
	replace Y`i'Party_Vote_Long = "POL: Together Party" if Y`i'Party_Vote == "Together_POL"

	replace Y`i'Party_Vote = "UPR_POL" if BB`i'Party == "PL: UPR - Union of Real Politics" | BB`i'Party == "POL: Platform Janusz Korwin-Mikke" | BB`i'Party == "POL: Real Politics Union"
	replace Y`i'Party_Vote_Long = "POL: UPR - Union of Real Politics" if Y`i'Party_Vote == "UPR_POL"

	replace Y`i'Party_Vote = "Freedom_POL" if BB`i'Party == "PL: UW - Union of Freedom" | BB`i'Party == "POL: Union of Freedom"
	replace Y`i'Party_Vote_Long = "POL: Union of Freedom (UW)" if Y`i'Party_Vote == "Freedom_POL"

	replace Y`i'Party_Vote = "Reconstruct_POL" if BB`i'Party == "PL: ROP - Movement for Reconstruction of Poland" | BB`i'Party == "POL: Movement for Reconstruction of Poland"
	replace Y`i'Party_Vote_Long = "POL: Movement for Reconstruction of Poland (ROP)" if Y`i'Party_Vote == "Reconstruct_POL"

	replace Y`i'Party_Vote = "Solidarity_POL" if BB`i'Party == "POL: Solidarity Citizens' Committee" | BB`i'Party == "POL: Solidarnosc"
	replace Y`i'Party_Vote_Long = "POL: Solidarity Citizens' Committee" if Y`i'Party_Vote == "Solidarity_POL"

	replace Y`i'Party_Vote = "LabourUnion_POL" if BB`i'Party == "POL: Union of Labor" | BB`i'Party == "PL: UP - Union of Labor"
	replace Y`i'Party_Vote_Long = "POL: Union of Labor (UP)" if Y`i'Party_Vote == "LabourUnion_POL"

	replace Y`i'Party_Vote = "SocialDems_POL" if BB`i'Party == "PL: SDPl Socjaldemokracja Polska" | BB`i'Party == "POL: Social Democracy"
	replace Y`i'Party_Vote_Long = "POL: Social Democracy (SDPL)" if Y`i'Party_Vote == "SocialDems_POL"

	replace Y`i'Party_Vote = "YourMovement_POL" if BB`i'Party == "POL: Palikot's Movement"
	replace Y`i'Party_Vote_Long = "POL: Your (Palikot's) Movement" if Y`i'Party_Vote == "YourMovement_POL"

	replace Y`i'Party_Vote = "SelfDefense_POL" if BB`i'Party == "POL: Self-defense of the Republic of Poland" | BB`i'Party == "PL: Samoobrona Rzeczpospolitej Polskiej"
	replace Y`i'Party_Vote_Long = "POL: Self-defense of the Republic of Poland (SRP)" if Y`i'Party_Vote == "SelfDefense_POL"

	replace Y`i'Party_Vote = "AllianceDems_POL" if BB`i'Party == "PL: SD (Stronnictwo Demokratyczne) (old party, craftmen and"
	replace Y`i'Party_Vote_Long = "POL: Alliance of Democrats (SD)" if Y`i'Party_Vote == "AllianceDems_POL"


	replace Y`i'Party_Vote = "LeftDemsAlliance_POL" if BB`i'Party == "PL: SLD - Alliance of Democratic Left" | BB`i'Party == "POL: Democratic Left Alliance" | BB`i'Party == "PL: Democratic Left Alliance"
	replace Y`i'Party_Vote_Long = "POL: Democratic Left Alliance (SLD)" if Y`i'Party_Vote == "LeftDemsAlliance_POL"

	replace Y`i'Party_Vote = "UnitedWorkers_POL" if BB`i'Party == "POL: Polish United Workers' Party"
	replace Y`i'Party_Vote_Long = "POL: Polish United Workers' Party (PZPR)" if Y`i'Party_Vote == "UnitedWorkers_POL"

	replace Y`i'Party_Vote = "SocialDemsRepublic_POL" if BB`i'Party == "PL: SdRP (Socjaldemokracja Rzeczpospolitej Polskiej) (new pos"
	replace Y`i'Party_Vote_Long = "POL: Social Democracy of the Republic of Poland (SdRP)" if Y`i'Party_Vote == "SocialDemsRepublic_POL"

	replace Y`i'Party_Vote = "ChristianNational_POL" if BB`i'Party == "PL: ZChN (Zwiazek Chrzescijansko-Narodowy), SP (Stronnictwo P"
	replace Y`i'Party_Vote_Long = "POL: Christian National Union (ZChN)" if Y`i'Party_Vote == "ChristianNational_POL"

	replace Y`i'Party_Vote = "Ancestral_POL" if BB`i'Party == "POL: Ancestral Home"
	replace Y`i'Party_Vote_Long = "POL: Ancestral Home" if Y`i'Party_Vote == "Ancestral_POL"

	replace Y`i'Party_Vote = "ChristDem3_POL" if BB`i'Party == "POL: Christian Democracy of the 3rd Polish Republic"
	replace Y`i'Party_Vote_Long = "POL: Christian Democracy of the 3rd Polish Republic (ChD III RP)" if Y`i'Party_Vote == "ChristDem3_POL"

	replace Y`i'Party_Vote = "Rebirth_POL" if BB`i'Party == "POL: National Rebirth of Poland"
	replace Y`i'Party_Vote_Long = "POL: National Rebirth of Poland (NOP)" if Y`i'Party_Vote == "Rebirth_POL"

	replace Y`i'Party_Vote = "Patriot_POL" if BB`i'Party == "POL: Patriotic Party"
	replace Y`i'Party_Vote_Long = "POL: Patriotic Party" if Y`i'Party_Vote == "Patriot_POL"

	replace Y`i'Party_Vote = "RightGeneral_POL" if BB`i'Party == "PL: Right-only general formulation"
	replace Y`i'Party_Vote_Long = "POL: Right-only general formulation" if Y`i'Party_Vote == "RightGeneral_POL"

	replace Y`i'Party_Vote = "National_POL" if BB`i'Party == "POL: National Party"
	replace Y`i'Party_Vote_Long = "POL: National Party" if Y`i'Party_Vote == "National_POL"

	/* Testing Poland*/
	tab Y`i'Party_Vote_Long if BB_Country=="Poland" & !missing(BB`i'Party) & Y_No`i'Party_Vote==0, missing
	tab BB`i'Party if BB_Country=="Poland" & Y_No`i'Party_Vote==0 & Y`i'Party_Vote_Long=="."

	count if Y`i'Party_Vote_Long=="." & BB_Country=="Poland" & !missing(BB`i'Party) & Y_No`i'Party_Vote==0
	assert r(N)==0

}

tab BB_Party BB_Year if Y_Party_Vote_Long=="." & BB_Country=="Poland" & !missing(BB_Party) & Y_No_Party_Vote==0
tab BB_2nd_Party BB_Year if Y_2nd_Party_Vote_Long=="." & BB_Country=="Poland" & !missing(BB_2nd_Party) & Y_No_2nd_Party_Vote==0
tab BB_Least_Party BB_Year if Y_Least_Party_Vote_Long=="." & BB_Country=="Poland" & !missing(BB_Least_Party) & Y_No_Least_Party_Vote==0

/**********************************
******* Austria ********************
**********************************/
foreach party in BB_Party BB_2nd_Party BB_Least_Party {
    if "`party'" == "BB_Party" {
        local i "$prefix_BB_Party"
    }
    else if "`party'" == "BB_2nd_Party" {
        local i "$prefix_BB_2nd_Party"
    }
    else if "`party'" == "BB_Least_Party" {
        local i "$prefix_BB_Least_Party"
    }

	replace Y`i'Party_Vote = "Alliance_AUS" if BB`i'Party == "AT: Alliance for the Future of Austria - BZÖ"
	replace Y`i'Party_Vote_Long = "AUS: Alliance for the Future of Austria (BZO)" if Y`i'Party_Vote == "Alliance_AUS"

	replace Y`i'Party_Vote = "Lugner_AUS" if BB`i'Party == "AT: DU - Lugner"
	replace Y`i'Party_Vote_Long = "AUS: Lugner List, Independents (DU)" if Y`i'Party_Vote == "Lugner_AUS"

	replace Y`i'Party_Vote = "MartinsList_AUS" if BB`i'Party == "AT: Dr. Martin’s List - For Democracy, Control, Justice\Liste Hans Peter Martin"
	replace Y`i'Party_Vote_Long = "AUS: Dr. Hans Peter Martins List" if Y`i'Party_Vote == "MartinsList_AUS"

	replace Y`i'Party_Vote = "Freedom_AUS" if BB`i'Party == "AT: Freedom Party of Austria - FPÖ" | BB`i'Party == "AT: FPÖ"
	replace Y`i'Party_Vote_Long = "AUS: Freedom Party of Austria (FPO)" if Y`i'Party_Vote == "Freedom_AUS"

	replace Y`i'Party_Vote = "Green_AUS" if BB`i'Party == "AT: Gruene" | BB`i'Party == "AT: The Greens - The Green Alternative"
	replace Y`i'Party_Vote_Long = "AUS: Greens" if Y`i'Party_Vote == "Green_AUS"

	replace Y`i'Party_Vote = "Communist_AUS" if BB`i'Party == "AT: KPÖ"
	replace Y`i'Party_Vote_Long = "AUS: Communist Party (KPO)" if Y`i'Party_Vote == "Communist_AUS"

	replace Y`i'Party_Vote = "LibForum_AUS" if BB`i'Party == "AT: Liberales Forum"
	replace Y`i'Party_Vote_Long = "AUS: Liberales Forum (LiF)" if Y`i'Party_Vote == "LibForum_AUS"

	replace Y`i'Party_Vote = "Peoples_AUS" if BB`i'Party == "AT: List Kurz / Austrian People's Party - ÖVP" | BB`i'Party == "AT: ÖVP"
	replace Y`i'Party_Vote_Long = "AUS: Austrian People's Party (OVP)" if Y`i'Party_Vote == "Peoples_AUS"

	replace Y`i'Party_Vote = "SocialDems_AUS" if BB`i'Party == "AT: Social Democratic Party of Austria - SPÖ" | BB`i'Party == "AT: SPÖ"
	replace Y`i'Party_Vote_Long = "AUS: Social Democratic Party of Austria (SPO)" if Y`i'Party_Vote == "SocialDems_AUS"

	replace Y`i'Party_Vote = "NewAustria_AUS" if BB`i'Party == "AT: The new Austria - NEOS"
	replace Y`i'Party_Vote_Long = "AUS: The New Austria (NEOS)" if Y`i'Party_Vote == "NewAustria_AUS"

	replace Y`i'Party_Vote = "PilzList_AUS" if BB`i'Party == "AT: List Peter Pilz"
	replace Y`i'Party_Vote_Long = "AUS: List Peter Pilz (JETZT)" if Y`i'Party_Vote == "PilzList_AUS"

	/* Testing Austria*/
	tab Y`i'Party_Vote_Long if BB_Country=="Austria" & !missing(BB`i'Party) & Y_No`i'Party_Vote==0, missing
	tab BB`i'Party if BB_Country=="Austria" & Y_No`i'Party_Vote==0 & Y`i'Party_Vote_Long=="."

	count if Y`i'Party_Vote_Long=="." & BB_Country=="Austria" & !missing(BB`i'Party) & Y_No`i'Party_Vote==0
	assert r(N)==0
}

tab BB_Party BB_Year if Y_Party_Vote_Long=="." & BB_Country=="Austria" & !missing(BB_Party) & Y_No_Party_Vote==0
tab BB_2nd_Party BB_Year if Y_2nd_Party_Vote_Long=="." & BB_Country=="Austria" & !missing(BB_2nd_Party) & Y_No_2nd_Party_Vote==0
tab BB_Least_Party BB_Year if Y_Least_Party_Vote_Long=="." & BB_Country=="Austria" & !missing(BB_Least_Party) & Y_No_Least_Party_Vote==0
/**********************************
******* Spain ********************
**********************************/

foreach party in BB_Party BB_2nd_Party BB_Least_Party {
		if "`party'" == "BB_Party" {
			local i "$prefix_BB_Party"
		}
		else if "`party'" == "BB_2nd_Party" {
			local i "$prefix_BB_2nd_Party"
		}
		else if "`party'" == "BB_Least_Party" {
			local i "$prefix_BB_Least_Party"
		}

	replace Y`i'Party_Vote = "Socialist_SPA" if BB`i'Party == "ES: PSOE (Partido Socialista Obrero Espanol) - Spanish Socialist Workers Party" | BB`i'Party == "ES: Spanish Workers' Socialist Party" | BB`i'Party == "ESP: Socialist People's Party" | BB`i'Party == "ES: Spanish Workers Socialist Party"
	replace Y`i'Party_Vote_Long = "SPA: Socialist Party (PSOE)" if Y`i'Party_Vote == "Socialist_SPA"

	replace Y`i'Party_Vote = "Popular_SPA" if BB`i'Party == "ES: Popular Party" | BB`i'Party == "ES: PP (Partido Popular) - Peoples Party (1981: AP)" | BB`i'Party == "ESP: People's Party"
	replace Y`i'Party_Vote_Long = "SPA: Popular Party (PP)" if Y`i'Party_Vote == "Popular_SPA"

	replace Y`i'Party_Vote = "UnitedLeft_SPA" if BB`i'Party == "ES: United Left" | BB`i'Party == "ES: IU (Izquierda Unida) - United Left (1981: PCE, Communist party)" | BB`i'Party == "ESP: United Left"
	replace Y`i'Party_Vote_Long = "SPA: United Left (IU)" if Y`i'Party_Vote == "UnitedLeft_SPA"

	replace Y`i'Party_Vote = "Catalonia_SPA" if BB`i'Party == "ES: IC (Iniciativa per Catalunya) - Initiative for Catalunya" | BB`i'Party == "ES: Initiative For Catalonia"
	replace Y`i'Party_Vote_Long = "SPA: Initiative for Catalonia (ICV)" if Y`i'Party_Vote == "Catalonia_SPA"

	replace Y`i'Party_Vote = "WeCan_SPA" if BB`i'Party == "ES: We Can"
	replace Y`i'Party_Vote_Long = "SPA: We Can (Podemos)" if Y`i'Party_Vote == "WeCan_SPA"

	replace Y`i'Party_Vote = "WeCanTogether_SPA" if BB`i'Party == "ES: We can together"
	replace Y`i'Party_Vote_Long = "SPA: We Can Together (En Comu Podem)" if Y`i'Party_Vote == "WeCanTogether_SPA"

	replace Y`i'Party_Vote = "Tide_SPA" if BB`i'Party == "ES: In Tide"
	replace Y`i'Party_Vote_Long = "SPA: In Tide" if Y`i'Party_Vote == "Tide_SPA"

	replace Y`i'Party_Vote = "Compromise_SPA" if BB`i'Party == "ES: Compromise"
	replace Y`i'Party_Vote_Long = "SPA: Compromise" if Y`i'Party_Vote == "Compromise_SPA"

	replace Y`i'Party_Vote = "Citizens_SPA" if BB`i'Party == "ES: Citizens"
	replace Y`i'Party_Vote_Long = "SPA: Citizens" if Y`i'Party_Vote == "Citizens_SPA"

	replace Y`i'Party_Vote = "Green_SPA" if BB`i'Party == "ES: Verdes - The Green Party" | BB`i'Party == "ESP: The Green Party"
	replace Y`i'Party_Vote_Long = "SPA: The Green Party" if Y`i'Party_Vote == "Green_SPA"

	replace Y`i'Party_Vote = "ValenciaUnion_SPA" if BB`i'Party == "ES: Unio Valenciana - Valencian Union" | BB`i'Party == "ESP: Valencian Union"
	replace Y`i'Party_Vote_Long = "SPA: Valencian Union" if Y`i'Party_Vote == "ValenciaUnion_SPA"

	replace Y`i'Party_Vote = "Navarrese_SPA" if BB`i'Party == "ES: UPN (Union del Pueblo Navarro) - Union of the Navarrese People" | BB`i'Party == "ESP: Union of the Navarrese People"
	replace Y`i'Party_Vote_Long = "SPA: Union of the Navarrese People" if Y`i'Party_Vote == "Navarrese_SPA"

	replace Y`i'Party_Vote = "Communist_SPA" if BB`i'Party == "ES: Partido de los Trabajadores de Espana- Unidad Communista"
	replace Y`i'Party_Vote_Long = "SPA: Party of the Workers of Spain - United Communist" if Y`i'Party_Vote == "Communist_SPA"

	replace Y`i'Party_Vote = "UnionProgressDemocracy_SPA" if BB`i'Party == "ES: UPyD" | BB`i'Party == "ESP: Union Progress and Democracy"
	replace Y`i'Party_Vote_Long = "SPA: Union Progress Democracy (UPyD)" if Y`i'Party_Vote == "UnionProgressDemocracy_SPA"

	replace Y`i'Party_Vote = "RepubCatalonia_SPA" if BB`i'Party == "ES: Republican Party of Catalonia" | BB`i'Party == "ES: ERC (Esquerra Republicana de Catalunya) - Republican Left of Catalonia" | BB`i'Party == "ESP: Republican Left of Catalonia"
	replace Y`i'Party_Vote_Long = "SPA: Republican Party of Catalonia" if Y`i'Party_Vote == "RepubCatalonia_SPA"

	replace Y`i'Party_Vote = "BasqueNational_SPA" if BB`i'Party == "ES: PNV (Partido Nacionalista Vasco) - Basque Nationalist Party" | BB`i'Party == "ES: Basque Nationalist Party" | BB`i'Party == "ESP: Basque Nationalist Party"
	replace Y`i'Party_Vote_Long = "SPA: Basque Nationalist Party (PNV)" if Y`i'Party_Vote == "BasqueNational_SPA"

	replace Y`i'Party_Vote = "NewLeft_SPA" if BB`i'Party == "ES: PDNI (Partido Democratico de la Nueva Izquierda)"
	replace Y`i'Party_Vote_Long = "SPA: Democratic Party of the New Left (PDNI)" if Y`i'Party_Vote == "NewLeft_SPA"

	replace Y`i'Party_Vote = "Aragonese_SPA" if BB`i'Party == "ES: PAR (Partido Aragones Regionalista) - Aragonese Party" | BB`i'Party == "ES: Aragonese Party" | BB`i'Party == "ESP: Aragonese Party"
	replace Y`i'Party_Vote_Long = "SPA: Aragonese Party (PAR)" if Y`i'Party_Vote == "Aragonese_SPA"

	replace Y`i'Party_Vote = "Anadalusian_SPA" if BB`i'Party == "ES: PA (Partido Andalucista) - Andalusian Party" | BB`i'Party == "ESP: Andalusian Party"
	replace Y`i'Party_Vote_Long = "SPA: Andalusian Party" if Y`i'Party_Vote == "Anadalusian_SPA"

	replace Y`i'Party_Vote = "AndalusianProg_SPA" if BB`i'Party == "ESP: Andalusian Progressive Party"
	replace Y`i'Party_Vote_Long = "SPA: Andalusian Progressive Party" if Y`i'Party_Vote == "AndalusianProg_SPA"

	replace Y`i'Party_Vote = "Asturias_SPA" if BB`i'Party == "ES: Asturias Forum"
	replace Y`i'Party_Vote_Long = "SPA: Asturias Forum (FAC)" if Y`i'Party_Vote == "Asturias_SPA"

	replace Y`i'Party_Vote = "DemSocialCenter_SPA" if BB`i'Party == "ES: CDS (Centro Democratico y Social) - Democratic and Social Centre (1981: UCD)" | BB`i'Party == "ESP: Democratic and Social Centre" | BB`i'Party == "ESP: Centrist union"
	replace Y`i'Party_Vote_Long = "SPA: Democratic and Social Center (CDS)" if Y`i'Party_Vote == "DemSocialCenter_SPA"

	replace Y`i'Party_Vote = "AragoneseCouncil_SPA" if BB`i'Party == "ES: CHA (Chunta Aragonesista) - Aragonese Council" | BB`i'Party == "ESP: Aragonese Council"
	replace Y`i'Party_Vote_Long = "SPA: Aragonese Council (CHA)" if Y`i'Party_Vote == "AragoneseCouncil_SPA"

	replace Y`i'Party_Vote = "CatalanDems_SPA" if BB`i'Party == "ES: Catalan Democratic Party"
	replace Y`i'Party_Vote_Long = "SPA: Catalan Democratic Party (PDeCAT)" if Y`i'Party_Vote == "CatalanDems_SPA"

	replace Y`i'Party_Vote = "YesFuture_SPA" if BB`i'Party == "ES: Yes to the Future"
	replace Y`i'Party_Vote_Long = "SPA: Yes to the Future" if Y`i'Party_Vote == "YesFuture_SPA"

	replace Y`i'Party_Vote = "Convergence_SPA" if BB`i'Party == "ES: CiU (Convergencia i Unio) - Convergence and Union" | BB`i'Party == "ESP: Convergence and Union of Catalonia"
	replace Y`i'Party_Vote_Long = "SPA: Convergence and Union of Catalonia (CiU)" if Y`i'Party_Vote == "Convergence_SPA"

	replace Y`i'Party_Vote = "CanarianCoalition_SPA" if BB`i'Party == "ES: CC (Coalicion Canaria) - Canarian Coalition" | BB`i'Party == "ESP: Canarian Coalition"
	replace Y`i'Party_Vote_Long = "SPA: Canarian Coalition (CC)" if Y`i'Party_Vote == "CanarianCoalition_SPA"

	replace Y`i'Party_Vote = "BasqueUnite_SPA" if BB`i'Party == "ES: Basque Country United"
	replace Y`i'Party_Vote_Long = "SPA: Basque Country Gather/United (EH-Bildu)" if Y`i'Party_Vote == "BasqueUnite_SPA"

	replace Y`i'Party_Vote = "GalicianLeft_SPA" if BB`i'Party == "ES: Coalicaci Gallega CG  - Galician Left" | BB`i'Party == "ESP: Galician Left"
	replace Y`i'Party_Vote_Long = "SPA: Galician Left (CG)" if Y`i'Party_Vote == "GalicianLeft_SPA"

	replace Y`i'Party_Vote = "GalicianNational_SPA" if BB`i'Party == "ES: BNG (Bloque Nacionalista Galego) - Galician Nationalist Bloc" | BB`i'Party == "ES: Galician Nationalist Block" | BB`i'Party == "ESP: Galician Nationalist Bloc"
	replace Y`i'Party_Vote_Long = "SPA: Galician Nationalist Block (BNG)" if Y`i'Party_Vote == "GalicianNational_SPA"

	replace Y`i'Party_Vote = "BasqueNationalAction_SPA" if BB`i'Party == "ES: Acción Nacionalista Vasca - ANV"
	replace Y`i'Party_Vote_Long = "SPA: Basque Nationalist Action (ANV)" if Y`i'Party_Vote == "BasqueNationalAction_SPA"

	replace Y`i'Party_Vote = "BasqueSolidarity_SPA" if BB`i'Party == "ES: EA (Eusko Alkartasuna) - Basque Solidarity" | BB`i'Party == "ESP: Basque Solidarity"
	replace Y`i'Party_Vote_Long = "SPA: Basque Solidarity (EA)" if Y`i'Party_Vote == "BasqueSolidarity_SPA"

	replace Y`i'Party_Vote = "BasqueLeft_SPA" if BB`i'Party == "ES: EE (Euskadiko Ezkerra) - Basque Country Left" | BB`i'Party == "ESP: Basque Country Left"
	replace Y`i'Party_Vote_Long = "SPA: Basque Country Left (EE)" if Y`i'Party_Vote == "BasqueLeft_SPA"

	replace Y`i'Party_Vote = "BasqueBatasuna_SPA" if BB`i'Party == "ES: HB - EH (Herri Batasuna - Euskal Herritarrok)" | BB`i'Party == "ESP: Herri Batasuna / Eusko Alkatasuna" | BB`i'Party == "ESP: People's Union - Basque citizens"
	replace Y`i'Party_Vote_Long = "SPA: Herri Batasuna Basque Nationalist (HB - EH)" if Y`i'Party_Vote == "BasqueBatasuna_SPA"

	replace Y`i'Party_Vote = "NavarreYes_SPA" if BB`i'Party == "ESP: Navarre Yes"
	replace Y`i'Party_Vote_Long = "SPA: Navarre Yes Party" if Y`i'Party_Vote == "NavarreYes_SPA"

	replace Y`i'Party_Vote = "CanaryLeft_SPA" if BB`i'Party == "ESP: Canary Left"
	replace Y`i'Party_Vote_Long = "SPA: Canary Left" if Y`i'Party_Vote == "CanaryLeft_SPA"

	replace Y`i'Party_Vote = "Cantabrian_SPA" if BB`i'Party == "ESP: Cantabrian Regionalist Party"
	replace Y`i'Party_Vote_Long = "SPA: Cantabrian Regionalist Party" if Y`i'Party_Vote == "Cantabrian_SPA"

	replace Y`i'Party_Vote = "CataloniaGreens_SPA" if BB`i'Party == "ESP: Initiative for Catalonia Greens"
	replace Y`i'Party_Vote_Long = "SPA: Initiative for Catalonia Greens" if Y`i'Party_Vote == "CataloniaGreens_SPA"

	replace Y`i'Party_Vote = "RuizMateos_SPA" if BB`i'Party == "ESP: Ruiz Mateos Electoral Group"
	replace Y`i'Party_Vote_Long = "SPA: Ruiz Mateos Electoral Group" if Y`i'Party_Vote == "RuizMateos_SPA"

	replace Y`i'Party_Vote = "LeftNational_SPA" if BB`i'Party == "ESP: Left nationalists parties"
	replace Y`i'Party_Vote_Long = "SPA: Left nationalists parties" if Y`i'Party_Vote == "LeftNational_SPA"

	replace Y`i'Party_Vote = "RightNational_SPA" if BB`i'Party == "ESP: Right nationalists parties"
	replace Y`i'Party_Vote_Long = "SPA: Right nationalists parties" if Y`i'Party_Vote == "RightNational_SPA"

	/* Testing Spain*/
	tab Y`i'Party_Vote_Long if BB_Country=="Spain" & !missing(BB`i'Party) & Y_No`i'Party_Vote==0, missing
	tab BB`i'Party if BB_Country=="Spain" & Y_No`i'Party_Vote==0 & Y`i'Party_Vote_Long=="."

	count if Y`i'Party_Vote_Long=="." & BB_Country=="Spain" & !missing(BB`i'Party) & Y_No`i'Party_Vote==0
	assert r(N)==0
}

tab BB_Party BB_Year if Y_Party_Vote_Long=="." & BB_Country=="Spain" & !missing(BB_Party) & Y_No_Party_Vote==0
tab BB_2nd_Party BB_Year if Y_2nd_Party_Vote_Long=="." & BB_Country=="Spain" & !missing(BB_2nd_Party) & Y_No_2nd_Party_Vote==0
tab BB_Least_Party BB_Year if Y_Least_Party_Vote_Long=="." & BB_Country=="Spain" & !missing(BB_Least_Party) & Y_No_Least_Party_Vote==0
/**********************************
******* Belgium ********************
**********************************/
foreach party in BB_Party BB_2nd_Party BB_Least_Party {
    if "`party'" == "BB_Party" {
        local i "$prefix_BB_Party"
    }
    else if "`party'" == "BB_2nd_Party" {
        local i "$prefix_BB_2nd_Party"
    }
    else if "`party'" == "BB_Least_Party" {
        local i "$prefix_BB_Least_Party"
    }

	replace Y`i'Party_Vote = "FlemishGreen_BEL" if BB`i'Party == "BE: Agalev" | BB`i'Party == "BE: Groen!"
	replace Y`i'Party_Vote_Long = "BEL: Flemish Green Party" if Y`i'Party_Vote == "FlemishGreen_BEL"

	replace Y`i'Party_Vote = "ChristianDemFlemish_BEL" if BB`i'Party == "BE: Christen-Democratisch en Vlaams - CD&V" | BB`i'Party == "BE: CVP"
	replace Y`i'Party_Vote_Long = "BEL: Christian Democratic and Flemish (CD&V)" if Y`i'Party_Vote == "ChristianDemFlemish_BEL"

	replace Y`i'Party_Vote = "NewFlemish_BEL" if BB`i'Party == "BE: Nieuw-Vlaamse Alliantie - N-VA"
	replace Y`i'Party_Vote_Long = "BEL: New Flemish Alliance (N-VA)" if Y`i'Party_Vote == "NewFlemish_BEL"

	/* Party for Freedom and Progress preceded two Belgian parties, one French and one Dutch-speaking. There are 2 votes for the Party for Freedom and Progress 16 years after it dissolves. Looking at the original BB`i'Party, one was for the French-speaking Reformist Movement and the other for the Dutch-speaking Open Flemish Liberals and Democrats. I put the two votes for Freedom and Progress after its dissolution in their respective succeeded parties
	In 1990 there are also votes for the party, since it is for PVV also defining this as Open Flemish. Manifesto treats this as one party as well (21421) */

	replace Y`i'Party_Vote = "OpenFlemish_BEL" if BB`i'Party == "BE: Vlaamse Liberalen en Democraten Open - VLD" | BB`i'Party == "BE: VLD" | (BB`i'Party == "BE: PVV")
	replace Y`i'Party_Vote_Long = "BEL: Open Flemish Liberals and Democrats (VLD)" if Y`i'Party_Vote == "OpenFlemish_BEL"

	replace Y`i'Party_Vote = "Reformist_BEL" if BB`i'Party == "BE: Mouvement Réformateur - MR"  | (BB`i'Party == "BE: Libéral Démocrate - LiDé" & BB_Year >= 2009)
	replace Y`i'Party_Vote_Long = "BEL: Reformist Movement (MR)" if Y`i'Party_Vote == "Reformist_BEL"

	replace Y`i'Party_Vote = "Forward_BEL" if BB`i'Party == "BE: SP" | BB`i'Party == "BE: Socialisten en Progressieven Anders - SP.A"
	replace Y`i'Party_Vote_Long = "BEL: Forward (Flemish Socialist Party)" if Y`i'Party_Vote == "Forward_BEL"

	replace Y`i'Party_Vote = "FlemishImport_BEL" if BB`i'Party == "BE: Vlaams Belang"
	replace Y`i'Party_Vote_Long = "BEL: Flemish Importance (Vlaams Belang)" if Y`i'Party_Vote == "FlemishImport_BEL"

	replace Y`i'Party_Vote = "Ecolo_BEL" if BB`i'Party == "BE: Ecolo"
	replace Y`i'Party_Vote_Long = "BEL: Ecolo (Greens)" if Y`i'Party_Vote == "Ecolo_BEL"

	replace Y`i'Party_Vote = "HumanCentre_BEL" if BB`i'Party == "BE: Centre Démocrate Humaniste - CDH" | BB`i'Party == "BE: PSC"
	replace Y`i'Party_Vote_Long = "BEL: Humanist Democratic Centre (cdH)" if Y`i'Party_Vote == "HumanCentre_BEL"

	replace Y`i'Party_Vote = "Socialist_BEL" if BB`i'Party == "BE: PS"
	replace Y`i'Party_Vote_Long = "BEL: Socialist (French-speaking)" if Y`i'Party_Vote == "Socialist_BEL"

	replace Y`i'Party_Vote = "Agir_BEL" if BB`i'Party == "BE: AGIR"
	replace Y`i'Party_Vote_Long = "BEL: Agir (Act)" if Y`i'Party_Vote == "Agir_BEL"

	replace Y`i'Party_Vote = "NationalFront_BEL" if BB`i'Party == "BE: Front National"
	replace Y`i'Party_Vote_Long = "BEL: National Front" if Y`i'Party_Vote == "NationalFront_BEL"

	replace Y`i'Party_Vote = "LijstDedecker_BEL" if BB`i'Party == "BE: Lijst Dedecker"
	replace Y`i'Party_Vote_Long = "BEL: Libertarian, Direct, Democratic (Lijst Dedecker)" if Y`i'Party_Vote == "LijstDedecker_BEL"

	replace Y`i'Party_Vote = "NewPolitics_BEL" if BB`i'Party == "BE: PNPb"
	replace Y`i'Party_Vote_Long = "BEL: Party for a New Politics in Belgium (PNPb)" if Y`i'Party_Vote == "NewPolitics_BEL"

	/* This is a mistkae PRL - FDF - MCC did not exist in 1990. The party is coded this way in the data but the survey asked about PRL 
	see page 218 here PopulistVotingDropbox\doc\evs\EVS Wave 2 1990\ZA4461_q_be.pdf. In 1999 actually asked about this parties (page 83 and 1999 survey)*/
	replace Y`i'Party_Vote = "PRL_BEL" if BB`i'Party == "BE: PRL - FDF - MCC"  & BB_Year<1995
	replace Y`i'Party_Vote_Long = "BEL: Liberal Reformist Party (PRL)" if Y`i'Party_Vote == "PRL_BEL"

	replace Y`i'Party_Vote = "LiberalAlliance_BEL" if BB`i'Party == "BE: PRL - FDF - MCC" & 1995<BB_Year
	replace Y`i'Party_Vote_Long = "BEL: Liberal Alliance (PRL-FDF-MCC)" if Y`i'Party_Vote == "LiberalAlliance_BEL"


	replace Y`i'Party_Vote = "Workers_BEL" if BB`i'Party == "BE: PTB"
	replace Y`i'Party_Vote_Long = "BEL: Workers Party of Belgium (PTB)" if Y`i'Party_Vote == "Workers_BEL"

	replace Y`i'Party_Vote = "FlemishBlock_BEL" if BB`i'Party == "BE: Vlaams Blok"
	replace Y`i'Party_Vote_Long = "BEL: Flemish Block (VB)" if Y`i'Party_Vote == "FlemishBlock_BEL"

	replace Y`i'Party_Vote = "Vivant_BEL" if BB`i'Party == "BE: Vivant"
	replace Y`i'Party_Vote_Long = "BEL: Vivant Party" if Y`i'Party_Vote == "Vivant_BEL"

	replace Y`i'Party_Vote = "WOW_BEL" if BB`i'Party == "BE: WOW"
	replace Y`i'Party_Vote_Long = "BEL: WOW (Flemish 1994)" if Y`i'Party_Vote == "WOW_BEL"

	replace Y`i'Party_Vote = "SocialLibs_BEL" if BB`i'Party == "BE: Sociaal Liberale Partij - SLP"
	replace Y`i'Party_Vote_Long = "BEL: Social Liberal Party (SLP)" if Y`i'Party_Vote == "SocialLibs_BEL"

	replace Y`i'Party_Vote = "Walloon_BEL" if BB`i'Party == "BE: RW"
	replace Y`i'Party_Vote_Long = "BEL: Walloon Rally (RW)" if Y`i'Party_Vote == "Walloon_BEL"

	replace Y`i'Party_Vote = "PVDS-AE_BEL" if BB`i'Party == "BE: PVDS - AE"
	replace Y`i'Party_Vote_Long = "BEL: PVDS-AE" if Y`i'Party_Vote == "PVDS-AE_BEL"

	replace Y`i'Party_Vote = "PTB-UA_BEL" if BB`i'Party == "BE: Parti du Travail de Belgique - Unité Antifasciste - PTB-UA"
	replace Y`i'Party_Vote_Long = "BEL: Workers Party - Antifascist Union (PTB-UA)" if Y`i'Party_Vote == "PTB-UA_BEL"

	/* Defined as two separate parties in manifesto*/
	replace Y`i'Party_Vote = "VU_BEL" if BB`i'Party == "BE: VU - ID - 21" & BB_Year==1990
	replace Y`i'Party_Vote_Long = "BEL: People's Union" if Y`i'Party_Vote == "VU_BEL"

	replace Y`i'Party_Vote = "VU-ID-21_BEL" if BB`i'Party == "BE: VU - ID - 21" & BB_Year==1999
	replace Y`i'Party_Vote_Long = "BEL: People's Union Complete Democracy for the 21st century" if Y`i'Party_Vote == "VU-ID-21_BEL"


	/* Testing Belgium*/
	tab Y`i'Party_Vote_Long if BB_Country=="Belgium" & !missing(BB`i'Party) & Y_No`i'Party_Vote==0, missing
	tab BB`i'Party if BB_Country=="Belgium" & Y_No`i'Party_Vote==0 & Y`i'Party_Vote_Long=="."

	count if Y`i'Party_Vote_Long=="." & BB_Country=="Belgium" & !missing(BB`i'Party) & Y_No`i'Party_Vote==0
	assert r(N)==0
}

tab BB_Party BB_Year if Y_Party_Vote_Long=="." & BB_Country=="Belgium" & !missing(BB_Party) & Y_No_Party_Vote==0
tab BB_2nd_Party BB_Year if Y_2nd_Party_Vote_Long=="." & BB_Country=="Belgium" & !missing(BB_2nd_Party) & Y_No_2nd_Party_Vote==0
tab BB_Least_Party BB_Year if Y_Least_Party_Vote_Long=="." & BB_Country=="Belgium" & !missing(BB_Least_Party) & Y_No_Least_Party_Vote==0

/**********************************
******* Israel ********************
**********************************/
foreach party in BB_Party BB_2nd_Party BB_Least_Party {
    if "`party'" == "BB_Party" {
        local i "$prefix_BB_Party"
    }
    else if "`party'" == "BB_2nd_Party" {
        local i "$prefix_BB_2nd_Party"
    }
    else if "`party'" == "BB_Least_Party" {
        local i "$prefix_BB_Least_Party"
    }

	replace Y`i'Party_Vote = "Likud_ISR" if BB`i'Party == "ISR: Likud-National Liberal Movement"
	replace Y`i'Party_Vote_Long = "ISR: Likud-National Liberal Movement" if Y`i'Party_Vote == "Likud_ISR"

	replace Y`i'Party_Vote = "Shas_ISR" if BB`i'Party == "ISR: Shas"
	replace Y`i'Party_Vote_Long = "ISR: Shas" if Y`i'Party_Vote == "Shas_ISR"

	replace Y`i'Party_Vote = "Labor_ISR" if BB`i'Party == "ISR: Israeli Labor Party"
	replace Y`i'Party_Vote_Long = "ISR: Israeli Labor Party" if Y`i'Party_Vote == "Labor_ISR"

	replace Y`i'Party_Vote = "UnitedTorah_ISR" if BB`i'Party == "ISR: United Torah Judaism"
	replace Y`i'Party_Vote_Long = "ISR: United Torah Judaism" if Y`i'Party_Vote == "UnitedTorah_ISR"

	replace Y`i'Party_Vote = "Beiteinu_ISR" if BB`i'Party == "ISR: Israel is Our Home - Yisrael Beiteinu"
	replace Y`i'Party_Vote_Long = "ISR: Israel is Our Home - Yisrael Beiteinu" if Y`i'Party_Vote == "Beiteinu_ISR"

	replace Y`i'Party_Vote = "BaAliya_ISR" if BB`i'Party == "ISR: Israel for Immigration - Yisrael BaAliya"
	replace Y`i'Party_Vote_Long = "ISR: Israel for Immigration - Yisrael BaAliya" if Y`i'Party_Vote == "BaAliya_ISR"

	replace Y`i'Party_Vote = "Meretz_ISR" if BB`i'Party == "ISR: Meretz"
	replace Y`i'Party_Vote_Long = "ISR: Meretz" if Y`i'Party_Vote == "Meretz_ISR"

	replace Y`i'Party_Vote = "Center_ISR" if BB`i'Party == "ISR: Center Party"
	replace Y`i'Party_Vote_Long = "ISR: Center Party" if Y`i'Party_Vote == "Center_ISR"

	replace Y`i'Party_Vote = "Meimad_ISR" if BB`i'Party == "ISR: Meimad"
	replace Y`i'Party_Vote_Long = "ISR: Meimad" if Y`i'Party_Vote == "Meimad_ISR"

	replace Y`i'Party_Vote = "ArabDem_ISR" if BB`i'Party == "ISR: Arab Democratic Party"
	replace Y`i'Party_Vote_Long = "ISR: Arab Democratic Party (Mada)" if Y`i'Party_Vote == "ArabDem_ISR"

	replace Y`i'Party_Vote = "NationalUnion_ISR" if BB`i'Party == "ISR: National Union"
	replace Y`i'Party_Vote_Long = "ISR: National Union" if Y`i'Party_Vote == "NationalUnion_ISR"

	replace Y`i'Party_Vote = "OneNation_ISR" if BB`i'Party == "ISR: One Nation"
	replace Y`i'Party_Vote_Long = "ISR: One Nation" if Y`i'Party_Vote == "OneNation_ISR"

	replace Y`i'Party_Vote = "Shinui_ISR" if BB`i'Party == "ISR: Shinui - Change"
	replace Y`i'Party_Vote_Long = "ISR: Shinui - Change" if Y`i'Party_Vote == "Shinui_ISR"

	replace Y`i'Party_Vote = "Religious_ISR" if BB`i'Party == "ISR: National religious party"
	replace Y`i'Party_Vote_Long = "ISR: National religious party" if Y`i'Party_Vote == "Religious_ISR"

	replace Y`i'Party_Vote = "Balad_ISR" if BB`i'Party == "ISR: National Democratic Assembly"
	replace Y`i'Party_Vote_Long = "ISR: Balad (National Democratic Assembly)" if Y`i'Party_Vote == "Balad_ISR"

	replace Y`i'Party_Vote = "Hadash_ISR" if BB`i'Party == "ISR: The Democratic Front for Peace and Equality"
	replace Y`i'Party_Vote_Long = "ISR: Hadash (The Democratic Front for Peace and Equality)" if Y`i'Party_Vote == "Hadash_ISR"

	/* Testing Israel*/
	tab Y`i'Party_Vote_Long if BB_Country=="Israel" & !missing(BB`i'Party) & Y_No`i'Party_Vote==0, missing
	tab BB`i'Party if BB_Country=="Israel" & Y_No`i'Party_Vote==0 & Y`i'Party_Vote_Long=="."

	count if Y`i'Party_Vote_Long=="." & BB_Country=="Israel" & !missing(BB`i'Party) & Y_No`i'Party_Vote==0
	assert r(N)==0
}

tab BB_Party BB_Year if Y_Party_Vote_Long=="." & BB_Country=="Israel" & !missing(BB_Party) & Y_No_Party_Vote==0
tab BB_2nd_Party BB_Year if Y_2nd_Party_Vote_Long=="." & BB_Country=="Israel" & !missing(BB_2nd_Party) & Y_No_2nd_Party_Vote==0
tab BB_Least_Party BB_Year if Y_Least_Party_Vote_Long=="." & BB_Country=="Israel" & !missing(BB_Least_Party) & Y_No_Least_Party_Vote==0
/**********************************
******* Bulgaria ********************
**********************************/

foreach party in BB_Party BB_2nd_Party BB_Least_Party {
    if "`party'" == "BB_Party" {
        local i "$prefix_BB_Party"
    }
    else if "`party'" == "BB_2nd_Party" {
        local i "$prefix_BB_2nd_Party"
    }
    else if "`party'" == "BB_Least_Party" {
        local i "$prefix_BB_Least_Party"
    }

	replace Y`i'Party_Vote = "Socialist_BGR" if BB`i'Party == "BG: Bulgarian Socialist Party (BSP)" | BB`i'Party == "BGR: Bulgarian Socialist Party" | BB`i'Party == "BG: Balgarska Socialisticheska Partiya (BSP) - Bulgarian Socialist Party"
	replace Y`i'Party_Vote_Long = "BGR: Bulgarian Socialist Party (BSP)" if Y`i'Party_Vote == "Socialist_BGR"

	replace Y`i'Party_Vote = "MovementRights_BGR" if BB`i'Party == "BGR: Movement for Rights and Freedoms" | BB`i'Party == "BG: Dvijenie za Prava i Svobodi (DPS) - Movement for Rights and Freedom" | BB`i'Party == "BG: Movement for Rights and Freedoms (MRF)" | BB`i'Party == "BG: MSRF - Turkish Movement for Rights and Freedom -"
	replace Y`i'Party_Vote_Long = "BGR: Movement for Rights and Freedoms (MRF)" if Y`i'Party_Vote == "MovementRights_BGR"

	replace Y`i'Party_Vote = "Democrat_BGR" if BB`i'Party == "BG: Democratic Party"
	replace Y`i'Party_Vote_Long = "BGR: Democratic Party" if Y`i'Party_Vote == "Democrat_BGR"

	replace Y`i'Party_Vote = "AgrarianNational_BGR" if BB`i'Party == "BG: Balgarski Zemedelski Naroden Sajuz (BZNS) - Bulgarian Agrarian National Union" | BB`i'Party == "BGR: Bulgarian Agrarian National Union" | BB`i'Party == "BG: BAP - Bulgarian Agrarian Party"
	replace Y`i'Party_Vote_Long = "BGR: Agrarian National Union (BZNS)" if Y`i'Party_Vote == "AgrarianNational_BGR"

	replace Y`i'Party_Vote = "Attack_BGR" if BB`i'Party == "BGR: Attack" | BB`i'Party == "BG: Attack" | BB`i'Party == "BG: National Union Attack"
	replace Y`i'Party_Vote_Long = "BGR: Attack" if Y`i'Party_Vote == "Attack_BGR"

	replace Y`i'Party_Vote = "Communist_BGR" if BB`i'Party == "BG: Bulgarian Communist Party"
	replace Y`i'Party_Vote_Long = "BGR: Communist Party" if Y`i'Party_Vote == "Communist_BGR"

	replace Y`i'Party_Vote = "SocialDems_BGR" if BB`i'Party == "BG: Bulgarian Social-Democratic Party  (BSDP)" | BB`i'Party == "BGR: Bulgarian Social-Democratic Party"
	replace Y`i'Party_Vote_Long = "BGR: Bulgarian Social-Democratic Party (BSDP)" if Y`i'Party_Vote == "SocialDems_BGR"

	replace Y`i'Party_Vote = "Womens_BGR" if BB`i'Party == "BG: Bulgarian Women's Party"
	replace Y`i'Party_Vote_Long = "BGR: Bulgarian Women's Party" if Y`i'Party_Vote == "Womens_BGR"

	replace Y`i'Party_Vote = "EuroLeft_BGR" if BB`i'Party == "BG: Bulgarska Evrolevitza (BEL) - The Euro-Left" | BB`i'Party == "BGR: Bulgarska Evrolevitza - The Euro-"
	replace Y`i'Party_Vote_Long = "BGR: The Euro-Left (BEL)" if Y`i'Party_Vote == "EuroLeft_BGR"

	replace Y`i'Party_Vote = "BusinessBlock_BGR" if BB`i'Party == "BG: Bulgarska biznes blok (BBB) - Bulgarian Business Block" | BB`i'Party == "BGR: Bulgarian Business Block"
	replace Y`i'Party_Vote_Long = "BGR: Bulgarian Business Block" if Y`i'Party_Vote == "BusinessBlock_BGR"

	replace Y`i'Party_Vote = "UnionDemForces_BGR" if BB`i'Party == "BG: Suyuz na demokratichnite sili (SDS) - Union of Democratic Forces" | BB`i'Party == "BG: Union of Democratic Forces" | BB`i'Party == "BGR: Union of Democratic Forces"
	replace Y`i'Party_Vote_Long = "BGR: Union of Democratic Forces (SDS)" if Y`i'Party_Vote == "UnionDemForces_BGR"

	replace Y`i'Party_Vote = "NewTimes_BGR" if BB`i'Party == "BGR: New Times" | BB`i'Party == "BG: The New Time"
	replace Y`i'Party_Vote_Long = "BGR: The New Time" if Y`i'Party_Vote == "NewTimes_BGR"

	replace Y`i'Party_Vote = "GreenParty_BGR" if BB`i'Party == "BG: Green Party of Bulgaria"
	replace Y`i'Party_Vote_Long = "BGR: Green Party of Bulgaria" if Y`i'Party_Vote == "GreenParty_BGR"

	replace Y`i'Party_Vote = "TheGreens_BGR" if BB`i'Party == "BG: Party The Greens"
	replace Y`i'Party_Vote_Long = "BGR: The Greens (Green Movement)" if Y`i'Party_Vote == "TheGreens_BGR"

	replace Y`i'Party_Vote = "DemocratsStrong_BGR" if BB`i'Party == "BGR: Democrats for a Strong Bulgaria" | BB`i'Party == "BG: Democrats for Strong Bulgaria" | BB`i'Party == "BG: Democrats for a Strong Bulgaria"
	replace Y`i'Party_Vote_Long = "BGR: Democrats for a Strong Bulgaria" if Y`i'Party_Vote == "DemocratsStrong_BGR"

	replace Y`i'Party_Vote = "Euroroma_BGR" if BB`i'Party == "BG: Euroroma - Tzvetelin Kanchev" | BB`i'Party == "BGR: Euroroma - Tzvetelin Kanchev"
	replace Y`i'Party_Vote_Long = "BGR: Euroroma - Tzvetelin Kanchev" if Y`i'Party_Vote == "Euroroma_BGR"

	replace Y`i'Party_Vote = "EuroDevelop_BGR" if BB`i'Party == "BG: Citizens for European Development of Bulgaria" | BB`i'Party == "BGR: Citizens for European Development of Bulgaria"
	replace Y`i'Party_Vote_Long = "BGR: Citizens for European Development of Bulgaria" if Y`i'Party_Vote == "EuroDevelop_BGR"

	replace Y`i'Party_Vote = "Georges_BGR" if BB`i'Party == "BG: Georgiovden Movement" | BB`i'Party == "BGR: George's Day Movement"
	replace Y`i'Party_Vote_Long = "BGR: George's Day Movement" if Y`i'Party_Vote == "Georges_BGR"

	replace Y`i'Party_Vote = "MovementStability_BGR" if BB`i'Party == "BG: National Movement for Stability and Progress" | BB`i'Party == "BGR: National Movement (Simeon II) for Stability and Progres" | BB`i'Party == "BG: Monarchist  - National Movement Simeon II (2006)"
	replace Y`i'Party_Vote_Long = "BGR: National Movement (Simeon II) for Stability and Progress (NDSV)" if Y`i'Party_Vote == "MovementStability_BGR"

	replace Y`i'Party_Vote = "NationalFront_BGR" if BB`i'Party == "BG: National Front for the Salvation of Bulgaria"
	replace Y`i'Party_Vote_Long = "BGR: National Front for the Salvation of Bulgaria" if Y`i'Party_Vote == "NationalFront_BGR"

	replace Y`i'Party_Vote = "National_BGR" if BB`i'Party == "BG: VMRO – Bulgarian National Movement"
	replace Y`i'Party_Vote_Long = "BGR: Bulgarian National Movement (IMRO)" if Y`i'Party_Vote == "National_BGR"

	replace Y`i'Party_Vote = "Macedonian_BGR" if BB`i'Party == "BG: Vnatrena Makedonska Revol. Org./Dem. Partija (VMRO/DPMNE) Internal Macedonian Revolutionary Organization" | BB`i'Party == "BGR: Internal Macedonian Revolutionary Organization"
	replace Y`i'Party_Vote_Long = "BGR: Internal Macedonian Revolutionary Organization" if Y`i'Party_Vote == "Macedonian_BGR"

	replace Y`i'Party_Vote = "Roma_BGR" if BB`i'Party == "BG: Roma party"
	replace Y`i'Party_Vote_Long = "BGR: Roma Party" if Y`i'Party_Vote == "Roma_BGR"

	replace Y`i'Party_Vote = "Citizens_BGR" if BB`i'Party == "BG:  Movement Bulgaria of the Citizens" | BB`i'Party == "BG: Movement Bulgaria of the Citizens"
	replace Y`i'Party_Vote_Long = "BGR: Movement Bulgaria of the Citizens" if Y`i'Party_Vote == "Citizens_BGR"

	replace Y`i'Party_Vote = "Revival_BGR" if BB`i'Party == "BG:  Alternative for Bulgarian Revival"
	replace Y`i'Party_Vote_Long = "BGR: Alternative for Bulgarian Revival" if Y`i'Party_Vote == "Revival_BGR"

	replace Y`i'Party_Vote = "DemsResponsibility_BGR" if BB`i'Party == "BG: DOST Democrats for Responsibility, Freedom, Tolerance"
	replace Y`i'Party_Vote_Long = "BGR: Democrats for Responsibility, Freedom, Tolerance (DOST)" if Y`i'Party_Vote == "DemsResponsibility_BGR"

	replace Y`i'Party_Vote = "FreeDemocrats_BGR" if BB`i'Party == "BGR: Union of Free Democrats" | BB`i'Party == "BG: Party of Free Democrats"
	replace Y`i'Party_Vote_Long = "BGR: Union of Free Democrats" if Y`i'Party_Vote == "FreeDemocrats_BGR"

	replace Y`i'Party_Vote = "Volya_BGR" if BB`i'Party == "BG: Political Party VOLYA"
	replace Y`i'Party_Vote_Long = "BGR: Political Party VOLYA" if Y`i'Party_Vote == "Volya_BGR"

	replace Y`i'Party_Vote = "Peoples_BGR" if BB`i'Party == "BG: Peoples Union" | BB`i'Party == "BGR: Agrarian People's Union"
	replace Y`i'Party_Vote_Long = "BGR: People's Union" if Y`i'Party_Vote == "Peoples_BGR"

	replace Y`i'Party_Vote = "Yes_BGR" if BB`i'Party == "BG: Yes, Bulgaria"
	replace Y`i'Party_Vote_Long = "BGR: Yes, Bulgaria" if Y`i'Party_Vote == "Yes_BGR"

	replace Y`i'Party_Vote = "Movement21_BGR" if BB`i'Party == "BG: Movement 21"
	replace Y`i'Party_Vote_Long = "BGR: Movement 21" if Y`i'Party_Vote == "Movement21_BGR"

	replace Y`i'Party_Vote = "LibDem_BGR" if BB`i'Party == "BG: Liberal-Democratic Union"
	replace Y`i'Party_Vote_Long = "BGR: Liberal-Democratic Union" if Y`i'Party_Vote == "LibDem_BGR"

	replace Y`i'Party_Vote = "Alternative_BGR" if BB`i'Party == "BG: Alternative for Bulgarian Revival"
	replace Y`i'Party_Vote_Long = "BGR: Alternative for Bulgarian Revival" if Y`i'Party_Vote == "Alternative_BGR"


	replace Y`i'Party_Vote = "BulgarianAgrarian_BGR" if BB`i'Party == "BG: Bulgarian Agrarian People's Union Alexander Stambolijski"
	replace Y`i'Party_Vote_Long = "BGR: Bulgarian Agrarian People's Union" if Y`i'Party_Vote == "BulgarianAgrarian_BGR"

	/* Testing Bulgaria*/
	tab Y`i'Party_Vote_Long if BB_Country=="Bulgaria" & !missing(BB`i'Party) & Y_No`i'Party_Vote==0, missing
	tab BB`i'Party if BB_Country=="Bulgaria" & Y_No`i'Party_Vote==0 & Y`i'Party_Vote_Long=="."

	count if Y`i'Party_Vote_Long=="." & BB_Country=="Bulgaria" & !missing(BB`i'Party) & Y_No`i'Party_Vote==0
	assert r(N)==0
}

tab BB_Party BB_Year if Y_Party_Vote_Long=="." & BB_Country=="Bulgaria" & !missing(BB_Party) & Y_No_Party_Vote==0
tab BB_2nd_Party BB_Year if Y_2nd_Party_Vote_Long=="." & BB_Country=="Bulgaria" & !missing(BB_2nd_Party) & Y_No_2nd_Party_Vote==0
tab BB_Least_Party BB_Year if Y_Least_Party_Vote_Long=="." & BB_Country=="Bulgaria" & !missing(BB_Least_Party) & Y_No_Least_Party_Vote==0
/**********************************
******* Croatia ********************
**********************************/
foreach party in BB_Party BB_2nd_Party BB_Least_Party {
    if "`party'" == "BB_Party" {
        local i "$prefix_BB_Party"
    }
    else if "`party'" == "BB_2nd_Party" {
        local i "$prefix_BB_2nd_Party"
    }
    else if "`party'" == "BB_Least_Party" {
        local i "$prefix_BB_Least_Party"
    }

	replace Y`i'Party_Vote = "DemUnion_HRV" if BB`i'Party == "HRV: Croatian Democratic Union" | BB`i'Party == "HR: Croatian democratic union" | BB`i'Party == "HR: Hrvatska Demokratska Zajednica (HDZ)"
	replace Y`i'Party_Vote_Long = "HRV: Croatian Democratic Union (HDZ)" if Y`i'Party_Vote == "DemUnion_HRV"

	replace Y`i'Party_Vote = "SocialDems_HRV" if BB`i'Party == "HR: Social democratic party of Croatia" | BB`i'Party == "HR: Socijaldemokratska Partija Hrvatske (SDP/SDPH)" | BB`i'Party == "HRV: Social Democratic Party of Croatia"
	replace Y`i'Party_Vote_Long = "HRV: Social Democratic Party of Croatia (SDP)" if Y`i'Party_Vote == "SocialDems_HRV"

	replace Y`i'Party_Vote = "Bridge_HRV" if BB`i'Party == "HR: Bridge of Independent Lists"
	replace Y`i'Party_Vote_Long = "HRV: Bridge of Independent Lists (Most)" if Y`i'Party_Vote =="Bridge_HRV"

	replace Y`i'Party_Vote = "Peasant_HRV" if BB`i'Party == "HRV: Croatian Peasant Party" | BB`i'Party == "HR: Croatian peasant party" | BB`i'Party == "HR: Hrvatska Seljacka Stranka (HSS)"
	replace Y`i'Party_Vote_Long = "HRV: Croatian Peasant Party (HSS)" if Y`i'Party_Vote == "Peasant_HRV"

	replace Y`i'Party_Vote = "Istrian_HRV" if BB`i'Party == "HR: Istarski Demokratski Sabor (IDS)" | BB`i'Party == "HR: Istrian democratic assembly" | BB`i'Party == "HRV: Istrian Democratic Assembly"
	replace Y`i'Party_Vote_Long = "HRV: Istrian democratic assembly (IDS)" if Y`i'Party_Vote == "Istrian_HRV"

	replace Y`i'Party_Vote = "SocialLiberal_HRV" if BB`i'Party == "HRV: Croatian Social Liberal Party" | BB`i'Party == "HR: Croatian social liberal party" | BB`i'Party == "HR: Hrvatska Socijalno Liberalna Stranka (HSLS)"
	replace Y`i'Party_Vote_Long = "HRV: Croatian Social Liberal Party (HSLS)" if Y`i'Party_Vote == "SocialLiberal_HRV"

	replace Y`i'Party_Vote = "IndependentSerb_HRV" if BB`i'Party == "HR: SDSS" | BB`i'Party == "HR: Independent democratic Serbian party"
	replace Y`i'Party_Vote_Long = "HRV: Independent Democratic Serbian Party (SDSS)" if Y`i'Party_Vote == "IndependentSerb_HRV"

	replace Y`i'Party_Vote = "Peoples_HRV" if BB`i'Party == "HRV: Croatian People's Party – Liberal Democrats" | BB`i'Party == "HR: Croatian people's party" | BB`i'Party == "HR: Hrvatska Narodna Stranka (HNS)"
	replace Y`i'Party_Vote_Long = "HRV: Croatian People's Party – Liberal Democrats (HNS)" if Y`i'Party_Vote == "Peoples_HRV"

	replace Y`i'Party_Vote = "Pensioners_HRV" if BB`i'Party == "HR: HSU" | BB`i'Party == "HR: Croatian party of pensioners"
	replace Y`i'Party_Vote_Long = "HRV: Croatian Party of Pensioners (HSU)" if Y`i'Party_Vote == "Pensioners_HRV"

	replace Y`i'Party_Vote = "Rights_HRV" if BB`i'Party == "HR: Hrvatska Stranka Prava (HSP)" | BB`i'Party == "HR: Croatian party of rights" | BB`i'Party == "HR: H SP" | BB`i'Party == "HRV: Croatian Party of Rights"
	replace Y`i'Party_Vote_Long = "HRV: Croatian Party of Rights (HSP)" if Y`i'Party_Vote == "Rights_HRV"

	replace Y`i'Party_Vote = "Rights1861_HRV" if BB`i'Party == "HR: Hrvatska Stranka Prava - 1861 (HSP 1861)" | BB`i'Party == "HRV: Croatian Party of Right of 1861"
	replace Y`i'Party_Vote_Long = "HRV: Croatian Party of Right of 1861 (HSP 1861)" if Y`i'Party_Vote == "Rights1861_HRV"

	replace Y`i'Party_Vote = "Liberal_HRV" if BB`i'Party == "HR: LS"
	replace Y`i'Party_Vote_Long = "HRV: Liberal Party (LS)" if Y`i'Party_Vote == "Liberal_HRV"

	replace Y`i'Party_Vote = "GorskiKotar_HRV" if BB`i'Party == "HR: Gorski-Kotar alliance" | BB`i'Party == "HR: PGS" | BB`i'Party == "HR: Gorski-Kotar alliance "
	replace Y`i'Party_Vote_Long = "HRV: Alliance of Primorje-Gorski Kotar (PGS)" if Y`i'Party_Vote == "GorskiKotar_HRV"

	replace Y`i'Party_Vote = "IndependentDems_HRV" if BB`i'Party == "HR: Hrvatski Nezavisni Demokrati (HND)" | BB`i'Party == "HRV: Croatian Independent Democrats"
	replace Y`i'Party_Vote_Long = "HRV: Croatian Independent Democrats (HND)" if Y`i'Party_Vote == "IndependentDems_HRV"

	replace Y`i'Party_Vote = "ChristianDems_HRV" if BB`i'Party == "HR: Hrvatska Krscanska Demokratska Unija (HKDU)" | BB`i'Party == "HRV: Croatian Christian Democratic Union" | BB`i'Party == "HR: HKDS"
	replace Y`i'Party_Vote_Long = "HRV: Croatian Christian Democratic Union (HKDU)" if Y`i'Party_Vote == "ChristianDems_HRV"

	replace Y`i'Party_Vote = "Dalmatian_HRV" if BB`i'Party == "HR: Dalmatinska Akcija (DA)" | BB`i'Party == "HRV: Dalmatian Action"
	replace Y`i'Party_Vote_Long = "HRV: Dalmatian Action (DA)" if Y`i'Party_Vote == "Dalmatian_HRV"

	replace Y`i'Party_Vote = "Slavonia_HRV" if BB`i'Party == "HR: Croatian democratic alliance of Slavonia and Baranja" | BB`i'Party == "HR: Croatian Democratic Alliance of Slavonia and Baranja HDSSB" | BB`i'Party == "HRV: Slavonia-Baranja Croatian Party"
	replace Y`i'Party_Vote_Long = "HRV: Slavonia-Baranja Croatian Party" if Y`i'Party_Vote == "Slavonia_HRV"

	replace Y`i'Party_Vote = "SocialDemUnion_HRV" if BB`i'Party == "HRV: Social Democratic Union / Left of Croatia" | BB`i'Party == "HR: Socijalno-demokratska Unija Hrvatske (SDU)" | BB`i'Party == "HR: Leftists of Croatia"
	replace Y`i'Party_Vote_Long = "HRV: Social Democratic Union / Left of Croatia" if Y`i'Party_Vote == "SocialDemUnion_HRV"

	replace Y`i'Party_Vote = "Green_HRV" if BB`i'Party == "HR: Green Party" | BB`i'Party == "HR: Green Party - Green Alternative" | BB`i'Party == "HR: Greens of Croatia"
	replace Y`i'Party_Vote_Long = "HRV: Green Party" if Y`i'Party_Vote == "Green_HRV"

	replace Y`i'Party_Vote = "Action_HRV" if BB`i'Party == "HR: Akcija Socijaldemokrata Hrvatske (ASH)" | BB`i'Party == "HRV: Akcija Socijaldemokrata Hrvatske"
	replace Y`i'Party_Vote_Long = "HRV: Social Democratic Action of Croatia (ASH)" if Y`i'Party_Vote == "Action_HRV"

	replace Y`i'Party_Vote = "Milan_HRV" if BB`i'Party == "HR: Bandić Milan 365"
	replace Y`i'Party_Vote_Long = "HRV: Bandić Milan 365 Labor and Solidarity Party" if Y`i'Party_Vote == "Milan_HRV"

	replace Y`i'Party_Vote = "HumanShield_HRV" if BB`i'Party == "HR: Human shield"
	replace Y`i'Party_Vote_Long = "HRV: Human Shield" if Y`i'Party_Vote == "HumanShield_HRV"

	replace Y`i'Party_Vote = "Popular_HRV" if BB`i'Party == "HR: HPS"
	replace Y`i'Party_Vote_Long = "HRV: Croatian Popular Party (HPS)" if Y`i'Party_Vote == "Popular_HRV"

	replace Y`i'Party_Vote = "Smart_HRV" if BB`i'Party == "HR: Smart"
	replace Y`i'Party_Vote_Long = "HRV: Smart (2017)" if Y`i'Party_Vote == "Smart_HRV"

	replace Y`i'Party_Vote = "SocialistLabor_HRV" if BB`i'Party == "HR: Socialist Labour Party of Croatia"
	replace Y`i'Party_Vote_Long = "HRV: Socialist Labour Party of Croatia" if Y`i'Party_Vote == "SocialistLabor_HRV"

	/* Testing Croatia*/
	tab Y`i'Party_Vote_Long if BB_Country=="Croatia" & !missing(BB`i'Party) & Y_No`i'Party_Vote==0, missing
	tab BB`i'Party if BB_Country=="Croatia" & Y_No`i'Party_Vote==0 & Y`i'Party_Vote_Long=="."

	count if Y`i'Party_Vote_Long=="." & BB_Country=="Croatia" & !missing(BB`i'Party) & Y_No`i'Party_Vote==0
	assert r(N)==0
}

tab BB_Party BB_Year if Y_Party_Vote_Long=="." & BB_Country=="Croatia" & !missing(BB_Party) & Y_No_Party_Vote==0
tab BB_2nd_Party BB_Year if Y_2nd_Party_Vote_Long=="." & BB_Country=="Croatia" & !missing(BB_2nd_Party) & Y_No_2nd_Party_Vote==0
tab BB_Least_Party BB_Year if Y_Least_Party_Vote_Long=="." & BB_Country=="Croatia" & !missing(BB_Least_Party) & Y_No_Least_Party_Vote==0
/**********************************
******* Cyprus ********************
**********************************/
foreach party in BB_Party BB_2nd_Party BB_Least_Party {
    if "`party'" == "BB_Party" {
        local i "$prefix_BB_Party"
    }
    else if "`party'" == "BB_2nd_Party" {
        local i "$prefix_BB_2nd_Party"
    }
    else if "`party'" == "BB_Least_Party" {
        local i "$prefix_BB_Least_Party"
    }

	replace Y`i'Party_Vote = "DemRally_CYP" if BB`i'Party == "CYP: Democratic Rally" | BB`i'Party == "CY: DISY"
	replace Y`i'Party_Vote_Long = "CYP: Democratic Rally (DISY)" if Y`i'Party_Vote == "DemRally_CYP"

	replace Y`i'Party_Vote = "Progressive_CYP" if BB`i'Party == "CYP: Progressive Party of Working People" | BB`i'Party == "CY: AKEL"
	replace Y`i'Party_Vote_Long = "CYP: Progressive Party of Working People (AKEL)" if Y`i'Party_Vote == "Progressive_CYP"

	replace Y`i'Party_Vote = "Democratic_CYP" if BB`i'Party == "CY: DIKO" | BB`i'Party == "CYP: The Democratic Party"
	replace Y`i'Party_Vote_Long = "CYP: The Democratic Party (DIKO)" if Y`i'Party_Vote == "Democratic_CYP"

	replace Y`i'Party_Vote = "MovementDemocracy_CYP" if BB`i'Party == "CY: Movement for Social Democracy - EDEK" | BB`i'Party == "CYP: Movement of Social Democrats"
	replace Y`i'Party_Vote_Long = "CYP: Movement for Social Democracy (EDEK)" if Y`i'Party_Vote == "MovementDemocracy_CYP"

	replace Y`i'Party_Vote = "Ecological_CYP" if BB`i'Party == "CY: Ecological and Environmental Movement" | BB`i'Party == "CYP: Ecological and Environmental Movement"
	replace Y`i'Party_Vote_Long = "CYP: Ecological and Environmental Movement" if Y`i'Party_Vote == "Ecological_CYP"

	replace Y`i'Party_Vote = "European_CYP" if BB`i'Party == "CY: European Party - EYROKO" | BB`i'Party == "CYP: European Party"
	replace Y`i'Party_Vote_Long = "CYP: European Party (EYROKO)" if Y`i'Party_Vote == "European_CYP"

	replace Y`i'Party_Vote = "UnitedDems_CYP" if BB`i'Party == "CYP: United Democrats"
	replace Y`i'Party_Vote_Long = "CYP: United Democrats" if Y`i'Party_Vote == "UnitedDems_CYP"

	replace Y`i'Party_Vote = "NationalFront_CYP" if BB`i'Party == "CYP: The National Popular Front"
	replace Y`i'Party_Vote_Long = "CYP: The National Popular Front" if Y`i'Party_Vote == "NationalFront_CYP"

	replace Y`i'Party_Vote = "Citizens_CYP" if BB`i'Party == "CYP: Citizens' Alliance"
	replace Y`i'Party_Vote_Long = "CYP: Citizens' Alliance" if Y`i'Party_Vote == "Citizens_CYP"
	tab BB`i'Party if BB_Country=="Cyprus" & Y`i'Party_Vote=="." & Y_No`i'Party_Vote !=1

	replace Y`i'Party_Vote = "Animal_CYP" if BB`i'Party == "CYP: Animal Party Cyprus"
	replace Y`i'Party_Vote_Long = "CYP: Animal Party Cyprus" if Y`i'Party_Vote == "Animal_CYP"

	replace Y`i'Party_Vote = "CommunalLib_CYP" if BB`i'Party == "CYP: Communal Liberation Party"
	replace Y`i'Party_Vote_Long = "CYP: Communal Liberation Party" if Y`i'Party_Vote == "CommunalLib_CYP"

	replace Y`i'Party_Vote = "DemAlignment_CYP" if BB`i'Party == "CYP: Democratic Alignment"
	replace Y`i'Party_Vote_Long = "CYP: Democratic Alignment" if Y`i'Party_Vote == "DemAlignment_CYP"

	replace Y`i'Party_Vote = "FightingDems_CYP" if BB`i'Party == "CYP: Fighting Democrats"
	replace Y`i'Party_Vote_Long = "CYP: Fighting Democrats" if Y`i'Party_Vote == "FightingDems_CYP"

	replace Y`i'Party_Vote = "NationalUnity_CYP" if BB`i'Party == "CYP: National Unity Party"
	replace Y`i'Party_Vote_Long = "CYP: National Unity Party" if Y`i'Party_Vote == "NationalUnity_CYP"

	replace Y`i'Party_Vote = "Peace_CYP" if BB`i'Party == "CYP: Peace and Democracy Movement"
	replace Y`i'Party_Vote_Long = "CYP: Peace and Democracy Movement" if Y`i'Party_Vote == "Peace_CYP"

	replace Y`i'Party_Vote = "RepublicanTurkish_CYP" if BB`i'Party == "CYP: Republican Turkish Party"
	replace Y`i'Party_Vote_Long = "CYP: Republican Turkish Party" if Y`i'Party_Vote == "RepublicanTurkish_CYP"

	replace Y`i'Party_Vote = "CommunalDem_CYP" if BB`i'Party == "CYP: The Communal Democracy Party"
	replace Y`i'Party_Vote_Long = "CYP: The Communal Democracy Party" if Y`i'Party_Vote == "CommunalDem_CYP"

	replace Y`i'Party_Vote = "Peoples_CYP" if BB`i'Party == "CYP: The People's Party"
	replace Y`i'Party_Vote_Long = "CYP: The People's Party" if Y`i'Party_Vote == "Peoples_CYP"

	replace Y`i'Party_Vote = "UnitedCyprus_CYP" if BB`i'Party == "CYP: United Cyprus Party"
	replace Y`i'Party_Vote_Long = "CYP: United Cyprus Party" if Y`i'Party_Vote == "UnitedCyprus_CYP"

	replace Y`i'Party_Vote = "Rebirth_CYP" if BB`i'Party == "CYP: Rebirth Party"
	replace Y`i'Party_Vote_Long = "CYP: Rebirth Party" if Y`i'Party_Vote == "Rebirth_CYP"

	replace Y`i'Party_Vote = "Solidarity_CYP" if BB`i'Party == "CYP: The Solidarity Movement"
	replace Y`i'Party_Vote_Long = "CYP: The Solidarity Movement" if Y`i'Party_Vote == "Solidarity_CYP"

	replace Y`i'Party_Vote = "FreedomReform_CYP" if BB`i'Party == "CYP: Freedom and Reform Party"
	replace Y`i'Party_Vote_Long = "CYP: Freedom and Reform Party" if Y`i'Party_Vote == "FreedomReform_CYP"

	replace Y`i'Party_Vote = "PoliticsPeople_CYP" if BB`i'Party == "CYP: Politics for the People Party"
	replace Y`i'Party_Vote_Long = "CYP: Politics for the People Party" if Y`i'Party_Vote == "PoliticsPeople_CYP"

	replace Y`i'Party_Vote = "PopularSocialist_CYP" if BB`i'Party == "CYP:  Popular Socialist Movement"
	replace Y`i'Party_Vote_Long = "CYP:  Popular Socialist Movement" if Y`i'Party_Vote == "PopularSocialist_CYP"

	replace Y`i'Party_Vote = "Nationalist_CYP" if BB`i'Party == "CYP: Nationalist Justice Party"
	replace Y`i'Party_Vote_Long = "CYP: Nationalist Justice Party" if Y`i'Party_Vote == "Nationalist_CYP"

	/* Testing Cyprus*/
	tab Y`i'Party_Vote_Long if BB_Country=="Cyprus" & !missing(BB`i'Party) & Y_No`i'Party_Vote==0, missing
	tab BB`i'Party if BB_Country=="Cyprus" & Y_No`i'Party_Vote==0 & Y`i'Party_Vote_Long=="."

	count if Y`i'Party_Vote_Long=="." & BB_Country=="Cyprus" & !missing(BB`i'Party) & Y_No`i'Party_Vote==0
	assert r(N)==0
}

tab BB_Party BB_Year if Y_Party_Vote_Long=="." & BB_Country=="Crprus" & !missing(BB_Party) & Y_No_Party_Vote==0
tab BB_2nd_Party BB_Year if Y_2nd_Party_Vote_Long=="." & BB_Country=="Crprus" & !missing(BB_2nd_Party) & Y_No_2nd_Party_Vote==0
tab BB_Least_Party BB_Year if Y_Least_Party_Vote_Long=="." & BB_Country=="Crprus" & !missing(BB_Least_Party) & Y_No_Least_Party_Vote==0
/**********************************
******* Czech Republic ********************
**********************************/
foreach party in BB_Party BB_2nd_Party BB_Least_Party {
    if "`party'" == "BB_Party" {
        local i "$prefix_BB_Party"
    }
    else if "`party'" == "BB_2nd_Party" {
        local i "$prefix_BB_2nd_Party"
    }
    else if "`party'" == "BB_Least_Party" {
        local i "$prefix_BB_Least_Party"
    }

	replace Y`i'Party_Vote = "CivicParty_CZE" if BB`i'Party == "CZE: Civic Democratic Party" | BB`i'Party == "CZ: Obcanska Demokraticka Strana (ODS) - Civic Democratic Party" | BB`i'Party == "CZ: ODS (Civic Democratic Party)"
	replace Y`i'Party_Vote_Long = "CZE: Civic Democratic Party (ODS)" if Y`i'Party_Vote == "CivicParty_CZE"

	replace Y`i'Party_Vote = "CivicAlliance_CZE" if BB`i'Party == "CZ: Obcanska demokraticka alliance (ODA) - Civic Democratic Alliance" | BB`i'Party == "CZE: Civic Democratic Alliance"
	replace Y`i'Party_Vote_Long = "CZE: Civic Democratic Alliance" if Y`i'Party_Vote == "CivicAlliance_CZE"

	replace Y`i'Party_Vote = "Pirate_CZE" if BB`i'Party == "CZE: Czech Pirate Party" | BB`i'Party == "CZ: Piráti (Pirates)"
	replace Y`i'Party_Vote_Long = "CZE: Czech Pirate Party" if Y`i'Party_Vote == "Pirate_CZE"

	replace Y`i'Party_Vote = "CommunistBohemia_CZE" if BB`i'Party == "CZE: Communist Party of Bohemia and Moravia" | BB`i'Party == "CZ: Komunistická strana Cech a Moravy (KSCM) - Communist Party of Bohemia and Moravia" | BB`i'Party == "CZ: KSČM (Communist Party of Bohemia and"
	replace Y`i'Party_Vote_Long = "CZE: Communist Party of Bohemia and Moravia" if Y`i'Party_Vote == "CommunistBohemia_CZE"

	replace Y`i'Party_Vote = "SocialDems_CZE" if BB`i'Party == "CZ: Czech Social Democratic Party" | BB`i'Party == "CZ: Ceskoslovenská strana sociální demokracie (CSSD) - Czechoslovak Party of Social Democracy" | BB`i'Party == "CZ: Klub socialnich demokratu" | BB`i'Party == "CZE: Czechoslovak Social Democratic Workers Party" | BB`i'Party == "CZ: ČSSD (Czech Social Democratic Party)"
	replace Y`i'Party_Vote_Long = "CZE: Czech Social Democratic Party (CSSD)" if Y`i'Party_Vote == "SocialDems_CZE"

	replace Y`i'Party_Vote = "Green_CZE" if BB`i'Party == "CZE: Green Party" | BB`i'Party == "CZ: Strana zelenych (SZ) - Green Party" | BB`i'Party == "CZ: SZ (Green Party)"
	replace Y`i'Party_Vote_Long = "CZE: Green Party" if Y`i'Party_Vote == "Green_CZE"

	replace Y`i'Party_Vote = "ANO_CZE" if BB`i'Party == "CZE: ANO 2011" | BB`i'Party == "CZ: ANO"
	replace Y`i'Party_Vote_Long = "CZE: ANO 2011" if Y`i'Party_Vote == "ANO_CZE"

	replace Y`i'Party_Vote = "Crown_CZE" if BB`i'Party == "CZ: Czech Crown"
	replace Y`i'Party_Vote_Long = "CZE: Czech Crown" if Y`i'Party_Vote == "Crown_CZE"

	replace Y`i'Party_Vote = "DemocraticUnion_CZE" if BB`i'Party == "CZ: Demokraticke Unie (DEU) - Democratic Union" | BB`i'Party == "CZE: Democratic Union"
	replace Y`i'Party_Vote_Long = "CZE: Democratic Union (DEU)" if Y`i'Party_Vote == "DemocraticUnion_CZE"

	replace Y`i'Party_Vote = "FreedomUnion_CZE" if BB`i'Party == "CZ: Unie Svobody (US) - Freedom Union" | BB`i'Party == "CZE: Freedom Union"
	replace Y`i'Party_Vote_Long = "CZE: Freedom Union" if Y`i'Party_Vote == "FreedomUnion_CZE"

	replace Y`i'Party_Vote = "FreedomDirectDemocracy_CZE" if BB`i'Party == "CZ: SPD (Freedom and Direct Democracy- Tomio" 
	replace Y`i'Party_Vote_Long = "CZE: Freedom and Direct Democracy (SPD)" if Y`i'Party_Vote == "FreedomDirectDemocracy_CZE"

	replace Y`i'Party_Vote = "Pensioners_CZE" if BB`i'Party == "CZ: Duchodci za zivotní jistoty (DZJ) - Pensioners’ for a Secure Life" | BB`i'Party == "CZE: Pensioners for a Secure Living"
	replace Y`i'Party_Vote_Long = "CZE: Pensioners for a Secure Living (DZJ)" if Y`i'Party_Vote == "Pensioners_CZE"

	replace Y`i'Party_Vote = "CivicFreedom_CZE" if BB`i'Party == "CZ: Hnuti za obcanskou svobodu (HOS) - Movement for Civil Freedom"
	replace Y`i'Party_Vote_Long = "CZE: Movement for Civic Freedom (HOS)" if Y`i'Party_Vote == "CivicFreedom_CZE"

	replace Y`i'Party_Vote = "Selfgoverning_CZE" if BB`i'Party == "CZ: Hnuti za samospravnou demokracii-Spolecnost pro Moravu a a Slezsko (HSD-SMS) - Movement for Self-Governing Democracy" | BB`i'Party == "CZE: Movement for Self-Governing Democracy / Society for Mor" | BB`i'Party == "CZ: Hnuti za samospravnou demokracii-Spolecnost pro Moravu a a Slezsko (HSD-SMS) - Movement for Self-Governing Democrac"
	replace Y`i'Party_Vote_Long = "CZE: Movement for Self-Governing Democracy for Moravia and Silesia (HSD-SMS)" if Y`i'Party_Vote == "Selfgoverning_CZE"

	replace Y`i'Party_Vote = "IndependentDems_CZE" if BB`i'Party == "CZ: Independent Democrats"
	replace Y`i'Party_Vote_Long = "CZE: Independent Democrats" if Y`i'Party_Vote == "IndependentDems_CZE"

	replace Y`i'Party_Vote = "EuropeanDemocrats_CZE" if BB`i'Party == "CZ: European Democrats - SNK"
	replace Y`i'Party_Vote_Long = "CZE: European Democrats (SNK)" if Y`i'Party_Vote == "EuropeanDemocrats_CZE"

	replace Y`i'Party_Vote = "4Visions_CZE" if BB`i'Party == "CZ: 4 Visions"
	replace Y`i'Party_Vote_Long = "CZE: 4 Visions" if Y`i'Party_Vote == "4Visions_CZE"

	replace Y`i'Party_Vote = "BalbinPoetic_CZE" if BB`i'Party == "CZ: Balbin Poetic Party"
	replace Y`i'Party_Vote_Long = "CZE: Balbin Poetic Party" if Y`i'Party_Vote == "BalbinPoetic_CZE"

	replace Y`i'Party_Vote = "NationalSocial_CZE" if BB`i'Party == "CZ: Ceska strana narodne socialni (CSNS) - Czech National Social(ist) Party" | BB`i'Party == "CZE: Czechoslovak National Social Party (1897)" | BB`i'Party == "CZ: Narodne socialni strana (NSS) -"
	replace Y`i'Party_Vote_Long = "CZE: Czech National Social Party (CSNS)" if Y`i'Party_Vote == "NationalSocial_CZE"

	replace Y`i'Party_Vote = "CzechSocialist_CZE" if BB`i'Party == "CZ: Ceskoslovenská strana socialististická (CSS) - Czechoslovak Socialist Party"
	replace Y`i'Party_Vote_Long = "CZE: Czech Socialist Party (CSS)" if Y`i'Party_Vote == "CzechSocialist_CZE"

	replace Y`i'Party_Vote = "AgainstViolence_CZE" if BB`i'Party == "CZE: Public Against Violence"
	replace Y`i'Party_Vote_Long = "CZE: Public Against Violence (VPN)" if Y`i'Party_Vote == "AgainstViolence_CZE"

	replace Y`i'Party_Vote = "Coexistence_CZE" if BB`i'Party == "CZE: Movement Coexistence"
	replace Y`i'Party_Vote_Long = "CZE: Movement Coexistence" if Y`i'Party_Vote == "Coexistence_CZE"

	replace Y`i'Party_Vote = "Moravians_CZE" if BB`i'Party == "CZ: Moravians"
	replace Y`i'Party_Vote_Long = "CZE: Moravians" if Y`i'Party_Vote == "Moravians_CZE"

	replace Y`i'Party_Vote = "MoravianDems_CZE" if BB`i'Party == "CZ: Morovska demokraticka strana (MDS) - Moravian Democratic Party"
	replace Y`i'Party_Vote_Long = "CZE: Moravian Democratic Party (MDS)" if Y`i'Party_Vote == "MoravianDems_CZE"

	replace Y`i'Party_Vote = "Folk_CZE" if BB`i'Party == "CZ: Folklore and Society"
	replace Y`i'Party_Vote_Long = "CZE: Folklore and Society" if Y`i'Party_Vote == "Folk_CZE"

	replace Y`i'Party_Vote = "Humanist_CZE" if BB`i'Party == "CZ: Humanistic Party"
	replace Y`i'Party_Vote_Long = "CZE: Humanistic Party" if Y`i'Party_Vote == "Humanist_CZE"

	replace Y`i'Party_Vote = "LawJustice_CZE" if BB`i'Party == "CZ: Law and Justice"
	replace Y`i'Party_Vote_Long = "CZE: Law and Justice" if Y`i'Party_Vote == "LawJustice_CZE"

	replace Y`i'Party_Vote = "LiberalReform_CZE" if BB`i'Party == "CZ: Liberal-Reform Party"
	replace Y`i'Party_Vote_Long = "CZE: Liberal-Reform Party" if Y`i'Party_Vote == "LiberalReform_CZE"

	/* Manifesto defines these as two parties*/
	replace Y`i'Party_Vote = "KDU_CZE" if (BB`i'Party == "CZ: Krest'anská a demokratická unie-Ceskoslovenská lidová strana (KDU-CSL) Christian Democratic Union-Czechoslovak" | BB`i'Party == "CZE: Christian Democratic Union / People's Party" | BB`i'Party == "CZ: KDU-ČSL (Christian Democratic Party - Czechoslovak Peoples´Party)") & BB_Year==1991
	replace Y`i'Party_Vote_Long = "CZE: Christian Democratic Union (KDU)" if Y`i'Party_Vote == "KDU_CZE"

	replace Y`i'Party_Vote = "ChristianDemUnion_CZE" if (BB`i'Party == "CZ: Krest’anská a demokratická unie-Ceskoslovenská lidová strana (KDU-CSL) Christian Democratic Union-Czechoslovak" | BB`i'Party == "CZE: Christian Democratic Union / People's Party" | BB`i'Party == "CZ: KDU-ČSL (Christian Democratic Party - Czechoslovak Peoples´Party)") & BB_Year>1991
	replace Y`i'Party_Vote_Long = "CZE: Christian Democratic Union / People's Party (KDU-CSL)" if Y`i'Party_Vote == "ChristianDemUnion_CZE"

	replace Y`i'Party_Vote = "Agricultural_CZE" if BB`i'Party == "CZ: Zemedelska strana (ZS) -"
	replace Y`i'Party_Vote_Long = "CZE: Agricultural Party (ZS)" if Y`i'Party_Vote == "Agricultural_CZE"

	replace Y`i'Party_Vote = "TOP09_CZE" if BB`i'Party == "CZE: Tradition Responsibility Prosperity" | BB`i'Party == "CZ: TOP 09"
	replace Y`i'Party_Vote_Long = "CZE: Tradition Responsibility Prosperity (TOP 09)" if Y`i'Party_Vote == "TOP09_CZE"

	replace Y`i'Party_Vote = "CivicMovement_CZE" if BB`i'Party == "CZ: Obcanske hnuti (OH) - Civil Movement"
	replace Y`i'Party_Vote_Long = "CZE: Civic Movement (OH)" if Y`i'Party_Vote == "CivicMovement_CZE"

	replace Y`i'Party_Vote = "CivilCoalition_CZE" if BB`i'Party == "CZ: Obcanskkoalice (OK) - Civil Coalition" | BB`i'Party == "CZE: Civil Coalition-Political Club"
	replace Y`i'Party_Vote_Long = "CZE: Civil Coalition (OK)" if Y`i'Party_Vote == "CivilCoalition_CZE"

	replace Y`i'Party_Vote = "CommonSense_CZE" if BB`i'Party == "CZ: Party of Common Sense"
	replace Y`i'Party_Vote_Long = "CZE: Party of Common Sense" if Y`i'Party_Vote == "CommonSense_CZE"

	replace Y`i'Party_Vote = "Helax-Ostrava_CZE" if BB`i'Party == "CZ: Helax-Ostrava is Having Fun"
	replace Y`i'Party_Vote_Long = "CZE: Helax-Ostrava is Having Fun" if Y`i'Party_Vote == "Helax-Ostrava_CZE"

	replace Y`i'Party_Vote = "NationalUnity_CZE" if BB`i'Party == "CZ: Czech Movement for National Unity"
	replace Y`i'Party_Vote_Long = "CZE: Czech Movement for National Unity" if Y`i'Party_Vote == "NationalUnity_CZE"

	replace Y`i'Party_Vote = "ChristianDems_CZE" if BB`i'Party == "CZ: Krestanskodemokraticka strana (KDS) -" | BB`i'Party == "CZE: Christian Democratic Party"
	replace Y`i'Party_Vote_Long = "CZE: Christian Democratic Party (KDS)" if Y`i'Party_Vote == "ChristianDems_CZE"

	replace Y`i'Party_Vote = "LiberalDems_CZE" if BB`i'Party == "CZ: Liberalne demokraticka strana (LDS) -"
	replace Y`i'Party_Vote_Long = "CZE: Liberal Democratic Party (LDS)" if Y`i'Party_Vote == "LiberalDems_CZE"

	replace Y`i'Party_Vote = "CommunistCzech_CZE" if BB`i'Party == "CZE: Communist Party of Czechoslovakia"
	replace Y`i'Party_Vote_Long = "CZE: Communist Party of Czechoslovakia" if Y`i'Party_Vote == "CommunistCzech_CZE"

	replace Y`i'Party_Vote = "RomaCivil_CZE" if BB`i'Party == "CZ: Romska obcanska iniciativa (ROI) - Romany Civil Inititiave"
	replace Y`i'Party_Vote_Long = "CZE: Roma Civil Initiative (ROI)" if Y`i'Party_Vote == "RomaCivil_CZE"

	replace Y`i'Party_Vote = "National_CZE" if BB`i'Party == "CZ: National Party"
	replace Y`i'Party_Vote_Long = "CZE: National Party" if Y`i'Party_Vote == "National_CZE"

	replace Y`i'Party_Vote = "DemocraticForum_CZE" if BB`i'Party == "CZ: Ceskoslovenské demokratické forum (CDF) - Czechoslovak Democratic Forum"
	replace Y`i'Party_Vote_Long = "CZE: Czechoslovak Democratic Forum (CDF)" if Y`i'Party_Vote == "DemocraticForum_CZE"

	//Unclear if these votes are for one of the parties with independents in the name or if they are actual independent candidates
	replace Y`i'Party_Vote = "Independent_CZE" if BB`i'Party == "CZ: Nezavisli (N) - Independents" | BB`i'Party == "CZE: Independent"
	replace Y`i'Party_Vote_Long = "CZE: Independents" if Y`i'Party_Vote == "Independent_CZE"

	replace Y`i'Party_Vote = "Farmers_CZE" if BB`i'Party == "CZE: Republican Party of Farmers and Peasants // Agrarian Pa"
	replace Y`i'Party_Vote_Long = "CZE: Republican Party of Farmers and Peasants" if Y`i'Party_Vote == "Farmers_CZE"

	replace Y`i'Party_Vote = "CoalitionRepublic_CZE" if BB`i'Party == "CZ: Sdruzeni pro Republiku - Rep. Strana Ceskoslovenska (SPR-RSE) - Rally for the Republic-Republican Party of Czechoslo" | BB`i'Party=="CZ: Coalition for the Czech Republic" | BB`i'Party == "CZE: Coalition for Republic – Republican Party of Czechoslov" | BB`i'Party == "CZ: Sdruzeni pro Republiku - Rep. Strana Ceskoslovenska (SPR-RSE) - Rally for the Republic-Republican Party of Czechosl"
	replace Y`i'Party_Vote_Long = "CZE: Coalition for the Republic" if Y`i'Party_Vote == "CoalitionRepublic_CZE"

	replace Y`i'Party_Vote = "WorkersSocialJustice_CZE" if BB`i'Party == "CZ: DSSS (Worker´s Party of Social Justice)"
	replace Y`i'Party_Vote_Long = "CZE: Workers Party of Social Justice (DSSS)" if Y`i'Party_Vote == "WorkersSocialJustice_CZE"

	replace Y`i'Party_Vote = "CivicRights_CZE" if BB`i'Party == "CZ: SPO (Party for the Rights of Citizens)"
	replace Y`i'Party_Vote_Long = "CZE: Party of Civic Rights (SPO)" if Y`i'Party_Vote == "CivicRights_CZE"

	replace Y`i'Party_Vote = "MayorsIndependents_CZE" if BB`i'Party == "CZ: STAN (Mayors and Independents)"
	replace Y`i'Party_Vote_Long = "CZE: Mayors and Independents (STAN)" if Y`i'Party_Vote == "MayorsIndependents_CZE"

	replace Y`i'Party_Vote = "FreeCitizens_CZE" if BB`i'Party == "CZ: Svobodní (Party of Free Citizens)"
	replace Y`i'Party_Vote_Long = "CZE: Party of Free Citizens (SSO)" if Y`i'Party_Vote == "FreeCitizens_CZE"

	replace Y`i'Party_Vote = "DawnDirectDemocracy_CZE" if BB`i'Party == "CZ: Úsvit-NK (Dawn- National Coalition)"
	replace Y`i'Party_Vote_Long = "CZE: Dawn of Direct Democracy" if Y`i'Party_Vote == "DawnDirectDemocracy_CZE"

	replace Y`i'Party_Vote = "Realists_CZE" if BB`i'Party == "CZ: Realisté (Realists)"
	replace Y`i'Party_Vote_Long = "CZE: Realists" if Y`i'Party_Vote == "Realists_CZE"

	/* Testing Czech Republic*/
	tab Y`i'Party_Vote_Long if BB_Country=="Czech Republic" & !missing(BB`i'Party) & Y_No`i'Party_Vote==0, missing
	tab BB`i'Party if BB_Country=="Czech Republic" & Y_No`i'Party_Vote==0 & Y`i'Party_Vote_Long=="."

	count if Y`i'Party_Vote_Long=="." & BB_Country=="Czech Republic" & !missing(BB`i'Party) & Y_No`i'Party_Vote==0
	assert r(N)==0
}

tab BB_Party BB_Year if Y_Party_Vote_Long=="." & BB_Country=="Czech Republic" & !missing(BB_Party) & Y_No_Party_Vote==0
tab BB_2nd_Party BB_Year if Y_2nd_Party_Vote_Long=="." & BB_Country=="Czech Republic" & !missing(BB_2nd_Party) & Y_No_2nd_Party_Vote==0
tab BB_Least_Party BB_Year if Y_Least_Party_Vote_Long=="." & BB_Country=="Czech Republic" & !missing(BB_Least_Party) & Y_No_Least_Party_Vote==0
/**********************************
******* Denmark ********************
**********************************/
foreach party in BB_Party BB_2nd_Party BB_Least_Party {
    if "`party'" == "BB_Party" {
        local i "$prefix_BB_Party"
    }
    else if "`party'" == "BB_2nd_Party" {
        local i "$prefix_BB_2nd_Party"
    }
    else if "`party'" == "BB_Least_Party" {
        local i "$prefix_BB_Least_Party"
    }

	replace Y`i'Party_Vote = "CentreDemocrats_DNK" if BB`i'Party == "DK: Centrum-Demokraterne - Centre Democrats"
	replace Y`i'Party_Vote_Long = "DNK: Centre Democrats" if Y`i'Party_Vote == "CentreDemocrats_DNK"

	replace Y`i'Party_Vote = "ChristianDems_DNK" if BB`i'Party == "DK: Christian Democrats"
	replace Y`i'Party_Vote_Long = "DNK: Christian Democrats" if Y`i'Party_Vote == "ChristianDems_DNK"

	replace Y`i'Party_Vote = "ConservativePeoples_DNK" if BB`i'Party == "DK: Conservative Peoples Party" | BB`i'Party == "DK: Konservative Folkeparti - Conservative Peoples Party"
	replace Y`i'Party_Vote_Long = "DNK: Conservative People's Party" if Y`i'Party_Vote == "ConservativePeoples_DNK"

	replace Y`i'Party_Vote = "DanishPeoples_DNK" if BB`i'Party == "DK: Danish People Party" | BB`i'Party == "DK: Danish Peoples Party"
	replace Y`i'Party_Vote_Long = "DNK: Danish People's Party" if Y`i'Party_Vote == "DanishPeoples_DNK"

	replace Y`i'Party_Vote = "SocialLiberal_DNK" if BB`i'Party == "DK: Danish Social Liberal Party" | BB`i'Party == "DK: Radikale Venstre - Social Liberals"
	replace Y`i'Party_Vote_Long = "DNK: Danish Social Liberal Party" if Y`i'Party_Vote == "SocialLiberal_DNK"

	replace Y`i'Party_Vote = "Communist_DNK" if BB`i'Party == "DK: Danmarks kommunistiske Parti - Communist Party of Denmark"
	replace Y`i'Party_Vote_Long = "DNK: Communist Party of Denmark" if Y`i'Party_Vote == "Communist_DNK"

	replace Y`i'Party_Vote = "Green_DNK" if BB`i'Party == "DK: De Gronne - The Green Party"
	replace Y`i'Party_Vote_Long = "DNK: The Green Party" if Y`i'Party_Vote == "Green_DNK"

	replace Y`i'Party_Vote = "Justice_DNK" if BB`i'Party == "DK: Denmarks Retsforbund - Denmarks Justice Party"
	replace Y`i'Party_Vote_Long = "DNK: Denmarks Justice Party" if Y`i'Party_Vote == "Justice_DNK"

	replace Y`i'Party_Vote = "Humanist_DNK" if BB`i'Party == "DK: Det Humnanistiske Parti - Humanist Party"
	replace Y`i'Party_Vote_Long = "DNK: Humanist Party" if Y`i'Party_Vote == "Humanist_DNK"

	replace Y`i'Party_Vote = "SocialistWorkers_DNK" if BB`i'Party == "DK: Internationalen -Socialistisk Arbejderparti - The Socialist Workers Party"
	replace Y`i'Party_Vote_Long = "DNK: The Socialist Workers Party" if Y`i'Party_Vote == "SocialistWorkers_DNK"

	replace Y`i'Party_Vote = "Liberal_DNK" if BB`i'Party == "DK: Venstre - Liberals" | BB`i'Party ==  "DK: Venstre, Denmarks Liberal Party"
	replace Y`i'Party_Vote_Long = "DNK: Denmarks Liberal Party (Venstre)" if Y`i'Party_Vote == "Liberal_DNK"

	replace Y`i'Party_Vote = "SocialDem_DNK" if BB`i'Party == "DK: The Social Democrats" | BB`i'Party == "DK: Socialdemokratiet - Social Democrats"
	replace Y`i'Party_Vote_Long = "DNK: The Social Democrats" if Y`i'Party_Vote == "SocialDem_DNK"

	replace Y`i'Party_Vote = "Progress_DNK" if BB`i'Party == "DK: Progress Party"
	replace Y`i'Party_Vote_Long = "DNK: Progress Party" if Y`i'Party_Vote == "Progress_DNK"

	replace Y`i'Party_Vote = "LiberalAlliance_DNK" if BB`i'Party == "DK: New Alliance (Liberal Alliance)" | BB`i'Party == "DK: Liberal Alliance"
	replace Y`i'Party_Vote_Long = "DNK: Liberal (New) Alliance" if Y`i'Party_Vote == "LiberalAlliance_DNK"

	replace Y`i'Party_Vote = "RedGreen_DNK" if BB`i'Party == "DK: Red-Green Alliance" | BB`i'Party == "DK: Unity List - Red-Green Alliance"
	replace Y`i'Party_Vote_Long = "DNK: Unity List - Red-Green Alliance" if Y`i'Party_Vote == "RedGreen_DNK"

	replace Y`i'Party_Vote = "SocialistPeoples_DNK" if BB`i'Party == "DK: Socialistisk Folkeparti - Socialist People Party" | BB`i'Party == "DK: Socialist Peoples Party"
	replace Y`i'Party_Vote_Long = "DNK: Socialist People's Party" if Y`i'Party_Vote == "SocialistPeoples_DNK"

	replace Y`i'Party_Vote = "Alternative_DNK" if BB`i'Party == "DK: The Alternative"
	replace Y`i'Party_Vote_Long = "DNK: The Alternative" if Y`i'Party_Vote == "Alternative_DNK"

	replace Y`i'Party_Vote = "NewRight_DNK" if BB`i'Party == "DK: The New Right"
	replace Y`i'Party_Vote_Long = "DNK: The New Right" if Y`i'Party_Vote == "NewRight_DNK"

	replace Y`i'Party_Vote = "LeftSocialists_DNK" if BB`i'Party == "DK: Ventresocialisterne - Left Socialists"
	replace Y`i'Party_Vote_Long = "DNK: Left Socialists" if Y`i'Party_Vote == "LeftSocialists_DNK"

	replace Y`i'Party_Vote = "ChristianPeoples_DNK" if BB`i'Party == "DK: Kristeligt Folkeparti - Christian People Party"
	replace Y`i'Party_Vote_Long = "DNK: Christian People's Party" if Y`i'Party_Vote == "ChristianPeoples_DNK"

	replace Y`i'Party_Vote = "CommonCourse_DNK" if BB`i'Party == "DK: Faelles Kurs - Common Course"
	replace Y`i'Party_Vote_Long = "DNK: Common Course" if Y`i'Party_Vote == "CommonCourse_DNK"

	/* Testing Denmark*/
	tab Y`i'Party_Vote_Long if BB_Country=="Denmark" & !missing(BB`i'Party) & Y_No`i'Party_Vote==0, missing
	tab BB`i'Party if BB_Country=="Denmark" & Y_No`i'Party_Vote==0 & Y`i'Party_Vote_Long=="."

	count if Y`i'Party_Vote_Long=="." & BB_Country=="Denmark" & !missing(BB`i'Party) & Y_No`i'Party_Vote==0
	assert r(N)==0
}

tab BB_Party BB_Year if Y_Party_Vote_Long=="." & BB_Country=="Denmark" & !missing(BB_Party) & Y_No_Party_Vote==0
tab BB_2nd_Party BB_Year if Y_2nd_Party_Vote_Long=="." & BB_Country=="Denmark" & !missing(BB_2nd_Party) & Y_No_2nd_Party_Vote==0
tab BB_Least_Party BB_Year if Y_Least_Party_Vote_Long=="." & BB_Country=="Denmark" & !missing(BB_Least_Party) & Y_No_Least_Party_Vote==0
/**********************************
******* Estonia ********************
**********************************/
foreach party in BB_Party BB_2nd_Party BB_Least_Party {
    if "`party'" == "BB_Party" {
        local i "$prefix_BB_Party"
    }
    else if "`party'" == "BB_2nd_Party" {
        local i "$prefix_BB_2nd_Party"
    }
    else if "`party'" == "BB_Least_Party" {
        local i "$prefix_BB_Least_Party"
    }
	
	replace Y`i'Party_Vote = "Center_EST" if BB`i'Party == "EE: Eesti Keskerakond (EK) - Estonian Centre Party" | BB`i'Party == "EE: Estonian Centre Party" | BB`i'Party == "EST: Estonian Centre Party"
	replace Y`i'Party_Vote_Long = "EST: Estonian Center Party (EK)" if Y`i'Party_Vote == "Center_EST"

	replace Y`i'Party_Vote = "Coalition_EST" if BB`i'Party == "EE: Eesti Koonderakond - Estonian Coalition Party" | BB`i'Party ==  "EST: Estonian Coalition Party"
	replace Y`i'Party_Vote_Long = "EST: Estonian Coalition Party (Koonderakond)" if Y`i'Party_Vote == "Coalition_EST"

	replace Y`i'Party_Vote = "Christian_EST" if BB`i'Party == "EE: Eesti Kristlik Rahvapartei (EKRP) - Estonian Christian People´s Party"
	replace Y`i'Party_Vote_Long = "EST: Estonian Christian People's Party (EKRP)" if Y`i'Party_Vote == "Christian_EST"

	replace Y`i'Party_Vote = "CountryPeoples_EST" if BB`i'Party == "EE: Eesti Maarahva Erakond (EME) - Estonian Country People’s Party" | BB`i'Party == "EST: Estonian Country People´s Party"
	replace Y`i'Party_Vote_Long = "EST: Estonian Country People's Party (EME)" if Y`i'Party_Vote == "CountryPeoples_EST"

	replace Y`i'Party_Vote = "Reform_EST" if BB`i'Party == "EE: Eesti Reformierakond (RE) - Estonian Reform Party" | BB`i'Party == "EE: Estonian Reform Party" | BB`i'Party == "EST: Estonian Reform Party"
	replace Y`i'Party_Vote_Long = "EST: Estonian Reform Party (RE)" if Y`i'Party_Vote == "Reform_EST"

	replace Y`i'Party_Vote = "Blue_EST" if BB`i'Party == "EE: Eesti Sinine Erakond (ESE) - Estonian Blue Party" | BB`i'Party == "EST: Estonian Blue Party"
	replace Y`i'Party_Vote_Long = "EST: Estonian Blue Party (ESE)" if Y`i'Party_Vote == "Blue_EST"

	replace Y`i'Party_Vote = "UnitedPeoples_EST" if BB`i'Party == "EE: Eestimaa Ühendatud Rahvapartei (EÜRP) - Estonian United People’s Party" | BB`i'Party == "EE: Estonian Constitution Party"
	replace Y`i'Party_Vote_Long = "EST: Estonian United People's Party (EURP)" if Y`i'Party_Vote == "UnitedPeoples_EST"

	replace Y`i'Party_Vote = "Greens_EST" if BB`i'Party == "EE: Erakond ´Eesti Rohelised´ (ER)" | BB`i'Party == "EE: Estonian Greens" | BB`i'Party == "EST: Estonian Greens"
	replace Y`i'Party_Vote_Long = "EST: Estonian Greens (ER)" if Y`i'Party_Vote == "Greens_EST"

	replace Y`i'Party_Vote = "ChristianDems_EST" if BB`i'Party == "EE: Estonian Christian Democrats"
	replace Y`i'Party_Vote_Long = "EST: Estonian Christian Democrats" if Y`i'Party_Vote == "ChristianDems_EST"

	replace Y`i'Party_Vote = "ConservativePeoples_EST" if BB`i'Party == "EE: Estonian Conseravative Peoples party" | BB`i'Party == "EST: People Party of Republicans and Conservatives"
	replace Y`i'Party_Vote_Long = "EST: Estonian Conservative People's Party" if Y`i'Party_Vote == "ConservativePeoples_EST"

	replace Y`i'Party_Vote = "Free_EST" if BB`i'Party == "EE: Estonian Free Party"
	replace Y`i'Party_Vote_Long = "EST: Estonian Free Party" if Y`i'Party_Vote == "Free_EST"

	replace Y`i'Party_Vote = "Independence_EST" if BB`i'Party == "EE: Estonian Independence Party"
	replace Y`i'Party_Vote_Long = "EST: Estonian Independence Party" if Y`i'Party_Vote == "Independence_EST"

	replace Y`i'Party_Vote = "Left_EST" if BB`i'Party == "EE: Estonian Left Party"
	replace Y`i'Party_Vote_Long = "EST: Estonian Left Party" if Y`i'Party_Vote == "Left_EST"

	replace Y`i'Party_Vote = "Pensioners_EST" if BB`i'Party == "EE: Estonian Pensioners and Families Party" | BB`i'Party == "EST: Estonian Pensioners' Union"
	replace Y`i'Party_Vote_Long = "EST: Estonian Pensioners and Families Party" if Y`i'Party_Vote == "Pensioners_EST"

	replace Y`i'Party_Vote = "PeoplesUnion_EST" if BB`i'Party == "EE: Estonian Peoples Union" | BB`i'Party ==  "EST: Estonian Peoples Union"
	replace Y`i'Party_Vote_Long = "EST: Estonian People's Union" if Y`i'Party_Vote == "PeoplesUnion_EST"

	replace Y`i'Party_Vote = "SocialDemocratic_EST" if BB`i'Party == "EE: Estonian Social Democratic Party" | BB`i'Party == "EE: Social Democratic Party" | BB`i'Party == "EE: Estonian Social Democratic Labour Party (ESDTP)" | BB`i'Party == "EST: Social Democratic Party -- Moderates"
	replace Y`i'Party_Vote_Long = "EST: Estonian Social Democratic Party" if Y`i'Party_Vote == "SocialDemocratic_EST"
	//ESDTP from the 1920s and no Social Democratic Party in the wave where it has votes (9), so that seems to be a WVS/EVS error and putting it with regular Social Democratic Party

	replace Y`i'Party_Vote = "ProPatria_EST" if BB`i'Party == "EE: Isamaaliit (I) - Pro Patria" | BB`i'Party == "EST: Pro Patria"
	replace Y`i'Party_Vote_Long = "EST: Pro Patria (I)" if Y`i'Party_Vote == "ProPatria_EST"

	replace Y`i'Party_Vote = "Moderates_EST" if BB`i'Party == "EE: Moodukad (M) - Moderates"
	replace Y`i'Party_Vote_Long = "EST: Moderates (M)" if Y`i'Party_Vote == "Moderates_EST"

	replace Y`i'Party_Vote = "FarmersUnion_EST" if BB`i'Party == "EE: Plumeeste Kogu (PK) - FarmersUnion" | BB`i'Party == "EST: Union of Farmers"
	replace Y`i'Party_Vote_Long = "EST: Farmers Union (PK)" if Y`i'Party_Vote == "FarmersUnion_EST"

	replace Y`i'Party_Vote = "Russian_EST" if BB`i'Party == "EE: Russian Unity Party" | BB`i'Party == "EE: Vene Erakond Eestis (VEE) - Russian Party in Estonia" | BB`i'Party == "EST: Russian Party in Estonia"
	replace Y`i'Party_Vote_Long = "EST: Russian Party in Estonia (VEE)" if Y`i'Party_Vote == "Russian_EST"

	replace Y`i'Party_Vote = "Forest_EST" if BB`i'Party == "EST: The Forest party"
	replace Y`i'Party_Vote_Long = "EST: The Forest party" if Y`i'Party_Vote == "Forest_EST"

	replace Y`i'Party_Vote = "Ice_EST" if BB`i'Party == "EST: The Ice party of the Estonian people"
	replace Y`i'Party_Vote_Long = "EST: The Ice party of the Estonian people" if Y`i'Party_Vote == "Ice_EST"

	replace Y`i'Party_Vote = "RuralUnion_EST" if BB`i'Party == "EE: The Rural Union"
	replace Y`i'Party_Vote_Long = "EST: The Rural Union" if Y`i'Party_Vote == "RuralUnion_EST"

	replace Y`i'Party_Vote = "ProPatriaResPublica_EST" if BB`i'Party == "EE: Pro Patria and Res Publica Union" | BB`i'Party == "EE: Union of Pro Patria and Res Publica" | BB`i'Party == "EST: Union of Pro Patria and Res Publica"
	replace Y`i'Party_Vote_Long = "EST: Union of Pro Patria and Res Publica" if Y`i'Party_Vote == "ProPatriaResPublica_EST"

	replace Y`i'Party_Vote = "NationalChildren_EST" if BB`i'Party == "EST: Estonian Confederation of National Children"
	replace Y`i'Party_Vote_Long = "EST: Estonian Confederation of National Children" if Y`i'Party_Vote == "NationalChildren_EST"

	replace Y`i'Party_Vote = "CountryUnion_EST" if BB`i'Party == "EST: Estonian Country Union"
	replace Y`i'Party_Vote_Long = "EST: Estonian Country Union" if Y`i'Party_Vote == "CountryUnion_EST"

	replace Y`i'Party_Vote = "DemocraticTask_EST" if BB`i'Party == "EST: Estonian Democratic Task Party"
	replace Y`i'Party_Vote_Long = "EST: Estonian Democratic Task Party" if Y`i'Party_Vote == "DemocraticTask_EST"

	replace Y`i'Party_Vote = "FarmersParty_EST" if BB`i'Party == "EST: Estonian Farmers’ Party"
	replace Y`i'Party_Vote_Long = "EST: Estonian Farmers Party" if Y`i'Party_Vote == "FarmersParty_EST"

	replace Y`i'Party_Vote = "DemocraticUnion_EST" if regexm(BB`i'Party, ".*Estonian Democratic Union.*") 
	replace Y`i'Party_Vote_Long = "EST: Estonian Democratic Union" if Y`i'Party_Vote == "DemocraticUnion_EST"

	replace Y`i'Party_Vote = "Royalist_EST" if BB`i'Party == "EST: Estonian rojalist party"
	replace Y`i'Party_Vote_Long = "EST: Estonian royalist party" if Y`i'Party_Vote == "Royalist_EST"

	replace Y`i'Party_Vote = "Future_EST" if BB`i'Party == "EST: Future’s Estonia Party"
	replace Y`i'Party_Vote_Long = "EST: Future’s Estonia Party" if Y`i'Party_Vote == "Future_EST"

	replace Y`i'Party_Vote = "Home_EST" if BB`i'Party == "EST: Party of Estonian home"
	replace Y`i'Party_Vote_Long = "EST: Party of Estonian home" if Y`i'Party_Vote == "Home_EST"

	replace Y`i'Party_Vote = "Development_EST" if BB`i'Party == "EST: The Development party"
	replace Y`i'Party_Vote_Long = "EST: The Development party" if Y`i'Party_Vote == "Development_EST"

	/* Testing Estonia*/
	tab Y`i'Party_Vote_Long if BB_Country=="Estonia" & !missing(BB`i'Party) & Y_No`i'Party_Vote==0, missing
	tab BB`i'Party if BB_Country=="Estonia" & Y_No`i'Party_Vote==0 & Y`i'Party_Vote_Long=="."

	count if Y`i'Party_Vote_Long=="." & BB_Country=="Estonia" & !missing(BB`i'Party) & Y_No`i'Party_Vote==0
	assert r(N)==0
}

tab BB_Party BB_Year if Y_Party_Vote_Long=="." & BB_Country=="Estonia" & !missing(BB_Party) & Y_No_Party_Vote==0
tab BB_2nd_Party BB_Year if Y_2nd_Party_Vote_Long=="." & BB_Country=="Estonia" & !missing(BB_2nd_Party) & Y_No_2nd_Party_Vote==0
tab BB_Least_Party BB_Year if Y_Least_Party_Vote_Long=="." & BB_Country=="Estonia" & !missing(BB_Least_Party) & Y_No_Least_Party_Vote==0
/**********************************
******* Finland ********************
**********************************/
foreach party in BB_Party BB_2nd_Party BB_Least_Party {
    if "`party'" == "BB_Party" {
        local i "$prefix_BB_Party"
    }
    else if "`party'" == "BB_2nd_Party" {
        local i "$prefix_BB_2nd_Party"
    }
    else if "`party'" == "BB_Least_Party" {
        local i "$prefix_BB_Least_Party"
    }
	
	replace Y`i'Party_Vote = "Center_FIN" if BB`i'Party == "FI: Center party" | BB`i'Party == "FI: Center party - KESK" | BB`i'Party == "FI: Centre Party of Finland (centre, big, conservative) - KESK" | BB`i'Party == "FIN: Agrarian Union / Centre Party"
	replace Y`i'Party_Vote_Long = "FIN: Center Party (KESK)" if Y`i'Party_Vote == "Center_FIN"

	//Started as Finnish Christian League then changed name to Christian Democrats, may need to combine the two Y`i'Party_Vote
	replace Y`i'Party_Vote = "ChristianDem_FIN" if BB`i'Party == "FI: Christian Democrats - KD" | BB`i'Party == "FI: Christian democrats" | BB`i'Party == "FIN: Christian Democrats / League" | BB`i'Party == "FI: Christian League of Finland (right, medium, religious)" | BB`i'Party == "FIN: Finnish Christian League"
	replace Y`i'Party_Vote_Long = "FIN: Christian Democrats (KD)" if Y`i'Party_Vote == "ChristianDem_FIN"

	replace Y`i'Party_Vote = "ConstitutionalRight_FIN" if BB`i'Party == "FI: Constitutional Party of the Right"
	replace Y`i'Party_Vote_Long = "FIN: Constitutional Party of the Right" if Y`i'Party_Vote == "ConstitutionalRight_FIN"

	// True Finns and the Finn party seem to be the same
	replace Y`i'Party_Vote = "Finns_FIN" if BB`i'Party == "FI: Finns Party" | BB`i'Party == "FIN: The Finns Party" | BB`i'Party == "FI: True Finns (right, small, conservative) - PS" | BB`i'Party == "FI: Rural Party"
	replace Y`i'Party_Vote_Long = "FIN: Finns Party" if Y`i'Party_Vote == "Finns_FIN"

	replace Y`i'Party_Vote = "GreenLeague_FIN" if BB`i'Party == "FI: Green League (centre, medium, progressive) - VIHR" | BB`i'Party == "FI: Green league" | BB`i'Party == "FIN: Green League"
	replace Y`i'Party_Vote_Long = "FIN: Green League (VIHR)" if Y`i'Party_Vote == "GreenLeague_FIN"

	replace Y`i'Party_Vote = "LeftAlliance_FIN" if BB`i'Party == "FI: Left Alliance (left, medium, progressive) - VAS" | BB`i'Party == "FI: Left alliance" | BB`i'Party == "FIN: Left Alliance"
	replace Y`i'Party_Vote_Long = "FIN: Left Alliance (VAS)" if Y`i'Party_Vote == "LeftAlliance_FIN"

	replace Y`i'Party_Vote = "LiberalPeoples_FIN" if BB`i'Party == "FI: Liberal Peoples Party"
	replace Y`i'Party_Vote_Long = "FIN: Liberal People's Party" if Y`i'Party_Vote == "LiberalPeoples_FIN"

	replace Y`i'Party_Vote = "NationalCoalition_FIN" if BB`i'Party == "FI: National Coalition Party" | BB`i'Party == "FI: National Coalition Party (right, big, conservative) - KOK" | BB`i'Party == "FIN: National Coalition Party"
	replace Y`i'Party_Vote_Long = "FIN: National Coalition Party (KOK)" if Y`i'Party_Vote == "NationalCoalition_FIN"

	replace Y`i'Party_Vote = "Pensioners_FIN" if BB`i'Party == "FI: Pensioners Party"
	replace Y`i'Party_Vote_Long = "FIN: Pensioners Party" if Y`i'Party_Vote == "Pensioners_FIN"

	replace Y`i'Party_Vote = "Reform_FIN" if BB`i'Party == "FI: Reform Group (centre, small, progressive)"
	replace Y`i'Party_Vote_Long = "FIN: Reform Group" if Y`i'Party_Vote == "Reform_FIN"

	replace Y`i'Party_Vote = "SocialDem_FIN" if BB`i'Party == "FI: Social Democratic Party of Finland - SDP" | BB`i'Party == "FI: Social democratic party" | BB`i'Party == "FIN: Social Democratic Party of Finland"
	replace Y`i'Party_Vote_Long = "FIN: Social Democratic Party of Finland (SDP)" if Y`i'Party_Vote == "SocialDem_FIN"

	replace Y`i'Party_Vote = "SwedishPeoples_FIN" if BB`i'Party == "FI: Swedish People's Party in Finland" | BB`i'Party == "FI: Swedish Peoples Party in Finland - RKP" | BB`i'Party == "FIN: Swedish People's Party"
	replace Y`i'Party_Vote_Long = "FIN: Swedish People's Party in Finland (RKP)" if Y`i'Party_Vote == "SwedishPeoples_FIN"

	replace Y`i'Party_Vote = "YoungFinns_FIN" if BB`i'Party == "FIN: The Young Finns"
	replace Y`i'Party_Vote_Long = "FIN: The Young Finns" if Y`i'Party_Vote == "YoungFinns_FIN"

	/* Testing Finland*/
	tab Y`i'Party_Vote_Long if BB_Country=="Finland" & !missing(BB`i'Party) & Y_No`i'Party_Vote==0, missing
	tab BB`i'Party if BB_Country=="Finland" & Y_No`i'Party_Vote==0 & Y`i'Party_Vote_Long=="."

	count if Y`i'Party_Vote_Long=="." & BB_Country=="Finland" & !missing(BB`i'Party) & Y_No`i'Party_Vote==0
	assert r(N)==0
}

tab BB_Party BB_Year if Y_Party_Vote_Long=="." & BB_Country=="Finland" & !missing(BB_Party) & Y_No_Party_Vote==0
tab BB_2nd_Party BB_Year if Y_2nd_Party_Vote_Long=="." & BB_Country=="Finland" & !missing(BB_2nd_Party) & Y_No_2nd_Party_Vote==0
tab BB_Least_Party BB_Year if Y_Least_Party_Vote_Long=="." & BB_Country=="Finland" & !missing(BB_Least_Party) & Y_No_Least_Party_Vote==0
/**********************************
******* Greece ********************
**********************************/
foreach party in BB_Party BB_2nd_Party BB_Least_Party {
    if "`party'" == "BB_Party" {
        local i "$prefix_BB_Party"
    }
    else if "`party'" == "BB_2nd_Party" {
        local i "$prefix_BB_2nd_Party"
    }
    else if "`party'" == "BB_Least_Party" {
        local i "$prefix_BB_Least_Party"
    }
	
/* Separating these to two parties since Manifesto defines them as two parties 34020, 34212 (one is a successor)*/
	replace Y`i'Party_Vote = "SYRIZA_GRC" if BB`i'Party == "GR: Coalition of the Radical Left - SYRIZA"
	replace Y`i'Party_Vote_Long = "GRC: Coalition of the Radical Left (SYRIZA)" if Y`i'Party_Vote == "SYRIZA_GRC"

	replace Y`i'Party_Vote = "SYRIZA-EKN_GRC" if BB`i'Party == "GRC: Coalition of the Radical Left"
	replace Y`i'Party_Vote_Long = "GRC: Coalition of the Radical Left (SYRIZA-EKN)" if Y`i'Party_Vote == "SYRIZA-EKN_GRC"

	replace Y`i'Party_Vote = "DemSocialists_GRC" if BB`i'Party == "GR: DIKKI - Democratic Socialists"
	replace Y`i'Party_Vote_Long = "GRC: Democratic Socialists (DIKKI)" if Y`i'Party_Vote == "DemSocialists_GRC"

	replace Y`i'Party_Vote = "Communist_GRC" if BB`i'Party == "GR: KKE - Communist Party of Greece" | BB`i'Party == "GRC: Communist Party of Greece"
	replace Y`i'Party_Vote_Long = "GRC: Communist Party of Greece (KKE)" if Y`i'Party_Vote == "Communist_GRC"

	replace Y`i'Party_Vote = "NewDemocracy_GRC" if BB`i'Party == "GR: Nea Demokratia (ND) - New Democracy (conservative)" | BB`i'Party == "GRC: New Democracy"
	replace Y`i'Party_Vote_Long = "GRC: New Democracy (ND)" if Y`i'Party_Vote == "NewDemocracy_GRC"

	replace Y`i'Party_Vote = "PanhellenicSocial_GRC" if BB`i'Party == "GR: PASOK - Panhellenic Socialist Movement"
	replace Y`i'Party_Vote_Long = "GRC: Panhellenic Socialist Movement (PASOK)" if Y`i'Party_Vote == "PanhellenicSocial_GRC"

	replace Y`i'Party_Vote = "PopularOrthodox_GRC" if BB`i'Party == "GR: Popular Orhtodox Rally - LAOS"
	replace Y`i'Party_Vote_Long = "GRC: Popular Orthodox Rally (LAOS)" if Y`i'Party_Vote == "PopularOrthodox_GRC"

	replace Y`i'Party_Vote = "RightWing_GRC" if BB`i'Party == "GR: Right Wing"
	replace Y`i'Party_Vote_Long = "GRC: Right Wing" if Y`i'Party_Vote == "RightWing_GRC"

	replace Y`i'Party_Vote = "CoalitionLeft_GRC" if BB`i'Party == "GR: SYN - Coalition of the Left, of Movements and Ecology"
	replace Y`i'Party_Vote_Long = "GRC: Coalition of the Left, of Movement and Ecology (SYN)" if Y`i'Party_Vote == "CoalitionLeft_GRC"

	replace Y`i'Party_Vote = "CourseFreedom_GRC" if BB`i'Party == "GRC: Course of Freedom"
	replace Y`i'Party_Vote_Long = "GRC: Course of Freedom" if Y`i'Party_Vote == "CourseFreedom_GRC"

	replace Y`i'Party_Vote = "DemocraticAlignment_GRC" if BB`i'Party == "GRC: Democratic Alignment"
	replace Y`i'Party_Vote_Long = "GRC: Democratic Alignment" if Y`i'Party_Vote == "DemocraticAlignment_GRC"

	replace Y`i'Party_Vote = "Independent_GRC" if BB`i'Party == "GRC: Independent Greeks"
	replace Y`i'Party_Vote_Long = "GRC: Independent Greeks" if Y`i'Party_Vote == "Independent_GRC"

	replace Y`i'Party_Vote = "GoldenDawn_GRC" if BB`i'Party == "GRC: People’s Association / Golden Dawn"
	replace Y`i'Party_Vote_Long = "GRC: Golden Dawn" if Y`i'Party_Vote == "GoldenDawn_GRC"

	replace Y`i'Party_Vote = "PopularUnity_GRC" if BB`i'Party == "GRC: Popular Unity"
	replace Y`i'Party_Vote_Long = "GRC: Popular Unity" if Y`i'Party_Vote == "PopularUnity_GRC"

	replace Y`i'Party_Vote = "River_GRC" if BB`i'Party == "GRC: The River"
	replace Y`i'Party_Vote_Long = "GRC: The River" if Y`i'Party_Vote == "River_GRC"
		
	replace Y`i'Party_Vote = "UnionCentrists_GRC" if BB`i'Party == "GRC: Union of Centrists"
	replace Y`i'Party_Vote_Long = "GRC: Union of Centrists" if Y`i'Party_Vote == "UnionCentrists_GRC"

	replace Y`i'Party_Vote = "Anticapitalist_GRC" if BB`i'Party == "GRC: Front of the Greek Anticapitalist Left"
	replace Y`i'Party_Vote_Long = "GRC: Front of the Greek Anticapitalist Left" if Y`i'Party_Vote == "Anticapitalist_GRC"

	/* Testing Greece*/
	tab Y`i'Party_Vote_Long if BB_Country=="Greece" & !missing(BB`i'Party) & Y_No`i'Party_Vote==0, missing
	tab BB`i'Party if BB_Country=="Greece" & Y_No`i'Party_Vote==0 & Y`i'Party_Vote_Long=="."

	count if Y`i'Party_Vote_Long=="." & BB_Country=="Greece" & !missing(BB`i'Party) & Y_No`i'Party_Vote==0
	assert r(N)==0
}

tab BB_Party BB_Year if Y_Party_Vote_Long=="." & BB_Country=="Greece" & !missing(BB_Party) & Y_No_Party_Vote==0
tab BB_2nd_Party BB_Year if Y_2nd_Party_Vote_Long=="." & BB_Country=="Greece" & !missing(BB_2nd_Party) & Y_No_2nd_Party_Vote==0
tab BB_Least_Party BB_Year if Y_Least_Party_Vote_Long=="." & BB_Country=="Greece" & !missing(BB_Least_Party) & Y_No_Least_Party_Vote==0
/**********************************
******* Iceland ********************
**********************************/
foreach party in BB_Party BB_2nd_Party BB_Least_Party {
    if "`party'" == "BB_Party" {
        local i "$prefix_BB_Party"
    }
    else if "`party'" == "BB_2nd_Party" {
        local i "$prefix_BB_2nd_Party"
    }
    else if "`party'" == "BB_Least_Party" {
        local i "$prefix_BB_Least_Party"
    }
	
	replace Y`i'Party_Vote = "PeoplesAlliance_ISL" if BB`i'Party == "IS: Alpyoubandalag - Peoples Alliance"
	replace Y`i'Party_Vote_Long = "ISL: People's Alliance" if Y`i'Party_Vote == "PeoplesAlliance_ISL"

	replace Y`i'Party_Vote = "SocialDemocratic_ISL" if BB`i'Party == "IS: Alpyouflokkur - Social Democratic Party"
	replace Y`i'Party_Vote_Long = "ISL: Social Democratic Party" if Y`i'Party_Vote == "SocialDemocratic_ISL"

	replace Y`i'Party_Vote = "Bright_ISL" if BB`i'Party == "IS: Bright Future"
	replace Y`i'Party_Vote_Long = "ISL: Bright Future" if Y`i'Party_Vote == "Bright_ISL"

	replace Y`i'Party_Vote = "Centre_ISL" if BB`i'Party == "IS: Centre Party"
	replace Y`i'Party_Vote_Long = "ISL: Centre Party" if Y`i'Party_Vote == "Centre_ISL"

	replace Y`i'Party_Vote = "Citizen_ISL" if BB`i'Party == "IS: Citizen Movement"
	replace Y`i'Party_Vote_Long = "ISL: Citizen Movement" if Y`i'Party_Vote == "Citizen_ISL"

	replace Y`i'Party_Vote = "Dawn_ISL" if BB`i'Party == "IS: Dawn - The organization of justice, fairness and democracy"
	replace Y`i'Party_Vote_Long = "ISL: Dawn; justice, fairness, and democracy" if Y`i'Party_Vote == "Dawn_ISL"

	replace Y`i'Party_Vote = "DemocracyMovement_ISL" if BB`i'Party == "IS: Democracy Movement"
	replace Y`i'Party_Vote_Long = "ISL: Democracy Movement" if Y`i'Party_Vote == "DemocracyMovement_ISL"

	replace Y`i'Party_Vote = "Progressive_ISL" if BB`i'Party == "IS: Framsoknarflokkur - Progressive Party" | BB`i'Party == "IS: The Progressive Party"
	replace Y`i'Party_Vote_Long = "ISL: Progressive Party" if Y`i'Party_Vote == "Progressive_ISL"

	replace Y`i'Party_Vote = "Humanist_ISL" if BB`i'Party == "IS: Humanist Party" | BB`i'Party == "IS: The Humanist Party" | BB`i'Party == "IS: Flok mannsins"
	replace Y`i'Party_Vote_Long = "ISL: Humanist Party" if Y`i'Party_Vote == "Humanist_ISL"

	replace Y`i'Party_Vote = "LeftGreen_ISL" if BB`i'Party == "IS: Left - Greens" | regexm(BB`i'Party,  ".*IS: The Left-Green.*") | BB`i'Party == "IS: The Left-Green Movement"
	replace Y`i'Party_Vote_Long = "ISL: Left - Greens" if Y`i'Party_Vote == "LeftGreen_ISL"

	replace Y`i'Party_Vote = "Liberal_ISL" if BB`i'Party == "IS: Liberal Party"
	replace Y`i'Party_Vote_Long = "ISL: Liberal Party" if Y`i'Party_Vote == "Liberal_ISL"

	replace Y`i'Party_Vote = "Pirate_ISL" if BB`i'Party == "IS: Pirate Party"
	replace Y`i'Party_Vote_Long = "ISL: Pirate Party" if Y`i'Party_Vote == "Pirate_ISL"

	replace Y`i'Party_Vote = "National_ISL" if BB`i'Party == "IS: Pjooarflok"
	replace Y`i'Party_Vote_Long = "ISL: Pjooarflok (National Party)" if Y`i'Party_Vote == "National_ISL"

	replace Y`i'Party_Vote = "Reform_ISL" if BB`i'Party == "IS: Reform Party"
	replace Y`i'Party_Vote_Long = "ISL: Reform Party" if Y`i'Party_Vote == "Reform_ISL"

	replace Y`i'Party_Vote = "Womens_ISL" if BB`i'Party == "IS: Samtok um kvennalista - Womens Alliance"
	replace Y`i'Party_Vote_Long = "ISL: Womens Alliance" if Y`i'Party_Vote=="Womens_ISL"

	replace Y`i'Party_Vote = "Independence_ISL" if BB`i'Party == "IS: Sjalfstaeoisflokkur - Independence Party" | BB`i'Party == "IS: The Independence Party"
	replace Y`i'Party_Vote_Long = "ISL: The Independence Party" if Y`i'Party_Vote == "Independence_ISL"

	replace Y`i'Party_Vote = "Peoples_ISL" if regexm(BB`i'Party, ".*IS: The People.*")
	replace Y`i'Party_Vote_Long = "ISL: The People's Party" if Y`i'Party_Vote == "Peoples_ISL"

	replace Y`i'Party_Vote = "NationalFront_ISL" if BB`i'Party == "IS: The Icelandic National Front" | BB`i'Party == "IS: The Icelandic National Front"
	replace Y`i'Party_Vote_Long = "ISL: The Icelandic National Front" if Y`i'Party_Vote == "NationalFront_ISL"

	replace Y`i'Party_Vote = "Christian_ISL" if BB`i'Party == "IS: The Christian Party"
	replace Y`i'Party_Vote_Long = "ISL: The Christian Party" if Y`i'Party_Vote == "Christian_ISL"

	replace Y`i'Party_Vote = "SocialDemAlliance_ISL" if BB`i'Party == "IS: The Alliance" | BB`i'Party == "IS: The Social Democratic Alliance"
	replace Y`i'Party_Vote_Long = "ISL: The Social Democratic Alliance" if Y`i'Party_Vote == "SocialDemAlliance_ISL"

	replace Y`i'Party_Vote = "PeoplesFront_ISL" if BB`i'Party == "IS: The People's Front of Iceland"
	replace Y`i'Party_Vote_Long = "ISL: The People's Front of Iceland" if Y`i'Party_Vote == "PeoplesFront_ISL"

	tab BB`i'Party if BB_Country=="Iceland" & Y`i'Party_Vote=="." & Y_No`i'Party_Vote !=1

	/* Testing Iceland*/
	tab Y`i'Party_Vote_Long if BB_Country=="Iceland" & !missing(BB`i'Party) & Y_No`i'Party_Vote==0, missing
	tab BB`i'Party if BB_Country=="Iceland" & Y_No`i'Party_Vote==0 & Y`i'Party_Vote_Long=="."

	count if Y`i'Party_Vote_Long=="." & BB_Country=="Iceland" & !missing(BB`i'Party) & Y_No`i'Party_Vote==0
	assert r(N)==0
}

tab BB_Party BB_Year if Y_Party_Vote_Long=="." & BB_Country=="Iceland" & !missing(BB_Party) & Y_No_Party_Vote==0
tab BB_2nd_Party BB_Year if Y_2nd_Party_Vote_Long=="." & BB_Country=="Iceland" & !missing(BB_2nd_Party) & Y_No_2nd_Party_Vote==0
tab BB_Least_Party BB_Year if Y_Least_Party_Vote_Long=="." & BB_Country=="Iceland" & !missing(BB_Least_Party) & Y_No_Least_Party_Vote==0
/***************************************
******* Netherlands ********************
***************************************/
foreach party in BB_Party BB_2nd_Party BB_Least_Party {
    if "`party'" == "BB_Party" {
        local i "$prefix_BB_Party"
    }
    else if "`party'" == "BB_2nd_Party" {
        local i "$prefix_BB_2nd_Party"
    }
    else if "`party'" == "BB_Least_Party" {
        local i "$prefix_BB_Least_Party"
    }
	
	replace Y`i'Party_Vote = "Democrats66_NLD" if BB`i'Party == "NL: D66" | BB`i'Party == "NL: Democrats 66" | BB`i'Party == "NLD: Democrats 66"  
	replace Y`i'Party_Vote_Long = "NLD: Democrats 66" if Y`i'Party_Vote == "Democrats66_NLD"

	replace Y`i'Party_Vote = "50Plus_NLD" if BB`i'Party == "NL: 50Plus" | BB`i'Party == "NLD: 50PLUS"
	replace Y`i'Party_Vote_Long = "NLD: 50 Plus" if Y`i'Party_Vote == "50Plus_NLD"

	replace Y`i'Party_Vote = "ChristianDemocraticAppeal_NLD" if BB`i'Party == "NL: CDA" | BB`i'Party == "NL: Christian Democratic Appeal" | BB`i'Party == "NLD: Christian Democratic Appeal"
	replace Y`i'Party_Vote_Long = "NLD: Christian Democratic Appeal" if Y`i'Party_Vote == "ChristianDemocraticAppeal_NLD"

	replace Y`i'Party_Vote = "CentreDemocrats_NLD" if BB`i'Party == "NL: Centrumdemocraten  - Center Democrats"
	replace Y`i'Party_Vote_Long = "NLD: Centre Democrats" if Y`i'Party_Vote == "CentreDemocrats_NLD"

	replace Y`i'Party_Vote = "Centre_NLD" if BB`i'Party == "NL: Centrumpartij" 
	replace Y`i'Party_Vote_Long = "NLD: Centre Party" if Y`i'Party_Vote == "Centre_NLD"

	replace Y`i'Party_Vote = "ChristianUnion_NLD" if BB`i'Party == "NL: Christian Union" | BB`i'Party == "NL: Christian Union (ChristenUnie)" | BB`i'Party == "NLD: Christian Union Party"
	replace Y`i'Party_Vote_Long = "NLD: Christian Union" if Y`i'Party_Vote == "ChristianUnion_NLD"

	replace Y`i'Party_Vote = "DENK_NLD" if BB`i'Party == "NL: Denk" 
	replace Y`i'Party_Vote_Long = "NLD: DENK" if Y`i'Party_Vote == "DENK_NLD"

	replace Y`i'Party_Vote = "DemocracyForum_NLD" if BB`i'Party == "NL: Forum for Democracy" 
	replace Y`i'Party_Vote_Long = "NLD: Forum for Democracy" if Y`i'Party_Vote == "DemocracyForum_NLD"

	replace Y`i'Party_Vote = "PoliticalLeague_NLD" if BB`i'Party == "NL: GPV" 
	replace Y`i'Party_Vote_Long = "NLD: Reformed Political Leaguen(GVP)" if Y`i'Party_Vote == "PoliticalLeague_NLD"

	replace Y`i'Party_Vote = "GreenLeft_NLD" if BB`i'Party == "NL: GreenLeft" | BB`i'Party == "NL: Groen Links" | BB`i'Party == "NLD: Green Left"
	replace Y`i'Party_Vote_Long = "NLD: Green Left" if Y`i'Party_Vote == "GreenLeft_NLD"

	replace Y`i'Party_Vote = "Labour_NLD" if BB`i'Party == "NL: Labour Party" | BB`i'Party == "NL: PvdA" | BB`i'Party == "NLD: Labour Party"
	replace Y`i'Party_Vote_Long = "NLD: Labour Party" if Y`i'Party_Vote == "Labour_NLD"

	replace Y`i'Party_Vote = "Freedom_NLD" if BB`i'Party == "NL: Party for Freedom" | BB`i'Party == "NL: Party for Freedom - PVV" | BB`i'Party == "NLD: Party for Freedom"
	replace Y`i'Party_Vote_Long = "NLD: Party for Freedom (PVV)" if Y`i'Party_Vote == "Freedom_NLD"

	replace Y`i'Party_Vote = "Animals_NLD" if BB`i'Party == "NL: Party for the Animals" | BB`i'Party == "NL: Party for the animals - PvdD" | BB`i'Party == "NLD: Party for the Animals"
	replace Y`i'Party_Vote_Long = "NLD: Party for the Animals" if Y`i'Party_Vote == "Animals_NLD"

	replace Y`i'Party_Vote = "FreedomDemocracy_NLD" if BB`i'Party == "NL: People's Party for Freedom and Democracy" | BB`i'Party == "NL: VVD" | BB`i'Party == "NLD: People´s Party for Freedom and Democracy"
	replace Y`i'Party_Vote_Long = "NLD: People’s Party for Freedom and Democracy" if Y`i'Party_Vote == "FreedomDemocracy_NLD"

	replace Y`i'Party_Vote = "Political_NLD" if BB`i'Party == "NL: Reformed Political Party" | BB`i'Party == "NL: SGP" | BB`i'Party == "NLD: Reformed Political Party"
	replace Y`i'Party_Vote_Long = "NLD: Reformed Political Party(SGP)" if Y`i'Party_Vote == "Political_NLD"

	replace Y`i'Party_Vote = "Socialist_NLD" if BB`i'Party == "NL: Socialist Party" | BB`i'Party == "NL: Socialistische Partij" | BB`i'Party == "NLD: Socialist Party"
	replace Y`i'Party_Vote_Long = "NLD: Socialist Party" if Y`i'Party_Vote == "Socialist_NLD"

	replace Y`i'Party_Vote = "PoliticalFederation_NLD" if BB`i'Party == "NL: RPF"
	replace Y`i'Party_Vote_Long = "NLD: Reformatory Political Federation" if Y`i'Party_Vote == "PoliticalFederation_NLD"

	replace Y`i'Party_Vote = "Green_NLD" if BB`i'Party == "NL: De Groenen - The Green Party"
	replace Y`i'Party_Vote_Long = "NLD: The Green Party" if Y`i'Party_Vote == "Green_NLD"

	replace Y`i'Party_Vote = "ProudNetherlands_NLD" if BB`i'Party == "NL: Group Verdonk\Proud of the Netherlands"
	replace Y`i'Party_Vote_Long = "NLD: Proud of the Netherlands" if Y`i'Party_Vote == "ProudNetherlands_NLD"

	tab BB`i'Party if BB_Country=="Netherlands" & Y`i'Party_Vote=="." & Y_No`i'Party_Vote !=1

	/* Testing Netherlands*/
	tab Y`i'Party_Vote_Long if BB_Country=="Netherlands" & !missing(BB`i'Party) & Y_No`i'Party_Vote==0, missing
	tab BB`i'Party if BB_Country=="Netherlands" & Y_No`i'Party_Vote==0 & Y`i'Party_Vote_Long=="."

	count if Y`i'Party_Vote_Long=="." & BB_Country=="Netherlands" & !missing(BB`i'Party) & Y_No`i'Party_Vote==0
	assert r(N)==0
}

tab BB_Party BB_Year if Y_Party_Vote_Long=="." & BB_Country=="Netherlands" & !missing(BB_Party) & Y_No_Party_Vote==0
tab BB_2nd_Party BB_Year if Y_2nd_Party_Vote_Long=="." & BB_Country=="Netherlands" & !missing(BB_2nd_Party) & Y_No_2nd_Party_Vote==0
tab BB_Least_Party BB_Year if Y_Least_Party_Vote_Long=="." & BB_Country=="Netherlands" & !missing(BB_Least_Party) & Y_No_Least_Party_Vote==0
/**********************************
******* Norway ********************
***********************************/
foreach party in BB_Party BB_2nd_Party BB_Least_Party {
    if "`party'" == "BB_Party" {
        local i "$prefix_BB_Party"
    }
    else if "`party'" == "BB_2nd_Party" {
        local i "$prefix_BB_2nd_Party"
    }
    else if "`party'" == "BB_Least_Party" {
        local i "$prefix_BB_Least_Party"
    }
	
	replace Y`i'Party_Vote = "Centre_NOR" if BB`i'Party == "NO: Center Party (SP)" | BB`i'Party == "NO: Centre Party" | BB`i'Party == "NOR: Center Party"  
	replace Y`i'Party_Vote_Long = "NOR: Centre Party" if Y`i'Party_Vote == "Centre_NOR"

	replace Y`i'Party_Vote = "ChristianDemocratic_NOR" if BB`i'Party == "NO: Christian Democratic Party" | BB`i'Party == "NO: Christian Democratic Party (KrF)" | BB`i'Party == "NOR: Christian Democratic Party"  
	replace Y`i'Party_Vote_Long = "NOR: Christian Democratic Party" if Y`i'Party_Vote == "ChristianDemocratic_NOR"

	replace Y`i'Party_Vote = "Coastal_NOR" if BB`i'Party == "NO: Coastal Party" 
	replace Y`i'Party_Vote_Long = "NOR: Coastal Party" if Y`i'Party_Vote == "Coastal_NOR"

	replace Y`i'Party_Vote = "Conservative_NOR" if BB`i'Party == "NO: Conservative Party" | BB`i'Party == "NO: Conservative Party (H)" | BB`i'Party == "NOR: Conservative Party"  
	replace Y`i'Party_Vote_Long = "NOR: Conservative Party" if Y`i'Party_Vote == "Conservative_NOR"

	replace Y`i'Party_Vote = "Green_NOR" if BB`i'Party == "NO: Green Party"
	replace Y`i'Party_Vote_Long = "NOR: Green Party" if Y`i'Party_Vote == "Green_NOR"

	replace Y`i'Party_Vote = "Labour_NOR" if BB`i'Party == "NO: Labour Party" | BB`i'Party == "NO: Labour Party (DNA)" | BB`i'Party == "NOR: Labour Party"  
	replace Y`i'Party_Vote_Long = "NOR: Labour Party" if Y`i'Party_Vote == "Labour_NOR"

	replace Y`i'Party_Vote = "Liberal_NOR" if BB`i'Party == "NO: Liberal Party" | BB`i'Party == "NO: Liberals (V)" | BB`i'Party == "NOR: Liberal Party"  
	replace Y`i'Party_Vote_Long = "NOR: Liberal Party" if Y`i'Party_Vote == "Liberal_NOR"

	replace Y`i'Party_Vote = "Progress_NOR" if BB`i'Party == "NO: Progress Party" | BB`i'Party == "NO: Progressive Party (FrP)" | BB`i'Party == "NOR: Progress Party"  
	replace Y`i'Party_Vote_Long = "NOR: Progress Party" if Y`i'Party_Vote == "Progress_NOR"

	replace Y`i'Party_Vote = "Red_NOR" if BB`i'Party == "NO: Red" | BB`i'Party == "NO: Red Party"  
	replace Y`i'Party_Vote_Long = "NOR: Red Party" if Y`i'Party_Vote == "Red_NOR"

	replace Y`i'Party_Vote = "SocialistLeft_NOR" if BB`i'Party == "NO: Socialist Left Party" | BB`i'Party == "NO: Socialist Left Party (SV)" | BB`i'Party == "NOR: Socialist Left Party"  
	replace Y`i'Party_Vote_Long = "NOR: Socialist Left Party" if Y`i'Party_Vote == "SocialistLeft_NOR"

	replace Y`i'Party_Vote = "TheChristians_NOR" if BB`i'Party == "NO: The Christians"   
	replace Y`i'Party_Vote_Long = "NOR: The Christians" if Y`i'Party_Vote == "TheChristians_NOR"

	replace Y`i'Party_Vote = "Communist_NOR" if BB`i'Party == "NOR: Communist Party" | BB`i'Party == "NO: M-L Party"  
	replace Y`i'Party_Vote_Long = "NOR: Norwegian Communist Party" if Y`i'Party_Vote == "Communist_NOR"

	replace Y`i'Party_Vote = "RedAlliance_NOR" if BB`i'Party == "NOR: Red Alliance"   
	replace Y`i'Party_Vote_Long = "NOR: Red Alliance" if Y`i'Party_Vote == "RedAlliance_NOR"

	replace Y`i'Party_Vote = "Democrats_NOR" if BB`i'Party == "NO: Democrats in Norway"   
	replace Y`i'Party_Vote_Long = "NOR: Democrats in Norway" if Y`i'Party_Vote == "Democrats_NOR"

	replace Y`i'Party_Vote = "Pensioners_NOR" if BB`i'Party == "NO: Pensioners' Party"   
	replace Y`i'Party_Vote_Long = "NOR: Pensioners' Party" if Y`i'Party_Vote == "Pensioners_NOR"
	 
	tab BB`i'Party if BB_Country=="Norway" & Y`i'Party_Vote=="." & Y_No`i'Party_Vote !=1

	/* Testing Norway*/
	tab Y`i'Party_Vote_Long if BB_Country=="Norway" & !missing(BB`i'Party) & Y_No`i'Party_Vote==0, missing
	tab BB`i'Party if BB_Country=="Norway" & Y_No`i'Party_Vote==0 & Y`i'Party_Vote_Long=="."

	count if Y`i'Party_Vote_Long=="." & BB_Country=="Norway" & !missing(BB`i'Party) & Y_No`i'Party_Vote==0
	assert r(N)==0
}

tab BB_Party BB_Year if Y_Party_Vote_Long=="." & BB_Country=="Norway" & !missing(BB_Party) & Y_No_Party_Vote==0
tab BB_2nd_Party BB_Year if Y_2nd_Party_Vote_Long=="." & BB_Country=="Norway" & !missing(BB_2nd_Party) & Y_No_2nd_Party_Vote==0
tab BB_Least_Party BB_Year if Y_Least_Party_Vote_Long=="." & BB_Country=="Norway" & !missing(BB_Least_Party) & Y_No_Least_Party_Vote==0
/***************************************
******* Switzerland ********************
***************************************/
foreach party in BB_Party BB_2nd_Party BB_Least_Party {
    if "`party'" == "BB_Party" {
        local i "$prefix_BB_Party"
    }
    else if "`party'" == "BB_2nd_Party" {
        local i "$prefix_BB_2nd_Party"
    }
    else if "`party'" == "BB_Least_Party" {
        local i "$prefix_BB_Least_Party"
    }
	
	replace Y`i'Party_Vote = "ChristianDemocratic_CHE" if BB`i'Party == "CH: Christian Democratic Party" | BB`i'Party == "CH: Christlich-demokratische volkspartei CVP" | BB`i'Party == "CHE: Christian Demmocrats" | BB`i'Party == "CHE: Christian Democratic People´s Party" 
	replace Y`i'Party_Vote_Long = "CHE: Christian Democratic People’s Party of Switzerland" if Y`i'Party_Vote == "ChristianDemocratic_CHE"

	replace Y`i'Party_Vote = "ChristianSocial_CHE" if BB`i'Party == "CH: Christlichsoziale partei CSP" | BB`i'Party == "CHE: Christian Social Party" 
	replace Y`i'Party_Vote_Long = "CHE: Christian Social Party" if Y`i'Party_Vote == "ChristianSocial_CHE"

	replace Y`i'Party_Vote = "ConservativeDemocrat_CHE" if BB`i'Party == "CH: Conservative Democratic Party" 
	replace Y`i'Party_Vote_Long = "CHE: Conservative Democratic Party of Switzerland" if Y`i'Party_Vote == "ConservativeDemocrat_CHE"

	replace Y`i'Party_Vote = "FederalDemocraticUnion_CHE" if BB`i'Party == "CH: Eidgensisch demokratische union (edu)" | BB`i'Party == "CH: Federal Democratic Union" 
	replace Y`i'Party_Vote_Long = "CHE: Federal Democratic Union" if Y`i'Party_Vote == "FederalDemocraticUnion_CHE"

	replace Y`i'Party_Vote = "Protestant_CHE" if BB`i'Party == "CH: Evangelical People's Party" | BB`i'Party == "CH: Evangelische volkspartei EVP" | BB`i'Party == "CHE: Evangelical People´s Party of Switzerland" | BB`i'Party == "CHE: Evangelical Protestant"
	replace Y`i'Party_Vote_Long = "CHE: Protestant People’s Party of Switzerland" if Y`i'Party_Vote == "Protestant_CHE"

	replace Y`i'Party_Vote = "RadicalDemocratic_CHE" if BB`i'Party == "CH: Freisinnig-demokratische partei FDP" | BB`i'Party == "CH: The Liberals (Merge from Radicals and Liberals)" | BB`i'Party == "CHE: The Liberals" | BB`i'Party == "CHE: Radical"
	replace Y`i'Party_Vote_Long = "CHE: FDP The Liberals / Radical Democratic Party" if Y`i'Party_Vote == "RadicalDemocratic_CHE"

	replace Y`i'Party_Vote = "GreenLiberal_CHE" if BB`i'Party == "CH: Green Liberal Party" | BB`i'Party == "CH: Green liberal party" 
	replace Y`i'Party_Vote_Long = "CHE: Green Liberal Party" if Y`i'Party_Vote == "GreenLiberal_CHE"

	replace Y`i'Party_Vote = "Green_CHE" if BB`i'Party == "CH: Green Party" | BB`i'Party == "CH: Green party" | BB`i'Party == "CHE: Green Party of Switzerland"
	replace Y`i'Party_Vote_Long = "CHE: Green Party of Switzerland" if Y`i'Party_Vote == "Green_CHE"

	replace Y`i'Party_Vote = "TicinoLeague_CHE" if BB`i'Party == "CH: Lega dei ticinesi" | BB`i'Party == "CH: Ticino League" | BB`i'Party == "CHE: Ticino League"
	replace Y`i'Party_Vote_Long = "CHE: Ticino League" if Y`i'Party_Vote == "TicinoLeague_CHE"

	replace Y`i'Party_Vote = "Liberal_CHE" if BB`i'Party == "CH: Liberale partei LPS" | BB`i'Party == "CHE: Liberal" | BB`i'Party == "CHE: Liberale partei"
	replace Y`i'Party_Vote_Long = "CHE: Liberal Party of Switzerland" if Y`i'Party_Vote == "Liberal_CHE"

	replace Y`i'Party_Vote = "Labour_CHE" if BB`i'Party == "CH: Partei der arbeit PdA" | BB`i'Party == "CH: Swiss Labour Party" | BB`i'Party == "CHE: Swiss Party of Labour" | BB`i'Party == "CHE: Labor Party (communist)"
	replace Y`i'Party_Vote_Long = "CHE: Swiss Labour Party" if Y`i'Party_Vote == "Labour_CHE"

	replace Y`i'Party_Vote = "Democrats_CHE" if BB`i'Party == "CH: Schweizer demokraten SD" | BB`i'Party == "CHE: Swiss Democrats" | BB`i'Party == "CHE: National Action/Vigilance" 
	replace Y`i'Party_Vote_Long = "CHE: Swiss Democrats" if Y`i'Party_Vote == "Democrats_CHE"

	replace Y`i'Party_Vote = "People_CHE" if BB`i'Party == "CH: Schweizerische volkspartei SVP" | BB`i'Party == "CH: Swiss People's Party" | BB`i'Party == "CHE: Swiss People´s Party" | BB`i'Party == "CHE: Democratic Union of the Center"
	replace Y`i'Party_Vote_Long = "CHE: Swiss People's Party" if Y`i'Party_Vote == "People_CHE"

	replace Y`i'Party_Vote = "SocialDemocrat_CHE" if BB`i'Party == "CH: Social Democratic Party  (socialist)" | BB`i'Party == "CH: Sozialdemokratische partei SPS" | BB`i'Party == "CHE: Social Democratic Party of Switzerland" | BB`i'Party == "CHE: Socialist" | BB`i'Party == "CH: Social Democratic Party (socialist)"
	replace Y`i'Party_Vote_Long = "CHE: Social Democratic Party of Switzerland" if Y`i'Party_Vote == "SocialDemocrat_CHE"

	replace Y`i'Party_Vote = "Keine_CHE" if BB`i'Party == "CHE: Keine" | BB`i'Party == "CHE: Keine Angabe" | BB`i'Party == "CHE: Kann s n entsch" 
	replace Y`i'Party_Vote_Long = "CHE: Keine" if Y`i'Party_Vote == "Keine_CHE"

	replace Y`i'Party_Vote = "AlternativeLeft_CHE" if BB`i'Party == "CH: The alternative Left"
	replace Y`i'Party_Vote_Long = "CHE: The alternative Left" if Y`i'Party_Vote == "AlternativeLeft_CHE"

	replace Y`i'Party_Vote = "Independents_CHE" if BB`i'Party == "CHE: Alliance of Independents AdI" | BB`i'Party == "CHE: LdU"
	replace Y`i'Party_Vote_Long = "CHE: Independents’ Alliance" if Y`i'Party_Vote == "Independents_CHE"

	replace Y`i'Party_Vote = "AlternativeList_CHE" if BB`i'Party == "CHE: Alternative liste (al)"
	replace Y`i'Party_Vote_Long = "CHE: Alternative list" if Y`i'Party_Vote == "AlternativeList_CHE"

	replace Y`i'Party_Vote = "Freedom_CHE" if BB`i'Party == "CHE: Freiheitspartei"
	replace Y`i'Party_Vote_Long = "CHE: Freedom Party of Switzerland" if Y`i'Party_Vote == "Freedom_CHE"

	replace Y`i'Party_Vote = "Solidarity_CHE" if BB`i'Party == "CHE: Solidarit"
	replace Y`i'Party_Vote_Long = "CHE: Solidarity" if Y`i'Party_Vote == "Solidarity_CHE"
	 
	replace Y`i'Party_Vote = "Pirate_CHE" if BB`i'Party == "CH: Pirate Party"
	replace Y`i'Party_Vote_Long = "CHE: Pirate Party" if Y`i'Party_Vote == "Pirate_CHE"
	 
	replace Y`i'Party_Vote = "Ecologist_CHE" if BB`i'Party == "CHE: Ecologist"
	replace Y`i'Party_Vote_Long = "CHE: Ecologist" if Y`i'Party_Vote == "Ecologist_CHE"

	replace Y`i'Party_Vote = "Frap_CHE" if BB`i'Party == "CHE: Frap"
	replace Y`i'Party_Vote_Long = "CHE: Frap" if Y`i'Party_Vote == "Frap_CHE"

	replace Y`i'Party_Vote = "MovementFrenchSpeaking_CHE" if BB`i'Party == "CH: Movement of the Citizens of French-speaking Switzerland"
	replace Y`i'Party_Vote_Long = "CHE: Movement of the Citizens of French-speaking Switzerland" if Y`i'Party_Vote == "MovementFrenchSpeaking_CHE"

	replace Y`i'Party_Vote = "ChristlichDemocraticUnion_CHE" if BB`i'Party == "CHE: Christlich Demokratische Union Deutschlands"
	replace Y`i'Party_Vote_Long = "CHE: Christlich Demokratische Union Deutschlands" if Y`i'Party_Vote == "ChristlichDemocraticUnion_CHE"

	tab BB`i'Party if BB_Country=="Switzerland" & Y`i'Party_Vote=="." & Y_No`i'Party_Vote !=1

	/* Testing Switzerland*/
	tab Y`i'Party_Vote_Long if BB_Country=="Switzerland" & !missing(BB`i'Party) & Y_No`i'Party_Vote==0, missing
	tab BB`i'Party if BB_Country=="Switzerland" & Y_No`i'Party_Vote==0 & Y`i'Party_Vote_Long=="."  

	count if Y`i'Party_Vote_Long=="." & BB_Country=="Switzerland" & !missing(BB`i'Party) & Y_No`i'Party_Vote==0
	assert r(N)==0
}

tab BB_Party BB_Year if Y_Party_Vote_Long=="." & BB_Country=="Switzerland" & !missing(BB_Party) & Y_No_Party_Vote==0
tab BB_2nd_Party BB_Year if Y_2nd_Party_Vote_Long=="." & BB_Country=="Switzerland" & !missing(BB_2nd_Party) & Y_No_2nd_Party_Vote==0
tab BB_Least_Party BB_Year if Y_Least_Party_Vote_Long=="." & BB_Country=="Switzerland" & !missing(BB_Least_Party) & Y_No_Least_Party_Vote==0
/************************************
******* Portugal ********************
************************************/
foreach party in BB_Party BB_2nd_Party BB_Least_Party {
    if "`party'" == "BB_Party" {
        local i "$prefix_BB_Party"
    }
    else if "`party'" == "BB_2nd_Party" {
        local i "$prefix_BB_2nd_Party"
    }
    else if "`party'" == "BB_Least_Party" {
        local i "$prefix_BB_Least_Party"
    }
	
	replace Y`i'Party_Vote = "Enough_PRT" if BB`i'Party == "PRT: Enough" | BB`i'Party == "PT: Enough"
	replace Y`i'Party_Vote_Long = "PRT: Enough" if Y`i'Party_Vote == "Enough_PRT"

	replace Y`i'Party_Vote = "LeftBloc_PRT" if BB`i'Party == "PRT: Left Block" | BB`i'Party == "PT: Bloco de esquerda - Left Block" | BB`i'Party == "PT: Left Block"
	replace Y`i'Party_Vote_Long = "PRT: Left Bloc" if Y`i'Party_Vote == "LeftBloc_PRT"

	replace Y`i'Party_Vote = "PeopleAnimalsNature_PRT" if BB`i'Party == "PRT: People–Animals–Nature" | BB`i'Party == "PT: People–Animals–Nature"
	replace Y`i'Party_Vote_Long = "PRT: People-Animals-Nature" if Y`i'Party_Vote == "PeopleAnimalsNature_PRT"

	replace Y`i'Party_Vote = "Free_PRT" if BB`i'Party == "PRT: FREE" | BB`i'Party == "PT: FREE"
	replace Y`i'Party_Vote_Long = "PRT: Free" if Y`i'Party_Vote == "Free_PRT"

	replace Y`i'Party_Vote = "PortugueseCommunist_PRT" if BB`i'Party == "PRT: Portuguese Communist Party"  | BB`i'Party == "PT: Portuguese Communist Party" | BB`i'Party == "PT: PC( R ) - Communist Party Reconstituted" | BB`i'Party == "PT: CDU-PCP - Communist Party"
	replace Y`i'Party_Vote_Long = "PRT: Portuguese Communist Party" if Y`i'Party_Vote == "PortugueseCommunist_PRT"

	replace Y`i'Party_Vote = "WorkersCommunist_PRT" if BB`i'Party == "PT: PCTP/MRPP - Communist Party of Portugese Workers" | BB`i'Party == "PRT: Portuguese Workers' Communist Party" | BB`i'Party == "PT: Communist Party of Portuguese Workers/Reorganizing Movement of the Portuguese Proletariat"
	replace Y`i'Party_Vote_Long = "PRT: Portuguese Workers' Communist Party" if Y`i'Party_Vote == "WorkersCommunist_PRT"

	replace Y`i'Party_Vote = "Alliance_PRT" if BB`i'Party == "PRT: Alliance" | BB`i'Party == "PT: Alliance"
	replace Y`i'Party_Vote_Long = "PRT: Alliance" if Y`i'Party_Vote == "Alliance_PRT"

	replace Y`i'Party_Vote = "SocialDemocrat_PRT" if BB`i'Party == "PRT: Social Democratic Party" | BB`i'Party == "PT: PPD-PSD - Social Democrats" | BB`i'Party == "PT: Social Democratic Party"
	replace Y`i'Party_Vote_Long = "PRT: Social Democratic Party" if Y`i'Party_Vote == "SocialDemocrat_PRT"

	replace Y`i'Party_Vote = "SocialDemocratCenter_PRT" if BB`i'Party == "PRT: Social Democratic Centre - Popular Party" | BB`i'Party == "PT: Social Democratic Centre - Popular Party" | BB`i'Party == "PT: CDS/PP - Social Democratic Christians"
	replace Y`i'Party_Vote_Long = "PRT: Social Democratic Center-Popular Party" if Y`i'Party_Vote == "SocialDemocratCenter_PRT"

	replace Y`i'Party_Vote = "Socialist_PRT" if BB`i'Party == "PRT: Socialist Party" | BB`i'Party == "PT: PS - Socialist Party" | BB`i'Party == "PT: Socialist Party"
	replace Y`i'Party_Vote_Long = "PRT: Socialist Party" if Y`i'Party_Vote == "Socialist_PRT"

	replace Y`i'Party_Vote = "LeftWing_PRT" if BB`i'Party == "PT: MRPP - Left Wing Party" | BB`i'Party == "PT: PSR - Left Wing Party" | BB`i'Party == "PT: UDP - Left Wing Party"
	replace Y`i'Party_Vote_Long = "PRT: Leftwing Union for the Socialist Democracy" if Y`i'Party_Vote == "LeftWing_PRT"

	replace Y`i'Party_Vote = "SocialistAlternative_PRT" if BB`i'Party == "PRT: Alternative Socialist Movement" | BB`i'Party == "PT: Alternative Socialist Movement"
	replace Y`i'Party_Vote_Long = "PRT: Socialist Alternative Movement" if Y`i'Party_Vote == "SocialistAlternative_PRT"

	replace Y`i'Party_Vote = "Earth_PRT" if BB`i'Party == "PRT: Earth Party" | BB`i'Party == "PT: Earth Party"
	replace Y`i'Party_Vote_Long = "PRT: Earth Party" if Y`i'Party_Vote == "Earth_PRT"

	replace Y`i'Party_Vote = "Liberal_PRT" if BB`i'Party == "PRT: Liberal Initiative" | BB`i'Party == "PT: Liberal Initiative"
	replace Y`i'Party_Vote_Long = "PRT: Liberal Initiative" if Y`i'Party_Vote == "Liberal_PRT"

	replace Y`i'Party_Vote = "PopularMonarchist_PRT" if BB`i'Party == "PT: PPM - Peoples Monarchist Party"
	replace Y`i'Party_Vote_Long = "PRT: Popular Monarchist Party" if Y`i'Party_Vote == "PopularMonarchist_PRT"

	replace Y`i'Party_Vote = "DemocraticRenewal_PRT" if BB`i'Party == "PT: PRD - Democratic Renewal Party"
	replace Y`i'Party_Vote_Long = "PRT: Democratic Renewal Party" if Y`i'Party_Vote == "DemocraticRenewal_PRT"

	replace Y`i'Party_Vote = "PopularDemocratic_PRT" if BB`i'Party == "PT: MDP/CDE - Portugese Democratic Party"
	replace Y`i'Party_Vote_Long = "PRT: Popular Democratic Movement" if Y`i'Party_Vote == "PopularDemocratic_PRT"

	tab BB`i'Party if BB_Country=="Portugal" & Y`i'Party_Vote=="." & Y_No`i'Party_Vote !=1

	/* Testing Portugal*/
	tab Y`i'Party_Vote_Long if BB_Country=="Portugal" & !missing(BB`i'Party) & Y_No`i'Party_Vote==0, missing
	tab BB`i'Party if BB_Country=="Portugal" & Y_No`i'Party_Vote==0 & Y`i'Party_Vote_Long=="."  

	count if Y`i'Party_Vote_Long=="." & BB_Country=="Portugal" & !missing(BB`i'Party) & Y_No`i'Party_Vote==0
	assert r(N)==0
}

tab BB_Party BB_Year if Y_Party_Vote_Long=="." & BB_Country=="Portugal" & !missing(BB_Party) & Y_No_Party_Vote==0
tab BB_2nd_Party BB_Year if Y_2nd_Party_Vote_Long=="." & BB_Country=="Portugal" & !missing(BB_2nd_Party) & Y_No_2nd_Party_Vote==0
tab BB_Least_Party BB_Year if Y_Least_Party_Vote_Long=="." & BB_Country=="Portugal" & !missing(BB_Least_Party) & Y_No_Least_Party_Vote==0
/************************************
******* Slovakia  *******************
************************************/

foreach party in BB_Party BB_2nd_Party BB_Least_Party {
    if "`party'" == "BB_Party" {
        local i "$prefix_BB_Party"
    }
    else if "`party'" == "BB_2nd_Party" {
        local i "$prefix_BB_2nd_Party"
    }
    else if "`party'" == "BB_Least_Party" {
        local i "$prefix_BB_Least_Party"
    }
	
	replace Y`i'Party_Vote = "NewCitizen_SVK" if BB`i'Party == "SK: Aliancia noveho obcana - ANO"
	replace Y`i'Party_Vote_Long = "SVK: Alliance of the New Citizen" if Y`i'Party_Vote == "NewCitizen_SVK"

	replace Y`i'Party_Vote = "ChristianDemocratic_SVK" if BB`i'Party == "SK: Christian Democratic Movement" | BB`i'Party == "SK: Krestanskodemokraticke hnutie (KDH) - Christian-Democratic Movement" | BB`i'Party == "SVK: Christian-Democratic Movement"
	replace Y`i'Party_Vote_Long = "SVK: Christian Democratic Movement" if Y`i'Party_Vote == "ChristianDemocratic_SVK"

	replace Y`i'Party_Vote = "HungarianChristianDemocratic_SVK" if BB`i'Party == "SK: Madarske krestansko-demokraticke hnutie (MKM) - Hungarian Christian-Democratic Movement" 
	replace Y`i'Party_Vote_Long = "SVK: Hungarian Christian-Democratic Movement" if Y`i'Party_Vote == "HungarianChristianDemocratic_SVK"

	replace Y`i'Party_Vote = "Communist_SVK" if BB`i'Party == "SK: Communist Party of Slovakia" | BB`i'Party == "SK: Komunistická strana Slovenska (KSS) - Communist Party of Slovakia" | BB`i'Party == "SVK: Communist Party of Slovakia"
	replace Y`i'Party_Vote_Long = "SVK: Communist Party of Slovakia" if Y`i'Party_Vote == "Communist_SVK"

	replace Y`i'Party_Vote = "DirectionSocialDemocracy_SVK" if BB`i'Party == "SK: DIRECTION - Social Democracy" | BB`i'Party == "SK: Socialna demokracia (SMER)"
	replace Y`i'Party_Vote_Long = "SVK: Direction-Social Democracy" if Y`i'Party_Vote == "DirectionSocialDemocracy_SVK"

	replace Y`i'Party_Vote = "DemocraticUnion_SVK" if BB`i'Party == "SK: Demokratickia Slovenska (DU) - Democratic Union of Slovakia" | BB`i'Party == "SVK: Democratic Union of Slovakia"
	replace Y`i'Party_Vote_Long = "SVK: Democratic Union of Slovakia" if Y`i'Party_Vote == "DemocraticUnion_SVK"

	replace Y`i'Party_Vote = "FreedomSolidarity_SVK" if BB`i'Party == "SK: Freedom and Solidarity" 
	replace Y`i'Party_Vote_Long = "SVK: Freedom and Solidarity" if Y`i'Party_Vote == "FreedomSolidarity_SVK"

	replace Y`i'Party_Vote = "MovementDemocratic_SVK" if BB`i'Party == "SK: Hnutie za demokraticke Slovensko (HZDS) - Movement for a Democratic Slovakia" | BB`i'Party == "SK: Ludova strana-Hnutie za demokraticke Slovensko - LS-HZDS" | BB`i'Party == "SVK: Movement for a Democratic Slovakia" 
	replace Y`i'Party_Vote_Long = "SVK: Movement for a Democratic Slovakia" if Y`i'Party_Vote == "MovementDemocratic_SVK"

	replace Y`i'Party_Vote = "Bridge_SVK" if BB`i'Party == "SK: MOST - HÍD" 
	replace Y`i'Party_Vote_Long = "SVK: Bridge" if Y`i'Party_Vote == "Bridge_SVK"

	replace Y`i'Party_Vote = "Kotleba_SVK" if BB`i'Party == "SK: Kotleba - People's Party Our Slovakia" | BB`i'Party == "SVK: People's Party Our Slovakia" | BB`i'Party == "SVK: Slovak People's Party"
	replace Y`i'Party_Vote_Long = "SVK: People's Party Our Slovakia" if Y`i'Party_Vote == "Kotleba_SVK"

	replace Y`i'Party_Vote = "HungarianCoalition_SVK" if BB`i'Party == "SK: Madrskkoalia (SMK) Hungarian Coalition" | BB`i'Party == "SK: The Party of the Hungarian Community" | BB`i'Party == "SVK: Party of the Hungarian Community"
	replace Y`i'Party_Vote_Long = "SVK: Party of the Hungarian Coalition" if Y`i'Party_Vote == "HungarianCoalition_SVK"

	replace Y`i'Party_Vote = "Coexistence_SVK" if BB`i'Party == "SVK: Coexistence (COEX, EGY) - Spoluzitie (Egyutelles) - Coe"  | BB`i'Party == "SK: Spoluzitie (Egyutelles) - Coexistence"
	replace Y`i'Party_Vote_Long = "SVK: Coexistence" if Y`i'Party_Vote == "Coexistence_SVK"

	replace Y`i'Party_Vote = "Network_SVK" if BB`i'Party == "SK: NETWORK"
	replace Y`i'Party_Vote_Long = "SVK: Network" if Y`i'Party_Vote == "Network_SVK"

	replace Y`i'Party_Vote = "OrdinaryIndependent_SVK" if BB`i'Party == "SK: Ordinary People and Independent Personalities" 
	replace Y`i'Party_Vote_Long = "SVK: Ordinary People and Independent Personalities" if Y`i'Party_Vote == "OrdinaryIndependent_SVK"

	replace Y`i'Party_Vote = "FreeForum_SVK" if BB`i'Party == "SK: Slobodne forum - SF" 
	replace Y`i'Party_Vote_Long = "SVK: Free Forum" if Y`i'Party_Vote == "FreeForum_SVK"

	replace Y`i'Party_Vote = "National_SVK" if BB`i'Party == "SK: Slovak National Party" | BB`i'Party == "SK: Slovenska narodna strana (SNS) - Slovak National Party" | BB`i'Party == "SVK: Slovak National Party"
	replace Y`i'Party_Vote_Long = "SVK: Slovak National Party" if Y`i'Party_Vote == "National_SVK"

	replace Y`i'Party_Vote = "DemocraticChristianUnion_SVK" if BB`i'Party == "SK: Slovenska demokraticka a krestanska unia-Demokraticka strana - SdkU-DS" 
	replace Y`i'Party_Vote_Long = "SVK: Slovak Democratic and Christian Union - Democartic Party" if Y`i'Party_Vote == "DemocraticChristianUnion_SVK"

	replace Y`i'Party_Vote = "SocialDemocratic_SVK" if BB`i'Party == "SK: Socialnodemokraticka strana na Slovensku (SDSS) - Social Democratic Party in Slovakia" | BB`i'Party == "SVK: Social Democratic Party of Slovakia" 
	replace Y`i'Party_Vote_Long = "SVK: Social Democratic Party of Slovakia" if Y`i'Party_Vote == "SocialDemocratic_SVK"

	replace Y`i'Party_Vote = "DemocraticLeft_SVK" if BB`i'Party == "SK: Strana demokratickej l avice (SDL) - Party of the Democratic Left"
	replace Y`i'Party_Vote_Long = "SVK: Party of the Democratic Left" if Y`i'Party_Vote == "DemocraticLeft_SVK"

	replace Y`i'Party_Vote = "CivicUnderstanding_SVK" if BB`i'Party == "SK: Strana obcianskeho porozumenia (SOP) - Party of Civil Understanding" | BB`i'Party == "SVK: Party of Civil Understanding" 
	replace Y`i'Party_Vote_Long = "SVK: Party of Civic Understanding" if Y`i'Party_Vote == "CivicUnderstanding_SVK"

	replace Y`i'Party_Vote = "AgainstViolence_SVK" if BB`i'Party == "SK: Verejnost proti nasiliu (VPN) - Public against violence" | BB`i'Party == "SVK: Public Against Violence" 
	replace Y`i'Party_Vote_Long = "SVK: Public Against Violence" if Y`i'Party_Vote == "AgainstViolence_SVK"

	replace Y`i'Party_Vote = "Family_SVK" if BB`i'Party == "SK: We Are Family - Boris Kollár" 
	replace Y`i'Party_Vote_Long = "SVK: We Are Family" if Y`i'Party_Vote == "Family_SVK"

	replace Y`i'Party_Vote = "WorkersAssociation_SVK" if BB`i'Party == "SK: Zdruzenie Robotnikov Slovenska (ZRS) - Association of Workers of Slovakia" | BB`i'Party == "SVK: Association of Workers of Slovakia" 
	replace Y`i'Party_Vote_Long = "SVK: Workers’ Association of Slovakia" if Y`i'Party_Vote == "WorkersAssociation_SVK"

	replace Y`i'Party_Vote = "CivicDemocratic_SVK" if BB`i'Party == "SVK: Civic Democratic Party" 
	replace Y`i'Party_Vote_Long = "SVK: Civic Democratic Party" if Y`i'Party_Vote == "CivicDemocratic_SVK"

	replace Y`i'Party_Vote = "Farmers_SVK" if BB`i'Party == "SK: Entrepreneurs & Farmers Union (UZPR)" | BB`i'Party == "SVK: Republican Party of Farmers and Peasants" 
	replace Y`i'Party_Vote_Long = "SVK: Republican Party of Farmers and Peasants" if Y`i'Party_Vote == "Farmers_SVK"

	replace Y`i'Party_Vote = "ChristianSocial_SVK" if BB`i'Party == "SVK: Christian Social Union of Slovakia" 
	replace Y`i'Party_Vote_Long = "SVK: Christian Social Union of Slovakia" if Y`i'Party_Vote == "ChristianSocial_SVK"

	replace Y`i'Party_Vote = "Businessmen_SVK" if BB`i'Party == "SK: Party of Businessmen and Entrepreneurs (SPZ)" 
	replace Y`i'Party_Vote_Long = "SVK: Party of Businessmen and Entrepreneurs" if Y`i'Party_Vote == "Businessmen_SVK"

	replace Y`i'Party_Vote = "Green_SVK" if BB`i'Party == "SK: Slovak Green Party" | BB`i'Party == "SK: Strana zelenych na Slovensku (SZS) - Party of Greens in Slovakia" | BB`i'Party == "SVK: Green Party – Green Alternative" | BB`i'Party == "SVK: Slovak Green Alterntive"
	replace Y`i'Party_Vote_Long = "SVK: Green Party of Slovakia" if Y`i'Party_Vote == "Green_SVK"

	replace Y`i'Party_Vote = "Democratic_SVK" if BB`i'Party == "SK: Demokraticka Strana (DS) - Democratic Party" | BB`i'Party == "SVK: Democratic Party" | BB`i'Party == "SVK: Party of the Democratic"
	replace Y`i'Party_Vote_Long = "SVK: Democratic Party" if Y`i'Party_Vote == "Democratic_SVK"

	replace Y`i'Party_Vote = "DemocraticCoalition_SVK" if BB`i'Party == "SK: Democratic Coalition (SDK)" | BB`i'Party == "SVK: Democratic Coalition ()"
	replace Y`i'Party_Vote = "DemocraticCoalition_SVK" if Y`i'Party_Vote == "Democratic_SVK" & 1998<=BB_Year // This party has not run independently since the 1998 election, when it was part of the democratic coalition.
	replace Y`i'Party_Vote = "DemocraticCoalition_SVK" if Y`i'Party_Vote == "Green_SVK" & 1998<=BB_Year & BB_Year<=1999 // This party has not run independently in the 1998 election, it was part of the democratic coalition.
	replace Y`i'Party_Vote_Long = "SVK: Slovak Democratic Coalition" if Y`i'Party_Vote == "DemocraticCoalition_SVK"

	tab BB`i'Party if BB_Country=="Slovakia" & Y`i'Party_Vote=="." & Y_No`i'Party_Vote !=1

	/* Testing Slovakia*/
	tab Y`i'Party_Vote_Long if BB_Country=="Slovakia" & !missing(BB`i'Party) & Y_No`i'Party_Vote==0, missing
	tab BB`i'Party if BB_Country=="Slovakia" & Y_No`i'Party_Vote==0 & Y`i'Party_Vote_Long=="."  

	count if Y`i'Party_Vote_Long=="." & BB_Country=="Slovakia" & !missing(BB`i'Party) & Y_No`i'Party_Vote==0
	assert r(N)==0
}

tab BB_Party BB_Year if Y_Party_Vote_Long=="." & BB_Country=="Slovakia" & !missing(BB_Party) & Y_No_Party_Vote==0
tab BB_2nd_Party BB_Year if Y_2nd_Party_Vote_Long=="." & BB_Country=="Slovakia" & !missing(BB_2nd_Party) & Y_No_2nd_Party_Vote==0
tab BB_Least_Party BB_Year if Y_Least_Party_Vote_Long=="." & BB_Country=="Slovakia" & !missing(BB_Least_Party) & Y_No_Least_Party_Vote==0
/***********************************
******* Slovenia *******************
***********************************/
foreach party in BB_Party BB_2nd_Party BB_Least_Party {
    if "`party'" == "BB_Party" {
        local i "$prefix_BB_Party"
    }
    else if "`party'" == "BB_2nd_Party" {
        local i "$prefix_BB_2nd_Party"
    }
    else if "`party'" == "BB_Least_Party" {
        local i "$prefix_BB_Least_Party"
    }
	
	/*Fixing a mistaken BB`i'Party in Wave 3 SVN: Positive Slovenia is a mistake and should be SI: DESUS - Democratic party of slovenian pensioners. See table in page 352 of WV3 Results v20180912 available at https://www.worldvaluessurvey.org/WVSDocumentationWV3.jsp - accessed June 5th 2022*/
	replace BB`i'Party = "SI: DESUS - Democratic party of slovenian pensioners" if BB`i'Party == "SVN: Positive Slovenia" & BB_Year == 1995

	replace Y`i'Party_Vote = "AssociatedSocialDemocrats_SVN" if BB`i'Party == "SI: Associated List of Social Democrats (ZLSD)" | BB`i'Party == "SVN: Social Democrats" & BB_Year <2004 /* Social Democratic party is the successor of ZLSD since 2004. See page 126 in manifesto Prties_MPDataset_MPDS2020b */ 
	replace Y`i'Party_Vote_Long = "SVN: Associated List of Social Democrats" if Y`i'Party_Vote == "AssociatedSocialDemocrats_SVN"

	replace Y`i'Party_Vote = "ChristianSocialists_SVN" if BB`i'Party == "SI: Christian Socialists (KS)" 
	replace Y`i'Party_Vote_Long = "SVN: Christian Socialists" if Y`i'Party_Vote == "ChristianSocialists_SVN"

	replace Y`i'Party_Vote = "PensionersDemocrat_SVN" if BB`i'Party == "SI: DESUS - Democratic party of slovenian pensioners" | BB`i'Party == "SI: Democratic Party of Pensioners of Slovenia (DeSUS)" | BB`i'Party == "SVN: Democratic Party of Pensioners of Slovenia"
	replace Y`i'Party_Vote_Long = "SVN: Democratic Party of Pensioners of Slovenia" if Y`i'Party_Vote == "PensionersDemocrat_SVN"

	replace Y`i'Party_Vote = "Democratic_SVN" if BB`i'Party == "SI: Democratic Party (DS)" | BB`i'Party == "SVN: Democratic Party of Slovenia"
	replace Y`i'Party_Vote_Long = "SVN: Democratic Party of Slovenia" if Y`i'Party_Vote == "Democratic_SVN"

	replace Y`i'Party_Vote = "Greens_SVN" if BB`i'Party == "SI: Green Party (ZS)" | BB`i'Party == "SVN: Green Party"
	replace Y`i'Party_Vote_Long = "SVN: Greens of Slovenia" if Y`i'Party_Vote == "Greens_SVN"

	replace Y`i'Party_Vote = "LiberalDemocracy_SVN" if BB`i'Party == "SI: Liberal Democracy of Slovenia (LDS)" | BB`i'Party == "SVN: Liberal Democracy of Slovenia"
	replace Y`i'Party_Vote_Long = "SVN: Liberal Democracy of Slovenia" if Y`i'Party_Vote == "LiberalDemocracy_SVN"

	replace Y`i'Party_Vote = "NewSlovenianChristian_SVN" if BB`i'Party == "SI: NSi - New Slovenia - Christian people's party" | BB`i'Party == "SI: New Slovenia-Christian (NSI)" | BB`i'Party == "SVN: New Slovenia – Christian Democrats"
	replace Y`i'Party_Vote_Long = "SVN: New Slovenian Christian People’s Party" if Y`i'Party_Vote == "NewSlovenianChristian_SVN"

	replace Y`i'Party_Vote = "Positive_SVN" if BB`i'Party == "SI: PS - Positive Slovenia" | BB`i'Party == "SVN: Positive Slovenia" 
	replace Y`i'Party_Vote_Long = "SVN: Positive Slovenia" if Y`i'Party_Vote == "Positive_SVN"

	replace Y`i'Party_Vote = "SocialDemocrats_SVN" if BB`i'Party == "SI: SD - Social democrats" | BB`i'Party == "SI: Socialni demokrati" | BB`i'Party == "SVN: Social Democrats" & BB_Year>=2004
	replace Y`i'Party_Vote_Long = "SVN: Social Democratic Party" if Y`i'Party_Vote == "SocialDemocrats_SVN"

	replace Y`i'Party_Vote = "SlovenianDemocratic_SVN" if BB`i'Party == "SI: SDS - Slovenian democratic party" | BB`i'Party == "SVN: Slovenian Democratic Party" 
	replace Y`i'Party_Vote_Long = "SVN: Slovenian Democratic Party" if Y`i'Party_Vote == "SlovenianDemocratic_SVN"

	replace Y`i'Party_Vote = "SocialDemocraticSlovenia_SVN" if BB`i'Party == "SI: Social Democratic Party of Slovenia (SDSS)" |  BB`i'Party == "SI: Social Democratic Party (SDS)" 
	replace Y`i'Party_Vote_Long = "SVN: Social-Democratic Party of Slovenia" if Y`i'Party_Vote == "SocialDemocraticSlovenia_SVN"

	replace Y`i'Party_Vote = "SlovenianPeople_SVN" if BB`i'Party == "SI: SLS - Slovene people's party" | BB`i'Party == "SI: Slovene Peoples Party (SLS)" | BB`i'Party == "SVN: Slovenian People's Party"
	replace Y`i'Party_Vote_Long = "SVN: Slovenian People’s Party" if Y`i'Party_Vote == "SlovenianPeople_SVN"

	replace Y`i'Party_Vote = "SlovenianNational_SVN" if BB`i'Party == "SI: SNS - Slovenian national party" | BB`i'Party == "SI: Slovene National Party (SNS)" | BB`i'Party == "SVN: Slovenian National Party" | BB`i'Party == "SVN: Slovenian National Right"
	replace Y`i'Party_Vote_Long = "SVN: Slovenian National Party" if Y`i'Party_Vote == "SlovenianNational_SVN"

	replace Y`i'Party_Vote = "ChristianDemocrats_SVN" if BB`i'Party == "SI: Slovene Christian Democrats (SKD)" | BB`i'Party == "SVN: Slovene Christian Democrats"
	replace Y`i'Party_Vote_Long = "SVN: Slovenian Christian Democrats" if Y`i'Party_Vote == "ChristianDemocrats_SVN"

	replace Y`i'Party_Vote = "NewPolitics_SVN" if BB`i'Party == "SI: Stranka nove politike" | BB`i'Party == "SVN: ZARES"
	replace Y`i'Party_Vote_Long = "SVN: Zares - New Politics" if Y`i'Party_Vote == "NewPolitics_SVN"

	replace Y`i'Party_Vote = "UnitedLeft_SVN" if BB`i'Party == "SI: ZL - United Left"
	replace Y`i'Party_Vote_Long = "SVN: United Left" if Y`i'Party_Vote == "UnitedLeft_SVN"

	replace Y`i'Party_Vote = "Socialist_SVN" if BB`i'Party == "SI: Socialist Party of Slovenia (SSS)"
	replace Y`i'Party_Vote_Long = "SVN: Socialist Party of Slovenia" if Y`i'Party_Vote == "Socialist_SVN"

	replace Y`i'Party_Vote = "SocialDemocraticRenewal_SVN" if BB`i'Party == "SI: Social-Democratic Renewal (SDP)"
	replace Y`i'Party_Vote_Long = "SVN: Social Democratic Renewal" if Y`i'Party_Vote == "SocialDemocraticRenewal_SVN"

	replace Y`i'Party_Vote = "SocialDemocraticUnion_SVN" if BB`i'Party == "SI: Social Democratic Union (SDU)"
	replace Y`i'Party_Vote_Long = "SVN: Social Democratic Union" if Y`i'Party_Vote == "SocialDemocraticUnion_SVN"

	replace Y`i'Party_Vote = "ModernCentre_SVN" if BB`i'Party == "SI: SMC - Party of Modern Center"
	replace Y`i'Party_Vote_Long = "SVN: Modern Centre Party" if Y`i'Party_Vote == "ModernCentre_SVN"

	replace Y`i'Party_Vote = "NationalDemocratic_SVN" if BB`i'Party == "SI: National Democratic Party (SDZ-NDS)" 
	replace Y`i'Party_Vote_Long = "SVN: National Democratic Party" if Y`i'Party_Vote == "NationalDemocratic_SVN"

	replace Y`i'Party_Vote = "Liberal_SVN" if BB`i'Party == "SI: Liberal Party (LS)" 
	replace Y`i'Party_Vote_Long = "SVN: Slovenian Liberal Party" if Y`i'Party_Vote == "Liberal_SVN"

	replace Y`i'Party_Vote = "LIPA_SVN" if BB`i'Party == "SI: LIPA" 
	replace Y`i'Party_Vote_Long = "SVN: LIPA Party" if Y`i'Party_Vote == "LIPA_SVN"

	replace Y`i'Party_Vote = "AlenkaBratušek_SVN" if BB`i'Party == "SI: ZAAB - Alliance of Alenka Bratusek"
	replace Y`i'Party_Vote_Long = "SVN: Party of Alenka Bratušek" if Y`i'Party_Vote == "AlenkaBratušek_SVN"

	tab BB`i'Party if BB_Country=="Slovenia" & Y`i'Party_Vote=="." & Y_No`i'Party_Vote !=1

	/* Testing Slovenia*/
	tab Y`i'Party_Vote_Long if BB_Country=="Slovenia" & !missing(BB`i'Party) & Y_No`i'Party_Vote==0, missing
	tab BB`i'Party if BB_Country=="Slovenia" & Y_No`i'Party_Vote==0 & Y`i'Party_Vote_Long=="."  

	count if Y`i'Party_Vote_Long=="." & BB_Country=="Slovenia" & !missing(BB`i'Party) & Y_No`i'Party_Vote==0
	assert r(N)==0
}

tab BB_Party BB_Year if Y_Party_Vote_Long=="." & BB_Country=="Slovenia" & !missing(BB_Party) & Y_No_Party_Vote==0
tab BB_2nd_Party BB_Year if Y_2nd_Party_Vote_Long=="." & BB_Country=="Slovenia" & !missing(BB_2nd_Party) & Y_No_2nd_Party_Vote==0
tab BB_Least_Party BB_Year if Y_Least_Party_Vote_Long=="." & BB_Country=="Slovenia" & !missing(BB_Least_Party) & Y_No_Least_Party_Vote==0
/**********************************
******* Romania *******************
**********************************/
foreach party in BB_Party BB_2nd_Party BB_Least_Party {
    if "`party'" == "BB_Party" {
        local i "$prefix_BB_Party"
    }
    else if "`party'" == "BB_2nd_Party" {
        local i "$prefix_BB_2nd_Party"
    }
    else if "`party'" == "BB_Least_Party" {
        local i "$prefix_BB_Least_Party"
    }
	
	replace Y`i'Party_Vote = "LiberalsDemocrats_ROU" if BB`i'Party == "RO: 'Aliance of Liberals and Democrats' Party" | BB`i'Party == "ROU: Alliance of Liberals and Democrats" 
	replace Y`i'Party_Vote_Long = "ROU: Alliance of Liberals and Democrats" if Y`i'Party_Vote == "LiberalsDemocrats_ROU"

	replace Y`i'Party_Vote = "DemocratLiberal_ROU" if BB`i'Party == "RO: Partidul Democrat-Liberal" | BB`i'Party == "ROU: Democratic Liberal Party"
	replace Y`i'Party_Vote_Long = "ROU: Democratic Liberal Party" if Y`i'Party_Vote == "DemocratLiberal_ROU"

	replace Y`i'Party_Vote = "SaveRomania_ROU" if BB`i'Party == "RO: 'Save Romania' Union" | BB`i'Party == "ROU: Union Save Romania"
	replace Y`i'Party_Vote_Long = "ROU: Save Romania Union" if Y`i'Party_Vote == "SaveRomania_ROU"

	replace Y`i'Party_Vote = "DemocraticConvention_ROU" if BB`i'Party == "RO: Center-right Coalition (PNL, PNL - PNTCD - AC - Ecologists) - (CDR)" | BB`i'Party == "RO: Conventia Democratica din Romania (CDR) as a whole" | BB`i'Party == "ROU: Conventia Democratica din Romania as a whole"
	replace Y`i'Party_Vote_Long = "ROU: Democratic Convention of Romania (CDR)" if Y`i'Party_Vote == "DemocraticConvention_ROU"

	replace Y`i'Party_Vote = "DemocraticHungarians_ROU" if BB`i'Party == "RO: Democratic Union of Hungarians from Romania" | BB`i'Party == "RO: Uniunea Democrata a Maghiarilor din Romania (UDMR) - Hungarians Union" | BB`i'Party == "ROU: Democratic Alliance of Hungarians in Romania"
	replace Y`i'Party_Vote_Long = "ROU: Democratic Union of Hungarians in Romania" if Y`i'Party_Vote == "DemocraticHungarians_ROU"

	replace Y`i'Party_Vote = "Ecologists_ROU" if BB`i'Party == "RO: Ecologists" | BB`i'Party == "RO: Partidul Ecologist Roman (PER) - Romanian Ecologist Part"
	replace Y`i'Party_Vote_Long = "ROU: Romanian Ecological Party" if Y`i'Party_Vote == "Ecologists_ROU"

	replace Y`i'Party_Vote = "GreaterRomania_ROU" if BB`i'Party == "RO: Great Romania Party" | BB`i'Party == "RO: Partidul Romania Mare (PRM) - Greater Romania Party" | BB`i'Party == "ROU: Greater Romania Party"
	replace Y`i'Party_Vote_Long = "ROU: Greater Romania Party" if Y`i'Party_Vote == "GreaterRomania_ROU"

	replace Y`i'Party_Vote = "ChristianDemocraticNational_ROU" if BB`i'Party == "RO: Partidul National Taranesc Crestin si Democrat (PNTCD) - National Peasant Party Christian Democrat" | BB`i'Party == "RO: Partidul Naţional Creştin Democrat" | BB`i'Party == "ROU: National Peasant Party Christian Democrat"
	replace Y`i'Party_Vote_Long = "ROU: Christian Democratic National Peasants' Party" if Y`i'Party_Vote == "ChristianDemocraticNational_ROU"

	replace Y`i'Party_Vote = "NationalLiberal_ROU" if BB`i'Party == "RO: National Liberal Party" | BB`i'Party == "RO: Partidul National Liberal (PNL) - National Liberal Party" | BB`i'Party == "ROU: National Liberal Party"
	replace Y`i'Party_Vote_Long = "ROU: National Liberal Party" if Y`i'Party_Vote == "NationalLiberal_ROU"

	replace Y`i'Party_Vote = "SocialDemocrat_ROU" if BB`i'Party == "RO: Partidul Social Democrat" | BB`i'Party == "RO: The Social-Democratic Party" | BB`i'Party == "ROU: Social Democratic Party" | BB`i'Party == "RO: Polul Democrat-Social din Romania (PDSR) - Democratic-Social Pole of Romania" | BB`i'Party == "ROU: Democratic-Social Pole of Romania" | BB`i'Party == "RO: Frontul Democrat al Salvarii Nationale (FDSN) Democratic National Salvation Front"  /* This three parties are the same party that change the name from Democratic National Salvation Front to Social Democratic Pole of Romania and then to Social Democratic Party in 2001 */
	replace Y`i'Party_Vote_Long = "ROU: Social Democratic Party" if Y`i'Party_Vote == "SocialDemocrat_ROU"

	replace Y`i'Party_Vote = "RomanianSocialDemocratic_ROU" if BB`i'Party == "RO: Partidul Social Democrat Roman (PSDR) - Social Democratic Party" 
	replace Y`i'Party_Vote_Long = "ROU: Romanian Social Democratic Party" if Y`i'Party_Vote == "RomanianSocialDemocratic_ROU"

	replace Y`i'Party_Vote = "RomanianSocialistDemocratic_ROU" if BB`i'Party == "RO: Partidul Socialist Democratic Roman (PSD) Romanian Socialist Democratic Party" 
	replace Y`i'Party_Vote_Long = "ROU: Romanian Socialist Democratic Party" if Y`i'Party_Vote == "RomanianSocialistDemocratic_ROU"

	replace Y`i'Party_Vote = "SocialistLabour_ROU" if BB`i'Party == "RO: Partidul Socialist al Muncii (PSM) Socialist Labor Party" | BB`i'Party == "ROU: Socialist Labor Party" | BB`i'Party == "RO: Socialist (PS/PSM)"
	replace Y`i'Party_Vote_Long = "ROU: Socialist Labour Party" if Y`i'Party_Vote == "SocialistLabour_ROU"

	replace Y`i'Party_Vote = "NationalUnity_ROU" if BB`i'Party == "RO: Partidul Unitatii Nationale Romane (PUNR) - Party of Romanian National Unity" | BB`i'Party == "ROU: Party of Romanian National Unity"
	replace Y`i'Party_Vote_Long = "ROU: Romanian National Unity Party" if Y`i'Party_Vote == "NationalUnity_ROU"

	replace Y`i'Party_Vote = "PeopleMovement_ROU" if BB`i'Party == "RO: Popular Movement Party" | BB`i'Party == "ROU: People´s Movement Party"
	replace Y`i'Party_Vote_Long = "ROU: People's Movement Party" if Y`i'Party_Vote == "PeopleMovement_ROU"

	replace Y`i'Party_Vote = "PeopleParty_ROU" if BB`i'Party == "ROU: People's Party – Dan Diaconescu" 
	replace Y`i'Party_Vote_Long = "ROU: People's Party - Dan Dianconescu" if Y`i'Party_Vote == "PeopleParty_ROU"

	replace Y`i'Party_Vote = "CivicAlliance_ROU" if BB`i'Party == "RO: The Civic Alliance (AC) (intelectuals)" 
	replace Y`i'Party_Vote_Long = "ROU: Party of the Civic Alliance" if Y`i'Party_Vote == "CivicAlliance_ROU"

	replace Y`i'Party_Vote = "CivicForce_ROU" if BB`i'Party == "ROU: Civic Force" 
	replace Y`i'Party_Vote_Long = "ROU: Civic Force" if Y`i'Party_Vote == "CivicForce_ROU"

	replace Y`i'Party_Vote = "Conservative_ROU" if BB`i'Party == "RO: The Conservative Party PC" | BB`i'Party == "ROU: The Conservative Party"
	replace Y`i'Party_Vote_Long = "ROU: Conservative Party" if Y`i'Party_Vote == "Conservative_ROU"

	replace Y`i'Party_Vote = "NationalLiberalCampeanu_ROU" if BB`i'Party == "ROU: National Liberal Party Campeanu"
	replace Y`i'Party_Vote_Long = "ROU: National Liberal Party Campeanu" if Y`i'Party_Vote == "NationalLiberalCampeanu_ROU"

	replace Y`i'Party_Vote = "NationalDemocratic_ROU" if BB`i'Party == "ROU: National Democratic Party"
	replace Y`i'Party_Vote_Long = "ROU: National Democratic Party" if Y`i'Party_Vote == "NationalDemocratic_ROU"

	replace Y`i'Party_Vote = "Liberal_ROU" if BB`i'Party == "ROU: Liberal Party ´93"
	replace Y`i'Party_Vote_Long = "ROU: Liberal Party" if Y`i'Party_Vote == "Liberal_ROU"

	replace Y`i'Party_Vote = "RomanianNational_ROU" if BB`i'Party == "ROU: National Romanian Party"
	replace Y`i'Party_Vote_Long = "ROU: Romanian National Party" if Y`i'Party_Vote == "RomanianNational_ROU"

	replace Y`i'Party_Vote = "NationalProgress_ROU" if BB`i'Party == "ROU: National Union for the Progress of Romania"
	replace Y`i'Party_Vote_Long = "ROU: National Union for the Progress of Romania" if Y`i'Party_Vote == "NationalProgress_ROU"

	replace Y`i'Party_Vote = "RightAlliance_ROU" if BB`i'Party == "ROU: Right Romania Alliance"
	replace Y`i'Party_Vote_Long = "ROU: Right Romania Alliance" if Y`i'Party_Vote == "RightAlliance_ROU"

	replace Y`i'Party_Vote = "SocialLiberalCoallition_ROU" if BB`i'Party == "ROU: Social Liberal Coallition = PSD+PNL+PC"
	replace Y`i'Party_Vote_Long = "ROU: Social Liberal Coallition" if Y`i'Party_Vote == "SocialLiberalCoallition_ROU"

	replace Y`i'Party_Vote = "JusticeTruth_ROU" if BB`i'Party == "ROU: The Justice and Truth Alliance"
	replace Y`i'Party_Vote_Long = "ROU: Justice and Truth Alliance" if Y`i'Party_Vote == "JusticeTruth_ROU"

	replace Y`i'Party_Vote = "AnyParty_ROU" if BB`i'Party == "RO: Any party"
	replace Y`i'Party_Vote_Long = "ROU: Any Party" if Y`i'Party_Vote == "AnyParty_ROU"

	replace Y`i'Party_Vote = "MovementRomania_ROU" if BB`i'Party == "RO: Movement for Romania (MPR)"
	replace Y`i'Party_Vote_Long = "ROU: Movement For Romania" if Y`i'Party_Vote == "MovementRomania_ROU"

	replace Y`i'Party_Vote = "NationalLiberalYoung_ROU" if BB`i'Party == "RO: National Liberal Party-The Young Wing (PNL-AT)"
	replace Y`i'Party_Vote_Long = "ROU: National Liberal Party-The Young Wing" if Y`i'Party_Vote == "NationalLiberalYoung_ROU"

	replace Y`i'Party_Vote = "Rroma_ROU" if BB`i'Party == "RO: Partida Romilor (PR) - Rroma Party"
	replace Y`i'Party_Vote_Long = "ROU: Rroma Party" if Y`i'Party_Vote == "Rroma_ROU"

	replace Y`i'Party_Vote = "FreeChange_ROU" if BB`i'Party == "RO: Partidul Liber Schimbist (PLS) - Party of Free Change"
	replace Y`i'Party_Vote_Long = "ROU: Party of Free Change" if Y`i'Party_Vote == "FreeChange_ROU"

	replace Y`i'Party_Vote = "RadicalLiberalism_ROU" if BB`i'Party == "RO: Radical Liberalism (UFD)"
	replace Y`i'Party_Vote_Long = "ROU: Radical Liberalism" if Y`i'Party_Vote == "RadicalLiberalism_ROU"

	replace Y`i'Party_Vote = "NationalInitiative_ROU" if BB`i'Party == "RO: Partidul Iniţiativa Naţională" 
	replace Y`i'Party_Vote_Long = "ROU: National Initiative Party" if Y`i'Party_Vote == "NationalInitiative_ROU"

	replace Y`i'Party_Vote = "DemocraticAgrarian_ROU" if BB`i'Party == "RO: Partidul Democrat Agrar din Romania (PDAR) - Agrarian Party" 
	replace Y`i'Party_Vote_Long = "ROU: Democratic Agrarian Party of Romania" if Y`i'Party_Vote == "DemocraticAgrarian_ROU"

	replace Y`i'Party_Vote = "Republican_ROU" if BB`i'Party == "RO: Partidul Republican (PR) Republican Party" 
	replace Y`i'Party_Vote_Long = "ROU: Republican Party" if Y`i'Party_Vote == "Republican_ROU"

	replace Y`i'Party_Vote = "NewGenerationChristianDemocratic_ROU" if BB`i'Party == "RO: Partidul Noua Generaţie-Creştin Democrat" 
	replace Y`i'Party_Vote_Long = "ROU: New Generation Party- Christian Democratic" if Y`i'Party_Vote == "NewGenerationChristianDemocratic_ROU"

	replace Y`i'Party_Vote = "Democratic_ROU" if BB`i'Party == "RO: Partidul Democrat (PD) - Democratic Party" | BB`i'Party == "ROU: Democratic Party" | BB`i'Party == "RO: Frontul Salvarii Nationale (FSN) - National Salvation Front" /* This two prties are the same party that change the name from National Salvation Front to Democratic party. */
	replace Y`i'Party_Vote_Long = "ROU: Democratic Party" if Y`i'Party_Vote == "Democratic_ROU"

	replace Y`i'Party_Vote = "EcologicalMovement_ROU" if BB`i'Party == "RO: Miscarea Ecologista din Romania (MER) - Romanian Ecologic Movement"
	replace Y`i'Party_Vote_Long = "ROU: Romanian Ecological Movement" if Y`i'Party_Vote == "EcologicalMovement_ROU"

	replace Y`i'Party_Vote = "Germans_ROU" if BB`i'Party == "RO: Germans Forum (FDG)"
	replace Y`i'Party_Vote_Long = "ROU: Germans Forum" if Y`i'Party_Vote == "Germans_ROU"

	replace Y`i'Party_Vote = "AllianceRomania_ROU" if BB`i'Party == "RO: Alianta pentru Romania (ApR) - Alliance for Romania" 
	replace Y`i'Party_Vote_Long = "ROU: Alliance for Romania" if Y`i'Party_Vote == "AllianceRomania_ROU"

	tab BB`i'Party if BB_Country=="Romania" & Y`i'Party_Vote=="." & Y_No`i'Party_Vote !=1

	/* Testing Romania*/
	tab Y`i'Party_Vote_Long if BB_Country=="Romania" & !missing(BB`i'Party) & Y_No`i'Party_Vote==0, missing
	tab BB`i'Party if BB_Country=="Romania" & Y_No`i'Party_Vote==0 & Y`i'Party_Vote_Long=="."  

	count if Y`i'Party_Vote_Long=="." & BB_Country=="Romania" & !missing(BB`i'Party) & Y_No`i'Party_Vote==0
	assert r(N)==0
}

tab BB_Party BB_Year if Y_Party_Vote_Long=="." & BB_Country=="Romania" & !missing(BB_Party) & Y_No_Party_Vote==0
tab BB_2nd_Party BB_Year if Y_2nd_Party_Vote_Long=="." & BB_Country=="Romania" & !missing(BB_2nd_Party) & Y_No_2nd_Party_Vote==0
tab BB_Least_Party BB_Year if Y_Least_Party_Vote_Long=="." & BB_Country=="Romania" & !missing(BB_Least_Party) & Y_No_Least_Party_Vote==0
/************************************
******* Lithuania *******************
************************************/
foreach party in BB_Party BB_2nd_Party BB_Least_Party {
    if "`party'" == "BB_Party" {
        local i "$prefix_BB_Party"
    }
    else if "`party'" == "BB_2nd_Party" {
        local i "$prefix_BB_2nd_Party"
    }
    else if "`party'" == "BB_Least_Party" {
        local i "$prefix_BB_Least_Party"
    }
	
	replace Y`i'Party_Vote = "HomelandUnionChristianDemocrat_LTU" if BB`i'Party == "LT: Homeland Union: Lithuanian Christian Democrats" | BB`i'Party == "LT: Tevynes Sajunga (Lietuvos konservatoriai) (TS-LK) - Homeland Union/ Lithuanian Conservatives" | BB`i'Party == "LTU: Homeland Union – Lithuanian Christian Democrats"
	replace Y`i'Party_Vote_Long = "LTU: Homeland Union - Lithuanian Christian Democrats" if Y`i'Party_Vote == "HomelandUnionChristianDemocrat_LTU"

	replace Y`i'Party_Vote = "ElectionActionPoles_LTU" if BB`i'Party == "LT: Electoral Action of Poles – Christian Families Aliance" | BB`i'Party == "LT: Lietuvos Lenku Rinkimu Akcija (LLRA) - Polish Election Action" | BB`i'Party == "LTU: Lithuanian Poles´ Electoral Action - Christian Family"
	replace Y`i'Party_Vote_Long = "LTU: Election Action of Lithuania's Poles" if Y`i'Party_Vote == "ElectionActionPoles_LTU"

	replace Y`i'Party_Vote = "DemocraticLabour_LTU" if BB`i'Party == "LT: Lietuvos demokratine darbo partija (LDDP) - Lithuanian Democratic Labour Party" | BB`i'Party == "LTU: Lithuanian Democratic Labour"
	replace Y`i'Party_Vote_Long = "LTU: Lithuanian Democratic Labour Party" if Y`i'Party_Vote == "DemocraticLabour_LTU"

	replace Y`i'Party_Vote = "LiberalCentreUnion_LTU" if BB`i'Party == "LT: Liberal and Center Union"
	replace Y`i'Party_Vote_Long = "LTU: Liberal and Centre Union" if Y`i'Party_Vote == "LiberalCentreUnion_LTU"

	replace Y`i'Party_Vote = "Labour_LTU" if BB`i'Party == "LT: Labour Party"
	replace Y`i'Party_Vote_Long = "LTU: Labour Party" if Y`i'Party_Vote == "Labour_LTU"

	replace Y`i'Party_Vote = "Young_LTU" if BB`i'Party == "LT: Lietuviu nacionaline partija Jaunoji Lietuva (LNPJL) - Lithuanian National Party ´Young Lithuania´" | BB`i'Party == "LTU: Lithuanian National Party ´Young Lithuania´"
	replace Y`i'Party_Vote_Long = "LTU: Young Lithuania" if Y`i'Party_Vote == "Young_LTU"

	replace Y`i'Party_Vote = "ChristianDemocratic_LTU" if BB`i'Party == "LT: Lietuvos krikscioniu demokratu partija (LKDP) - Lithuanian Christian Democratic Party" | BB`i'Party == "LTU: Lithuanian Christian Democratic Party"
	replace Y`i'Party_Vote_Long = "LTU: Lithuanian Christian Democratic Party" if Y`i'Party_Vote == "ChristianDemocratic_LTU"

	replace Y`i'Party_Vote = "CentreUnion_LTU" if BB`i'Party == "LT: Lietuvos centro sajunga (LCS) - Lithuanian Center Union" | BB`i'Party == "LTU: Center Union of Lithuania"
	replace Y`i'Party_Vote_Long = "LTU: Lithuanian Centre Union" if Y`i'Party_Vote == "CentreUnion_LTU"

	replace Y`i'Party_Vote = "LiberalUnion_LTU" if BB`i'Party == "LT: Lietuvos liberalu sajunga (LLS) - Lithuanian Liberal Union" | BB`i'Party == "LTU: Lithuanian Liberal Union" | BB`i'Party == "LT: Lithuanian Freedom Union (Liberals)"
	replace Y`i'Party_Vote_Long = "LTU: Lithuanian Liberal Union" if Y`i'Party_Vote == "LiberalUnion_LTU"

	replace Y`i'Party_Vote = "LiberalRepublic_LTU" if BB`i'Party == "LT: Liberal Union of Lithuanian Republic"
	replace Y`i'Party_Vote_Long = "LTU: Liberals Movement of the Republic of Lithuania" if Y`i'Party_Vote == "LiberalRepublic_LTU"

	replace Y`i'Party_Vote = "ChristianDemocraticUnion_LTU" if BB`i'Party == "LT: Lietuvos krikscioniu demokratu sajunga (KDS) - Christian Democratic Union"
	replace Y`i'Party_Vote_Long = "LTU: Christian Democratic Union" if Y`i'Party_Vote == "ChristianDemocraticUnion_LTU"

	replace Y`i'Party_Vote = "SocialDemocratic_LTU" if BB`i'Party == "LT: Lietuvos socialdemokratu partija (LSDP) - Lithianian Socialdemocratic Party" | BB`i'Party == "LT: Lithuanian Social Democratic Party" | BB`i'Party == "LTU: Social Democratic Party of Lithuania"
	replace Y`i'Party_Vote_Long = "LTU: Lithuanian Social Democratic Party" if Y`i'Party_Vote == "SocialDemocratic_LTU"

	replace Y`i'Party_Vote = "PeasantGreen_LTU" if BB`i'Party == "LT: Lithuanian Farmers and Greens Union" | BB`i'Party == "LT: Lithuanian Peasants Populist Union"
	replace Y`i'Party_Vote_Long = "LTU: Lithuanian Peasant and Green Union" if Y`i'Party_Vote == "PeasantGreen_LTU"

	replace Y`i'Party_Vote = "Peasants_LTU" if BB`i'Party == "LT: Lietuvos valstieciu partija (LVP) - Lithuanian Peasants Party" | BB`i'Party == "LTU: Lithuanian Peasant Party"
	replace Y`i'Party_Vote_Long = "LTU: Lithuanian Peasants Party" if Y`i'Party_Vote == "Peasants_LTU"

	replace Y`i'Party_Vote = "Green_LTU" if BB`i'Party == "LT: Lithuanian Green Party"
	replace Y`i'Party_Vote_Long = "LTU: Lithuanian Green Party" if Y`i'Party_Vote == "Green_LTU"

	replace Y`i'Party_Vote = "OrderJustice_LTU" if BB`i'Party == "LT: Party 'Order and Justice'"
	replace Y`i'Party_Vote_Long = "LTU: Order and Justice" if Y`i'Party_Vote == "OrderJustice_LTU"

	replace Y`i'Party_Vote = "SocialLiberals_LTU" if BB`i'Party == "LT: New Union (socialliberals)"
	replace Y`i'Party_Vote_Long = "LTU: New Union (Social Liberals)" if Y`i'Party_Vote == "SocialLiberals_LTU"

	replace Y`i'Party_Vote = "Centre_LTU" if BB`i'Party == "LT: Lithuanian Centre Party"
	replace Y`i'Party_Vote_Long = "LTU: Lithuanian Centre Party" if Y`i'Party_Vote == "Centre_LTU"

	replace Y`i'Party_Vote = "ForJust_LTU" if BB`i'Party == "LT: Lithuanian People's Union 'For Just Lithuania'"
	replace Y`i'Party_Vote_Long = "LTU: People's Union 'For Just Lithuania'" if Y`i'Party_Vote == "ForJust_LTU"

	replace Y`i'Party_Vote = "List_LTU" if BB`i'Party == "LT: Political Party 'List of Lithuania'"
	replace Y`i'Party_Vote_Long = "LTU: Political Party 'List of Lithuania' (LS)" if Y`i'Party_Vote == "List_LTU"

	replace Y`i'Party_Vote = "NationalResurrection_LTU" if BB`i'Party == "LT: Revival of Nation Party" 
	replace Y`i'Party_Vote_Long = "LTU: National Resurrection Party" if Y`i'Party_Vote == "NationalResurrection_LTU"

	replace Y`i'Party_Vote = "NationalMinorities_LTU" if BB`i'Party == "LTU: Alliance of the Lithuanian National Minorities" 
	replace Y`i'Party_Vote_Long = "LTU: Alliance of the Lithuanian National Minorities" if Y`i'Party_Vote == "NationalMinorities_LTU"

	replace Y`i'Party_Vote = "Democratic_LTU" if BB`i'Party == "LT: Democratic Party" | BB`i'Party == "LT: National Democratic Party"
	replace Y`i'Party_Vote_Long = "LTU: Lithuanian Democratic Party" if Y`i'Party_Vote == "Democratic_LTU"

	replace Y`i'Party_Vote = "LibertyLeague_LTU" if BB`i'Party == "LT: Lietuvos Laisves Lyga (LLL) - Lithuanian Liberty League" | BB`i'Party == "LTU: Lithuanian Liberty League"
	replace Y`i'Party_Vote_Long = "LTU: Lithuanian Liberty League" if Y`i'Party_Vote == "LibertyLeague_LTU"

	replace Y`i'Party_Vote = "NationalUnion_LTU" if BB`i'Party == "LT: Lietuvos Tautininku Sajunga (LTS) - Lithuanian National Union" | BB`i'Party == "LTU: Lithuanian National Union"
	replace Y`i'Party_Vote_Long = "LTU: Lithuanian National Union" if Y`i'Party_Vote == "NationalUnion_LTU"

	replace Y`i'Party_Vote = "LogicOfLife_LTU" if BB`i'Party == "LT: Lietuvos gyvenimo logikos partija (LGLP) - Lithuanian Logic of Life Party" | BB`i'Party == "LTU: Lithuanian Logic of Life Party"
	replace Y`i'Party_Vote_Long = "LTU: Lithuanian Logic of Life Party" if Y`i'Party_Vote == "LogicOfLife_LTU"

	replace Y`i'Party_Vote = "LibertyUnion_LTU" if BB`i'Party == "LT: Lietuvos laisves sajunga - Lithuanian Liberty Union"
	replace Y`i'Party_Vote_Long = "LTU: Lithuanian Liberty Union" if Y`i'Party_Vote == "LibertyUnion_LTU"

	replace Y`i'Party_Vote = "PeopleParty_LTU" if BB`i'Party == "LT: Lietuvos liaudies partija (LLP) - Lithuanian People Party" | BB`i'Party == "LTU: Lithuanian People´s Party"
	replace Y`i'Party_Vote_Long = "LTU: Lithuanian People Party" if Y`i'Party_Vote == "PeopleParty_LTU"

	replace Y`i'Party_Vote = "PoliticalPrisoners_LTU" if BB`i'Party == "LT: Lietuvos politiniu kaliniu ir tremtiniu sajunga (LPKTS) -Lithuanian Political Prisoners and Deported People Union" | BB`i'Party == "LTU: Lithuanian Political Prisoners and Deported People Unio"
	replace Y`i'Party_Vote_Long = "LTU: Lithuanian Union of Political Prisoners and Deportees" if Y`i'Party_Vote == "PoliticalPrisoners_LTU"

	replace Y`i'Party_Vote = "RussianUnion_LTU" if BB`i'Party == "LT: Lietuvos rusu sajunga (LRS) - Lithuanian Russians Union" | BB`i'Party == "LTU: Lithuanian Russian Union"
	replace Y`i'Party_Vote_Long = "LTU: Lithuanian Russian Union" if Y`i'Party_Vote == "RussianUnion_LTU"

	replace Y`i'Party_Vote = "Socialist_LTU" if BB`i'Party == "LT: Lietuvos socialistu partija (LSP) - Lithuanian Socialist Party" | BB`i'Party == "LTU: Lithuanian Socialist Party"
	replace Y`i'Party_Vote_Long = "LTU: Socialist Party of Lithuania" if Y`i'Party_Vote == "Socialist_LTU"

	replace Y`i'Party_Vote = "Justice_LTU" if BB`i'Party == "LT: Lithuanian Justice Party" | BB`i'Party == "LTU: Lithuanian Social Justice Union"
	replace Y`i'Party_Vote_Long = "LTU: Lithuanian Justice Party" if Y`i'Party_Vote == "Justice_LTU"

	replace Y`i'Party_Vote = "Independence_LTU" if BB`i'Party == "LT: Nepriklausomybes partija - Independence Party"
	replace Y`i'Party_Vote_Long = "LTU: Independence Party" if Y`i'Party_Vote == "Independence_LTU"

	replace Y`i'Party_Vote = "MovementElections_LTU" if BB`i'Party == "LT: Movement Elections 96"
	replace Y`i'Party_Vote_Long = "LTU: Movement Elections 96" if Y`i'Party_Vote == "MovementElections_LTU"

	replace Y`i'Party_Vote = "Industrial_LTU" if BB`i'Party == "LT: Lithuanian Industrial Party"
	replace Y`i'Party_Vote_Long = "LTU: Lithuanian Industrial Party" if Y`i'Party_Vote == "Industrial_LTU"

	replace Y`i'Party_Vote = "Economy_LTU" if BB`i'Party == "LTU: Lithuanian Party of Economy"
	replace Y`i'Party_Vote_Long = "LTU: Lithuanian Party of Economy" if Y`i'Party_Vote == "Economy_LTU"

	replace Y`i'Party_Vote = "NationalProgress_LTU" if BB`i'Party == "LTU: Party of National Progress"
	replace Y`i'Party_Vote_Long = "LTU: Party of National Progress" if Y`i'Party_Vote == "NationalProgress_LTU"

	replace Y`i'Party_Vote = "CitizensAlliance_LTU" if BB`i'Party == "LT: Alliance of Lithuanian Citizens"
	replace Y`i'Party_Vote_Long = "LTU: Alliance of Lithuanian Citizens" if Y`i'Party_Vote == "CitizensAlliance_LTU"

	replace Y`i'Party_Vote = "CivicDemocratic_LTU" if BB`i'Party == "LT: Citizen's Democracy Party"
	replace Y`i'Party_Vote_Long = "LTU: Civic Democratic Party" if Y`i'Party_Vote == "CivicDemocratic_LTU"

	replace Y`i'Party_Vote = "Republican_LTU" if BB`i'Party == "LTU: Republican Party"
	replace Y`i'Party_Vote_Long = "LTU: Republican Party" if Y`i'Party_Vote == "Republican_LTU"

	replace Y`i'Party_Vote = "WomensNewDemocratic_LTU" if BB`i'Party == "LT: Lietuvos moteru partija (LMP) - Lithuanian Women´s Party /New Democracy" | BB`i'Party == "LTU: Lithuanian Women's Party -- New Democratic Party" // There is a problem with the last party name, solved later. 
	replace Y`i'Party_Vote_Long = "LTU: Lithuanian Womens Party - New Democratic Party" if Y`i'Party_Vote == "WomensNewDemocratic_LTU"

	/* This part solving the problem with "LTU: Lithuanian Women's Party -- New Democratic Party" */
	replace Y`i'Party_Vote = "WomensNewDemocratic_LTU" if BB_Country == "Lithuania" & Y_No`i'Party_Vote !=1  & Y`i'Party_Vote_Long =="."
	replace Y`i'Party_Vote_Long = "LTU: Lithuanian Womens Party - New Democratic Party" if Y`i'Party_Vote == "WomensNewDemocratic_LTU"

	tab BB`i'Party if BB_Country == "Lithuania" & Y`i'Party_Vote=="." & Y_No`i'Party_Vote !=1

	/* Testing Lithuania*/
	tab Y`i'Party_Vote_Long if BB_Country == "Lithuania" & !missing(BB`i'Party) & Y_No`i'Party_Vote==0, missing
	tab BB`i'Party if BB_Country == "Lithuania" & Y_No`i'Party_Vote==0 & Y`i'Party_Vote_Long=="."  

	count if Y`i'Party_Vote_Long=="." & BB_Country == "Lithuania" & !missing(BB`i'Party) & Y_No`i'Party_Vote==0
	assert r(N)==0
}

tab BB_Party BB_Year if Y_Party_Vote_Long=="." & BB_Country=="Lithuania" & !missing(BB_Party) & Y_No_Party_Vote==0
tab BB_2nd_Party BB_Year if Y_2nd_Party_Vote_Long=="." & BB_Country=="Lithuania" & !missing(BB_2nd_Party) & Y_No_2nd_Party_Vote==0
tab BB_Least_Party BB_Year if Y_Least_Party_Vote_Long=="." & BB_Country=="Lithuania" & !missing(BB_Least_Party) & Y_No_Least_Party_Vote==0
/***************************************
********** United State ****************
***************************************/
foreach party in BB_Party BB_2nd_Party BB_Least_Party {
    if "`party'" == "BB_Party" {
        local i "$prefix_BB_Party"
    }
    else if "`party'" == "BB_2nd_Party" {
        local i "$prefix_BB_2nd_Party"
    }
    else if "`party'" == "BB_Least_Party" {
        local i "$prefix_BB_Least_Party"
    }
	
		replace Y`i'Party_Vote = "Democrat_USA" if BB`i'Party == "US: Democrat" | BB`i'Party == "USA: Democratic Party"
		replace Y`i'Party_Vote_Long = "USA: Democratic Party" if Y`i'Party_Vote == "Democrat_USA"

		replace Y`i'Party_Vote = "Republican_USA" if BB`i'Party == "US: Republican" | BB`i'Party == "USA: Republican Party"
		replace Y`i'Party_Vote_Long = "USA: Republican Party" if Y`i'Party_Vote == "Republican_USA"

		replace Y`i'Party_Vote = "Independent_USA" if (BB`i'Party == "Independent candidate" & BB_Country == "US") | BB`i'Party == "US: Independent"| BB`i'Party == "USA: Independent"
		replace Y`i'Party_Vote_Long = "USA: Independent" if Y`i'Party_Vote == "Independent_USA"

		replace Y`i'Party_Vote = "Libertarian_USA" if BB`i'Party == "US: Libertarian" | BB`i'Party == "USA: Libertarian"
		replace Y`i'Party_Vote_Long = "USA: Libertarian" if Y`i'Party_Vote == "Libertarian_USA"

		replace Y`i'Party_Vote = "Green_USA" if BB`i'Party == "USA: Green Party of the United States"
		replace Y`i'Party_Vote_Long = "USA: Green Party of the United States" if Y`i'Party_Vote == "Green_USA"

		replace Y`i'Party_Vote = "Reform_USA" if BB`i'Party == "USA: Reform Party"
		replace Y`i'Party_Vote_Long = "USA: Reform Party of the United States of America" if Y`i'Party_Vote == "Reform_USA"

		tab BB`i'Party if BB_Country == "US" & Y`i'Party_Vote=="." & Y_No`i'Party_Vote !=1

		/* Testing US*/
		tab Y`i'Party_Vote_Long if BB_Country == "US" & !missing(BB`i'Party) & Y_No`i'Party_Vote==0, missing
		tab BB`i'Party if BB_Country == "US" & Y_No`i'Party_Vote==0 & Y`i'Party_Vote_Long=="."  

		count if Y`i'Party_Vote_Long=="." & BB_Country == "US" & !missing(BB`i'Party) & Y_No`i'Party_Vote==0
		assert r(N)==0
}

tab BB_Party BB_Year if Y_Party_Vote_Long=="." & BB_Country=="US" & !missing(BB_Party) & Y_No_Party_Vote==0
tab BB_2nd_Party BB_Year if Y_2nd_Party_Vote_Long=="." & BB_Country=="US" & !missing(BB_2nd_Party) & Y_No_2nd_Party_Vote==0
tab BB_Least_Party BB_Year if Y_Least_Party_Vote_Long=="." & BB_Country=="US" & !missing(BB_Least_Party) & Y_No_Least_Party_Vote==0

/* Replace non-ascii characters that cause issues*/
ds *, has(type string)
foreach party in BB_Party BB_2nd_Party BB_Least_Party {
    if "`party'" == "BB_Party" {
        local i "$prefix_BB_Party"
    }
    else if "`party'" == "BB_2nd_Party" {
        local i "$prefix_BB_2nd_Party"
    }
    else if "`party'" == "BB_Least_Party" {
        local i "$prefix_BB_Least_Party"
    }
	
foreach x in Y`i'Party_Vote_Long {
     forvalues i=128/255 {
         replace `x' = subinstr(`x', char(`i'), "", .)
     }
}
}
//charlist Y`i'Party_Vote_Long

// save "$POPULISM_DROPBOX/data/intdata/temp/wvs_politics_clean.dta", replace
save "$POPULISM_DROPBOX/data/intdata/temp/ivs_politics_clean.dta", replace

timer off 2
timer list 2
display `r(t1)'

