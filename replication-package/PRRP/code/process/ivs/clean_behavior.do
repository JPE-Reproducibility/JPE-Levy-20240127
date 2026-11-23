/* Purpose: clean behavior variables */

global checkDistinctOn 0

capture program drop checkDistinct
program define checkDistinct
	if ($checkDistinctOn) {
		distinct `1'
		assert r(ndistinct)==`2'
	}
end

use "$POPULISM_DROPBOX/data/intdata/temp/ivs_clean_dem.dta", clear

//How often discusses political matters with friends
//Codebook A062
decode A062, gen (temp_discuss_pol)
gen X_behav_discuss_pol = .
replace X_behav_discuss_pol = 0 if temp_discuss_pol == "Never"
replace X_behav_discuss_pol = 1 if temp_discuss_pol == "Occasionally"
replace X_behav_discuss_pol = 2 if temp_discuss_pol == "Frequently"
checkDistinct X_behav_discuss_pol 3

//How often do you attend religious services
//Codebook F028
decode F028, gen (temp_rel_services)
gen X_dem_rel_services = .
replace X_dem_rel_services = 0 if temp_rel_services=="Never practically never"
replace X_dem_rel_services = 1 if temp_rel_services=="Less often"
replace X_dem_rel_services = 2 if temp_rel_services=="Once a year"
replace X_dem_rel_services = 3 if temp_rel_services=="Other specific holy days"
replace X_dem_rel_services = 4 if temp_rel_services=="Only on special holy days/Christmas/Easter days"
replace X_dem_rel_services = 5 if temp_rel_services=="Once a month"
replace X_dem_rel_services = 6 if temp_rel_services=="Once a week"
replace X_dem_rel_services = 7 if temp_rel_services=="More than once a week"
checkDistinct X_dem_rel_services 8

//Member of an Environmental Organization
//Codebook A103
decode A103, gen (temp_env_member)
decode A071, gen (temp_belong_conserv)
decode A071B, gen (temp_belong_env)
gen X_dem_environ_org = .
replace X_dem_environ_org = 0 if temp_env_member == "Not a member" | temp_env_member == "Inactive member" | temp_belong_env == "Not mentioned" | temp_belong_conserv == "Not mentioned"
replace X_dem_environ_org = 1 if temp_env_member == "Active member" | temp_belong_env == "Mentioned" | temp_belong_conserv == "Mentioned"
checkDistinct X_dem_environ_org 2

//Member of Religious Organization
//Codebook A098
decode A098, gen (temp_religious_org_member)
decode A065, gen (temp_belong_religious_org)
gen X_dem_religious_org_member = .
replace X_dem_religious_org_member = 0 if temp_religious_org_member=="Inactive member" | temp_religious_org_member=="Not a member" | temp_belong_religious_org=="Not mentioned"
replace X_dem_religious_org_member = 1 if temp_religious_org_member=="Active member" | temp_belong_religious_org=="Mentioned"
checkDistinct X_dem_religious_org_member 2

//Active or inactive member: sports or recreation organization
//Codebook A099
decode A099, gen (temp_sports_org_member)
decode A074, gen (temp_belong_sports)
gen X_dem_sports_org_member = .
replace X_dem_sports_org_member = 0 if temp_sports_org_member=="Inactive member" | temp_sports_org_member=="Not a member" | temp_belong_sports=="Not mentioned"
replace X_dem_sports_org_member = 1 if temp_sports_org_member=="Active member" | temp_belong_sports=="Mentioned"
checkDistinct X_dem_sports_org_member 2

//Active or inactive member: art, music, or educational organization
//Codebook A100
decode A100, gen (temp_art_org_member)
decode A066, gen (temp_belong_cultural)
gen X_dem_art_org_member = .
replace X_dem_art_org_member = 0 if temp_art_org_member=="Inactive member" | temp_art_org_member=="Not a member" | temp_belong_cultural=="Not mentioned"
replace X_dem_art_org_member = 1 if temp_art_org_member=="Active member" | temp_belong_cultural=="Mentioned"
checkDistinct X_dem_art_org_member 2

//Active or inactive member: political party
//Codebook A102
decode A102, gen (temp_pol_party_member)
decode A068, gen (temp_belong_pol)
gen X_dem_pol_party_member = .
replace X_dem_pol_party_member = 0 if temp_pol_party_member=="Inactive member" | temp_pol_party_member=="Not a member" | temp_belong_pol=="Not mentioned"
replace X_dem_pol_party_member = 1 if temp_pol_party_member=="Active member" | temp_belong_pol=="Mentioned"
checkDistinct X_dem_pol_party_member 2

//Active or inactive member: professional organization
//Codebook A104
decode A104, gen (temp_prof_org_member)
decode A072, gen (temp_belong_prof)
gen X_dem_prof_org_member = .
replace X_dem_prof_org_member = 0 if temp_prof_org_member=="Inactive member" | temp_prof_org_member=="Not a member" | temp_belong_prof=="Not mentioned"
replace X_dem_prof_org_member = 1 if temp_prof_org_member=="Active member" | temp_belong_prof=="Mentioned"
checkDistinct X_dem_prof_org_member 2

//Active or inactive member: charitable or humanitarian organization
//Codebook A105
decode A105, gen (temp_charit_org_member)
decode A080_01, gen (temp_belong_charit)
gen X_dem_charit_org_member = .
replace X_dem_charit_org_member = 0 if temp_charit_org_member=="Inactive member" | temp_charit_org_member=="Not a member" | temp_belong_charit=="Not mentioned"
replace X_dem_charit_org_member = 1 if temp_charit_org_member=="Active member" | temp_belong_charit=="Mentioned"
checkDistinct X_dem_charit_org_member 2

//Active or inactive member: any other organization
//Codebook A106
decode A106, gen (temp_other_org_member)
decode A079, gen (temp_belong_other)
gen X_dem_other_org_member = .
replace X_dem_other_org_member = 0 if temp_other_org_member=="Inactive member" | temp_other_org_member=="Not a member" | temp_belong_other=="Not mentioned"
replace X_dem_other_org_member = 1 if temp_other_org_member=="Active member" | temp_belong_other=="Mentioned"
checkDistinct X_dem_other_org_member 2

//Political action: Signing a petition
//Codebook E025
decode E025, gen (temp_sign_petition)
gen X_behav_sign_petition = .
replace X_behav_sign_petition = 0 if temp_sign_petition=="Would never do"
replace X_behav_sign_petition = 1 if temp_sign_petition=="Might do"
replace X_behav_sign_petition = 2 if temp_sign_petition=="Have done"
checkDistinct X_behav_sign_petition 3

//Political action: Joining in boycotts
//Codebook E026
decode E026, gen (temp_join_boycott)
gen X_behav_join_boycott = .
replace X_behav_join_boycott = 0 if temp_join_boycott=="Would never do"
replace X_behav_join_boycott = 1 if temp_join_boycott=="Might do"
replace X_behav_join_boycott = 2 if temp_join_boycott=="Have done"
checkDistinct X_behav_join_boycott 3

//Political action: Attending lawful/peaceful demonstrations
//Codebook E027
decode E027, gen (temp_demonstrations)
gen X_behav_demonstrations = .
replace X_behav_demonstrations = 0 if temp_demonstrations=="Would never do"
replace X_behav_demonstrations = 1 if temp_demonstrations=="Might do"
replace X_behav_demonstrations = 2 if temp_demonstrations=="Have done"
checkDistinct X_behav_demonstrations 3

//Political action: Joining unofficial strikes
//Codebook E028
decode E028, gen (temp_join_strike)
gen X_behav_join_strike = .
replace X_behav_join_strike = 0 if temp_join_strike=="Would never do"
replace X_behav_join_strike = 1 if temp_join_strike=="Might do"
replace X_behav_join_strike = 2 if temp_join_strike=="Have done"
checkDistinct X_behav_join_strike 3

//Information source: Daily newspaper. Note not using E261 for media questions, only ased in last wave
//Codebook E248B
decode E248B, gen (temp_info_news)
gen X_behav_info_newspaper = .
replace X_behav_info_newspaper = 0 if temp_info_news == "Never"
replace X_behav_info_newspaper = 1 if temp_info_news == "Less than monthly"
replace X_behav_info_newspaper = 2 if temp_info_news == "Monthly"
replace X_behav_info_newspaper = 3 if temp_info_news == "Weekly"
replace X_behav_info_newspaper = 4 if temp_info_news == "Daily"
checkDistinct X_behav_info_newspaper 5

//Information source: Talk with friends or colleagues
//Codebook E254B
decode E254B, gen (temp_info_friends)
gen X_behav_info_friends = .
replace X_behav_info_friends = 0 if temp_info_friends == "Never"
replace X_behav_info_friends = 1 if temp_info_friends == "Less than monthly"
replace X_behav_info_friends = 2 if temp_info_friends == "Monthly"
replace X_behav_info_friends = 3 if temp_info_friends == "Weekly"
replace X_behav_info_friends = 4 if temp_info_friends == "Daily"
checkDistinct X_behav_info_friends 5

//Information source: TV news
//Codebook E258B
decode E258B, gen (temp_info_TV)
gen X_behav_info_TV = .
replace X_behav_info_TV = 0 if temp_info_TV == "Never"
replace X_behav_info_TV = 1 if temp_info_TV == "Less than monthly"
replace X_behav_info_TV = 2 if temp_info_TV == "Monthly"
replace X_behav_info_TV = 3 if temp_info_TV == "Weekly"
replace X_behav_info_TV = 4 if temp_info_TV == "Daily"
checkDistinct X_behav_info_TV 5

//Information source: Radio news
//Codebook E259B
decode E259B, gen (temp_info_radio)
gen X_behav_info_radio = .
replace X_behav_info_radio = 0 if temp_info_radio == "Never"
replace X_behav_info_radio = 1 if temp_info_radio == "Less than monthly"
replace X_behav_info_radio = 2 if temp_info_radio == "Monthly"
replace X_behav_info_radio = 3 if temp_info_radio == "Weekly"
replace X_behav_info_radio = 4 if temp_info_radio == "Daily"
checkDistinct X_behav_info_radio 5

//Information source: Mobile phone
//Codebook E260B
decode E260B, gen (temp_info_phone)
gen X_behav_info_phone = .
replace X_behav_info_phone = 0 if temp_info_phone == "Never"
replace X_behav_info_phone = 1 if temp_info_phone == "Less than monthly"
replace X_behav_info_phone = 2 if temp_info_phone == "Monthly"
replace X_behav_info_phone = 3 if temp_info_phone == "Weekly"
replace X_behav_info_phone = 4 if temp_info_phone == "Daily"
checkDistinct X_behav_info_phone 5

//Information source: Email
//Codebook E261B
decode E261B, gen (temp_info_email)
gen X_behav_info_email = .
replace X_behav_info_email = 0 if temp_info_email == "Never"
replace X_behav_info_email = 1 if temp_info_email == "Less than monthly"
replace X_behav_info_email = 2 if temp_info_email == "Monthly"
replace X_behav_info_email = 3 if temp_info_email == "Weekly"
replace X_behav_info_email = 4 if temp_info_email == "Daily"
checkDistinct X_behav_info_email 5

//Information source: Internet
//Codebook E262B
decode E262B, gen (temp_info_internet)
gen X_behav_info_internet = .
replace X_behav_info_internet = 0 if temp_info_internet == "Never"
replace X_behav_info_internet = 1 if temp_info_internet == "Less than monthly"
replace X_behav_info_internet = 2 if temp_info_internet == "Monthly"
replace X_behav_info_internet = 3 if temp_info_internet == "Weekly"
replace X_behav_info_internet = 4 if temp_info_internet == "Daily"
checkDistinct X_behav_info_internet 5


save "$POPULISM_DROPBOX/data/intdata/temp/ivs_clean_behav.dta", replace
