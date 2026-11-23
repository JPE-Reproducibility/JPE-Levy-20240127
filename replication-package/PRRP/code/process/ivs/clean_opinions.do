/* Purpose: clean opinion variables */

global checkDistinctOn 1

capture program drop checkDistinct
program define checkDistinct
	if ($checkDistinctOn) {
		distinct `1'
		assert r(ndistinct)==`2'
	}
end

use "$POPULISM_DROPBOX/data/intdata/temp/ivs_clean_behav.dta", clear
run "code/process/utils.do"



// Self- reported political scale
decode E033, gen(temp_self_pol)
gen X_general_self_political_scale = .
replace X_general_self_political_scale = 1 if temp_self_pol=="Left"
replace X_general_self_political_scale = 2 if temp_self_pol=="2"
replace X_general_self_political_scale = 3 if temp_self_pol=="3"
replace X_general_self_political_scale = 4 if temp_self_pol=="4"
replace X_general_self_political_scale = 5 if temp_self_pol=="5"
replace X_general_self_political_scale = 6 if temp_self_pol=="6"
replace X_general_self_political_scale = 7 if temp_self_pol=="7"
replace X_general_self_political_scale = 8 if temp_self_pol=="8"
replace X_general_self_political_scale = 9 if temp_self_pol=="9"
replace X_general_self_political_scale = 10 if temp_self_pol=="Right"
checkDistinct X_general_self_political_scale 10

gen Unimp_general_self_political = X_general_self_political

// Aims of respondent index
//First aim of respondent
decode E003, gen (temp_first_aim_respondent)
decode E004, gen (temp_second_aim_respondent)

replace temp_first_aim_respondent = "" if inlist(temp_first_aim_respondent, "Don´t know", "Don't know", "No answer", "Not asked in survey", "Missing; Unknown", "Missing: other")
replace temp_second_aim_respondent = "" if inlist(temp_second_aim_respondent , "Don´t know", "Don't know", "No answer", "Not asked in survey", "Missing; Unknown", "Missing: other")
gen missing_aim_respondent = temp_first_aim_respondent=="" | temp_second_aim_respondent=="" | temp_first_aim_respondent == temp_second_aim_respondent 


gen X_general_1_order_2_say_gov = 0 if !missing_aim_respondent 
replace X_general_1_order_2_say_gov = 1 if temp_first_aim_respondent == "Maintaining order in the nation" & temp_second_aim_respondent == "Give people more say"
checkDistinct X_general_1_order_2_say_gov 2

gen X_general_1_order_2_prices = 0 if !missing_aim_respondent 
replace X_general_1_order_2_prices = 1 if temp_first_aim_respondent == "Maintaining order in the nation" & temp_second_aim_respondent == "Fighting rising prices"
checkDistinct X_general_1_order_2_prices 2

gen X_general_1_order_2_speech = 0 if !missing_aim_respondent 
replace X_general_1_order_2_speech = 1 if temp_first_aim_respondent == "Maintaining order in the nation" & temp_second_aim_respondent == "Protecting freedom of speech"
checkDistinct X_general_1_order_2_speech 2

gen X_general_1_say_gov_2_order = 0 if !missing_aim_respondent 
replace X_general_1_say_gov_2_order = 1 if temp_first_aim_respondent == "Give people more say" & temp_second_aim_respondent == "Maintaining order in the nation"
checkDistinct X_general_1_say_gov_2_order 2

gen X_general_1_say_gov_2_prices = 0 if !missing_aim_respondent 
replace X_general_1_say_gov_2_prices = 1 if temp_first_aim_respondent == "Give people more say" & temp_second_aim_respondent == "Fighting rising prices"
checkDistinct X_general_1_say_gov_2_prices 2

gen X_general_1_say_gov_2_speech = 0 if !missing_aim_respondent 
replace X_general_1_say_gov_2_speech = 1 if temp_first_aim_respondent == "Give people more say" & temp_second_aim_respondent == "Protecting freedom of speech"
checkDistinct X_general_1_say_gov_2_speech 2

gen X_general_1_prices_2_order = 0 if !missing_aim_respondent 
replace X_general_1_prices_2_order = 1 if temp_first_aim_respondent == "Fighting rising prices" & temp_second_aim_respondent == "Maintaining order in the nation"
checkDistinct X_general_1_prices_2_order 2

gen X_general_1_prices_2_say_gov = 0 if !missing_aim_respondent 
replace X_general_1_prices_2_say_gov = 1 if temp_first_aim_respondent == "Fighting rising prices" & temp_second_aim_respondent == "Give people more say"
checkDistinct X_general_1_prices_2_say_gov 2

gen X_general_1_prices_2_speech = 0 if !missing_aim_respondent 
replace X_general_1_prices_2_speech = 1 if temp_first_aim_respondent == "Fighting rising prices" & temp_second_aim_respondent == "Protecting freedom of speech"
checkDistinct X_general_1_prices_2_speech 2

gen X_general_1_speech_2_order = 0 if !missing_aim_respondent 
replace X_general_1_speech_2_order = 1 if temp_first_aim_respondent == "Protecting freedom of speech" & temp_second_aim_respondent == "Maintaining order in the nation"
checkDistinct X_general_1_speech_2_order 2

gen X_general_1_speech_2_prices = 0 if !missing_aim_respondent 
replace X_general_1_speech_2_prices = 1 if temp_first_aim_respondent == "Protecting freedom of speech" & temp_second_aim_respondent == "Fighting rising prices"
checkDistinct X_general_1_speech_2_prices 2

//Aims of country: first choice
//Codebook E001
decode E001, gen (temp_first_aim_country)
decode E002, gen (temp_second_aim_country)

tab temp_first_aim_country, missing

replace temp_first_aim_country = "" if inlist(temp_first_aim_country, "Don´t know", "Don't know", "No answer", "Not asked in survey", "Missing; Unknown", "Missing: other")
replace temp_second_aim_country = "" if inlist(temp_second_aim_country, "Don´t know", "Don't know", "No answer", "Not asked in survey", "Missing; Unknown", "Missing: other")
gen missing_aim_country = temp_first_aim_country=="" | temp_second_aim_country=="" | temp_first_aim_country == temp_second_aim_country

gen X_general_country_1_econ_2_def = 0 if !missing_aim_country
replace X_general_country_1_econ_2_def = 1 if temp_first_aim_country == "A high level of economic growth" & temp_second_aim_country == "Strong defence forces"
checkDistinct X_general_country_1_econ_2_def 2

gen X_general_country_1_econ_2_say = 0 if !missing_aim_country
replace X_general_country_1_econ_2_say = 1 if temp_first_aim_country == "A high level of economic growth" & temp_second_aim_country == "People have more say about how things are done"
checkDistinct X_general_country_1_econ_2_say 2

gen X_general_country_1_econ_2_cit = 0 if !missing_aim_country
replace X_general_country_1_econ_2_cit = 1 if temp_first_aim_country == "A high level of economic growth" & temp_second_aim_country == "Trying to make our cities and countryside more beautiful"
checkDistinct X_general_country_1_econ_2_cit 2

gen X_general_country_1_def_2_econ = 0 if !missing_aim_country
replace X_general_country_1_def_2_econ = 1 if temp_first_aim_country == "Strong defence forces" & temp_second_aim_country == "A high level of economic growth"
checkDistinct X_general_country_1_def_2_econ 2

gen X_general_country_1_def_2_say = 0 if !missing_aim_country
replace X_general_country_1_def_2_say = 1 if temp_first_aim_country == "Strong defence forces" & temp_second_aim_country == "People have more say about how things are done"
checkDistinct X_general_country_1_def_2_say 2

gen X_general_country_1_def_2_cities = 0 if !missing_aim_country
replace X_general_country_1_def_2_cities = 1 if temp_first_aim_country == "Strong defence forces" & temp_second_aim_country == "Trying to make our cities and countryside more beautiful"
checkDistinct X_general_country_1_def_2_cities 2

gen X_general_country_1_say_2_econ = 0 if !missing_aim_country
replace X_general_country_1_say_2_econ = 1 if temp_first_aim_country == "People have more say about how things are done" & temp_second_aim_country == "A high level of economic growth"
checkDistinct X_general_country_1_say_2_econ 2

gen X_general_country_1_say_2_def = 0 if !missing_aim_country
replace X_general_country_1_say_2_def = 1 if temp_first_aim_country == "People have more say about how things are done" & temp_second_aim_country == "Strong defence forces"
checkDistinct X_general_country_1_say_2_def 2

gen X_general_country_1_say_2_cities = 0 if !missing_aim_country
replace X_general_country_1_say_2_cities = 1 if temp_first_aim_country == "People have more say about how things are done" & temp_second_aim_country == "Trying to make our cities and countryside more beautiful"
checkDistinct X_general_country_1_say_2_cities 2

gen X_general_country_1_cities_2_say = 0 if !missing_aim_country
replace X_general_country_1_cities_2_say = 1 if temp_first_aim_country == "Trying to make our cities and countryside more beautiful" & temp_second_aim_country == "People have more say about how things are done"
checkDistinct X_general_country_1_cities_2_say 2

gen X_general_country_1_cit_2_econ = 0 if !missing_aim_country
replace X_general_country_1_cit_2_econ = 1 if temp_first_aim_country == "Trying to make our cities and countryside more beautiful" & temp_second_aim_country == "A high level of economic growth"
checkDistinct X_general_country_1_cit_2_econ 2

//Most important:first choice
//Codebook E005
decode E005, gen (temp_most_import)
decode E006, gen (temp_second_import)

tab temp_most_import, missing

replace temp_most_import = "" if inlist(temp_most_import, "Don´t know", "Don't know", "No answer", "Not asked in survey", "Missing; Unknown", "Missing: other")
replace temp_second_import = "" if inlist(temp_second_import, "Don´t know", "Don't know", "No answer", "Not asked in survey", "Missing; Unknown", "Missing: other")
gen missing_import = temp_most_import=="" | temp_second_import=="" | temp_most_import == temp_second_import

/*gen econ_most_important = 0 if !missing_import
replace econ_most_important = 1 if temp_most_import=="A stable economy"

gen econ_second_important = 0 if !missing_import
replace econ_second_important = 1 if temp_second_import=="A stable economy"

gen crime_most_important = 0 if !missing_import
replace crime_most_important = 1 if temp_most_import=="The fight against crime"

gen crime_second_important = 0 if !missing_import
replace crime_second_important = 1 if temp_second_import=="The fight against crime"*/


gen X_general_1_econ_2_humane = 0 if !missing_import
replace X_general_1_econ_2_humane = 1 if temp_most_import=="A stable economy" & temp_second_import == "Progress toward a less impersonal and more humane  society"
checkDistinct X_general_1_econ_2_humane 2

gen X_general_1_econ_2_ideas = 0 if !missing_import
replace X_general_1_econ_2_ideas = 1 if temp_most_import=="A stable economy" & temp_second_import == "Ideas count more than money"
checkDistinct X_general_1_econ_2_ideas 2

gen X_general_1_econ_2_crime = 0 if !missing_import
replace X_general_1_econ_2_crime = 1 if temp_most_import=="A stable economy" & temp_second_import == "The fight against crime"
checkDistinct X_general_1_econ_2_crime 2

gen X_general_1_humane_2_econ = 0 if !missing_import
replace X_general_1_humane_2_econ = 1 if temp_most_import=="Progress toward a less impersonal and more humane  society" & temp_second_import == "A stable economy"
checkDistinct X_general_1_humane_2_econ 2

gen X_general_1_humane_2_ideas = 0 if !missing_import
replace X_general_1_humane_2_ideas = 1 if temp_most_import=="Progress toward a less impersonal and more humane  society" & temp_second_import == "Ideas count more than money"
checkDistinct X_general_1_humane_2_ideas 2

gen X_general_1_humane_2_crime = 0 if !missing_import
replace X_general_1_humane_2_crime = 1 if temp_most_import=="Progress toward a less impersonal and more humane  society" & temp_second_import == "The fight against crime"
checkDistinct X_general_1_humane_2_crime 2

gen X_general_1_ideas_2_econ = 0 if !missing_import
replace X_general_1_ideas_2_econ = 1 if temp_most_import=="Ideas count more than money" & temp_second_import == "A stable economy"
checkDistinct X_general_1_ideas_2_econ 2

gen X_general_1_ideas_2_humane = 0 if !missing_import
replace X_general_1_ideas_2_humane = 1 if temp_most_import=="Ideas count more than money" & temp_second_import == "Progress toward a less impersonal and more humane  society"
checkDistinct X_general_1_ideas_2_humane 2

gen X_general_1_ideas_2_crime = 0 if !missing_import
replace X_general_1_ideas_2_crime = 1 if temp_most_import=="Ideas count more than money" & temp_second_import == "The fight against crime"
checkDistinct X_general_1_ideas_2_crime 2

gen X_general_1_crime_2_econ = 0 if !missing_import
replace X_general_1_crime_2_econ = 1 if temp_most_import=="The fight against crime" & temp_second_import == "A stable economy"
checkDistinct X_general_1_crime_2_econ 2

gen X_general_1_crime_2_ideas = 0 if !missing_import
replace X_general_1_crime_2_ideas = 1 if temp_most_import=="The fight against crime" & temp_second_import == "Ideas count more than money"
checkDistinct X_general_1_crime_2_ideas 2

// Future changes: Greater respect for authority
decode E018, gen (temp_authority)
gen X_op_respect_for_authority = .
replace X_op_respect_for_authority = 0 if temp_authority == "Bad thing"
replace X_op_respect_for_authority = 1 if temp_authority == "Don´t mind"
replace X_op_respect_for_authority = 2 if temp_authority == "Good thing"
checkDistinct X_op_respect_for_authority 3

// If jobs are scarce, priority should be given to citizens over immigrants; 0 = disagree strongly, 1 = neither should get priority, 2 = agree strongly
decode C002, gen(str_immigrantjobs)
gen X_op_immigrant_jobs_index = .
replace X_op_immigrant_jobs_index = 0 if str_immigrantjobs == "Disagree"
replace X_op_immigrant_jobs_index = 1 if str_immigrantjobs == "Neither"
replace X_op_immigrant_jobs_index = 2 if str_immigrantjobs == "Agree"
checkDistinct X_op_immigrant_jobs_index 3

// Private vs state ownership of business 
decode E036, gen (temp_private_state)
gen X_op_PrivateVsState = .
replace X_op_PrivateVsState = 0 if temp_private_state=="Private ownership of business should be increased"
replace X_op_PrivateVsState = 1 if temp_private_state=="2"
replace X_op_PrivateVsState = 2 if temp_private_state=="3"
replace X_op_PrivateVsState = 3 if temp_private_state=="4"
replace X_op_PrivateVsState = 4 if temp_private_state=="5"
replace X_op_PrivateVsState = 5 if temp_private_state=="6"
replace X_op_PrivateVsState = 6 if temp_private_state=="7"
replace X_op_PrivateVsState = 7 if temp_private_state=="8"
replace X_op_PrivateVsState = 8 if temp_private_state=="9"
replace X_op_PrivateVsState = 9 if temp_private_state=="Government ownership of business should be increased"
checkDistinct X_op_PrivateVsState 10


// Income equality versus larger income differences (higher value = larger income difference)
decode E035, gen (temp_income_inequal)
gen X_op_income_equality_index = .
replace X_op_income_equality_index = 0 if temp_income_inequal=="Incomes should be made more equal"
replace X_op_income_equality_index = 1 if temp_income_inequal=="2"
replace X_op_income_equality_index = 2 if temp_income_inequal=="3"
replace X_op_income_equality_index = 3 if temp_income_inequal=="4"
replace X_op_income_equality_index = 4 if temp_income_inequal=="5"
replace X_op_income_equality_index = 5 if temp_income_inequal=="6"
replace X_op_income_equality_index = 6 if temp_income_inequal=="7"
replace X_op_income_equality_index = 7 if temp_income_inequal=="8"
replace X_op_income_equality_index = 8 if temp_income_inequal=="9"
replace X_op_income_equality_index = 9 if temp_income_inequal=="We need larger income differences as incentives"

// Protecting the Environment vs Economic Growth Question
decode B008, gen(temp_environVsEconGrowth)
gen X_op_environVsEconGrowth = 0 if temp_environVsEconGrowth== "Protecting environment"
replace X_op_environVsEconGrowth = 1 if temp_environVsEconGrowth== "Other answer"
replace X_op_environVsEconGrowth = 2 if temp_environVsEconGrowth== "Economy growth and creating jobs"
checkDistinct X_op_environVsEconGrowth 3

// People who don't work turn lazy - "disagree strongly" (0) to "agree strongly" (4) Index
decode C038, gen(temp_NoWorkLazy)
gen X_op_NoWorkLazy = 4 if temp_NoWorkLazy == "Strongly agree" 
replace X_op_NoWorkLazy = 3 if temp_NoWorkLazy == "Agree" 
replace X_op_NoWorkLazy = 2 if temp_NoWorkLazy == "Neither agree nor disagree" 
replace X_op_NoWorkLazy = 1 if temp_NoWorkLazy == "Disagree" 
replace X_op_NoWorkLazy = 0 if temp_NoWorkLazy == "Strongly disagree" 
checkDistinct X_op_NoWorkLazy 5

// Government vs Personal Responsibility - Higher Value = more personal responsibility
decode E037, gen (temp_gov_resp)
gen X_op_GovtVsPersonalResp = . 
replace X_op_GovtVsPersonalResp = 9 if temp_gov_resp=="People should take more responsibility"
replace X_op_GovtVsPersonalResp = 8 if temp_gov_resp=="2"
replace X_op_GovtVsPersonalResp = 7 if temp_gov_resp=="3"
replace X_op_GovtVsPersonalResp = 6 if temp_gov_resp=="4"
replace X_op_GovtVsPersonalResp = 5 if temp_gov_resp=="5"
replace X_op_GovtVsPersonalResp = 4 if temp_gov_resp=="6"
replace X_op_GovtVsPersonalResp = 3 if temp_gov_resp=="7"
replace X_op_GovtVsPersonalResp = 2 if temp_gov_resp=="8"
replace X_op_GovtVsPersonalResp = 1 if temp_gov_resp=="9"
replace X_op_GovtVsPersonalResp = 0 if temp_gov_resp=="The government should take more responsibility"
checkDistinct X_op_GovtVsPersonalResp 10

// Competition Harmful (lower value) or Good (higher value)
decode E039, gen (temp_competition)
gen X_op_Competition_Markets = .
replace X_op_Competition_Markets = 9 if temp_competition=="Competition is good"
replace X_op_Competition_Markets = 8 if temp_competition=="2"
replace X_op_Competition_Markets = 7 if temp_competition=="3"
replace X_op_Competition_Markets = 6 if temp_competition=="4"
replace X_op_Competition_Markets = 5 if temp_competition=="5"
replace X_op_Competition_Markets = 4 if temp_competition=="6"
replace X_op_Competition_Markets = 3 if temp_competition=="7"
replace X_op_Competition_Markets = 2 if temp_competition=="8"
replace X_op_Competition_Markets = 1 if temp_competition=="9"
replace X_op_Competition_Markets = 0 if temp_competition=="Competition is harmful"
checkDistinct X_op_Competition_Markets 10

// Political system: Having a strong leader
decode E114, gen (temp_leader_strong)
gen X_op_Strong_Leader_Good = .
replace X_op_Strong_Leader_Good = 0 if temp_leader_strong == "Very bad" 
replace X_op_Strong_Leader_Good = 1 if temp_leader_strong == "Fairly bad"
replace X_op_Strong_Leader_Good = 2 if temp_leader_strong == "Fairly good"
replace X_op_Strong_Leader_Good = 3 if temp_leader_strong == "Very good"
checkDistinct X_op_Strong_Leader_Good 4

// How proud of nationality? 
decode G006, gen (temp_nation_pride)
gen X_op_Pride_Nationality = .
replace X_op_Pride_Nationality = 0 if temp_nation_pride == "Not at all proud"
replace X_op_Pride_Nationality = 1 if temp_nation_pride == "Not very proud"
replace X_op_Pride_Nationality = 2 if temp_nation_pride == "Quite proud"
replace X_op_Pride_Nationality = 3 if temp_nation_pride == "Very proud"
checkDistinct X_op_Pride_Nationality 4

// Happiness Index 
decode A008, gen (temp_happy)
gen X_op_happiness = .
replace X_op_happiness = 0 if temp_happy == "Not at all happy"
replace X_op_happiness = 1 if temp_happy == "Not very happy"
replace X_op_happiness = 2 if temp_happy == "Quite happy"
replace X_op_happiness = 3 if temp_happy == "Very happy"
checkDistinct X_op_happiness 4

// Most people can generally be trusted; 0 = "need to be very careful", 1 = yes
decode A165, gen(temp_ppl_trust)
gen X_op_ppl_trusted = .
replace X_op_ppl_trusted = 0 if temp_ppl_trust == "Can´t be too careful"
replace X_op_ppl_trusted = 1 if temp_ppl_trust == "Most people can be trusted"
checkDistinct X_op_ppl_trusted 2

// Satisfaction with life index
decode A170, gen (temp_satis_life)
gen X_op_satis_life = .
replace X_op_satis_life = 0 if temp_satis_life=="Dissatisfied"
replace X_op_satis_life = 1 if temp_satis_life=="2"
replace X_op_satis_life = 2 if temp_satis_life=="3"
replace X_op_satis_life = 3 if temp_satis_life=="4"
replace X_op_satis_life = 4 if temp_satis_life=="5"
replace X_op_satis_life = 5 if temp_satis_life=="6"
replace X_op_satis_life = 6 if temp_satis_life=="7"
replace X_op_satis_life = 7 if temp_satis_life=="8"
replace X_op_satis_life = 8 if temp_satis_life=="9"
replace X_op_satis_life = 9 if temp_satis_life=="Satisfied"
checkDistinct X_op_satis_life 10

// Freedom of choice index
decode A173, gen (temp_freedom_choice)
gen X_op_freedom_of_choice = .
replace X_op_freedom_of_choice = 0 if temp_freedom_choice=="None at all"
replace X_op_freedom_of_choice = 1 if temp_freedom_choice=="2"
replace X_op_freedom_of_choice = 2 if temp_freedom_choice=="3"
replace X_op_freedom_of_choice = 3 if temp_freedom_choice=="4"
replace X_op_freedom_of_choice = 4 if temp_freedom_choice=="5"
replace X_op_freedom_of_choice = 5 if temp_freedom_choice=="6"
replace X_op_freedom_of_choice = 6 if temp_freedom_choice=="7"
replace X_op_freedom_of_choice = 7 if temp_freedom_choice=="8"
replace X_op_freedom_of_choice = 8 if temp_freedom_choice=="9"
replace X_op_freedom_of_choice = 9 if temp_freedom_choice=="A great deal"
checkDistinct X_op_freedom_of_choice 10

// Jobs scarce: Men should have more right to a job than women
decode C001, gen (temp_men_jobs)
gen X_op_men_jobs_priority = .
replace X_op_men_jobs_priority = 0 if temp_men_jobs == "Disagree"
replace X_op_men_jobs_priority = 1 if temp_men_jobs == "Neither"
replace X_op_men_jobs_priority = 2 if temp_men_jobs == "Agree"
checkDistinct X_op_men_jobs_priority 3

// Satisfaction with your current political system - does NOT include observations from time series data 
decode E111, gen (temp_satis_polsystem)
gen X_op_satis_polsystem = .
replace X_op_satis_polsystem = 0 if temp_satis_polsystem=="Bad"
replace X_op_satis_polsystem = 1 if temp_satis_polsystem=="2"
replace X_op_satis_polsystem = 2 if temp_satis_polsystem=="3"
replace X_op_satis_polsystem = 3 if temp_satis_polsystem=="4"
replace X_op_satis_polsystem = 4 if temp_satis_polsystem=="5"
replace X_op_satis_polsystem = 5 if temp_satis_polsystem=="6"
replace X_op_satis_polsystem = 6 if temp_satis_polsystem=="7"
replace X_op_satis_polsystem = 7 if temp_satis_polsystem=="8"
replace X_op_satis_polsystem = 8 if temp_satis_polsystem=="9"
replace X_op_satis_polsystem = 9 if temp_satis_polsystem=="Very good"
checkDistinct X_op_satis_polsystem 10

// Good or bad: Political system with experts, not politicians, making decisions. 0 = very bad, 3 = very good
decode E115, gen (temp_experts_rule)
gen X_op_experts_rule = .
replace X_op_experts_rule = 0 if temp_experts_rule == "Very bad"
replace X_op_experts_rule = 1 if temp_experts_rule == "Fairly bad"
replace X_op_experts_rule = 2 if temp_experts_rule == "Fairly good"
replace X_op_experts_rule = 3 if temp_experts_rule == "Very good"
checkDistinct X_op_experts_rule 4

// Government taxes the rich to subsidize the poor: Against democracy or part of it.  0 = against, 10 = essential part of it
decode E224, gen (temp_redist)
gen X_op_redistribution_democratic = .
replace X_op_redistribution_democratic = 0 if E224 == 0
replace X_op_redistribution_democratic = 1 if temp_redist=="Not an essential characteristic of democracy"
replace X_op_redistribution_democratic = 2 if temp_redist=="2"
replace X_op_redistribution_democratic = 3 if temp_redist=="3"
replace X_op_redistribution_democratic = 4 if temp_redist=="4"
replace X_op_redistribution_democratic = 5 if temp_redist=="5"
replace X_op_redistribution_democratic = 6 if temp_redist=="6"
replace X_op_redistribution_democratic = 7 if temp_redist=="7"
replace X_op_redistribution_democratic = 8 if temp_redist=="8"
replace X_op_redistribution_democratic = 9 if temp_redist=="9"
replace X_op_redistribution_democratic = 10 if temp_redist=="An essential characteristic of democracy"
checkDistinct X_op_redistribution_democratic 11

// How much do you trust people of another nationality? - higher value = more trust in other nationalities
decode G007_36_B, gen (temp_trust_other)
gen X_op_trust_otherNat = .
replace X_op_trust_otherNat = 0 if temp_trust_other == "Do not trust at all"
replace X_op_trust_otherNat = 1 if temp_trust_other == "Do not trust very much"
replace X_op_trust_otherNat = 2 if temp_trust_other == "Trust somewhat"
replace X_op_trust_otherNat = 3 if temp_trust_other == "Trust completely"
checkDistinct X_op_trust_otherNat 4

// How close do you feel to your local community?
decode G020, gen (temp_loc_community)
gen X_op_local_community = .
replace X_op_local_community = 0 if temp_loc_community == "Strongly disagree"
replace X_op_local_community = 1 if temp_loc_community == "Disagree"
replace X_op_local_community = 2 if temp_loc_community == "Agree"
replace X_op_local_community = 3 if temp_loc_community == "Strongly agree"
checkDistinct X_op_local_community 4

// Do you feel as though you are a citizen of your country?
decode G021, gen (temp_cit_country) 
gen X_op_citizen_country = .
replace X_op_citizen_country = 0 if temp_cit_country == "Strongly disagree"
replace X_op_citizen_country = 1 if temp_cit_country == "Disagree"
replace X_op_citizen_country = 2 if temp_cit_country == "Agree"
replace X_op_citizen_country = 3 if temp_cit_country == "Strongly agree"
checkDistinct X_op_citizen_country 4

// Importance of Family
//codebook A001
decode A001, gen (temp_family_import)
gen X_op_family_import = .
replace X_op_family_import = 0 if temp_family_import == "Not at all important" 
replace X_op_family_import = 1 if temp_family_import == "Not very important"
replace X_op_family_import = 2 if temp_family_import == "Rather important"
replace X_op_family_import = 3 if temp_family_import == "Very important"
checkDistinct X_op_family_import 4

// Importance of Friends
//codebook A002
decode A002, gen (temp_friends_import)
gen X_op_friends_import = .
replace X_op_friends_import = 0 if temp_friends_import == "Not at all important" 
replace X_op_friends_import = 1 if temp_friends_import == "Not very important"
replace X_op_friends_import = 2 if temp_friends_import == "Rather important"
replace X_op_friends_import = 3 if temp_friends_import == "Very important"
checkDistinct X_op_friends_import 4

// Importance of Leisure Time
//codebook A003
decode A003, gen (temp_leisure_import)
gen X_op_leisure_import = .
replace X_op_leisure_import = 0 if temp_leisure_import == "Not at all important" 
replace X_op_leisure_import = 1 if temp_leisure_import == "Not very important"
replace X_op_leisure_import = 2 if temp_leisure_import == "Rather important"
replace X_op_leisure_import = 3 if temp_leisure_import == "Very important"
checkDistinct X_op_leisure_import 4

// Importance of Politics
//codebook A004
decode A004, gen (temp_pol_import)
gen X_op_pol_import = .
replace X_op_pol_import = 0 if temp_pol_import == "Not at all important" 
replace X_op_pol_import = 1 if temp_pol_import == "Not very important"
replace X_op_pol_import = 2 if temp_pol_import == "Rather important"
replace X_op_pol_import = 3 if temp_pol_import == "Very important"
checkDistinct X_op_pol_import 4

// Importance of Work
//codebook A005
decode A005, gen (temp_work_import)
gen X_op_work_import = .
replace X_op_work_import = 0 if temp_work_import == "Not at all important" 
replace X_op_work_import = 1 if temp_work_import == "Not very important"
replace X_op_work_import = 2 if temp_work_import == "Rather important"
replace X_op_work_import = 3 if temp_work_import == "Very important"
checkDistinct X_op_work_import 4

// Importance of Religion
//codebook A006
decode A006, gen (temp_religion_import)
gen X_op_religion_import = .
replace X_op_religion_import = 0 if temp_religion_import == "Not at all important" 
replace X_op_religion_import = 1 if temp_religion_import == "Not very important"
replace X_op_religion_import = 2 if temp_religion_import == "Rather important"
replace X_op_religion_import = 3 if temp_religion_import == "Very important"
checkDistinct X_op_religion_import 4

//State of Health (Subjective)
//codebook A009
decode A009, gen (temp_health)
gen X_dem_state_health = .
replace X_dem_state_health = 0 if temp_health == "Very poor"
replace X_dem_state_health = 1 if temp_health == "Poor"
replace X_dem_state_health = 2 if temp_health == "Fair"
replace X_dem_state_health = 3 if temp_health == "Good"
replace X_dem_state_health = 4 if temp_health == "Very good"
checkDistinct X_dem_state_health 5

//Important child qualities: List from which respondent picks up to 5
//Important child qualities: Good manners
//codebook A027
decode A027, gen (temp_child_manners)
gen X_op_child_manners_import = .
replace X_op_child_manners_import = 0 if temp_child_manners == "Not mentioned"
replace X_op_child_manners_import = 1 if temp_child_manners == "Important"
checkDistinct X_op_child_manners_import 2

//Important child qualities: Independence
//codebook A029
decode A029, gen (temp_child_indep)
gen X_op_child_indep_import = .
replace X_op_child_indep_import = 0 if temp_child_indep == "Not mentioned"
replace X_op_child_indep_import = 1 if temp_child_indep == "Important"
checkDistinct X_op_child_indep_import 2

//Important child qualities: Hard work
//codebook A030
decode A030, gen (temp_child_hardwork)
gen X_op_child_work_import = .
replace X_op_child_work_import = 0 if temp_child_hardwork == "Not mentioned"
replace X_op_child_work_import = 1 if temp_child_hardwork == "Important"
checkDistinct X_op_child_work_import 2

//Important child qualities: Feeling of responsibility
//codebook A032
decode A032, gen (temp_child_resp)
gen X_op_child_resp_import = .
replace X_op_child_resp_import = 0 if temp_child_resp == "Not mentioned"
replace X_op_child_resp_import = 1 if temp_child_resp == "Important"
checkDistinct X_op_child_resp_import 2

//Important child qualities: Imagination
//codebook A034
decode A034, gen (temp_child_imag)
gen X_op_child_imag_import = .
replace X_op_child_imag_import = 0 if temp_child_imag == "Not mentioned"
replace X_op_child_imag_import = 1 if temp_child_imag == "Important"
checkDistinct X_op_child_imag_import 2

//Impportant child qualities: Tolerance and respect for other people
//codebook A035
decode A035, gen (temp_child_tol)
gen X_op_child_tol_import = .
replace X_op_child_tol_import = 0 if temp_child_tol == "Not mentioned"
replace X_op_child_tol_import = 1 if temp_child_tol == "Important"
checkDistinct X_op_child_tol_import 2

//Important child qualities: Thrift, saving money and things
//codebook A038
decode A038, gen (temp_child_thrift)
gen X_op_child_thrift_import = .
replace X_op_child_thrift_import = 0 if temp_child_thrift == "Not mentioned"
replace X_op_child_thrift_import = 1 if temp_child_thrift == "Important"
checkDistinct X_op_child_thrift_import 2

//Important child qualities: Determination and perseverance
//Codebook A039
decode A039, gen (temp_child_persev)
gen X_op_child_persev_import = .
replace X_op_child_persev_import = 0 if temp_child_persev == "Not mentioned"
replace X_op_child_persev_import = 1 if temp_child_persev == "Important"
checkDistinct X_op_child_persev_import 2

//Important child qualities: Religious Faith
//Codebook A040
decode A040, gen (temp_child_faith)
gen X_op_child_faith_import = .
replace X_op_child_faith_import = 0 if temp_child_faith == "Not mentioned"
replace X_op_child_faith_import = 1 if temp_child_faith == "Important"
checkDistinct X_op_child_faith_import 2

//Important child qualities: Unselfishness
//Codebook A041
decode A041, gen (temp_child_unself)
gen X_op_child_unself_import = .
replace X_op_child_unself_import = 0 if temp_child_unself == "Not mentioned"
replace X_op_child_unself_import = 1 if temp_child_unself == "Important"
checkDistinct X_op_child_unself_import 2

//Important child qualities: Obedience
//Codebook A042
decode A042, gen (temp_child_obed)
gen X_op_child_obed_import = .
replace X_op_child_obed_import = 0 if temp_child_obed == "Not mentioned"
replace X_op_child_obed_import = 1 if temp_child_obed == "Important"
checkDistinct X_op_child_obed_import 2

//Don't want as neighbors: heavy drinkers
//Codebook A124_03
decode A124_03, gen (temp_neighbors_drinkers)
gen X_op_neighbors_drinkers = .
replace X_op_neighbors_drinkers = 0 if temp_neighbors_drinkers=="Not mentioned"
replace X_op_neighbors_drinkers = 1 if temp_neighbors_drinkers=="Mentioned"
checkDistinct X_op_neighbors_drinkers 2
 
//Don't want as neighbors: people who have AIDS
//Codebook A124_07
decode A124_07, gen (temp_neighbors_AIDS)
gen X_op_neighbors_AIDS = .
replace X_op_neighbors_AIDS = 0 if temp_neighbors_AIDS=="Not mentioned"
replace X_op_neighbors_AIDS = 1 if temp_neighbors_AIDS=="Mentioned"
checkDistinct X_op_neighbors_AIDS 2

//Don't want as neighbors: drug addicts
//Codebook A124_08
decode A124_08, gen (temp_neighbors_drugs)
gen X_op_neighbors_drugs = .
replace X_op_neighbors_drugs = 0 if temp_neighbors_drugs=="Not mentioned"
replace X_op_neighbors_drugs = 1 if temp_neighbors_drugs=="Mentioned"
checkDistinct X_op_neighbors_drugs 2

//Don't want as neighbors: people of a different religion
//Codebook A124_12
decode A124_12, gen (temp_neighbors_dif_religion)
gen X_op_neighbors_dif_religion = .
replace X_op_neighbors_dif_religion = 0 if temp_neighbors_dif_religion=="Not mentioned"
replace X_op_neighbors_dif_religion = 1 if temp_neighbors_dif_religion=="Mentioned"
checkDistinct X_op_neighbors_dif_religion 2

// Don't want: Neighbors of a different race
decode A124_02, gen (temp_neighbors_dif_race)
gen X_op_neighbors_diff_race = .
replace X_op_neighbors_diff_race = 0 if temp_neighbors_dif_race == "Not mentioned"
replace X_op_neighbors_diff_race = 1 if temp_neighbors_dif_race == "Mentioned" 
checkDistinct X_op_neighbors_diff_race 2

// Dont want: Neighbors that are immigrants/foreigners 
decode A124_06, gen (temp_neighbors_immigrants)
gen X_op_neighbors_immigrants = .
replace X_op_neighbors_immigrants = 0 if temp_neighbors_immigrants == "Not mentioned"
replace X_op_neighbors_immigrants = 1 if temp_neighbors_immigrants == "Mentioned"
checkDistinct X_op_neighbors_immigrants 2

//Neighbors: Unmarried couples living together
//Codebook A124_42
decode A124_42, gen (temp_neighbors_unmarried)
gen X_op_neighbors_unmarried = .
replace X_op_neighbors_unmarried = 0 if temp_neighbors_unmarried == "Not mentioned"
replace X_op_neighbors_unmarried = 1 if temp_neighbors_unmarried == "Mentioned"
checkDistinct X_op_neighbors_unmarried 2

// Don't want: Neighbors who are homosexual
decode A124_09, gen (temp_neighbors_homo)
gen X_op_neighbors_homo = .
replace X_op_neighbors_homo = 0 if temp_neighbors_homo == "Not mentioned"
replace X_op_neighbors_homo = 1 if temp_neighbors_homo == "Mentioned"
checkDistinct X_op_neighbors_homo 2

//Satisfaction with financial situation of household
//Codebook C006
decode C006, gen (temp_satis_financial)
gen X_op_finan_satis = .
replace X_op_finan_satis = 0 if temp_satis_financial=="Dissatisfied"
replace X_op_finan_satis = 1 if temp_satis_financial=="2"
replace X_op_finan_satis = 2 if temp_satis_financial=="3"
replace X_op_finan_satis = 3 if temp_satis_financial=="4"
replace X_op_finan_satis = 4 if temp_satis_financial=="5"
replace X_op_finan_satis = 5 if temp_satis_financial=="6"
replace X_op_finan_satis = 6 if temp_satis_financial=="7"
replace X_op_finan_satis = 7 if temp_satis_financial=="8"
replace X_op_finan_satis = 8 if temp_satis_financial=="9"
replace X_op_finan_satis = 9 if temp_satis_financial=="Satisfied"
checkDistinct X_op_finan_satis 10

//Work is a duty towards society
//Codebook C039
decode C039, gen (temp_work_duty)
gen X_op_work_duty = .
replace X_op_work_duty = 0 if temp_work_duty=="Strongly disagree"
replace X_op_work_duty = 1 if temp_work_duty=="Disagree"
replace X_op_work_duty = 2 if temp_work_duty=="Neither agree nor disagree"
replace X_op_work_duty = 3 if temp_work_duty=="Agree"
replace X_op_work_duty = 4 if temp_work_duty=="Strongly agree"
checkDistinct X_op_work_duty 5

//Work should come first even if it means less spare time
//Codebook C041
decode C041, gen (temp_work_first)
gen X_op_work_first = .
replace X_op_work_first = 0 if temp_work_first=="Strongly disagree"
replace X_op_work_first = 1 if temp_work_first=="Disagree"
replace X_op_work_first = 2 if temp_work_first=="Neither agree nor disagree"
replace X_op_work_first = 3 if temp_work_first=="Agree"
replace X_op_work_first = 4 if temp_work_first=="Strongly agree"
checkDistinct X_op_work_first 5

//One of main goals in life has been to make my parents proud
//Codebook D054
decode D054, gen (temp_parents_proud)
gen X_op_parents_proud = .
replace X_op_parents_proud = 0 if temp_parents_proud == "Strongly disagree"
replace X_op_parents_proud = 1 if temp_parents_proud=="Disagree"
replace X_op_parents_proud = 2 if temp_parents_proud=="Agree"
replace X_op_parents_proud = 3 if temp_parents_proud=="Agree strongly"
checkDistinct X_op_parents_proud 4

//Being a housewife is just as fulfilling
//Codebook D057
decode D057, gen (temp_housewife_fulfill)
gen X_op_housewife_fulfill = .
replace X_op_housewife_fulfill = 0 if temp_housewife_fulfill=="Strongly disagree"
replace X_op_housewife_fulfill = 1 if temp_housewife_fulfill=="Disagree"
replace X_op_housewife_fulfill = 2 if temp_housewife_fulfill=="Agree"
replace X_op_housewife_fulfill = 3 if temp_housewife_fulfill=="Agree strongly"
checkDistinct X_op_housewife_fulfill 4

//Men make better political leaders than women do
//Codebook D059
decode D059, gen (temp_men_pol_leaders)
gen X_op_men_pol_leaders = .
replace X_op_men_pol_leaders = 0 if temp_men_pol_leaders=="Strongly disagree"
replace X_op_men_pol_leaders = 1 if temp_men_pol_leaders=="Disagree"
replace X_op_men_pol_leaders = 2 if temp_men_pol_leaders=="Agree"
replace X_op_men_pol_leaders = 3 if temp_men_pol_leaders=="Agree strongly"
checkDistinct X_op_men_pol_leaders 4

//University is more important for a boy than for a girl
//Codebook D060
decode D060, gen (temp_boy_uni)
gen X_op_boy_uni = .
replace X_op_boy_uni = 0 if temp_boy_uni=="Strongly disagree"
replace X_op_boy_uni = 1 if temp_boy_uni=="Disagree"
replace X_op_boy_uni = 2 if temp_boy_uni=="Agree"
replace X_op_boy_uni = 3 if temp_boy_uni=="Agree strongly"
checkDistinct X_op_boy_uni 4

//Pre-school child suffers with a working mother
//Codebook D061
decode D061, gen (temp_preK_work_mom)
gen X_op_preK_work_mom = .
replace X_op_preK_work_mom = 0 if temp_preK_work_mom=="Strongly disagree"
replace X_op_preK_work_mom = 1 if temp_preK_work_mom=="Disagree"
replace X_op_preK_work_mom = 2 if temp_preK_work_mom=="Agree"
replace X_op_preK_work_mom = 3 if temp_preK_work_mom=="Agree strongly"
checkDistinct X_op_preK_work_mom 4

//Men make better business executives than women do
//Codebook D078
decode D078, gen (temp_men_business)
gen X_op_men_business = .
replace X_op_men_business = 0 if temp_men_business=="Strongly disagree"
replace X_op_men_business = 1 if temp_men_business=="Disagree"
replace X_op_men_business = 2 if temp_men_business=="Agree"
replace X_op_men_business = 3 if temp_men_business=="Agree strongly"
checkDistinct X_op_men_business 4

//Willingness to fight for country
//Codebook E012
decode E012, gen (temp_willing_fight)
gen X_op_willing_fight = .
replace X_op_willing_fight = 0 if temp_willing_fight=="No"
replace X_op_willing_fight = 1 if temp_willing_fight=="Yes"
checkDistinct X_op_willing_fight 2

//Future changes: Less immportance placed on work
//Codebook E015
decode E015, gen (temp_work_less_import)
gen X_op_work_less_import = .
replace X_op_work_less_import = 0 if temp_work_less_import=="Bad thing"
replace X_op_work_less_import = 1 if temp_work_less_import=="Don´t mind"
replace X_op_work_less_import = 2 if temp_work_less_import=="Good thing"
checkDistinct X_op_work_less_import 3

//Future changes: More emphasis on technology
//Codebook E016
decode E016, gen (temp_more_tech)
gen X_op_more_tech = .
replace X_op_more_tech = 0 if temp_more_tech=="Bad thing"
replace X_op_more_tech = 1 if temp_more_tech=="Don´t mind"
replace X_op_more_tech = 2 if temp_more_tech=="Good thing"
checkDistinct X_op_more_tech 3

//Interest in politics
//Codebook E023
decode E023, gen (temp_pol_interest)
gen X_op_pol_interest = .
replace X_op_pol_interest = 0 if temp_pol_interest=="Not at all interested"
replace X_op_pol_interest = 1 if temp_pol_interest=="Not very interested"
replace X_op_pol_interest = 2 if temp_pol_interest=="Somewhat interested"
replace X_op_pol_interest = 3 if temp_pol_interest=="Very interested"
checkDistinct X_op_pol_interest 4

//Basic kinds of attitudes concerning society
//Codebook E034
decode E034, gen (temp_society_attitude)

gen X_op_society_radically_changed = .
replace X_op_society_radically_changed = 0 if temp_society_attitude=="Society must be gradually improved by reforms" | temp_society_attitude=="Society must be valiantly defended"
replace X_op_society_radically_changed = 1 if temp_society_attitude=="Society must be radically changed"
checkDistinct X_op_society_radically_changed 2

gen X_op_society_valiantly_defended = .
replace X_op_society_valiantly_defended = 0 if temp_society_attitude=="Society must be gradually improved by reforms" | temp_society_attitude=="Society must be radically changed"
replace X_op_society_valiantly_defended = 1 if temp_society_attitude=="Society must be valiantly defended"
checkDistinct X_op_society_valiantly_defended 2

//Hard work brings success
//Codebook E040
decode E040, gen (temp_work_brings_success)
gen X_op_work_brings_success = .
replace X_op_work_brings_success = 0 if temp_work_brings_success=="Success is more a matter of luck and connections"
replace X_op_work_brings_success = 1 if temp_work_brings_success=="9"
replace X_op_work_brings_success = 2 if temp_work_brings_success=="8"
replace X_op_work_brings_success = 3 if temp_work_brings_success=="7"
replace X_op_work_brings_success = 4 if temp_work_brings_success=="6"
replace X_op_work_brings_success = 5 if temp_work_brings_success=="5"
replace X_op_work_brings_success = 6 if temp_work_brings_success=="4"
replace X_op_work_brings_success = 7 if temp_work_brings_success=="3"
replace X_op_work_brings_success = 8 if temp_work_brings_success=="2"
replace X_op_work_brings_success = 9 if temp_work_brings_success=="In the long run, hard work usually brings a better life"
checkDistinct X_op_work_brings_success 10

// Confidence in the press - higher value = more confidence in press
decode E069_04, gen (temp_confid_press)
gen X_op_confidence_press = .
replace X_op_confidence_press = 0 if temp_confid_press == "None at all"
replace X_op_confidence_press = 1 if temp_confid_press == "Not very much"
replace X_op_confidence_press = 2 if temp_confid_press == "Quite a lot"
replace X_op_confidence_press = 3 if temp_confid_press == "A great deal"
checkDistinct X_op_confidence_press 4

// Confidence in labor unions - higher value = more confidence
decode E069_05, gen (temp_confid_unions)
gen X_op_confidence_unions = .
replace X_op_confidence_unions = 0 if temp_confid_unions == "None at all"
replace X_op_confidence_unions = 1 if temp_confid_unions == "Not very much"
replace X_op_confidence_unions = 2 if temp_confid_unions == "Quite a lot"
replace X_op_confidence_unions = 3 if temp_confid_unions == "A great deal"
checkDistinct X_op_confidence_unions 4

// Confidence in the police - higher value = more confidence
decode E069_06, gen (temp_confid_police)
gen X_op_confidence_police = .
replace X_op_confidence_police = 0 if temp_confid_police == "None at all"
replace X_op_confidence_police = 1 if temp_confid_police == "Not very much"
replace X_op_confidence_police = 2 if temp_confid_police == "Quite a lot"
replace X_op_confidence_police = 3 if temp_confid_police == "A great deal"
checkDistinct X_op_confidence_police 4

// Confidence in Parliament - higher value = more confidence 
decode E069_07, gen(temp_confid_parl)
gen X_op_confidence_parliament = .
replace X_op_confidence_parliament = 0 if temp_confid_parl == "None at all"
replace X_op_confidence_parliament = 1 if temp_confid_parl == "Not very much"
replace X_op_confidence_parliament = 2 if temp_confid_parl == "Quite a lot"
replace X_op_confidence_parliament = 3 if temp_confid_parl == "A great deal"
checkDistinct X_op_confidence_parliament 4

// Confidence in the political parties - higher value = more confidence
decode E069_12, gen (temp_confid_pol_parties)
gen X_op_confidence_polparties = .
replace X_op_confidence_polparties = 0 if temp_confid_pol_parties == "None at all"
replace X_op_confidence_polparties = 1 if temp_confid_pol_parties == "Not very much"
replace X_op_confidence_polparties = 2 if temp_confid_pol_parties == "Quite a lot"
replace X_op_confidence_polparties = 3 if temp_confid_pol_parties == "A great deal"
checkDistinct X_op_confidence_polparties 4

// Confidence in the courts 
decode E069_17, gen (temp_confid_courts)
gen X_op_confidence_courts = .
replace X_op_confidence_courts = 0 if temp_confid_courts == "None at all"
replace X_op_confidence_courts = 1 if temp_confid_courts == "Not very much"
replace X_op_confidence_courts = 2 if temp_confid_courts == "Quite a lot"
replace X_op_confidence_courts = 3 if temp_confid_courts == "A great deal"
checkDistinct X_op_confidence_courts 4

// Confidence in the EU
decode E069_18, gen (temp_confid_EU)
gen X_op_confidence_EU = .
replace X_op_confidence_EU = 0 if temp_confid_EU == "None at all"
replace X_op_confidence_EU = 1 if temp_confid_EU == "Not very much"
replace X_op_confidence_EU = 2 if temp_confid_EU == "Quite a lot"
replace X_op_confidence_EU = 3 if temp_confid_EU == "A great deal"
checkDistinct X_op_confidence_EU 4

replace X_op_confidence_EU = 0 if BB_Country == "US"

// Confidence in the UN 
decode E069_20, gen (temp_confid_UN)
gen X_op_confidence_UN = .
replace X_op_confidence_UN = 0 if temp_confid_UN == "None at all"
replace X_op_confidence_UN = 1 if temp_confid_UN == "Not very much"
replace X_op_confidence_UN = 2 if temp_confid_UN == "Quite a lot"
replace X_op_confidence_UN = 3 if temp_confid_UN == "A great deal"
checkDistinct X_op_confidence_UN 4

// Confidence in govt Index - higher value = more confidence
decode E069_11, gen (temp_confid_gov)
gen X_op_confidence_in_govt = .
replace X_op_confidence_in_govt = 0 if temp_confid_gov == "None at all" 
replace X_op_confidence_in_govt = 1 if temp_confid_gov == "Not very much"
replace X_op_confidence_in_govt = 2 if temp_confid_gov == "Quite a lot"
replace X_op_confidence_in_govt = 3 if temp_confid_gov == "A great deal"
checkDistinct X_op_confidence_in_govt 4

//Confidence: Churches
//Codebook E069_01
decode E069_01, gen (temp_church_confid)
gen X_op_confidence_church = .
replace X_op_confidence_church = 0 if temp_church_confid=="None at all"
replace X_op_confidence_church = 1 if temp_church_confid=="Not very much"
replace X_op_confidence_church = 2 if temp_church_confid=="Quite a lot"
replace X_op_confidence_church = 3 if temp_church_confid=="A great deal"
checkDistinct X_op_confidence_church 4

//Confidence: Armed Forces
//Codebook E069_02
decode E069_02, gen (temp_army_confid)
gen X_op_confidence_army = .
replace X_op_confidence_army = 0 if temp_army_confid=="None at all"
replace X_op_confidence_army = 1 if temp_army_confid=="Not very much"
replace X_op_confidence_army = 2 if temp_army_confid=="Quite a lot"
replace X_op_confidence_army = 3 if temp_army_confid=="A great deal"
checkDistinct X_op_confidence_army 4

//Confidence: The Civil Services
//Codebook E069_08
decode E069_08, gen (temp_civil_services_confid)
gen X_op_confidence_civil_services = .
replace X_op_confidence_civil_services = 0 if temp_civil_services_confid=="None at all"
replace X_op_confidence_civil_services = 1 if temp_civil_services_confid=="Not very much"
replace X_op_confidence_civil_services = 2 if temp_civil_services_confid=="Quite a lot"
replace X_op_confidence_civil_services = 3 if temp_civil_services_confid=="A great deal"
checkDistinct X_op_confidence_civil_services 4

//Confidence: Television
//Codebook E069_10
decode E069_10, gen (temp_tv_confid)
gen X_op_confidence_tv = .
replace X_op_confidence_tv = 0 if temp_tv_confid=="None at all"
replace X_op_confidence_tv = 1 if temp_tv_confid=="Not very much"
replace X_op_confidence_tv = 2 if temp_tv_confid=="Quite a lot"
replace X_op_confidence_tv = 3 if temp_tv_confid=="A great deal"
checkDistinct X_op_confidence_tv 4

//Confidence: Major companies
//Codebook E069_13
decode E069_13, gen (temp_company_confid)
gen X_op_confidence_big_company = .
replace X_op_confidence_big_company = 0 if temp_company_confid=="None at all"
replace X_op_confidence_big_company = 1 if temp_company_confid=="Not very much"
replace X_op_confidence_big_company = 2 if temp_company_confid=="Quite a lot"
replace X_op_confidence_big_company = 3 if temp_company_confid=="A great deal"
checkDistinct X_op_confidence_big_company 4

//Confidence: The environmental protection movement
//Codebook E069_14
decode E069_14, gen (temp_environ_confid)
gen X_op_confidence_environ = .
replace X_op_confidence_environ = 0 if temp_environ_confid=="None at all"
replace X_op_confidence_environ = 1 if temp_environ_confid=="Not very much"
replace X_op_confidence_environ = 2 if temp_environ_confid=="Quite a lot"
replace X_op_confidence_environ = 3 if temp_environ_confid=="A great deal"
checkDistinct X_op_confidence_environ 4

//Confidence: The women's movement
//Codebook E069_15
decode E069_15, gen (temp_women_mvt_confid)
gen X_op_confidence_women_mvt = .
replace X_op_confidence_women_mvt = 0 if temp_women_mvt_confid=="None at all"
replace X_op_confidence_women_mvt = 1 if temp_women_mvt_confid=="Not very much"
replace X_op_confidence_women_mvt = 2 if temp_women_mvt_confid=="Quite a lot"
replace X_op_confidence_women_mvt = 3 if temp_women_mvt_confid=="A great deal"
checkDistinct X_op_confidence_women_mvt 4

//Confidence: NATO
//Codebook E069_19
decode E069_19, gen (temp_nato_confid)
gen X_op_confidence_NATO = .
replace X_op_confidence_NATO = 0 if temp_nato_confid=="None at all"
replace X_op_confidence_NATO = 1 if temp_nato_confid=="Not very much"
replace X_op_confidence_NATO = 2 if temp_nato_confid=="Quite a lot"
replace X_op_confidence_NATO = 3 if temp_nato_confid=="A great deal"
checkDistinct X_op_confidence_NATO 4

//Confidence: The NAFTA
//Codebook E069_24
decode E069_24, gen (temp_NAFTA_confid)
gen X_op_confidence_NAFTA = .
replace X_op_confidence_NAFTA = 0 if temp_NAFTA_confid=="None at all"
replace X_op_confidence_NAFTA = 1 if temp_NAFTA_confid=="Not very much"
replace X_op_confidence_NAFTA = 2 if temp_NAFTA_confid=="Quite a lot"
replace X_op_confidence_NAFTA = 3 if temp_NAFTA_confid=="A great deal"
checkDistinct X_op_confidence_NAFTA 4

/* Note the following were not asked, dropping: Confidence: The SAARC, The Mercosur, The Association of South East Asian Nations ASEAN,
The Arab League, The Free Commerce Treaty (Tratado de libre comercio), CIS*/
drop E069_21 E069_22 E069_26 E069_27 E069_30 E069_43

//Confidence: The APEC
//Codebook E069_29
decode E069_29, gen (temp_APEC_confid)
gen X_op_confidence_APEC = .
replace X_op_confidence_APEC = 0 if temp_APEC_confid=="None at all"
replace X_op_confidence_APEC = 1 if temp_APEC_confid=="Not very much"
replace X_op_confidence_APEC = 2 if temp_APEC_confid=="Quite a lot"
replace X_op_confidence_APEC = 3 if temp_APEC_confid=="A great deal"
checkDistinct X_op_confidence_APEC 4

//Confidence: Charitable or humanitarian organizations
//Codebook E069_40
decode E069_40, gen (temp_charit_org_confid)
gen X_op_confidence_charit_org = .
replace X_op_confidence_charit_org = 0 if temp_charit_org_confid=="None at all"
replace X_op_confidence_charit_org = 1 if temp_charit_org_confid=="Not very much"
replace X_op_confidence_charit_org = 2 if temp_charit_org_confid=="Quite a lot"
replace X_op_confidence_charit_org = 3 if temp_charit_org_confid=="A great deal"
checkDistinct X_op_confidence_charit_org 4

//Confidence: Banks
//Codebook E069_41
decode E069_41, gen (temp_banks_confid)
gen X_op_confidence_banks = .
replace X_op_confidence_banks = 0 if temp_banks_confid=="None at all"
replace X_op_confidence_banks = 1 if temp_banks_confid=="Not very much"
replace X_op_confidence_banks = 2 if temp_banks_confid=="Quite a lot"
replace X_op_confidence_banks = 3 if temp_banks_confid=="A great deal"
checkDistinct X_op_confidence_banks 4

//Confidence: Universities
//Codebook E069_54
decode E069_54, gen (temp_unis_confid)
gen X_op_confidence_unis = .
replace X_op_confidence_unis = 0 if temp_unis_confid=="None at all"
replace X_op_confidence_unis = 1 if temp_unis_confid=="Not very much"
replace X_op_confidence_unis = 2 if temp_unis_confid=="Quite a lot"
replace X_op_confidence_unis = 3 if temp_unis_confid=="A great deal"
checkDistinct X_op_confidence_unis 4

//Political system: Having the army rule
//Codebook E116
decode E116, gen (temp_army_rule)
gen X_op_army_rule = .
replace X_op_army_rule = 0 if temp_army_rule=="Very bad"
replace X_op_army_rule = 1 if temp_army_rule=="Fairly bad"
replace X_op_army_rule = 2 if temp_army_rule=="Fairly good"
replace X_op_army_rule = 3 if temp_army_rule=="Very good"
checkDistinct X_op_army_rule 4

//Political system: Having a democratic political system
//Codebook E117
decode E117, gen (temp_democracy)
gen X_op_democracy = .
replace X_op_democracy = 0 if temp_democracy=="Very bad"
replace X_op_democracy = 1 if temp_democracy=="Fairly bad"
replace X_op_democracy = 2 if temp_democracy=="Fairly good"
replace X_op_democracy = 3 if temp_democracy=="Very good"
checkDistinct X_op_democracy 4

//Respect for individual human rights nowadays
//Codebook E124
decode E124, gen (temp_resp_human_rights)
gen X_op_resp_human_rights = .
replace X_op_resp_human_rights = 0 if temp_resp_human_rights=="There is no respect at all"
replace X_op_resp_human_rights = 1 if temp_resp_human_rights=="There is not much respect"
replace X_op_resp_human_rights = 2 if temp_resp_human_rights=="There is some respect"
replace X_op_resp_human_rights = 3 if temp_resp_human_rights=="There is a lot of respect for individual human rights"
*more than 10% missing in a given wave
checkDistinct X_op_resp_human_rights 4

//Science and technology are making our lives healthier, easier, and more comfortable
//Codebook E217
decode E217, gen (temp_science_life_better)
gen X_op_science_life_better = .
replace X_op_science_life_better = 0 if temp_science_life_better=="Completely disagree"
replace X_op_science_life_better = 1 if temp_science_life_better=="2"
replace X_op_science_life_better = 2 if temp_science_life_better=="3"
replace X_op_science_life_better = 3 if temp_science_life_better=="4"
replace X_op_science_life_better = 4 if temp_science_life_better=="5"
replace X_op_science_life_better = 5 if temp_science_life_better=="6"
replace X_op_science_life_better = 6 if temp_science_life_better=="7"
replace X_op_science_life_better = 7 if temp_science_life_better=="8"
replace X_op_science_life_better = 8 if temp_science_life_better=="9"
replace X_op_science_life_better = 9 if temp_science_life_better=="Completely agree"
*more than 10% missing in a given wave (3,11)
checkDistinct X_op_science_life_better 10

//Because of science and technology, there will be more opportunities for the next generation
//Codebook E218
decode E218, gen (temp_science_opps)
gen X_op_science_opps = .
replace X_op_science_opps = 0 if temp_science_opps=="Completely disagree"
replace X_op_science_opps = 1 if temp_science_opps=="2"
replace X_op_science_opps = 2 if temp_science_opps=="3"
replace X_op_science_opps = 3 if temp_science_opps=="4"
replace X_op_science_opps = 4 if temp_science_opps=="5"
replace X_op_science_opps = 5 if temp_science_opps=="6"
replace X_op_science_opps = 6 if temp_science_opps=="7"
replace X_op_science_opps = 7 if temp_science_opps=="8"
replace X_op_science_opps = 8 if temp_science_opps=="9"
replace X_op_science_opps = 9 if temp_science_opps=="Completely agree"
*more than 10% missing in a given wave (3,11)
checkDistinct X_op_science_opps 10

//We depend too much on science and not enough on faith
//Codebook E220
decode E220, gen (temp_too_science)
gen X_op_too_science = .
replace X_op_too_science = 0 if temp_too_science=="Completely disagree"
replace X_op_too_science = 1 if temp_too_science=="2"
replace X_op_too_science = 2 if temp_too_science=="3"
replace X_op_too_science = 3 if temp_too_science=="4"
replace X_op_too_science = 4 if temp_too_science=="5"
replace X_op_too_science = 5 if temp_too_science=="6"
replace X_op_too_science = 6 if temp_too_science=="7"
replace X_op_too_science = 7 if temp_too_science=="8"
replace X_op_too_science = 8 if temp_too_science=="9"
replace X_op_too_science = 9 if temp_too_science=="Completely agree"
*more than 10% missing in a given wave (3,11)
checkDistinct X_op_too_science 10

//Democracy: Religious authorities interpret the laws
//Codebook E225
decode E225, gen (temp_relig_law)
gen X_op_relig_law = .
replace X_op_relig_law = 0 if E225 == 0
replace X_op_relig_law = 1 if temp_relig_law=="Not an essential characteristic of democracy"
replace X_op_relig_law = 2 if temp_relig_law=="2"
replace X_op_relig_law = 3 if temp_relig_law=="3"
replace X_op_relig_law = 4 if temp_relig_law=="4"
replace X_op_relig_law = 5 if temp_relig_law=="5"
replace X_op_relig_law = 6 if temp_relig_law=="6"
replace X_op_relig_law = 7 if temp_relig_law=="7"
replace X_op_relig_law = 8 if temp_relig_law=="8"
replace X_op_relig_law = 9 if temp_relig_law=="9"
replace X_op_relig_law = 10 if temp_relig_law=="An essential characteristic of democracy"
*more than 10% missing in a given wave (3,11)
checkDistinct X_op_relig_law 11

//Democracy: People choose their leaders in free elections
//Codebook E226
decode E226, gen (temp_free_elections)
gen X_op_free_elections = .
replace X_op_free_elections = 0 if E226 == 0
replace X_op_free_elections = 1 if temp_free_elections=="Not an essential characteristic of democracy"
replace X_op_free_elections = 2 if temp_free_elections=="2"
replace X_op_free_elections = 3 if temp_free_elections=="3"
replace X_op_free_elections = 4 if temp_free_elections=="4"
replace X_op_free_elections = 5 if temp_free_elections=="5"
replace X_op_free_elections = 6 if temp_free_elections=="6"
replace X_op_free_elections = 7 if temp_free_elections=="7"
replace X_op_free_elections = 8 if temp_free_elections=="8"
replace X_op_free_elections = 9 if temp_free_elections=="9"
replace X_op_free_elections = 10 if temp_free_elections=="An essential characteristic of democracy"
*more than 10% missing in a given wave (3,11)
checkDistinct X_op_free_elections 11

//Democracy: People receive state aid for unenmployment
//Codebook E227
decode E227, gen (temp_aid_unemploy)
gen X_op_aid_unemploy = .
replace X_op_aid_unemploy = 0 if E227 == 0
replace X_op_aid_unemploy = 1 if temp_aid_unemploy=="Not an essential characteristic of democracy"
replace X_op_aid_unemploy = 2 if temp_aid_unemploy=="2"
replace X_op_aid_unemploy = 3 if temp_aid_unemploy=="3"
replace X_op_aid_unemploy = 4 if temp_aid_unemploy=="4"
replace X_op_aid_unemploy = 5 if temp_aid_unemploy=="5"
replace X_op_aid_unemploy = 6 if temp_aid_unemploy=="6"
replace X_op_aid_unemploy = 7 if temp_aid_unemploy=="7"
replace X_op_aid_unemploy = 8 if temp_aid_unemploy=="8"
replace X_op_aid_unemploy = 9 if temp_aid_unemploy=="9"
replace X_op_aid_unemploy = 10 if temp_aid_unemploy=="An essential characteristic of democracy"
*more than 10% missing in a given wave (3,11)
checkDistinct X_op_aid_unemploy 11

//Democracy: The army takes over when government is incompetent
//Codebook E228
decode E228, gen (temp_army_takeover)
gen X_op_army_takeover = .
replace X_op_army_takeover = 0 if E228 == 0
replace X_op_army_takeover = 1 if temp_army_takeover=="Not an essential characteristic of democracy"
replace X_op_army_takeover = 2 if temp_army_takeover=="2"
replace X_op_army_takeover = 3 if temp_army_takeover=="3"
replace X_op_army_takeover = 4 if temp_army_takeover=="4"
replace X_op_army_takeover = 5 if temp_army_takeover=="5"
replace X_op_army_takeover = 6 if temp_army_takeover=="6"
replace X_op_army_takeover = 7 if temp_army_takeover=="7"
replace X_op_army_takeover = 8 if temp_army_takeover=="8"
replace X_op_army_takeover = 9 if temp_army_takeover=="9"
replace X_op_army_takeover = 10 if temp_army_takeover=="An essential characteristic of democracy"
*more than 10% missing in a given wave (3,11)
checkDistinct X_op_army_takeover 11

//Democracy: Civil rights protect people's liberty against oppression
//Codebook E229
decode E229, gen (temp_civil_rights_protect)
gen X_op_civil_rights_protect = .
replace X_op_civil_rights_protect = 0 if E229 == 0
replace X_op_civil_rights_protect = 1 if temp_civil_rights_protect=="Not an essential characteristic of democracy"
replace X_op_civil_rights_protect = 2 if temp_civil_rights_protect=="2"
replace X_op_civil_rights_protect = 3 if temp_civil_rights_protect=="3"
replace X_op_civil_rights_protect = 4 if temp_civil_rights_protect=="4"
replace X_op_civil_rights_protect = 5 if temp_civil_rights_protect=="5"
replace X_op_civil_rights_protect = 6 if temp_civil_rights_protect=="6"
replace X_op_civil_rights_protect = 7 if temp_civil_rights_protect=="7"
replace X_op_civil_rights_protect = 8 if temp_civil_rights_protect=="8"
replace X_op_civil_rights_protect = 9 if temp_civil_rights_protect=="9"
replace X_op_civil_rights_protect = 10 if temp_civil_rights_protect=="An essential characteristic of democracy"
*more than 10% missing in a given wave (3,11)
checkDistinct X_op_civil_rights_protect 11

//Democracy: Women have the same rights as men
//Codebook E233
decode E233, gen (temp_women_dem_rights)
gen X_op_women_dem_rights = .
replace X_op_women_dem_rights = 0 if E233 == 0
replace X_op_women_dem_rights = 1 if temp_women_dem_rights=="Not an essential characteristic of democracy"
replace X_op_women_dem_rights = 2 if temp_women_dem_rights=="2"
replace X_op_women_dem_rights = 3 if temp_women_dem_rights=="3"
replace X_op_women_dem_rights = 4 if temp_women_dem_rights=="4"
replace X_op_women_dem_rights = 5 if temp_women_dem_rights=="5"
replace X_op_women_dem_rights = 6 if temp_women_dem_rights=="6"
replace X_op_women_dem_rights = 7 if temp_women_dem_rights=="7"
replace X_op_women_dem_rights = 8 if temp_women_dem_rights=="8"
replace X_op_women_dem_rights = 9 if temp_women_dem_rights=="9"
replace X_op_women_dem_rights = 10 if temp_women_dem_rights=="An essential characteristic of democracy"
*more than 10% missing in a given wave (3,11)
checkDistinct X_op_women_dem_rights 11

//The world is better off, or worse off, because of science and technology
//Codebook E234
decode E234, gen (temp_world_science_better)
gen X_op_world_science_better = . 
replace X_op_world_science_better = 0 if temp_world_science_better=="A lot worse off"
replace X_op_world_science_better = 1 if temp_world_science_better=="2"
replace X_op_world_science_better = 2 if temp_world_science_better=="3"
replace X_op_world_science_better = 3 if temp_world_science_better=="4"
replace X_op_world_science_better = 4 if temp_world_science_better=="5"
replace X_op_world_science_better = 5 if temp_world_science_better=="6"
replace X_op_world_science_better = 6 if temp_world_science_better=="7"
replace X_op_world_science_better = 7 if temp_world_science_better=="8"
replace X_op_world_science_better = 8 if temp_world_science_better=="9"
replace X_op_world_science_better = 9 if temp_world_science_better=="A lot better off"
*more than 10% missing in a given wave (3,11)
checkDistinct X_op_world_science_better 10

//Importance of living in a democracy
//Codebook E235
decode E235, gen (temp_import_live_democracy)
gen X_op_import_live_democracy = .
replace X_op_import_live_democracy = 0 if temp_import_live_democracy=="Not at all important"
replace X_op_import_live_democracy = 1 if temp_import_live_democracy=="2"
replace X_op_import_live_democracy = 2 if temp_import_live_democracy=="3"
replace X_op_import_live_democracy = 3 if temp_import_live_democracy=="4"
replace X_op_import_live_democracy = 4 if temp_import_live_democracy=="5"
replace X_op_import_live_democracy = 5 if temp_import_live_democracy=="6"
replace X_op_import_live_democracy = 6 if temp_import_live_democracy=="7"
replace X_op_import_live_democracy = 7 if temp_import_live_democracy=="8"
replace X_op_import_live_democracy = 8 if temp_import_live_democracy=="9"
replace X_op_import_live_democracy = 9 if temp_import_live_democracy=="Absolutely important"
*more than 10% missing in a given wave (3,11)
checkDistinct X_op_import_live_democracy 10

//Democraticness in own country (how democratically is this country being governed today)
//Codebook E236
decode E236, gen (temp_country_democracy)
gen X_op_country_democracy = .
replace X_op_country_democracy = 0 if temp_country_democracy=="Not at all democratic"
replace X_op_country_democracy = 1 if temp_country_democracy=="2"
replace X_op_country_democracy = 2 if temp_country_democracy=="3"
replace X_op_country_democracy = 3 if temp_country_democracy=="4"
replace X_op_country_democracy = 4 if temp_country_democracy=="5"
replace X_op_country_democracy = 5 if temp_country_democracy=="6"
replace X_op_country_democracy = 6 if temp_country_democracy=="7"
replace X_op_country_democracy = 7 if temp_country_democracy=="8"
replace X_op_country_democracy = 8 if temp_country_democracy=="9"
replace X_op_country_democracy = 9 if temp_country_democracy=="Completely democratic"
*more than 10% missing in a given wave (3,11)
checkDistinct X_op_country_democracy 10

//Believe in: God
//Codebook F050
decode F050, gen (temp_believe_god)
gen X_op_god = .
replace X_op_god = 0 if temp_believe_god=="No"
replace X_op_god = 1 if temp_believe_god=="Yes"
checkDistinct X_op_god 2

//Believe in: Life after death
//Codebook F051
decode F051, gen (temp_believe_lifeaftdeath)
gen X_op_life_after_death = .
replace X_op_life_after_death = 0 if temp_believe_lifeaftdeath=="No"
replace X_op_life_after_death = 1 if temp_believe_lifeaftdeath=="Yes"
checkDistinct X_op_life_after_death 2

//Believe in: Hell
//Codebook F053
decode F053, gen (temp_believe_hell)
gen X_op_believe_hell = .
replace X_op_believe_hell = 0 if temp_believe_hell=="No"
replace X_op_believe_hell = 1 if temp_believe_hell=="Yes"
checkDistinct X_op_believe_hell 2

//Believe in: Heaven
//Codebook F054
decode F054, gen (temp_believe_heaven)
gen X_op_believe_heaven = .
replace X_op_believe_heaven = 0 if temp_believe_heaven=="No"
replace X_op_believe_heaven = 1 if temp_believe_heaven=="Yes"
checkDistinct X_op_believe_heaven 2

//How important is God in your life
//Codebook F063
decode F063, gen (temp_god_import)
gen X_op_god_import = .
replace X_op_god_import = 0 if temp_god_import=="Not at all important"
replace X_op_god_import = 1 if temp_god_import=="2"
replace X_op_god_import = 2 if temp_god_import=="3"
replace X_op_god_import = 3 if temp_god_import=="4"
replace X_op_god_import = 4 if temp_god_import=="5"
replace X_op_god_import = 5 if temp_god_import=="6"
replace X_op_god_import = 6 if temp_god_import=="7"
replace X_op_god_import = 7 if temp_god_import=="8"
replace X_op_god_import = 8 if temp_god_import=="9"
replace X_op_god_import = 9 if temp_god_import=="Very important"
checkDistinct X_op_god_import 10

//Justifiable: Claiming government benefits to which you are not entitled
//Codebook F114A
decode F114A, gen (temp_claim_ben_just)
gen X_op_just_claim_ben = .
replace X_op_just_claim_ben = 0 if temp_claim_ben_just=="Never justifiable"
replace X_op_just_claim_ben = 1 if temp_claim_ben_just=="2"
replace X_op_just_claim_ben = 2 if temp_claim_ben_just=="3"
replace X_op_just_claim_ben = 3 if temp_claim_ben_just=="4"
replace X_op_just_claim_ben = 4 if temp_claim_ben_just=="5"
replace X_op_just_claim_ben = 5 if temp_claim_ben_just=="6"
replace X_op_just_claim_ben = 6 if temp_claim_ben_just=="7"
replace X_op_just_claim_ben = 7 if temp_claim_ben_just=="8"
replace X_op_just_claim_ben = 8 if temp_claim_ben_just=="9"
replace X_op_just_claim_ben = 9 if temp_claim_ben_just=="Always justifiable"
checkDistinct X_op_just_claim_ben 10

//Justifiable: Stealing property
//Codebook F114B
decode F114B, gen (temp_just_steal)
gen X_op_just_steal = .
replace X_op_just_steal = 0 if temp_just_steal=="Never justifiable"
replace X_op_just_steal = 1 if temp_just_steal=="2"
replace X_op_just_steal = 2 if temp_just_steal=="3"
replace X_op_just_steal = 3 if temp_just_steal=="4"
replace X_op_just_steal = 4 if temp_just_steal=="5"
replace X_op_just_steal = 5 if temp_just_steal=="6"
replace X_op_just_steal = 6 if temp_just_steal=="7"
replace X_op_just_steal = 7 if temp_just_steal=="8"
replace X_op_just_steal = 8 if temp_just_steal=="9"
replace X_op_just_steal = 9 if temp_just_steal=="Always justifiable"
checkDistinct X_op_just_steal 10

//Justifiable: Parents beating children
//Codebook F114C
decode F114C, gen (temp_just_parents_beat)
gen X_op_just_parents_beat = .
replace X_op_just_parents_beat = 0 if temp_just_parents_beat=="Never justifiable"
replace X_op_just_parents_beat = 1 if temp_just_parents_beat=="2"
replace X_op_just_parents_beat = 2 if temp_just_parents_beat=="3"
replace X_op_just_parents_beat = 3 if temp_just_parents_beat=="4"
replace X_op_just_parents_beat = 4 if temp_just_parents_beat=="5"
replace X_op_just_parents_beat = 5 if temp_just_parents_beat=="6"
replace X_op_just_parents_beat = 6 if temp_just_parents_beat=="7"
replace X_op_just_parents_beat = 7 if temp_just_parents_beat=="8"
replace X_op_just_parents_beat = 8 if temp_just_parents_beat=="9"
replace X_op_just_parents_beat = 9 if temp_just_parents_beat=="Always justifiable"
checkDistinct X_op_just_parents_beat 10

//Justifiable: Violence against other people
//Codebook F114D
decode F114D, gen (temp_just_violence)
gen X_op_just_violence = .
replace X_op_just_violence = 0 if temp_just_violence=="Never justifiable"
replace X_op_just_violence = 1 if temp_just_violence=="2"
replace X_op_just_violence = 2 if temp_just_violence=="3"
replace X_op_just_violence = 3 if temp_just_violence=="4"
replace X_op_just_violence = 4 if temp_just_violence=="5"
replace X_op_just_violence = 5 if temp_just_violence=="6"
replace X_op_just_violence = 6 if temp_just_violence=="7"
replace X_op_just_violence = 7 if temp_just_violence=="8"
replace X_op_just_violence = 8 if temp_just_violence=="9"
replace X_op_just_violence = 9 if temp_just_violence=="Always justifiable"
checkDistinct X_op_just_violence 10

//Justifiable: Avoiding a fare on public transport
//Codebook F115
decode F115, gen (temp_avoid_fare_just)
gen X_op_just_avoid_fare = .
replace X_op_just_avoid_fare = 0 if temp_avoid_fare_just=="Never justifiable"
replace X_op_just_avoid_fare = 1 if temp_avoid_fare_just=="2"
replace X_op_just_avoid_fare = 2 if temp_avoid_fare_just=="3"
replace X_op_just_avoid_fare = 3 if temp_avoid_fare_just=="4"
replace X_op_just_avoid_fare = 4 if temp_avoid_fare_just=="5"
replace X_op_just_avoid_fare = 5 if temp_avoid_fare_just=="6"
replace X_op_just_avoid_fare = 6 if temp_avoid_fare_just=="7"
replace X_op_just_avoid_fare = 7 if temp_avoid_fare_just=="8"
replace X_op_just_avoid_fare = 8 if temp_avoid_fare_just=="9"
replace X_op_just_avoid_fare = 9 if temp_avoid_fare_just=="Always justifiable"
checkDistinct X_op_just_avoid_fare 10

//Justifiable: Cheating on taxes
//Codebook F116
decode F116, gen (temp_cheat_tax_just)
gen X_op_just_cheat_tax = .
replace X_op_just_cheat_tax = 0 if temp_cheat_tax_just=="Never justifiable"
replace X_op_just_cheat_tax = 1 if temp_cheat_tax_just=="2"
replace X_op_just_cheat_tax = 2 if temp_cheat_tax_just=="3"
replace X_op_just_cheat_tax = 3 if temp_cheat_tax_just=="4"
replace X_op_just_cheat_tax = 4 if temp_cheat_tax_just=="5"
replace X_op_just_cheat_tax = 5 if temp_cheat_tax_just=="6"
replace X_op_just_cheat_tax = 6 if temp_cheat_tax_just=="7"
replace X_op_just_cheat_tax = 7 if temp_cheat_tax_just=="8"
replace X_op_just_cheat_tax = 8 if temp_cheat_tax_just=="9"
replace X_op_just_cheat_tax = 9 if temp_cheat_tax_just=="Always justifiable"
checkDistinct X_op_just_cheat_tax 10

//Justifiable: Someone accepting a bribe
//Codebook F117
decode F117, gen (temp_bribe_just)
gen X_op_just_bribe = .
replace X_op_just_bribe = 0 if temp_bribe_just=="Never justifiable"
replace X_op_just_bribe = 1 if temp_bribe_just=="2"
replace X_op_just_bribe = 2 if temp_bribe_just=="3"
replace X_op_just_bribe = 3 if temp_bribe_just=="4"
replace X_op_just_bribe = 4 if temp_bribe_just=="5"
replace X_op_just_bribe = 5 if temp_bribe_just=="6"
replace X_op_just_bribe = 6 if temp_bribe_just=="7"
replace X_op_just_bribe = 7 if temp_bribe_just=="8"
replace X_op_just_bribe = 8 if temp_bribe_just=="9"
replace X_op_just_bribe = 9 if temp_bribe_just=="Always justifiable"
checkDistinct X_op_just_bribe 10

//Justifiable: Homosexuality
//Codebook F118
decode F118, gen (temp_homo_just)
gen X_op_just_homosex = .
replace X_op_just_homosex = 0 if temp_homo_just=="Never justifiable"
replace X_op_just_homosex = 1 if temp_homo_just=="2"
replace X_op_just_homosex = 2 if temp_homo_just=="3"
replace X_op_just_homosex = 3 if temp_homo_just=="4"
replace X_op_just_homosex = 4 if temp_homo_just=="5"
replace X_op_just_homosex = 5 if temp_homo_just=="6"
replace X_op_just_homosex = 6 if temp_homo_just=="7"
replace X_op_just_homosex = 7 if temp_homo_just=="8"
replace X_op_just_homosex = 8 if temp_homo_just=="9"
replace X_op_just_homosex = 9 if temp_homo_just=="Always justifiable"
checkDistinct X_op_just_homosex 10

//Justifiable: Prostitution
//Codebook F119
decode F119, gen (temp_prostitution_just)
gen X_op_just_prostitution = .
replace X_op_just_prostitution = 0 if temp_prostitution_just=="Never justifiable"
replace X_op_just_prostitution = 1 if temp_prostitution_just=="2"
replace X_op_just_prostitution = 2 if temp_prostitution_just=="3"
replace X_op_just_prostitution = 3 if temp_prostitution_just=="4"
replace X_op_just_prostitution = 4 if temp_prostitution_just=="5"
replace X_op_just_prostitution = 5 if temp_prostitution_just=="6"
replace X_op_just_prostitution = 6 if temp_prostitution_just=="7"
replace X_op_just_prostitution = 7 if temp_prostitution_just=="8"
replace X_op_just_prostitution = 8 if temp_prostitution_just=="9"
replace X_op_just_prostitution = 9 if temp_prostitution_just=="Always justifiable"
checkDistinct X_op_just_prostitution 10

//Justifiable: Abortion
//Codebook F120
decode F120, gen (temp_abortion)
gen X_op_just_abortion = .
replace X_op_just_abortion = 0 if temp_abortion=="Never justifiable"
replace X_op_just_abortion = 1 if temp_abortion=="2"
replace X_op_just_abortion = 2 if temp_abortion=="3"
replace X_op_just_abortion = 3 if temp_abortion=="4"
replace X_op_just_abortion = 4 if temp_abortion=="5"
replace X_op_just_abortion = 5 if temp_abortion=="6"
replace X_op_just_abortion = 6 if temp_abortion=="7"
replace X_op_just_abortion = 7 if temp_abortion=="8"
replace X_op_just_abortion = 8 if temp_abortion=="9"
replace X_op_just_abortion = 9 if temp_abortion=="Always justifiable"
checkDistinct X_op_just_abortion 10

//Justifiable: Divorce
//Codebook F121
decode F121, gen (temp_divorce_just)
gen X_op_just_divorce = .
replace X_op_just_divorce = 0 if temp_divorce_just=="Never justifiable"
replace X_op_just_divorce = 1 if temp_divorce_just=="2"
replace X_op_just_divorce = 2 if temp_divorce_just=="3"
replace X_op_just_divorce = 3 if temp_divorce_just=="4"
replace X_op_just_divorce = 4 if temp_divorce_just=="5"
replace X_op_just_divorce = 5 if temp_divorce_just=="6"
replace X_op_just_divorce = 6 if temp_divorce_just=="7"
replace X_op_just_divorce = 7 if temp_divorce_just=="8"
replace X_op_just_divorce = 8 if temp_divorce_just=="9"
replace X_op_just_divorce = 9 if temp_divorce_just=="Always justifiable"
checkDistinct X_op_just_divorce 10

//Justifiable: Euthanasia
//Codebook F122
decode F122, gen (temp_euthanasia_just)
gen X_op_just_euthanasia = .
replace X_op_just_euthanasia = 0 if temp_euthanasia_just=="Never justifiable"
replace X_op_just_euthanasia = 1 if temp_euthanasia_just=="2"
replace X_op_just_euthanasia = 2 if temp_euthanasia_just=="3"
replace X_op_just_euthanasia = 3 if temp_euthanasia_just=="4"
replace X_op_just_euthanasia = 4 if temp_euthanasia_just=="5"
replace X_op_just_euthanasia = 5 if temp_euthanasia_just=="6"
replace X_op_just_euthanasia = 6 if temp_euthanasia_just=="7"
replace X_op_just_euthanasia = 7 if temp_euthanasia_just=="8"
replace X_op_just_euthanasia = 8 if temp_euthanasia_just=="9"
replace X_op_just_euthanasia = 9 if temp_euthanasia_just=="Always justifiable"
checkDistinct X_op_just_euthanasia 10

//Justifiable: Suicide
//Codebook F123
decode F123, gen (temp_suicide_just)
gen X_op_just_suicide = .
replace X_op_just_suicide = 0 if temp_suicide_just=="Never justifiable"
replace X_op_just_suicide = 1 if temp_suicide_just=="2"
replace X_op_just_suicide = 2 if temp_suicide_just=="3"
replace X_op_just_suicide = 3 if temp_suicide_just=="4"
replace X_op_just_suicide = 4 if temp_suicide_just=="5"
replace X_op_just_suicide = 5 if temp_suicide_just=="6"
replace X_op_just_suicide = 6 if temp_suicide_just=="7"
replace X_op_just_suicide = 7 if temp_suicide_just=="8"
replace X_op_just_suicide = 8 if temp_suicide_just=="9"
replace X_op_just_suicide = 9 if temp_suicide_just=="Always justifiable"
checkDistinct X_op_just_suicide 10

//Justifiable: Sex before marriage
//Codebook F135A
decode F135A, gen (temp_just_sex)
gen X_op_just_sex = .
replace X_op_just_sex = 0 if temp_just_sex=="Never justifiable"
replace X_op_just_sex = 1 if temp_just_sex=="2"
replace X_op_just_sex = 2 if temp_just_sex=="3"
replace X_op_just_sex = 3 if temp_just_sex=="4"
replace X_op_just_sex = 4 if temp_just_sex=="5"
replace X_op_just_sex = 5 if temp_just_sex=="6"
replace X_op_just_sex = 6 if temp_just_sex=="7"
replace X_op_just_sex = 7 if temp_just_sex=="8"
replace X_op_just_sex = 8 if temp_just_sex=="9"
replace X_op_just_sex = 9 if temp_just_sex=="Always justifiable"
checkDistinct X_op_just_sex 10

//Justifiable: For a man to beat his wife
//Codebook F199
decode F199, gen (temp_just_wife_beat)
gen X_op_just_wife_beat = .
replace X_op_just_wife_beat = 0 if temp_just_wife_beat=="Never justifiable"
replace X_op_just_wife_beat = 1 if temp_just_wife_beat=="2"
replace X_op_just_wife_beat = 2 if temp_just_wife_beat=="3"
replace X_op_just_wife_beat = 3 if temp_just_wife_beat=="4"
replace X_op_just_wife_beat = 4 if temp_just_wife_beat=="5"
replace X_op_just_wife_beat = 5 if temp_just_wife_beat=="6"
replace X_op_just_wife_beat = 6 if temp_just_wife_beat=="7"
replace X_op_just_wife_beat = 7 if temp_just_wife_beat=="8"
replace X_op_just_wife_beat = 8 if temp_just_wife_beat=="9"
replace X_op_just_wife_beat = 9 if temp_just_wife_beat=="Always justifiable"
checkDistinct X_op_just_wife_beat 10

//How much you trust: Your family (B)
//Codebook D001_B
decode D001_B, gen (temp_trust_fam)
gen X_op_trust_family = .
replace X_op_trust_family = 0 if temp_trust_fam=="Do not trust at all"
replace X_op_trust_family = 1 if temp_trust_fam=="Do not trust very much"
replace X_op_trust_family = 2 if temp_trust_fam=="Trust somewhat"
replace X_op_trust_family = 3 if temp_trust_fam=="Trust completely"
checkDistinct X_op_trust_family 4

//Trust: Your neighborhood (B)
//Codebook G007_18_B
decode G007_18_B, gen (temp_neighborhood_trust)
gen X_op_neighborhood_trust = .
replace X_op_neighborhood_trust = 0 if temp_neighborhood_trust=="Do not trust at all"
replace X_op_neighborhood_trust = 1 if temp_neighborhood_trust=="Do not trust very much"
replace X_op_neighborhood_trust = 2 if temp_neighborhood_trust=="Trust somewhat"
replace X_op_neighborhood_trust = 3 if temp_neighborhood_trust=="Trust completely"
checkDistinct X_op_neighborhood_trust 4

//Trust: People you know personally (B)
//Codebook G007_33_B
decode G007_33_B, gen (temp_personal_trust)
gen X_op_personal_trust = .
replace X_op_personal_trust = 0 if temp_personal_trust=="Do not trust at all"
replace X_op_personal_trust = 1 if temp_personal_trust=="Do not trust very much"
replace X_op_personal_trust = 2 if temp_personal_trust=="Trust somewhat"
replace X_op_personal_trust = 3 if temp_personal_trust=="Trust completely"
checkDistinct X_op_personal_trust 4

//Trust: People you meet for the first time (B)
//Codebook G007_34_B
decode G007_34_B, gen (temp_first_meet_trust)
gen X_op_first_meet_trust = .
replace X_op_first_meet_trust = 0 if temp_first_meet_trust=="Do not trust at all"
replace X_op_first_meet_trust = 1 if temp_first_meet_trust=="Do not trust very much"
replace X_op_first_meet_trust = 2 if temp_first_meet_trust=="Trust somewhat"
replace X_op_first_meet_trust = 3 if temp_first_meet_trust=="Trust completely"
checkDistinct X_op_first_meet_trust 4

//Trust: People of another religion (B)
//Codebook G007_35_B
decode G007_35_B, gen (temp_dif_religion_trust)
gen X_op_dif_religion_trust = .
replace X_op_dif_religion_trust = 0 if temp_dif_religion_trust=="Do not trust at all"
replace X_op_dif_religion_trust = 1 if temp_dif_religion_trust=="Do not trust very much"
replace X_op_dif_religion_trust = 2 if temp_dif_religion_trust=="Trust somewhat"
replace X_op_dif_religion_trust = 3 if temp_dif_religion_trust=="Trust completely"
checkDistinct X_op_dif_religion_trust 4

//Worries: Losing my job or not finding a job
//Codebook: H006_01
decode H006_01, gen (temp_worries_job)
gen X_op_worries_job = .
replace X_op_worries_job = 0 if temp_worries_job == "Not at all"
replace X_op_worries_job = 1 if temp_worries_job == "Not much"
replace X_op_worries_job = 2 if temp_worries_job == "A great deal"
replace X_op_worries_job = 3 if temp_worries_job == "Very much"
checkDistinct X_op_worries_job 4

//Worries: Not being able to give one's children a good education
//Codebook: H006_02
decode H006_02, gen (temp_worries_child)
gen X_op_worries_child = .
replace X_op_worries_child = 0 if temp_worries_child == "Not at all"
replace X_op_worries_child = 1 if temp_worries_child == "Not much"
replace X_op_worries_child = 2 if temp_worries_child == "A great deal"
replace X_op_worries_child = 3 if temp_worries_child == "Very much"
checkDistinct X_op_worries_child 4

//Worries: A war involving my country
//Codebook: H006_03
decode H006_03, gen (temp_worries_war)
gen X_op_worries_war = .
replace X_op_worries_war = 0 if temp_worries_war == "Not at all"
replace X_op_worries_war = 1 if temp_worries_war == "Not much"
replace X_op_worries_war = 2 if temp_worries_war == "A great deal"
replace X_op_worries_war = 3 if temp_worries_war == "Very much"
checkDistinct X_op_worries_war 4

//Worries: A terrorist attack
//Codebook: H006_04
decode H006_04, gen (temp_worries_terror)
gen X_op_worries_terror = .
replace X_op_worries_terror = 0 if temp_worries_terror == "Not at all"
replace X_op_worries_terror = 1 if temp_worries_terror == "Not much"
replace X_op_worries_terror = 2 if temp_worries_terror == "A great deal"
replace X_op_worries_terror = 3 if temp_worries_terror == "Very much"
checkDistinct X_op_worries_terror 4

//Worries: A civil war
//Codebook: H006_05
decode H006_05, gen (temp_worries_civil)
gen X_op_worries_civil = .
replace X_op_worries_civil = 0 if temp_worries_civil == "Not at all"
replace X_op_worries_civil = 1 if temp_worries_civil == "Not much"
replace X_op_worries_civil = 2 if temp_worries_civil == "A great deal"
replace X_op_worries_civil = 3 if temp_worries_civil == "Very much"
checkDistinct X_op_worries_civil 4

//Evaluate the impact of immigrants on the development of [your country]
//Codebook: G052
decode G052, gen (temp_immigrant_impact)
gen X_op_immigrant_impact = .
replace X_op_immigrant_impact = 0 if temp_immigrant_impact == "Very bad"
replace X_op_immigrant_impact = 1 if temp_immigrant_impact == "Quite bad"
replace X_op_immigrant_impact = 2 if temp_immigrant_impact == "Neither good, nor bad"
replace X_op_immigrant_impact = 3 if temp_immigrant_impact == "Quite good"
replace X_op_immigrant_impact = 4 if temp_immigrant_impact == "Very good"
checkDistinct X_op_immigrant_impact 5


//Removing intermediate variables
drop str_* temp_*

//Saving data
save "$POPULISM_DROPBOX/data/intdata/ivs/full_clean_ivs.dta", replace
// Only respondent identifiers and missingness inputs are consumed by shy-voter analysis.
preserve
keep BB_Id X_op* X_dem*
save "$POPULISM_DROPBOX/data/cleandata/ivs/full_clean_ivs.dta", replace
restore

