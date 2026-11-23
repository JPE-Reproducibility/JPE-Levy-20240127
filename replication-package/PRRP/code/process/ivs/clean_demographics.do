use "$POPULISM_DROPBOX/data/intdata/temp/ivs_clean_with_parlgov.dta", replace

global checkDistinctOn 0

capture program drop checkDistinct
program define checkDistinct
	if ($checkDistinctOn) {
		distinct `1'
		assert r(ndistinct)==`2'
	}
end


// Town size variable - Note that all waves besides EVS Wave 4 and EVS wave 5 has 8 "buckets", but EVS Wave 4 and 5 data only has 5 "buckets". Missing WVS and EVS Wave 1 data (question not asked in survey).

gen str_townsize = string(X049)
gen temp_log_town_size = .

// Use middle value of range based on X049 label
 
replace temp_log_town_size = log(1000) if str_townsize == "1" 
replace temp_log_town_size = log(3500) if str_townsize == "2"
replace temp_log_town_size = log(7500) if str_townsize == "3"
replace temp_log_town_size = log(15000) if str_townsize == "4"
replace temp_log_town_size = log(35000) if str_townsize == "5"
replace temp_log_town_size = log(75000) if str_townsize == "6"
replace temp_log_town_size = log(300000) if str_townsize == "7" 
replace temp_log_town_size = log(500000*8.35) if str_townsize == "8" 

gen str_townsize_EVS4 = string(X049a)
gen temp_log_town_size_EVS = .

replace temp_log_town_size_EVS = log(2500) if str_townsize_EVS4 == "1" 
replace temp_log_town_size_EVS = log(12500) if str_townsize_EVS4 == "2"
replace temp_log_town_size_EVS = log(60000) if str_townsize_EVS4 == "3"
replace temp_log_town_size_EVS = log(300000) if str_townsize_EVS4 == "4"
replace temp_log_town_size_EVS = log(500000*8.35) if str_townsize_EVS4 == "5"

gen X_dem_log_town_size = .
replace X_dem_log_town_size = temp_log_town_size_EVS if BB_Wave == "EVS Wave 4 (2008-2010)" | BB_Wave == "EVS Wave 5 (2017-2020)"
replace X_dem_log_town_size = temp_log_town_size if BB_Wave == "EVS Wave 2 (1990-1993)" | BB_Wave == "EVS Wave 3 (1999-2001)" | BB_Wave == "WVS Wave 2 (1989-1993)" | BB_Wave == "WVS Wave 3 (1994-1998)" | BB_Wave == "WVS Wave 4 (1999-2004)" | BB_Wave == "WVS Wave 5 (2005-2009)" | BB_Wave == "WVS Wave 6 (2010-2014)" | BB_Wave == "WVS Wave 7 (2017-2020)"

gen str_townsize_cs = string(X049CS)
//Czech Republic - Data from WVS Wave 3 only (...\Dropbox\Populist Voting\doc\wvs\WVS_Wave_3\F00008204-WV3_Codebook_v20180912.pdf Page 87)
replace X_dem_log_town_size = log(500) if str_townsize_cs == "203001" & X_dem_log_town_size==. //0-1000
replace X_dem_log_town_size = log(3000) if str_townsize_cs == "203002" & X_dem_log_town_size==. // 1,000-5,000
replace X_dem_log_town_size = log(12500) if str_townsize_cs == "203003" & X_dem_log_town_size==. // 5,000-20,0000
replace X_dem_log_town_size = log(60000) if str_townsize_cs == "203004" & X_dem_log_town_size==. //20,000-100,000
replace X_dem_log_town_size = log(500000*8.35) if str_townsize_cs == "203005" & X_dem_log_town_size==. // more than 100,000
//Estonia - Data from WVS Wave 3 only (...\Dropbox\Populist Voting\doc\wvs\WVS_Wave_3\F00008204-WV3_Codebook_v20180912.pdf Page 87)
replace X_dem_log_town_size = log(1000) if str_townsize_cs == "233001" & X_dem_log_town_size==. // 0-2,000
replace X_dem_log_town_size = log(26000) if str_townsize_cs == "233002" & X_dem_log_town_size==. // 2,000-50,000
replace X_dem_log_town_size = log(50000) if str_townsize_cs == "233003" & X_dem_log_town_size==. // more than 50,000
replace X_dem_log_town_size = log(426000) if str_townsize_cs == "233004" & X_dem_log_town_size==. // Tallinn(426,000)
//Finland - Data from WVS Wave 3 & EVS wave 2 (...\Dropbox\Populist Voting\doc\wvs\WVS_Wave_3\F00008204-WV3_Codebook_v20180912.pdf Page 87)
replace X_dem_log_town_size = log(2000) if str_townsize_cs == "246001" & X_dem_log_town_size==. //0-4,000 
replace X_dem_log_town_size = log(6000) if str_townsize_cs == "246002" & X_dem_log_town_size==. //4,000-8,000
replace X_dem_log_town_size = log(19000) if str_townsize_cs == "246003" & X_dem_log_town_size==. //8,000-30,000
replace X_dem_log_town_size = log(55000) if str_townsize_cs == "246004" & X_dem_log_town_size==. //30,000-80,000
replace X_dem_log_town_size = log(80000*8.35) if str_townsize_cs == "246005" & X_dem_log_town_size==. //more than 80,000
//Finland - Data from WVS Wave 5 only (...\Dropbox\Populist Voting\doc\wvs\WVS_Wave_5\F00007945-WV5_Codebook_v20180912.pdf" Page 89)
replace X_dem_log_town_size = log(5000) if str_townsize_cs == "246011" & X_dem_log_town_size==. //0-5,000
replace X_dem_log_town_size = log(7500) if str_townsize_cs == "246012" & X_dem_log_town_size==. //5,000-10,000
replace X_dem_log_town_size = log(15000) if str_townsize_cs == "246013" & X_dem_log_town_size==. //10,000-20,000
replace X_dem_log_town_size = log(60000) if str_townsize_cs == "246014" & X_dem_log_town_size==. //20,000-100,000
replace X_dem_log_town_size = log(500000*8.35) if str_townsize_cs == "246015" & X_dem_log_town_size==. //more than 100,000
//Spain - Data from WVS Wave 5&6 (...\Dropbox\Populist Voting\doc\wvs\WVS_Wave_5\F00007945-WV5_Codebook_v20180912.pdf" Page 89)
replace X_dem_log_town_size = log(1000) if str_townsize_cs == "724001" & X_dem_log_town_size==. //0-2,000 
replace X_dem_log_town_size = log(3500) if str_townsize_cs == "724002" & X_dem_log_town_size==. //2,000-5,000
replace X_dem_log_town_size = log(7500) if str_townsize_cs == "724003" & X_dem_log_town_size==. //5,000-10,000
replace X_dem_log_town_size = log(30000) if str_townsize_cs == "724004" & X_dem_log_town_size==. // 10,000-50,000
replace X_dem_log_town_size = log(75000) if str_townsize_cs == "724005" & X_dem_log_town_size==. //50,000-100,000
replace X_dem_log_town_size = log(175000) if str_townsize_cs == "724006" & X_dem_log_town_size==. //100,000-250,000
replace X_dem_log_town_size = log(250000) if str_townsize_cs == "724007" & X_dem_log_town_size==. // more than 250,000
replace X_dem_log_town_size = log(500000*8.35) if str_townsize_cs == "724008" & X_dem_log_town_size==. // Madrid(3.223M) or Barcelona(1.62M)
//Sweden - Data from WVS Wave 3 only (...\Dropbox\Populist Voting\doc\wvs\WVS_Wave_3\F00008204-WV3_Codebook_v20180912.pdf Page 87)
replace X_dem_log_town_size = log(100) if str_townsize_cs == "752001" & X_dem_log_town_size==. //countruside
replace X_dem_log_town_size = log(600) if str_townsize_cs == "752002" & X_dem_log_town_size==. //200-1000
replace X_dem_log_town_size = log(3000) if str_townsize_cs == "752003" & X_dem_log_town_size==. //1,000-5,000
replace X_dem_log_town_size = log(7500) if str_townsize_cs == "752004" & X_dem_log_town_size==. //5,000-10,000
replace X_dem_log_town_size = log(15000) if str_townsize_cs == "752005" & X_dem_log_town_size==. //10,000-20,000
replace X_dem_log_town_size = log(35000) if str_townsize_cs == "752006" & X_dem_log_town_size==. //20,000-50,000
replace X_dem_log_town_size = log(75000) if str_townsize_cs == "752007" & X_dem_log_town_size==. //50,000-100,000
replace X_dem_log_town_size = log(500000*8.35) if str_townsize_cs == "752008" & X_dem_log_town_size==. //more than 100,000



// Populism Index (not present in this dataset)
// MISSING VALUES FROM WVS WAVE 6 GERMANY DATA - question not present in time series data
//gen str_pop_index = string(Type_Populism)
//destring str_pop_index, gen(P_Populism_Index)
//replace P_Populism_Index = . if P_Populism_Index <= 0

// Religious Person Dummy Variable 
// Roughly 1,800 missing observations, from both WVS Wave 6 time series and WVS Wave 7 integrated datasets
decode F034, gen (temp_religious)
gen X_dem_Religious = .
replace X_dem_Religious = 0 if temp_religious == "Not a religious person" | temp_religious == "A convinced atheist"
replace X_dem_Religious = 1 if temp_religious == "A religious person"
checkDistinct X_dem_Religious 2

// Athiest Person Dummy
gen X_dem_Athiest = .
replace X_dem_Athiest = 0 if temp_religious == "A religious person" | temp_religious == "Not a religious person"
replace X_dem_Athiest = 1 if temp_religious == "A convinced atheist"
checkDistinct X_dem_Athiest 2

// Sex Dummy Variable - 0 = female, 1 = male
decode X001, gen (temp_sex)
gen X_dem_male = .
replace X_dem_male = 0 if temp_sex == "Female" 
replace X_dem_male = 1 if temp_sex == "Male"
checkDistinct X_dem_male 2

// Age Variable
gen str_age = string(X003)
replace str_age = "." if str_age == ".a" | str_age == ".b" | str_age == ".e"
destring str_age, gen(X_dem_age)

// Cohort
gen X_dem_cohort = BB_Year - X_dem_age

// Marital Status
//Codebook: X007
decode X007, gen (temp_marital)

//Married or living together
gen X_dem_married = .
replace X_dem_married = 0 if temp_marital == "Divorced" | temp_marital == "Separated" | temp_marital == "Widowed" | temp_marital == "Single/Never married" | temp_marital == "Divorced, Separated or Widow"
replace X_dem_married = 1 if temp_marital == "Married" | temp_marital == "Living together as married" | temp_marital == "Living apart but steady relation (married,cohabitation)"
checkDistinct X_dem_married 2

//Divorced, separated, or widow
gen X_dem_divorced = .
replace X_dem_divorced = 0 if temp_marital == "Married" | temp_marital == "Living together as married" | temp_marital == "Living apart but steady relation (married,cohabitation)" | temp_marital == "Single/Never married"
replace X_dem_divorced = 1 if temp_marital == "Divorced" | temp_marital == "Separated" | temp_marital == "Widowed" | temp_marital == "Divorced, Separated or Widow"
checkDistinct X_dem_divorced 2

//Single
gen X_dem_single = .
replace X_dem_single = 0 if temp_marital == "Divorced" | temp_marital == "Separated" | temp_marital == "Widowed" | temp_marital == "Divorced, Separated or Widow" | temp_marital == "Married" | temp_marital == "Living together as married" | temp_marital == "Living apart but steady relation (married,cohabitation)"
replace X_dem_single = 1 if temp_marital == "Single/Never married"
checkDistinct X_dem_single 2

// Number of children 
gen str_children = string(X011)
destring str_children, gen(X_dem_children)
replace X_dem_children = . if X_dem_children == .a | X_dem_children == .b | X_dem_children == .d | X_dem_children == .e

// Employment Status
//Employed
decode X028, gen (temp_employ)
gen X_dem_employed = .
replace X_dem_employed = 0 if temp_employ == "Retired" | temp_employ== "Housewife" | temp_employ == "Students" | temp_employ == "Unemployed"
replace X_dem_employed = 1 if temp_employ == "Part time"
replace X_dem_employed = 2 if temp_employ == "Full time" | temp_employ == "Self employed"
checkDistinct X_dem_employed 3

//Self-employed
gen X_dem_self_employed = .
replace X_dem_self_employed = 0 if temp_employ == "Full time" | temp_employ == "Part time" | temp_employ == "Retired" | temp_employ== "Housewife" | temp_employ == "Students" | temp_employ == "Unemployed"
replace X_dem_self_employed = 1 if temp_employ == "Self employed"
checkDistinct X_dem_self_employed 2
 
//Retired
gen X_dem_retired = .
replace X_dem_retired = 0 if temp_employ == "Full time" | temp_employ == "Part time" | temp_employ == "Self employed" | temp_employ== "Housewife" | temp_employ == "Students" | temp_employ == "Unemployed"
replace X_dem_retired = 1 if temp_employ == "Retired"
checkDistinct X_dem_retired 2

//Housewife
gen X_dem_housewife = .
replace X_dem_housewife = 0 if temp_employ == "Full time" | temp_employ == "Part time" | temp_employ == "Self employed" | temp_employ == "Retired" | temp_employ == "Students" | temp_employ == "Unemployed"
replace X_dem_housewife = 1 if temp_employ== "Housewife"
checkDistinct X_dem_housewife 2

//Student
gen X_dem_student = .
replace X_dem_student = 0 if temp_employ == "Full time" | temp_employ == "Part time" | temp_employ == "Self employed" | temp_employ == "Retired" | temp_employ == "Housewife" | temp_employ == "Unemployed"
replace X_dem_student = 1 if temp_employ == "Students"
checkDistinct X_dem_student 2

//Unemployed
gen X_dem_unemployed = .
replace X_dem_unemployed = 0 if temp_employ == "Full time" | temp_employ == "Part time" | temp_employ == "Self employed" | temp_employ == "Retired" | temp_employ == "Housewife" | temp_employ == "Students"
replace X_dem_unemployed = 1 if temp_employ == "Unemployed"
checkDistinct X_dem_unemployed 2

//Other
gen X_dem_employ_other = .
replace X_dem_employ_other = 0 if temp_employ == "Full time" | temp_employ == "Part time" | temp_employ == "Self employed" | temp_employ == "Retired" | temp_employ == "Housewife" | temp_employ == "Students" | temp_employ == "Unemployed"
replace X_dem_employ_other = 1 if temp_employ == "Other"
checkDistinct X_dem_employ_other 2

// Profession
decode X036, gen (temp_profession)
decode V097EF, gen (temp_profession_wave5)
decode X028_01, gen(temp_profession_wave45)

gen temp_dem_profession = "."
replace temp_dem_profession = "Never had a job" if temp_profession == "Never had a job" | temp_profession_wave5 == "Never had a job" | temp_profession_wave45 == "Never had a paid job"
replace temp_dem_profession = "Professional worker" if temp_profession == "Professional worker" | temp_profession_wave5 == "Professional and technical (e.g.: doctor, teacher)"
replace temp_dem_profession = "Higher administrative/ Manager" if temp_profession == "Employer/manager of establishment with 10 or more employed" | temp_profession == "Employer/manager of establishment with less  than 10 employed" | temp_profession == "Supervisory Non manual -office worker" | temp_profession_wave5 == "Higher administrative (e.g.: banker)" 
replace temp_dem_profession = "Non manual office worker" if temp_profession == "Foreman and supervisor" | temp_profession == "Middle level non-manual office worker" | temp_profession == "Junior level non manual" | temp_profession == "Non manual -office worker" | temp_profession_wave5 == "Clerical (e.g.: secretary, clerk, office manager)" | temp_profession_wave5 == "Sales (e.g.: sales manager, shop owner)"
replace temp_dem_profession = "Skilled manual" if temp_profession == "Skilled manual" | temp_profession_wave5 == "Service (e.g.: restaurant owner, police officer)" | temp_profession_wave5 == "Skilled worker (e.g.: foreman, motor mechanic)"
replace temp_dem_profession = "Semi skilled manual worker" if temp_profession == "Semi-skilled manual worker" | temp_profession_wave5 == "Semi-skilled worker (for example: bricklayer, bus driver, ca"
replace temp_dem_profession = "Unskilled manual" if temp_profession == "Unskilled manual" | temp_profession_wave5 == "Unskilled worker (e.g.: labourer, porter)"
replace temp_dem_profession = "Agricultural worker" if temp_profession == "Agricultural worker" | temp_profession_wave5 == "Farm worker (e.g.: farm labourer, tractor driver)"
replace temp_dem_profession = "Farm owner/Farm manager" if temp_profession == "Farmer: has own farm" | temp_profession_wave5 == "Farm owner, farm manager"
replace temp_dem_profession = "Other" if temp_profession == "Other" | temp_profession == "Member of armed forces" 
checkDistinct temp_dem_profession 10

gen temp_profession_2_digit_EVS4 = X035_2 if BB_Wave == "EVS Wave 4 (2008-2010)" 
replace temp_dem_profession = "Professional worker" if 20<=temp_profession_2_digit_EVS4 & temp_profession_2_digit_EVS4<=34 
replace temp_dem_profession = "Higher administrative/ Manager" if 10<=temp_profession_2_digit_EVS4 & temp_profession_2_digit_EVS4<=14
replace temp_dem_profession = "Non manual office worker" if 40<=temp_profession_2_digit_EVS4 & temp_profession_2_digit_EVS4<=44 
replace temp_dem_profession = "Skilled manual" if 50<=temp_profession_2_digit_EVS4 & temp_profession_2_digit_EVS4<=54 
replace temp_dem_profession = "Semi skilled manual worker" if 70<=temp_profession_2_digit_EVS4 & temp_profession_2_digit_EVS4<=75 
replace temp_dem_profession = "Unskilled manual" if 80<=temp_profession_2_digit_EVS4 & temp_profession_2_digit_EVS4<=93 

// Binary variables for profession, base variable- skilled manual
gen X_dem_oc_professional = temp_dem_profession == "Professional worker" if temp_dem_profession!= "."
gen X_dem_oc_manager = temp_dem_profession == "Higher administrative/ Manager" if temp_dem_profession!= "."
gen X_dem_oc_non_manual_office = temp_dem_profession == "Non manual office worker" if temp_dem_profession!= "."
gen X_dem_oc_semi_skilled_manual = temp_dem_profession == "Semi skilled manual worker" if temp_dem_profession!= "."
gen X_dem_oc_unskilled_manual = temp_dem_profession == "Unskilled manual" if temp_dem_profession!= "."
gen X_dem_oc_agricultural_worker = temp_dem_profession == "Agricultural worker" if temp_dem_profession!= "."
gen X_dem_oc_farm_owner_or_manager = temp_dem_profession == "Farm owner/Farm manager" if temp_dem_profession!= "."
gen X_dem_oc_other = temp_dem_profession == "Other" if temp_dem_profession!= "."

// Scale of Incomes
gen str_inc_scale_evs = string(X047_EVS)
destring str_inc_scale_evs, gen(temp_inc_scale_evs)
gen str_inc_scale_wvs = string(X047_WVS)
destring str_inc_scale_wvs, gen(temp_inc_scale_wvs)

gen X_dem_Income_Decile = temp_inc_scale_evs
replace X_dem_Income_Decile = temp_inc_scale_wvs if BB_Wave_WVS==1

// Member of Labor Union
decode A101, gen (temp_union_mem)
gen temp_labor_union_mem = .
replace temp_labor_union_mem = 0 if temp_union_mem == "Not a member"
replace temp_labor_union_mem = 1 if temp_union_mem == "Active member" | temp_union_mem == "Inactive member"
checkDistinct temp_labor_union_mem 2

decode A067, gen (temp_belong_union_mem)
gen temp_belong_labor_union_mem = .
replace temp_belong_labor_union_mem = 0 if temp_belong_union_mem == "Not mentioned"
replace temp_belong_labor_union_mem = 1 if temp_belong_union_mem == "Mentioned"
checkDistinct temp_belong_labor_union_mem 2

gen X_dem_labor_union_mem = .
replace X_dem_labor_union_mem = temp_labor_union_mem if BB_Wave == "WVS Wave 3 (1994-1998)" | BB_Wave == "WVS Wave 5 (2005-2009)" | BB_Wave == "WVS Wave 6 (2010-2014)" | BB_Wave == "WVS Wave 7 (2017-2020)"
replace X_dem_labor_union_mem = temp_belong_labor_union_mem if BB_Wave == "EVS Wave 2 (1990-1993)" | BB_Wave == "EVS Wave 3 (1999-2001)" | BB_Wave == "EVS Wave 4 (2008-2010)" ///
| BB_Wave == "EVS Wave 5 (2017-2020)" | BB_Wave == "WVS Wave 2 (1989-1993)" | BB_Wave == "WVS Wave 4 (1999-2004)"

//Mother immigrant
//Codebook G026
decode G026, gen (temp_mom_immigrant)
gen X_dem_mom_immigrant = .
replace X_dem_mom_immigrant = 0 if temp_mom_immigrant=="No"
replace X_dem_mom_immigrant = 1 if temp_mom_immigrant=="Yes"
checkDistinct X_dem_mom_immigrant 2
 
//Father immigrant
//Codebook G027
decode G027, gen (temp_dad_immigrant)
gen X_dem_dad_immigrant = .
replace X_dem_dad_immigrant = 0 if temp_dad_immigrant=="No"
replace X_dem_dad_immigrant = 1 if temp_dad_immigrant=="Yes"
checkDistinct X_dem_dad_immigrant 2

//Frequency you/family (last 12 month): Gone without enough food to eat
//Codebook H008_01
decode H008_01, gen (temp_not_enough_food)
gen X_dem_not_enough_food = .
replace X_dem_not_enough_food = 0 if temp_not_enough_food=="Never"
replace X_dem_not_enough_food = 1 if temp_not_enough_food=="Rarely"
replace X_dem_not_enough_food = 2 if temp_not_enough_food=="Sometimes"
replace X_dem_not_enough_food = 3 if temp_not_enough_food=="Often"
checkDistinct X_dem_not_enough_food 4

//Do you live with your parents
//Codebook X026
gen temp_live_w_parents = string(X026)
gen temp_X_dem_live_w_parents = .
replace temp_X_dem_live_w_parents = 0 if temp_live_w_parents=="0"
replace temp_X_dem_live_w_parents = 1 if temp_live_w_parents=="1"

gen temp_live_w_parents_wave5 = string(X026_01)
gen temp_wave_5_live_w_parents = .
replace temp_wave_5_live_w_parents = 0 if temp_live_w_parents_wave5=="1"
replace temp_wave_5_live_w_parents = 1 if temp_live_w_parents_wave5=="2" | temp_live_w_parents_wave5=="3" | temp_live_w_parents_wave5=="4"

gen X_dem_live_w_parents = .
replace X_dem_live_w_parents = temp_X_dem_live_w_parents if BB_Wave_num != 6
replace X_dem_live_w_parents = temp_wave_5_live_w_parents if BB_Wave_num == 6
checkDistinct X_dem_live_w_parents 2

//Are you the chief wage earner in your house
//Codebook X040
decode X040, gen (temp_chief_wage)
gen X_dem_chief_wage = .
replace X_dem_chief_wage = 0 if temp_chief_wage=="No"
replace X_dem_chief_wage = 1 if temp_chief_wage=="Yes"
checkDistinct X_dem_chief_wage 2

//Is the chief wage earner employed now
//Codebook X041
decode X041, gen (temp_chief_wage_employed)
gen X_dem_chief_wage_employed = .
replace X_dem_chief_wage_employed = 0 if temp_chief_wage_employed=="No"
replace X_dem_chief_wage_employed = 1 if temp_chief_wage_employed=="Yes"
checkDistinct X_dem_chief_wage_employed 2

//Family savings during past year
//Codebook X044
decode X044, gen (temp_family_savings)
gen X_dem_family_saved = .
replace X_dem_family_saved = 1 if temp_family_savings=="Save money"
replace X_dem_family_saved = 0 if temp_family_savings=="Just get by" | temp_family_savings=="Spent savings and borrowed money" | temp_family_savings=="Spent some savings and borrowed money"
checkDistinct X_dem_family_saved 2

gen X_dem_family_got_by = .
replace X_dem_family_got_by = 1 if temp_family_savings=="Just get by"
replace X_dem_family_got_by = 0 if temp_family_savings=="Save money" | temp_family_savings=="Spent savings and borrowed money" | temp_family_savings=="Spent some savings and borrowed money"
checkDistinct X_dem_family_got_by 2

gen X_dem_spent_savings_and_borrowed = .
replace X_dem_spent_savings_and_borrowed = 1 if temp_family_savings=="Spent savings and borrowed money"
replace X_dem_spent_savings_and_borrowed = 0 if temp_family_savings=="Save money" | temp_family_savings=="Just get by" | temp_family_savings=="Spent some savings and borrowed money"
checkDistinct X_dem_spent_savings_and_borrowed 2

//Social class (subjective)
//Codebook X045
decode X045, gen (temp_social_class)
gen X_dem_social_class = .
replace X_dem_social_class = 0 if temp_social_class=="Lower class"
replace X_dem_social_class = 1 if temp_social_class=="Working class"
replace X_dem_social_class = 2 if temp_social_class=="Lower middle class"
replace X_dem_social_class = 3 if temp_social_class=="Upper middle class"
replace X_dem_social_class = 4 if temp_social_class=="Upper class"
checkDistinct X_dem_social_class 5

//Institution of occupation
//Codebook X052
decode X052, gen (temp_place_job)
gen X_dem_work_private_bus = .
replace X_dem_work_private_bus = 1 if temp_place_job=="Private business"
replace X_dem_work_private_bus = 0 if temp_place_job=="Public institution" | temp_place_job=="Self-employed" | temp_place_job=="Private non-profit organization"
checkDistinct X_dem_work_private_bus 2
 
gen X_dem_work_public_inst = .
replace X_dem_work_public_inst = 1 if temp_place_job=="Public institution"
replace X_dem_work_public_inst = 0 if temp_place_job=="Private business" | temp_place_job=="Self-employed" | temp_place_job=="Private non-profit organization"
checkDistinct X_dem_work_public_inst 2

gen X_dem_work_non_prof = .
replace X_dem_work_non_prof = 1 if temp_place_job=="Private non-profit organization"
replace X_dem_work_non_prof = 0 if temp_place_job=="Private business" | temp_place_job=="Self-employed" | temp_place_job=="Public institution"
checkDistinct X_dem_work_non_prof 2

//Religion
//Codebook F025
decode F025, gen (temp_rel)

gen X_dem_rel_protestant = .
replace X_dem_rel_protestant = 1 if temp_rel=="Protestant" 
replace X_dem_rel_protestant = 0 if temp_rel=="Roman Catholic" | temp_rel=="Muslim" | temp_rel=="Hindu" | temp_rel=="Buddhist" | temp_rel=="Orthodox (Russian/Greek/etc.)" | temp_rel=="Jew" | temp_rel=="Do not belong to a denomination" | temp_rel== "Other"
checkDistinct X_dem_rel_protestant 2
 
gen X_dem_rel_catholic = .
replace X_dem_rel_catholic = 1 if temp_rel=="Roman Catholic"
replace X_dem_rel_catholic = 0 if temp_rel=="Muslim" | temp_rel=="Hindu" | temp_rel=="Buddhist" | temp_rel=="Orthodox (Russian/Greek/etc.)" | temp_rel=="Jew" | temp_rel=="Do not belong to a denomination" | temp_rel== "Other" | temp_rel=="Protestant" | temp_rel=="Other Christian (Evangelical/Pentecostal/Free church/etc.)" 
checkDistinct X_dem_rel_catholic 2

gen X_dem_rel_muslim = .
replace X_dem_rel_muslim = 1 if temp_rel=="Muslim" 
replace X_dem_rel_muslim = 0 if temp_rel=="Roman Catholic" | temp_rel=="Hindu" | temp_rel=="Buddhist" | temp_rel=="Orthodox (Russian/Greek/etc.)" | temp_rel=="Jew" | temp_rel=="Do not belong to a denomination" | temp_rel== "Other" | temp_rel=="Protestant" | temp_rel=="Other Christian (Evangelical/Pentecostal/Free church/etc.)" 
checkDistinct X_dem_rel_muslim 2

gen X_dem_rel_christian_other = .
replace X_dem_rel_christian_other = 1 if temp_rel=="Other Christian (Evangelical/Pentecostal/Free church/etc.)" | temp_rel=="Orthodox (Russian/Greek/etc.)"
replace X_dem_rel_christian_other = 0 if temp_rel=="Protestant" | temp_rel=="Roman Catholic" | temp_rel=="Muslim" | temp_rel=="Hindu" | temp_rel=="Buddhist" | temp_rel=="Jew" | temp_rel=="Do not belong to a denomination" | temp_rel== "Other"
checkDistinct X_dem_rel_christian_other 2

gen X_dem_rel_jew = .
replace X_dem_rel_jew = 1 if temp_rel=="Jew" 
replace X_dem_rel_jew = 0 if temp_rel=="Do not belong to a denomination" | temp_rel=="Other Christian (Evangelical/Pentecostal/Free church/etc.)" | temp_rel=="Orthodox (Russian/Greek/etc.)" | temp_rel=="Protestant" | temp_rel=="Roman Catholic" | temp_rel=="Muslim" | temp_rel== "Other" | temp_rel=="Hindu" | temp_rel=="Buddhist" 
checkDistinct X_dem_rel_jew 2

gen X_dem_rel_no_rel = .
replace X_dem_rel_no_rel = 1 if temp_rel=="Do not belong to a denomination"
replace X_dem_rel_no_rel = 0 if temp_rel=="Other Christian (Evangelical/Pentecostal/Free church/etc.)" | temp_rel=="Orthodox (Russian/Greek/etc.)" | temp_rel=="Protestant" | temp_rel=="Roman Catholic" | temp_rel=="Muslim" | temp_rel=="Hindu" | temp_rel=="Buddhist" | temp_rel=="Jew" | temp_rel== "Other"
checkDistinct X_dem_rel_no_rel 2

gen X_dem_rel_other = .
replace X_dem_rel_other = 1 if temp_rel== "Other" | temp_rel=="Hindu" | temp_rel=="Buddhist" 
replace X_dem_rel_other = 0 if temp_rel=="Do not belong to a denomination" | temp_rel=="Other Christian (Evangelical/Pentecostal/Free church/etc.)" | temp_rel=="Orthodox (Russian/Greek/etc.)" | temp_rel=="Protestant" | temp_rel=="Roman Catholic" | temp_rel=="Muslim" 
checkDistinct X_dem_rel_other 2

//Former Religion
//Codebook F027
decode F027, gen (temp_rel2)

gen X_dem_former_protestant = .
replace X_dem_former_protestant = 1 if temp_rel2=="Protestant"
replace X_dem_former_protestant = 0 if temp_rel2=="Buddhist" | temp_rel2=="Did not belong to a former denomination" | temp_rel2=="Hindu" | temp_rel2=="Jew" | temp_rel2=="Muslim" | temp_rel2=="Orthodox (Russian/Greek/etc.)" | temp_rel2=="Other" | temp_rel2=="Roman Catholic" | temp_rel2=="Other Christian (Evangelical/Pentecostal/Free church/etc.)"
checkDistinct X_dem_former_protestant 2
 
gen X_dem_former_catholic = .
replace X_dem_former_catholic = 1 if temp_rel2=="Roman Catholic"
replace X_dem_former_catholic = 0 if temp_rel2=="Buddhist" | temp_rel2=="Did not belong to a former denomination" | temp_rel2=="Hindu" | temp_rel2=="Jew" | temp_rel2=="Muslim" | temp_rel2=="Orthodox (Russian/Greek/etc.)" | temp_rel2=="Other" | temp_rel2=="Other Christian (Evangelical/Pentecostal/Free church/etc.)" | temp_rel2=="Protestant"
checkDistinct X_dem_former_catholic 2

gen X_dem_former_muslim = .
replace X_dem_former_muslim = 1 if temp_rel2=="Muslim"
replace X_dem_former_muslim = 0 if temp_rel2=="Buddhist" | temp_rel2=="Did not belong to a former denomination" | temp_rel2=="Hindu" | temp_rel2=="Jew" | temp_rel2=="Orthodox (Russian/Greek/etc.)" | temp_rel2=="Other" | temp_rel2=="Other Christian (Evangelical/Pentecostal/Free church/etc.)" | temp_rel2=="Protestant" | temp_rel2=="Roman Catholic"
checkDistinct X_dem_former_muslim 2

gen X_dem_former_christian_other = .
replace X_dem_former_christian_other = 1 if temp_rel2=="Other Christian (Evangelical/Pentecostal/Free church/etc.)" | temp_rel2=="Orthodox (Russian/Greek/etc.)"
replace X_dem_former_christian_other = 0 if temp_rel2=="Buddhist" | temp_rel2=="Did not belong to a former denomination" | temp_rel2=="Hindu" | temp_rel2=="Jew" | temp_rel2=="Other" | temp_rel2=="Protestant" | temp_rel2=="Roman Catholic" | temp_rel2=="Muslim"
checkDistinct X_dem_former_christian_other 2

gen X_dem_former_jew = .
replace X_dem_former_jew = 1 if temp_rel2=="Jew"
replace X_dem_former_jew = 0 if temp_rel2=="Buddhist" | temp_rel2=="Did not belong to a former denomination" | temp_rel2=="Hindu" | temp_rel2=="Other" | temp_rel2=="Protestant" | temp_rel2=="Roman Catholic" | temp_rel2=="Muslim" | temp_rel2=="Other Christian (Evangelical/Pentecostal/Free church/etc.)" | temp_rel2=="Orthodox (Russian/Greek/etc.)"
checkDistinct X_dem_former_jew 2

gen X_dem_former_hindu = .
replace X_dem_former_hindu = 1 if temp_rel2=="Hindu"
replace X_dem_former_hindu = 0 if temp_rel2=="Buddhist" | temp_rel2=="Did not belong to a former denomination" | temp_rel2=="Other" | temp_rel2=="Protestant" | temp_rel2=="Roman Catholic" | temp_rel2=="Muslim" | temp_rel2=="Other Christian (Evangelical/Pentecostal/Free church/etc.)" | temp_rel2=="Orthodox (Russian/Greek/etc.)" | temp_rel2=="Jew" 
checkDistinct X_dem_former_hindu 2

gen X_dem_former_buddhist = .
replace X_dem_former_buddhist = 1 if temp_rel2=="Buddhist"
replace X_dem_former_buddhist = 0 if temp_rel2=="Did not belong to a former denomination" | temp_rel2=="Other" | temp_rel2=="Protestant" | temp_rel2=="Roman Catholic" | temp_rel2=="Muslim" | temp_rel2=="Other Christian (Evangelical/Pentecostal/Free church/etc.)" | temp_rel2=="Orthodox (Russian/Greek/etc.)" | temp_rel2=="Jew" | temp_rel2=="Hindu"
checkDistinct X_dem_former_buddhist 2

gen X_dem_former_other = .
replace X_dem_former_other = 1 if temp_rel2=="Other"
replace X_dem_former_other = 0 if temp_rel2=="Did not belong to a former denomination" | temp_rel2=="Protestant" | temp_rel2=="Roman Catholic" | temp_rel2=="Muslim" | temp_rel2=="Other Christian (Evangelical/Pentecostal/Free church/etc.)" | temp_rel2=="Orthodox (Russian/Greek/etc.)" | temp_rel2=="Jew" | temp_rel2=="Hindu" | temp_rel2=="Buddhist"
checkDistinct X_dem_former_other 2

//Language spoken at home
//Codebook G016
decode G016, gen (temp_lang_home)

sort BB_Wave BB_Country
by BB_Wave BB_Country: egen commonLang = mode(G016)
gen X_dem_lang_home_common = .
replace X_dem_lang_home_common = 1 if G016 == commonLang & G016 < 9999
replace X_dem_lang_home_common = 0 if G016 != commonLang & G016 < 9999
checkDistinct X_dem_lang_home_common 2

label drop G016

gen X_dem_lang_home_english = .
replace X_dem_lang_home_english = 0 if G016 > 0 & G016 < 9999
replace X_dem_lang_home_english = 1 if temp_lang_home=="English"
checkDistinct X_dem_lang_home_english 2


/* Recoded ignore
gen X_dem_vocation_train = .
replace X_dem_vocation_train = 1 if temp_education == "Incomplete secondary school: technical/vocational type" | temp_education == "Complete secondary school: technical/vocational type/secondary"
replace X_dem_vocation_train = 0 if temp_education == "Inadequately completed elementary education" | temp_education == "Completed (compulsory) elementary education" | temp_education == "Incomplete secondary: university-preparatory type/Secondary," | temp_education == "Complete secondary: university-preparatory type/full secondary" | temp_education == "Some university without degree/higher education - lower-level tertiary" | temp_education == "University with degree/higher education - upper-level tertiary"
checkDistinct X_dem_vocation_train 2


decode X025R, gen (temp_education_3_value)
tab X025R BB_Wave, missing
gen X_dem_education_basic = .
replace X_dem_education_basic = 1 if temp_education_3_value == "Lower"
replace X_dem_education_basic = 2 if temp_education_3_value == "Middle"
replace X_dem_education_basic = 3 if temp_education_3_value == "Upper"
checkDistinct X_dem_education_basic 3
*/


//Education
//Codebook X025
decode X025, gen (temp_education)
tab X025 BB_Wave, missing

gen temp_dem_education_most_waves = .
replace temp_dem_education_most_waves = 3 if temp_education == "Inadequately completed elementary education" | temp_education == "Not applicable; No formal education"
replace temp_dem_education_most_waves = 6 if temp_education == "Completed (compulsory) elementary education"
replace temp_dem_education_most_waves = 8 if temp_education == "Incomplete secondary school: technical/vocational type" | temp_education == "Incomplete secondary: university-preparatory type/Secondary,"
replace temp_dem_education_most_waves = 12 if temp_education == "Complete secondary school: technical/vocational type/secondary" | temp_education == "Complete secondary: university-preparatory type/full secondary"
replace temp_dem_education_most_waves = 14 if temp_education == "Some university without degree/higher education - lower-level tertiary"
replace temp_dem_education_most_waves = 17 if temp_education == "University with degree/higher education - upper-level tertiary"
checkDistinct temp_dem_education_most_waves 21

decode X025A_01, gen (temp_education_WVS7)
tab X025A_01 BB_Wave, missing

gen temp_dem_education_WVS7 = .
replace temp_dem_education_WVS7 = 3 if temp_education_WVS7 == "Less than primary"
replace temp_dem_education_WVS7 = 6 if temp_education_WVS7 == "Primary"
replace temp_dem_education_WVS7 = 8 if temp_education_WVS7 == "Lower secondary"
replace temp_dem_education_WVS7 = 12 if temp_education_WVS7 == "Upper secondary"
replace temp_dem_education_WVS7 = 14 if temp_education_WVS7 == "Post-secondary non tertiary" | temp_education_WVS7 == "Short-cycle tertiary"
replace temp_dem_education_WVS7 = 17 if temp_education_WVS7 == "Bachelor or equivalent"
replace temp_dem_education_WVS7 = 17 if temp_education_WVS7 == "Master or equivalent"
replace temp_dem_education_WVS7 = 17 if temp_education_WVS7 == "Doctoral or equivalent"
checkDistinct temp_dem_education_WVS7 21

gen X_dem_education = .
replace X_dem_education = temp_dem_education_WVS7 if BB_Wave == "WVS Wave 7 (2017-2020)"
replace X_dem_education = temp_dem_education_most_waves if BB_Wave != "WVS Wave 7 (2017-2020)"

// Record the reported age at which the respondent completed education.
gen X_dem_education_completed_age = .
replace X_dem_education_completed_age = X023 if X023>=0 & X023<=70

/* Missing education for wave 2 EVS and WVS, can perhaps impute using number of years*/
tab BB_Wave X_dem_education, missing


//Year of birth
//Codebook X002
gen str_year_birth = string(X002)
destring str_year_birth, gen(X_dem_year_birth)
replace X_dem_year_birth = . if X_dem_year_birth == .a | X_dem_year_birth == .b | X_dem_year_birth == .e

//Number of people in household
//Codebook X013
gen str_num_household = string(X013)
destring str_num_household, gen(X_dem_num_household)
replace X_dem_num_household = . if X_dem_num_household == .a | X_dem_num_household == .b | X_dem_num_household == .e


// Saving data to dropbox
drop temp_* str_*
save "$POPULISM_DROPBOX/data/intdata/temp/ivs_clean_dem.dta", replace
