# *************************************************************************
# Description ----
# *************************************************************************

# Clean party leaders dataset

# *************************************************************************
# SETUP ----
# *************************************************************************

rm(list = ls())

library(dplyr)
library(tidyr)
library(haven)
library(data.table)

# paths
source("code/util/commonVars.R")
source("code/process/paths.R")

# load
party_leaders <- fread(file.path(authorcreated_path, "partyleader", "party_leaders.csv"))
wage <- read_dta(file.path(datasets_path, "IPUMS", "wage_occ_2023.dta")) %>%
  select(occupation = occ, ln_wage)

# clean main demographics
party_leaders <- party_leaders %>%
  mutate(across(everything(), ~ case_when(
    . == "." ~ NA,
    TRUE ~ .
  ))) %>% 
  mutate(
    gender = case_when(
      gender == "Male" ~ 1,
      gender == "Female" ~ 0,
      TRUE ~ NA),
    marital_status = case_when(
      marital_status == "Married" ~ 1,
      marital_status == "Single" ~ 0,
      TRUE ~ NA),
    minority = case_when(
      minority == "Yes" ~ 1,
      minority == "No" ~ 0,
      TRUE ~ NA),
    criminal_record = case_when(
      criminal_record == "Yes" ~ 1,
      criminal_record == "No" ~ 0,
      TRUE ~ NA),
    age_squared = as.numeric(age)^2)

# clean years of education
party_leaders <- party_leaders %>%
  mutate(
    education = case_when(
      education %in% c("Elementary school") ~ 6,
      education %in% c("Middle School") ~ 8,
      education %in% c("Secondary School", "High School") ~ 12,
      education %in% c("BA", "BSc", "BEng", "LLB") ~ 15,
      education %in% c("LLM", "Ma", "MA", "MBA", "MEng", "MSc") ~ 17,
      education %in% c("PhD", "JD") ~ 20,
      TRUE ~ NA)) 

# clean occupation
party_leaders <- party_leaders %>%
  mutate(
    occupation = case_when(
      occupation %in% c("Actor", "Poet") ~ "2700",
      occupation %in% c("Administration Director") ~ "0101",
      occupation %in% c("Archaeological Staff", "Architect") ~ "1305",
      occupation %in% c("Artist") ~ "2600",
      occupation %in% c("Athlete") ~ "2721",
      occupation %in% c("Auctioneer") ~ "5150",
      occupation %in% c("Author") ~ "2859",
      occupation %in% c("Banker") ~ "0910",
      occupation %in% c("Bankruptcy administrator") ~ "0900",
      occupation %in% c("Bookstore Owner", "Businessman", "Entrepreneur", "CEO", "Executive") ~ "0010",
      occupation %in% c("Charity Worker") ~ "2014",
      occupation %in% c("Chemist") ~ "1720",
      occupation %in% c("Christian Missionary") ~ "2040",
      occupation %in% c("Civil Engineer") ~ "1360",
      occupation %in% c("Class Assistant (civil service)") ~ "2330",
      occupation %in% c("Collaborator", "Project manager", "Project Manager") ~ "0705",
      occupation %in% c("Conductor") ~ "9240",
      occupation %in% c("Consultant", "Office manager", "Manager", "Managing Director", "Councillor",
                        "Director", "Director-General") ~ "0440",
      occupation %in% c("Designer") ~ "2632",
      occupation %in% c("Detective assistant (police)") ~ "3820",
      occupation %in% c("Developer", "Technologist") ~ "1551",
      occupation %in% c("Doctor", "Physician", "Ophthalmologist", "Pediatrician") ~ "3090",
      occupation %in% c("Domestic helper") ~ "3601",
      occupation %in% c("Economic Director", "Economist") ~ "1800",
      occupation %in% c("Editor") ~ "2830",
      occupation %in% c("Education Director", "Higher Education Ofiicial", "Rector", "University Dean") ~ "2050",
      occupation %in% c("Engineer") ~ "1530",
      occupation %in% c("Entertainer") ~ "2770",
      occupation %in% c("EU rapporteur", "Journalist", "Language specialist", "News reporter", 
                        "Reporter", "Sports reporter") ~ "2810",
      occupation %in% c("Factory worker") ~ "8990",
      occupation %in% c("Family Therapist") ~ "2003",
      occupation %in% c("Farmer") ~ "0205",
      occupation %in% c("Forum Chair", "Policy official", "Political Advisor", 
                        "Political coordinator (not in politics)",
                        "President of Fisherman Association",
                        "President of non-partisan organisation",
                        "Representative of the IDB",
                        "Student union president", "Student assistant",
                        "UNESCO Commission Chairman", "Union Leader",
                        "Union representative", "Volunteer",
                        "Youth consultant") ~ "0060",
      occupation %in% c("Gendarme", "Officer (armed forces)", "Partisan") ~ "9840",
      occupation %in% c("Health Care Advisorr") ~ "0350",
      occupation %in% c("Human Resource Manager") ~ "0136",
      occupation %in% c("Insurancer") ~ "4810",
      occupation %in% c("Interior restorer") ~ "6230",
      occupation %in% c("Investment Banker") ~ "0845",
      occupation %in% c("IT Specialist") ~ "1005",
      occupation %in% c("Lawyer") ~ "2100",
      occupation %in% c("Lecturer", "Professor", "Postsecondary Teacher") ~ "2205",
      occupation %in% c("Judge", "Legal Counsel", "Legal secratery", "Prosecutor") ~ "2180",
      occupation %in% c("Mechanical Engineer") ~ "1460",
      occupation %in% c("Metalworker") ~ "8025",
      occupation %in% c("Musician") ~ "2752",   
      occupation %in% c("NGO Executive") ~ "0060",
      occupation %in% c("No") ~ "0030",
      occupation %in% c("Notary") ~ "2170",
      occupation %in% c("PR consultant") ~ "2825",
      occupation %in% c("Research Assitant", "Research Associate", "Research Fellow",
                        "Researcher") ~ "1970",
      occupation %in% c("Sales consultant") ~ "0052",
      occupation %in% c("Salesman", "Salesperson") ~ "4760",
      occupation %in% c("Secretary") ~ "5740",
      occupation %in% c("Senior Technician", "Technologist") ~ "1555",
      occupation %in% c("State Budget Director") ~ "0120",
      occupation %in% c("Tax Advisor") ~ "0930",
      occupation %in% c("Primary School Teacher", "Middle School Teacher", "Elementary School Teacher") ~ "2310",
      occupation %in% c("High School Teacher", "Secondary School Teacher") ~ "2320",
      occupation %in% c("Teacher") ~ "2360",   
      occupation %in% c("Teaching Assistant") ~ "2545",
      occupation %in% c("Veterinarian", "Veterinary Inspector") ~ "3250",
      occupation %in% c("Waitress") ~ "4110",
      occupation %in% c("Web Developer") ~ "1031",
      occupation %in% c("Welder") ~ "8140",
      TRUE ~ NA)) %>% 
      mutate(occupation = as.numeric(occupation)) %>% 
      left_join(wage) %>% 
      mutate(occupation = ln_wage) %>% 
      select(-ln_wage)

# make everything numeric for imputation
party_leaders <- party_leaders %>% 
  mutate(across(c(gender, education, age, age_squared, occupation, marital_status, 
                  divorce, minority, criminal_record), as.numeric))

saveRDS(party_leaders, paste0(cleandata_path, "/leaders/party_leaders.rds"))
