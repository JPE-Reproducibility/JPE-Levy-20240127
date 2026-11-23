
#load libraries
# Run analyze_voter_parties_correlation.R 
# to get the MANIFESTO data set with all the desired variables
# and the complete 'DATA' with both IVS and Manifesto

library(fuzzyjoin)
library(dplyr)
library(data.table)

source("code/util/commonVars.R")
source("code/process/paths.R")

# store the folder path where the data is saved
folderPath <- file.path(raw_path, "public", "femaleShare")

# Prepare for matching with new datasets (to add female share) ------------

# Read WVS data from dropbox:
WVS <- read_dta(file = pathWVS_Data) 

# If running analysis on ALL waves (1-6), ensure correct data file:
# WVS <- read_dta(file = paste0(intermediate_data_path, "/WVS/wvs_clean_1to6_waves.dta"))

# Add wave numbers
WVS <- WVS %>%
  left_join(waveLabelsDF, by = c("BB_Wave_num"="waveNum"), keep = F) %>% 
  as.data.table()

# Load data at the manifesto*year level for merging with voters
MANIFESTO = readRDS(paste0(cleandata_path, "/manifesto/manifesto_populist.rds")) %>%
  select(Y_manifesto_key, parfam)

DATA <- merge(WVS, MANIFESTO, by = "Y_manifesto_key")

DATA = as.data.table(DATA)

# ensure that for each unique party, parlgov key is consistent
DATA <- DATA %>%
  group_by(BB_Party) %>%
  mutate(Y_parlgov_id = ifelse(any(is.na(Y_parlgov_id)), 
                               mean(Y_parlgov_id, na.rm = TRUE), 
                               Y_parlgov_id))

# add specific missing parlgov id's
DATA <- DATA %>%
  mutate(Y_parlgov_id = ifelse(BB_Party == "GB-GBN: Liberal Democrat", 659, Y_parlgov_id)) %>%
  mutate(Y_parlgov_id = ifelse(BB_Party == "IT: Five Star Movement", 2155, Y_parlgov_id))
  

# Add a column to denote party&year combination
DATA$partyYear <- paste0(as.character(DATA$Y_manifesto_party_id), as.character(DATA$BB_Year))

# keep only desired variables in DATA:
# Y_manifesto_party_id; 
# Y_parlgov_id;
# BB_Wave_num;
# BB_Country;
# BB_Party; 
# BB_Year; 
# parfam; 
# partyYear

# keep only one row for each partyYear value using distinct()

# Merge with the Weeks et al. dataset -------------------------------------

load(file.path(folderPath, "Replication_data.RData")) # 'newd'

# Add a column to denote party&year combination

newd <- newd %>% 
  rename(Y_manifesto_party_id = party) %>%
  mutate(partyYear = paste0(as.character(Y_manifesto_party_id), as.character(year))) %>%
  dplyr::select(c("Y_manifesto_party_id", "countryname", "pfem_new2", "partyYear", "year"))

# Check if any partyYear appears more than once (we want a single appearance of a party in a year)
duplicate_partyYear <- newd %>%
  count(partyYear) %>%        # Count occurrences of each partyYear
  filter(n > 1) %>%           # Filter those that appear more than once
  pull(partyYear)             # Extract the values as a vector

# Display the duplicates
duplicate_partyYear

# Fix partyYear duplicates:
# If a party has more than one appearance in one year, it will be impossible to merege 
# To handle duplicates: keep only one appearance of a partyYear, 
# and define the female share as the average female share of that party in that Year.
newd <- newd %>%
  group_by(partyYear) %>%                         # Look at every partyYear 
  mutate(
    pfem_new2 = dplyr::if_else(partyYear %in% duplicate_partyYear, 
                        mean(pfem_new2, na.rm = TRUE), 
                        pfem_new2),               # if more than one appearance- use average
    keep_row = row_number() == 1                  # Mark the row to keep (first one)
  ) %>%
  filter(keep_row) %>%                            # Keep only the marked rows
  dplyr::select(-keep_row) %>%                           # Drop the column that marked the keep
  ungroup()%>%
  mutate(pfem_new2 = ifelse(is.nan(pfem_new2), NA, pfem_new2))  # Mark NaN as NA                                     


# Omit rows with unavailable female share info
validFem <- subset(newd, !(is.na(pfem_new2)))

# create a subset of 'newd' to include only rows with a matching partyYear value in 'DATA'
a <- subset(validFem, partyYear %in% unique(DATA$partyYear))

# create subset of 'newd' to include rows WITHOUT a partyYear match in 'DATA'
# remove rows without female share information
b <- subset(validFem, !(partyYear %in% unique(DATA$partyYear)))

# merge subset 'a' with 'DATA'
DATA <- DATA %>%
  left_join(a %>% dplyr::select(partyYear, pfem_new2, year), 
            by = c("partyYear")) %>%
  rename(originYear = year)

# We've successfully matched party&years that appeared both in DATA and in newd
# For the observations in DATA that were not matched: 
# We will look for an observation of the same party in newd, in a different year
# We will match observations of the same party, that are close enough in time (+- 5 years) 

c <- subset(DATA, !is.na(pfem_new2)) #the subset of DATA that was matched
d <- subset(DATA, is.na(pfem_new2)) #the subset that wasn't matched
d <- d %>% dplyr::select(-pfem_new2)

# Now we handle the unmatched observations in DATA ("d") 
# using observations in newd with no exact party-year match ("b")
# for each partyYear value in "d", we look for the closest partyYear in "b"
# under the constraint that the distance between them cannot exceed 5.

b$partyYear <- as.numeric(b$partyYear) # make partyYear numeric for distance calculations
d$partyYear <- as.numeric(d$partyYear)
c$partyYear <- as.numeric(c$partyYear) # for later merging, partyYear should be of same type

# in "b", add columns to mark the 5 year intervals around each partyYear 
b <- b %>%
  rowwise() %>%
  mutate(plusFive = partyYear + 5,
         minusFive = partyYear - 5,
         partyYearMatch = NA) %>%
  ungroup()

# now we go over each unique partyYear value in the unmatched subset of DATA ('d')
# we find the row in 'b' with the closest partyYear value (no more than 5 year distance)
# Iterate over each unique value of partyYear in the unmatched DATA subset
for (X in unique(d$partyYear)) {
  
  # Check if X is within seq(b$partyYear, b$plusFive) for each row in b
  b <- b %>%
    rowwise() %>%
    mutate(
      in_plus_interval = (X > partyYear & X <= plusFive),
      in_minus_interval = (X >= minusFive & X < partyYear)
    ) %>%
    ungroup()
  
  # Identify rows where X fits either interval
  matching_rows <- b %>%
    filter(in_plus_interval | in_minus_interval)
  
  if (nrow(matching_rows) == 1) {
    # If X fits only one row, assign it to that row's partyYearMatch
    b$partyYearMatch[which(b$partyYear == matching_rows$partyYear)] <- X
  } else if (nrow(matching_rows) > 1) {
    # If X fits multiple rows, select the one with the smallest distance
    best_match <- matching_rows %>%
      mutate(distance = abs(partyYear - X)) %>%
      arrange(distance) %>%
      slice(1)
    
    # Assign X to the best matching row
    b$partyYearMatch[which(b$partyYear == best_match$partyYear)] <- X
  }
  
  # If X doesn't fit any row in the plus interval, check the minus interval
  if (nrow(matching_rows) == 0) {
    matching_rows <- b %>%
      filter(in_minus_interval)
    
    if (nrow(matching_rows) == 1) {
      b$partyYearMatch[which(b$partyYear == matching_rows$partyYear)] <- X
    } else if (nrow(matching_rows) > 1) {
      best_match <- matching_rows %>%
        mutate(distance = abs(partyYear - X)) %>%
        arrange(distance) %>%
        slice(1)
      
      b$partyYearMatch[which(b$partyYear == best_match$partyYear)] <- X
    }
  }
}

# Now we have b$partyYearMatch, which denotes the closest d$partyYear value 
# we want to merge b into d, based on b$partyYearMatch == d$partyYear

b <- b %>% rename(originPartyYear = partyYear)
b <- b %>% rename(partyYear = partyYearMatch)
b <- b %>% rename(originYear = year)

d <- d %>% left_join(b %>% dplyr::select(partyYear, pfem_new2, originYear, originPartyYear), 
                     by = c("partyYear"))

DATA_afterWeeks <- bind_rows(c, d)


paste0("share of matched rows in data: ", 
       length(na.omit(DATA_afterWeeks$pfem_new2))/nrow(DATA_afterWeeks),
       " share of matched party appearances in data: ",
       length(unique(DATA_afterWeeks[!is.na(DATA_afterWeeks$pfem_new2),]$partyYear))/length(unique(DATA$partyYear)))

# handle duplicate originYear columns, merge into one
DATA_afterWeeks <- DATA_afterWeeks %>% 
  mutate(originYear = dplyr::coalesce(originYear, originYear.x, originYear.y)) %>%
  dplyr::select(-originYear.x, -originYear.y) # Remove extra columns

# Use the Adams et al. data to match the remaining unmatched rows ---------

# Create a subset of rows in DATA that we were unable to match using Weeks et al.
# add a parlgov-based key for matching with the new dataset
DATA_afterWeeks$parlgovYear <- paste0(as.character(DATA_afterWeeks$Y_parlgov_id), 
                                     as.character(DATA_afterWeeks$BB_Year))
DATA_remaining <- DATA_afterWeeks[is.na(DATA_afterWeeks$pfem_new2),]

# load the Adams et al. dataset ('multilevel_data')
load(file.path(folderPath, "multilevel_1-5-22.Rdata"))

# filter out observations without parlgov id (no key for merging)
multilevel_data <- multilevel_data %>% subset(!is.na(from_parlgov_id))

#create 'parlgovYear' column - a key for merging by party&year
multilevel_data$parlgovYear <- paste0(as.character(multilevel_data$from_parlgov_id), 
                                      as.character(multilevel_data$year))

# filter multilevel_data to only keep columns relevant for merging
multilevel_data <- multilevel_data %>%
  dplyr::select(from_parlgov_id, year, from_pfeml, parlgovYear)

# To add the female share (multilevel_data$from_pfmel) to our data,
# first ensure that each party&year combination appears once 
# Check for 'parlgovYear' duplicates in the multilevel_data 
duplicate_parlgovYear <- multilevel_data %>%
  count(parlgovYear) %>%
  filter(n > 1) %>%
  pull(parlgovYear)


# Handle duplicates: 
# if a parlgovYear value has more than one appearance, average `from_pfmel`
multilevel_data <- multilevel_data %>%
  group_by(parlgovYear) %>%
  mutate(
    from_pfeml = dplyr::if_else(parlgovYear %in% duplicate_parlgovYear, 
                         mean(from_pfeml, na.rm = TRUE), 
                         from_pfeml),
    keep_row = row_number() == 1 # Mark first row to keep
  ) %>%
  filter(keep_row) %>%
  dplyr::select(-keep_row) %>%
  ungroup()%>%
  mutate(from_pfeml = ifelse(is.nan(from_pfeml), 
                             NA,  # Mark NaN as NA
                             100*from_pfeml)) # change to 1-100 scale (as in Weeks)                                     

# Omit rows with unavailable female share info
multilevel_data <- multilevel_data %>% subset(!is.na(from_pfeml))

# create a subset of 'multilevel_data' with only rows with a matching partyYear value in 'DATA'
a_adams <- subset(multilevel_data, parlgovYear %in% DATA_remaining$parlgovYear)


# Match to observations in the original data with the exact parlgovYear
DATA_remaining <- DATA_remaining %>%
  left_join(a_adams %>% dplyr::select(parlgovYear, from_pfeml), 
            by = "parlgovYear") %>%
  mutate(pfem_new2 = dplyr::coalesce(pfem_new2, from_pfeml)) # Use from_pfeml where pfem_new2 is missing

paste0("Using the Adams et al. dataset, we covered ", 
       sum(!is.na(DATA_remaining$pfem_new2)) / nrow(DATA_remaining), 
       " of the remaining observations")

# Handle unmatched rows by matching same party, different year, within +-5 year range
# Create subset for rows in multilevel_data that didn't match any row in our data
b_adams <- subset(multilevel_data, !(parlgovYear %in% DATA_remaining$parlgovYear)) 

# Create subset for rows in our data that were not matched

c_adams <- subset(DATA_remaining, !(is.na(pfem_new2))) #the part in DATA_remaining that was matched
d_adams <- subset(DATA_remaining, is.na(pfem_new2)) #the subset that wasn't matched
d_adams <- d_adams %>% dplyr::select(-pfem_new2)

# ensure parlgovYear is numeric to enable distance calculation
# remove rows with missing parlgov id
b_adams <- b_adams %>% #the adams et al. subset with valid femshare but no exact parlgovYear match
  filter(!is.na(from_parlgov_id)) %>% #remove rows with invalid key
  mutate(parlgovYear = as.numeric(parlgovYear)) 
d_adams <- d_adams %>% # the remaining data we were unable to match
  filter(!is.na(Y_parlgov_id)) %>%
  mutate(parlgovYear = as.numeric(parlgovYear)) %>%
  dplyr::select(-from_pfeml)
c_adams$parlgovYear = as.numeric(c_adams$parlgovYear) #consistent variable type for later merging

# in "b_adams", add columns to mark the 5 year intervals around each partyYear
b_adams <- b_adams %>%
  rowwise() %>%
  mutate(
    plusFive = parlgovYear + 5,
    minusFive = parlgovYear - 5,
    parlgovYearMatch = NA
  ) %>%
  ungroup()

# Perform matching within ±5 years by the same logic as before:
# go over unique parlgovYear values in the unmatched subset of our remaining data ('d_adams')
for (X in unique(d_adams$parlgovYear)) {
  
  # Check if X is within the plus and minus intervals for each row in b_adams
  b_adams <- b_adams %>%
    rowwise() %>%
    mutate(
      in_plus_interval = (X > parlgovYear & X <= parlgovYear + 5),
      in_minus_interval = (X >= parlgovYear - 5 & X < parlgovYear)
    ) %>%
    ungroup()
  
  # Identify rows where X fits either interval
  matching_rows <- b_adams %>%
    filter(in_plus_interval | in_minus_interval)
  
  if (nrow(matching_rows) == 1) {
    # If X fits only one row, assign it to that row's parlgovYearMatch
    b_adams$parlgovYearMatch[which(b_adams$parlgovYear == matching_rows$parlgovYear)] <- X
  } else if (nrow(matching_rows) > 1) {
    # If X fits multiple rows, select the one with the smallest distance
    best_match <- matching_rows %>%
      mutate(distance = abs(parlgovYear - X)) %>%
      arrange(distance) %>%
      slice(1)
    
    # Assign X to the best matching row
    b_adams$parlgovYearMatch[which(b_adams$parlgovYear == best_match$parlgovYear)] <- X
  }
  
  # If X doesn't fit any row in the plus interval, check the minus interval
  if (nrow(matching_rows) == 0) {
    matching_rows <- b_adams %>%
      filter(in_minus_interval)
    
    if (nrow(matching_rows) == 1) {
      b_adams$parlgovYearMatch[which(b_adams$parlgovYear == matching_rows$parlgovYear)] <- X
    } else if (nrow(matching_rows) > 1) {
      best_match <- matching_rows %>%
        mutate(distance = abs(parlgovYear - X)) %>%
        arrange(distance) %>%
        slice(1)
      
      b_adams$parlgovYearMatch[which(b_adams$parlgovYear == best_match$parlgovYear)] <- X
    }
  }
}

# Add the matched rows back to `d_adams`
b_adams <- b_adams %>% rename(originParlgovYear = parlgovYear) # keep track of original year
b_adams <- b_adams %>% rename(parlgovYear = parlgovYearMatch) # rename for merging

d_adams <- d_adams %>%
  left_join(b_adams %>% dplyr::select(parlgovYear, from_pfeml, originParlgovYear), 
            by = "parlgovYear") 

# combine the two DATA_remaining subsets:
# 'c_adams' (observations matched with exact parlgov id & year)
# with 'b_adams' (observations matched with approximated year)
DATA_afterAdams <- bind_rows(c_adams, d_adams)


# merge DATA_afterAdams into DATA_afterWeeks

DATA_afterWeeks <- DATA_afterWeeks %>%
  mutate(parlgovYear = as.numeric(parlgovYear),
         from_pfeml = NA) 

# for every row in DATA_afterWeeks, 
# find the corresponding row in DATA_afterAdams based on parlgovYear
# plugin the from_pfeml value

DATA_afterWeeks <- DATA_afterWeeks %>%
  #mutate(parlgovYear = as.numeric(parlgovYear)) %>%
  rowwise() %>% # apply operation row by row
  mutate(
    from_pfeml = {
      # look for a row with the same parlgovYear key in the data after matching with adams
      # if there exists more than one matching row, use the first one to merge 
      matched_pfeml <- DATA_afterAdams$from_pfeml[DATA_afterAdams$parlgovYear == parlgovYear]
      if (length(matched_pfeml) > 0) matched_pfeml[1] else NA
    },
    originParlgovYear = {
      matchedOrigin <- DATA_afterAdams$originParlgovYear[DATA_afterAdams$parlgovYear == parlgovYear]
      if (length(matchedOrigin) > 0) matchedOrigin[1] else NA
    }
  ) %>%
  ungroup()

# define the resulting df as 'final data'
finalData <- DATA_afterWeeks 

# ensure that no row has both from_pfeml and pfem_new2 values
# Identify rows where both pfem_new2 and from_pfeml have non-NA values
conflicting_rows <- finalData %>%
  filter(!is.na(pfem_new2) & !is.na(from_pfeml))

# Check if any such rows exist
if (nrow(conflicting_rows) > 0) {
  print("There are rows where both pfem_new2 and from_pfeml have values:")
  print(conflicting_rows)
} else {
  print("No conflicts: Every row has at most one non-NA value in pfem_new2 and from_pfeml.")
}

# Save a dataframe of unmatched combinations for the next steps
# get the combinations with invalid female share
total_combinations <- finalData %>%
  distinct(BB_Party, BB_Year, BB_Country, Y_manifesto_party_id, Y_parlgov_id,
           partyYear, parlgovYear, Y_partyfacts_id)

# get the combinations with a valid female share
valid_combinations <- finalData %>%
  filter(!is.na(pfem_new2) | !is.na(from_pfeml)) %>%
  distinct(BB_Party, BB_Year, BB_Country, Y_manifesto_party_id, Y_parlgov_id,
           partyYear, parlgovYear, Y_partyfacts_id) 


missing_combinations <- setdiff(total_combinations, valid_combinations)



# handle remaining missing combinations using global legislator db -----------------------------------

globalDat <- haven::read_dta(file.path(folderPath, "global_legislator_database.dta"))

# THIS DATASET GIVES INDIVIDUAL-LEVEL INFO ON LEGISLATORS.
# Filter out countries missing in our original data

globalDat <- globalDat %>% 
  filter(country_name %in% finalData$BB_Country)

# create a new df - convert from legislator level to party level 
aggregateGl <- globalDat %>% 
  select(c("country_name", "country_code", "gender", "party", "year_of_election", 
           "parliamentary_period")) %>%
  mutate(
    gender = ifelse(gender == "F", 1, 0),  # Convert gender to binary (F = 1, M = 0)
    partyPeriod = paste0(as.character(party), ":", as.character(parliamentary_period))
  ) %>%
  group_by(partyPeriod) %>%
  mutate(
    femShare = 100 * (sum(gender, na.rm = TRUE) / n())  # Calculate femShare as a percentage
  ) %>%
  slice(1) %>%  # Keep only one observation of each unique party+period
  ungroup()


# manually assign parlgov id to parties

aggregateGl$Y_parlgov_id <- NA

# Assign Y_parlgov_id using case_when
aggregateGl <- aggregateGl %>%
  mutate(
    Y_parlgov_id = case_when(
      country_name == "Estonia" & party == "Conservative Peoples Party of Estonia" ~ 417,
      country_name == "Germany" & party == "The Left Party" ~ 791,
      country_name == "Hungary" & party == "MSZP" ~ 1591,
      country_name == "Hungary" & party == "Fidesz" ~ 921,
      country_name == "Italy" & party == "Five Star Movement" ~ 2155,
      country_name == "Lithuania" & party == "TT - Order and Justice Political Group" ~ 1421,
      country_name == "Netherlands" & party == "50PLUS (50+)" ~ 2109,
      country_name == "Netherlands" & party == "Party for the Animals (PvdD)" ~ 990,
      country_name == "Netherlands" & party == "Reformed Political Party (SGP)" ~ 1251,
      country_name == "Poland" & party == "Civic Platform (PO)" ~ 512,
      country_name == "Poland" & party == "Kukiz'15" ~ 2600,
      country_name == "Poland" & party == "Law and Justice (PiS)" ~ 528,
      country_name == "Poland" & party == "Modern Party" ~ 2601,
      country_name == "Poland" & party == "Polish Peasant Party (PSL)" ~ 664,
      country_name == "Portugal" & party == "CDS-PP" ~ 251,
      country_name == "Slovenia" & party == "Democratic Party of Pensioners of Slovenia" ~ 1587,
      country_name == "Spain" & party == "Citizens (Cs)" ~ 2375,
      country_name == "Spain" & party == "Basque Country Gather (EH Bildu)" ~ 2603,
      country_name == "Switzerland" & party == "Conservative Democratic Party" ~ 1213,
      country_name == "Switzerland" & party == "Green Liberal Party (PVL)" ~ 308,
      country_name == "Switzerland" & party == "Geneva Citizen's Movement (MCG)" ~ 2599,
      TRUE ~ NA_real_  # Default case
    )
  )

# Filter out rows in aggregateGl without parlgov id
aggregateGl <- aggregateGl %>% filter(!is.na(Y_parlgov_id))
# create key baded on parlgov&year
aggregateGl$parlgovYear <- paste0(as.character(aggregateGl$Y_parlgov_id), 
                                      as.character(aggregateGl$year_of_election))

# use the new key to merge female share into missing_observations
# perform a range-based join into missing_combinations
aggregateGl$parlgovYear <- as.numeric(aggregateGl$parlgovYear)
missing_combinations$parlgovYear <- as.numeric(missing_combinations$parlgovYear)

matched_data <- fuzzy_left_join(
  missing_combinations,
  aggregateGl,
  by = c("parlgovYear" = "parlgovYear"),
  match_fun = function(x, y) abs(x - y) <= 5  # Match within ±5 years
)

# Select desired columns and update missing_combinations$femShare
missing_combinations <- matched_data %>%
  rename(parlgovYear = parlgovYear.x, Y_parlgov_id = Y_parlgov_id.y,
         originGlYear = year_of_election) %>%  # Merge femShare from aggregateGl
  select(-Y_parlgov_id.x, -parlgovYear.y) 


# create a df of combinations we matched using legislator data, name it "dfToMerge"

dfToMerge <- missing_combinations %>% select(c("parlgovYear", "originGlYear", "femShare")) %>%
  filter(!is.na(femShare))

# merge dfToMerge with finalData to ADD NEW FEMALE SHARE INFO TO FINAL DATA

finalData <- finalData %>%
  mutate(originGlYear = NA,
         femShare = NA)

for (i in unique(dfToMerge$parlgovYear)) { #go over every party-year combo
  tempFem <- dfToMerge$femShare[dfToMerge$parlgovYear == i] #keep female share
  tempOrigin <- dfToMerge$originGlYear[dfToMerge$parlgovYear == i] #keep original year
  
  # Assign tempFem and tempOrigin to all matching rows in finalData
  finalData$femShare[!is.na(finalData$parlgovYear) & finalData$parlgovYear == i] <- tempFem #plug this female share into the corresponding row in finalData
  finalData$originGlYear[!is.na(finalData$parlgovYear) & finalData$parlgovYear == i] <- tempOrigin
}


# Ensure single female-share in each row, and merge into one column -------


# verify that each observation in finalData has only one female-share value
finalData <- finalData %>%
  rowwise() %>%
  mutate(valid_count = sum(!is.na(c(femShare, from_pfeml, pfem_new2)))) %>%
  ungroup()

violations <- finalData %>%
  filter(valid_count > 1)

# Output results
if (nrow(violations) > 0) {
  print("Some rows have more than one valid value:")
  print(violations)
} else {
  print("All rows satisfy the condition of having at most one valid female share value.")
}

# Merge into one `femShare` column
finalData <- finalData %>%
  mutate(femShare = dplyr::coalesce(femShare, from_pfeml, pfem_new2)) %>%
  select(-from_pfeml, -pfem_new2, -valid_count)

#verify that each observation in finalData has only one 'origin-year' value
finalData <- finalData %>%
  rowwise() %>%
  mutate(valid_count = sum(!is.na(c(originGlYear, originParlgovYear, originPartyYear)))) %>%
  ungroup()

# Check for violations
violations <- finalData %>%
  filter(valid_count > 1)

if (nrow(violations) > 0) {
  stop("Some rows have more than one valid origin year. Please resolve conflicts.")
}

# Merge columns into `originFemaleYear`
finalData <- finalData %>%
  mutate(
    originFemaleYear = dplyr::coalesce(originGlYear, originParlgovYear, originPartyYear)
  ) %>%
  select(-originGlYear, -originParlgovYear, -originPartyYear, -originYear, 
         -parlgovYear, -partyYear, -valid_count) 

# if no originYear is available, use BB_Year
finalData <- finalData %>%
  mutate(originFemaleYear = ifelse(is.na(originFemaleYear), BB_Year, originFemaleYear))
 

# count share of rows with valid female share, keep subset of unma --------
total_combinations <- finalData %>%
  distinct(BB_Party, BB_Year, BB_Country, Y_manifesto_party_id, Y_parlgov_id,
           originFemaleYear, Y_partyfacts_id)

valid_combinations <- finalData %>%
  filter(!is.na(femShare)) %>%
  distinct(BB_Party, BB_Year, BB_Country, Y_manifesto_party_id, Y_parlgov_id,
           originFemaleYear, Y_partyfacts_id) 

share_valid <- nrow(valid_combinations) / nrow(total_combinations)

missing_combinations2 <- setdiff(total_combinations, valid_combinations)

# Print the results
cat("Total unique BB_Party & BB_Year combinations:", nrow(total_combinations), "\n")
cat("Unique combinations with valid femShare:", nrow(valid_combinations), "\n")
cat("Share of combinations with valid femShare:", round(share_valid * 100, 2), "%\n")


saveRDS(finalData, file = paste0(cleandata_path, "/ivs/ivs_femaleShare.RDS"))


# Weighted share of parties w/o female share ----------------------------------------------

# Create a dummy for is.femShare == 1 if female share data is available
# Calculate the weighted average of is.femShare (use BB_Weight_CW)
# If weighted average is close enough to 1- most missing parties are relatively small

finalData$is.femShare <- ifelse(!is.na(finalData$femShare), 1, 0)
total_combinations2 <- finalData %>%
  distinct(BB_Party, BB_Year, BB_Country, Y_manifesto_party_id, Y_parlgov_id,
           originFemaleYear, Y_partyfacts_id, is.femShare, BB_Weight_CW)
weighted.coverage <- weighted.mean(total_combinations2$is.femShare, 
                                   total_combinations2$BB_Weight_CW,
                                   na.rm = T)
paste0("After weighting the observations, the female-share coverage is: ", weighted.coverage)
