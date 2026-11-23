# *************************************************************************
# Description ----
# *************************************************************************

# This file cleans data, which are used for model estimation, for all waves

# This code runs before model estimation and provide cleaned data

# *************************************************************************
# SETUP ----
# *************************************************************************
source("code/util/commonVars.R")
library(haven)
library(randomForest)
library(dplyr)
library(tidyr)
library(conflicted)
library(stringr)
library(data.table)


conflict_prefer("intersect", "base")
conflicts_prefer(dplyr::summarize)

ONLY_SWEDEN <- FALSE
NO_DEMOGRAPHICS <- file.exists("code/noDemographicsConfig")
WITHCZSK <- file.exists("code/withCZSKConfig")
NOGRIT <- file.exists("code/noGRITConfig")
PLEDGE <- file.exists("code/pledgeConfig")
STYLE <- file.exists("code/styleGoogleConfig")
COHORT <- file.exists("code/cohortConfig")
EASTSOUTH <- file.exists("code/eastSouthConfig")
WESTNORTH <- file.exists("code/westNorthConfig")

# Load X data (voter demographics and opinions)
if (NO_DEMOGRAPHICS) {
  X <- read_dta(pathWVS_Data_no_Dem)
} else if (WITHCZSK) {
  X <- read_dta(pathWVS_Data_with_CZSK)
} else if (NOGRIT) {
  X <- read_dta(pathWVS_Data_NoGRIT)
} else if (EASTSOUTH) {
  X <- read_dta(pathWVS_Data_EastSouth)
} else if (WESTNORTH) {
  X <- read_dta(pathWVS_Data_WestNorth)
} else {
  X <- read_dta(pathWVS_Data)
}

if (COHORT){
  X <- data.frame(X) %>% 
    select(-X_dem_age)
} else if (!NO_DEMOGRAPHICS){
  X <- data.frame(X) %>% 
    select(-X_dem_cohort)  
}


# Load Z data (party positions)
if (WITHCZSK) {
  Z <- readRDS(manifestoModelData_withCZSK)
} else {
  Z <- readRDS(manifestoModelData)
}

if(NO_DEMOGRAPHICS){
  Z <- Z %>% 
    select(-c(gender, education, age, occupation, marital_status, divorce, minority, femShare))
}

if (PLEDGE){
  Z <- Z %>% 
  mutate(party_date = paste0(Y_manifesto_party_id, "_", date))
  
  pledges <- readRDS(paste0(cleandata_path, "/manifesto/pledges.rds"))

  labels <- sapply(Z, function(col) attr(col, "label"), simplify = FALSE)
  
  for (col in colnames(pledges)) {
    if (startsWith(col, "per") && col %in% names(labels)) {
      attr(pledges[[col]], "label") <- labels[[col]]
    }
  }
  
  Z <- Z %>% 
    select(-starts_with("per"), pervote) 
  
  Z <- pledges %>% 
    inner_join(Z) %>% 
    select(-party_date)
}



Z <- data.frame(Z)
LABELS <- sapply(Z, function(x) attributes(x)[["label"]])

# FOR TESTING ONLY
if (ONLY_SWEDEN) {
  X <- X[X$BB_Country == "Sweden", ]
}

# define waves to track
grps <- unique(X$BB_Wave_num)
grps <- grps[order(grps)]

# exclude people who did not vote
X <- X[!is.na(X$Y_manifesto_key), ]

# drop parties with missing values
if(STYLE) {
  varsZ <- c(grep("^per[1-7]", colnames(Z), value = TRUE), 
             "top_insult_score", "total_words", "average_words_clause", "mattr",
             "gender", "education", "age", "occupation", "marital_status", "divorce", "minority", "femShare")  
} else if (!NO_DEMOGRAPHICS) {
  varsZ <- c(grep("^per[1-7]", colnames(Z), value = TRUE), 
              "gender", "education", "age", "occupation", "marital_status", "divorce", "minority", "femShare")
} else {
  varsZ <- grep("^per[1-7]", colnames(Z), value = TRUE)
}


ccz <- (complete.cases((Z[, varsZ])))
print("missing data in parties:")
print(paste0(Z$Y_manifesto_key, Z$BB_Wave_num)[!ccz])
Z <- Z[ccz, ]


# Drop parties with less than 10 voters in data
y.tmp <- paste0(X$Y_manifesto_key, X$BB_Wave_num)
dat.ap <- table(y.tmp)
# y.tmp <- names(dat.ap[dat.ap > 4])
y.tmp <- names(dat.ap[dat.ap > 9])
all.party <- paste0(Z$Y_manifesto_key, Z$BB_Wave_num)
print("missing voters")
has.vot <- (all.party %in% y.tmp)
print(all.party[!has.vot])
Z <- Z[has.vot, ]

# Drop people who voted for parties that are not in CMP:
y.tmp <- paste0(X$Y_manifesto_key, X$BB_Wave_num)
all.party <- paste0(Z$Y_manifesto_key, Z$BB_Wave_num)
print("missing parties")
miss <- unique(paste(
  X$Y_Party_Vote[!(y.tmp %in% all.party)],
  X$BB_Wave_num[!(y.tmp %in% all.party)]
))
print(miss)
X <- X[y.tmp %in% all.party, ]

# Drop countries with only one party
y.tmp <- paste0(X$BB_Country, X$BB_Wave_num)
Z.os.full <- paste0(Z$BB_Country, Z$BB_Wave_num)
tab <- table(Z.os.full)
miss <- names(tab)[tab < 2]
good <- names(tab)[tab > 1]
print("countries with only one party")
print(miss)
X <- X[y.tmp %in% good, ]
Z <- Z[Z.os.full %in% good, ]

X.all <- X
Z.all <- Z

# X
varsX <- grep("^X_", colnames(X.all), value = T)

# Drop general variables
varsX <- varsX[-grep("^X_general", varsX, value = F)]

# Drop variables with only missing in one wave
varsX <- varsX[sapply(varsX, function(v) {
  all(sapply(grps, function(g) {
    any(!is.na(X[X$BB_Wave_num == g, v]))
  }))
})]

# Drop variables without variations
varsX <- varsX[
  sapply(varsX, function(v) {
    all(sapply(grps, function(g) {
      #any(sd(X[X$BB_Wave_num == g, v], na.rm = T) > 0)
      xg <- X[X$BB_Wave_num == g, ][[v]]
      any(sd(xg, na.rm = TRUE) > 0)
    }))
  })
]

X <- X[, varsX]

# Export the number of variables used
length(varsX)
if (!isTRUE(getOption("populist.inputs.only",FALSE)) & !NO_DEMOGRAPHICS & ! EASTSOUTH & ! WESTNORTH) {
  savetexvalue::save_tex_value(values = length(varsX),
                               names = "numXVariables", 
                               file_name = "output/values/modelValues.tex", 
                               digits = 0)
}


# Id for respondent
stopifnot(length(unique(X.all$BB_Id)) == length(X.all$BB_Id))
rownames(X) <- X.all$BB_Id


# Z
# Drop variables with only missing in one wave
varsZ <- varsZ[
  sapply(varsZ, function(v) {
    all(sapply(grps, function(g) {
      any(!is.na(Z[Z$BB_Wave_num == g, v]))
    }))
  })
]

# Drop no variation

varsZ <- varsZ[
  sapply(varsZ, function(v) {
    all(sapply(grps, function(g) {
      any(sd(Z[Z$BB_Wave_num == g, v], na.rm = T) > 0)
    }))
  })
]


Z <- Z[, varsZ]

# Load names for each party
rownames(Z) <- paste0(Z.all$Y_manifesto_key, Z.all$BB_Wave_num)


# Keep only countries in both data sets
for (g in grps) {
  cntr <- intersect(
    unique(X.all$BB_Country[X.all$BB_Wave_num == g]),
    unique(Z.all$BB_Country[Z.all$BB_Wave_num == g])
  )
  print("dropped countries in wave")
  print(g)
  print(unique(X.all$BB_Country[X.all$BB_Wave_num == g & !(X.all$BB_Country %in% cntr)]))
  print(unique(Z.all$BB_Country[Z.all$BB_Wave_num == g & !(Z.all$BB_Country %in% cntr)]))

  # Take out from X and Z
  X <- X[X.all$BB_Wave_num != g | X.all$BB_Country %in% cntr, ]
  Z <- Z[Z.all$BB_Wave_num != g | Z.all$BB_Country %in% cntr, ]
  print("X rows before and after")
  print(nrow(X.all))
  X.all <- X.all[X.all$BB_Wave_num != g | X.all$BB_Country %in% cntr, ]
  print(nrow(X.all))
  print("Z rows before and after")

  print(nrow(Z.all))
  Z.all <- Z.all[Z.all$BB_Wave_num != g | Z.all$BB_Country %in% cntr, ]
  print(nrow(Z.all))
}

# Test for missing values
for (x in varsX) {
  stopifnot(all(!is.na(X[, x])))
}

for (z in varsZ) {
  stopifnot(all(!is.na(Z[, z])))
}
