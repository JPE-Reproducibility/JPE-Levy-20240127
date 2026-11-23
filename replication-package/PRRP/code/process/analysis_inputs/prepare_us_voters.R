# Final U.S. inputs for the enabled counterfactual maps; data/intdata stays disposable.
source("code/util/commonVars.R")
us <- haven::read_dta(file.path(DB.folder, "data/intdata", "WVS", "wvs_clean_4to6_waves_with_US.dta"))
us <- us[us$BB_Wave_num == 6 & us$BB_Country == "US", ]
us <- us[, c("BB_Id", "BB_Country", "BB_Wave_num", "BB_Weight_CW", grep("^X_(?!general)", names(us), value=TRUE, perl=TRUE))]
stopifnot(nrow(us)>0, !anyDuplicated(us$BB_Id))
saveRDS(us, file.path(cleandata_path, "WVS", "us_voters.rds"))
