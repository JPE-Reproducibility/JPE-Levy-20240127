# This code uses IPUMS 2023 data to compute weekly wages by occupations
source("code/util/commonVars.R")

library(haven)

ipums_input <- file.path(DB.folder, "data/rawdata", "downloaded", "public", "IPUMS", "cps_00047.dta")
if (!file.exists(ipums_input)) {
  stop("Required IPUMS source file not found: ", ipums_input)
}

ipums_output_dir <- file.path(datasets_path, "IPUMS")
dir.create(ipums_output_dir, recursive = TRUE, showWarnings = FALSE)

df=read_dta(ipums_input)

df = df |>
  filter(year==2023&!is.na(wkswork1)&!is.na(incwage)&!is.na(occ))|>
  filter(incwage!=0&incwage!=99999999)|>
  filter(occ!=0&wkswork1!=0)|>
  mutate(wage=incwage/wkswork1)

df_wage = df|>
  select(occ, wage,asecwt)|>
  # drop top 10 & bottom 10 % wage
  filter(wage>quantile(wage,0.1)&wage<quantile(wage,0.9))|>
  group_by(occ)|>
  summarise(
    wage=weighted.mean(
      wage, asecwt, na.rm=T
    ),
    n=n()
  )|>
  ungroup()|>
  mutate(ln_wage=log(wage))

# save as dta
write_dta(df_wage, file.path(ipums_output_dir, "wage_occ_2023.dta"))
