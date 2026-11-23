use "$POPULISM_DROPBOX/data/rawdata/downloaded/not_share/Manifesto/MPDataset_MPDS2021a_stata14.dta", clear

gen manifestoYear = year(edate)
rename (party partyname countryname) (Y_manifesto_party_id manifesto_party_name manifesto_country_name)

egen partyYearManifesto = group(edate Y_manifesto_party_id)
distinct partyYearManifesto 
assert r(ndistinct) == _N

gen Y_manifesto_key = _n

/* Drop old manifestos*/
drop if manifestoYear<1980

/* Drop four digits manifestos: The four digit variables (per1011-per7062) are sub-categories mostly addressing issues in transitional democracies in (mostly) Central and Eastern European countries. However, they were introduced in version one of the coding instructions and were gradually abandoned in most of the countries as the issues could also be coded into the three digit main categories.*/
drop per1011-per7062

/* Drop new manifesto codes only recently introduced*/
drop per103_1-per703_2

save "$POPULISM_DROPBOX/data/cleandata/manifesto/manifesto.dta", replace
