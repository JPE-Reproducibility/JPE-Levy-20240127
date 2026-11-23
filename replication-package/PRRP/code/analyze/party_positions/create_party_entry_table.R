# Create the Appendix table of Wave 6 PRRP entries absent from Wave 4.
library(dplyr)
library(haven)
library(kableExtra)
library(stringr)

source("code/util/commonVars.R")
# Load the final IVS party-choice data after the paper's party-support filters.
ivs <- read_dta(pathWVS_Data)

manifesto_names <- bind_rows(
  readRDS(manifestoModelData),
  readRDS(manifestoModelData_withCZSK)
) %>%
  mutate(Y_manifesto_party_id = as.numeric(zap_labels(Y_manifesto_party_id))) %>%
  arrange(BB_Country, Y_manifesto_party_id, desc(BB_Wave_num)) %>%
  group_by(BB_Country, Y_manifesto_party_id) %>%
  summarise(manifesto_party_name = dplyr::first(manifesto_party_name), .groups = "drop")

# Fill names for party choices that are in IVS but not in manifesto rows.
manual_names <- tibble::tribble(
  ~Y_manifesto_party_id, ~manual_party_name,
  82720, "Dawn - National Coalition",
  34730, "Independent Greeks",
  43902, "Geneva Citizen Movement",
  96725, "We Are Family"
)

# Assign manual absence reasons, using the original table where available.
manual_reasons <- tibble::tribble(
  ~party, ~reason,
  "Conservative People's Party of Estonia", "Founded in 2012",
  "Alternative for Germany", "Founded in 2013",
  "Golden Dawn", "Very low support",
  "Independent Greeks", "Founded in 2012",
  "Brothers of Italy", "Founded in 2012 (split from People of Freedom)",
  "Forum for Democracy", "Founded in 2016",
  "Sweden Democrats", "Low support (not asked in survey)",
  "Geneva Citizen Movement", "Did not run in federal elections",
  "United Kingdom Independence Party", "Low support (not asked in survey)"
)

# Add known earlier alternatives that are not observed in the final IVS data.
manual_alternatives <- tibble::tribble(
  ~country, ~manual_alternative,
  "Estonia", "Independence Party",
  "Germany", "National Democratic Party",
  "UK", "British National Party"
)

# Clean the IVS party labels before writing them to the table.
normalize_party_name <- function(x) {
  x <- str_remove(x, "^[^:]+:\\s*")
  x <- str_replace_all(x, "\u00c3\u0161", "U")
  x <- str_replace_all(x, "\u00c2", "")
  x <- str_replace_all(x, "\u00e2\u20ac\u2122", "'")
  x <- str_replace_all(x, "\u00b4", "'")
  x <- iconv(x, to = "ASCII//TRANSLIT")
  x
}

# Use manifesto names when available and IVS labels only as a fallback.
party_names <- ivs %>%
  filter(Y_No_Party_Vote == 0) %>%
  mutate(party_name = normalize_party_name(BB_Party)) %>%
  distinct(BB_Country, Y_manifesto_party_id, party_name) %>%
  left_join(manifesto_names, by = c("BB_Country", "Y_manifesto_party_id")) %>%
  left_join(manual_names, by = "Y_manifesto_party_id") %>%
  mutate(party_name = normalize_party_name(coalesce(manual_party_name, manifesto_party_name, party_name))) %>%
  group_by(BB_Country, Y_manifesto_party_id) %>%
  summarise(party_name = dplyr::first(party_name), .groups = "drop")

# Identify Wave 6 PRRP party choices that were not observed in Wave 4.
wave4_parties <- ivs %>%
  filter(BB_Wave_num == 4, Y_No_Party_Vote == 0) %>%
  distinct(Y_manifesto_party_id)

entry_parties <- ivs %>%
  filter(BB_Wave_num == 6, Y_FR_Pop == 1, Y_No_Party_Vote == 0) %>%
  anti_join(wave4_parties, by = "Y_manifesto_party_id") %>%
  distinct(BB_Country, Y_manifesto_party_id) %>%
  left_join(party_names, by = c("BB_Country", "Y_manifesto_party_id"))

# List same-country Wave 4 PRRP/RRP alternatives observed in IVS.
alternative_parties <- ivs %>%
  filter(BB_Wave_num == 4, Y_FR_Pop == 1, Y_No_Party_Vote == 0) %>%
  distinct(BB_Country, Y_manifesto_party_id) %>%
  left_join(party_names, by = c("BB_Country", "Y_manifesto_party_id")) %>%
  group_by(BB_Country) %>%
  summarise(
    alternative = paste(sort(unique(party_name)), collapse = ", "),
    .groups = "drop"
  )

# Keep the table columns used in the paper.
party_entry_table <- entry_parties %>%
  left_join(alternative_parties, by = "BB_Country") %>%
  mutate(
    country = BB_Country,
    party = party_name,
    alternative = coalesce(alternative, ""),
    .keep = "none"
  ) %>%
  left_join(manual_reasons, by = "party") %>%
  left_join(manual_alternatives, by = "country") %>%
  mutate(alternative = if_else(alternative == "", manual_alternative, alternative)) %>%
  mutate(alternative = if_else(is.na(alternative), "", alternative)) %>%
  arrange(country, party) %>%
  select(country, party, reason, alternative)

# Save the generated table as a LaTeX tabular using kable. We keep the four
# columns separate (Country, Party Name, Reason, Alternative) and rely on
# fixed-width wrapping columns below so the table fits in portrait (no
# landscape).
party_entry_kable <- kbl(
  party_entry_table,
  booktabs = TRUE,
  format = "latex",
  escape = TRUE,
  align = c("l", "l", "l", "l"),
  linesep = "",
  col.names = c(
    "Country",
    "Party Name",
    "Reason for Absence in 2005--2009",
    "Alternative Radical-Right Parties"
  )
)

row_end <- rawToChar(as.raw(c(92, 92)))
kable_lines <- strsplit(as.character(party_entry_kable), "\n", fixed = TRUE)[[1]]

# Use raggedright fixed-width columns so the four columns wrap and fit the
# portrait \textwidth (16.6cm: letter paper, 2.5cm side margins).
tabular_line <- grep("\\begin{tabular}", kable_lines, fixed = TRUE)[1]
kable_lines[tabular_line] <- paste0(
  "\\begin{tabular}[t]{",
  ">{\\raggedright\\arraybackslash}p{2.5cm}",
  ">{\\raggedright\\arraybackslash}p{3.8cm}",
  ">{\\raggedright\\arraybackslash}p{4.3cm}",
  ">{\\raggedright\\arraybackslash}p{3.4cm}}"
)

# Add the (1)-(4) column-number row just beneath the top rule.
toprule_line <- grep("toprule", kable_lines, fixed = TRUE)[1]
column_number_row <- paste0(
  "\\multicolumn{1}{c}{(1)} & \\multicolumn{1}{c}{(2)} & ",
  "\\multicolumn{1}{c}{(3)} & \\multicolumn{1}{c}{(4)}",
  row_end
)
kable_lines <- append(kable_lines, column_number_row, after = toprule_line)

# Shade every other data row.
midrule_line <- grep("midrule", kable_lines, fixed = TRUE)[1]
for (entry_id in seq_len(nrow(party_entry_table))) {
  if (entry_id %% 2 == 1) {
    row_index <- midrule_line + entry_id
    kable_lines[row_index] <- paste0("\\rowcolor{gray!10}", kable_lines[row_index])
  }
}
numbered_kable <- paste(
  kable_lines,
  collapse = "\n"
)
attributes(numbered_kable) <- attributes(party_entry_kable)
party_entry_kable <- numbered_kable

# Write the table to the paper output directory.
save_kable(
  party_entry_kable,
  "output/tables/descriptive_stat/party_entry_list.tex",
  float = FALSE
)
