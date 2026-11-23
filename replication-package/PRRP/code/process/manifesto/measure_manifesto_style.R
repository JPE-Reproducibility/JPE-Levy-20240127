# *************************************************************************
# Description ----
# *************************************************************************

# Measure manifesto styles for parties

# *************************************************************************
# SETUP ----
# *************************************************************************

rm(list = ls())

library(manifestoR)
library(quanteda)
library(stringr)
library(dplyr)
library(tidyr)
library(haven)
library(tibble)
library(data.table)

# paths
source("code/util/commonVars.R")
source("code/process/paths.R")
source("code/analyze/party_positions/manifesto_general_functions.R")

# output directory:
outdir <- paste0(cleandata_path, "/manifesto/")

# load and process corpora
corpora <- readRDS(paste0(DB.folder, "data/rawdata/authorcreated/Supply/manifestotexts/manifesto_texts.rds"))
corpora_sw <- readRDS(file.path(intermediate_data_path, "manifesto", "manifesto_texts_withoutsw.rds"))

corpora <- lapply(corpora, function(x) {
  lapply(x, as.data.frame)
})
corpora_sw <- lapply(corpora_sw, function(x) {
  lapply(x, as.data.frame)
})

# detect corpus splited by sentence
no_cmp_code <- function(corpora) {
  no_cmp_code_list <- list()
  
  for (country in names(corpora)) {
    for (party_election in names(corpora[[country]])) {
      df <- corpora[[country]][[party_election]]
      if (!"cmp_code" %in% names(df)) {
        if (!country %in% names(no_cmp_code_list)) {
          no_cmp_code_list[[country]] <- list()
        }
        no_cmp_code_list[[country]][[party_election]] <- df
      }
    }
  }
  
  return(no_cmp_code_list)
}

corpora_sentences <- no_cmp_code(corpora)

# *************************************************************************
# MEASURE MANIFESTO STYLE ----
# *************************************************************************

# calculate number of sentences for each corpus
count_sentences <- function(party_election_df) {
  if ("original_text" %in% names(party_election_df)) {
    all_text <- paste(party_election_df$original_text, collapse = " ")
    sentences <- unlist(strsplit(all_text, "(?<=[.!?])\\s+|\n+", perl = TRUE))
    total_sentences <- length(sentences)
    return(total_sentences)
  }
  return(NA)
}

# process one outlier -- Go Italy in 2018, Slovenian National Party in 2011 and 	
# People-Animals-Nature in 2019
add_period <- function(df) {
  for (col in c("original_text", "translated_text")) {
    sentences <- df[[col]]

    for (i in 2:length(sentences)) {
      if (grepl("^[[:upper:]]", sentences[i])) {
        if (!grepl("[.?!]$", sentences[i - 1])) {
          df[[col]][i - 1] <- paste0(sentences[i - 1], ".")
        }
      }
    }
  }
  return(df)
}

corpora$Italy[["32610_201803"]] <- add_period(corpora$Italy[["32610_201803"]])
corpora$Slovenia[["97710_201112"]] <- add_period(corpora$Slovenia[["97710_201112"]])
corpora$Portugal[["35120_201910"]] <- add_period(corpora$Portugal[["35120_201910"]])

manifesto_sentences_all <- bind_rows(lapply(names(corpora), function(country) {
  country_df <- corpora[[country]]
  
  country_sentences <- lapply(names(country_df), function(party_election) {
    party_election_df <- country_df[[party_election]]
    total_sentences <- count_sentences(party_election_df)
    
    return(data.frame(country = country, 
                      party_date = party_election, 
                      total_sentences = total_sentences))
  })
  
  return(do.call(rbind, country_sentences))
}))

# calculate number of clauses for each corpus
manifesto_clauses_all <- do.call(rbind, lapply(seq_along(corpora), function(i) {
  data.frame(country = names(corpora)[i], clauses = sapply(corpora[[i]], nrow))
})) %>%
  rownames_to_column(var = "party_date")

manifesto_clauses_all <- manifesto_clauses_all[!manifesto_clauses_all$party_date %in% 
                                                      unlist(lapply(corpora_sentences, names)), ]

# estimate number of clauses for those with only sentences by number of sentences times clauses to sentence ratio
df <- inner_join(manifesto_clauses_all, manifesto_sentences_all, 
                 by = c("party_date", "country"))

df <- df %>%
  group_by(country) %>%
  summarize(
    total_clauses_country = sum(clauses, na.rm = TRUE),
    total_sentences_country = sum(total_sentences, na.rm = TRUE)) %>%
  mutate(clause_to_sentence_ratio = total_clauses_country / total_sentences_country)

df <- left_join(manifesto_sentences_all, df,
                by = "country") %>% 
  select(country, party_date, total_sentences, clause_to_sentence_ratio) %>% 
  mutate(total_clauses = total_sentences * clause_to_sentence_ratio) %>% 
  filter(!party_date %in% manifesto_clauses_all$party_date) %>% 
  select(party_date, country,  
         clauses = total_clauses)

sort_party_date <- function(ids) {
  split_ids <- strsplit(ids, "_")
  
  dates <- sapply(split_ids, function(x) as.numeric(x[2]))
  parties <- sapply(split_ids, function(x) x[1])
  
  order_indices <- order(dates, parties)
  
  sorted_ids <- ids[order_indices]
  return(sorted_ids)
}

manifesto_clauses_all <- bind_rows(manifesto_clauses_all, df) %>%
  arrange(country, factor(party_date, levels = sort_party_date(unique(manifesto_clauses_all$party_date))))

# calculate total word count for each corpus
count_total_words <- function(my_corpus) {
  country_party_words <- lapply(my_corpus, function(party_list) {
    party_word_counts <- sapply(party_list, function(party) {
      if ("original_text" %in% names(party)) {
        all_text <- party$original_text
        all_words <- unlist(strsplit(all_text, "\\s+"))
        total_words <- length(all_words)
        return(total_words)
      } else {
        return(NA)
      }
    })
    
    party_df <- data.frame(party = names(party_word_counts), total_words = party_word_counts)
    return(party_df)
  })
  
  return(bind_rows(country_party_words, .id = "country"))
}

total_words_all <- count_total_words(corpora)
total_words_all_sw <- count_total_words(corpora_sw)

# Calculate MATTR for one party-election corpus.
calculate_manifesto_mattr <- function(party_election_df) {
  if ("original_text" %in% names(party_election_df)) {
    all_text <- paste(party_election_df$original_text, collapse = " ")
    tokens_text <- tokens(
      all_text,
      remove_numbers = TRUE,
      remove_punct = TRUE,
      remove_symbols = TRUE,
      remove_url = TRUE
    ) %>%
      tokens_tolower()
    return(calculate_mattr(tokens_text[[1]], window = 200L))
  }
  return(NA)  
}

mattr_all <- bind_rows(lapply(names(corpora_sw), function(country) {
  country_df <- corpora_sw[[country]]
  

  country_mattr <- lapply(names(country_df), function(party_election) {
    party_election_df <- country_df[[party_election]]
    total_mattr <- calculate_manifesto_mattr(party_election_df)

    return(data.frame(country = country, 
                      party_election = party_election, 
                      MATTR = total_mattr))
  })
  
  return(do.call(rbind, country_mattr))
}))

# Scale MATTR
scale_mattr <- function(mattr_df) {
  mattr_df$MATTR_raw <- mattr_df$MATTR
  mattr_df$MATTR <- scale(mattr_df$MATTR)
  return(mattr_df)
}

scaled_mattr_all<- scale_mattr(mattr_all)

# combine measures by manifesto keys
corpus_index <- bind_rows(lapply(names(corpora_sw), function(country) {
  data.frame(country = country, party_date = names(corpora_sw[[country]]))
}))

mattr_df <- scaled_mattr_all %>%
  rename(party_date = party_election,
         mattr = MATTR,
         mattr_raw = MATTR_raw)

clauses_df <- manifesto_clauses_all %>%
  select(country, party_date, clauses)

sentences_df <- manifesto_sentences_all %>%
  rename(sentences = total_sentences)

total_words_df <- total_words_all %>%
  rename(party_date = party,
         total_words = total_words)

total_words_sw_df <- total_words_all_sw %>%
  rename(party_date = party,
         total_words_sw = total_words)

complexity <- corpus_index %>%
  left_join(mattr_df, by = c("country", "party_date")) %>%
  left_join(clauses_df, by = c("country", "party_date")) %>%
  left_join(sentences_df, by = c("country", "party_date")) %>%
  left_join(total_words_df, by = c("party_date")) %>%
  left_join(total_words_sw_df, by = c("party_date")) %>%
  select(party_date, mattr, mattr_raw, clauses, sentences,
         total_words, total_words_sw, everything())

# scale measures
complexity$average_words_sentence <- complexity$total_words/complexity$sentences
complexity$average_words_clause <- complexity$total_words / complexity$clauses

complexity <- complexity %>% 
  select(party_date, mattr, mattr_raw, clauses, sentences, total_words,
         average_words_sentence, average_words_clause, country_name = country)

# add party names etc.
manifesto_populist <- readRDS(paste0(cleandata_path, "/manifesto/manifesto_populist.rds"))
manifesto_populist$party_date <- paste0(manifesto_populist$Y_manifesto_party_id, "_", manifesto_populist$date)

complexity <- complexity %>%
  left_join(manifesto_populist, by = "party_date") %>% 
  select(manifesto_party_name, party_date, mattr, mattr_raw,
         clauses, sentences, total_words,
         average_words_sentence, average_words_clause,
         country_name, manifestoYear, pervote, PRRP = Y_Far_Right)

saveRDS(complexity, file = paste0(outdir, "manifesto_style.rds"))
