# *************************************************************************
# Description ----
# *************************************************************************

# Remove stopwords for manifesto texts

# *************************************************************************
# SETUP ----
# *************************************************************************

rm(list = ls())

library(quanteda)
library(data.table)

# paths
source("code/util/commonVars.R")
source("code/process/paths.R")

# output directory:
outdir <- file.path(intermediate_data_path, "manifesto")
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

# load extra stopwords
extra_stopwords <- fread(paste0(DB.folder, "data/rawdata/downloaded/public/word_freq/extra_stopwords.csv"))
iceland_stopwords <- fread(paste0(DB.folder, "data/rawdata/downloaded/public/word_freq/iceland_stopwords.txt"),
                           col.names = "stopword") %>% 
  mutate(language = "icelandic")
slovakia_stopwords <- fread(paste0(DB.folder, "data/rawdata/downloaded/public/word_freq/slovakia_stopwords.txt"),
                            col.names = "stopword") %>% 
  mutate(language = "slovak")
poland_stopwords <- fread(paste0(DB.folder, "data/rawdata/downloaded/public/word_freq/poland_stopwords.txt"),
                          col.names = "stopword") %>% 
  mutate(language = "polish")
lithuania_stopwords <- fread(paste0(DB.folder, "data/rawdata/downloaded/public/word_freq/lithuania_stopwords.txt"),
                             col.names = "stopword") %>% 
  mutate(language = "lithuanian")
estonia_stopwords <- fread(paste0(DB.folder, "data/rawdata/downloaded/public/word_freq/estonia_stopwords.txt"),
                           col.names = "stopword") %>% 
  mutate(language = "estonian")
czech_stopwords <- fread(paste0(DB.folder, "data/rawdata/downloaded/public/word_freq/czech_stopwords.txt"),
                         col.names = "stopword") %>% 
  mutate(language = "czech")

extra_stopwords <- bind_rows(extra_stopwords, iceland_stopwords, slovakia_stopwords,
                             poland_stopwords, lithuania_stopwords, estonia_stopwords,
                             czech_stopwords)

# load corpus
texts <- readRDS(paste0(DB.folder, "data/rawdata/authorcreated/Supply/manifestotexts/manifesto_texts.rds"))

# *************************************************************************
# PROCESS ----
# *************************************************************************

country_stopwords_list <- list("Sweden" = stopwords("swedish"),
                               "Denmark" = stopwords("danish"),
                               "Finland" = unlist(as.list(subset(extra_stopwords, language == "finnish", select = "stopword"))),
                               "Netherlands" = stopwords("dutch"),
                               "Italy" = stopwords("italian"),
                               "Spain" = stopwords("spanish"),
                               "Greece" = unlist(as.list(subset(extra_stopwords, language == "greek", select = "stopword"))),
                               "Portugal" = stopwords("portuguese"),
                               "Germany" = stopwords("german"),
                               "Austria" = stopwords("german"),
                               "United Kingdom" = stopwords("english"),
                               "Hungary" = unlist(as.list(subset(extra_stopwords, language == "hungarian", select = "stopword"))),
                               "Slovenia" = unlist(as.list(subset(extra_stopwords, language == "slovene", select = "stopword"))),
                               "Norway" = stopwords("norwegian"),
                               "France" = stopwords("french"),
                               "Switzerland" = c(stopwords("german"), stopwords("french")),
                               "Iceland" = unlist(as.list(subset(extra_stopwords, language == "icelandic", select = "stopword"))),
                               "Slovakia" = unlist(as.list(subset(extra_stopwords, language == "slovak", select = "stopword"))),
                               "Poland" = unlist(as.list(subset(extra_stopwords, language == "polish", select = "stopword"))),
                               "Lithuania" = unlist(as.list(subset(extra_stopwords, language == "lithuanian", select = "stopword"))),
                               "Estonia" = unlist(as.list(subset(extra_stopwords, language == "estonian", select = "stopword"))),
                               "Czech Republic" = unlist(as.list(subset(extra_stopwords, language == "czech", select = "stopword"))))

stopwords_en <- stopwords("english")

remove_stopwords <- function(texts, stopwords_list) {
  if (!is.null(names(texts)) && any(duplicated(names(texts)))) {
    names(texts) <- make.unique(names(texts))
  }
  
  texts[is.na(texts)] <- ""
  
  cleaned_texts <- tokens(as.character(texts)) %>%
    tokens_remove(stopwords_list) %>%
    as.list()
  
  cleaned_texts <- sapply(cleaned_texts, function(x) paste(x, collapse = " "))
  
  return(cleaned_texts)
}

process_corpora <- function(corpora, country_stopwords_list) {
  processed_corpora <- list()
  
  for (country in names(corpora)) {
    stopwords_list <- country_stopwords_list[[country]]
    
    processed_country_corpora <- lapply(corpora[[country]], function(manifesto) {

      if (!"original_text" %in% names(manifesto)) {
        manifesto$original_text <- NA
      } else {
        manifesto$original_text <- remove_stopwords(manifesto$original_text, stopwords_list)
      }
      
      if (!"translated_text" %in% names(manifesto)) {
        manifesto$translated_text <- NA
      } else {
        manifesto$translated_text <- remove_stopwords(manifesto$translated_text, stopwords_en)
      }
      
      return(manifesto)
    })
    
    processed_corpora[[country]] <- Filter(Negate(is.null), processed_country_corpora)
  }
  
  return(processed_corpora)
}

processed_texts <- process_corpora(texts, country_stopwords_list)

saveRDS(processed_texts, file = file.path(outdir, "manifesto_texts_withoutsw.rds"))
