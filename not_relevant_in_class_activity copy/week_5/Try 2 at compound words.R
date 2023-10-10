# load quanteda family of packages
library(quanteda)
library(quanteda.dictionaries)
library(quanteda.textmodels)
library(quanteda.textplots)
library(quanteda.textstats)

# load other libraries
library(readtext)
library(spacyr)
library(tidyverse)

# define location of datafile
data_file <- "data/tc_journals.RData"

# load data
load(data_file)

# Create a corpus object called "title_corp_compound_words"
title_corp_compound_words <- corpus(tc_journals, text_field = "article_title")

# create a tokens object without punctuation, separators, and numbers
title_compound_word_tokens <- tokens(title_corp_compound_words, remove_punct = TRUE,
                       remove_separators = TRUE,
                       remove_numbers = TRUE)

# removing stopwords
title_compound_word_tokens <- title_compound_word_tokens %>%
  tokens_remove(stopwords("en"))

# Making document feature matrices
## Without stopwords
dfm_title_compound_word_tokens <- dfm(title_compound_word_tokens)

collocations_compound <- title_compound_word_tokens %>% 
  tokens_remove(stopwords("en")) %>% 
  tokens_select(pattern = "^[A-Z]", valuetype = "regex", 
                case_insensitive = TRUE, padding = TRUE) %>% 
  textstat_collocations(min_count = 3, tolower = TRUE)

# Making a corpus
corpus_collocations_compound <- corpus

# making some tokens
tokens_collocations_compound <- tokens(collocations_compound, remove_punct = TRUE,
                                       remove_separators = TRUE,
                                       remove_numbers = TRUE)
