# stemming on article abstracts

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
library(dplyr)
library(SnowballC)

# define location of datafile
data_file <- "data/tc_journals.RData"

# load data
load(data_file)

# Creating a corpus
abstract_corpus <- corpus(tc_journals, text_field = "abstract")

# Making some tokens
abstract_tokens <- tokens(abstract_corpus, remove_punct = TRUE,
                          remove_separators = TRUE,
                          remove_numbers = TRUE)

# removing stopwords, creating a new tokens object
abstract_tokens_no_stop <- abstract_tokens %>% 
  tokens_remove(stopwords("en"))

# Stemming, using quanteda
abstracts_stemmed <- tokens_wordstem(
  abstract_tokens_no_stop,
  language = "english"
)

# Making a document-feature matrix
dfm_abstract <- dfm(
  abstracts_stemmed,
  tolower = TRUE,
  remove_padding = FALSE
)

# checking out the top features
topfeatures(dfm_abstract, 100)

# grabbing only the top 25 most frequent tokens
abstracts_top_25_stems <- textstat_frequency(dfm_abstract, n = 25)

# sorting by frequency
abstracts_top_25_stems$feature <- with(
  abstracts_top_25_stems,
  reorder(feature, -frequency))

# making a graph, hell yea
ggplot(abstracts_top_25_stems, aes(
  x = feature, y = frequency)) + 
  geom_point() +
  theme(axis.text = element_text(angle = 45, hjust = 1)) +
  labs(title = "Top 25 stems in TC article abstracts 2005-2023")

