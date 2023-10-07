# Try three at doing analysis of compound words in R
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

# Making a corpus
title_corpus <- corpus(tc_journals, text_field = "article_title")

# Making tokens
title_tokens <- tokens(title_corpus, remove_punct = TRUE,
                                      remove_separators = TRUE,
                                      remove_numbers = TRUE)

# create a new tokens object by removing stopwords from the existing tokens object  
title_tokens_nostop <- title_tokens %>%
  tokens_remove(stopwords("en"))

# stemming, using quanteda. tokens_wordstem needs a tokens object. 
titles_stemmed <- tokens_wordstem(
  title_tokens_nostop,
  language = "english"
)

# making a dfm
dfm_titles <- dfm(
  titles_stemmed,
  tolower = TRUE,
  remove_padding = FALSE
)

# checking out the top features of the dfm
topfeatures(dfm_titles, 100)

# grabbing only the top 25 most frequent tokens
top_25_stems <- textstat_frequency(dfm_titles, n = 25)

# sort by reverse frequency order
top_25_stems$feature <- with(top_25_stems, reorder(feature, -frequency))

# making a graph, hell yeah
ggplot(top_25_stems, aes(
  x = feature, y = frequency)) +
  geom_point() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Top 25 stems in TC article titles 2005-2023")

