# Try three at doing analysis of compound words in R
# load quanteda family of packages
library(quanteda)
library(quanteda.dictionaries)
library(quanteda.textmodels)
library(quanteda.textplots)
library(quanteda.textstats)
library(forcats)

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

glimpse(tc_journals)

# Making a corpus
title_corpus_compound_words <- corpus(tc_journals, text_field = "article_title")

# Making tokens
title_tokens_compound_words <- tokens(title_corpus_compound_words, remove_punct = TRUE,
                       remove_separators = TRUE,
                       remove_numbers = TRUE)
# collocations
col <- title_tokens_compound_words %>% 
  tokens_remove(stopwords("en")) %>% 
  tokens_select(pattern = "^[A-Z]", valuetype = "regex", 
                case_insensitive = TRUE, padding = TRUE) %>% 
  textstat_collocations(min_count = 15, tolower = TRUE)

# stemming
wordStem(col, language = "en")

# Trying to make a graph, I guess

col %>% 
  mutate(col$collocation, fct_reorder(col$collocation, col$count))

ggplot(col, aes(x = col$collocation, y = col$count)) +
  geom_point() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(title = "Top collocations in TC Article Titles (2005-2023)")


