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

load(data_file)

glimpse(tc_journals)

# reactable package time
require(reactable)

# drop source title
table_data <- tc_journals %>%
  mutate(
    article = paste(author_full_names," (",abbreviation,", ",publication_year,")"
    )
  ) %>%
  select(article,
         article_title,
         abstract)

# create table (define columns, add parameters & formatting)
reactable(table_data,
          columns = list(
            article = colDef(name = "Article", sortable = TRUE, filterable = TRUE),
            article_title = colDef(name = "Title", sortable = TRUE, filterable = TRUE, width = 175),
            abstract = colDef(name = "Abstract", sortable = TRUE, filterable = TRUE, width = 500)
          ),
          defaultColDef = colDef(align = "left", width = 125),
          searchable = TRUE, # Enable search
          sortable = TRUE, # Enable sorting
          defaultPageSize = 3,
          highlight = TRUE,
          outlined = TRUE,
          striped = TRUE,
          compact = TRUE
) ## This is cool!

# Create a corpus object called "title_corp"
title_corp <- corpus(tc_journals, text_field = "article_title")

# print the corpus
print(title_corp)

summary(title_corp)

# summary of the corpus (including metadata for the texts)
summary(title_corp, n = 2)

# create a tokens object without punctuation, separators, and numbers
title_tokens <- tokens(title_corp, remove_punct = TRUE,
                       remove_separators = TRUE,
                       remove_numbers = TRUE)

# check the result
title_tokens[1:3]

head(stopwords("en"), 15)

# create a new tokens object by removing stopwords from the existing tokens object  
title_tokens_nostop <- title_tokens %>%
  tokens_remove(stopwords("en"))

# check the result
title_tokens_nostop[1:3]


# Below we create two dfms: one from each of our tokens objects (w/ stopwords and w/o stopwords)

## syntax option 1 (with stopwords)
titles_dfm <- title_tokens %>%
  dfm()

## syntax option 2 (without stopwords)
titles_nostop_dfm <- dfm(title_tokens_nostop)

# Print
print(titles_dfm)
print(titles_nostop_dfm)

topfeatures(titles_dfm, 20)
topfeatures(titles_nostop_dfm, 20)

# get top n features
features_titles_dfm <- textstat_frequency(titles_dfm, n = 50)

# sort by reverse frequency order
features_titles_dfm$feature <- with(features_titles_dfm, reorder(feature, -frequency))

ggplot(features_titles_dfm, aes(x = feature, y = frequency)) +
  geom_point() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(title = "Top words in TC Article Titles (2005-2023)")

# get top n features of no stop dfm
features_titles_nostop_dfm <- textstat_frequency(titles_nostop_dfm, n = 50)

# sort by reverse frequency order
features_titles_nostop_dfm$feature <- with(features_titles_nostop_dfm, reorder(feature, -frequency))
library(ggplot2)
ggplot(features_titles_nostop_dfm, aes(x = feature, y = frequency)) +
  geom_point() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Top Words in TC articles without stopwords (2005-2023)")

sorted_features <- features_titles_nostop_dfm %>%
  arrange(desc(frequency))%>%
  select(feature, frequency, docfreq)%>%
  reactable(defaultPageSize = 20,
            highlight = TRUE,
            outlined = TRUE,
            striped = TRUE,
            compact = TRUE)

sorted_features

# min count is a big parameter here. 
col <- title_tokens %>% 
  tokens_remove(stopwords("en")) %>% 
  tokens_select(pattern = "^[A-Z]", valuetype = "regex", 
                case_insensitive = TRUE, padding = TRUE) %>% 
  textstat_collocations(min_count = 15, tolower = TRUE)
print(col)

comp_title_toks <- tokens_compound(title_tokens, pattern = col)

head(comp_title_toks)
