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

collocations_of_5 <- title_tokens %>% 
  tokens_remove(stopwords("en")) %>% 
  tokens_select(pattern = "^[A-Z]", valuetype = "regex", 
                case_insensitive = TRUE, padding = TRUE) %>% 
  textstat_collocations(min_count = 5, tolower = TRUE)

head(collocations_of_5)

# making some charts, I guess
collocations_of_5$count <- with(collocations_of_5, reorder(collocation, count))

ggplot(collocations_of_5, aes(x = collocation, y = count)) + 
  geom_point() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(title = "Top collocated words in TC Article Titles (2005-2023)")