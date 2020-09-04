# Loading packages

library(tidyverse)
library(tidytext)
library(wordcloud)
library(igraph)
library(ggraph)
library(widyr)
library(tm)
library(lubridate)

### preparing data set

## read data

file <- file.choose()

db <- read_lines(file)

## removing irrelevant information and structuring data

texto <- db %>%
  str_subset("^[A-Za-z]") %>% 
  str_remove_all("</i>") %>% 
  trimws() %>%
  unlist()

texto <- tibble(line = 1:length(texto), text = texto)


texto <- texto %>% 
  unnest_tokens(word, text)

### text analysis

## loading data stop words

data("stop_words")

## word frequencies

texto %>%
  count(word, sort = TRUE) %>%
  anti_join(stop_words) %>% 
  filter(n > 10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col(colour = "magenta", fill = "magenta2") +
  xlab(NULL) +
  coord_flip() + 
  theme_minimal()

## word cloud

x <- texto %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, index = line %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative) %>% 
  count(sentiment, word, sort = TRUE) %>% 
  group_by(word) %>% 
  summarize(total = sum(n))

wordcloud(x$word, x$total, min.freq = 1,
          max.words = 200, random.order = F, rot.per = 0.35, 
          colors = brewer.pal(8, "Dark2"))

## dictionaries

AFINN <- get_sentiments("afinn")

NCR <- get_sentiments("nrc")

BING <- get_sentiments("bing")

## Poladity words separated into positive and negative

texto %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup() %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE, colour = "magenta", fill = "magenta2") +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Intensidade do Sentimento",
       x = NULL) +
  coord_flip() + 
  theme_minimal()

## Correlation between words and n - grams

texto <- db %>%
  str_subset("^[A-Za-z]") %>% 
  str_remove_all("</i>") %>% 
  trimws() %>%
  unlist()

texto <- tibble(line = 1:length(texto), text = texto)

bigrams <- texto %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

bigrams_separated <- bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

## new bigram counts:

bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE) %>% 
  na.omit()

### Redes de bigrams

bigram_graph <- bigram_counts %>%
  filter(n > 1) %>%
  graph_from_data_frame()

set.seed(2016)
a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "magenta", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()






























