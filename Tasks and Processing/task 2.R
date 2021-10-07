## loading libraries

library(tidyverse)
library(tidytext)
library(stopwords)

## creating tibbles for stopwords and profanities

profanity <- tibble(profanity) %>% rename(word = 1)
stopwords <- stopwords("en") %>% tibble() %>% rename(word = 1)
stopwords$word <- gsub("[^a-zA-Z0-9[:space:]]", "", stopwords$word)

## using cleaning function from task 1 on all three datasets and printing most common words, stopwords excluded
## finally combining into one dataset and displaying as a barchart
## also showing a second barchart with the three datasets separately displaying the most common words

cleaningfn1(blogs_sample)
blogs_sample <- newdataset
blogs_sample_df <- tibble(blogs_sample)
blogs_sample_unnest <- blogs_sample_df %>%
  unnest_tokens(word, blogs_sample) %>%
  mutate(source="blogs") %>%
  anti_join(profanity) %>% 
  anti_join(stopwords)
blogs_sample_unnest

cleaningfn1(twitter_sample)
twitter_sample <- newdataset
twitter_sample_df <- tibble(twitter_sample)
twitter_sample_unnest <- twitter_sample_df %>%
  unnest_tokens(word, twitter_sample) %>%
  mutate(source="twitter") %>%
  anti_join(profanity) %>% 
  anti_join(stopwords)
twitter_sample_unnest

cleaningfn1(news_sample)
news_sample <- newdataset
news_sample_df <- tibble(news_sample)
news_sample_unnest <- news_sample_df %>%
  unnest_tokens(word, news_sample) %>%
  mutate(source="news") %>%
  anti_join(profanity) %>% 
  anti_join(stopwords)
news_sample_unnest

all_sample_unnest <- rbind(blogs_sample_unnest, twitter_sample_unnest, news_sample_unnest) %>%
  count(word) %>%
  arrange(desc(n))
all_sample_unnest

all_sample_unnest %>% top_n(20) %>%
  ggplot(aes(x=reorder(word, -n), y = n)) + 
  geom_col() + 
  xlab("Word") +
  ylab("Count") +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) 

rbind(blogs_sample_unnest, twitter_sample_unnest, news_sample_unnest) %>%
  group_by(source) %>%
  count(word) %>% 
  arrange(desc(n)) %>%
  top_n(20) %>%
  ggplot(aes(x=reorder(word, n), y = n)) + 
  geom_col() + 
  xlab("Word") +
  ylab("Count") +
  coord_flip() +
  facet_grid(~source, scales="free")


## bigrams
## storing in an object for later use
## displaying with bar chart

blogs_sample_bigram <- blogs_sample_df %>%
  unnest_tokens(bigram, blogs_sample, token = "ngrams", n=2)
twitter_sample_bigram <- twitter_sample_df %>%
  unnest_tokens(bigram, twitter_sample, token = "ngrams", n=2)
news_sample_bigram <- news_sample_df %>%
  unnest_tokens(bigram, news_sample, token = "ngrams", n=2)
all_sample_bigram <- rbind(blogs_sample_bigram, twitter_sample_bigram, news_sample_bigram) %>%
  count(bigram) %>%
  arrange(desc(n))

saveRDS(all_sample_bigram, "./bigram_data.rds")

all_sample_bigram %>% top_n(20) %>%
  ggplot(aes(x=reorder(bigram, -n), y = n)) + 
  geom_col() + 
  xlab("Bigram") +
  ylab("Count") +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) 


## trigrams
## storing in an object for later use
## displaying with bar chart

blogs_sample_trigram <- blogs_sample_df %>%
  unnest_tokens(trigram, blogs_sample, token = "ngrams", n=3) %>%
  na.omit()
twitter_sample_trigram <- twitter_sample_df %>%
  unnest_tokens(trigram, twitter_sample, token = "ngrams", n=3) %>%
  na.omit()
news_sample_trigram <- news_sample_df %>%
  unnest_tokens(trigram, news_sample, token = "ngrams", n=3) %>% 
  na.omit()
all_sample_trigram <- rbind(blogs_sample_trigram, twitter_sample_trigram, news_sample_trigram) %>%
  count(trigram) %>%
  arrange(desc(n))

saveRDS(all_sample_trigram, "./trigram_data.rds")

all_sample_trigram %>% top_n(20) %>%
  ggplot(aes(x=reorder(trigram, -n), y = n)) + 
  geom_col() + 
  xlab("Trigram") +
  ylab("Count") +
  scale_x_discrete(guide = guide_axis(angle=90)) 


## how many unique words account for 50 and 90 percent of all word occurrences 

total_n <- sum(all_sample_unnest$n)
total_word <- nrow(all_sample_unnest)
  
all_sample_cumsum <- all_sample_unnest %>% mutate(proportion = n/total_n, cumsum = cumsum(proportion))

cover_0.5 <- which(all_sample_cumsum$cumsum > 0.5)[1]
cover_0.9 <- which(all_sample_cumsum$cumsum > 0.9)[1]

cover_0.5/total_word
cover_0.9/total_word
