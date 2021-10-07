library(tidyverse)
library(tidytext)

rm(list = ls())

start <- Sys.time()
set.seed(210914)

blogs <- readLines("./Coursera-SwiftKey/final/en_US/en_US.blogs.txt")
twitter <- readLines("./Coursera-SwiftKey/final/en_US/en_US.twitter.txt")
news <- readLines("./Coursera-SwiftKey/final/en_US/en_US.news.txt")

blogs_sample <- sample(blogs, length(blogs)*0.3)
twitter_sample <- sample(twitter, length(twitter)*0.3)
news_sample <- sample(news, length(news)*0.3)
all_sample <- c(blogs_sample, twitter_sample, news_sample)

testing_data <- sample(all_sample, length(all_sample)*0.2)
testing_data <- gsub("[^a-zA-Z]*$", "", testing_data)
testing_data_nolastword <- gsub(" [[:alpha:]]*$", "", testing_data)
all_sample <- setdiff(all_sample, testing_data)

saveRDS(testing_data_nolastword, "./testingdatanolastword6task5.rds")
saveRDS(testing_data, "./testingdata6task5.rds")

rm(blogs, twitter, news, blogs_sample, twitter_sample, news_sample, testing_data_nolastword, testing_data)

cleaningfn1 <- function(data){
  newdataset <<- gsub("http[^[:space:]]*", "", data)
  newdataset <<- tolower(newdataset)
  newdataset <<- gsub(" u ", " you ", newdataset)
  newdataset <<- gsub("^u ", "you ", newdataset)
  newdataset <<- gsub(" u$", " you", newdataset)
  newdataset <<- gsub("[[:punct:]]{2,}", " ", newdataset)
  newdataset <<- gsub("[^a-z[:space:]]", "", newdataset)
  newdataset <<- gsub("( the )|( a )", " ", newdataset)
  newdataset <<- gsub("^((the )|(a ))", " ", newdataset)
  newdataset <<- gsub("(( the)|( a))$", " ", newdataset)
  newdataset <<- gsub("\\s+", " ", newdataset)
  newdataset <<- gsub("\\s$", "", newdataset)
  newdataset <<- gsub("^\\s", "", newdataset)
}

cleaningfn1(all_sample)
all_sample <- newdataset

profanityURL <- "https://raw.githubusercontent.com/RobertJGabriel/Google-profanity-words/master/list.txt"
if(!file.exists("./profanity.txt")){download.file(profanityURL, "./profanity.txt")}
profanity <- read.table("./profanity.txt")
profanity <- as.character(profanity[,1])
profanity <- tibble(profanity) %>% rename(word = 1)
profanity <- profanity[!grepl("[^a-zA-Z[:space:]]", profanity$word),]
profanity <- profanity[!(nchar(profanity$word)==1),]
profanity$word <- tolower(profanity$word)
OKwords <- c("god", "hell", "blow", "job", "carpet", "bunny", "balls", "lmfao", "mother", "pawn", "snatch", "hit")
profanity <- profanity[!(profanity$word %in% OKwords),]
profanity <- rbind(profanity, tibble(word = "bullshit"))
profkey <- paste0("\\b(", paste0(profanity$word, collapse = "|"), ")\\b")
all_sample <- all_sample[!grepl(profkey, all_sample)]

all_sample_df <- tibble(all_sample)

all_sample_bigram <- all_sample_df %>%
  unnest_tokens(bigram, all_sample, token = "ngrams", n=2) %>%
  count(bigram) %>%
  arrange(desc(n)) %>%
  filter(n>1)

all_sample_trigram <- all_sample_df %>%
  unnest_tokens(trigram, all_sample, token = "ngrams", n=3) %>%
  na.omit() %>%
  count(trigram) %>%
  arrange(desc(n)) %>%
  filter(n>1)

all_sample <- sample(all_sample, length(all_sample)*0.5)
all_sample_df <- tibble(all_sample)

## CAUTION this may take a VERY long time to run, only rerun when absolutely necessary
all_sample_quadgram <- all_sample_df %>%
  unnest_tokens(quadgram, all_sample, token = "ngrams", n=4) %>%
  na.omit() %>%
  count(quadgram) %>%
  arrange(desc(n)) %>%
  filter(n>1)

all_sample <- sample(all_sample, length(all_sample)*0.5)
all_sample_df <- tibble(all_sample)

all_sample_quintgram <- all_sample_df %>%
  unnest_tokens(quintgram, all_sample, token = "ngrams", n=5) %>%
  na.omit() %>%
  count(quintgram) %>%
  arrange(desc(n)) %>%
  filter(n>1)

all_sample <- sample(all_sample, length(all_sample)*0.5)
all_sample_df <- tibble(all_sample)

all_sample_sextgram <- all_sample_df %>%
  unnest_tokens(sextgram, all_sample, token = "ngrams", n=6) %>%
  na.omit() %>%
  count(sextgram) %>%
  arrange(desc(n)) %>% 
  filter(n>1)

saveRDS(all_sample_bigram, "./bigram_data24task5.rds")
saveRDS(all_sample_trigram, "./trigram_data24task5.rds")
saveRDS(all_sample_quadgram, "./quadgram_data12task5.rds")
saveRDS(all_sample_quintgram, "./quintgram_data6task5.rds")
saveRDS(all_sample_sextgram, "./sextgram_data3task5.rds")

end <- Sys.time()
total <- end-start
total



