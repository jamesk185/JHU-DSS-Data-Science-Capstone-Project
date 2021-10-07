library(tidyverse)
library(tidytext)

## correctly removing profanity from ngram datasets
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

bigram_data <- readRDS("./bigram_data.rds")
trigram_data <- readRDS("./trigram_data.rds")
quadgram_data <- readRDS("./quadgram_data.rds")

bigram_data <- bigram_data[!grepl(profkey, bigram_data$bigram),]
trigram_data <- trigram_data[!grepl(profkey, trigram_data$trigram),]
quadgram_data <- quadgram_data[!grepl(profkey, quadgram_data$quadgram),]

## overwriting to have final clean ngram objects
saveRDS(bigram_data, "./bigram_data.rds")
saveRDS(trigram_data, "./trigram_data.rds")
saveRDS(quadgram_data, "./quadgram_data.rds")

bigram_data <- readRDS("./bigram_data.rds")
trigram_data <- readRDS("./trigram_data.rds")
quadgram_data <- readRDS("./quadgram_data.rds")

## function edited to take only a simple string argument 
## and to work for string input of length greater than 3
next_word_predictor <- function(string){
  cleanstring <- gsub("[^a-zA-Z[:space:]]", "", string)
  cleanstring <- tolower(cleanstring)
  cleanstringsplit <- strsplit(cleanstring, split = " ")[[1]]
  x <- length(cleanstringsplit)
  
  if(x==1){
    twoword_id <- grep(paste0("^", cleanstring, " "), bigram_data$bigram)[1]
    first_comb <- bigram_data[twoword_id,][1]
    strsplit(first_comb$bigram, " ")[[1]][2]
  }
  else if(x==2){
    threeword_id <- grep(paste0("^", cleanstring, " "), trigram_data$trigram)[1]
    first_comb <- trigram_data[threeword_id,][1]
    strsplit(first_comb$trigram, " ")[[1]][3]
  }
  else if(x==3){
    fourword_id <- grep(paste0("^", cleanstring," "), quadgram_data$quadgram)[1]
    first_comb <- quadgram_data[fourword_id,][1]
    strsplit(first_comb$quadgram, " ")[[1]][4]
  }
  else if(x>3){
    fourword_id <- grep(paste0("^", cleanstringsplit[x-2]," ", cleanstringsplit[x-1], " ", cleanstringsplit[x], " "),
                        quadgram_data$quadgram)[1]
    first_comb <- quadgram_data[fourword_id,][1]
    strsplit(first_comb$quadgram, " ")[[1]][4]
  }
  else{
    print("error")
  }
}

## testing the model
next_word_predictor("The guy in front of me just bought a pound of bacon, a bouquet, and a case of")
next_word_predictor("You're the reason why I smile everyday. Can you follow me please? It would mean the")
next_word_predictor("Hey sunshine, can you follow me and make me the")
quadgram_data[grep("make me the ", quadgram_data$quadgram),]

sum(quadgram_data$n==1)/nrow(quadgram_data)

## a large percentage of the data is unique occurrences of a word combination
## increasing the sample size to potentially create less non-unique occurrences 
## and then removing unique occurrences from the dataset may increase accuracy and speed of the model simultaneously

## trying the same model with a greater sample size
## see full code run for resampling test file
bigram_data <- readRDS("./bigram_data12.rds")
trigram_data <- readRDS("./trigram_data12.rds")
quadgram_data <- readRDS("./quadgram_data12.rds")

## function edited to use trigrams if no quadgrams and bigrams if no trigrams
next_word_predictor <- function(string){
  cleanstring <- gsub("[^a-zA-Z[:space:]]", "", string)
  cleanstring <- tolower(cleanstring)
  cleanstringsplit <- strsplit(cleanstring, split = " ")[[1]]
  x <- length(cleanstringsplit)
  
  fourword_id <- grep(paste0("^", cleanstringsplit[x-2]," ", cleanstringsplit[x-1], " ", cleanstringsplit[x], " "),
                      quadgram_data$quadgram)[1]
  fourword_comb <- quadgram_data[fourword_id,][1]
  fourth <- strsplit(fourword_comb$quadgram, " ")[[1]][4]
  
  threeword_id <- grep(paste0("^", cleanstringsplit[x-1]," ", cleanstringsplit[x], " "),
                       trigram_data$trigram)[1]
  threeword_comb <- trigram_data[threeword_id,][1]
  third <- strsplit(threeword_comb$trigram, " ")[[1]][3]
  
  twoword_id <- grep(paste0("^", cleanstringsplit[x], " "),
                     bigram_data$bigram)[1]
  twoword_comb <- bigram_data[twoword_id,][1]
  second <- strsplit(twoword_comb$bigram, " ")[[1]][2]
  
  if(!is.na(fourth)){
    print(fourth)
  } else if(!is.na(third)){
    print(third)
  } else if(!is.na(second))
    print(second)
  else{
    print("error")
  }
}

## creating quintgram and sextgram datasets 
## this time a new sample, a smaller one, will be used as it takes too long to compute
start <- Sys.time()
set.seed(220914)

blogs <- readLines("./Coursera-SwiftKey/final/en_US/en_US.blogs.txt")
twitter <- readLines("./Coursera-SwiftKey/final/en_US/en_US.twitter.txt")
news <- readLines("./Coursera-SwiftKey/final/en_US/en_US.news.txt")

blogs_sample <- sample(blogs, length(blogs)*0.06)
twitter_sample <- sample(twitter, length(twitter)*0.06)
news_sample <- sample(news, length(news)*0.06)
all_sample <- c(blogs_sample, twitter_sample, news_sample)

cleaningfn1 <- function(data){
  newdataset <<- gsub("http[^[:space:]]*", "", data)
  newdataset <<- gsub("[^a-zA-Z[:space:]]", "", newdataset)
  newdataset <<- tolower(newdataset)
}

cleaningfn1(all_sample)
all_sample <- newdataset
all_sample_df <- tibble(all_sample)

all_sample_quintgram <- all_sample_df %>%
  unnest_tokens(quintgram, all_sample, token = "ngrams", n=5) %>%
  na.omit() %>%
  count(quintgram) %>%
  arrange(desc(n)) %>%
  filter(n>1)

all_sample_sextgram <- all_sample_df %>%
  unnest_tokens(sextgram, all_sample, token = "ngrams", n=6) %>%
  na.omit() %>%
  count(sextgram) %>%
  arrange(desc(n)) %>% filter(n>1)

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
all_sample_quintgram <- all_sample_quintgram[!grepl(profkey, all_sample_quintgram$quintgram),]
all_sample_sextgram <- all_sample_sextgram[!grepl(profkey, all_sample_sextgram$sextgram),]

end <- Sys.time()
total <- end-start
total

## saving next six percent quintgram and sextgram objects
saveRDS(all_sample_quintgram, "./quintgram_data6.rds")
saveRDS(all_sample_sextgram, "./sextgram_data6.rds")

quintgram_data <- readRDS("./quintgram_data6.rds")
sextgram_data <- readRDS("./sextgram_data6.rds")

## writing a new function to also check for quintgrams and sextgrams
rm(list = ls())

bigram_data <- readRDS("./bigram_data12.rds")
trigram_data <- readRDS("./trigram_data12.rds")
quadgram_data <- readRDS("./quadgram_data12.rds")
quintgram_data <- readRDS("./quintgram_data6.rds")
sextgram_data <- readRDS("./sextgram_data6.rds")

next_word_predictor <- function(string){
  cleanstring <- gsub("[^a-zA-Z[:space:]]", "", string)
  cleanstring <- tolower(cleanstring)
  cleanstringsplit <- strsplit(cleanstring, split = " ")[[1]]
  x <- length(cleanstringsplit)
  
  sixword_id <- grep(paste0("^", cleanstringsplit[x-4]," ", cleanstringsplit[x-3], " ", cleanstringsplit[x-2], " ",
                            cleanstringsplit[x-1], " ", cleanstringsplit[x], " "),
                     sextgram_data$sextgram)[1]
  sixword_comb <- sextgram_data[sixword_id,][1]
  sixth <- strsplit(sixword_comb$sextgram, " ")[[1]][6]
  
  fiveword_id <- grep(paste0("^", cleanstringsplit[x-3], " ", cleanstringsplit[x-2], " ",
                             cleanstringsplit[x-1], " ", cleanstringsplit[x], " "),
                      quintgram_data$quintgram)[1]
  fiveword_comb <- quintgram_data[fiveword_id,][1]
  fifth <- strsplit(fiveword_comb$quintgram, " ")[[1]][5]
  
  fourword_id <- grep(paste0("^", cleanstringsplit[x-2]," ", cleanstringsplit[x-1], " ", cleanstringsplit[x], " "),
                      quadgram_data$quadgram)[1]
  fourword_comb <- quadgram_data[fourword_id,][1]
  fourth <- strsplit(fourword_comb$quadgram, " ")[[1]][4]
  
  threeword_id <- grep(paste0("^", cleanstringsplit[x-1]," ", cleanstringsplit[x], " "),
                       trigram_data$trigram)[1]
  threeword_comb <- trigram_data[threeword_id,][1]
  third <- strsplit(threeword_comb$trigram, " ")[[1]][3]
  
  twoword_id <- grep(paste0("^", cleanstringsplit[x], " "),
                     bigram_data$bigram)[1]
  twoword_comb <- bigram_data[twoword_id,][1]
  second <- strsplit(twoword_comb$bigram, " ")[[1]][2]
  
  if(!is.na(sixth)){
    print(sixth)
  } else if(!is.na(fifth)){
    print(fifth)
  } else if(!is.na(fourth)){
    print(fourth)
  } else if(!is.na(third)){
    print(third)
  } else if(!is.na(second))
    print(second)
  else{
    print("error")
  }
}

## in order to test accuracy I will resample again, creating a test and a train set. see full code run for resampling test file.
## in order to test accuracy and speed of a few models i will write them in separate code files

## trying to improve speed with rprof

Rprof("nextwordpredictor.out")
next_word_predictor("You're the reason why I smile everyday. Can you follow me please? It would mean the")
Rprof(NULL)
summaryRprof("nextwordpredictor.out")
