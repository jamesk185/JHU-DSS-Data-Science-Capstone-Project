## creating a quadgram variable with the same method as used in task 2
## writing to a new object for later loading

blogs_sample_quadgram <- blogs_sample_df %>%
  unnest_tokens(quadgram, blogs_sample, token = "ngrams", n=4) %>%
  na.omit()
twitter_sample_quadgram <- twitter_sample_df %>%
  unnest_tokens(quadgram, twitter_sample, token = "ngrams", n=4) %>%
  na.omit()
news_sample_quadgram <- news_sample_df %>%
  unnest_tokens(quadgram, news_sample, token = "ngrams", n=4) %>% 
  na.omit()
all_sample_quadgram <- rbind(blogs_sample_quadgram, twitter_sample_quadgram, news_sample_quadgram) %>%
  count(quadgram) %>%
  arrange(desc(n))

saveRDS(all_sample_quadgram, "./quadgram_data.rds")


## loading the three objects for use in a modelling function

bigram_data <- readRDS("./bigram_data.rds")
trigram_data <- readRDS("./trigram_data.rds")
quadgram_data <- readRDS("./quadgram_data.rds")


## writing a function for predicting the next word after typing 1, 2 or 3 words

next_word_predictor <- function(x, firstword=NULL, secondword=NULL, thirdword=NULL){
  if(x==2){
      twoword_id <- grep(paste0("^", firstword, " "), bigram_data$bigram)[1]
      first_comb <- bigram_data[twoword_id,][1]
      strsplit(first_comb$bigram, " ")[[1]][2]
    }
    else if (x==3){
      threeword_id <- grep(paste0("^", firstword, " ", secondword, " "), trigram_data$trigram)[1]
      first_comb <- trigram_data[threeword_id,][1]
      strsplit(first_comb$trigram, " ")[[1]][3]
    }
    else if (x==4){
      fourword_id <- grep(paste0("^", firstword, " ", secondword, " ", thirdword," "), quadgram_data$quadgram)[1]
      first_comb <- quadgram_data[fourword_id,][1]
      strsplit(first_comb$quadgram, " ")[[1]][4]
    }
    else{
      print("error")
    }
}


## testing with examples

next_word_predictor(6, "when")
next_word_predictor(2, "speaking")
next_word_predictor(3, "people", "always")
next_word_predictor(4, "why", "do", "they")

next_word_predictor(2, "terrible")
next_word_predictor(3, "elephants", "are")
next_word_predictor(4, "if", "only", "i")
