rm(list = ls())

## a model testing against bigrams, trigrams, quadgrams, quintgrams and sextgrams
## only word combinations that occurred more than once included in the datasets 
## bi tri and quad 12 percent of source sampled, quint and sext resampled and sampled at 6 percent

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
  } else if(!is.na(second)){
    print(second)
  } else{
    print("error")
  }
}

## ~0.68secs
start <- Sys.time()

next_word_predictor("You're the reason why I smile everyday. Can you follow me please? It would mean the")

end <- Sys.time()
total <- end-start
total

## 
testing_data <- readRDS("./testingdata3.rds")
testing_data_nolastword <- readRDS("./testingdatanolastword3.rds")

testresults <- sapply(testing_data_nolastword[1001:2000], next_word_predictor)
testresults <- paste0(testing_data_nolastword[1001:2000], " ", testresults)
sum(testresults==testing_data[1001:2000])/1001
