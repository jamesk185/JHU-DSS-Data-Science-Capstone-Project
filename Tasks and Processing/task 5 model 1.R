rm(list = ls())

## a model testing against bigrams, trigrams and quadgrams
## only word combination occurrences with count over one included in datasets
## 24 percent of source file sampled for bigram, trigram. 12 percent for quadgram.

bigram_data <- readRDS("./bigram_data24.rds")
trigram_data <- readRDS("./trigram_data24.rds")
quadgram_data <- readRDS("./quadgram_data12.rds")

next_word_predictor <- function(string){
  cleanstring <- tolower(string)
  cleanstring <- gsub(" u ", " you ", cleanstring)
  cleanstring <- gsub("^u ", "you ", cleanstring)
  cleanstring <- gsub(" u$", " you", cleanstring)
  cleanstring <- gsub("[[:punct:]]{2,}", " ", cleanstring)
  cleanstring <- gsub("[^a-z[:space:]]", "", cleanstring)
  cleanstring <- gsub("\\s+", " ", cleanstring)
  cleanstring <- gsub("\\s$", "", cleanstring)
  cleanstring <- gsub("^\\s", "", cleanstring)
  cleanstringsplit <- strsplit(cleanstring, split = " ")[[1]]
  x <- length(cleanstringsplit)
  
  fourword_id <- grep(paste0("^", cleanstringsplit[x-2]," ", cleanstringsplit[x-1], " ", cleanstringsplit[x], " "),
                      quadgram_data$quadgram)[1]
  fourword_comb <- quadgram_data[fourword_id,][1]
  fourth <- strsplit(fourword_comb$quadgram, " ")[[1]][4]
  
  if(!is.na(fourth)){
    print(fourth)
  } else{
    threeword_id <- grep(paste0("^", cleanstringsplit[x-1]," ", cleanstringsplit[x], " "),
                         trigram_data$trigram)[1]
    threeword_comb <- trigram_data[threeword_id,][1]
    third <- strsplit(threeword_comb$trigram, " ")[[1]][3]
    if(!is.na(third)){
      print(third)
    } else{
      twoword_id <- grep(paste0("^", cleanstringsplit[x], " "),
                         bigram_data$bigram)[1]
      twoword_comb <- bigram_data[twoword_id,][1]
      second <- strsplit(twoword_comb$bigram, " ")[[1]][2]
      if(!is.na(second)){
        print(second)
      } else {
        print("")
      }
    }
  }
}

## ~0.058 seconds
start <- Sys.time()
next_word_predictor("You're the reason why I smile everyday. Can you follow me please? It would mean the")
end <- Sys.time()
total <- end-start
total

## pre task 5 model results: 0.107, 0.133, 0.119
## post task 5 model results: , , 
testing_data <- readRDS("./testingdata6.rds")
testing_data_nolastword <- readRDS("./testingdatanolastword6.rds")

testresults <- sapply(testing_data_nolastword[1:1000], next_word_predictor)
testresults <- paste0(testing_data_nolastword[1:1000], " ", testresults)
sum(testresults==testing_data[1:1000])/1000
