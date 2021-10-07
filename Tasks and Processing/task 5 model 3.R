rm(list = ls())

## a model testing against bigrams, trigrams, quadgrams, quintgrams and sextgrams
## only word combination occurrences with count over one included in datasets
## 24 percent of source file sampled for bigram, trigram. 12 for quadgram. 6 for quintgram. 3 for sextgram.
## model provides four likely word choices as output

bigram_data <- readRDS("./bigram_data24.rds")
trigram_data <- readRDS("./trigram_data24.rds")
quadgram_data <- readRDS("./quadgram_data12.rds")
quintgram_data <- readRDS("./quintgram_data6.rds")
sextgram_data <- readRDS("./sextgram_data3.rds")

next_word_predictor <- function(string){
  cleanstring <- tolower(string)
  cleanstring <- gsub( " & ", " and ", cleanstring)
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
  
  sixword_id <- grep(paste0("^", cleanstringsplit[x-4], " ", cleanstringsplit[x-3], " ", cleanstringsplit[x-2]," ", cleanstringsplit[x-1], " ", cleanstringsplit[x], " "),
                     sextgram_data$sextgram)[1:4]
  sixword_comb <- sextgram_data[sixword_id,][1]
  sixth <- c(strsplit(sixword_comb$sextgram, " ")[[1]][6],
             strsplit(sixword_comb$sextgram, " ")[[2]][6],
             strsplit(sixword_comb$sextgram, " ")[[3]][6],
             strsplit(sixword_comb$sextgram, " ")[[4]][6])
  
  fiveword_id <- grep(paste0("^", cleanstringsplit[x-3]," ", cleanstringsplit[x-2], " ", cleanstringsplit[x-1], " ", cleanstringsplit[x], " "),
                      quintgram_data$quintgram)[1:4]
  fiveword_comb <- quintgram_data[fiveword_id,][1]
  fifth <- c(strsplit(fiveword_comb$quintgram, " ")[[1]][5],
             strsplit(fiveword_comb$quintgram, " ")[[2]][5],
             strsplit(fiveword_comb$quintgram, " ")[[3]][5],
             strsplit(fiveword_comb$quintgram, " ")[[4]][5])
  
  fourword_id <- grep(paste0("^", cleanstringsplit[x-2], " ", cleanstringsplit[x-1], " ", cleanstringsplit[x], " "),
                      quadgram_data$quadgram)[1:4]
  fourword_comb <- quadgram_data[fourword_id,][1]
  fourth <- c(strsplit(fourword_comb$quadgram, " ")[[1]][4], 
              strsplit(fourword_comb$quadgram, " ")[[2]][4],
              strsplit(fourword_comb$quadgram, " ")[[3]][4],
              strsplit(fourword_comb$quadgram, " ")[[4]][4])
  
  threeword_id <- grep(paste0("^", cleanstringsplit[x-1], " ", cleanstringsplit[x], " "),
                       trigram_data$trigram)[1:4]
  threeword_comb <- trigram_data[threeword_id,][1]
  third <- c(strsplit(threeword_comb$trigram, " ")[[1]][3],
             strsplit(threeword_comb$trigram, " ")[[2]][3],
             strsplit(threeword_comb$trigram, " ")[[3]][3],
             strsplit(threeword_comb$trigram, " ")[[4]][3])
  
  twoword_id <- grep(paste0("^", cleanstringsplit[x], " "),
                     bigram_data$bigram)[1:4]
  twoword_comb <- bigram_data[twoword_id,][1]
  second <- c(strsplit(twoword_comb$bigram, " ")[[1]][2],
              strsplit(twoword_comb$bigram, " ")[[2]][2],
              strsplit(twoword_comb$bigram, " ")[[3]][2],
              strsplit(twoword_comb$bigram, " ")[[4]][2])
  
  all <- c(sixth, fifth, fourth, third, second)
  all <- all[!is.na(all)]
  all <- unique(all)
  all <- all[1:4]
  all <- all[!is.na(all)]
  if(length(all)<4){
    alllol <- c(all, "lol")
    print(alllol)
  }
  else{
    print(all)
  }
}

## 0.4
start <- Sys.time()
next_word_predictor("You're the reason why I smile everyday. Can you follow me please? It would mean the")
end <- Sys.time()
total <- end-start
total

testing_data_lastwordonly <- readRDS("./testingdatalastwordonly.rds")
testing_data <- readRDS("./testingdata6.rds")
testing_data_nolastword <- readRDS("./testingdatanolastword6.rds")

## test results: , , 0.209
testresults <- sapply(testing_data_nolastword[1001:2000], next_word_predictor)
testresultHITS <- NULL
for(x in 1001:2000){
  y <- testing_data_lastwordonly[x] %in% testresults[[x-1000]]
  testresultHITS <- c(testresultHITS, y)
}
sum(testresultHITS)/1000


