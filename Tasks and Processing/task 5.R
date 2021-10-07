## model has been improved by increasing sample size of bigram and trigram datasets. see task 5 model 1 code file
## model with quintgrams and sextgrams that half in sample size respectively was attempted. see task 5 model 2 code file
## task 5 model 2 code is slightly quicker and accuracy slightly higher
## will proceed with model 2

library(tidyverse)

## after running task 5 model 2 code i will look at the errors and look for common problems
testing_data <- readRDS("./testingdata6.rds")
testing_data_nolastword <- readRDS("./testingdatanolastword6.rds")

testresults <- sapply(testing_data_nolastword[1:1000], next_word_predictor)
testresults <- paste0(testing_data_nolastword[1:1000], " ", testresults)
sum(testresults!=testing_data[1:1000])/1000
sum(testresults==testing_data[1:1000])/1000

wrongID <- which(testresults!=testing_data[1:1000])
rightID <- which(testresults==testing_data[1:1000])


wrongsTable <- data.frame(matrix(ncol = 5, nrow = 1))
cols <- c("real", "guessed", "prev1", "prev2", "prev3")
colnames(wrongsTable) <- cols

for(x in 1:length(wrongID)){
  real <- tail(strsplit(testing_data[wrongID][x], split= " ")[[1]], 1)
  guessed <- tail(strsplit(testresults[wrongID][x], split= " ")[[1]], 1)
  prev1 <- tail(strsplit(testing_data_nolastword[wrongID][x], split= " ")[[1]], 1)
  prev2 <- tail(strsplit(testing_data_nolastword[wrongID][x], split= " ")[[1]], 2)
  prev2 <- paste0(prev2[1], " ", prev2[2])
  prev3 <- tail(strsplit(testing_data_nolastword[wrongID][x], split= " ")[[1]], 3)
  prev3 <- paste0(prev3[1], " ", prev3[2], " ", prev3[3])
  wrongsTable <- rbind(wrongsTable, c(real, guessed, prev1, prev2, prev3))
}
wrongsTable <- wrongsTable[-1,]
wrongsTable %>% count(real) %>% arrange(desc(n)) %>% filter(n>1) %>% top_n(20)
wrongsTable %>% count(guessed) %>% arrange(desc(n)) %>% filter(n>1) %>% top_n(20)


rightsTable <- data.frame(matrix(ncol = 4, nrow = 1))
cols2 <- c("last", "prev1", "prev2", "prev3")
colnames(rightsTable) <- cols2

for(x in 1:length(rightID)){
  last <- tail(strsplit(testing_data[rightID][x], split= " ")[[1]], 1)
  prev1 <- tail(strsplit(testing_data_nolastword[rightID][x], split= " ")[[1]], 1)
  prev2 <- tail(strsplit(testing_data_nolastword[rightID][x], split= " ")[[1]], 2)
  prev2 <- paste0(prev2[1], " ", prev2[2])
  prev3 <- tail(strsplit(testing_data_nolastword[rightID][x], split= " ")[[1]], 3)
  prev3 <- paste0(prev3[1], " ", prev3[2], " ", prev3[3])
  rightsTable <- rbind(rightsTable, c(last, prev1, prev2, prev3))
}
rightsTable <- rightsTable[-1,]
rightsTable %>% count(last) %>% arrange(desc(n)) %>% filter(n>1) %>% top_n(20)
  
## it appears that the majority of errors are caused by stopwords that arent correctly guessed often
## perhaps better accuracy can be achieved by removing some of these stopwords with cleaningfn1
## try the same test one more time with a bigger test size and confirm results

testing_data <- readRDS("./testingdata6.rds")
testing_data_nolastword <- readRDS("./testingdatanolastword6.rds")

## note that this takes a LONG time to run
testresults <- sapply(testing_data_nolastword[1:10000], next_word_predictor)
testresults <- paste0(testing_data_nolastword[1:10000], " ", testresults)
sum(testresults!=testing_data[1:10000])/10000
sum(testresults==testing_data[1:10000])/10000

wrongID <- which(testresults!=testing_data[1:10000])
rightID <- which(testresults==testing_data[1:10000])


wrongsTable <- data.frame(matrix(ncol = 5, nrow = 1))
cols <- c("real", "guessed", "prev1", "prev2", "prev3")
colnames(wrongsTable) <- cols

for(x in 1:length(wrongID)){
  real <- tail(strsplit(testing_data[wrongID][x], split= " ")[[1]], 1)
  guessed <- tail(strsplit(testresults[wrongID][x], split= " ")[[1]], 1)
  prev1 <- tail(strsplit(testing_data_nolastword[wrongID][x], split= " ")[[1]], 1)
  prev2 <- tail(strsplit(testing_data_nolastword[wrongID][x], split= " ")[[1]], 2)
  prev2 <- paste0(prev2[1], " ", prev2[2])
  prev3 <- tail(strsplit(testing_data_nolastword[wrongID][x], split= " ")[[1]], 3)
  prev3 <- paste0(prev3[1], " ", prev3[2], " ", prev3[3])
  wrongsTable <- rbind(wrongsTable, c(real, guessed, prev1, prev2, prev3))
}
wrongsTable <- wrongsTable[-1,]
saveRDS(wrongsTable, "./wrongsTable10000.rds")

rightsTable <- data.frame(matrix(ncol = 4, nrow = 1))
cols2 <- c("last", "prev1", "prev2", "prev3")
colnames(rightsTable) <- cols2

for(x in 1:length(rightID)){
  last <- tail(strsplit(testing_data[rightID][x], split= " ")[[1]], 1)
  prev1 <- tail(strsplit(testing_data_nolastword[rightID][x], split= " ")[[1]], 1)
  prev2 <- tail(strsplit(testing_data_nolastword[rightID][x], split= " ")[[1]], 2)
  prev2 <- paste0(prev2[1], " ", prev2[2])
  prev3 <- tail(strsplit(testing_data_nolastword[rightID][x], split= " ")[[1]], 3)
  prev3 <- paste0(prev3[1], " ", prev3[2], " ", prev3[3])
  rightsTable <- rbind(rightsTable, c(last, prev1, prev2, prev3))
}
rightsTable <- rightsTable[-1,]
saveRDS(rightsTable, "./rightsTable10000.rds")

## skip to here to avoid rerunning all the code
wrongsTable <- readRDS("./wrongsTable10000.rds")
rightsTable <- readRDS("./rightsTable10000.rds")

wrongsTable %>% count(real) %>% arrange(desc(n)) %>% filter(n>1) %>% top_n(20)
wrongsTable %>% count(guessed) %>% arrange(desc(n)) %>% filter(n>1) %>% top_n(20)
wrongsTable %>% count(prev1) %>% arrange(desc(n)) %>% filter(n>1) %>% top_n(20)
wrongsTable %>% count(prev2) %>% arrange(desc(n)) %>% filter(n>1) %>% top_n(20)
wrongsTable %>% count(prev3) %>% arrange(desc(n)) %>% filter(n>1) %>% top_n(20)
rightsTable %>% count(last) %>% arrange(desc(n)) %>% filter(n>1) %>% top_n(20)

## the, and, a, i seem to be regular mistakes without being regular correct guesses
## lol is a particularly high last word occurrence that is very hard to guess correctly
## the single letter 'd' appears high in the unable to be guessed table. this will have to be investigated

## having tried removing stop words, accuracy actually goes down
## will next try a function that outputs 4 word predicted word choices from which user can select correct one

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
              strsplit(twoword_comb$bigram, " ")[[1]][2],
              strsplit(twoword_comb$bigram, " ")[[1]][2],
              strsplit(twoword_comb$bigram, " ")[[1]][2])
  
  all <- c(sixth, fifth, fourth, third, second)
  all <- all[!is.na(all)]
  all <- all[1:4]
  all <- all[!is.na(all)]
  print(unique(all))
}

## create new onlylastword variable for testing new 4predict code
testing_data <- readRDS("./testingdata6.rds")
testing_data_nolastword <- readRDS("./testingdatanolastword6.rds")

cleaningfn1(testing_data)
testing_data <- newdataset
testing_data_lastwordonly <- NULL
for(x in 1:length(testing_data)){
  y <- tail(strsplit(testing_data[x], split = " ")[[1]], 1)
  if(identical(y, character(0))){
    y <- ""
  }
  testing_data_lastwordonly <- c(testing_data_lastwordonly, y)
}
testing_data_lastwordonly <- as.list(testing_data_lastwordonly)
saveRDS(testing_data_lastwordonly, "./testingdatalastwordonly.rds")

testing_data_lastwordonly <- readRDS("./testingdatalastwordonly.rds")

## run test
## test results: 0.233, 0.242, 0.217
testresults <- sapply(testing_data_nolastword[2001:3000], next_word_predictor)
testresultHITS <- NULL
for(x in 2001:3000){
  y <- testing_data_lastwordonly[x] %in% testresults[[x-2000]]
  testresultHITS <- c(testresultHITS, y)
}
sum(testresultHITS)/1000

## this model improves potential of finding accurate word so will proceed with this and put code in task 5 model 3 code file
## will run same tests as were previously run in this code file on the new model to see if any final improvements can be made
testresults <- sapply(testing_data_nolastword[1:5000], next_word_predictor)
testresultHITS <- NULL
for(x in 1:5000){
  y <- testing_data_lastwordonly[x] %in% testresults[[x]]
  testresultHITS <- c(testresultHITS, y)
}
sum(testresultHITS)/5000

wrongsTable <- data.frame(matrix(ncol = 2, nrow = 1))
cols <- c("real", "prev1")
colnames(wrongsTable) <- cols
for(x in 1:sum(!testresultHITS)){
  real <- unlist(testing_data_lastwordonly[!testresultHITS][x])
  prev1 <- tail(strsplit(testing_data_nolastword[!testresultHITS][x], split= " ")[[1]], 1)
  wrongsTable <- rbind(wrongsTable, c(real, prev1))
}
wrongsTable <- wrongsTable[-1,]
saveRDS(wrongsTable, "./wrongsTable5000.rds")

wrongsTable <- readRDS("./wrongsTable5000.rds")
wrongsTable %>% count(real) %>% arrange(desc(n)) %>% filter(n>1) %>% top_n(50)
wrongsTable %>% count(prev1) %>% arrange(desc(n)) %>% filter(n>1) %>% top_n(50)

## investigating the rogue d
head(testing_data[grep("^d$", unlist(testing_data_lastwordonly))], 10)

## testing one last time to see where and if speed can be improved
Rprof("nextwordpredictor.out")
next_word_predictor("Elephants are the only species to be so ahead")
Rprof(NULL)
summaryRprof("nextwordpredictor.out")



             