set.seed(210914)

blogs_sample <- sample(blogs, length(blogs)*0.05)
twitter_sample <- sample(twitter, length(twitter)*0.05)
news_sample <- sample(news, length(news)*0.05)
all_sample <- c(blogs_sample, twitter_sample, news_sample)

if(!file.exists("./en_US.all_sample.txt")){writeLines(all_sample, "./en_US.all_sample.txt")}

head(all_sample)

all_sample[25320]

cleaningfn1 <- function(data){
  newdataset <<- gsub("http[^[:space:]]*", "", data)
  newdataset <<- gsub("[^a-zA-Z[:space:]]", "", newdataset)
  newdataset <<- tolower(newdataset)
}
cleaningfn1(all_sample)
all_sample <- newdataset

all_sample[25320]

profanityURL <- "https://raw.githubusercontent.com/RobertJGabriel/Google-profanity-words/master/list.txt"
if(!file.exists("./profanity.txt")){download.file(profanityURL, "./profanity.txt")}
profanity <- read.table("./profanity.txt")
profanity <- as.character(profanity[,1])
head(profanity)

for(x in 1:length(profanity)){
  all_sample <- gsub(paste0(" ", profanity[x], " "), " ", all_sample)
  all_sample <- gsub(paste0(" ", profanity[x], "$"), "", all_sample)
  all_sample <- gsub(paste0("^", profanity[x], " "), " ", all_sample)
}

all_sample[25320]
