rm(list = ls())

if(!file.exists("./Coursera-SwiftKey")){
  unzip("C:/Users/james/Documents/R/JHU DSS Data Science Capstone DATA/Coursera-SwiftKey.zip", exdir = "./Coursera-SwiftKey")}

list.files("./Coursera-SwiftKey/final/en_US")
blogsize <- file.size("./Coursera-SwiftKey/final/en_US/en_US.blogs.txt")
blogsize/(1024^2)

twitter <- readLines("./Coursera-SwiftKey/final/en_US/en_US.twitter.txt")
head(twitter)
length(twitter)
summary(nchar(twitter))

blogs <- readLines("./Coursera-SwiftKey/final/en_US/en_US.blogs.txt")
summary(nchar(blogs))

news <- readLines("./Coursera-SwiftKey/final/en_US/en_US.news.txt")
summary(nchar(news))

sum(grepl(" love ", twitter))/sum(grepl(" hate ", twitter))

twitter[grepl(" biostats ", twitter)]

sum(grepl("A computer once beat me at chess, but it was no match for me at kickboxing", twitter))
