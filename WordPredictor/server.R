library(shiny)

shinyServer(function(session, input, output) {

    bigram_data <- readRDS("C:/Users/james/Documents/R/JHU DSS Data Science Capstone/bigram_data24.rds")
    trigram_data <- readRDS("C:/Users/james/Documents/R/JHU DSS Data Science Capstone/trigram_data24.rds")
    quadgram_data <- readRDS("C:/Users/james/Documents/R/JHU DSS Data Science Capstone/quadgram_data12.rds")
    quintgram_data <- readRDS("C:/Users/james/Documents/R/JHU DSS Data Science Capstone/quintgram_data6.rds")
    sextgram_data <- readRDS("C:/Users/james/Documents/R/JHU DSS Data Science Capstone/sextgram_data3.rds")
    
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
        sixth <- sixth[!is.na(sixth)]
        
        if(length(sixth)>=4){
            all <- sixth
        } else{
            fiveword_id <- grep(paste0("^", cleanstringsplit[x-3]," ", cleanstringsplit[x-2], " ", cleanstringsplit[x-1], " ", cleanstringsplit[x], " "),
                                quintgram_data$quintgram)[1:4]
            fiveword_comb <- quintgram_data[fiveword_id,][1]
            fifth <- c(strsplit(fiveword_comb$quintgram, " ")[[1]][5],
                       strsplit(fiveword_comb$quintgram, " ")[[2]][5],
                       strsplit(fiveword_comb$quintgram, " ")[[3]][5],
                       strsplit(fiveword_comb$quintgram, " ")[[4]][5])
            fifth <- fifth[!is.na(fifth)]
            
            if(length(c(sixth, fifth))>=4){
                all <- c(sixth, fifth)
            } else{
                fourword_id <- grep(paste0("^", cleanstringsplit[x-2], " ", cleanstringsplit[x-1], " ", cleanstringsplit[x], " "),
                                    quadgram_data$quadgram)[1:4]
                fourword_comb <- quadgram_data[fourword_id,][1]
                fourth <- c(strsplit(fourword_comb$quadgram, " ")[[1]][4], 
                            strsplit(fourword_comb$quadgram, " ")[[2]][4],
                            strsplit(fourword_comb$quadgram, " ")[[3]][4],
                            strsplit(fourword_comb$quadgram, " ")[[4]][4])
                fourth <- fourth[!is.na(fourth)]
                
                if(length(c(sixth, fifth, fourth))>=4){
                    all <- c(sixth, fifth, fourth)
                } else{
                    threeword_id <- grep(paste0("^", cleanstringsplit[x-1], " ", cleanstringsplit[x], " "),
                                         trigram_data$trigram)[1:4]
                    threeword_comb <- trigram_data[threeword_id,][1]
                    third <- c(strsplit(threeword_comb$trigram, " ")[[1]][3],
                               strsplit(threeword_comb$trigram, " ")[[2]][3],
                               strsplit(threeword_comb$trigram, " ")[[3]][3],
                               strsplit(threeword_comb$trigram, " ")[[4]][3])
                    third <- third[!is.na(third)]
                    
                    if(length(c(sixth, fifth, fourth, third))>=4){
                        all <- c(sixth, fifth, fourth, third)
                    } else{
                        twoword_id <- grep(paste0("^", cleanstringsplit[x], " "),
                                           bigram_data$bigram)[1:4]
                        twoword_comb <- bigram_data[twoword_id,][1]
                        second <- c(strsplit(twoword_comb$bigram, " ")[[1]][2],
                                    strsplit(twoword_comb$bigram, " ")[[2]][2],
                                    strsplit(twoword_comb$bigram, " ")[[3]][2],
                                    strsplit(twoword_comb$bigram, " ")[[4]][2])
                        second <- second[!is.na(second)]
                        all <- c(sixth, fifth, fourth, third, second)
                    }
                }
            }
        }
        all <- unique(all)
        all <- all[1:4]
        all <- all[!is.na(all)]
        if(length(all)<4){
            all <- c(all, "lol")
            print(all)
        } else{
            print(all)
        }
        
    }
    
    all <- reactive(next_word_predictor(input$string1))

    output$out1 <- renderText({all()[1]})
    output$out2 <- renderText({all()[2]})
    output$out3 <- renderText({all()[3]})
    output$out4 <- renderText({all()[4]})
    
    observeEvent(input$button1, {
        newstring <- paste0(input$string1, " ", all()[1])
        updateTextInput(session, "string1", value = newstring)
    })
    
    observeEvent(input$button2, {
        newstring <- paste0(input$string1, " ", all()[2])
        updateTextInput(session, "string1", value = newstring)
    })
    
    observeEvent(input$button3, {
        newstring <- paste0(input$string1, " ", all()[3])
        updateTextInput(session, "string1", value = newstring)
    })
    
    observeEvent(input$button4, {
        newstring <- paste0(input$string1, " ", all()[4])
        updateTextInput(session, "string1", value = newstring)
    })
})

