library(shiny)

shinyUI(fluidPage(
    titlePanel("Word Predictor"),
    sidebarLayout(
        sidebarPanel(
            h5("Type text to see the next four predicted words."),
            h5("Click on a predicted word to add it to your sentence.")
        ),
        mainPanel(
            tabsetPanel(type="tabs",
                        tabPanel("App", br(), textAreaInput("string1", "Enter text:", value = "text", width = "600px", height = "40px"), br(),
                                 actionButton("button1", textOutput("out1")),
                                 actionButton("button2", textOutput("out2")),
                                 actionButton("button3", textOutput("out3")),
                                 actionButton("button4", textOutput("out4"))),
                        tabPanel("Info", br(),
                                 h5("App created by James Kowalik as part of the Data Science Capstone course in the Johns Hopkins University Data Science Specialization."), 
                                 br(),
                                 h5("Completed in October 2021."),
                                 br(),
                                 h5("See code and associated files at:"), 
                                 uiOutput("url1")
                                 )            )
        )
    )
))
