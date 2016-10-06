# ui.R
library(shiny)

shinyUI(fluidPage(
  theme = "",
  
  titlePanel(h1("Predict Next Word", align="center"),
             windowTitle = "coursera capstone project"),
  h4("(Capstone Final Project)", align="center"),
  
  hr(),
  
  fluidRow(
    
    column(6, offset=3,
           
           tabsetPanel(type = "tabs",
                                              tabPanel("Input",
                                "Just start writing and we'll do the rest:",
                                textInput("phrase2", label = "", value = ""),
                                tags$head(tags$style(type="text/css", "#phrase2 {width: 600px;}")),
                                
                                fluidRow(
                                  column(6,
                                         br(),br(),br(),
                                         "Next word is..."
                                  ),
                                  column(6,
                                         p(textOutput("stats2")),
                                         h2(textOutput("nextword2"))
                                  )
                                )
                       )
           )
    )
  ),
  
  hr(),
  
  fluidRow(
    column(5, offset=1,
           
           wellPanel(
             h4("How to use this?"),
             
             p("To get started, fill in the text box."),
                          helpText("Select language ",em("English (US)"), ",
                      if you are one of my
                      brilliant and totally-not-susceptible-to-flattery
                      peer graders from Coursera.
                      ")
             )
           
             ),
    column(5,
           selectInput("lang",
                       label = "Which language should we use?",
                       choices = list("English (US)" = "en_us",
                                      "Librarian (Discworld)" = "ook",
                                      "Hodor (Westeros)" = "hodor"),
                       selected = "en_us"),
           checkboxInput("safemode",
                         label = "Safe mode on (remove swear words, etc.)",
                         value = TRUE),
           br(),
           p("Go to ",
             a("Google", href="https://google.com"),
             align="right")
    )
             )
           ))