#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

source("./PredNextWord.R")

ui <- fluidPage(
   
   # Application title
   titlePanel("Mark Barkell's Text Prediction Fun"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         textInput("queryText",
                     "Current Text Needing Suggestions"
                     ),
         submitButton("Request Prediction", icon("refresh"))
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         textOutput("predictiveText")
      )
   )
)

giveForthPossibleString <- function(model, queryText) {
  predictionCandidates <- predictBasedOnPrev(model, queryText)
  
  availableWords <- data.frame(word = rev(predictionCandidates)$word) %>% top_n(n=20)
  txt <- paste(availableWords$word)
  
  return (txt)
}

# Define server logic required to draw a histogram
server <- function(input, output) {
   ba <- readRDS("./all10.cnt.rds")
   output$predictiveText <- renderText({
     possible <- giveForthPossibleString(ba,input$queryText)
     return (possible)
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

