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
        withTags ( h2("Prediction based on the words without consider the last a partial word:") ),
         textOutput("predictiveText"),
         withTags ( h2("Based on paritial characters of last word:") ),
         textOutput("predictiveFilteringOnLastWord")
      )
   )
)

giveForthPossibleString <- function(model, queryText) {
  txt <- "No prediction yet."
  if (!grepl("^ *$", queryText)) {
    predictionCandidates <- predictBasedOnPrev(model, queryText)
    
    availableWords <- data.frame(itemid = 1:(dim(predictionCandidates)[1]), word = rev(predictionCandidates)$word)
    availableWords <- availableWords %>% top_n(n=20, itemid)
    txt <- paste(availableWords$word)
    txt <- paste(txt, ".")
    print(paste("all query text:", queryText, "result:", txt))
  }
  return (txt)
}

giveForthPossibleStringFilteringOnLastWord <- function(model, queryText) {
  txt <- "No prediction yet."
  if (!grepl("^ *$", queryText)) {
    pattern <- "(.*) +([a-z]+) *$"
    startWords <- sub(pattern, "\\1", queryText, ignore.case = TRUE, perl = TRUE)
    lastWord <- sub(pattern, "\\2", queryText, ignore.case = TRUE, perl = TRUE)
    
    print(paste("last word queryText", queryText,",start words:", startWords, ", last word:", lastWord))
    predictionCandidates <- predictBasedOnPrev(model, startWords)
    availableWords <- data.frame(itemid = 1:(dim(predictionCandidates)[1]), word = rev(predictionCandidates)$word) %>% filter(grepl(paste0("^", lastWord), word))
    availableWords <- availableWords %>% top_n(n=20, itemid)
    txt <- paste(availableWords$word)
    txt <- paste(txt, ".")
  }
  return (txt)
}

# Define server logic required to draw a histogram
server <- function(input, output) {
   ba <- readRDS("./all10.cnt.rds")
   print(paste("ba dim", dim(ba)))
   output$predictiveText <- renderText({
     possible <- giveForthPossibleString(ba,input$queryText)
     return (possible)
   })
   output$predictiveFilteringOnLastWord <- renderText({
     possible <- giveForthPossibleStringFilteringOnLastWord(ba, input$queryText)
     return (possible)
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

