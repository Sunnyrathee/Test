library(shiny)
source("LeadMatching.R")
cat("\nhere 1")
options(shiny.maxRequestSize=500*1024^2)
shinyServer(function(input, output) {
  cat("\nhere 2")
  output$outputtable1 <- renderTable({
    cat("\nhere 3")
      infile <- input$file1
      
      if (is.null(infile))
        return(NULL)
      something <- read.csv(infile$datapath, header=TRUE)
      outputtable1 <- leadMatch(something)
      outputtable1
  })
})
