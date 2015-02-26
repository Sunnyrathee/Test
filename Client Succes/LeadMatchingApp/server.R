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
      outputtable1 <- leadMatch(infile)
      return(total1)
  })
})
