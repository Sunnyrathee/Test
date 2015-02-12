#server.R

library(shiny)
source("/Users/brentducote/strategicinsights/AllDataManipulation_App.R")

clients <- as.matrix(unique(AD$client))
verticals <- as.matrix(unique(AD$vertical))

# This code is run once
shinyServer(
  function(input, output) {
    #this code is run once per visit
    output$table <- renderTable({

          
      #client <- input$client
      cat("assigning client")
      
      assign("client", input$client, envir = .GlobalEnv)
      source("/Users/brentducote/strategicinsights/AllDataManipulation_App.R")
      goal <<- input$goal
      benchmark <<- input$benchmark
      submit <<- input$submit
      
      
      cat("\n--------------------------------------")
      
      cat("\nClient: ",input$client)
      cat("\nGoal: ",input$goal)
      cat("\nBenchmark: ", input$benchmark)
      cat("\nSubmit: ", input$submit)
           
#matrix(c(input$client, input$goal, input$benchmark, input$submit), ncol=4, byrow=FALSE)
outputMatrix
      })

output$refTable <- renderTable({
  refMatrix
      })
 }
)

 

