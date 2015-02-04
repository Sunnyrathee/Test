#server.R

library(shiny)
source("../AllDataManipulation_App.R")

clients <- as.matrix(unique(AD$client))
verticals <- as.matrix(unique(AD1$vertical))

# This code is run once
shinyServer(
  function(input, output) {
    #this code is run once per visit
    output$table <- renderTable({

      cat("\n--------------------------------------")

      cat("\nClient: ",input$client)
      cat("\nGoal: ",input$goal)
      cat("\nBenchmark: ", input$benchmark)
      cat("\nSubmit: ", input$submit)
    
View(input$client)
      
# client <- input$client,
#                  clients = client)


     })
  }
)

 

