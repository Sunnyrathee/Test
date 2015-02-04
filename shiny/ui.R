# ui.R
clients <- as.matrix(unique(AD$client))
verticals <- as.matrix(unique(AD1$vertical))
shinyUI(fluidPage(
  titlePanel("Insightpool SI tool (by Client)"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Enter in the appropriate information below:"),
    
      selectInput("client", 
                  label = "Choose client",
                  choices = clients,
                  selected = ""),
  
      selectInput("goal", 
                  label = "Choose goal",
                  choices = c("", "Engagement Rate", "CTR"),
                  selected = ""),
            
      sliderInput("benchmark", 
                  label = ("Set your benchmark - default is set to internal average - (%)"),
                  min = 0, max = 5, value = BM*100, step = .01),
      
      
      actionButton("submit",
                  label = "Submit")
                      
       ),
    
    mainPanel(tableOutput("table"))
  )
))