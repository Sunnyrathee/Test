# ui.R

AD <- read.csv("/Users/brentducote/Dropbox/Strategic Insights/R Directory/Copy Analysis/AllData_Worksheet.csv", header=TRUE)
BM = 0.0084

clients <- as.matrix(unique(AD$client))
verticals <- as.matrix(unique(AD$vertical))
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