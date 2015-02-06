# ui.R

AD <- read.csv("/Users/brentducote/Dropbox/Strategic Insights/R Directory/Copy Analysis/AllData_Worksheet.csv", header=TRUE)
#BM <- .0084

clients <- as.matrix(unique(AD$client))
verticals <- as.matrix(unique(AD$vertical))
imps <- AD$impressions
linkImps <- AD$impressions [AD$link == "1"]
RTs <- AD$retweets
replies <- AD$replies
favs <- AD$favorites
clicks <- AD$url.clicks
engmnt <- clicks + favs + replies + RTs
overallER <- sum(engmnt)/sum(imps)

shinyUI(fluidPage(
  titlePanel("Insightpool SI tool (by Client)"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Enter in the appropriate information below:"),
    
      selectInput("client", 
                  label = "Choose client",
                  choices = clients,
                  selected = clients[1]),
  
      selectInput("goal", 
                  label = "Choose goal",
                  choices = c("", "Engagement Rate", "CTR"),
                  selected = ""),
            
      numericInput("benchmark", 
                   label = "Enter benchmark",
                   value = .0084, step = .001),
      
      actionButton("submit", label = "Submit")

       ),
    
    mainPanel(tableOutput("table"))
  )
))