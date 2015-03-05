

library(shiny)
library(ggplot2)

shinyUI(navbarPage("Web Testing", id = "nav",
                   
  tabPanel("Conversion Rate",
           
    sidebarLayout(sidebarPanel(
      
      numericInput("conversionsControl", label = h5("Control Group Conversions"), value = 100),
      numericInput("conversionsTest", label = h5("Test Group Conversions"), value = 100),
      numericInput("observationsControl", label = h5("Control Group Observations"), value = 1000),
      numericInput("observationsTest", label = h5("Test Group Observations"), value = 1000),
      numericInput("numOfTestGroups", label = h5("Number of Test Groups"), value = 1)
      
    ),
    
    mainPanel(
      fluidRow(
        column(10,
               
           h3("Test Results"),  
           hr(),
           textOutput('testPvalue'),
           hr(),
           textOutput('testEstimate'),
           hr(),
           textOutput('testConfInt')
               
          )
        )
      )
    )        
  ),
  
  tabPanel("Data",

    sidebarLayout(sidebarPanel(
      
        fluidRow(
          fileInput("itemData", label = h4("Upload Item Data as CSV"))
        ),
        
        
        fluidRow(
          fileInput("userData", label = h4("upload User Data as CSV"))
        )
      
      ),
    
    mainPanel(
      
      h3("Item Data:"),
      hr(),
      fluidRow(
        textOutput("errorCheck")
      ),
      
      hr(),
      h3("User Data:"),
      hr(),
      fluidRow(
        textOutput("errorCheckUser")  
      )
      
    )
  )  
    
  ),
    
  tabPanel("Profitability",
           
    sidebarLayout(sidebarPanel(
      
      checkboxInput("includeDropShip", label = "Include Drop Shippers", value = TRUE), #add explanation
      hr(),
      checkboxInput("includeNonShipped", label = "Include Orders That Have Not Shipped", value = TRUE),
      hr(),
      numericInput("perItemCost", label = h5("Enter per item cost"), value = 1.05),
      hr(),
      numericInput("weightBasedCost", label = h5("Enter cost per 1/100th lb"), value = 0.0015)
      
      ),
    
    mainPanel(
      
      plotOutput("summaryPlot")
      
      )
    
    )
  ),
  
  tabPanel("Summary",
           
    fluidRow(
      dataTableOutput(outputId = 'summaryTable')         
    )
  )
                   
))


# testData = data.frame(alt = c(rep("a", 5), rep("b", 5), rep("c", 5), rep("a", 5), rep("b", 5), rep("c", 5)),
#                       idOrder = sample(1:20, 30, replace = T),
#                       idItem = c(1:30),
#                       costs = runif(30),
#                       revenue = runif(30, min = 1, max = 2),
#                       weight = sample(1:100, 30),
#                       idAccount = c(1:5, 11:15, 21:25, 1:5, 11:15, 21:25))
#                       
# write.csv(testData, file = "testData.csv")
# 
# testDataUser = data.frame(idAccount = 1:312, 
#                           alt = c(rep("a", 10), rep("b", 10), rep("c", 10), 
#                                   sample(c("a", "b", "c"), (312-30), replace = T)))
# write.csv(testDataUser, file = "testDataUser.csv")
