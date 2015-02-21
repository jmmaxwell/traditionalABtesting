

library(shiny)

shinyUI(navbarPage("Web Testing", id = "nav",
                   
  tabPanel("Conversion Rate",
           
    sidebarLayout(sidebarPanel(
      
      numericInput("conversionsControl", label = h5("Control Group Conversions"), value = 0),
      numericInput("conversionsTest", label = h5("Test Group Conversions"), value = 0),
      numericInput("observationsControl", label = h5("Control Group Observations"), value = 1),
      numericInput("observationsTest", label = h5("Test Group Observations"), value = 1),
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
           
    fluidRow(
      fileInput("data", label = h4("Upload Data as CSV"))
      ),
    
    fluidRow(
      dataTableOutput(outputId="table")
      )       
  ),
  
  tabPanel("Profitability"
             
  )

))


# testData = data.frame(alt = c(rep("a", 10), rep("b", 10), rep("c", 10)),
#                       costs = runif(30),
#                       revenue = runif(30, min = 1, max = 2),
#                       weight = sample(1:100, 30)
#                       )
# write.csv(testData, file = "testData.csv")


