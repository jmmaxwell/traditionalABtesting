

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
      h5("Variable names must match the following exactly: idInventory, Alt, idorder, billingAddress1, shipAddress1, soldPrice, 
         shipPrice, date, trackingIdentifier, tbWeight, methodInternal, shipCost, idShipment"),
      hr(),
      fluidRow(
        textOutput("errorCheck")
      ),
      
      hr(),
      h3("User Data:"),
      hr(),
      h5("Variable names must match the following exactly: Alt, trackingIdentifier, date"),
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
      
      plotOutput("profitPerUserPlot"),
      hr(),
      dataTableOutput("tTester")
      
      )
    
    )
  ),
  
  tabPanel("Summary",
           
    sidebarLayout(sidebarPanel(
      
      downloadButton('downloadData', 'Download')
      
    ),
    
  mainPanel(  
    
    dataTableOutput(outputId = 'summaryTable')
    
    # put download option here
    
    )
  
  ))
  
))


