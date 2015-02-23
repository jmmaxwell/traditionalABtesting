
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyServer(function(input, output) {
  
  ## Conversion Rate ##
  
  results <- reactive({
    
    prop.test(c(input$conversionsControl, input$conversionsTest),
              c(input$observationsControl, input$observationsTest),
              conf.level = 1 - (.05/input$numOfTestGroups))
  
    })
  
  output$testPvalue <- renderText({
  
    paste0('The p-value is: ', 
           ifelse(results()$p.value < 0.00001, "less than 1e-5", round(results()$p.value, 5)*input$numOfTestGroups))
  
  })
  
  output$testEstimate <- renderText({
    
    paste0('The difference in proportions is: ', round(results()$estimate[2] - results()$estimate[1], 4))
    
  })
  
  output$testConfInt <- renderText({
    
    paste0('The 95% confidence interval is: ', round(results()$conf.int[1], 4), ' to ', round(results()$conf.int[2], 4))
    
  })
  
  
  #####################
  
  
  
  ## Data ##
  
  #item
  
  itemData <- reactive({
    
    itemDataFile <- input$itemData
    
    if (is.null(itemDataFile))
      return(NULL)
    
    read.csv(itemDataFile$datapath)
    
  })
  
  output$itemTable <- renderDataTable({
    
    itemData()
    
  })
  
  #user
  
  userData <- reactive({
    
    userDataFile <- input$userData
    
    if (is.null(userDataFile))
      return(NULL)
    
    read.csv(userDataFile$datapath)
    
  })
  
  output$userTable <- renderDataTable({
    
    userData()
    
  })
  
  #########
  
  ## profitability ##
  
  
  
})
