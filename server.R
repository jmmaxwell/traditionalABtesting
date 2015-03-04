nameList = c("X", "alt", "idOrder", "idItem", "costs", "revenue", "weight", "idAccount")
nameListUser = c("X", "alt", "idAccount")

library(shiny)
library(dplyr)
library(ggplot2)

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
  
  
  errorCheck <- reactive({
    
    if(is.null(itemData())){
      return("No Data Uploaded")
    }else{
      if(length(names(itemData())) != length(intersect(names(itemData()), nameList))){
        return("Error: names must follow naming convention")
      }else{
        return("Data is in the correct format")
      }
    }
    
  })
  
  output$errorCheck <- renderText({
    
    errorCheck()   
    
  })
  
  #user
  
  userData <- reactive({
    
    userDataFile <- input$userData
    
    if (is.null(userDataFile))
      return(NULL)
    
    read.csv(userDataFile$datapath)
    
  })
  
  
  errorCheckUser <- reactive({
    
    if(is.null(userData())){
      return("No Data Uploaded")
    }else{
      if(length(names(userData())) != length(intersect(names(userData()), nameListUser))){
        return("Error: names must follow naming convention")
      }else{
        return("Data is in the correct format")
      }
    }
    
  })
  
  output$errorCheckUser <- renderText({
    
    errorCheckUser()   
    
  })
  
  #####################
  
  ## profitability ##
  
  userItemData <- reactive({
    
    userData <- userData()
    itemData <- itemData()
    
    if(nameList %in% names(userData) & nameListUser %in% names(itemData)){
      
      userData %>%
        left_join(itemData, by = c("idAccount", "alt")) %>%
        mutate(costs = ifelse(is.na(costs), 0, costs)) %>%
        mutate(revenue = ifelse(is.na(revenue), 0, revenue)) %>%
        mutate(profit = revenue - costs) %>%
        group_by(alt) %>%
        summarise(totalProfit = sum(profit)) -> userItemData
      
      return(userItemData)
      
    }else{
      
      return(NULL)
      
    }
    
  })
  
  output$names <- renderText({
    
    if (is.null(input$itemData))
      return('No Data')
    
    names(itemData())
    
  })
  
  output$profitPlot <- renderPlot({
    
    userItemData <- userItemData()
    
    if (!is.null(userItemData)){
      
      ggplot(userItemData, aes(x = alt, y = totalProfit, fill = alt)) + 
        geom_bar(stat = "identity")
      
    } else {
      return(NULL)
    }
    
    
  })
  
  
})