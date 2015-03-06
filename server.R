nameList = c("idInventory", "Alt", "idorder", "billingAddress1", "shipAddress1","soldPrice",
             "shipPrice", "date", "trackingIdentifier", "tbWeight", "methodInternal", 
             "shipCost", "idShipment")
nameListUser = c("Alt", "trackingIdentifier", "date")

library(shiny)
library(dplyr)
library(ggplot2)
library(lubridate)
library(data.table)

options(shiny.maxRequestSize=50*1024^2)

shinyServer(function(input, output) {
  
  ## Conversion Rate ##
  
  results <- reactive({
    
    prop.test(c(input$conversionsControl, input$conversionsTest),
              c(input$observationsControl, input$observationsTest),
              conf.level = 1 - (.05/input$numOfTestGroups))
    
  })
  
  output$testPvalue <- renderText({
    
    paste0('The p-value is: ', 
           ifelse(results()$p.value < 0.00001, "less than 1e-5", 
                  ifelse(round(results()$p.value, 5)*input$numOfTestGroups > 1, 1, round(results()$p.value, 5)*input$numOfTestGroups)))
    
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
    
    read.csv(itemDataFile$datapath, stringsAsFactors = F)
    
    
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
    
    read.csv(userDataFile$datapath, stringsAsFactors = F)
    
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
  
  
  ## add some data error handling (e.g. nulls, making sure variables are the correct format, etc)
  
  DropShippers <- reactive({
  
      itemData <- itemData()
    
    if(!is.null(itemData)){
      
      itemData %>%
        group_by(trackingIdentifier, Alt, day(date)) %>%
        summarise(ordersPerUser = n_distinct(idorder)) %>%
        group_by(trackingIdentifier) %>%
        summarise(dailyOrders = mean(ordersPerUser)) %>%
        filter(dailyOrders >= 1.1) %>%
        select(trackingIdentifier) -> DropShippers
      
      return(DropShippers)
      
    } else {
      
      return(NULL)
      
    }
    
  })
  

  lastDay <- reactive({
    
    itemData <- itemData()
    
    if(!is.null(itemData)){
      
      itemData %>%
        mutate(hasShipCost = ifelse(is.na(as.numeric(shipCost)), 0, 1)) %>%
        group_by(day = as.Date(date)) %>%
        summarise(propShipped = mean(hasShipCost)) %>%
        filter(propShipped >= .9) -> goodDays
      
      lastDay = max(goodDays$day)
      
      return(lastDay)
      
    } else {
      
      return(NULL)
      
    }
    
  })
  
  
  #### filtering Item Data ####
  
  filteredItemData <- reactive({
    
    itemData <- itemData()
    DropShippers <- DropShippers()
    lastDay <- lastDay()
    
    if (is.null(itemData)){
      
      return(itemData)
      
    } else {
      
      itemData$soldPrice <- as.numeric(itemData$soldPrice)
      itemData$shipPrice <- as.numeric(itemData$shipPrice)
      itemData$date <- as.POSIXct(itemData$date)
      itemData$tbWeight <- as.numeric(itemData$tbWeight)
      itemData$shipCost <- as.numeric(itemData$shipCost)
      
      if (!(input$includeDropShip | input$includeNonShipped)){
        
       itemData %>%
          filter(!(trackingIdentifier %in% DropShippers$trackingIdentifier),
                 as.Date(date) <= lastDay + 1) -> filteredItemData
        
        return(filteredItemData)
        
      } else {
        
        if (!input$includeDropShip) {
          
          itemData %>%
            filter(!(trackingIdentifier %in% DropShippers$trackingIdentifier)) -> filteredItemData
          
          return(filteredItemData)
          
        } else {
          
          if (!input$includeNonShipped){
            
            itemData %>%
              filter(as.Date(date) <= lastDay + 1) -> filteredItemData
            
            return(filteredItemData)
            
          } else {
            
            return(itemData)
            
          }
          
        }
        
      }
      
    }
    
  })
  
  
  #### filtering user Data ####
  
  filteredUserData <- reactive({
    
    userData <- userData()
    DropShippers <- DropShippers()
    lastDay <- lastDay()
    
    if (is.null(userData)){
      
      return(NULL)
      
    } else {
      
      userData$date <- as.POSIXct(userData$date)
      
      if (!input$includeDropShip & !input$includeNonShipped){
        
        userData %>%
          filter(!(trackingIdentifier %in% DropShippers$trackingIdentifier),
                 as.Date(date) <= lastDay + 1) -> filteredUserData
        
        return(filteredUserData)
        
      } else {
        
        if (!input$includeDropShip) {
          
          userData %>%
            filter(!(trackingIdentifier %in% DropShippers$trackingIdentifier)) -> filteredUserData
          
          return(filteredUserData)
          
        } else {
          
          if (!input$includeNonShipped){
            
            userData %>%
              filter(as.Date(date) <= lastDay + 1) -> filteredUserData
            
            return(filteredUserData)
            
          } else {
            
            return(userData)
            
          }
          
        }
        
      }
      
    }
    
  })
  
  #### make some sort of report
  
  #### output summary data
  
  summaryData <- reactive({
    
    filteredUserData <- filteredUserData()
    filteredItemData <- filteredItemData()
    
    if (is.null(filteredUserData) | is.null(filteredItemData)){
      
      return(NULL)
      
    } else {
    
      weightCost <- input$weightBasedCost
      itemCost <- input$perItemCost
      
      filteredItemData %>%
        mutate(revenue = (soldPrice + shipPrice), cost = (tbWeight * weightCost + itemCost)) %>%
        mutate(profit = revenue - cost) -> profitData
      
      profitData %>%
        mutate(paidForShipping = ifelse(shipPrice == 0, 0, 1)) %>%
        group_by(Alt) %>%
        summarise(userWhoBought = n_distinct(trackingIdentifier),
                  Profit = sum(profit, na.rm = T),
                  Revenue = sum(revenue, na.rm = T),
                  Cost = sum(cost, na.rm = T),
                  Orders = n_distinct(idorder),
                  Items = n_distinct(idInventory),
                  Shipments = n_distinct(idShipment),
                  soldRevenue = sum(soldPrice),
                  shipRevenue = sum(shipPrice),
                  shipCost = sum(shipCost, na.rm = T),
                  paidForShipping = sum(paidForShipping, na.rm = T)) -> groupedProfitData
      
      filteredUserData %>%
        group_by(Alt) %>%
        summarise(totalUsers = n_distinct(trackingIdentifier)) -> groupedUserData
      
      groupedProfitData %>%
        inner_join(groupedUserData, by = "Alt") %>%
        mutate(buyersPerUser = userWhoBought/totalUsers,
               profitPerUser = Profit/totalUsers,
               revenuePerUser = Revenue/totalUsers,
               costPerUser = Cost/totalUsers,
               ordersPerUser = Orders/totalUsers,
               itemsPerUser = Items/totalUsers,
               profitPerBuyer = Profit/userWhoBought,
               revenuePerBuyer = Revenue/userWhoBought,
               costPerBuyer = Cost/userWhoBought,
               ordersPerBuyer = Orders/userWhoBought,
               itemsPerBuyer = Items/userWhoBought,
               revenuePerItem = Revenue/Items) -> summaryData
      
      return(summaryData)
      
    }
    
  })
  
  
  output$downloadData <- downloadHandler(
    filename = function() { 
      paste('summaryData', Sys.Date(),'.csv', sep='') 
    },
    content = function(file) {
      write.csv(summaryData(), file)
    } 
    
  )
  
  
  output$summaryTable <- renderDataTable({
    
    if(!is.null(summaryData())){
      
      summaryData()
      
    } else {
      
      NULL
      
    }
    
  })
  
  ##### t-tests:
  
  profitPerUser <- reactive({
    
    if (is.null(filteredUserData())) {
      
      return(NULL)
      
    } else {
    
      filteredUserData <- filteredUserData()
      filteredItemData <- filteredItemData()
      
      weightCost <- input$weightBasedCost
      itemCost <- input$perItemCost
      
      filteredUserData %>%
        group_by(trackingIdentifier) %>%
        summarise(Alt = max(Alt)) %>%
        filter(trackingIdentifier != "") %>%
        select(trackingIdentifier, Alt) -> uniqueUsers
      
      filteredItemData %>%
        mutate(revenue = (soldPrice + shipPrice), cost = (tbWeight * weightCost + itemCost)) %>% 
        mutate(profit = revenue - cost) %>%
        group_by(trackingIdentifier, Alt) %>%
        summarise(userProfit = sum(profit, na.rm = T)) %>%
        filter(trackingIdentifier != "") -> profitData
      
      uniqueUsers %>%
        left_join(profitData, by = c("trackingIdentifier", "Alt")) %>%
        mutate(profit = ifelse(is.na(userProfit), 0, userProfit)) %>%
        select(trackingIdentifier, Alt, profit) -> profitPerUser
      
      return(profitPerUser)
      
    }
      
  })
  
  
  plotData <- reactive({
    
    if (!is.null(profitPerUser())) {
      
      profitPerUser() %>%
      group_by(Alt) %>%
      summarise(avgProfitPerUser = mean(profit, na.rm = T)) -> plotData
      
      return(plotData)
      
    } else {
      
      return(NULL)
      
    }
  
  })
  
  
  output$profitPerUserPlot <- renderPlot({
    
    if (is.null(plotData())) {
      
      return(NULL)
      
    } else {
      
      ggplot(plotData(), aes(x = Alt, y = avgProfitPerUser, fill = Alt)) + geom_bar(stat = "identity")
      
    }
  
  })
  
  
  output$tTester <- renderDataTable({
    
    if (is.null(profitPerUser())) {
      
      return(NULL)
      
    } else {
      
      
      profitPerUser <- profitPerUser()
    
    names <- unique(itemData()$Alt)
    
    grid <- expand.grid(names, names, stringsAsFactors = FALSE)
    grid <- grid[!(grid$Var1 == grid$Var2),]
    
    Ttester <- function(row){
      
      profitPerUser %>%
        filter(Alt == grid$Var1[row]) %>%
        select(profit) -> testGroup1
      
      profitPerUser %>%
        filter(Alt == grid$Var2[row]) %>%
        select(profit) -> testGroup2
      
      results <- t.test(testGroup1$profit, testGroup2$profit)
      
      pvalue <- results$p.value
      confint <- results$conf.int
      difference <- results$estimate
      
      data <- data.frame(group1       = grid$Var1[row],
                         group2       = grid$Var2[row],
                         pvalue       = pvalue,
                         confintLower = confint[1],
                         confintUpper = confint[2],
                         difference   = difference)
    return(data)
      
    }
    
    bigList <- lapply(X = (1:nrow(grid)), FUN = Ttester)
  
    output <- rbindlist(bigList)
    
    output <- data.table(output)
    
    return(output)
      
    }
    
    
  })
  
  
  
})

