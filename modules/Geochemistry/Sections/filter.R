###Geochemistry filtering options ----
output$environmentfilteringoptionsUI <- renderUI({
  req(environmentdata$table)
  colnames <- names(environmentdata$table1)
  output <- tagList(
    selectInput("environmentfilteroption1", "Select Variable Category",
                choices = c("NULL", colnames),
                selected = "NULL",
                multiple = FALSE)
  )
  return(output)
})
output$environmentfilteringoptionsUI1 <- renderUI({
  req(environmentdata$table)
  output <- tagList(
    if(!is.null(av(input$environmentfilteroption1))){
      if(is.character(environmentdata$table1[[input$environmentfilteroption1]]) || 
         is.factor(environmentdata$table1[[input$environmentfilteroption1]])){
        options <- as.list(unique(environmentdata$table1[[input$environmentfilteroption1]]))
        selectInput("environmentfilteroption2", "Filter Sub Category",
                    choices = c("NULL", options),
                    selected = "NULL",
                    multiple = TRUE)
      }else if(is.numeric(environmentdata$table1[[input$environmentfilteroption1]]) || 
               is.integer(environmentdata$table1[[input$environmentfilteroption1]])){
        min <- min(environmentdata$table1[[input$environmentfilteroption1]])
        max <- max(environmentdata$table1[[input$environmentfilteroption1]])
        sliderInput("environmentfilteroption2", "Filter Sub Category",
                    min = min,
                    max = max,
                    value = c(min, max),
                    step = 1)
      }else if(is.POSIXct(environmentdata$table1[[input$environmentfilteroption1]])){
        sliderInput("environmentfilteroption2", "Filter Sub Category",
                    min = min(environmentdata$table1[[input$environmentfilteroption1]]),
                    max = max(environmentdata$table1[[input$environmentfilteroption1]]),
                    value = c(min(environmentdata$table1[[input$environmentfilteroption1]]),
                              max(environmentdata$table1[[input$environmentfilteroption1]])))
      }
    }
  )
  return(output)
})
output$environmentfilteringoptionsUI2 <- renderUI({
  req(environmentdata$table)
  if(!is.null(av(input$environmentfilteroption1))){
    output <- tagList(
      actionButton("environmentfilterrender", "Filter", width = "100%")
      ,
      hr()
      ,
      actionButton("environmentfilterrenderreset", "Reset Filters", width = "100%")
    )
    return(output)
  }
})
observeEvent(input$environmentfilterrenderreset, {
  req(environmentdata$table)
  environmentdata$filter <- environmentdata$table1
})
observeEvent(input$environmentfilterrender, {
  req(environmentdata$table)
  if(!is.null(av(input$environmentfilteroption1))){
    if(is.character(environmentdata$table1[[input$environmentfilteroption1]]) ||
       is.factor(environmentdata$table1[[input$environmentfilteroption1]])){
      environmentdata$filter <- environmentdata$filter %>% filter(!!as.symbol(input$environmentfilteroption1) %in% input$environmentfilteroption2)
    }else if(is.numeric(environmentdata$table1[[input$environmentfilteroption1]]) || 
             is.integer(environmentdata$table1[[input$environmentfilteroption1]])){
      environmentdata$filter <- environmentdata$filter %>% filter(!!as.symbol(input$environmentfilteroption1) >= input$environmentfilteroption2[1] &
                                                                    !!as.symbol(input$environmentfilteroption1) <= input$environmentfilteroption2[2])
      
    }else if(is.POSIXct(environmentdata$table1[[input$environmentfilteroption1]])){
      environmentdata$filter <- environmentdata$filter %>% filter(!!as.symbol(input$environmentfilteroption1) >= input$environmentfilteroption2[1] &
                                                                    !!as.symbol(input$environmentfilteroption1) <= input$environmentfilteroption2[2])
    }
  }else {
    environmentdata$filter <- NULL
  }
  return(environmentdata$filter)
})
output$environmentoriginaltable2 <- renderDataTable({
  req(environmentdata$table)
  environmentdata$table1
})
output$environmentfilteredtable <- renderDataTable({
  req(environmentdata$table)
  req(environmentdata$filter)
  environmentdata$filter
})
downloadTable(id = "environmentfiltertabledownload", tableid = environmentdata$filter)
#table output, updates when filters are applied
output$environmentfiltertableUI <- renderUI({
  #req(environmentdata$table)
  validate(
    need(!is.null(environmentdata$table), "Please Upload a Dataset")
  )
  
  if(is.null(environmentdata$filter)){
    output <- tagList(
      tags$h3("Original Table")
      ,
      splitLayout(dataTableOutput("environmentoriginaltable2"))
    )
  }else {
    output <- tagList(
      tags$h3("Filtered Table")
      ,
      splitLayout(dataTableOutput("environmentfilteredtable"))
      ,
      downloadTableUI("environmentfiltertabledownload")
    )
  }
  return(output)
})
#Updates radio buttons to display filtered dataset after filters are applied
observeEvent(input$environmentfilterrender, {
  updateRadioButtons(session, "environmentdatasource", "Select Dataset to Use:",
                     choices = c("Original" = "Original",
                                 "Filtered" = "Filtered"),
                     selected = "Original", inline = TRUE)
}, once = TRUE)
observe({
  req(environmentdata$table1)
  if(is.null(environmentdata$filter)){
    environmentdata$use <- environmentdata$table1
  }else{
    if(input$environmentdatasource == "Original" || input$environmentdatasource1 == "Original" || input$environmentdatasource2 == "Original"){
      environmentdata$use <- environmentdata$table1
    }else if(input$environmentdatasource == "Filtered" || input$environmentdatasource1 == "Filtered" || input$environmentdatasource2 == "Filtered"){
      environmentdata$use <- environmentdata$filter
    }
  }
})