###Phenotype filtering options ----
output$phenotypefilteringoptionsUI <- renderUI({
  req(phenotypedata$table)
  colnames <- names(phenotypedata$table1)
  output <- tagList(
    selectInput("phenotypefilteroption1", "Select Variable Category",
                choices = c("NULL", colnames),
                selected = "NULL",
                multiple = FALSE)
  )
  return(output)
})
output$phenotypefilteringoptionsUI1 <- renderUI({
  req(phenotypedata$table)
  output <- tagList(
    if(!is.null(av(input$phenotypefilteroption1))){
      if(is.character(phenotypedata$table1[[input$phenotypefilteroption1]]) || 
         is.factor(phenotypedata$table1[[input$phenotypefilteroption1]])){
        options <- as.list(unique(phenotypedata$table1[[input$phenotypefilteroption1]]))
        selectInput("phenotypefilteroption2", "Filter Sub Category",
                    choices = c("NULL", options),
                    selected = "NULL",
                    multiple = TRUE)
      }else if(is.numeric(phenotypedata$table1[[input$phenotypefilteroption1]]) || 
               is.integer(phenotypedata$table1[[input$phenotypefilteroption1]])){
        min <- min(phenotypedata$table1[[input$phenotypefilteroption1]])
        max <- max(phenotypedata$table1[[input$phenotypefilteroption1]])
        sliderInput("phenotypefilteroption2", "Filter Sub Category",
                    min = min,
                    max = max,
                    value = c(min, max))
      }else if(is.POSIXct(phenotypedata$table1[[input$phenotypefilteroption1]])){
        sliderInput("phenotypefilteroption2", "Filter Sub Category",
                    min = min(phenotypedata$table1[[input$phenotypefilteroption1]]),
                    max = max(phenotypedata$table1[[input$phenotypefilteroption1]]),
                    value = c(min(phenotypedata$table1[[input$phenotypefilteroption1]]),
                              max(phenotypedata$table1[[input$phenotypefilteroption1]])))
      }
    }
  )
  return(output)
})
output$phenotypefilteringoptionsUI2 <- renderUI({
  req(phenotypedata$table)
  if(!is.null(av(input$phenotypefilteroption1))){
    output <- tagList(
      actionButton("phenotypefilterrender", "Filter", width = "100%")
      ,
      hr()
      ,
      actionButton("phenotypefilterrenderreset", "Reset Filters", width = "100%")
    )
    return(output)
  }
})
observeEvent(input$phenotypefilterrenderreset, {
  req(phenotypedata$table)
  phenotypedata$filter <- phenotypedata$table1
})
observeEvent(input$phenotypefilterrender, {
  req(phenotypedata$table)
  if(!is.null(av(input$phenotypefilteroption1))){
    if(is.character(phenotypedata$table1[[input$phenotypefilteroption1]]) ||
       is.factor(phenotypedata$table1[[input$phenotypefilteroption1]])){
      phenotypedata$filter <- phenotypedata$filter %>% filter(!!as.symbol(input$phenotypefilteroption1) %in% input$phenotypefilteroption2)
    }else if(is.numeric(phenotypedata$table1[[input$phenotypefilteroption1]]) || 
             is.integer(phenotypedata$table1[[input$phenotypefilteroption1]])){
      phenotypedata$filter <- phenotypedata$filter %>% filter(!!as.symbol(input$phenotypefilteroption1) >= input$phenotypefilteroption2[1] &
                                                                !!as.symbol(input$phenotypefilteroption1) <= input$phenotypefilteroption2[2])
      
    }else if(is.POSIXct(phenotypedata$table1[[input$phenotypefilteroption1]])){
      phenotypedata$filter <- phenotypedata$filter %>% filter(!!as.symbol(input$phenotypefilteroption1) >= input$phenotypefilteroption2[1] &
                                                                !!as.symbol(input$phenotypefilteroption1) <= input$phenotypefilteroption2[2])
    }
  }else {
    phenotypedata$filter <- NULL
  }
  return(phenotypedata$filter)
})
output$phenotypeoriginaltable2 <- renderDataTable({
  req(phenotypedata$table)
  phenotypedata$table1
})
output$phenotypefilteredtable <- renderDataTable({
  req(phenotypedata$table)
  req(phenotypedata$filter)
  phenotypedata$filter
})
downloadTable(id = "phenotypefiltertabledownload", tableid = phenotypedata$filter)
#table output, updates when filters are applied
output$phenotypefiltertableUI <- renderUI({
  req(phenotypedata$table)
  if(is.null(phenotypedata$filter)){
    output <- tagList(
      tags$h3("Original Table")
      ,
      splitLayout(dataTableOutput("phenotypeoriginaltable2"))
    )
  }else {
    output <- tagList(
      tags$h3("Filtered Table")
      ,
      splitLayout(dataTableOutput("phenotypefilteredtable"))
      ,
      downloadTableUI("phenotypefiltertabledownload")
    )
  }
  return(output)
})