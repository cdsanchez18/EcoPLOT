##Environmental Data Logs ----
#Informs user of which dataset was selected at the time of their analysis
observe({
  req(environmentdata$table1)
  if(is.null(environmentdata$filter)){
    log_event("Using the Original Dataset", name = "environment Data", type = "", status = "")
  }else{
    if(input$environmentdatasource1 == "Original"){
      log_event("Using the Original Dataset", name = "environment Data", type = "", status = "")
    }else if(input$environmentdatasource1 == "Filtered"){
      log_event("Using the Filtered Dataset", name = "environment Data", type = "", status = "")
    }
  }
})
#informs users of variable class changes and if/when they are reset
observeEvent(input$environmentchangeclass, {
  req(environmentdata$table)
  if(!is.null(av(input$environmentcolnames))){
    if(input$environmentclassoptions == "date"){
      log_event(paste(input$environmentcolnames, "to class", input$environmentclassoptions, "in the format of", input$environmentdateformat),
                name = "environment Data", type = "", status = "")
    }else if(input$environmentclassoptions == "time"){
      log_event(paste(input$environmentcolnames, "to class", input$environmentclassoptions, "in the format of", input$environmenttimeformat),
                name = "environment Data", type = "", status = "")
    }else if(input$environmentclassoptions == "timestamp"){
      log_event(paste(input$environmentcolnames, "to class", input$environmentclassoptions, "in the format of", input$environmentdateformat, input$environmenttimeformat),
                name = "environment Data", type = "", status = "")
    }else {
      log_event(input$environmentcolnames, "to class", input$environmentclassoptions, name = "", type = "", status = "")
    }
  }
})
observeEvent(input$environmentresetclass, {
  req(environmentdata$table)
  log_event("environment Classes Reset", name = "", type = "", status = "")
})