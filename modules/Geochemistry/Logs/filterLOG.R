#record filtering options applied and message when they are reset
observeEvent(input$environmentfilterrender, {
  req(environmentdata$table)
  if(!is.null(av(input$environmentfilteroption1)) && !is.null(av(input$environmentfilteroption2))){
    log_event(input$environmentfilteroption1, input$environmentfilteroption2, name = "Applied environment Filtering Options", type = "", status = "")
  }
})
observeEvent(input$environmentfilterrenderreset,{
  req(environmentdata$table)
  log_event("Reset environment Filters", type = "", status = "")
})