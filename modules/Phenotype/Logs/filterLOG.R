#record filtering options applied and message when they are reset
observeEvent(input$phenotypefilterrender, {
  req(phenotypedata$table)
  if(!is.null(av(input$phenotypefilteroption1)) && !is.null(av(input$phenotypefilteroption2))){
    log_event(input$phenotypefilteroption1, input$phenotypefilteroption2, name = "Applied Phenotype Filtering Options", type = "", status = "")
  }
})
observeEvent(input$phenotypefilterrenderreset,{
  req(phenotypedata$table)
  log_event("Reset Phenotype Filters", type = "", status = "")
})