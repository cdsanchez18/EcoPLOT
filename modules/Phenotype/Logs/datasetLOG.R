##Plant Data Logs ----
#Informs user of which dataset was selected at the time of their analysis
observe({
  req(phenotypedata$table1)
  if(is.null(phenotypedata$filter)){
    log_event("Using the Original Dataset", name = "Plant Data", type = "", status = "")
  }else{
    if(input$phenotypedatasource1 == "Original"){
      log_event("Using the Original Dataset", name = "Plant Data", type = "", status = "")
    }else if(input$phenotypedatasource1 == "Filtered"){
      log_event("Using the Filtered Dataset", name = "Plant Data", type = "", status = "")
    }
  }
})
#informs users of variable class changes and if/when they are reset
observeEvent(input$plantchangeclass, {
  req(phenotypedata$table)
  if(!is.null(av(input$plantcolnames))){
    if(input$plantclassoptions == "date"){
      log_event(paste(input$plantcolnames, "to class", input$plantclassoptions, "in the format of", input$phenotypedateformat),
                name = "Plant Data", type = "", status = "")
    }else if(input$plantclassoptions == "time"){
      log_event(paste(input$plantcolnames, "to class", input$plantclassoptions, "in the format of", input$phenotypetimeformat),
                name = "Plant Data", type = "", status = "")
    }else if(input$plantclassoptions == "timestamp"){
      log_event(paste(input$plantcolnames, "to class", input$plantclassoptions, "in the format of", input$phenotypedateformat, input$phenotypetimeformat),
                name = "Plant Data", type = "", status = "")
    }else {
      log_event(input$plantcolnames, "to class", input$plantclassoptions, name = "", type = "", status = "")
    }
  }
})
observeEvent(input$plantresetclass, {
  req(phenotypedata$table)
  log_event("Plant Classes Reset", name = "", type = "", status = "")
})
