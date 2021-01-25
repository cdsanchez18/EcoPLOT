##Amplicon Data Logs ----
#Informs user of what dataset they are using 
observe({
  if(is.null(phyloseqobj()))return(NULL)
  if(is.null(input$phyloseqfilter)){
    log_event("Using the Original Dataset", name = "Amplicon Data", type = "", status = "")
  }else{
    if(input$amplicondatasource == "Original"){
      log_event("Using the Original Dataset", name = "Amplicon Data", type = "", status = "")
    }else if(input$amplicondatasource == "Filtered"){
      log_event("Using the Filtered Dataset", name = "Amplicon Data", type = "", status = "")
    }
  }
})
