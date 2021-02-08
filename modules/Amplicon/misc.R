##hide sidebar when viewing the tables -----
observeEvent(input[["tabstart"]], {
  if(input[["tabstart"]] == 2 || input[["tabstart"]] == 3 || 
     input[["tabstart"]] == 4 || input[["tabstart"]] == 5){
    hideElement(selector = "#homesidebar")
    removeCssClass("homemain", "col-sm-8")
    addCssClass("homemain", "col-sm-12")
  }else {
    showElement(selector = "#homesidebar")
    removeCssClass("homemain", "col-sm-12")
    addCssClass("homemain", "col-sm-8")
  }
})
observeEvent(input[["filtertabset"]], {
  if(input[["filtertabset"]] == 2 || input[["filtertabset"]] == 3 || 
     input[["filtertabset"]] == 4 || input[["filtertabset"]] == 5){
    hideElement(selector = "#filtersidebar")
    removeCssClass("filtermain", "col-sm-8")
    addCssClass("filtermain", "col-sm-12")
  }else {
    showElement(selector = "#filtersidebar")
    removeCssClass("filtermain", "col-sm-12")
    addCssClass("filtermain", "col-sm-8")
  }
})
###section to change selected dataset(original or filtered) based on radio buttons-----
##after filters are applied, this will update the dataset options (original to original and filtered)
observeEvent(input$phyloseqfilter, {
  if(is.null(phyloseqobj()))return(NULL)
  updateRadioButtons(session, "amplicondatasource", "Select Dataset to Use:",
                     choices = c("Original",
                                 "Filtered"),
                     selected = "Original", inline = TRUE)
}, once = TRUE)
observeEvent(input$phyloseqfilter, {
  if(is.null(phyloseqobj()))return(NULL)
  updateRadioButtons(session, "amplicondatasource1", "Select Dataset to Use:",
                     choices = c("Original",
                                 "Filtered"),
                     selected = "Original", inline = TRUE)
}, once = TRUE)
observeEvent(input$phyloseqfilter, {
  if(is.null(phyloseqobj()))return(NULL)
  updateRadioButtons(session, "amplicondatasource2", "Select Dataset to Use:",
                     choices = c("Original",
                                 "Filtered"),
                     selected = "Original", inline = TRUE)
}, once = TRUE)
observeEvent(input$phyloseqfilter, {
  if(is.null(phyloseqobj()))return(NULL)
  updateRadioButtons(session, "amplicondatasource3", "Select Dataset to Use:",
                     choices = c("Original",
                                 "Filtered"),
                     selected = "Original", inline = TRUE)
}, once = TRUE)
observeEvent(input$phyloseqfilter, {
  if(is.null(phyloseqobj()))return(NULL)
  updateRadioButtons(session, "amplicondatasource4", "Select Dataset to Use:",
                     choices = c("Original",
                                 "Filtered"),
                     selected = "Original", inline = TRUE)
}, once = TRUE)


##change all radio buttons in module to reflect which dataset is chosen
observe({
  if(is.null(updatedphyloseq()))return(NULL)
  if(input$amplicondatasource == "Original"){
    updateRadioButtons(session, "irfDatasetselect", "Select Dataset to Use:",
                       choices = c("Original",
                                   "Filtered"),
                       selected = "Original", inline = TRUE)
    updateRadioButtons(session, "amplicondatasource1", "Select Dataset to Use:",
                       choices = c("Original",
                                   "Filtered"),
                       selected = "Original", inline = TRUE)
    updateRadioButtons(session, "amplicondatasource2", "Select Dataset to Use:",
                       choices = c("Original",
                                   "Filtered"),
                       selected = "Original", inline = TRUE)
    updateRadioButtons(session, "amplicondatasource3", "Select Dataset to Use:",
                       choices = c("Original",
                                   "Filtered"),
                       selected = "Original", inline = TRUE)
    updateRadioButtons(session, "amplicondatasource4", "Select Dataset to Use:",
                       choices = c("Original",
                                   "Filtered"),
                       selected = "Original", inline = TRUE)
  }else if(input$amplicondatasource == "Filtered"){
    updateRadioButtons(session, "irfDatasetselect", "Select Dataset to Use:",
                       choices = c("Original",
                                   "Filtered"),
                       selected = "Filtered", inline = TRUE)
    updateRadioButtons(session, "amplicondatasource1", "Select Dataset to Use:",
                       choices = c("Original",
                                   "Filtered"),
                       selected = "Filtered", inline = TRUE)
    updateRadioButtons(session, "amplicondatasource2", "Select Dataset to Use:",
                       choices = c("Original",
                                   "Filtered"),
                       selected = "Filtered", inline = TRUE)
    updateRadioButtons(session, "amplicondatasource3", "Select Dataset to Use:",
                       choices = c("Original",
                                   "Filtered"),
                       selected = "Filtered", inline = TRUE)
    updateRadioButtons(session, "amplicondatasource4", "Select Dataset to Use:",
                       choices = c("Original",
                                   "Filtered"),
                       selected = "Filtered", inline = TRUE)
  }
})
observe({
  if(is.null(updatedphyloseq()))return(NULL)
  if(input$amplicondatasource1 == "Original"){
    updateRadioButtons(session, "irfDatasetselect", "Select Dataset to Use:",
                       choices = c("Original",
                                   "Filtered"),
                       selected = "Original", inline = TRUE)
    updateRadioButtons(session, "amplicondatasource", "Select Dataset to Use:",
                       choices = c("Original",
                                   "Filtered"),
                       selected = "Original", inline = TRUE)
    updateRadioButtons(session, "amplicondatasource2", "Select Dataset to Use:",
                       choices = c("Original",
                                   "Filtered"),
                       selected = "Original", inline = TRUE)
    updateRadioButtons(session, "amplicondatasource3", "Select Dataset to Use:",
                       choices = c("Original",
                                   "Filtered"),
                       selected = "Original", inline = TRUE)
    updateRadioButtons(session, "amplicondatasource4", "Select Dataset to Use:",
                       choices = c("Original",
                                   "Filtered"),
                       selected = "Original", inline = TRUE)
  }else if(input$amplicondatasource1 == "Filtered"){
    updateRadioButtons(session, "irfDatasetselect", "Select Dataset to Use:",
                       choices = c("Original",
                                   "Filtered"),
                       selected = "Filtered", inline = TRUE)
    updateRadioButtons(session, "amplicondatasource", "Select Dataset to Use:",
                       choices = c("Original",
                                   "Filtered"),
                       selected = "Filtered", inline = TRUE)
    updateRadioButtons(session, "amplicondatasource2", "Select Dataset to Use:",
                       choices = c("Original",
                                   "Filtered"),
                       selected = "Filtered", inline = TRUE)
    updateRadioButtons(session, "amplicondatasource3", "Select Dataset to Use:",
                       choices = c("Original",
                                   "Filtered"),
                       selected = "Filtered", inline = TRUE)
    updateRadioButtons(session, "amplicondatasource4", "Select Dataset to Use:",
                       choices = c("Original",
                                   "Filtered"),
                       selected = "Filtered", inline = TRUE)
  }
})
observe({
  if(is.null(updatedphyloseq()))return(NULL)
  if(input$amplicondatasource2 == "Original"){
    updateRadioButtons(session, "irfDatasetselect", "Select Dataset to Use:",
                       choices = c("Original",
                                   "Filtered"),
                       selected = "Original", inline = TRUE)
    updateRadioButtons(session, "amplicondatasource1", "Select Dataset to Use:",
                       choices = c("Original",
                                   "Filtered"),
                       selected = "Original", inline = TRUE)
    updateRadioButtons(session, "amplicondatasource", "Select Dataset to Use:",
                       choices = c("Original",
                                   "Filtered"),
                       selected = "Original", inline = TRUE)
    updateRadioButtons(session, "amplicondatasource3", "Select Dataset to Use:",
                       choices = c("Original",
                                   "Filtered"),
                       selected = "Original", inline = TRUE)
    updateRadioButtons(session, "amplicondatasource4", "Select Dataset to Use:",
                       choices = c("Original",
                                   "Filtered"),
                       selected = "Original", inline = TRUE)
  }else if(input$amplicondatasource2 == "Filtered"){
    updateRadioButtons(session, "irfDatasetselect", "Select Dataset to Use:",
                       choices = c("Original",
                                   "Filtered"),
                       selected = "Filtered", inline = TRUE)
    updateRadioButtons(session, "amplicondatasource1", "Select Dataset to Use:",
                       choices = c("Original",
                                   "Filtered"),
                       selected = "Filtered", inline = TRUE)
    updateRadioButtons(session, "amplicondatasource", "Select Dataset to Use:",
                       choices = c("Original",
                                   "Filtered"),
                       selected = "Filtered", inline = TRUE)
    updateRadioButtons(session, "amplicondatasource3", "Select Dataset to Use:",
                       choices = c("Original",
                                   "Filtered"),
                       selected = "Filtered", inline = TRUE)
    updateRadioButtons(session, "amplicondatasource4", "Select Dataset to Use:",
                       choices = c("Original",
                                   "Filtered"),
                       selected = "Filtered", inline = TRUE)
  }
})
observe({
  if(is.null(updatedphyloseq()))return(NULL)
  if(input$amplicondatasource3 == "Original"){
    updateRadioButtons(session, "irfDatasetselect", "Select Dataset to Use:",
                       choices = c("Original",
                                   "Filtered"),
                       selected = "Original", inline = TRUE)
    updateRadioButtons(session, "amplicondatasource1", "Select Dataset to Use:",
                       choices = c("Original",
                                   "Filtered"),
                       selected = "Original", inline = TRUE)
    updateRadioButtons(session, "amplicondatasource2", "Select Dataset to Use:",
                       choices = c("Original",
                                   "Filtered"),
                       selected = "Original", inline = TRUE)
    updateRadioButtons(session, "amplicondatasource", "Select Dataset to Use:",
                       choices = c("Original",
                                   "Filtered"),
                       selected = "Original", inline = TRUE)
    updateRadioButtons(session, "amplicondatasource4", "Select Dataset to Use:",
                       choices = c("Original",
                                   "Filtered"),
                       selected = "Original", inline = TRUE)
  }else if(input$amplicondatasource3 == "Filtered"){
    updateRadioButtons(session, "irfDatasetselect", "Select Dataset to Use:",
                       choices = c("Original",
                                   "Filtered"),
                       selected = "Filtered", inline = TRUE)
    updateRadioButtons(session, "amplicondatasource1", "Select Dataset to Use:",
                       choices = c("Original",
                                   "Filtered"),
                       selected = "Filtered", inline = TRUE)
    updateRadioButtons(session, "amplicondatasource2", "Select Dataset to Use:",
                       choices = c("Original",
                                   "Filtered"),
                       selected = "Filtered", inline = TRUE)
    updateRadioButtons(session, "amplicondatasource", "Select Dataset to Use:",
                       choices = c("Original",
                                   "Filtered"),
                       selected = "Filtered", inline = TRUE)
    updateRadioButtons(session, "amplicondatasource4", "Select Dataset to Use:",
                       choices = c("Original",
                                   "Filtered"),
                       selected = "Filtered", inline = TRUE)
  }
})
observe({
  if(is.null(updatedphyloseq()))return(NULL)
  if(input$amplicondatasource4 == "Original"){
    updateRadioButtons(session, "irfDatasetselect", "Select Dataset to Use:",
                       choices = c("Original",
                                   "Filtered"),
                       selected = "Original", inline = TRUE)
    updateRadioButtons(session, "amplicondatasource1", "Select Dataset to Use:",
                       choices = c("Original",
                                   "Filtered"),
                       selected = "Original", inline = TRUE)
    updateRadioButtons(session, "amplicondatasource2", "Select Dataset to Use:",
                       choices = c("Original",
                                   "Filtered"),
                       selected = "Original", inline = TRUE)
    updateRadioButtons(session, "amplicondatasource3", "Select Dataset to Use:",
                       choices = c("Original",
                                   "Filtered"),
                       selected = "Original", inline = TRUE)
    updateRadioButtons(session, "amplicondatasource", "Select Dataset to Use:",
                       choices = c("Original",
                                   "Filtered"),
                       selected = "Original", inline = TRUE)
  }else if(input$amplicondatasource4 == "Filtered"){
    updateRadioButtons(session, "irfDatasetselect", "Select Dataset to Use:",
                       choices = c("Original",
                                   "Filtered"),
                       selected = "Filtered", inline = TRUE)
    updateRadioButtons(session, "amplicondatasource1", "Select Dataset to Use:",
                       choices = c("Original",
                                   "Filtered"),
                       selected = "Filtered", inline = TRUE)
    updateRadioButtons(session, "amplicondatasource2", "Select Dataset to Use:",
                       choices = c("Original",
                                   "Filtered"),
                       selected = "Filtered", inline = TRUE)
    updateRadioButtons(session, "amplicondatasource3", "Select Dataset to Use:",
                       choices = c("Original",
                                   "Filtered"),
                       selected = "Filtered", inline = TRUE)
    updateRadioButtons(session, "amplicondatasource", "Select Dataset to Use:",
                       choices = c("Original",
                                   "Filtered"),
                       selected = "Filtered", inline = TRUE)
    
  }
})
observe({
  if(is.null(updatedphyloseq()))return(NULL)
  if(input$irfDatasetselect == "Original"){
    updateRadioButtons(session, "amplicondatasource", "Select Dataset to Use:",
                       choices = c("Original",
                                   "Filtered"),
                       selected = "Original", inline = TRUE)
    updateRadioButtons(session, "amplicondatasource1", "Select Dataset to Use:",
                       choices = c("Original",
                                   "Filtered"),
                       selected = "Original", inline = TRUE)
    updateRadioButtons(session, "amplicondatasource2", "Select Dataset to Use:",
                       choices = c("Original",
                                   "Filtered"),
                       selected = "Original", inline = TRUE)
    updateRadioButtons(session, "amplicondatasource", "Select Dataset to Use:",
                       choices = c("Original",
                                   "Filtered"),
                       selected = "Original", inline = TRUE)
    updateRadioButtons(session, "amplicondatasource4", "Select Dataset to Use:",
                       choices = c("Original",
                                   "Filtered"),
                       selected = "Original", inline = TRUE)
  }else if(input$irfDatasetselect == "Filtered"){
    updateRadioButtons(session, "amplicondatasource", "Select Dataset to Use:",
                       choices = c("Original",
                                   "Filtered"),
                       selected = "Filtered", inline = TRUE)
    updateRadioButtons(session, "amplicondatasource1", "Select Dataset to Use:",
                       choices = c("Original",
                                   "Filtered"),
                       selected = "Filtered", inline = TRUE)
    updateRadioButtons(session, "amplicondatasource2", "Select Dataset to Use:",
                       choices = c("Original",
                                   "Filtered"),
                       selected = "Filtered", inline = TRUE)
    updateRadioButtons(session, "amplicondatasource", "Select Dataset to Use:",
                       choices = c("Original",
                                   "Filtered"),
                       selected = "Filtered", inline = TRUE)
    updateRadioButtons(session, "amplicondatasource4", "Select Dataset to Use:",
                       choices = c("Original",
                                   "Filtered"),
                       selected = "Filtered", inline = TRUE)
  }
})



##this will change the dataset choice based on which radio button is selected 
ampliconuse <- reactive({
  if(is.null(phyloseqobj()))return(NULL)
  if(is.null(input$phyloseqfilter)){
    data <- phyloseqobj()
  }else{
    if(input$amplicondatasource == "Original" || input$amplicondatasource2 == "Original" || 
       input$amplicondatasource3 == "Original" || input$amplicondatasource4 == "Original" ||
       input$irfDatasetselect == "Original"){
      data <- phyloseqobj()
    }else if(input$amplicondatasource == "Filtered" || input$amplicondatasource2 == "Filtered" ||
             input$amplicondatasource3 == "Filtered" || input$amplicondatasource4 == "Filtered" ||
             input$irfDatasetselect == "Filtered"){
      data <- updatedphyloseq()
    }
  }
  return(data)
})
