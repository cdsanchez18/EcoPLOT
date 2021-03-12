
observeEvent(input$phenotypeexampledata, {
  req(environmentdata$table1)
  req(phenotypedata$table1)
  if(!is.null(environmentdata$table1) && !is.null(phenotypedata$table1)){
    if(length(intersect(names(environmentdata$table1), names(phenotypedata$table))) >= 1){
      phenotypedata$table1 <- left_join(phenotypedata$table1, environmentdata$table1) %>% na.omit()
      phenotypedata$filter <- phenotypedata$table1
    } 
  }else if(is.null(environmentdata$table)){
    NULL
  }
})


##Add reactivity to dataset choice, if one option changes so will the other 
#geochem radio button
observeEvent(input$environmentfilterrender, {
  updateRadioButtons(session, "environmentdatasource2", "Select Dataset to Use:",
                     choices = c("Original",
                                 "Filtered"),
                     selected = "Original", inline = TRUE)
}, once = TRUE)
#stats radio button
observeEvent(input$environmentfilterrender, {
  updateRadioButtons(session, "environmentdatasource1", "Select Dataset to Use:",
                     choices = c("Original",
                                 "Filtered"),
                     selected = "Original", inline = TRUE)
}, once = TRUE)
#change filter to match ratio
observe({
  req(environmentdata$table)
  req(input$environmentfilterrender)
  if(input$environmentdatasource2 == "Original"){
    updateRadioButtons(session, "environmentdatasource", "Select Dataset to Use:",
                       choices = c("Original",
                                   "Filtered"),
                       selected = "Original", inline = TRUE)
  }else if(input$environmentdatasource2 == "Filtered"){
    updateRadioButtons(session, "environmentdatasource", "Select Dataset to Use:",
                       choices = c("Original",
                                   "Filtered"),
                       selected = "Filtered", inline = TRUE)
  }
})
observe({
  req(environmentdata$table)
  req(input$environmentfilterrender)
  if(input$environmentdatasource2 == "Original"){
    updateRadioButtons(session, "environmentdatasource1", "Select Dataset to Use:",
                       choices = c("Original",
                                   "Filtered"),
                       selected = "Original", inline = TRUE)
  }else if(input$environmentdatasource2 == "Filtered"){
    updateRadioButtons(session, "environmentdatasource1", "Select Dataset to Use:",
                       choices = c("Original",
                                   "Filtered"),
                       selected = "Filtered", inline = TRUE)
  }
})
#change filter to match stats
observe({
  req(environmentdata$table)
  req(input$environmentfilterrender)
  if(input$environmentdatasource1 == "Original"){
    updateRadioButtons(session, "environmentdatasource", "Select Dataset to Use:",
                       choices = c("Original",
                                   "Filtered"),
                       selected = "Original", inline = TRUE)
  }else if(input$environmentdatasource1 == "Filtered"){
    updateRadioButtons(session, "environmentdatasource", "Select Dataset to Use:",
                       choices = c("Original",
                                   "Filtered"),
                       selected = "Filtered", inline = TRUE)
  }
})
observe({
  req(environmentdata$table)
  req(input$environmentfilterrender)
  if(input$environmentdatasource1 == "Original"){
    updateRadioButtons(session, "environmentdatasource2", "Select Dataset to Use:",
                       choices = c("Original",
                                   "Filtered"),
                       selected = "Original", inline = TRUE)
  }else if(input$environmentdatasource1 == "Filtered"){
    updateRadioButtons(session, "environmentdatasource2", "Select Dataset to Use:",
                       choices = c("Original",
                                   "Filtered"),
                       selected = "Filtered", inline = TRUE)
  }
})
#change stats to match filter 
observe({
  req(environmentdata$table)
  req(input$environmentfilterrender)
  if(input$environmentdatasource == "Original"){
    updateRadioButtons(session, "environmentdatasource1", "Select Dataset to Use:",
                       choices = c("Original",
                                   "Filtered"),
                       selected = "Original", inline = TRUE)
  }else if(input$environmentdatasource == "Filtered"){
    updateRadioButtons(session, "environmentdatasource1", "Select Dataset to Use:",
                       choices = c("Original",
                                   "Filtered"),
                       selected = "Filtered", inline = TRUE)
  }
})
observe({
  req(environmentdata$table)
  req(input$environmentfilterrender)
  if(input$environmentdatasource == "Original"){
    updateRadioButtons(session, "environmentdatasource2", "Select Dataset to Use:",
                       choices = c("Original",
                                   "Filtered"),
                       selected = "Original", inline = TRUE)
  }else if(input$environmentdatasource == "Filtered"){
    updateRadioButtons(session, "environmentdatasource2", "Select Dataset to Use:",
                       choices = c("Original",
                                   "Filtered"),
                       selected = "Filtered", inline = TRUE)
  }
})

#hide sidebar panel to read instructions for performing statistics 
observeEvent(input[["environmentstats"]], {
  if(input[["environmentstats"]] == 1){
    hideElement(selector = "#environmentstatssidebar")
    removeCssClass("environmentstats1", "col-sm-8")
    addCssClass("environmentstats1", "col-sm-12")
  }else {
    showElement(selector = "#environmentstatssidebar")
    removeCssClass("environmentstats1", "col-sm-12")
    addCssClass("environmentstats1", "col-sm-8")
  }
})

observeEvent(input$environmentsidebarhide == TRUE, {
  req(input$environmentsidebarhide == TRUE)
  hideElement(selector = "#environmentplotsidebar")
  removeCssClass("environmentplotmainpanel", "col-sm-8")
  addCssClass("environmentplotmainpanel", "col-sm-12")
})
observeEvent(input$environmentsidebarhide == FALSE, {
  req(input$environmentsidebarhide == FALSE)
  showElement(selector = "#environmentplotsidebar")
  removeCssClass("environmentplotmainpanel", "col-sm-12")
  addCssClass("environmentplotmainpanel", "col-sm-8")
})

