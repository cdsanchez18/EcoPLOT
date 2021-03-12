
##Add reactivity to dataset choice, if one option changes so will the other 
observeEvent(input$phenotypefilterrender, {
  updateRadioButtons(session, "phenotypedatasource1", "Select Dataset to Use:",
                     choices = c("Original",
                                 "Filtered"),
                     selected = "Original", inline = TRUE)
}, once = TRUE)
observe({
  req(phenotypedata$table)
  req(input$phenotypefilterrender)
  if(input$phenotypedatasource1 == "Original"){
    updateRadioButtons(session, "phenotypedatasource", "Select Dataset to Use:",
                       choices = c("Original",
                                   "Filtered"),
                       selected = "Original", inline = TRUE)
  }else if(input$phenotypedatasource1 == "Filtered"){
    updateRadioButtons(session, "phenotypedatasource", "Select Dataset to Use:",
                       choices = c("Original",
                                   "Filtered"),
                       selected = "Filtered", inline = TRUE)
  }
})
observe({
  req(phenotypedata$table)
  req(input$phenotypefilterrender)
  if(input$phenotypedatasource == "Original"){
    updateRadioButtons(session, "phenotypedatasource1", "Select Dataset to Use:",
                       choices = c("Original",
                                   "Filtered"),
                       selected = "Original", inline = TRUE)
  }else if(input$phenotypedatasource == "Filtered"){
    updateRadioButtons(session, "phenotypedatasource1", "Select Dataset to Use:",
                       choices = c("Original",
                                   "Filtered"),
                       selected = "Filtered", inline = TRUE)
  }
})
#This section preserves the option currently selected, but updates the list to include newly made columns
currentselectionx1 <- reactiveVal(NULL)
observeEvent(input$phenotypex1, {
  currentselectionx1(input$phenotypex1)
})
observeEvent(input$phenotypeactionbutton, {
  updateSelectInput(session, "phenotypex1", "Select Variable to Graph Along X-Axis:", 
                    choices = c("NULL", colnames(phenotypedata$table1)),
                    selected = currentselectionx1()
  )
})
currentselectiony <- reactiveVal(NULL)
observeEvent(input$phenotypey, {
  currentselectiony(input$phenotypey)
})
observeEvent(input$phenotypeactionbutton, {
  updateSelectInput(session, "phenotypey", "Select Variable to Graph Along Y Axis:",
                    choices = c("NULL", colnames(phenotypedata$table1)),
                    selected = currentselectiony())
})
currentselectioncolor <- reactiveVal(NULL)
observeEvent(input$phenotypecoloroption, {
  currentselectioncolor(input$phenotypecoloroption)
})
observeEvent(input$phenotypeactionbutton, {
  updateSelectInput(session, "phenotypecoloroption", "Select Factor to Color",
                    choices = c("NULL", names(dplyr::select_if(phenotypedata$table1, is.character)),
                                names(dplyr::select_if(phenotypedata$table1, is.factor))),
                    selected = currentselectioncolor())
})

observe({
  req(phenotypedata$table1)
  if(is.null(phenotypedata$filter)){
    phenotypedata$use <- phenotypedata$table1
  }else{
    if(input$phenotypedatasource1 == "Original"){
      phenotypedata$use <- phenotypedata$table1
    }else if(input$phenotypedatasource1 == "Filtered"){
      phenotypedata$use <- phenotypedata$filter
    }
  }
})


#hide sidebar panel to read instructions for performing statistics 
observeEvent(input[["plantstats"]], {
  if(input[["plantstats"]] == 1){
    hideElement(selector = "#plantstatssidebar")
    removeCssClass("plantstats1", "col-sm-8")
    addCssClass("plantstats1", "col-sm-12")
  }else {
    showElement(selector = "#plantstatssidebar")
    removeCssClass("plantstats1", "col-sm-12")
    addCssClass("plantstats1", "col-sm-8")
  }
})

observeEvent(input$phenotypesidebarhide == TRUE, {
  req(input$phenotypesidebarhide == TRUE)
    hideElement(selector = "#plantplotsidebar")
    removeCssClass("plantplotmainpanel", "col-sm-8")
    addCssClass("plantplotmainpanel", "col-sm-12")
})
observeEvent(input$phenotypesidebarhide == FALSE, {
  req(input$phenotypesidebarhide == FALSE)
  showElement(selector = "#plantplotsidebar")
  removeCssClass("plantplotmainpanel", "col-sm-12")
  addCssClass("plantplotmainpanel", "col-sm-8")
})

#will merge with environment data if column req are met
observeEvent(input$phenotypedata, {
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
