###Filtering of dataset -----
#create UI to select tax rank 
output$phyloseq_tax_ranks <- renderUI({
  if(is.null(phyloseqobj()))return(NULL)
  ranks <- list("NULL"="NULL")
  ranks <- c(ranks, as.list(rank_names(amplicondata$original, errorIfNULL=FALSE)))
  ranks <- c(ranks, list(ASV="ASV"))
  return(
    selectInput("filter_rank", "Taxonomic Ranks", 
                ranks, selected = "NULL", multiple = FALSE)
  )
})
#create UI to select specific taxa at specified ranks
output$unique_taxa <- renderUI({
  if(is.null(phyloseqobj()))return(NULL)
  unique <- list("NULL" = "NULL")
  if(!is.null(av(input$filter_rank))){
    if(input$filter_rank == "ASV"){
      unique <- c(unique, as.list(taxa_names(amplicondata$original)))
    } else {
      unique <- c(unique, as.list(get_taxa_unique(amplicondata$original, input$filter_rank)))
    }
  }
  return(
    selectInput(inputId = "filter_taxa", label = "Select Taxa",
                choices = unique, selected = "NULL", multiple = TRUE)
  )
})
#create first UI to filter by experimental design
output$samplefilter <- renderUI({
  if(is.null(phyloseqobj()))return(NULL)
  sampleIDs <-  list("NULL"="NULL")
  sampleIDs <- c(sampleIDs, as.list(sample_variables(amplicondata$original, errorIfNULL=FALSE)))
  output <- tagList(
    selectInput("filter_sample1", "Mapping Variable Category:",
                choices = sampleIDs,
                selected = "NULL",
                multiple = FALSE)
    ,
    selectInput("filter_sample_selection1", "Subcategory:",
                choices = "NULL",
                selected = "NULL",
                multiple = TRUE)
  )
  return(output)
})

observe({
  if(!is.null(av(input$filter_sample1))){
    updateSelectInput(session, 
                      inputId = "filter_sample_selection1",
                      label = "Subcategory",
                      choices = c("NULL", as.list(unique(as(get_variable(amplicondata$original, input$filter_sample1), "character")))),
                      selected = "NULL")
  }
}) 

#Action buttons to create or remove experimental design filters
output$addsamplefilter <- renderUI({
  if(is.null(phyloseqobj()))return(NULL)
  output <- tagList(
    actionButton("addsamplefilter1", "Add Mapping Filter", width = "100%")
    ,
    actionButton("resetsampleselection", "Reset Filters", width = "100%")
  )
})
#dynamically add additional experimental design filters
samplecounter <- reactiveVal(1)
observeEvent(input$addsamplefilter1, {
  samplecounter1 <<- samplecounter() + 1
  samplecounter(samplecounter1)
  sampleIDs <-  list("NULL"="NULL")
  sampleIDs <- c(sampleIDs, as.list(sample_variables(amplicondata$original, errorIfNULL=FALSE)))
  if(samplecounter() <= 5){
    insertUI(
      selector = '#samplecontainer',
      where = "beforeEnd",
      ui = tags$div(id = paste0("addsamplevariableui", paste(samplecounter())),
                    selectInput(paste("filter_sample", paste(samplecounter()), sep = ""), "Mapping Variable Category:",
                                choices = sampleIDs,
                                selected = "NULL")
                    ,
                    selectInput(paste("filter_sample_selection", paste(samplecounter()), sep = ""), "Subcategory:",
                                choices = "NULL",
                                selected = "NULL",
                                multiple = TRUE)
      )
    )
  } else return(NULL)
})
#remove created filter UI
observeEvent(input$resetsampleselection, {
  removeUI(
    selector = '#addsamplevariableui2, #addsamplevariableui3, #addsamplevariableui4, #addsamplevariableui5',
    multiple = TRUE
  )
})
#update options depending on what design class you choose
observe({
  if(!is.null(av(input$filter_sample2))){
    updateSelectInput(session, 
                      inputId = "filter_sample_selection2",
                      label = "Subcategory:",
                      choices = c("NULL", as.list(unique(as(get_variable(amplicondata$original, input$filter_sample2), "character")))),
                      selected = "NULL")
  }
})
observe({
  if(!is.null(av(input$filter_sample3))){
    updateSelectInput(session, 
                      inputId = "filter_sample_selection3",
                      label = "Mapping Variable Category:",
                      choices = c("NULL", as.list(unique(as(get_variable(amplicondata$original, input$filter_sample3), "character")))),
                      selected = "NULL")
  } 
})
observe({
  if(!is.null(av(input$filter_sample4))){
    updateSelectInput(session, 
                      inputId = "filter_sample_selection4",
                      label = "Subcategory:",
                      choices = c("NULL", as.list(unique(as(get_variable(amplicondata$original, input$filter_sample4), "character")))),
                      selected = "NULL")
  } 
})
observe({
  if(!is.null(av(input$filter_sample5))){
    updateSelectInput(session, 
                      inputId = "filter_sample_selection5",
                      label = "Mapping Variable Category:",
                      choices = c("NULL", as.list(unique(as(get_variable(amplicondata$original, input$filter_sample5), "character")))),
                      selected = "NULL")
  } 
})
#reset counter (what keeps track of filter count)
observeEvent(input$resetsampleselection, {
  samplecounter(1)
})
##notifications that you cannot load any more filter options
observe({
  if(samplecounter() >= 6){
    showNotification(ui= "You Have Made the Maximum Number of Filter Selections",
                     duration = 5, 
                     type = "error")
  }
})
#UI options to filter by specific sampleID
output$phyloseq_sample_variables <- renderUI({
  if(is.null(phyloseqobj()))return(NULL)
  sampleIDs <-  list("NULL"="NULL")
  sampleIDs <- c(sampleIDs, as.list(sample_names(amplicondata$original)))
  return(
    selectInput("filter_sample", "Sample Variables", sampleIDs, "NULL", multiple = TRUE)
  )
})
#create UI to filter by min sample presence
output$phyloseqfilteroptions <- renderUI({
  #if(is.null(phyloseqobj()))return(NULL)
  output <- tagList(
    numericInput("phyloseqmincount", "Minimum number of counts per ASV:",
                 value = 0,
                 min = 0,
                 max = 1000)
    ,
    sliderInput("phyloseqminpresence", "Minimum sample presence (in percent):",
                min = 0, max = 100, post  = "%", value = 0)
    ,
    numericInput("phyloseqminpresencenumber", "Minimum sample presence (number of samples):",
                 value = 0,
                 min = 0,
                 max = 300,
                 step = 1)
    ,
    actionButton("phyloseqfilter", "Apply Filters", width = "100%")
  )
  return(output)
})

updatedphyloseq <- eventReactive(input$phyloseqfilter, {
  withProgress(message = "Applying Filters:", {
    obj <- amplicondata$original
    isolate({
      if(inherits(obj, "phyloseq")){
        if(!is.null(av(input$filter_taxa)) ){
          keepTaxa <- NULL
          if(!is.null(tax_table(obj, FALSE))){
            if(input$filter_rank == "ASV"){
              keepTaxa = input$filter_taxa
            } else {
              TT = as(tax_table(obj), "matrix")
              keepTaxa = TT[, input$filter_rank] %in% input$filter_taxa 
            }
            if(length(keepTaxa) > 1){
              obj <- prune_taxa(keepTaxa, obj)
            } else {
              warning("Bad subset_taxa specification. ntaxa(obj) one or fewer OTUs")
            }
          }
        }
        if(!is.null(av(input$filter_sample))){
          keepSamples = NULL
          if(!is.null(sample_data(obj, FALSE))){
            keepSamples = input$filter_sample
            if(length(keepSamples) > 1){
              obj <- prune_samples(keepSamples, obj)
            } else {
              warning("Bad subset_taxa specification. ntaxa(obj) one or fewer OTUs")
            }
          }
        }
        if(!is.null(av(input$filter_sample1))){
          keepSamples2 = NULL
          if(!is.null(sample_data(obj, FALSE))){
            test <- as(sample_data(obj), "data.frame")
            if(!is.null(av(input$filter_sample1)) && samplecounter() >= 1){
              test <- test %>% dplyr::filter(!!as.symbol(input$filter_sample1) %in% input$filter_sample_selection1)
              if(!is.null(av(input$filter_sample2)) && samplecounter() >= 2){
                test <- test %>% dplyr::filter(!!as.symbol(input$filter_sample2) %in% input$filter_sample_selection2)
                if(!is.null(av(input$filter_sample3)) && samplecounter() >= 3){
                  test <- test %>% dplyr::filter(!!as.symbol(input$filter_sample3) %in% input$filter_sample_selection3)  
                  if(!is.null(av(input$filter_sample4)) && samplecounter() >= 4){
                    test <- test %>% dplyr::filter(!!as.symbol(input$filter_sample4) %in% input$filter_sample_selection4)  
                    if(!is.null(av(input$filter_sample5)) && samplecounter() >= 5){
                      test <- test %>% dplyr::filter(!!as.symbol(input$filter_sample5) %in% input$filter_sample_selection5) 
                    }
                  }
                }
              }
            } 
            IDs <- as(rownames(test), "character")
            obj <- prune_samples(IDs, obj)
          }
        }
        if(input$phyloseqmincount > 0){
          if(input$phyloseqminpresence == 0 && input$phyloseqminpresencenumber == 0){
            obj <- prune_taxa({taxa_sums(obj) > input$phyloseqmincount}, obj)
          } else if (input$phyloseqminpresence != 0 && input$phyloseqminpresencenumber == 0){    
            filter <- phyloseq::genefilter_sample(obj, filterfun_sample(function(x) x >= input$phyloseqmincount),
                                                  A = ((input$phyloseqminpresence*0.01)*nsamples(obj)))
            obj <- prune_taxa(filter, obj)
          } else if (input$phyloseqminpresence == 0 && input$phyloseqminpresencenumber != 0){
            filter <- phyloseq::genefilter_sample(obj, filterfun_sample(function(x) x >= input$phyloseqmincount),
                                                  A = input$phyloseqminpresencenumber)
            obj <- prune_taxa(filter, obj)
          } else if(input$phyloseqminpresence != 0 && input$phyloseqminpresencenumber != 0){
            warning("Filtering by percentage and sample count may severly reduce size of dataset")
            filter <- phyloseq::genefilter_sample(obj, filterfun_sample(function(x) x >= input$phyloseqmincount),
                                                  A = ((input$phyloseqminpresence*0.01)*nsamples(obj)))
            obj <- prune_taxa(filter, obj)
            filter1 <- phyloseq::genefilter_sample(obj, filterfun_sample(function(x) x >= input$phyloseqmincount),
                                                   A = input$phyloseqminpresencenumber)
            obj <- prune_taxa(filter1, obj)
          }
        }
        return(obj)
      }else{
        return(NULL)
      }
    })
  })
})
# observeEvent(input$phyloseqfilter, {
#   req(updatedphyloseq())
#   amplicondata$filtered <- updatedphyloseq()
# })
observeEvent(input$phyloseqfilter, {
  if(input$phyloseqminpresence != 0 && input$phyloseqminpresencenumber != 0){
    showNotification(ui= "Filtering by Percentage and Sample Number Can Greatly Decrease Size of Dataset",
                     duration = 5, 
                     type = "warning")
  }
})


#updated OTU table ----
updatedphylootu <- reactive({
  if(is.null(updatedphyloseq()))return(NULL)
  otu_table(amplicondata$filtered)
})
output$updatedphyloseqtable <- renderDataTable({
  if(is.null(updatedphyloseq()))return(NULL)
  otu_table(amplicondata$filtered)
})
output$updatedphyloseqtableoutput <- renderUI({
  validate(
    need(input$makefile, "Please Upload a Dataset"),
    need(input$phyloseqfilter, "No Filters Have Been Applied")
  )
  
    output <- tagList(
      splitLayout(dataTableOutput("updatedphyloseqtable"))
      ,
      downloadTableUI("filteredphylotabledownload")
    )
  
  return(output)
})
downloadTable(id = "filteredphylotabledownload", tableid = updatedphylootu())
#updated taxonomy table -----
updatedtaxtablefile <- reactive({
  if(is.null(updatedphyloseq()))return(NULL)
  tax_table(amplicondata$filtered)
})
output$updatedtaxtable <- renderDataTable({
  if(is.null(updatedphyloseq()))return(NULL)
  tax_table(amplicondata$filtered)
})
output$updatedtaxtableoutput <- renderUI({
  # if(input$phyloseqfilter == 0){
  #   output <- tags$h3("No Filters Applied")
  # }else {
  validate(
    need(input$makefile, "Please Upload a Dataset"),
    need(input$phyloseqfilter, "No Filters Have Been Applied")
  )
    output <- tagList(
      splitLayout(dataTableOutput("updatedtaxtable"))
      ,
      downloadTableUI("filteredtaxtabledownload")
    )
  
  return(output)
})
downloadTable(id = "filteredtaxtabledownload", tableid = updatedtaxtablefile())
#updated sample data table ----
updatedphylosample <- reactive({
  if(is.null(updatedphyloseq()))return(NULL)
  sample_data(amplicondata$filtered)
})
output$updatedmappingtable <- renderDataTable({
  if(is.null(updatedphyloseq()))return(NULL)
  sample_data(amplicondata$filtered)
})
output$updatedmappingtableoutput <- renderUI({
  # if(input$phyloseqfilter == 0){
  #   output <- tags$h3("No Filters Applied")
  # }else {
  validate(
    need(input$makefile, "Please Upload a Dataset"),
    need(input$phyloseqfilter, "No Filters Have Been Applied")
  )
    output <- tagList(
      splitLayout(dataTableOutput("updatedmappingtable"))
      ,
      downloadTableUI("filteredmappingtabledownload")
    )
  #}
  return(output)
})
downloadTable(id= "filteredmappingtabledownload", tableid = updatedphylosample())

#updated tree data -----
updatedtreedf <- reactive({
  if(is.null(updatedphyloseq()))return(NULL)
  tibble::as_tibble(phy_tree(amplicondata$filtered))
})
output$updatedtreetable <- renderDataTable({
  if(is.null(updatedphyloseq()))return(NULL)
  updatedtreedf()
})
output$updatedtreetableoutput <- renderUI({
 # if(input$phyloseqfilter == 0){
 #   output <- tags$h3("No Filters Applied")
 # }else {
  validate(
    need(input$makefile, "Please Upload a Dataset"),
    need(input$phyloseqfilter, "No Filters Have Been Applied")
  )
    if(is.null(phylotree()))
      output <- tags$h3("No Tree Uploaded")
    else {
      output <- tagList(
        splitLayout(dataTableOutput("updatedtreetable"))
        ,
        downloadTableUI("filteredtreetabledownload")
      )
    }
  #}
})
downloadTable(id= "filteredtreetabledownload", tableid = updatedtreedf())



output$sidebarfilteroutputui <- renderUI({
  req(amplicondata$use)
  output <- tagList(
    tags$div(tags$h5("Filter by Taxonomy or ASV:"),
           align = "center"),
  uiOutput("phyloseq_tax_ranks"),
  uiOutput("unique_taxa"),
  hr(),
  tags$div(tags$h5("Filter by Sample ID:"),
           align = "center"),
  uiOutput("phyloseq_sample_variables"),
  hr(),
  tags$div(tags$h5("Filter by Experimental Design:"),
           align = "center"),
  uiOutput("samplefilter"),
  uiOutput("samplecontainer"),
  uiOutput("addsamplefilter"),
  hr(),
  tags$div(tags$h5("Filter by Presence:"),
           align = "center"),
  uiOutput("phyloseqfilteroptions")
  )
  return(output)
})
output$filtertabledownloadsidebar <- renderUI({
  req(amplicondata$use)
  output <- tagList(
    fluidRow(
      column(4,
             wellPanel(
  radioButtons("filtersamplecountplotdownload", "Select Histogram to Download",
               choices = c("Original Sample Count",
                           "Original Taxa Count"),
               inline = TRUE)
  ,
  downloadPlotUI("filterhistogramplots"))
      )
    )
  )
  return(output)
})
observeEvent(input$phyloseqfilter, {
  req(amplicondata$use)
  updateRadioButtons(session = getDefaultReactiveDomain(),
                     inputId = "filtersamplecountplotdownload",
                     label = "Select Histogram to Download",
                     choices = c("Original Sample Count",
                                 "Original Taxa Count",
                                 "Filtered Sample Count",
                                 "Filtered Taxa Count"),
                     inline = TRUE)
})

output$mainpanelfilteroutputui <- renderUI({
  validate(
    need(input$makefile, "Please Upload a Dataset")#,
    #need(input$phyloseqfilter, "No Filters Have Been Applied")
  )
  
  req(amplicondata$use)
  output <- tagList(
    tags$h3("Original Data Summary"),
  verbatimTextOutput("originalphyloseqsummary"),
  splitLayout(
    plotOutput("originalsamplecounthist"),
    plotOutput("originaltaxacounthist")
  ),
  wellPanel(
  splitLayout(
    uiOutput("counthistsummaryorigsample"),
    uiOutput("counthisttaxa")
    )
  )
  #,
  #splitLayout(
  #  downloadPlotUI("originalsamplecounthistplot"),
  #  downloadPlotUI("originaltaxacounthistplot")
  #)
  ,
  hr()
  ,
  tags$h3("Filtered Data Summary"),
  )
  return(output)
})

selectedPlot <- reactive({
  req(amplicondata$use)
  #if(is.null(updatedphyloseq())){
 # switch(input$filtersamplecountplotdownload,
  #       "Original Sample Count" = originalsamplehistplot(),
  #       "Original Taxa Count" = originaltaxahistplot())
  #}else {
    switch(input$filtersamplecountplotdownload,
           "Original Sample Count" = originalsamplehistplot(),
           "Original Taxa Count" = originaltaxahistplot(),
           "Filtered Sample Count" = updatedsamplehistplot(),
           "Filtered Taxa Count" = updatedtaxahistplot())
  #}
})
downloadPlot("filterhistogramplots", selectedPlot())

output$mainpanelfilteroutputui2 <- renderUI({
  validate(
    #need(input$makefile, "Please Upload a Dataset"),
    need(input$phyloseqfilter, "No Filters Have Been Applied")
  )
  
  req(amplicondata$use)
  output <- tagList(
    verbatimTextOutput("updatedphyloseqsummary"),
    splitLayout(
      plotOutput("updatedsamplecounthist"),
      plotOutput("updatedtaxacounthist")
    ),
    wellPanel(
    splitLayout(
      uiOutput("counthistsummaryorigsampleupdated"),
      uiOutput("counthisttaxaupdated")
      )
    )#,
    #splitLayout(
    #  downloadPlotUI("updatedsamplecounthistplot"),
    #  downloadPlotUI("updatedtaxacounthistplot")
    #)
  )
  return(output)
})




