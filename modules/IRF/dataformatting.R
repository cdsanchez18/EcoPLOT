
##update dataset options after a filter is applied
observeEvent(input$phyloseqfilter, {
  if(is.null(phyloseqobj()))return(NULL)
  updateRadioButtons(session, "irfDatasetselect", "Select Dataset to Use:",
                     choices = c("Original",
                                 "Filtered"),
                     selected = "Original", inline = TRUE)
}, once = TRUE)
#update selected dataset to reflect changes made in other tabs
observe({
  if(is.null(updatedphyloseq()))return(NULL)
  if(input$amplicondatasource == "Original"){
    updateRadioButtons(session, "irfDatasetselect", "Select Dataset to Use:",
                       choices = c("Original",
                                   "Filtered"),
                       selected = "Original", inline = TRUE)
  }else if(input$amplicondatasource == "Filtered"){
    updateRadioButtons(session, "irfDatasetselect", "Select Dataset to Use:",
                       choices = c("Original",
                                   "Filtered"),
                       selected = "Filtered", inline = TRUE)
  }
})

##dataset is amplicon use
output$irfUIoptions <- renderUI({
  if(is.null(phyloseqobj()))return(NULL)
  output <- tagList(
    conditionalPanel(condition = "input.IRF == 2",
    actionButton("makeIRFdataset", "Prepare Data for IRF", width = "100%")
    )
    ,
    #hr()
    #,
    conditionalPanel(condition = "input.IRF == 3",
                     numericInput("IRFsetseed", "Set Seed (For Repeatability)",
                                  value = 100, min = 1)
                     ,
                     sliderInput("IRFpercentage", "Select Percentage of Data to Include in Training Dataset
                                 (Remaining will go to Test Dataset)", min = 60, max = 99, step = 1, post = "%",
                                 value = 80)
                     ,
                     selectInput("IRFyvar", "Select Output Variable",
                                 choices = c("NULL", sample_variables(phyloseqobj())),
                                 selected = "NULL")
                     ,
                     selectInput("IRFexclude", "Select Variables to Exclude From Analysis",
                                 choices = c(sample_variables(phyloseqobj())),
                                 multiple = TRUE)
                     ,
                     actionButton("parseIRF", "Parse Datasets for IRF", width = "100%"))
    ,
    #conditionalPanel(condition = "input.IRF == 4", 
                     #hr(),
     #                actionButton("encodeIRF", "Encode Column Variables for IRF", width = "100%"))
    #,
    conditionalPanel(condition = "input.IRF == 4",
                     #hr()
                     #,
                     tags$h5("IRF Parameters")
                     ,
                     radioButtons("IRFdefault", "Use Default Parameters", 
                                  choices = c("Yes", "No"),
                                  selected = "Yes", inline = TRUE)
                     ,
                     conditionalPanel(condition = "input.IRFdefault == 'No'",
                                      fluidRow(
                                        column(6, numericInput("IRFdepth", "Select Depth", value = 5, min = 1, max = 10, step = 1)),
                                        column(6, numericInput("IRFnchild", "Select nchild", value = 2, min = 1, max = 10, step = 1))),
                                      fluidRow(
                                        column(6, numericInput("IRFntree", "Select ntree", value = 100, min = 10, max = 300, step = 1)),
                                        #column(6, numericInput("IRFniter", "Select n.iter", value = 5, min = 1, max = 10, step = 1))),
                                        column(12, numericInput("IRFnbootstrap", "Select nbootstrap", value = 30, min = 1, max = 50, step = 1)))
                                      #fluidRow(
                                      #  column(12, numericInput("IRFnbootstrap", "Select nbootstrap", value = 30, min = 1, max = 50, step = 1)))
                     )
                     ,
                     checkboxInput("IRFinteractions", "Should iRF look for interactions?", value = FALSE, width = "100%")
                     ,
                     tags$h6(tags$b("NOTE:"), "Finding interactions is computationally intensive. 
                             Run times may vary based on the size of your data. For more information and system requirements, please see the guide at the beginning of the module.")
                     ,
                     actionButton("performIRF", "Run IRF", width = "100%")
                     )
  )
    
  
  return(output)
})


IRFdataset <- eventReactive(input$makeIRFdataset, {
  if(is.null(phyloseqobj()))return(NULL)
  withProgress(message = "Preparing Dataset", {
  phylomelt <- psmelt(ampliconuse()) %>% tidyr::fill(c(Kingdom, Phylum, Class, Order, Family, Genus))#%>% na.omit()
  
  phylomelt <- phylomelt %>% 
    mutate(taxonomy = paste(phylomelt$Kingdom, phylomelt$Phylum, phylomelt$Class, phylomelt$Order, phylomelt$Family,
                             phylomelt$Genus, #phylomelt$Species, 
                            phylomelt$OTU, sep = ";__")) %>%
    select(-c(Kingdom, Phylum, Class, Order, Family, Genus)) #%>%
  
  phylomelt <- phylomelt %>% tidyr::pivot_wider(id_cols = Sample,
                                                names_from = c(taxonomy),
                                                values_from = Abundance,
                                                values_fill = 0)
  mapping <- data.frame(sample_data(ampliconuse()))
  #mapping$Sample <- row.names(mapping)
  
  phylomelt <- left_join(data.frame(sample_data(ampliconuse())), phylomelt)
   #return(phylomelt)
  })
  return(phylomelt)
})

output$IRFdatasetoutput <- DT::renderDT({
  if(is.null(IRFdataset()))return(NULL)
  IRFdataset() %>% select(1:50)
}, server = TRUE, options = list(searching = FALSE, pageLength = 50))
output$IRFdatasetoutputUI <- renderUI({
  withProgress(message = "Preparing dataset", {
    splitLayout(
      DT::DTOutput("IRFdatasetoutput"))
  })
})


###INDEX WITH SPECIFIED PERCENTAGES AND PARSE TESTING AND TRAINING DATASETS
train_index1 <- eventReactive(input$parseIRF, {
  if(is.null(phyloseqobj()))return(NULL)
 sample(1:nrow(IRFdataset()), (0.01*input$IRFpercentage)*nrow(IRFdataset()))
})
test_index1 <- eventReactive(input$parseIRF, {
  if(is.null(phyloseqobj()))return(NULL)
  req(train_index1())
  BiocGenerics::setdiff(1:nrow(IRFdataset()), train_index1())
})
Xtrain <- eventReactive(input$parseIRF, {
  if(is.null(train_index1()))return(NULL)
  if(!is.null(av(input$IRFyvar))){
    data <- IRFdataset()[train_index1(), ] %>% select(-c(input$IRFyvar, input$IRFexclude, "Sample"))
    data[sapply(data, is.character)] <- lapply(data[sapply(data, is.character)], 
                                                     as.factor)
    data
  }
})
Ytrain <- eventReactive(input$parseIRF, {
  if(is.null(train_index1()))return(NULL)
  if(!is.null(av(input$IRFyvar))){
  IRFdataset()[train_index1(), ] %>% select(input$IRFyvar)
  #data[sapply(data, is.character)] <- lapply(data[sapply(data, is.character)], 
  #                                           as.factor)
  
  }
})
Xtest <- eventReactive(input$parseIRF, {
  if(is.null(test_index1()))return(NULL)
  if(!is.null(av(input$IRFyvar))){
    data <- IRFdataset()[test_index1(),] %>% select(-c(input$IRFyvar, input$IRFexclude, "Sample"))
    data[sapply(data, is.character)] <- lapply(data[sapply(data, is.character)], 
                                               as.factor)
    data
  }
})
Ytest <- eventReactive(input$parseIRF, {
  if(is.null(test_index1()))return(NULL)
  if(!is.null(av(input$IRFyvar))){
  IRFdataset()[test_index1(), ] %>% select(input$IRFyvar)
  #data[sapply(data, is.character)] <- lapply(data[sapply(data, is.character)], 
  #                                           as.factor)
  #data
  }
})
observeEvent(input$parseIRF, {
  if(is.null(av(input$IRFyvar))){
    showNotification("Please Select A Y Variable", type = "warning")
  }
})
output$iRFdatasets <- renderUI({
  if(is.null(test_index1()))return(NULL)
  output <- tagList(
    fluidRow(
      splitLayout(
      column(6,
             tags$u(tags$h5("X Train Dataset")),
             dataTableOutput("testoutput")),
      column(6, 
             tags$u(tags$h5("Y Train Dataset")),
             dataTableOutput("testoutput1")))),
    hr(),
    fluidRow(
      splitLayout(
      column(6,
             tags$u(tags$h5("X Test Dataset")),
             dataTableOutput("testoutput2")),
      column(6,
             tags$u(tags$h5("Y Test Dataset")),
             dataTableOutput("testoutput3"))))
  )
  return(output)
})

######ENCODING, TURNS FACTOR VARIABLES INTO NUMERIC COLUMNS (REQUIREMENT)
# IRFencoding <- eventReactive(input$encodeIRF, {
#   if(is.null(test_index1()))return(NULL)
#   if(!is.null(av(input$IRFyvar))){
#     req(Xtrain())
#     withProgress(message = "Encoding Variables", {
#     dataPreparation::build_encoding(data_set = Xtrain(), cols = "auto", verbose = TRUE)
#     })
#   }else {
#     NULL
#   }
# })
output$testtest <- renderPrint({
  str(Xtrain()[, sapply(Xtrain(), is.factor)])
})
output$testtest1 <- renderPrint({
  str(Ytrain()[, sapply(Ytrain(), is.factor)])
})
output$testtest2 <- renderPrint({
  if(is.null(av(input$IRFyvar)))return(NULL)
  class(IRFdataset()[train_index1(), input$IRFyvar])
  #ncol(Xtrain())
})
# Xtrainencoded <- eventReactive(input$encodeIRF,{
#   if(is.null(phyloseqobj()))return(NULL)
#   #if(!is.null(IRFencoding())){
#     req(Xtrain())
#     withProgress("Encoding Training Data", {
#       # dataPreparation::one_hot_encoder(data_set = Xtrain(), 
#       #                          encoding = IRFencoding(), 
#       #                          drop = TRUE, verbose = TRUE)
#       mltools::one_hot(as.numeric(Xtrain()))
#     })
#   #}
# })
# Xtestencoded <- eventReactive(input$encodeIRF,{
#   if(is.null(phyloseqobj()))return(NULL)
#   if(!is.null(IRFencoding())){
#     req(Xtrain())
#     withProgress("Encoding Test Data", {
#       dataPreparation::one_hot_encoder(data_set = as.data.frame(Xtest()), 
#                                encoding = as.list(IRFencoding()), 
#                                drop = TRUE, verbose = TRUE)
#     })
#   }
# })

###RUN IRF ANALYSIS
IRFmodel <- eventReactive(input$performIRF, {
  if(is.null(Xtrain()))return(NULL)
  #withProgress("Performing IRF", {
   
  # if(input$IRFdefault == "Yes"){
  #   rit.param <- list(depth=5, nchild=2, ntree=100, class.id=1, class.cut=NULL)
  #   model <- iRF(x = data.matrix(Xtrain()),
  #                y = data.matrix(Ytrain()),
  #                xtest = data.matrix(Xtest()),
  #                ytest = data.matrix(Ytest()),
  #                n.iter = 1,   # Number of iterations
  #                n.core = 1,    # Use 2 cores for parallel traininge
  #                interactions.return = c(1),
  #                # Return the iteration with highest OOB accuracy
  #                select.iter = TRUE,
  #                # Number of bootstrap samples to calculate stability scores
  #                n.bootstrap = 30,
  #                # Use ranger as the underlying random forest package
  #                type = 'ranger',
  #                # Parameters for RIT
  #                rit.param= rit.param
  #   )
  # }else {
    #rit.param <- list(depth= input$IRFdepth, nchild= input$IRFnchild, ntree= input$IRFntree, class.id=1, class.cut=NULL)
    rit.param <- list(depth=5, nchild=2, ntree=100, class.id=1, class.cut=NULL)
    registerDoParallel(cores = 2)
    model <- iRF(x = data.matrix(Xtrain()),
                 y = as.factor(IRFdataset()[train_index1(), input$IRFyvar]),#%>% select(input$IRFyvar),
                 xtest = data.matrix(Xtest()),
                 ytest = as.factor(IRFdataset()[test_index1(), input$IRFyvar]),
                 n.iter = 1,   # Number of iterations
                 n.core = 1,    # Use 2 cores for parallel traininge
                 #interactions.return = c(input$IRFniter),
                 # Return the iteration with highest OOB accuracy
                 select.iter = TRUE,
                 # Number of bootstrap samples to calculate stability scores
                 n.bootstrap = input$IRFnbootstrap,
                 # Use ranger as the underlying random forest package
                 type = 'ranger',
                 # Parameters for RIT
                 rit.param= rit.param,
                 bootstrap.forest = FALSE
    )
 # }
  #})
  #return(model)
  model
})
output$IRFoutput <- renderPrint({
  if(is.null(IRFmodel()))return(NULL)
  IRFmodel()
})


###Datatable outputs
output$testoutput <- renderDataTable({
  if(is.null(train_index1()))return(NULL)
  withProgress(message = "Making Test and Training Datasets", {
  Xtrain()%>% select(1:50)
  })
})
output$testoutput1 <- renderDataTable({
  if(is.null(train_index1()))return(NULL)
  Ytrain()
})
output$testoutput2 <- renderDataTable({
  if(is.null(test_index1()))return(NULL)
  Xtest() %>% select(1:50)
})
output$testoutput3 <- renderDataTable({
  if(is.null(test_index1()))return(NULL)
  Ytest()
})
output$Xtrainencodedoutput <- renderDataTable({
  if(is.null(Xtrainencoded()))return(NULL)
  withProgress(message = "Encoding Data", {
  Xtrainencoded() #%>% select(1:50)
  })
})
output$Xtestencodedoutput <- renderDataTable({
  if(is.null(Xtrainencoded()))return(NULL)
  Xtestencoded() #%>% select(1:50)
})
