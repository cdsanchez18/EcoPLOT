
##update dataset options after a filter is applied
observeEvent(input$phyloseqfilter, {
  if(is.null(phyloseqobj()))return(NULL)
  updateRadioButtons(session, "irfDatasetselect", "Select Dataset to Use:",
                     choices = c("Original",
                                 "Filtered"),
                     selected = "Original", inline = TRUE)
}, once = TRUE)

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
                     #numericInput("IRFsetseed", "Set Seed (For Repeatability)",
                     #             value = 100, min = 1)
                     #,
                     sliderInput("IRFpercentage", "Select Percentage of Data to Include in Training Dataset
                                 (Remaining will go to Test Dataset)", min = 60, max = 99, step = 1, post = "%",
                                 value = 80)
                     ,
                     selectInput("IRFyvar", "Select Output Variable",
                                 choices = c("NULL", sample_variables(amplicondata$use)),
                                 selected = "NULL")
                     ,
                     selectInput("IRFexclude", "Select Variables to Exclude From Analysis",
                                 choices = c(sample_variables(amplicondata$use)),
                                 multiple = TRUE)
                     ,
                     actionButton("parseIRF", "Parse Datasets for IRF", width = "100%")
                     ,
                     uiOutput("IRFtabledownloadUI"))
    ,
    #conditionalPanel(condition = "input.IRF == 4", 
                     #hr(),
     #                actionButton("encodeIRF", "Encode Column Variables for IRF", width = "100%"))
    #,
    conditionalPanel(condition = "input.IRF == 4",
                     #hr()
                     #,
                     tags$h4("Set iRF Parameters")
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
                                        column(6, numericInput("IRFnbootstrap", "Select nbootstrap", value = 30, min = 1, max = 50, step = 1)))
                                      #fluidRow(
                                      #  column(12, numericInput("IRFnbootstrap", "Select nbootstrap", value = 30, min = 1, max = 50, step = 1)))
                     )
                     ,
                     checkboxInput("IRFinteractions", "Should iRF look for interactions?", value = FALSE, width = "100%")
                     ,
                     tags$h6(tags$b("NOTE:"), "Finding interactions is computationally intensive. 
                             Run times may vary based on the size of your data. For more information and system requirements, please see the guide at the beginning of the module.")
                     ,
                     numericInput("IRFiterations", "How Many Iterations of iRF Should be Performed?", value = 1, min = 1,max = 10,step = 1,width = "100%")
                     ,
                     actionButton("performIRF", "Run IRF", width = "100%")
                     )
    ,
    conditionalPanel(condition = "input.IRF == 5",
                     uiOutput("irfplotui")
                     #tags$h3("Variable Importance Plot"),
                     #numericInput("IRFvarimpplotsize", "Select Size of Plot:", value = 800, min = 100,width = "100%"),
                     #numericInput("IRFvarimpninteractions", "How Many Variables Should be Shown?", value = 30, min = 1, max = 50, step = 1),
                     #downloadPlotUI("IRFvarimpdownload")
                     )
  )
    
  
  return(output)
})


IRFdataset <- eventReactive(input$makeIRFdataset, {
  if(is.null(phyloseqobj()))return(NULL)
  withProgress(message = "Preparing Dataset", {
  phylomelt <- psmelt(amplicondata$use) %>% tidyr::fill(c(Kingdom, Phylum, Class, Order, Family, Genus))#%>% na.omit()
  
  phylomelt <- phylomelt %>% 
    mutate(taxonomy = paste(phylomelt$Kingdom, phylomelt$Phylum, phylomelt$Class, phylomelt$Order, phylomelt$Family,
                             phylomelt$Genus, #phylomelt$Species, 
                            phylomelt$OTU, sep = ";__")) %>%
    select(-c(Kingdom, Phylum, Class, Order, Family, Genus)) #%>%
  
  phylomelt <- phylomelt %>% tidyr::pivot_wider(id_cols = Sample,
                                                names_from = c(taxonomy),
                                                values_from = Abundance,
                                                values_fill = 0)
  mapping <- data.frame(sample_data(amplicondata$use))
  #mapping$Sample <- row.names(mapping)
  
  phylomelt <- left_join(data.frame(sample_data(amplicondata$use)), phylomelt)
  phylomelt[sapply(phylomelt, is.character)] <- lapply(phylomelt[sapply(phylomelt, is.character)], 
                                             as.factor)
   #return(phylomelt)
  })
  return(phylomelt)
})

output$IRFdatasetoutput <- DT::renderDT({
  if(is.null(IRFdataset()))return(NULL)
  IRFdataset() %>% select(1:50)
}, server = TRUE, options = list(searching = FALSE, pageLength = 50))

output$IRFdatasetoutputUI <- renderUI({
  validate(
    need(input$makefile, "Please Upload an Amplicon Dataset"),
    need(input$makeIRFdataset, "Table Will Appear Here.")
  )
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
    data <- IRFdataset()[train_index1(), ] %>% select(-c(input$IRFyvar, input$IRFexclude, "Sample", "Row_ID"))
    data[sapply(data, is.character)] <- lapply(data[sapply(data, is.character)], 
                                                     as.factor)
    data
  }
})
Ytrain <- eventReactive(input$parseIRF, {
  if(is.null(train_index1()))return(NULL)
  if(!is.null(av(input$IRFyvar))){
  IRFdataset()[train_index1(), ] %>% select(input$IRFyvar)
  }
})
Xtest <- eventReactive(input$parseIRF, {
  if(is.null(test_index1()))return(NULL)
  if(!is.null(av(input$IRFyvar))){
    data <- IRFdataset()[test_index1(),] %>% select(-c(input$IRFyvar, input$IRFexclude, "Sample", "Row_ID"))
    data[sapply(data, is.character)] <- lapply(data[sapply(data, is.character)], 
                                               as.factor)
    data
  }
})
Ytest <- eventReactive(input$parseIRF, {
  if(is.null(test_index1()))return(NULL)
  if(!is.null(av(input$IRFyvar))){
  IRFdataset()[test_index1(), ] %>% select(input$IRFyvar)
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
      column(3,
             hr()),
      column(6,
             wellPanel(tags$div(
               tags$h3("View Tables")), align = "center")),
      column(3,
             hr())
    ),
    fluidRow(
      splitLayout(
      column(6,
             tags$u(tags$h4("X Train Dataset")),
             dataTableOutput("testoutput")),
      column(6, 
             tags$u(tags$h4("Y Train Dataset")),
             dataTableOutput("testoutput1")))),
    hr(),
    fluidRow(
      splitLayout(
      column(6,
             tags$u(tags$h4("X Test Dataset")),
             dataTableOutput("testoutput2")),
      column(6,
             tags$u(tags$h4("Y Test Dataset")),
             dataTableOutput("testoutput3"))))
  )
  return(output)
})

output$testtest <- renderPrint({
  str(IRFdataset() %>% select(colnames(data.frame(sample_data(amplicondata$use)) %>% select(-c(input$IRFexclude)))))
})
output$testtest1 <- renderUI({
  validate(
    need(input$makefile, "Please Upload an Amplicon Dataset"),
         need(input$makeIRFdataset, "You Must First Complete Step 1")
  )
  output <- tagList(
    wellPanel(tags$div(tags$h4("The following lists your sample variables and their associated classes in R. iRF treats factor variables
                     differently than it does numeric or integer variables. A", tags$b("classification model"), "will be used for categorical variables
                               and a", tags$b("regression model"), "will be used for continuous variables.", tags$br(), "Run times may vary based on the selected variable's class."),
             align = "center")),
    verbatimTextOutput("testtest")
  )
  return(output)
})
output$testtest2 <- renderPrint({
  if(is.null(Xtrain()))return(NULL)
  !!as.symbol(input$partdepx)
})

observeEvent(input$performIRF, {
  if(is.null(Xtrain()))return(NULL)
  showNotification("Running iRF, do not click 'Run iRF' again.", type = "warning", duration = NULL)
})

###RUN IRF ANALYSIS
IRFmodel <- eventReactive(input$performIRF, {
  if(is.null(Xtrain()))return(NULL)
  #set.seed(input$IRFsetseed)
  rit.param <- list(depth= input$IRFdepth, nchild= input$IRFnchild, ntree= input$IRFntree, class.id=1, class.cut=NULL)
  if(is.character(IRFdataset()[train_index1(), input$IRFyvar]) || is.factor(IRFdataset()[train_index1(), input$IRFyvar])){
    if(input$IRFinteractions == TRUE){
      #doParallel::registerDoParallel(cores = 2)
      model <- iRF(x = data.matrix(Xtrain()),
                   y = as.factor(IRFdataset()[train_index1(), input$IRFyvar]),#%>% select(input$IRFyvar),
                   xtest = data.matrix(Xtest()),
                   ytest = as.factor(IRFdataset()[test_index1(), input$IRFyvar]),
                   n.iter = input$IRFiterations,   # Number of iterations
                   n.core = 1,    # Use 2 cores for parallel traininge
                   int.return = as.numeric(1:input$IRFiterations),
                   # Return the iteration with highest OOB accuracy
                   select.iter = TRUE,
                   # Number of bootstrap samples to calculate stability scores
                   n.bootstrap = input$IRFnbootstrap,
                   # Use ranger as the underlying random forest package
                   type = "randomForest",#'ranger',
                   # Parameters for RIT
                   rit.param= rit.param#,
                   #bootstrap.forest = FALSE
      )
    }else{
      #doParallel::registerDoParallel(cores = 2)
      model <- iRF(x = data.matrix(Xtrain()),
                   y = as.factor(IRFdataset()[train_index1(), input$IRFyvar]),#%>% select(input$IRFyvar),
                   xtest = data.matrix(Xtest()),
                   ytest = as.factor(IRFdataset()[test_index1(), input$IRFyvar]),
                   n.iter = input$IRFiterations,   # Number of iterations
                   n.core = 1,    # Use 2 cores for parallel traininge
                   # Return the iteration with highest OOB accuracy
                   select.iter = TRUE,
                   # Number of bootstrap samples to calculate stability scores
                   n.bootstrap = input$IRFnbootstrap,
                   # Use ranger as the underlying random forest package
                   type = "randomForest",#'ranger',
                   # Parameters for RIT
                   rit.param= rit.param#,
                   #bootstrap.forest = FALSE
      )
    }
  }else if(is.numeric(IRFdataset()[train_index1(), input$IRFyvar]) || is.integer(IRFdataset()[train_index1(), input$IRFyvar])){
    if(input$IRFinteractions == TRUE){
      #doParallel::registerDoParallel(cores = 2)
      model <- iRF(x = data.matrix(Xtrain()),
                   y = IRFdataset()[train_index1(), input$IRFyvar],#Ytrain(),#%>% select(input$IRFyvar),
                   xtest = data.matrix(Xtest()),
                   ytest = IRFdataset()[test_index1(), input$IRFyvar],
                   n.iter = input$IRFiterations,   # Number of iterations
                   n.core = 1,    # Use 2 cores for parallel traininge
                   int.return = as.numeric(1:input$IRFiterations),
                   # Return the iteration with highest OOB accuracy
                   select.iter = TRUE,
                   # Number of bootstrap samples to calculate stability scores
                   n.bootstrap = input$IRFnbootstrap,
                   # Use ranger as the underlying random forest package
                   type = "randomForest",#'ranger',
                   # Parameters for RIT
                   rit.param= rit.param#,
                   #bootstrap.forest = FALSE
      )
    }else{
      #doParallel::registerDoParallel(cores = 2)
      model <- iRF(x = data.matrix(Xtrain()),
                   y = IRFdataset()[train_index1(), input$IRFyvar],#%>% select(input$IRFyvar),
                   xtest = data.matrix(Xtest()),
                   ytest = IRFdataset()[test_index1(), input$IRFyvar],
                   n.iter = input$IRFiterations,   # Number of iterations
                   n.core = 1,    # Use 2 cores for parallel traininge
                   # Return the iteration with highest OOB accuracy
                   select.iter = TRUE,
                   # Number of bootstrap samples to calculate stability scores
                   n.bootstrap = input$IRFnbootstrap,
                   # Use ranger as the underlying random forest package
                   type = "randomForest",#'ranger',
                   # Parameters for RIT
                   rit.param= rit.param#,
                   #bootstrap.forest = FALSE
      )
    }
  }
  model
})
output$IRFoutput <- renderPrint({
  if(is.null(IRFmodel()))return(NULL)
  IRFmodel()
})
output$IRFoutputui<- renderUI({
  validate(
    need(input$makefile, "Please Upload an Amplicon Dataset"),
    need(input$makeIRFdataset, "You Must First Complete Step 1"),
    need(input$parseIRF, "You Must First Complete Step 2")
  )
  if(is.null(IRFmodel()))return(NULL)
  output <- tagList(
    hr(),
    tags$h3("View iRF Results"),
    verbatimTextOutput("IRFoutput")
  )
  return(output)
})
importanceplot <- reactive({
  if(is.null(IRFmodel()))return(NULL)
  iRF::varImpPlot(IRFmodel()$rf.list,#[[input$rfselected]], 
                  main = "Variable Importance Plot", n.var = input$IRFvarimpninteractions)
})
output$IRFvarimp <- renderPlot({
  if(is.null(IRFmodel()))return(NULL)
  importanceplot()
})
output$IRFplotui <- renderUI({
  validate(
    need(input$makefile, "Please Upload an Amplicon Dataset"),
    need(input$makeIRFdataset, "You Must First Complete Step 1"),
    need(input$parseIRF, "You Must First Complete Step 2"),
    need(input$performIRF, "You Must First Complete Step 3")
  )
  if(is.null(IRFmodel()))return(NULL)
  output <- tagList(
    tags$div(tags$h3("Variable Importance Plots show how each variable affects the accuracy of the model. Those variables that exhibit a larger effect are given a
                     larger importance measurement. Variables are listed in descending order of importance."), align = "center")
    ,
    plotOutput("IRFvarimp", height = input$IRFvarimpplotsize)
    )
  return(output)
})
downloadPlot("IRFvarimpdownload", importanceplot())

interactionplot <- reactive({
  if(is.null(IRFmodel()))return(NULL)
  if(input$IRFinteractions == TRUE){
    if(!is.null(av(input$IRFintx))){
    ggplot(IRFmodel()$interaction[1:input$IRFinteractionnuminteractions, ], aes(y = int, x = !!as.symbol(input$IRFintx))) + 
        geom_point(shape = 1) + theme_bw() + 
      labs(title = "Variable Interaction Plot") + theme(axis.text.y = element_text(size = input$IRFinteractionplotyaxissize),
                                                        axis.title.y = element_blank(),
                                                        plot.title = element_text(face = "bold", size = rel(1), hjust = 0.5))
    }else {
      NULL
    }
  }else{
    NULL
  }
})
output$interactionplotrender <- renderPlot({
  if(is.null(IRFmodel()))return(NULL)
  interactionplot()
})
downloadPlot("IRFinteractionplotdownload", interactionplot())
output$irfinteractionoutput <- renderUI({
  if(input$IRFinteractions == TRUE){
    output <- tagList(
      wellPanel(
      tags$h3("Variable Interaction Plot"),
      selectInput("IRFintx", "Select Variable to View on X Axis",
                  choices = c("NULL", as.list(colnames(IRFmodel()$interaction))),
                  selected = "NULL"),
      numericInput("IRFinteractionplotheight", "Select Plot Height:", value = 700, min = 100),
      numericInput("IRFinteractionnuminteractions", "Select Number of Interactions to Show:", value = 10, min = 1, max = 20, step = 1),
      numericInput("IRFinteractionplotyaxissize", "Select Size of Y Axis Text:", value = 8, min = 1, max = 20, step = 1),
      downloadPlotUI("IRFinteractionplotdownload"))
    )
  }else{
    output <- NULL
  }
  return(output)
})
output$irfinteractionoutput2 <- renderUI({
  if(is.null(IRFmodel()))return(NULL)
  if(input$IRFinteractions == TRUE){
    output <- tagList(
      fluidRow(
        column(4, 
               uiOutput("irfinteractionoutput")),
        column(8,
               plotOutput("interactionplotrender", height = input$IRFinteractionplotheight))
      )
    )
    return(output)
  }
})
output$partdepplot1 <- renderUI({
  if(is.null(IRFmodel()))return(NULL)
  output <- tagList(
    hr(),
    fluidRow(
    column(4,
    wellPanel(tags$h3("Partial Dependence Plot")
              ,
    selectInput("partdepxvar", "Select Variable to Observe Partial Dependence",
                choices = c("NULL", sample_variables(amplicondata$use)),
                selected = "NULL")
    ,
    if(is.character(IRFdataset()[train_index1(), input$IRFyvar]) || is.factor(IRFdataset()[train_index1(), input$IRFyvar])){
    selectInput("partdepclass", "Select Class of Output to Focus on:",
                choices = c("NULL", as.list(levels(as.factor(sample_data(amplicondata$use)[[input$IRFyvar]])))),
                selected = "NULL")
  }else{
    NULL
  }
  ,
  downloadPlotUI("partialdepplot"))),
  column(8,
         uiOutput("partialdependenceplotoutput"))
    )
  )
  return(output)
})
output$testprintpartial <- renderPrint({
  if(is.null(IRFmodel()))return(NULL)
  names(Xtrain()[input$partdepxvar])
})
partialdependenceplot <- reactive({
  if(is.null(IRFmodel()))return(NULL)
  if(!is.null(av(input$partdepxvar))){
    if(is.character(IRFdataset()[train_index1(), input$IRFyvar]) || is.factor(IRFdataset()[train_index1(), input$IRFyvar])){
      if(!is.null(av(input$partdepclass))){
        #BiocGenerics::
        plot <- do.call("partialPlot", 
                list(x=IRFmodel()$rf.list,#[[input$rfselected]], 
                     pred.data=data.matrix(Xtrain()), 
                     x.var = input$partdepxvar, which.class = input$partdepclass,
                     ylab = paste("Marginal Effect of", input$partdepxvar),
                     main = paste("Partial Dependence on", input$partdepxvar, "on", input$IRFyvar, input$partdepclass)))
       plot
       }else{
        NULL
      }
    }else if(is.integer(IRFdataset()[train_index1(), input$IRFyvar]) || is.numeric(IRFdataset()[train_index1(), input$IRFyvar])){
      plot <- do.call("partialPlot", 
                      list(x=IRFmodel()$rf.list,#[[input$rfselected]], 
                           pred.data=data.matrix(Xtrain()), 
                           x.var = input$partdepxvar,
                           ylab = paste("Marginal Effect of", input$partdepxvar),
                           main = paste("Partial Dependence of", input$partdepxvar, "on", input$IRFyvar)))
      plot
    }
  }else {
    plot <- NULL
    plot
  }
  plot
})
output$partialdependenceplot1 <- renderPlot({
  if(is.null(IRFmodel()))return(NULL)
  partialdependenceplot()
})
output$partialdependenceplot2 <- renderPrint({
  if(is.null(IRFmodel()))return(NULL)
  if(!is.null(av(input$partdepxvar))){
      str(IRFdataset() %>% select(colnames(data.frame(sample_data(amplicondata$use)) %>% select(c(input$partdepxvar)))))
  }else {
    NULL
  }
})

output$partialdependenceplotoutput <- renderUI({
  if(is.null(IRFmodel()))return(NULL)
  output <- tagList(
    tags$div(tags$h4("Partial Dependence Plots allow you to observe the marginal effect of each independent variable on the machine learning model's
                     ability to predict the selected output variable value or class. This plot can be used along with the Variable Importance Plot above 
                     to see the relationship between the independent variable and output variable."), align = "center")
    ,
    plotOutput("partialdependenceplot1")
    ,
    verbatimTextOutput("partialdependenceplot2")
    ,
    wellPanel(tags$div(tags$h4("If you have selected a categorical variable to view on the x-axis, you will notice 
                      that the variable names have been converted to numeric identifiers. The output above prints the internal numeric structure assigned 
                               to each level of a factor variable and should be used to guide your analysis.")), align = "center")
    )
  return(output)
})
downloadPlot("partialdepplot", partialdependenceplot())
varimportancetable <- reactive({
  if(is.null(IRFmodel()))return(NULL)
  data.frame(IRFmodel()$rf.list$importance)
})
output$varimportancetable1 <- renderDataTable({
  if(is.null(IRFmodel()))return(NULL)
  varimportancetable()
})
downloadTable("downloadvarimptable", varimportancetable())
varinteractiontable <- reactive({
  if(is.null(IRFmodel()$interaction))return(NULL)
  data.frame(IRFmodel()$interaction)
})
output$varinteractiontable1 <- renderDataTable({
  if(is.null(IRFmodel()$interaction))return(NULL)
  varinteractiontable()
})
output$varinteractiontable2 <- renderUI({
  if(is.null(IRFmodel()$interaction))return(NULL)
  output <- tagList(
    fluidRow(
      column(3,
             hr()),
      column(6,
             wellPanel(tags$div(tags$b(tags$h4("View Discovered Variable Interactions")), align = "center"))),
      column(3,
             hr())
    ),
    fluidRow(
      column(3,
             wellPanel(downloadTableUI("downloadinteractiontable"))),
      column(9,
             splitLayout(
             dataTableOutput("varinteractiontable1")))
    )
  )
  return(output)
})

output$irfplotui <- renderUI({
  req(input$performIRF)
  output <- tagList(
    numericInput("rfselected", label = "Which Iteration Should be Used?", value = 1, min = 1, max = input$IRFiterations)
    ,
    tags$h3("Variable Importance Plot"),
    numericInput("IRFvarimpplotsize", "Select Size of Plot:", value = 600, min = 100,width = "100%"),
    numericInput("IRFvarimpninteractions", "How Many Variables Should be Shown?", value = 10, min = 1, max = 50, step = 1),
    downloadPlotUI("IRFvarimpdownload")
  )
})


output$irftableui <- renderUI({
  validate(
    need(input$makefile, "Please Upload an Amplicon Dataset"),
    need(input$makeIRFdataset, "You Must First Complete Step 1"),
    need(input$parseIRF, "You Must First Complete Step 2"),
    need(input$performIRF, "You Must First Complete Step 3")
  )
  if(is.null(IRFmodel()))return(NULL)
  output <- tagList(
    fluidRow(
      fluidRow(
        column(3,
               hr()),
        column(6,
               wellPanel(tags$div(tags$b(tags$h4("View Variable Importances")), align = "center"))),
        column(3,
               hr())
      ),
      column(3,
      wellPanel(downloadTableUI("downloadvarimptable"))),
      column(9,
             dataTableOutput("varimportancetable1"))
    )
    ,
    uiOutput("varinteractiontable2")
  )
  return(output)
})

output$IRFtabledownloadUI <- renderUI({
  req(input$parseIRF)
  output <- tagList(
    hr()
    ,
    radioButtons("IRFtabledownloadoptions", "Select Plot to Download",
                choices = c("X Train",
                            "Y Train",
                            "X Test",
                            "Y Test"),
                selected = "X Train", inline = TRUE)
    ,
    downloadTableUI("IRFtables")
  )
  return(output)
})
selectedIRFTable <- reactive({
  req(input$parseIRF)
  switch(input$IRFtabledownloadoptions,
         "X Train" = Xtrain(),
         "Y Train" = Ytrain(),
         "X Test" = Xtest(),
         "Y Test" = Ytest())
  #}
})
downloadTable("IRFtables", selectedIRFTable())

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
