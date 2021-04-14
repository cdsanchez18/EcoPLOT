
##Plant File upload (phenotype data)

# output$plantfileupload <- renderUI({
#   output <- tagList(
#     checkboxInput("phenotypeexampledata", "Use Example Data", value = FALSE, width = "100%")
#     ,
#     fluidRow(
#       column(5, hr()),
#       column(2,tags$div(tags$h4("OR"), align="center")),
#       column(5, hr())
#     )
#     ,
#     h4(tags$u("Upload File:")," EcoPLOT accepts",tags$b(".csv, .txt, .xlsx "),"file formats.")
#     ,
#     fileInput("phenotypedata", "Select File",
#               multiple = FALSE,
#               accept = c("text/csv",
#                          "text/comma-separated-values,text/plain",
#                          ".csv"))
#     ,
#     tags$hr(),
#     fluidRow(
#       column(4,
#              checkboxInput("header", "Header", TRUE)),
#       column(4,
#              radioButtons("sep", "Separator",
#                           choices = c(Comma = ",",
#                                       Semicolon = ";",
#                                       Tab = "\t"),
#                           selected = ","))
#       ,
#       column(4,
#              radioButtons("quote", "Quote",
#                           choices = c(None = "",
#                                       "Double Quote" = '"',
#                                       "Single Quote" = "'"),
#                           selected = '"')))
#     ,
#     uiOutput("phenotypemergeUI")
#     # conditionalPanel("input.environmentdata || input.environmentexampledata",
#     #                  conditionalPanel("input.phenotypedata || input.phenotypeexampledata",
#     #                  hr()
#     #                  ,
#     #                  tags$h4("Merge Datasets")
#     #                  ,
#     #                  checkboxInput("phenotypemergefiles", "Merge Environment and Phenotype Datasets?", value = FALSE, width = "100%")))
#   )
#   return(output)
# })
output$phenotypemergeUI <- renderUI({
  req(phenotypedata$table1)
  req(environmentdata$table1)
  output <- tagList(
    hr()
    ,
    tags$div(tags$h4("Merge Environment and Phenotype Datasets"), align = "center")
    ,
    uiOutput("phenotypemergeUI2")
    # ,
    # selectInput("environmentmergeColumn", "Select Common Column ID to Join By:",
    #             choices = c("NULL", as.list(intersect(colnames(environmentdata$table1), colnames(phenotypedata$table1)))),
    #             selected = "NULL")
  )
  return(output)
})
output$phenotypemergeUI2 <- renderUI({
  isolate(
  actionButton("phenotypemergefiles", "Merge Files", width = "100%")
  )
})
#observeEvent(input$phenotypemergefiles, {
#   #req(input$phenotypemergefiles)
#   #updateCheckboxInput(session = getDefaultReactiveDomain(), "phenotypemergefiles", "Merge Environment and Phenotype Datasets?", value = TRUE)
#   updateCheckboxInput(session = getDefaultReactiveDomain(), "environmentmergefiles", "Merge Environment and Phenotype Datasets?", value = TRUE)
# })
# observeEvent(input$environmentmergefiles, {
#   #req(input$environmentmergefiles)
#   updateCheckboxInput(session = getDefaultReactiveDomain(), "phenotypemergefiles", "Merge Environment and Phenotype Datasets?", value = TRUE)
#   #updateCheckboxInput(session = getDefaultReactiveDomain(), "environmentmergefiles", "Merge Environment and Phenotype Datasets?", value = TRUE)
# })

#create reactive values object
phenotypedata <- reactiveValues(path = NULL)
#code to upload file and display error message if an error occurs
observeEvent(input$phenotypedata, {
  req(input$phenotypedata)
  if(tolower(tools::file_ext(input$phenotypedata$datapath)) == "csv" ||
     tolower(tools::file_ext(input$phenotypedata$datapath)) == "txt"){
    #if(!is.null(environmentdata$table)){
      phenotypedata$table <- read.csv(input$phenotypedata$datapath, sep = input$sep, quote = input$quote, header = input$header)
    #   if(length(intersect(names(phenotypedata$table), names(environmentdata$table))) >= 1){
    #     phenotypedata$table <- read.csv(input$phenotypedata$datapath, sep = input$sep, quote = input$quote, 
    #                                     header = input$header)
    #     phenotypedata$table <- left_join(phenotypedata$table, environmentdata$table) %>% na.omit()
    #     phenotypeselections$table[["Row_ID"]] <- rownames(phenotypeselections$table)
    #   } else {
    #     phenotypedata$table <- read.csv(input$phenotypedata$datapath, sep = input$sep, quote = input$quote, 
    #                                     header = input$header)
    #     #phenotypeselections$table[["Row_ID"]] <- rownames(phenotypeselections$table)
    #   }
    # }else if(is.null(environmentdata$table)){
    #   phenotypedata$table <- read.csv(input$phenotypedata$datapath, sep = input$sep, quote = input$quote, 
    #                                   header = input$header)
    #   #phenotypeselections$table[["Row_ID"]] <- rownames(phenotypeselections$table)
    # }
  }else if(tolower(tools::file_ext(input$phenotypedata$datapath)) == "xlsx"){
    phenotypedata$table <- read_excel(path = input$phenotypedata$datapath)
    #phenotypeselections$table[["Row_ID"]] <- rownames(phenotypeselections$table)
  } else {
    showNotification("File Type Not Recognized. Please Select a Different File.", duration = NULL, type = "error")
  }
})

observeEvent(input$phenotypedata, {
  if(!is.null(phenotypedata$table))return(NULL)
    showNotification("Error in Uploading File. Please make sure format options are correct.")
})
#load up example data when box is checked
observeEvent(input$phenotypeexampledata, {
  req(input$phenotypeexampledata)
  phenotypedata$table <- read.csv("data/testphenotype.csv") %>% na.omit()
  #phenotypeselections$table$Row_ID <- rownames(phenotypeselections$table)
})
observeEvent(input$phenotypedata, {
  updateCheckboxInput(session, "phenotypeexampledata", "Use Example Data", value = FALSE)
})
#maintains main file, but creates copy for downstream work
observeEvent(input$phenotypeexampledata, {
  if(is.null(phenotypedata$table))return(NULL)
  req(phenotypedata$table)
  output <- phenotypedata$table
  output$Row_ID <- rownames(phenotypedata$table)
  phenotypedata$table1 <- output #phenotypedata$table
  phenotypedata$filter <- phenotypedata$table1
})
observeEvent(input$phenotypedata, {
  req(input$phenotypedata)
  req(phenotypedata$table)
  if(is.null(phenotypedata$table))return(NULL)
  output <- phenotypedata$table
  output$Row_ID <- rownames(phenotypedata$table)
  phenotypedata$table1 <- output #phenotypedata$table
  phenotypedata$filter <- phenotypedata$table1
})

##merge datasets if user chooses to do so
mergedata <- reactiveValues(path = NULL)
observeEvent(input$phenotypemergefiles, {
  req(phenotypedata$table1)
  req(environmentdata$table1)
  req(input$phenotypemergefiles)
  #req(input$environmentmergefiles)
  
  if(length(intersect(colnames(phenotypedata$table1), colnames(environmentdata$table1))) >= 1){
  mergedata$table <- left_join(phenotypedata$table1 %>% select(-Row_ID), environmentdata$table1 %>% select(-Row_ID))
  mergedata$table <- mergedata$table %>% mutate(Row_ID = rownames(mergedata$table))
  phenotypedata$table1 <- mergedata$table
  phenotypedata$filter <- mergedata$table
  environmentdata$table1 <- mergedata$table
  environmentdata$filter <- mergedata$table
  }else{
    showNotification("No common column ID's are shared between the datasets. They will not be merged", type = "warning")
  }
  
  #environmentdata$table1 <- mergedata
  
})
# observeEvent(input$phenotypedata, {
#   req(input$phenotypedata)
#   if(!is.null(environmentdata$table1) && !is.null(phenotypedata$table1)){
#     if(length(intersect(names(environmentdata$table1), names(phenotypedata$table1))) >= 1){
#       showNotification("Phenotype and Environmental datasets have been merged.", type = "warning")
#     } 
#   }else if(is.null(environmentdata$table)){
#     NULL
#   }
# })
# observeEvent(input$phenotypeexampledata, {
#   req(environmentdata$table1)
#   req(phenotypedata$table1)
#   if(!is.null(environmentdata$table1) && !is.null(phenotypedata$table1)){
#     if(length(intersect(names(environmentdata$table1), names(phenotypedata$table1))) >= 1){
#       showNotification("Phenotype and Environmental datasets have been merged.", type = "warning")
#     } 
#   }else if(is.null(environmentdata$table)){
#     NULL
#   }
# })
#code to melt dataframe for boxplot (required for plotting)
observe({
  if(is.null(phenotypedata$table))return(NULL)
  req(phenotypedata$table)
  names <- names(dplyr::select_if(phenotypedata$table1, is.numeric))
  if(is.null(phenotypedata$filter)){
    phenotypedata$melt <- tryCatch(phenotypedata$table1 %>% pivot_longer(cols = all_of(names),
                                                                names_to = "Measure",
                                                                values_to = "Value"),
                                   error = function(cond){
                                     message("Error")
                                     return(NULL)
                                   },
                                   warning = function(cond){
                                     message("Warning")
                                     return(NULL)
                                   })
  }else if(!is.null(phenotypedata$filter)){
    if(input$phenotypedatasource == "Original"){
      phenotypedata$melt <- tryCatch(phenotypedata$table1 %>% pivot_longer(cols = all_of(names),
                                                                  names_to = "Measure",
                                                                  values_to = "Value"),
                                     error = function(cond){
                                       message("Error")
                                       return(NULL)
                                     },
                                     warning = function(cond){
                                       message("Warning")
                                       return(NULL)
                                     })
    }else if(input$phenotypedatasource == "Filtered"){
      phenotypedata$melt <- tryCatch(phenotypedata$filter %>% pivot_longer(cols = all_of(names),
                                                                  names_to = "Measure",
                                                                  values_to = "Value"),
                                     error = function(cond){
                                       message("Error")
                                       return(NULL)
                                     },
                                     warning = function(cond){
                                       message("Warning")
                                       return(NULL)
                                     })
    }
  }
})
observe({
  req(phenotypedata$table)
  if(!is.null(phenotypedata$melt))return(NULL)
  showNotification("Error Uploading File. Please Review Upload Options.", type = "error")
})
#UI for changing variable class
output$plantvariableclassUI <- renderUI({
  req(phenotypedata$table)
  if(is.null(phenotypedata$table))return(NULL)
  output <- tagList(
    selectInput("plantcolnames", "Select Variable to Alter",
                choices = c("NULL", 
                            as.list(colnames(phenotypedata$table1))),
                selected = "NULL")
    ,
    selectInput("plantclassoptions", "Desired Variable Class",
                choices = c("NULL" = "NULL",
                            "Numeric" = "numeric",
                            "Factor" = "factor",
                            "Character" = "character",
                            "Logical" = "logical",
                            "Date" = "date",
                            "Time" = "time",
                            "Timestamp (Date + Time)" = "timestamp"),
                selected = "NULL")
    ,
    conditionalPanel(condition = "input.plantclassoptions == 'date' || 
                     input.plantclassoptions == 'timestamp'",
                     radioButtons("phenotypedateformat", "Select Date Format",
                                  choices = c("Year/Month/Day" = "ymd",
                                              "Year/Day/Month" = "ydm",
                                              "Month/Day/Year" = "mdy",
                                              "Month/Year/Day" = "myd",
                                              "Day/Month/Year" = "dmy",
                                              "Day/Year/Month" = "dym")))
    ,
    conditionalPanel(condition = "input.plantclassoptions == 'time' || 
                     input.plantclassoptions == 'timestamp'",
                     radioButtons("phenotypetimeformat", "Select Time Format",
                                  choices = c("Hour:Minute:Second" = "%H:%M:%S",
                                              "Hour:Minute" = "%H:%M",
                                              "Minute:Second" = "%M:%S")))
    ,
    actionButton("plantchangeclass", "Change Class", width = "100%")
    ,
    hr()
    ,
    actionButton("plantresetclass", "Reset Class Changes", width = "100%")
  )
  return(output)
})
#code to change class
observeEvent(input$plantchangeclass,{
  if(input$plantclassoptions != "time" && input$plantclassoptions != "date" && input$plantclassoptions != "timestamp"){
    phenotypedata$table1 <- eval(parse(text = paste0("phenotypedata$table1 %>% mutate(",
                                                     input$plantcolnames,
                                                     " = as.",
                                                     input$plantclassoptions,
                                                     "(",
                                                     input$plantcolnames,
                                                     "))")))
  } else if(input$plantclassoptions == "date"){
    phenotypedata$table1[[input$plantcolnames]] <- lubridate::parse_date_time(phenotypedata$table[[input$plantcolnames]],
                                                                              orders = input$phenotypedateformat,
                                                                              locale = "en_US.UTF-8")
    
  }else if(input$plantclassoptions == "time"){
    phenotypedata$table1[[input$plantcolnames]] <- lubridate::parse_date_time(phenotypedata$table[[input$plantcolnames]],
                                                                              orders = input$phenotypetimeformat,
                                                                              locale = "en_US.UTF-8")
  }else if(input$plantclassoptions == "timestamp"){
    phenotypedata$table1[[input$plantcolnames]] <- lubridate::parse_date_time(x = phenotypedata$table[[input$plantcolnames]], 
                                                                              orders = paste(input$phenotypedateformat, input$phenotypetimeformat),
                                                                              locale = "en_US.UTF-8")
  }
})
#code to reset changes upon "reset"
observeEvent(input$plantresetclass,{
  req(phenotypedata$table)
  phenotypedata$table1 <- phenotypedata$table
})
#View Table of file to the UI
output$phenotypedatatable <- renderDataTable({
  req(phenotypedata$table)
  phenotypedata$table1
})
output$plantuploadmain <- renderUI({
  req(phenotypedata$table)
  output <- tagList(
    splitLayout(dataTableOutput("phenotypedatatable"))
  )
  return(output)
})
#print summary of file to the UI
output$plantdatasummary <- renderPrint({
  req(phenotypedata$table)
  summary(phenotypedata$table1)
  
})
#code block for change class UI
output$changeclassUI <- renderUI({
  req(phenotypedata$table)
  output <- tagList(
    tags$div(id = "class-change",
             fluidRow(
               column(2,
                      hr()),
               column(8,
                      wellPanel(
                        tags$h4(
                          "Variable classes affect how variables can be used in plots or statistical analyses. EcoPLOT recognizes quantitative variables as", tags$b("Factors"), "or as", tags$b("Characters"), 
                          "and continuous variables as", tags$b("Numeric"), "or as", tags$b("Integers"),".", tags$b("Time, Date, and Timestamp"), 
                          "classes require additional formatting options.", align = "center"),
                        tags$h4("If EcoPLOT recognizes a variable incorrectly, you may alter it here.", align = "center")
                      )),
               column(2,
                      hr())
             )
             ,
             fluidRow(
               column(4,
                      wellPanel(
                        uiOutput("plantvariableclassUI"))),
               column(8,
                      wellPanel(
                        verbatimTextOutput("plantdatasummary"))))
    )
  )
  return(output)
})