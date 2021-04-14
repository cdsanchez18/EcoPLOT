# output$environmentfileupload <- renderUI({
#   output <- tagList(
#     checkboxInput("environmentexampledata", "Use Example Data", value = FALSE, width = "100%")
#     ,
#     fluidRow(
#       column(5, hr()),
#       column(2,tags$div(tags$h4("OR"), align="center")),
#       column(5, hr())
#     )
#     ,
#     h4(tags$u("Upload File:")," EcoPLOT accepts",tags$b(".csv, .txt, .xlsx "),"file formats.")
#     ,
#     fileInput("environmentdata", "Select File",
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
#     conditionalPanel("input.phenotypedata || input.phenotypeexampledata",
#                      conditionalPanel("input.environmentfileuploaded || input.environmentexampledata",
#                                       hr()
#                                       ,
#                                       tags$h4("Merge Datasets")
#                                       ,
#                                       checkboxInput("environmentmergefiles", "Merge Environment and Phenotype Datasets?", value = FALSE, width = "100%")))
#   )
#   return(output)
# })
# getenvironmentdata <- reactive({
#   if(is.null(input$environmentdata))return(NULL)
# })
output$environmentmergeUI <- renderUI({
  req(phenotypedata$table1)
  req(environmentdata$table1)
  output <- tagList(
    hr()
    ,
    tags$div(tags$h4("Merge Environment and Phenotype Datasets"), align = "center")
    ,
    actionButton("environmentmergefiles", "Merge Files", width = "100%")
    # ,
    # selectInput("environmentmergeColumn", "Select Common Column ID to Join By:",
    #             choices = c("NULL", as.list(intersect(colnames(environmentdata$table1), colnames(phenotypedata$table1)))),
    #             selected = "NULL")
  )
  return(output)
})

#create reactive values object
environmentdata <- reactiveValues(path = NULL)
#code to upload file and display error message if an error occurs
observeEvent(input$environmentdata, {
  req(input$environmentdata)
  if(tolower(tools::file_ext(input$environmentdata$datapath)) == "csv" ||
     tolower(tools::file_ext(input$environmentdata$datapath)) == "txt"){
    environmentdata$table <- read.csv(input$environmentdata$datapath, sep = input$sep, quote = input$quote, 
                                      header = input$header
    )
    #}
  }else if(tolower(tools::file_ext(input$environmentdata$datapath)) == "xlsx"){
    environmentdata$table <- read_excel(path = input$environmentdata$datapath)
  } else {
    
    showNotification("File Type Not Recognized. Please Select a Different File.", duration = NULL, type = "error")
  }
})


#load up example data when box is checked
observeEvent(input$environmentexampledata, {
  req(input$environmentexampledata)
  environmentdata$table <- read.csv("data/testenvironment.csv")
})
observeEvent(input$environmentdata, {
  updateCheckboxInput(session, "environmentexampledata", "Use Example Data", value = FALSE)
})
#maintains main file, but creates copy for downstream work
observeEvent(input$environmentexampledata, {
  req(environmentdata$table)
  output <- environmentdata$table
  output$Row_ID <- rownames(environmentdata$table)
  environmentdata$table1 <- output#environmentdata$table
  environmentdata$filter <- environmentdata$table1
})
observeEvent(input$environmentdata, {
  req(environmentdata$table)
  output <- environmentdata$table
  output$Row_ID <- rownames(environmentdata$table)
  environmentdata$table1 <- output#environmentdata$table
  environmentdata$filter <- environmentdata$table1
})
#code to melt dataframe for boxplot (required for plotting)
observe({
  req(environmentdata$table)
  names <- names(dplyr::select_if(environmentdata$table1, is.numeric))
  if(is.null(environmentdata$filter)){
    environmentdata$melt <- tryCatch(environmentdata$table1 %>% pivot_longer(cols = all_of(names),
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
  }else if(!is.null(environmentdata$filter)){
    if(input$environmentdatasource == "Original"){
      environmentdata$melt <- tryCatch(environmentdata$table1 %>% pivot_longer(cols = all_of(names),
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
    }else if(input$environmentdatasource == "Filtered"){
      environmentdata$melt <- tryCatch(environmentdata$filter %>% pivot_longer(cols = all_of(names),
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
  req(environmentdata$table)
  if(!is.null(environmentdata$melt))return(NULL)
  showNotification("Error Uploading File. Please Review Upload Options.", type = "error")
})
#auto merge files if they are both present
# observeEvent(input$environmentdata, {
#   req(environmentdata$table1)
#   req(phenotypedata$table1)
#   if(!is.null(environmentdata$table1) && !is.null(phenotypedata$table1)){
#     if(length(intersect(names(environmentdata$table1), names(phenotypedata$table1))) >= 1){
#       environmentdata$table1 <- left_join(environmentdata$table1, phenotypedata$table1) %>% na.omit()
#       environmentdata$filter <- environmentdata$table1
#     } 
#   }else if(is.null(phenotypedata$table)){
#     NULL
#   }
# })
# observeEvent(input$environmentdata, {
#   req(environmentdata$table1)
#   req(phenotypedata$table1)
#   if(!is.null(environmentdata$table1) && !is.null(phenotypedata$table1)){
#     if(length(intersect(names(environmentdata$table1), names(phenotypedata$table1))) >= 1){
#       showNotification("Phenotype and Environmental datasets have been merged.", type = "warning")
#     } 
#   }else if(is.null(phenotypedata$table)){
#     NULL
#   }
# })
# observeEvent(input$environmentexampledata, {
#   req(environmentdata$table1)
#   req(phenotypedata$table1)
#   if(!is.null(environmentdata$table1) && !is.null(phenotypedata$table1)){
#     if(length(intersect(names(environmentdata$table1), names(phenotypedata$table1))) >= 1){
#       environmentdata$table1 <- left_join(environmentdata$table1, phenotypedata$table1) %>% na.omit()
#       environmentdata$filter <- environmentdata$table1
#     } 
#   }else if(is.null(phenotypedata$table)){
#     NULL
#   }
# })
# observeEvent(input$environmentexampledata, {
#   req(environmentdata$table1)
#   req(phenotypedata$table1)
#   if(!is.null(environmentdata$table1) && !is.null(phenotypedata$table1)){
#     if(length(intersect(names(environmentdata$table1), names(phenotypedata$table1))) >= 1){
#       showNotification("Phenotype and Environmental datasets have been merged.", type = "warning")
#     } 
#   }else if(is.null(phenotypedata$table)){
#     NULL
#   }
# })
#UI for changing variable class
output$environmentvariableclassUI <- renderUI({
  req(environmentdata$table)
  output <- tagList(
    selectInput("environmentcolnames", "Select Variable to Alter",
                choices = c("NULL", 
                            as.list(colnames(environmentdata$table1))),
                selected = "NULL")
    ,
    selectInput("environmentclassoptions", "Desired Variable Class",
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
    conditionalPanel(condition = "input.environmentclassoptions == 'date' || 
                     input.environmentclassoptions == 'timestamp'",
                     radioButtons("environmentdateformat", "Select Date Format",
                                  choices = c("Year/Month/Day" = "ymd",
                                              "Year/Day/Month" = "ydm",
                                              "Month/Day/Year" = "mdy",
                                              "Month/Year/Day" = "myd",
                                              "Day/Month/Year" = "dmy",
                                              "Day/Year/Month" = "dym")))
    ,
    conditionalPanel(condition = "input.environmentclassoptions == 'time' || 
                     input.environmentclassoptions == 'timestamp'",
                     radioButtons("environmenttimeformat", "Select Time Format",
                                  choices = c("Hour:Minute:Second" = "%H:%M:%S",
                                              "Hour:Minute" = "%H:%M",
                                              "Minute:Second" = "%M:%S")))
    ,
    actionButton("environmentchangeclass", "Change Class", width = "100%")
    ,
    hr()
    ,
    actionButton("environmentresetclass", "Reset Class Changes", width = "100%")
  )
  return(output)
})
#code to change class
observeEvent(input$environmentchangeclass,{
  if(input$environmentclassoptions != "time" && input$environmentclassoptions != "date" && input$environmentclassoptions != "timestamp"){
    environmentdata$table1 <- eval(parse(text = paste0("environmentdata$table1 %>% mutate(",
                                                       input$environmentcolnames,
                                                       " = as.",
                                                       input$environmentclassoptions,
                                                       "(",
                                                       input$environmentcolnames,
                                                       "))")))
  } else if(input$environmentclassoptions == "date"){
    environmentdata$table1[[input$environmentcolnames]] <- lubridate::parse_date_time(environmentdata$table[[input$environmentcolnames]],
                                                                                      orders = input$environmentdateformat,
                                                                                      locale = "en_US.UTF-8")
    
  }else if(input$environmentclassoptions == "time"){
    environmentdata$table1[[input$environmentcolnames]] <- lubridate::parse_date_time(environmentdata$table[[input$environmentcolnames]],
                                                                                      orders = input$environmenttimeformat,
                                                                                      locale = "en_US.UTF-8")
  }else if(input$environmentclassoptions == "timestamp"){
    environmentdata$table1[[input$environmentcolnames]] <- lubridate::parse_date_time(x = environmentdata$table[[input$environmentcolnames]], 
                                                                                      orders = paste(input$environmentdateformat, input$environmenttimeformat),
                                                                                      locale = "en_US.UTF-8")
  }
})
#code to reset changes upon "reset"
observeEvent(input$environmentresetclass,{
  req(environmentdata$table)
  environmentdata$table1 <- environmentdata$table
})
#View Table of file
output$environmentdatatable <- renderDataTable({
  req(environmentdata$table)
  environmentdata$table1
})
#print summary of file
output$environmentdatasummary <- renderPrint({
  req(environmentdata$table)
  summary(environmentdata$table1)
  
})
#code block for change class UI
output$environmentchangeclassUI <- renderUI({
  req(environmentdata$table)
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
                        uiOutput("environmentvariableclassUI"))),
               column(8,
                      wellPanel(
                        verbatimTextOutput("environmentdatasummary"))))
    )
  )
  return(output)
})
output$environmentuploadmain <- renderUI({
  req(environmentdata$table)
  output <- tagList(
    splitLayout(dataTableOutput("environmentdatatable"))
  )
  return(output)
})
