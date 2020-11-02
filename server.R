##The following code presents the server side processing of EcoPLOT

shinyServer(function(input, output, session){
  options(shiny.maxRequestSize=100*1024^4)
#EcoPOD Data-----------------------------------------------------------------------
  ##upload dataset and save as dataframe
ecopoddata <- reactive({
    ecopod <- input$ecopoddata
    if (is.null(ecopod)) return(NULL)
    ecopod <- read_excel(ecopod$datapath, sheet= 1)
    ecopod$Timestamp <- format(ecopod$Timestamp, 
                               format = "%Y-%m-%d %H:%M")
    ecopod
})
  ##visualize uploaded data
  output$ecopoddatatable <- renderTable({
    if (is.null(ecopoddata())) return(NULL)
    head(ecopoddata())
})
  ##melt uploaded dataset so that it can be easily graphed
  ecopodmelt <- reactive({
    if (is.null(ecopoddata())) return(NULL)
    ecopod <- ecopoddata() %>% gather(Sensor, Value, -Timestamp) 
    ecopod$Value <- as.numeric(ecopod$Value)
    ecopod
})
  ##view melted dataset
  output$ecopodmelt1 <- renderTable({
    if (is.null(ecopoddata())) return(NULL)
    head(ecopodmelt())
})
  output$ecopodx <- renderUI({
    if (is.null(ecopodmelt())) return(NULL)
    selectInput("ecopodx", "Sensor(s)", 
                choices = ecopodmelt()$Sensor,
                selected = ecopodmelt()$Sensor[1],
                multiple = TRUE)
})
  
  mindateecopod <- reactive({
    if (is.null(ecopoddata())) return(NULL)
    min(ecopodmelt()$Timestamp)
  })
  maxdateecopod <- reactive({
    if (is.null(ecopoddata())) return(NULL)
    max(ecopodmelt()$Timestamp)
  })
  
  output$slider0 <- renderUI({
    if (is.null(ecopoddata())) return(NULL)
    sliderInput("slider0", "Choose Range to Show:",
                min = as.POSIXct(mindateecopod()),
                max = as.POSIXct(maxdateecopod()),
                value = c(as.POSIXct(mindateecopod()),
                          as.POSIXct(maxdateecopod())),
                step= 300,
                timeFormat = "%Y-%m-%d %r")
  })
  ecopodfilter <- reactive({
    if (is.null(ecopoddata())) return(NULL)
    ecopodmelt() %>% 
      filter(Timestamp >= input$slider0[1] & 
               Timestamp <= input$slider0[2],
             Sensor %in% input$ecopodx)
})
  ecopodfiltertable <- eventReactive(input$ecopodrender, {
    if (is.null(input$ecopodrender)) return(NULL)
    head(ecopodfilter())
  })
  output$ecopodfiltertable <- renderTable({
    if (is.null(ecopoddata())) return(NULL)
    ecopodfiltertable()
})
  output$downloadecopoddata <- downloadHandler(
    filename = function() {"nameyourfile.csv"},
    content = function(file){
      write.csv(ecopodfiltertable(), file)
})

ecopodplot <- eventReactive(input$ecopodrender, {
    ggplot(ecopodfilter(), aes(x = as.POSIXct(Timestamp), y = Value, 
                               col = Sensor)) + 
      geom_point(size = input$size0) + 
      ggtitle(input$title0) + 
      theme(axis.text.x = element_text(color = "black", size = 10, 
                                       angle = 45)) +
      scale_x_datetime(breaks = input$ecopodspace) + 
      labs(y = input$ecopodyaxis,x = input$ecopodxaxis)                          
})
  output$ecopodplot <- renderPlotly({
    ecopodplot()
})
#Additional Sensors----------------------------------------------------------------
##upload dataset and save as dataframe
sensordata <- reactive({
  sensor <- input$sensordata
  if (is.null(sensor)) return(NULL)
  sensor <- read_excel(sensor$datapath, sheet= 1)
  sensor$Timestamp <- format(sensor$Timestamp, 
                             format = "%Y-%m-%d %H:%M")
                       
  sensor
})
##visualize uploaded data
output$sensordatatable <- renderTable({
  if (is.null(sensordata())) return(NULL)
  head(sensordata())
})
##melt uploaded dataset so that it can be easily graphed
sensormelt <- reactive({
  if (is.null(sensordata())) return(NULL)
  sensor <- sensordata() %>% gather(Sensor, Value, -Timestamp) 
  sensor$Value <- as.numeric(sensor$Value)
  sensor
})
##view melted dataset
output$sensormelt1 <- renderTable({
  if (is.null(sensordata())) return(NULL)
  head(sensormelt())
})
output$sensorx <- renderUI({
  if (is.null(sensormelt())) return(NULL)
  selectInput("sensorx", "Sensor(s)", 
              choices = sensormelt()$Sensor,
              selected = sensormelt()$Sensor[1],
              multiple = TRUE)
})

mindate <- reactive({
  if (is.null(sensordata())) return(NULL)
  min(sensormelt()$Timestamp)
})
maxdate <- reactive({
  if (is.null(sensordata())) return(NULL)
  max(sensormelt()$Timestamp)
})

output$slider1 <- renderUI({
  if (is.null(sensordata())) return(NULL)
    sliderInput("slider1", "Choose Range to Show:",
                min = as.POSIXct(mindate()),
                max = as.POSIXct(maxdate()),
                value = c(as.POSIXct(mindate()),
                          as.POSIXct(maxdate())),
                step= 300,
                timeFormat = "%Y-%m-%d %r")
})
sensorfilter <- reactive({
  if (is.null(sensordata())) return(NULL)
  sensormelt() %>% 
    filter(Timestamp >= input$slider1[1] & 
             Timestamp <= input$slider1[2],
           Sensor %in% input$sensorx)
})
sensorfiltertable <- eventReactive(input$sensorrender, {
  if (is.null(input$sensorrender)) return(NULL)
  head(sensorfilter())
})
output$sensorfiltertable <- renderTable({
  if (is.null(sensordata())) return(NULL)
  sensorfiltertable()
})
output$downloadsensordata <- downloadHandler(
  filename = function() {"nameyourfile.csv"},
  content = function(file){
    write.csv(filtered(), file)
  })
sensorplot <- eventReactive(input$sensorrender, {
  if (is.null(input$sensorrender)) return(NULL)
  ggplot(sensorfilter(), aes(x = as.POSIXct(Timestamp), y = Value, 
                             col = Sensor)) + 
    geom_point(size = input$size1) + ggtitle(input$title1) + 
    theme(axis.text.x = element_text(color = "black", size = 10, 
                                     angle = 45)) +
    scale_x_datetime(breaks = input$sensorspace ) + 
    labs(x = input$sensorxaxis)                          
})
output$sensorplot <- renderPlotly({
    sensorplot()
})

#Plant Phenotype-------------------------------------------------------------------
output$plantfileupload <- renderUI({
  output <- tagList(
    h3("Upload File: EcoPLOT accepts .csv, .txt, .xlsx file formats.")
    ,
    fileInput("phenotypedata", "Select File",
              multiple = FALSE,
              accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv"))
    ,
    tags$hr(),
    checkboxInput("header", "Header", TRUE),
    radioButtons("sep", "Separator",
                 choices = c(Comma = ",",
                             Semicolon = ";",
                             Tab = "\t"),
                 selected = ",")
    ,
    radioButtons("quote", "Quote",
                 choices = c(None = "",
                             "Double Quote" = '"',
                             "Single Quote" = "'"),
                 selected = '"')
    )
  return(output)
})
phenotypedata <- reactiveValues(path = NULL)
observeEvent(input$phenotypedata, {
  req(input$phenotypedata)
if(tolower(tools::file_ext(input$phenotypedata$datapath)) == "csv" ||
   tolower(tools::file_ext(input$phenotypedata$datapath)) == "txt"){
  if(!is.null(geochemistrydata$table)){
    phenotypedata$table <- read.csv(input$phenotypedata$datapath, sep = input$sep, quote = input$quote, 
                                    header = input$header
                                    )
    if(length(intersect(names(phenotypedata$table), names(geochemistrydata$table))) >= 1){
    phenotypedata$table <- read.csv(input$phenotypedata$datapath, sep = input$sep, quote = input$quote, 
                                    header = input$header
                                    )
    phenotypedata$table <- left_join(phenotypedata$table, geochemistrydata$table) %>% na.omit()
    } else {
      phenotypedata$table <- read.csv(input$phenotypedata$datapath, sep = input$sep, quote = input$quote, 
                                      header = input$header
                                      )
      }
    }else if(is.null(geochemistrydata$table)){
      phenotypedata$table <- read.csv(input$phenotypedata$datapath, sep = input$sep, quote = input$quote, 
                                      header = input$header
                                      )
    }
  }else if(tolower(tools::file_ext(input$phenotypedata$datapath)) == "xlsx"){
    phenotypedata$table <- read_excel(path = input$phenotypedata$datapath)
  } else {
    showNotification("File Type Not Recognized. Please Select a Different File.", duration = 10, type = "error")
  }
})
observe({
  req(phenotypedata$table)
  phenotypedata$table1 <- phenotypedata$table
  
})
observe({
  req(phenotypedata$table)
  names <- names(dplyr::select_if(phenotypedata$table1, is.numeric))
  if(is.null(phenotypedata$filter)){
    phenotypedata$melt <- phenotypedata$table1 %>% pivot_longer(cols = all_of(names),
                                                                names_to = "Measure",
                                                                values_to = "Value")
  }else if(!is.null(phenotypedata$filter)){
    if(input$phenotypedatasource == "Original"){
      phenotypedata$melt <- phenotypedata$table1 %>% pivot_longer(cols = all_of(names),
                                                                  names_to = "Measure",
                                                                  values_to = "Value")
    }else if(input$phenotypedatasource == "Filtered"){
      phenotypedata$melt <- phenotypedata$filter %>% pivot_longer(cols = all_of(names),
                                                                  names_to = "Measure",
                                                                  values_to = "Value")
    }
  }
  })
output$plantvariableclassUI <- renderUI({
  req(phenotypedata$table)
  output <- tagList(
    tags$h5("If R recognizes a column variable incorrectly, you may 
            alter it here")
    ,
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
observeEvent(input$plantresetclass,{
  req(phenotypedata$table)
  phenotypedata$table1 <- phenotypedata$table
})
#Test 
output$testprint <- renderPrint({
  req(phenotypedata$table)
  class(phenotypedata$table1$Timestamp)
})
#View Table of file
output$phenotypedatatable <- renderDataTable({
  req(phenotypedata$table)
  phenotypedata$table1
})
#print summary of file
output$plantdatasummary <- renderPrint({
  req(phenotypedata$table)
  summary(phenotypedata$table1)
})
output$plantuploadmain <- renderUI({
  req(phenotypedata$table)
  output <- tagList(
    splitLayout(dataTableOutput("phenotypedatatable"))
    ,
    tags$div(id="sidebar",
    tags$h4("View a Summary of the Uploaded Data")
    ,
    verbatimTextOutput("plantdatasummary"))
  )
  return(output)
})

###Phenotype filtering options ----
output$phenotypefilteringoptionsUI <- renderUI({
  req(phenotypedata$table)
  colnames <- names(phenotypedata$table1)
  output <- tagList(
    selectInput("phenotypefilteroption1", "Select Variable Category",
                choices = c("NULL", colnames),
                selected = "NULL",
                multiple = FALSE)
  )
  return(output)
})
output$phenotypefilteringoptionsUI1 <- renderUI({
  req(phenotypedata$table)
  output <- tagList(
  if(!is.null(av(input$phenotypefilteroption1))){
    if(is.character(phenotypedata$table1[[input$phenotypefilteroption1]]) || 
       is.factor(phenotypedata$table1[[input$phenotypefilteroption1]])){
      options <- as.list(unique(phenotypedata$table1[[input$phenotypefilteroption1]]))
      
      selectInput("phenotypefilteroption2", "Filter Sub Category",
                  choices = c("NULL", options),
                  selected = "NULL",
                  multiple = TRUE)
    }else if(is.numeric(phenotypedata$table1[[input$phenotypefilteroption1]]) || 
             is.integer(phenotypedata$table1[[input$phenotypefilteroption1]])){
      min <- min(phenotypedata$table1[[input$phenotypefilteroption1]])
      max <- max(phenotypedata$table1[[input$phenotypefilteroption1]])
      
      sliderInput("phenotypefilteroption2", "Filter Sub Category",
                  min = min,
                  max = max,
                  value = c(max/4, max/2),
                  step = 1)
    }else if(is.POSIXct(phenotypedata$table1[[input$phenotypefilteroption1]])){
      sliderInput("phenotypefilteroption2", "Filter Sub Category",
                  min = min(phenotypedata$table1[[input$phenotypefilteroption1]]),
                  max = max(phenotypedata$table1[[input$phenotypefilteroption1]]),
                  value = c(min(phenotypedata$table1[[input$phenotypefilteroption1]]),
                  max(phenotypedata$table1[[input$phenotypefilteroption1]])))
    }
  } else {
    NULL
  }
  ,
  if(!is.null(av(input$phenotypefilteroption1))){
  actionButton("phenotypefilterrender", "Filter", width = "100%")
  }else{NULL}
  )
})
observeEvent(input$phenotypefilterrender, {
  req(phenotypedata$table)
  if(!is.null(av(input$phenotypefilteroption1))){
    if(is.character(phenotypedata$table1[[input$phenotypefilteroption1]]) ||
       is.factor(phenotypedata$table1[[input$phenotypefilteroption1]])){
      phenotypedata$filter <- phenotypedata$table1 %>% filter(!!as.symbol(input$phenotypefilteroption1) %in% input$phenotypefilteroption2)
    }else if(is.numeric(phenotypedata$table1[[input$phenotypefilteroption1]]) || 
             is.integer(phenotypedata$table1[[input$phenotypefilteroption1]])){
      phenotypedata$filter <- phenotypedata$table1 %>% filter(!!as.symbol(input$phenotypefilteroption1) >= input$phenotypefilteroption2[1] &
                                                               !!as.symbol(input$phenotypefilteroption1) <= input$phenotypefilteroption2[2])
      
    }else if(is.POSIXct(phenotypedata$table1[[input$phenotypefilteroption1]])){
      phenotypedata$filter <- phenotypedata$table1 %>% filter(!!as.symbol(input$phenotypefilteroption1) >= input$phenotypefilteroption2[1] &
                                                               !!as.symbol(input$phenotypefilteroption1) <= input$phenotypefilteroption2[2])
    }
  }else {
    phenotypedata$filter <- NULL
  }
  return(phenotypedata$filter)
})
output$phenotypeoriginaltable2 <- renderDataTable({
  req(phenotypedata$table)
  phenotypedata$table1
})
output$phenotypefilteredtable <- renderDataTable({
  req(phenotypedata$table)
  req(phenotypedata$filter)
  phenotypedata$filter
})
downloadTable(id = "phenotypefiltertabledownload", tableid = phenotypedata$filter)
output$phenotypefiltertableUI <- renderUI({
  req(phenotypedata$table)
  if(is.null(phenotypedata$filter)){
    output <- tagList(
      tags$h3("Original Table")
      ,
      splitLayout(dataTableOutput("phenotypeoriginaltable2"))
    )
  }else {
    output <- tagList(
      tags$h3("Filtered Table")
      ,
      splitLayout(dataTableOutput("phenotypefilteredtable"))
      ,
      downloadTableUI("phenotypefiltertabledownload")
    )
  }
  return(output)
})
# output$phenotypedatasourceUI <- renderUI({
#   shiny::radioButtons("phenotypedatasource", "Select Dataset to Use:",
#                      choices = c("Original"),
#                      selected = "Original", inline = TRUE)
# })

observeEvent(input$phenotypefilterrender, {
  updateRadioButtons(session, "phenotypedatasource", "Select Dataset to Use:",
                     choices = c("Original" = "Original",
                                 "Filtered" = "Filtered"),
                     selected = "Original", inline = TRUE)
})

observe({
  req(phenotypedata$table1)
  if(is.null(phenotypedata$filter)){
    phenotypedata$use <- phenotypedata$table1
  }else{
    if(input$phenotypedatasource == "Original"){
      phenotypedata$use <- phenotypedata$table1
    }else if(input$phenotypedatasource == "Filtered"){
      phenotypedata$use <- phenotypedata$filter
    }
  }
})

##phenotype plot UI Options-----
output$phenotypeplotUI <- renderUI({
  req(phenotypedata$table)
  output <- tagList(
    shiny::selectInput("phenotypeplottype", "Select Plot Type",
                choices = c("Histogram" = "histogram",
                            "Scatter" = "scatter",
                            "Boxplot" = "boxplot",
                            "Barplot" = "barplot"),
                selected = "histogram")
    ,
    conditionalPanel("input.phenotypeplottype == 'barplot'",
                     radioButtons("phenotypebarplottype", "Select View",
                                  choices = c("Count of Cases" = "bin",
                                              "Values of Column" = "identity"),
                                  selected = "bin",
                                  inline = TRUE))
    ,
    #phenotypedata$table1
    conditionalPanel("input.phenotypeplottype == 'barplot'",
                     selectInput("phenotypebarplotxaxis", "Select X Axis Variable",
                                 choices = c("NULL", colnames(dplyr::select_if(phenotypedata$use, is.factor)),
                                             colnames(dplyr::select_if(phenotypedata$use, is.character))
                                             ),
                                 selected = "NULL"))
    ,
    conditionalPanel("input.phenotypeplottype == 'barplot' && input.phenotypebarplottype == 'identity'",
                     selectInput("phenotypebarplotyaxis", "Select Y Axis Variable",
                                 choices = c("NULL", colnames(phenotypedata$use)),
                                 selected = "NULL"))
    ,
    conditionalPanel("input.phenotypeplottype == 'barplot'",
                     selectInput("phenotypebarplotfill", "Select Variable to Fill",
                                 choices = c("NULL", colnames(phenotypedata$use)),
                                 selected = "NULL"))
    ,
    conditionalPanel("input.phenotypeplottype == 'barplot' && input.phenotypebarplotfill != 'NULL'",
                     radioButtons("phenotypebarplotpos", "Bar Plot Type",
                                  choices = c("Stacked" = "stacked",
                                              "Dodged" = "dodge"),
                                  selected = "stacked"))
    ,
    conditionalPanel("input.phenotypeplottype == 'barplot' && input.phenotypebarplottype == 'identity'",
                    radioButtons("phenotypebarploterror", "Show Error Bars?",
                                 choices = c("Yes" = "yes",
                                             "No" = "no"),
                                 selected = "no",
                                 inline = TRUE))
    ,
    conditionalPanel(condition = "input.phenotypeplottype == 'histogram'",
                     radioButtons("phenotypehistplottype", "Select View",
                                  choices = c("Count" = "count",
                                              "Density" = "density"),
                                  selected = "count",
                                  inline = TRUE))
    ,
    conditionalPanel(condition = "input.phenotypeplottype == 'histogram'",
    selectInput("phenotypex", "Select Variable to Graph Along X-Axis:", 
                choices = c("NULL", colnames(dplyr::select_if(phenotypedata$use, is.numeric))),
                selected = "NULL",
                multiple = FALSE))
    ,
    conditionalPanel(condition =  "input.phenotypeplottype == 'scatter'",
                     selectInput("phenotypex1", "Select Variable to Graph Along X-Axis:", 
                                 choices = c("NULL", colnames(phenotypedata$use)),
                                 selected = "NULL",
                                 multiple = FALSE))
    ,
    conditionalPanel(condition = "input.phenotypeplottype == 'boxplot'",
                     selectInput("phenotypex2", "Select Variable(s) to Graph Along X-Axis:",
                                 choices = c("NULL", colnames(dplyr::select_if(phenotypedata$use, is.numeric))),
                                 selected = "NULL",
                                 multiple = TRUE))
    ,
    conditionalPanel(condition = "input.phenotypeplottype == 'scatter'",
                     selectInput("phenotypey", "Select Variable to Graph Along Y Axis:",
                                 choices = c("NULL", colnames(dplyr::select_if(phenotypedata$use, is.numeric))),
                                 selected = "NULL",
                                 multiple = FALSE))
    ,
    conditionalPanel(condition = "input.phenotypeplottype == 'histogram'",
                     selectInput("phenotypefacet", "Select Variable to Facet Around",
                                 choices = c("NULL", colnames(dplyr::select_if(phenotypedata$use, is.character)),
                                             colnames(dplyr::select_if(phenotypedata$use, is.factor))),
                                 selected = "NULL",
                                 multiple = FALSE))
    ,
    conditionalPanel(condition = "input.phenotypeplottype == 'histogram'",
                     numericInput("phenotypebinwidth", "Select Bin Width",
                                  value = 1, min = 0, max = 100))
    ,
    textInput("phenotypetitle", "Create Title for Plot",
              placeholder = "Plot Title")
    ,
    conditionalPanel(condition= "input.phenotypeplottype == 'histogram'",
                     textInput("phenotypexaxislabel", "Create X Axis Label",
                               placeholder = "X Axis Label"))
    ,
    conditionalPanel(condition = "input.phenotypeplottype == 'scatter' || 
                     input.phenotypeplottype=='boxplot'",
                     textInput("phenotypexaxislabel1", "Create X Axis Label",
                               placeholder = "X Axis Label"),
                     textInput("phenotypeyaxislabel1", "Create Y Axis Label",
                               placeholder = "Y Axis Label"))
    ,
    conditionalPanel(condition= "input.phenotypecoloroption != 'NULL'",
                     textInput("phenotypelegendlabel1", "Create Title For Legend",
                               placeholder = "Legend Title"))
    ,
    conditionalPanel(condition = "input.phenotypeplottype == 'histogram'",
                     colourpicker::colourInput("phenotypecolor", "Select Bar Color",
                                 value = "white"))
    ,
    conditionalPanel(condition = "input.phenotypeplottype == 'scatter' ||
                     input.phenotypeplottype == 'boxplot'",
                     selectInput("phenotypecoloroption", "Select Factor to Color",
                                 choices = c("NULL", names(dplyr::select_if(phenotypedata$use, is.character)),
                                             names(dplyr::select_if(phenotypedata$use, is.factor))),
                                 selected = "NULL"))
    ,
    conditionalPanel(condition= "input.phenotypeplottype == 'scatter'",
                     radioButtons("phenotyperegressionline", "Add Linear Regression?",
                                  choices = c("Yes",
                                              "No"),
                                  selected = "No"))
    ,
    conditionalPanel(condition = "input.phenotypeplottype == 'boxplot'",
                     radioButtons("phenotypefreeyaxis", "Free Y Axis?",
                                  choices = c("Yes",
                                              "No"),
                                  selected = "Yes"))
    ,
    conditionalPanel(condition = "input.phenotypeplottype == 'scatter' 
                     && input.phenotypecoloroption == 'NULL'",
                     colourpicker::colourInput("phenotypecolor1", "Select Color",
                                 value = "black"))
    ,
    downloadPlotUI("phenotypeplotdownload")
  )
  return(output)
})
downloadPlot("phenotypeplotdownload", phenotypeplot())
phenotypeplot <- reactive({
  req(phenotypedata$table)
  if(input$phenotypeplottype == "histogram"){
    if(!is.null(av(input$phenotypex))){
      if(input$phenotypehistplottype == "count"){
  plot <- ggplot(phenotypedata$use, aes(x = !!as.symbol(input$phenotypex))) +
                   geom_histogram(fill = input$phenotypecolor, color = "black",
                                  binwidth = input$phenotypebinwidth) +
    labs(title = input$phenotypetitle, y = "Sample Count",
         x = input$phenotypexaxislabel)
    }else{
      plot <- ggplot(phenotypedata$use, aes(x = !!as.symbol(input$phenotypex))) +
        geom_histogram(stat = "density",fill = input$phenotypecolor, color = "black") +
        labs(title = input$phenotypetitle, y = "Sample Count",
             x = input$phenotypexaxislabel)
    }
      if(!is.null(av(input$phenotypefacet))){
        phenotypedata$use[[input$phenotypefacet]] %>% sort()
        plot <- plot + facet_wrap(paste("~", input$phenotypefacet))
      }else{
        plot <- plot
      }
  }else {
    plot <- NULL
  }
  }else if(input$phenotypeplottype == "scatter"){
      if(!is.null(av(input$phenotypex1)) && !is.null(av(input$phenotypey))){
        if(!is.null(av(input$phenotypecoloroption))){
          plot <- ggplot(phenotypedata$use, aes(x = !!as.symbol(input$phenotypex1),
                                                  y = !!as.symbol(input$phenotypey),
                                                  color = !!as.symbol(input$phenotypecoloroption))) + 
            geom_point() + 
            labs(title= input$phenotypetitle, x = input$phenotypexaxislabel1,
                 y = input$phenotypeyaxislabel1, color = input$phenotypelegendlabel1) #+
            #abline(!!as.symbol(input$phenotypey) ~ !!as.symbol(input$phenotypex), data = phenotypedata$table, 
             #      color = input$phenotypecoloroption)
          if(input$phenotyperegressionline == "Yes"){
            plot <- plot + geom_smooth(method = lm, se = FALSE)
          }else {
            plot
          }
        }else{
          plot <- ggplot(phenotypedata$use, aes(x = !!as.symbol(input$phenotypex1),
                                                  y = !!as.symbol(input$phenotypey))) +
            geom_point(color = input$phenotypecolor1) + 
            labs(title= input$phenotypetitle, x = input$phenotypexaxislabel1,
                 y = input$phenotypeyaxislabel1)
          if(input$phenotyperegressionline == "Yes"){
            plot <- plot + geom_smooth(method = lm, se = FALSE)
          }else {
            plot
          }
        }
      }else{
        plot <- NULL
      }
  }else if(input$phenotypeplottype == "boxplot"){
    if(!is.null(av(input$phenotypex2))){
    filtered_data <- phenotypedata$melt %>% filter(Measure %in% input$phenotypex2)
    if(!is.null(av(input$phenotypecoloroption))){
    plot <- ggplot(filtered_data, aes(x = Measure, y = Value, fill = !!as.symbol(input$phenotypecoloroption))) +
      geom_boxplot() + 
      labs(title= input$phenotypetitle, x = input$phenotypexaxislabel1,
           y = input$phenotypeyaxislabel1, fill = input$phenotypelegendlabel1) 
    }else {
      plot <- ggplot(filtered_data, aes(x = Measure, y = Value)) +
        geom_boxplot() + 
        labs(title= input$phenotypetitle, x = input$phenotypexaxislabel1,
             y = input$phenotypeyaxislabel1) 
    }
    if(input$phenotypefreeyaxis == "Yes"){
      plot <- plot + facet_wrap(~Measure, scales = "free") +
        theme(axis.text.x = element_blank())
    }else{
      plot <- plot
    }
    }else {
      plot <- NULL
    }
  }else if(input$phenotypeplottype == "barplot"){
    if(input$phenotypebarplottype == "identity"){
      if(!is.null(av(input$phenotypebarplotxaxis)) && !is.null(av(input$phenotypebarplotyaxis))){
        if(!is.null(av(input$phenotypebarplotfill))){
          data1 <- data_summary(data = phenotypedata$use, varname = input$phenotypebarplotyaxis,
                               groupnames = c(input$phenotypebarplotfill,
                                              input$phenotypebarplotxaxis))
          # paste0("c(", paste(input$phenotypebarpotfill,
          #                    input$phenotypebarplotxaxis,
          #                    sep = " , "), ")")
          
          plot <- ggplot(data1, aes(x = !!as.symbol(input$phenotypebarplotxaxis), 
                                   y = !!as.symbol(input$phenotypebarplotyaxis),
                                   fill = !!as.symbol(input$phenotypebarplotfill)))
        }else {
          data1 <- EcoPLOT::data_summary(data = phenotypedata$use, varname = input$phenotypebarplotyaxis,
                               groupnames = input$phenotypebarplotxaxis)
          
          plot <- ggplot(data1, aes(x = !!as.symbol(input$phenotypebarplotxaxis), 
                                   y = !!as.symbol(input$phenotypebarplotyaxis)))
        }
        if(input$phenotypebarplotpos == "dodge"){
          plot <- plot + geom_bar(stat = "identity", position = position_dodge())
        }else {
          plot <- plot + geom_bar(stat = "identity")
        }
        if(input$phenotypebarploterror == "yes"){
          plot <- plot + geom_errorbar(aes(ymin= !!as.symbol(input$phenotypebarplotyaxis) - sd,
                                           ymax = !!as.symbol(input$phenotypebarplotyaxis) + sd),
                                       position = "dodge")
        }else{
          plot <- plot
        }
      }else {
        plot <- NULL
      }
    }else if(input$phenotypebarplottype == "bin"){
        if(!is.null(av(input$phenotypebarplotxaxis))){
          if(!is.null(av(input$phenotypebarplotfill))){
            plot <- ggplot(phenotypedata$use, aes(x = !!as.symbol(input$phenotypebarplotxaxis),
                                                    fill = !!as.symbol(input$phenotypebarplotfill)))
          }else {
            plot <- ggplot(phenotypedata$use, aes(x = !!as.symbol(input$phenotypebarplotxaxis)))
          }
          if(input$phenotypebarplotpos == "dodge"){
            plot <- plot + geom_bar(position = position_dodge())
          }else {
            plot <- plot + geom_bar()
          }
        }else {
          plot <- NULL
        }
      }
  }else {
    return(NULL)
  }
  return(plot)
})
output$phenotypeplot1 <- renderPlot({
  req(phenotypedata$table)
  phenotypeplot()
})
output$phenotypeplotmainUI <- renderUI({
  req(phenotypedata$table)
  plotOutput("phenotypeplot1")
})
output$phenotypecorrelation <- renderPrint({
  req(phenotypedata$table)
  if(input$phenotypeplottype == "scatter"){
    if(!is.null(av(input$phenotypex1)) && !is.null(av(input$phenotypey))){
      paste("Pearson's Correlation Coefficient:", cor(phenotypedata$use[[input$phenotypex1]], phenotypedata$use[[input$phenotypey]]))
    }else{
      "Pearson's Correlation Coefficient: NA"
    }
  }else{
    "Pearson's Correlation Coefficient: NA"
  }
})
output$correlationoutput <- renderUI({
  req(phenotypedata$table)
  if(input$phenotypeplottype == "scatter"){
    verbatimTextOutput("phenotypecorrelation")
  }else if(input$phenotypeplottype != "scatter"){
    NULL
  }
})

observeEvent(input$phenotypefilterrender, {
  updateRadioButtons(session, "phenotypedatasource1", "Select Dataset to Use:",
                     choices = c("Original",
                                 "Filtered"),
                     selected = "Original", inline = TRUE)
})
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
#parametric
output$phenotypeparametricUI <- renderUI({
  req(phenotypedata$table)
  output <- tagList(
    selectInput("phenotypeparametrictesttype", "Select Parametric Test",
                choices = c("T-Test" = "ttest",
                            "One-Way ANOVA" = "1anova",
                            "Two-Way ANOVA" = "2anova",
                            "Tukey HSD" = "tukeyhsd"),
                selected = "ttest")
    ,
    selectInput("phenotypeparametricvar1", "Select Continuous Variable:",
                choices = c("NULL", names(dplyr::select_if(phenotypedata$use, is.numeric))),
                selected = "NULL")
    ,
    selectInput("phenotypeparametricvar2", "Select Categorical Variable:",
                choices = c("NULL", names(dplyr::select_if(phenotypedata$table, is.character)),
                            names(dplyr::select_if(phenotypedata$use, is.factor))),
                selected = "NULL")
    ,
    conditionalPanel(condition = "input.phenotypeparametrictesttype == '2anova' || 
                     input.phenotypeparametrictesttype == 'tukeyhsd'",
                     selectInput("phenotypeparametricvar3", "Select Categorical Variable:",
                                 choices = c("NULL", names(dplyr::select_if(phenotypedata$table, is.character)),
                                             names(dplyr::select_if(phenotypedata$use, is.factor))),
                                 selected = "NULL"))
  )
  return(output)
})
output$phenotypeparametricwork <- renderPrint({
  req(phenotypedata$table)
  if(input$phenotypeparametrictesttype == "ttest"){
    if(!is.null(av(input$phenotypeparametricvar1)) && !is.null(av(input$phenotypeparametricvar2))){
      t.test(phenotypedata$table[[input$phenotypeparametricvar1]] ~ phenotypedata$table[[input$phenotypeparametricvar2]])
    }else if(!is.null(av(input$phenotypeparametricvar1)) && is.null(av(input$phenotypeparametricvar2))){
      t.test(phenotypedata$table[[input$phenotypeparametricvar1]])
    }else {
      NULL
    }
  }else if(input$phenotypeparametrictesttype == "1anova"){
    if(!is.null(av(input$phenotypeparametricvar1)) && !is.null(av(input$phenotypeparametricvar2))){
      summary(aov(phenotypedata$use[[input$phenotypeparametricvar1]] ~ phenotypedata$use[[input$phenotypeparametricvar2]], data = phenotypedata$use))
      }
  }else if(input$phenotypeparametrictesttype == "2anova"){
    if(!is.null(av(input$phenotypeparametricvar1)) && !is.null(av(input$phenotypeparametricvar2)) && !is.null(av(input$phenotypeparametricvar3))){
      summary(aov(phenotypedata$use[[input$phenotypeparametricvar1]] ~ phenotypedata$use[[input$phenotypeparametricvar2]] + phenotypedata$use[[input$phenotypeparametricvar3]],
                  data = phenotypedata$use))
      }
  }else if(input$phenotypeparametrictesttype == "tukeyhsd"){
    if(!is.null(av(input$phenotypeparametricvar1)) && !is.null(av(input$phenotypeparametricvar2))){
      if(!is.null(av(input$phenotypeparametricvar3))){
        TukeyHSD(aov(phenotypedata$use[[input$phenotypeparametricvar1]] ~ phenotypedata$use[[input$phenotypeparametricvar2]] + phenotypedata$use[[input$phenotypeparametricvar3]], data = phenotypedata$use))
      }else {
        TukeyHSD(aov(phenotypedata$use[[input$phenotypeparametricvar1]] ~ phenotypedata$use[[input$phenotypeparametricvar2]], data = phenotypedata$use))
        }
      }
  }
})
output$phenotypeparametricMain <- renderUI({
  output <- tagList(
    verbatimTextOutput("phenotypeparametricwork")
  )
})
#non parametric
output$phenotypenonparametricUI <- renderUI({
  req(phenotypedata$table)
  output <- tagList(
    selectInput("phenotypenonparametrictesttype", "Select Non-Parametric Test",
                choices = c("Kruskal Wallis" = "kw"))
    ,
    selectInput("phenotypenonparametricvar1", "Select Continuous Variable:",
                choices = c("NULL", names(dplyr::select_if(phenotypedata$use, is.numeric))),
                selected = "NULL")
    ,
    selectInput("phenotypenonparametricvar2", "Select Grouping Variable:",
                choices = c("NULL", names(dplyr::select_if(phenotypedata$use, is.character)),
                            names(dplyr::select_if(phenotypedata$use, is.factor))),
                selected = "NULL")
  )
  return(output)
})
output$phenotypenonparametricwork <- renderPrint({
  req(phenotypedata$table)
  if(input$phenotypenonparametrictesttype == "kw"){
    if(!is.null(av(input$phenotypenonparametricvar1)) && !is.null(av(input$phenotypenonparametricvar2))){
      kruskal.test(phenotypedata$use[[input$phenotypenonparametricvar1]], phenotypedata$use[[input$phenotypenonparametricvar2]])
    }
  }else {
    NULL
  }
})
output$phenotypenonparametricMain <- renderUI({
  output <- tagList(
    verbatimTextOutput("phenotypenonparametricwork")
  )
})

# #MICROBIOME ----------------------------------------------------------
# #saves uploaded file as a dataframe
# myData <- reactive({
#   inFile <- input$file1
#   if (is.null(inFile)) return(NULL)
#   read.csv(inFile$datapath, header = input$header)
# })
# 
# #Create taxonomic datasets based on uploaded CSV File
# species <- reactive({
#   if(is.null(myData())){return()}
#   s <- myData() %>% select(sampleID, Depth, Treatment, Compartment, 
#                                Time_Point, Species, count)
# })
# genus <- reactive({
#   if(is.null(myData())){return()}
#   g <- myData() %>% select(sampleID, Depth, Treatment, 
#                                Compartment, Time_Point, Genus, count) 
# })
# family <- reactive({
# if(is.null(myData())){return()}
#   f <- myData() %>% select(sampleID, Depth, Treatment, 
#                            Compartment, Time_Point, Family, count) 
# })
# order <- reactive({
# if(is.null(myData())){return()}
#   o <- myData() %>% select(sampleID, Depth, Treatment, 
#                            Compartment, Time_Point, Order, count) 
# })
# class <- reactive({
#   if(is.null(myData())){return()}
#   c <- myData() %>% select(sampleID, Depth, Treatment, 
#                            Compartment, Time_Point, Class, count) 
# })
# phylum <- reactive({
#   if(is.null(myData())){return()}
#   p <- myData() %>% select(sampleID, Depth, Treatment, 
#                            Compartment, Time_Point, Phylum, count) 
# })
# kingdom <- reactive({
#   if(is.null(myData())){return()}
#   k <- myData() %>% select(sampleID, Depth, Treatment, 
#                                Compartment, Time_Point, Kingdom, count) 
# })
# 
# #creates tables for new datasets
# output$contents <- renderTable({
#   if(is.null(myData())){return()}
#   head(myData())
# })
# output$species <- renderTable({
#   if(is.null(myData())){return()}
#   head(species())
# })
# output$genus <- renderTable({
#   if(is.null(myData())){return()}
#  head(genus())
# })
# output$family <- renderTable({
#   if(is.null(myData())){return()}
#   head(family())
# })
# output$order <- renderTable({
#   head(order())
# })
# output$class <- renderTable({
#   if(is.null(myData())){return()}
#   head(class())
# })
# output$phylum <- renderTable({
#   head(phylum())
# })
# output$kingdom <- renderTable({
#   head(kingdom())
# })
# 
# #creates Dynamic UI for tables
# output$tabs <- renderUI({
#   if(is.null(myData()))
#     tags$h1("Please Make Sure Your Data is Properly Formatted Before Uploading!") 
#   else
#     tabsetPanel(
#       tabPanel("File", splitLayout(tableOutput("contents"))),
#       tabPanel("Species", splitLayout(tableOutput("species"))),
#       tabPanel("Genus", splitLayout(tableOutput("genus"))),
#       tabPanel("Family", splitLayout(tableOutput("family"))),
#       tabPanel("Order", splitLayout(tableOutput("order"))),
#       tabPanel("Class", splitLayout(tableOutput("class"))),
#       tabPanel("Phylum", splitLayout(tableOutput("phylum"))),
#       tabPanel("Kingdom", splitLayout(tableOutput("kingdom"))))
# })
# 
# #allows user to choose what taxonomic level they want to plot
# datasetInput <- reactive({
#   if (is.null(myData())) return(NULL)
#   switch(input$data,
#          "Kingdom" = kingdom(),
#          "Phylum"= phylum(),
#          "Class" = class(),
#          "Order" = order(),
#          "Family" = family(),
#          "Genus" = genus(), 
#          "Species" = species())
# })
# 
# nosampleID <- reactive({
#   if (is.null(myData())) return(NULL)
#   select(datasetInput(), -"sampleID") 
# })
# 
# taxonomy <- renderText({
#   if (is.null(myData())) return(NULL)
#   colnames(nosampleID()[5])
# })
# 
# #creates dynamic UI for column variables 
# output$treatment <- renderUI({
#   if (is.null(myData())) return(NULL)
#   selectInput("treatment", "Choose Treatment(s) to Show:", 
#               choices = levels(datasetInput()$Treatment), 
#               selected = "",
#               multiple = TRUE)
# })
# output$timepoint <- renderUI({
#   if (is.null(myData())) return(NULL)
#   selectInput("timepoint", "Choose Timepoint(s) to Show:", 
#               choices = levels(datasetInput()$Time_Point), 
#               selected = "",
#               multiple = TRUE)
# })
# output$compartment <- renderUI({
#   if (is.null(myData())) return(NULL)
# selectInput("compartment", "Choose Compartment(s) to Show:", 
#               choices = levels(datasetInput()$Compartment), 
#               selected = "",
#               multiple = TRUE)
# })
# output$depth <- renderUI({
#   if (is.null(myData())) return(NULL)
#   selectInput("depth", "Choose Depth(s) to Show:", 
#               choices = levels(datasetInput()$Depth), 
#               selected = "",
#               multiple = TRUE)
# })
# output$threshold <- renderUI({
#   if (is.null(myData())) return(NULL)
#   numericInput("threshold", "Choose minimum ASV Value for Table:",
#                value = 0)
# })
# 
# #allows user to download filtered dataset
# output$downloaddata <- downloadHandler(
#   filename = function() {"nameyourfile.csv"},
#   content = function(file){
#     write.csv(filtered(), file)
# })
# 
# #allows user to plot what they want graphed on the x axis and if they want to facet
# output$xaxis <- renderUI({
#   if (is.null(myData())) return(NULL)
#   selectInput("xaxis", "Select What To Graph Along X-Axis:",
#               choices = names(grouped()),
#               selected = "", 
#               multiple = FALSE)
# })
# output$facet <- renderUI({
#   if (is.null(myData())) return(NULL)
#   selectInput("facet", "Select What Variable to Facet Around:",
#               choices = names(grouped()),
#               selected = "",
#               multiple = TRUE)
# })
# 
# #filters dataset and aggregates rows based on user input
# filtered <- eventReactive(input$render, {
#   if (is.null(myData())) return(NULL)
#   nosampleID() %>% 
#    # mutate(nosampleID()[5] %>% if_else(count <= input$threshold(),
#    #                                    "unassigned", "4")) %>%
#    filter( Treatment %in% input$treatment & Time_Point %in% input$timepoint &
#                      Compartment %in% input$compartment & Depth %in% input$depth)  
# })
# grouped <- reactive({
#   if (is.null(myData())) return(NULL)
#   aggregate(filtered()[6], by = filtered()[1:5], sum)
# })
# finalfilter <- reactive({
#   if (is.null(input$facet))
#     grouped() %>% select(input$xaxis, taxonomy(), count)
#     else
#       grouped() %>% select(input$xaxis, input$facet, taxonomy(), count)
# })
# finalfinalfilter <- reactive({
#   if(is.null(input$facet))
#     aggregate(finalfilter()[3], by = finalfilter()[1:2], sum)
#   else
#     aggregate(finalfilter()[4], by = finalfilter()[1:3], sum)
# })
# output$Filter <- renderTable({
#   if (is.null(myData())) return(NULL)
#   head(grouped())
# })
# output$finalfilter <- renderTable({
#   if (is.null(myData())) return(NULL)
#   head(finalfinalfilter())
# })
# 
# #output plot based on user input
# output$OTUplot <- renderPlotly({
#   if (is.null(myData())) return(NULL)
#   ggplot(finalfinalfilter(), aes_string(x = input$xaxis, fill = taxonomy())) + 
#     geom_bar(aes(y = count), stat = "identity", 
#                    position = "fill", colour = "black",
#                    show.legend = TRUE) + ggtitle(input$title5) + 
#     labs(x = input$xaxis5, y = input$yaxis5, fill = taxonomy()) +
#     facet_wrap(input$facet) + theme_bw()
# })

#MICROBIOME 2-----
#file upload----
output$fileformatoptions <- renderUI({
  radioButtons("fileformat", "How is Your Data Formatted?",
               choices = c("QIIME1 (.biom)"= "qiime1",
                           "QIIME2 (.qza)" = "qiime2",
                           #"BIOM (.biom)" = "biom",
                           "No Format (.csv/.txt/.tsv)" = "none"),
               selected = "none")
})
output$fileuploadoptions <- renderUI({
  if(is.null(input$fileformat))return(NULL)
  if(input$fileformat == "none"){
    output <- tagList(
      tags$hr(),
      fileInput("otufile1", "Upload OTU File:",
                multiple = FALSE,
                accept = c(
                  ".csv",
                  ".txt",
                  ".tsv",
                  "text/csv",
                  "text/comma-separated-values,text/plain"
                )),
      fileInput("taxfile1", "Upload Taxonomy File:",
                multiple = FALSE,
                accept = c(
                  ".csv",
                  "text/csv",
                  "text/comma-separated-values,text/plain"
                )),
      fileInput("mappingfile1", "Upload Mapping File:",
                multiple = FALSE,
                accept = c(
                  ".csv",
                  "text/csv",
                  "text/comma-separated-values,text/plain"
                )),
      hr(),
      tags$div(tags$h5(tags$b("NOTE:"), "EcoPLOT only accepts", tags$b("Newick"),
                       "and", tags$b("Nexus"),"file formats for trees"),
               align = "center"),
      fileInput("treefile1", "Upload Tree File (optional):",
                multiple = FALSE,
                accept = c(
                  ".newick",".nex", ".nxs", ".tree"
                ))
    )
  }else if(input$fileformat == "qiime2"){
    output <- tagList(
      fileInput("otufile1", "Upload OTU File:",
                multiple = FALSE,
                accept = c(
                  ".qza"
                )),
      fileInput("taxfile1", "Upload Taxonomy File:",
                multiple = FALSE,
                accept = c(
                  ".qza")),
      fileInput("mappingfile1", "Upload Mapping File (as TSV):",
                multiple = FALSE,
                accept = c(
                  ".tsv", 
                  "text/csv",
                  "text/comma-separated-values,text/plain")),
      hr(),
      tags$div(tags$h5(tags$b("NOTE:"), "EcoPLOT only accepts", tags$b("Newick"),
                       "and", tags$b("Nexus"),"file formats for trees"),
               align = "center"),
      fileInput("treefile1", "Upload Tree File (optional):",
                multiple = FALSE,
                accept = c(
                  ".newick",".nex", ".nxs", ".tree"
                ))
    )
  }else if(input$fileformat == "qiime1"){
    output <- tagList(
      fileInput("otufile1", "Upload OTU File (.biom):",
                multiple = FALSE,
                accept = c(
                  ".biom"
                )),
      radioButtons("mappingformat", "How is your Mapping File Formatted?",
                   choices = c("Tab" = "\t",
                               "Comma" = ",",
                               "Semicolon" = ";"),
                   selected = "\t"),
      fileInput("mappingfile1", "Upload Mapping File:",
                multiple = FALSE,
                accept = c(
                  ".tsv",
                  ".csv",
                  "text/csv",
                  "text/comma-separated-values,text/plain"
                  )),
      tags$div(tags$h5(tags$b("NOTE:"), "EcoPLOT only accepts", tags$b("Newick"),
                                       "and", tags$b("Nexus"),"file formats for trees"),
               align = "center"),
      hr(),
      fileInput("treefile1", "Upload Tree File (optional):",
                multiple = FALSE,
                accept = c(
                  ".newick", ".nex", ".nxs", ".tree"
                ))
    )
  }
  return(output)
})

otufile <- reactive({
  inFile <- input$otufile1
  if (is.null(inFile)) return(NULL)
  
  if(input$fileformat == "none"){
    obj <- read.csv(file = inFile$datapath)
    file <- obj %>% 
      mutate_if(is.numeric, round)
    row.names(file) <- obj$otu_id
    file <- file %>% select(-1) %>% data.matrix(file)
    file <- otu_table(file, taxa_are_rows = TRUE)
  }else if(input$fileformat == "qiime2"){
    file <- inFile$datapath
  }else if(input$fileformat == "qiime1"){
    file <- import_biom(BIOMfilename = inFile$datapath)
  }
  return(file)

})
taxonomyfile <- reactive({
  inFile <- input$taxfile1
  if (is.null(inFile)) return(NULL)
  
  if(input$fileformat == "none"){
    obj <- read.csv(file = inFile$datapath)
    file <- obj
    row.names(file) <- obj$otu_id
    file <- file %>% select(-1)
    file <- as.matrix(file)
    file <- tax_table(file) 
  }else if(input$fileformat == "qiime2"){
    file <- inFile$datapath
  }else if(input$fileformat == "qiime1"){
    #file <- import_biom(BIOMfilename = inFile$datapath)
    NULL
  }
  return(file)
})
mappingfile <- reactive({
  inFile <- input$mappingfile1
  if (is.null(inFile)) return(NULL)
  if(input$fileformat == "none"){
    obj <- read.csv(file = inFile$datapath) %>% na.omit()
    file <- sample_data(obj)
    row.names(file) <- obj$sampleID
  }else if(input$fileformat == "qiime2"){
    file <- inFile$datapath
  }else if(input$fileformat == "qiime1"){
    obj <- read.csv(file = inFile$datapath,
                     sep = input$mappingformat,
                     row.names = 1)
    file <- sample_data(obj)
  }
  return(file)
})
phylotree <- reactive({
  inFile <- input$treefile1
  if (is.null(inFile)) return(NULL)
  withProgress(message = "Reading Tree File",
               detail = "This may take a while...", {
                 tree <- read_tree(inFile$datapath)
               })
  return(tree)
})
output$phyloaction <- renderUI({
  actionButton("makefile", "Upload", width = "100%")
})
###Make initial phyloseq object ------
phyloseqobj <- eventReactive(input$makefile, {
  withProgress(message = "Reading Files",
               detail = "This may take a while...", {
  if(!is.null(phylotree())){
    if(input$fileformat == "none"){
      file <- phyloseq(otufile(), taxonomyfile(), mappingfile(), phylotree())
    } else if(input$fileformat == "qiime2"){
      file <- qza_to_phyloseq(features = otufile(), 
                              taxonomy = taxonomyfile(), 
                              metadata = mappingfile(), 
                              tree = phylotree())
    } else if(input$fileformat == "qiime1"){
      file <- merge_phyloseq(otufile(), mappingfile(), phylotree())
    }
  }else if(is.null(phylotree())){
    if(input$fileformat == "none"){
      file <- phyloseq(otufile(), taxonomyfile(), mappingfile())
    } else if(input$fileformat == "qiime2"){
      file <- qza_to_phyloseq(features = otufile(), 
                              taxonomy = taxonomyfile(), 
                              metadata = mappingfile())
    } else if(input$fileformat == "qiime1"){
      file <- merge_phyloseq(otufile(), mappingfile())
    }
  }
               })
  return(file)
})
output$phyloseqprint <- renderPrint({
  if(is.null(phyloseqobj()))return(NULL)
  phyloseqobj()
})
## Produce Initial Tables -----
output$otutable <- renderDataTable({
  if(is.null(phyloseqobj()))return(NULL)
  withProgress(message = "Rendering OTU Table:", {
  otu_table(phyloseqobj())
               })
})
output$otutableoutput <- renderUI({
  if(input$makefile == 0){
    tags$h3("Please Upload Files")
  }else {
    splitLayout(dataTableOutput("otutable"))
  }
})
output$taxtable <- renderDataTable({
  if(is.null(phyloseqobj()))return(NULL)
  withProgress(message = "Rendering Taxonomy Table:", {
  tax_table(phyloseqobj())
               })
})
output$taxtableoutput <- renderUI({
  if(input$makefile == 0){
    tags$h3("Please Upload Files")
  }else{
    splitLayout(dataTableOutput("taxtable"))
  }
})
output$mappingtableoutput <- renderDataTable({
  if(is.null(phyloseqobj()))return(NULL)
  withProgress(message = "Rendering Mapping Table:", {
    sample_data(phyloseqobj())
  })
})
output$mappingtableoutputdisplay <- renderUI({
  if(input$makefile == 0){
    tags$h3("Please Upload Files")
  }else{
    splitLayout(dataTableOutput("mappingtableoutput"))
  }
})
treedf <- reactive({
  if(is.null(phylotree()))return(NULL)
  tibble::as_tibble(phylotree())
})
output$treedftable <- renderDataTable({
  if(is.null(treedf())) return(NULL)
  treedf()
})
output$treedftableoutput <- renderUI({
  if(is.null(treedf())){
    output <- tags$h3("No Tree File Uploaded")
  }else {
    output <- tagList(
      splitLayout(dataTableOutput("treedftable"))
      ,
      tags$h3("To View Your Uploaded Tree Graphically, Proceed to 
              the Community Composition Tab")
    )
  }
  return(output)
})
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

#create tree plot-----
output$treeoptions <- renderUI({
  if(is.null(phy_tree(datasetbpuse()))){
    output <- tags$h3("Phylogenetic Tree Required")
  }else{
    withProgress(message = "Creating Plot Options",
                 detail = "This may take a while...", {
  output <- tagList(
  selectInput("treestyle1", "Select Tree Style:",
              choices = c("Rectangular" = "rectangular",
                          "Slanted" = "slanted",
                          "Fan" = "fan",
                          "Circular" = "circular",
                          "Radial" = "radial",
                          "Equal Angle" = "equal_angle",
                          "Daylight" = "daylight"), 
              selected = "rectangular")
  ,
  selectInput("treenode1", "Select Node to View:",
              choices = treeuse() %>% 
    select(label) %>% 
    arrange(label) %>% 
    pull(label))
,
  numericInput("treetaxonomy1", "Select Ancestral Levels:",
               min = 1,
               value = 10)
,
  numericInput("phylotreeplotheight1", "Select Height of Plot:",
               value = 1250)
,
  numericInput("phylotreelabelsize1", "Select Label Size:",
               min = 2,
               value = 3)
,
  actionButton("phylotreeplotrender1", "Render Tree Plot:")
,
hr()
,
  EcoPLOT::downloadPlotUI(id = "phylotreeplotdownload")
)
                 })
  }
  return(output)
})
treeuse <- reactive({
  if(is.null(phyloseqobj()))return(NULL)
  switch(input$bpdataset, 
         "original" = treedf(),
         "filtered" = updatedtreedf())
  
})
phylotreeplotrender <- eventReactive(input$phylotreeplotrender1, {
  if(is.null(phy_tree(datasetbpuse())))return(NULL)
  withProgress(message = "Making Tree",
               detail = "This may take a while...", {
                 phylotreesubset <- tree_subset(phy_tree(datasetbpuse()), 
                                                node = input$treenode1, 
                                                levels_back = input$treetaxonomy1)
                 if (isS4(phylotreesubset)) {
                   labels <- phylotreesubset@phylo$tip.label
                 } else {
                   labels <- phylotreesubset$tip.label
                 }
                 
                 labels_df <- tibble(
                   label = labels,
                   genus = str_extract(label, "[^;]+;[^;]+$") %>% str_replace(";[^;]+$", ""),
                   species = str_extract(label, "[^;]+$")
                 )  %>% 
                   mutate(
                     species = if_else(is.na(genus), "", str_replace(species, "s__", "")),
                     genus = if_else(is.na(genus), label, str_replace(genus, "g__", ""))
                   )
                 
                 ggtree(phylotreesubset, layout = input$treestyle1) %<+% labels_df +
                   geom_tiplab(aes(label = paste(genus, species)),
                               size = input$phylotreelabelsize1)
                 
               })
})

output$phylotreeplot <- renderPlot({
  req(input$phylotreeplotrender1)
  if(is.null(phylotree()))return(NULL)
  isolate(phylotreeplotrender())
})
output$phylotreeplotui <- renderUI({
  req(input$phylotreeplotrender1)
  if(is.null(phylotree()))return(NULL)
  isolate(plotOutput("phylotreeplot", height = input$phylotreeplotheight1))
})
EcoPLOT::downloadPlot(id = "phylotreeplotdownload", plotid = phylotreeplotrender())

counthistogram <- eventReactive(input$rendercounthist, {
  if(input$histogramdisplay1 == "asv"){
    taxasums1 <- as.data.frame(taxa_sums(phyloseqobj()))
    taxasums1$count <- taxasums1$`taxa_sums(phyloseqobj())`
    taxasums1$asv_id <- rownames(taxasums1)
    return(taxasums1)
  }else if(input$histogramdisplay1 == "sample"){
    objsums1 <- as.data.frame(sample_sums(phyloseqobj()))
    objsums1$count <- objsums1$`sample_sums(phyloseqobj())`
    objsums1$sampleID <- rownames(objsums1)
    return(objsums1)
  }
})
output$counthistsummary <- renderUI({
  if(is.null(counthistogram()))return(NULL)
  avg <- paste("Average Number of Counts:", mean(counthistogram()[["count"]]))
  min <- paste("Minimum number of counts:", min(counthistogram()[["count"]]))
  max <- paste("Maximum Number of Counts:", max(counthistogram()[["count"]]))
  HTML(paste(avg, min, max, sep = '<br/>'))
})
##view stacked barplot of data---- 
#render UI options for barplot
output$bpdatasetchoice <- renderUI({
  radioButtons("bpdataset", "Select Dataset to Use:",
               choices = c("Filtered" = "filtered",
                           "Original" = "original"),
               selected = "original")
})

datasetbpuse <- reactive({
  if(is.null(phyloseqobj()))return(NULL)
  switch(input$bpdataset, 
         "original" = phyloseqobj(),
         "filtered" = updatedphyloseq())
})

output$bpplotui <- renderUI({
  if(is.null(datasetbpuse()))return(NULL)

  data <- c(as.list(unique(sample_variables(datasetbpuse()))))
  names <- list("NULL"="NULL")
  names <- c(names, as.list(rank_names(datasetbpuse())))
  
  output <- tagList(
    selectInput("bptaxrank1", "Select Taxonomic Rank to Depict:",
                 choices = c(names),
                selected = "NULL",
                multiple = FALSE)
    ,
    numericInput("bptaxthreshold1", label = "Filter Out Low Abundance Taxa (min percent):",
                 value = 2, min = 0, max = 100, step = 1)
    ,
    selectInput("bpfacetoption", "Select Variable to Facet Around:",
                   choices = c("NULL", data),
                   selected = "NULL",
                   multiple = FALSE)
    ,
    conditionalPanel(condition = "input.bpfacetoption != 'NULL'",
                     selectInput("bpfacetoption2", "Select Second Variable to Facet Around:",
                                 choices = c("NULL", data),
                                 selected = "NULL",
                                 multiple = FALSE))
    ,
    sliderInput("bpaxisangle1", "Select Angle of X Axis Text:",
                min = 0, max = 90, value = 45, step = 5)
    ,
    numericInput("bpaxistextsize", "Select Size of X Axis Text:",
                 min = 3, max = 20, value = 5)
    ,
    numericInput("bpxaxislabelsize", "Select Size of X Axis Label:",
                 min = 3, max = 30, value = 10)
    ,
    sliderInput("bpyaxisangle1", "Select Angle of Y Axis Text:",
                min = 0, max = 90, value = 45, step = 5)
    ,
    numericInput("bpyaxistextsize", "Select Size of Y Axis Text:",
                 min = 3, max = 20, value = 5)
    ,
    numericInput("bpyaxislabelsize", "Select Size of Y Axis Label:",
                 min = 3, max = 30, value = 10)
    ,
    actionButton("bptaxrender1", "Make Barplot", width = '100%')
  )
  return(output)
})

barplotfilter <- eventReactive(input$bptaxrender1,{
  if(is.null(datasetbpuse()))return(NULL)
  withProgress(message = "Applying Filters:", {
      glom1 <- tax_glom(datasetbpuse(), taxrank = input$bptaxrank1) %>% 
        transform_sample_counts(function(x) {x/sum(x)})
      glomdata1 <- psmelt(glom1)
      glomdata1[[input$bptaxrank1]] <- as.character(glomdata1[[input$bptaxrank1]])
      #glomdata1[["sampleID"]] <- as.character(glomdata1[["sampleID"]])
      glomdata1[[input$bptaxrank1]][glomdata1$Abundance < (input$bptaxthreshold1*0.01)] <- "Low Abundance"
      glomdata1[[input$bptaxrank1]] <- as.factor(glomdata1[[input$bptaxrank1]])
      legend_order <- levels(glomdata1[[input$bptaxrank1]])
      LA_pos <- match("Low Abundance", legend_order)
      legend_order <- legend_order[-LA_pos]
      glomdata1[[input$bptaxrank1]] <- factor(glomdata1[[input$bptaxrank1]], levels = c(legend_order, "Low Abundance"))
      
      dataset <- glomdata1
  })
  return(dataset)
})
barplotplot1 <- reactive({
  if(is.null(barplotfilter()))return(NULL)
  withProgress(message = "Making Barplot:",
               detail = "This may take a while...", {
  plot <-  ggplot(data = barplotfilter()) + 
    geom_bar(aes(x = Sample, y = Abundance, fill = !!as.symbol(input$bptaxrank1)), stat = "identity", position = "stack") + #theme(axis.text.x = element_blank()) + 
    labs(x = "Samples", y = "Abundance", #fill = input$bptaxrank1,
         title = paste(input$bptaxrank1, "Community Composition")) +
    theme(legend.position= "right", axis.text.x = element_text(color = "black", size = isolate(input$bpaxistextsize), 
                                                               angle = isolate(input$bpaxisangle1)),
                                    axis.text.y = element_text(color = "black", size = input$bpyaxistextsize,
                                                                angle = input$bpyaxisangle1),
          axis.title.x = element_text(size = input$bpxaxislabelsize), axis.title.y = element_text(size = input$bpyaxislabelsize),
          legend.text = element_text(face = "italic"))
  
  if(!is.null(av(input$bpfacetoption)) && is.null(av(input$bpfacetoption2))){
    plot <- plot + facet_grid(paste("~", paste(input$bpfacetoption)), scales = "free_x", drop = TRUE)
  }else if(!is.null(av(input$bpfacetoption)) && !is.null(av(input$bpfacetoption2))){
    plot <- plot + facet_grid(paste(input$bpfacetoption, "~", paste(input$bpfacetoption2)), scales = "free_x", drop = TRUE)
  }else if(is.null(av(input$bpfacetoption)) && is.null(input$bpfacetoption2)){
    plot <- plot
  }
    })
  return(plot)
})

output$barplotplot <- renderPlot({
  barplotplot1()
})
output$stackedbarplotgraph <- renderUI({
  if(is.null(datasetbpuse())) return(NULL)
  plotOutput("barplotplot")
})
downloadPlot(id = "barplotdownload", plotid = barplotplot1())

#alpha div using phyloseq options-----
output$alphadivoptions <- renderUI({
  if(is.null(alphadatasetuse()))return(NULL)
  output <- tagList(
    tags$div(tags$h4("You are Viewing the", paste(input$alphadataset), "Dataset"),
             align = "center")
    ,
    selectInput("phyloseqalphaoptions1", "Select Alpha Diversity Methods to Visualize:",
                choices = c("Observed ASVs" = "Observed",
                            "Chao1" = "Chao1",
                            "ACE" = "ACE", 
                            "Shannon" = "Shannon", 
                            "Simpson" = "Simpson", 
                            "InvSimpson" = "InvSimpson"),
                multiple = TRUE)
    ,
    # radioButtons("phyloseqplottype1", "Select Plot Type:",
    #              choices = c("Box Plot" = "box",
    #                          "Scatter Plot" = "scatter"))
    # ,
    selectInput("phyloxaxis", "Select Which Factor to Compare on the X-Axis:",
                choices = as.list(sample_variables(phyloseqobj())),
                multiple = FALSE)
    ,
    selectInput("phylocolor", "Select What Factor to Color:",
                choices = c( "NULL", as.list(sample_variables(phyloseqobj()))),
                multiple = FALSE,
                selected = "NULL")
    ,
    tags$div(tags$h5(tags$b("Note:"),"If multiple methods selected, use 'variable' option to maintain separation."), align = "center")
    ,
    selectInput("phylofacet", "Select Facet Option:",
                choices = c("NULL", "variable",as.list(sample_variables(phyloseqobj()))),
                multiple = FALSE,
                selected = "NULL")
    ,
    conditionalPanel(condition = "input.phylofacet != 'NULL'",
    selectInput("phylofacet2", "Select Second Facet Option:",
                choices = c("NULL", "variable", as.list(sample_variables(phyloseqobj()))),
                multiple = FALSE,
                selected = "NULL"))
    ,
    textInput(inputId = "alphaphyloseqxaxis1", label = "Create X axis Label",
              placeholder = "X Axis")
    ,
    numericInput("alphaphyloseqxaxislabelsize", "Select Size of X Axis Label Text",
                 value = 15, min = 3, max = 30)
    ,
    sliderInput(inputId = "alphaphyloseqxaxisangle", label = "Select Angle of X Axis Text",
                min = 0, max = 90, value = 0, step = 5)
    ,
    numericInput("alphaphyloseqxaxistextsize", label = "Select Size of X Axis Text",
                 value = 10, min = 3, max = 30)
    ,
    textInput(inputId = "alphaphyloseqyaxis1", label = "Create Y axis Label",
              placeholder = "Y Axis")
    ,
    numericInput("alphaphyloseqyaxislabelsize", "Select Size of Y Axis Label Text",
                 value = 15, min = 3, max = 30)
    ,
    sliderInput(inputId = "alphaphyloseqyaxisangle", label = "Select Angle of Y Axis Text",
                min = 0, max = 90, value = 0, step = 5)
    ,
    numericInput("alphaphyloseqayaxistextsize", label = "Select Size of Y Axis Text",
                 value = 10, min = 3, max = 30)
    ,
    actionButton("phyloseqplotrender1", "Render Plot", width = "100%")
  )
  return(output)
})
##Alpha Diversity stats table and mann whitney
phyloseqalpharichness <- eventReactive(input$renderalphastattable, {
  if(is.null(phyloseqobj()))return(NULL)
  withProgress(message = "Performing Diversity Metrics",
               detail = "This may take a while...", {
  phyloseq::estimate_richness(alphadatasetuse())
               })
})
output$phyloseqalphatable <- renderDataTable({
  phyloseqalpharichness()
})
output$phyloseqalphatableui <- renderUI({
  validate(
    need(input$makefile, message =  "Please Upload Files"),
    need(input$renderalphastattable, message =  "Table Will Appear Here")
  )
  splitLayout(dataTableOutput("phyloseqalphatable"))
})
downloadTable(id = "alphadiversitystattable",tableid = phyloseqalpharichness())
output$alphadivdatasetoption <- renderUI({
  radioButtons("alphadataset", "Select Dataset to Use:",
               choices = c("Filtered" = "filtered",
                           "Original" = "original"),
               selected = "original")
})
alphadatasetuse <- reactive({
  if(is.null(phyloseqobj()))return(NULL)
  switch(input$alphadataset,
         "filtered" = updatedphyloseq(),
         "original" = phyloseqobj())
})
output$alphadivstatoptions <- renderUI({
  if(is.null(phyloseqobj()))return(NULL)
  output <- tagList(
    actionButton("renderalphastattable", "Make Table")
    ,
    tags$div(tags$h5(tags$b("Note:"), "This will produce a table of standard alpha diversity 
            estimates."), align = "center")
    ,
    hr()
    ,
    conditionalPanel(condition = "input.renderalphastattable",
    selectInput("alphastatoptions", "Select Alpha Diversity Methods to Compare:",
                choices = c("Observed ASVs" = "Observed",
                            "Chao1" = "Chao1",
                            "ACE" = "ACE", 
                            "Shannon" = "Shannon", 
                            "Simpson" = "Simpson", 
                            "InvSimpson" = "InvSimpson"),
                multiple = FALSE)
    ,
    actionButton("performalphastats", "Perform Statistics"))
  )
  return(output)
})
alphadivstatresult <- eventReactive(input$performalphastats, {
  if(is.null(phyloseqobj()))return(NULL)
  alphastats <- list()
  for(i in sample_variables(alphadatasetuse())[!grepl(pattern = "ID",x = sample_variables(alphadatasetuse()))]){  
    alphastats[[i]] <- pairwise.wilcox.test(phyloseqalpharichness()[[input$alphastatoptions]], sample_data(alphadatasetuse())[[i]], p.adjust.method = "bonf")
  }
  alphastats
})
output$alphadivstatprint <- renderPrint({
  if(is.null(phyloseqobj()))return(NULL)
  alphadivstatresult()
})
###Alpha Diversity Plot 
phyloseqplot <- reactive({
  # validate(
  #   need(input$makefile, message =  "Please Upload Files"),
  #   need(input$renderalphastattable, message =  "Table Will Appear Here")
  # )
  req(input$phyloseqplotrender1)
  if(is.null(alphadatasetuse()))return(NULL)
  withProgress(message = "Making Plot",
               detail = "This may take a while...", {
  if(is.null(av(input$phylocolor))){
    plot <- phyloseq::plot_richness(alphadatasetuse(), x = input$phyloxaxis,
                                    measures = input$phyloseqalphaoptions1,
                                    scales = "free_y")
    plot$layers <- plot$layers[-1]
  }else {
    plot <- phyloseq::plot_richness(alphadatasetuse(), color = input$phylocolor, x = input$phyloxaxis,
                                    measures = input$phyloseqalphaoptions1,
                                    scales = "free_y")
    plot$layers <- plot$layers[-1]
  }
  #if(input$phyloseqplottype1 == "box"){
    plot <- plot + geom_boxplot() +
      labs(x = paste(input$alphaphyloseqxaxis1), y = paste(input$alphaphyloseqyaxis1),
           title = "Alpha Diversity") +
      theme(legend.position= "right", axis.text.x = element_text(color = "black", size = isolate(input$alphaphyloseqxaxistextsize), 
                                                                 angle = isolate(input$alphaphyloseqxaxisangle)),
            axis.text.y = element_text(color = "black", size = input$alphaphyloseqyaxistextsize,
                                       angle = input$alphaphyloseqyaxisangle),
            axis.title.x = element_text(size = input$alphaphyloseqxaxislabelsize), axis.title.y = element_text(size = input$alphaphyloseqyaxislabelsize))
    if(!is.null(av(input$phylofacet)) && !is.null(av(input$phylofacet2))){
      plot <- plot + facet_wrap(paste(input$phylofacet, paste("~", paste(input$phylofacet2))), scales = "free_y")#paste("~", paste(input$phylofacet, "+", paste(input$phylofacet2)))) 
    }else if(!is.null(av(input$phylofacet))){
      plot <- plot + facet_grid(paste("~", paste(input$phylofacet))) 
    }
  #}
  # }else if(input$phyloseqplottype1 == "scatter"){
  #   plot <- plot + geom_point(size = 0.5, position = "jitter") +
  #     labs(x = paste(input$alphaphyloseqxaxis1), y = paste(input$alphaphyloseqyaxis1),
  #          title = "Alpha Diversity") +
  #     theme(legend.position= "right", axis.text.x = element_text(color = "black", size = isolate(input$alphaphyloseqxaxistextsize), 
  #                                                                angle = isolate(input$alphaphyloseqxaxisangle)),
  #           axis.text.y = element_text(color = "black", size = input$alphaphyloseqyaxistextsize,
  #                                      angle = input$alphaphyloseqyaxisangle),
  #           axis.title.x = element_text(size = input$alphaphyloseqxaxislabelsize), axis.title.y = element_text(size = input$alphaphyloseqyaxislabelsize))
  #   if(!is.null(av(input$phylofacet)) && !is.null(av(input$phylofacet2))){
  #     plot <- plot + facet_wrap(paste(input$phylofacet, paste("~", paste(input$phylofacet2))), scales = "free_y") #facet_grid(paste(input$phylofacet, "~", paste(input$phylofacet2)), scales = "free_y") 
  #   }else if(!is.null(av(input$phylofacet))){
  #     plot <- plot + facet_wrap(paste("~", paste(input$phylofacet)), scales = "free_y")
  #   }
  # }
               })
  return(plot)
})
output$phyloseqplot1 <- renderPlot({
  #if(is.null(phyloseqplot()))
  phyloseqplot()
})
downloadPlot(id = "alphadiversityplotdownload", plotid = phyloseqplot())
#statistics -----
anovaresult <- reactive({
  req(input$alphaplotrender)
  z <- aov(mappingfileupdated()[[input$diversityindex]] ~ as.factor(mappingfileupdated()[[input$alpha]]), 
      mappingfileupdated())
  z
})
output$alphaanova1 <- renderPrint({
  req(input$alphaplotrender)
  isolate(
    summary(anovaresult())
  )
})
output$tukeyresult <- renderPrint({
  req(input$tukeyhsdrender)
  TukeyHSD(anovaresult())
})
output$tukeyresult1 <- renderPrint({
  req(input$tukeyhsdrender)
  summary(tukeyresult())
})
output$tukeytable <- renderUI({
  verbatimTextOutput("tukey_result1")
})

###Filtering of dataset -----
#create UI to select tax rank 
output$phyloseq_tax_ranks <- renderUI({
  if(is.null(phyloseqobj()))return(NULL)
  ranks <- list("NULL"="NULL")
  ranks <- c(ranks, as.list(rank_names(phyloseqobj(), errorIfNULL=FALSE)))
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
      unique <- c(unique, as.list(taxa_names(phyloseqobj())))
    } else {
      unique <- c(unique, as.list(get_taxa_unique(phyloseqobj(), input$filter_rank)))
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
  sampleIDs <- c(sampleIDs, as.list(sample_variables(phyloseqobj(), errorIfNULL=FALSE)))
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
                      choices = c("NULL", as.list(unique(as(get_variable(phyloseqobj(), input$filter_sample1), "character")))),
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
  sampleIDs <- c(sampleIDs, as.list(sample_variables(phyloseqobj(), errorIfNULL=FALSE)))
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
                           choices = c("NULL", as.list(unique(as(get_variable(phyloseqobj(), input$filter_sample2), "character")))),
                           selected = "NULL")
  }
})
observe({
  if(!is.null(av(input$filter_sample3))){
    updateSelectInput(session, 
                      inputId = "filter_sample_selection3",
                      label = "Mapping Variable Category:",
                      choices = c("NULL", as.list(unique(as(get_variable(phyloseqobj(), input$filter_sample3), "character")))),
                      selected = "NULL")
  } 
})
observe({
  if(!is.null(av(input$filter_sample4))){
    updateSelectInput(session, 
                      inputId = "filter_sample_selection4",
                      label = "Subcategory:",
                      choices = c("NULL", as.list(unique(as(get_variable(phyloseqobj(), input$filter_sample4), "character")))),
                      selected = "NULL")
  } 
})
observe({
  if(!is.null(av(input$filter_sample5))){
    updateSelectInput(session, 
                      inputId = "filter_sample_selection5",
                      label = "Mapping Variable Category:",
                      choices = c("NULL", as.list(unique(as(get_variable(phyloseqobj(), input$filter_sample5), "character")))),
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
  sampleIDs <- c(sampleIDs, as.list(sample_names(phyloseqobj())))
  return(
    selectInput("filter_sample", "Sample Variables", sampleIDs, "NULL", multiple = TRUE)
  )
})
#create UI to filter by min sample presence
output$phyloseqfilteroptions <- renderUI({
  #if(is.null(phyloseqobj()))return(NULL)
  output <- tagList(
    numericInput("phyloseqmincount", "Minimum number of counts per ASV:",
                 value = 5,
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
    obj <- phyloseqobj()
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
            IDs <- as(test$sampleID, "character")
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
observeEvent(input$phyloseqfilter, {
  if(input$phyloseqminpresence != 0 && input$phyloseqminpresencenumber != 0){
    showNotification(ui= "Filtering by Percentage and Sample Number Can Greatly Decrease Size of Dataset",
                     duration = 5, 
                     type = "warning")
  }
})

##phyloseq filter summary outputs ----
originalsamplehistplot <- reactive({
  if(is.null(phyloseqobj()))return(NULL)
  ggplot(data.frame(sample_sums(phyloseqobj())), 
         aes(x = sample_sums(phyloseqobj()))) + geom_histogram() +
    xlab("Number of Reads") + ylab("Sample Count") + scale_x_log10(labels = scales::comma) +
    labs(title= "Number of Reads per Sample")
})
output$originalsamplecounthist <- renderPlot({
  if(is.null(phyloseqobj()))return(NULL)
  originalsamplehistplot()
})
output$counthistsummaryorigsample <- renderUI({
  if(is.null(phyloseqobj()))return(NULL)
  avg <- paste("Average Number of Counts:", round(mean(sample_sums(phyloseqobj())), 3))
  min <- paste("Minimum number of Counts:", min(sample_sums(phyloseqobj())))
  max <- paste("Maximum Number of Counts:", max(sample_sums(phyloseqobj())))
  HTML(
    paste(avg, min, max, sep = '<br/>')
    )
})
downloadPlot(id = "originalsamplecounthistplot", originalsamplehistplot())
originaltaxahistplot <- reactive({
  if(is.null(phyloseqobj()))return(NULL)
  ggplot(data.frame(taxa_sums(phyloseqobj())), 
         aes(x = taxa_sums(phyloseqobj()))) + geom_histogram() + 
    xlab("Number of Reads") + ylab("ASV Count") + scale_x_log10(labels = scales::comma) +
    labs(title= "Number of Reads per ASV")
})
output$originaltaxacounthist <- renderPlot({
  if(is.null(phyloseqobj()))return(NULL)
  originaltaxahistplot()
})
output$counthisttaxa <- renderUI({
  if(is.null(phyloseqobj()))return(NULL)
  avg <- paste("Average Number of Counts:", round(mean(taxa_sums(phyloseqobj())), 3))
  min <- paste("Minimum number of Counts:", min(taxa_sums(phyloseqobj())))
  max <- paste("Maximum Number of Counts:", max(taxa_sums(phyloseqobj())))
  HTML(paste(avg, min, max, sep = '<br/>'))
})
downloadPlot(id = "originaltaxacounthistplot", plotid = originaltaxahistplot())
updatedsamplehistplot <- reactive({
  if(is.null(updatedphyloseq()))return(NULL)
  ggplot(data.frame(sample_sums(updatedphyloseq())), 
         aes(x = sample_sums(updatedphyloseq()))) + geom_histogram() + 
    xlab("Number of Reads") + ylab("Sample Count") + scale_x_log10(labels = scales::comma)+
    labs(title= "Number of Reads per Sample")
})
output$updatedsamplecounthist <- renderPlot({
  if(is.null(updatedphyloseq()))return(NULL)
  updatedsamplehistplot()
})
output$counthistsummaryorigsampleupdated <- renderUI({
  if(is.null(updatedphyloseq()))return(NULL)
  avg <- paste("Average Number of Counts:", round(mean(sample_sums(updatedphyloseq())),3))
  min <- paste("Minimum number of Counts:", min(sample_sums(updatedphyloseq())))
  max <- paste("Maximum Number of Counts:", max(sample_sums(updatedphyloseq())))
  HTML(
    paste(avg, min, max, sep = '<br/>')
  )
})
downloadPlot(id = "updatedsamplecounthistplot", plotid = updatedsamplehistplot())
updatedtaxahistplot <- reactive({
  if(is.null(updatedphyloseq()))return(NULL)
  ggplot(data.frame(taxa_sums(updatedphyloseq())), 
         aes(x = taxa_sums(updatedphyloseq()))) + geom_histogram() + 
    xlab("Number of Reads") + ylab("ASV Count") + scale_x_log10(labels = scales::comma)+
    labs(title= "Number of Reads per ASV")
})
output$updatedtaxacounthist <- renderPlot({
  if(is.null(updatedphyloseq()))return(NULL)
  updatedtaxahistplot()
})
output$counthisttaxaupdated <- renderUI({
  if(is.null(updatedphyloseq()))return(NULL)
  avg <- paste("Average Number of Counts:", round(mean(taxa_sums(updatedphyloseq())), 3))
  min <- paste("Minimum number of Counts:", min(taxa_sums(updatedphyloseq())))
  max <- paste("Maximum Number of Counts:", max(taxa_sums(updatedphyloseq())))
  HTML(paste(avg, min, max, sep = '<br/>'))
})
downloadPlot(id = "updatedtaxacounthistplot", plotid = updatedtaxahistplot())
output$originalphyloseqsummary <- renderPrint({
  if(is.null(phyloseqobj()))return(NULL)
  phyloseqobj()
})
output$updatedphyloseqsummary <- renderPrint({
  if(is.null(updatedphyloseq()))return(NULL)
  updatedphyloseq()
})
#updated OTU table ----
updatedphylootu <- reactive({
  if(is.null(updatedphyloseq()))return(NULL)
  otu_table(updatedphyloseq())
})
output$updatedphyloseqtable <- renderDataTable({
  if(is.null(updatedphyloseq()))return(NULL)
  otu_table(updatedphyloseq())
})
output$updatedphyloseqtableoutput <- renderUI({
  if(input$phyloseqfilter == 0){
    output <- tags$h3("No Filters Applied")
  }else {
    output <- tagList(
    splitLayout(dataTableOutput("updatedphyloseqtable"))
    ,
    downloadTableUI("filteredphylotabledownload")
    )
  }
  return(output)
})
downloadTable(id = "filteredphylotabledownload", tableid = updatedpylootu())
#updated taxonomy table -----
updatedtaxtablefile <- reactive({
  if(is.null(updatedphyloseq()))return(NULL)
  tax_table(updatedphyloseq())
})
output$updatedtaxtable <- renderDataTable({
  if(is.null(updatedphyloseq()))return(NULL)
  tax_table(updatedphyloseq())
})
output$updatedtaxtableoutput <- renderUI({
  if(input$phyloseqfilter == 0){
  output <- tags$h3("No Filters Applied")
  }else {
  output <- tagList(
    splitLayout(dataTableOutput("updatedtaxtable"))
    ,
    downloadTableUI("filteredtaxtabledownload")
  )
}
    return(output)
})
downloadTable(id = "filteredtaxtabledownload", tableid = updatedtaxtablefile())
#updated sample data table ----
updatedphylosample <- reactive({
  if(is.null(updatedphyloseq()))return(NULL)
  sample_data(updatedphyloseq())
})
output$updatedmappingtable <- renderDataTable({
  if(is.null(updatedphyloseq()))return(NULL)
  sample_data(updatedphyloseq())
})
output$updatedmappingtableoutput <- renderUI({
  if(input$phyloseqfilter == 0){
    output <- tags$h3("No Filters Applied")
  }else {
    output <- tagList(
      splitLayout(dataTableOutput("updatedmappingtable"))
      ,
      downloadTableUI("filteredmappingtabledownload")
    )
  }
  return(output)
})
downloadTable(id= "filteredmappingtabledownload", tableid = updatedphylosample())

#updated tree data -----
updatedtreedf <- reactive({
  if(is.null(updatedphyloseq()))return(NULL)
  tibble::as_tibble(phy_tree(updatedphyloseq()))
})
output$updatedtreetable <- renderDataTable({
  if(is.null(updatedphyloseq()))return(NULL)
  updatedtreedf()
})
output$updatedtreetableoutput <- renderUI({
  if(input$phyloseqfilter == 0){
    output <- tags$h3("No Filters Applied")
  }else {
    if(is.null(phylotree()))
    output <- tags$h3("No Tree Uploaded")
    else {
      output <- tagList(
        splitLayout(dataTableOutput("updatedtreetable"))
        ,
        downloadTableUI("filteredtreetabledownload")
      )
    }
  }
})
downloadTable(id= "filteredtreetabledownload", tableid = updatedtreedf())

#Heatmap -----
output$heatmapoptions <- renderUI({
  if(is.null(phyloseqobj()))return(NULL)
  selectInput("heatmapmethod", "Select Method:",
              choices = c("NULL" = "NULL",
                          "DCA", "CCA", "RDA", 
                          "CAP", "DPCoA", "NMDS", 
                          "MDS", "PCoA"),
              selected = "NULL")
})
output$heatmapoptions1 <- renderUI({
  if(is.null(phyloseqobj()))return(NULL)
  selectInput("heatmapdistance", "Select Distance Method:",
              choices = list(
                Standard = c("NULL" = "NULL",
                             "Bray" = "bray",
                             "Jaccard" = "jaccard",
                             "Euclidean" = "euclidean",
                             "DPCoA" = "dpcoa",
                             "JSD" = "jsd"),
                Require_Phylogenetic_Tree = c(
                  "Unweighted Unifrac" = "unifrac",
                  "Weighted Unifrac" = "wunifrac"
                )),
              selected = "NULL")
})
output$heatmapoptions2 <- renderUI({
    if(is.null(phyloseqobj()))return(NULL)
    sampleIDs <-  list()
    sampleIDs <- c(sampleIDs, as.list(sample_variables(phyloseqobj(), errorIfNULL=FALSE)))
    sampleIDs <- c(sampleIDs, list(Sample="Sample"))
    return(
      selectInput("heatmapxaxis", "Select How to Label Xaxis:", sampleIDs, "Sample", multiple = FALSE)
    )
})
output$heatmapoptions3 <- renderUI({
  if(is.null(phyloseqobj()))return(NULL)
  ranks <- list()
  ranks <- c(ranks, as.list(rank_names(phyloseqobj(), errorIfNULL=FALSE)))
  ranks <- c(ranks, list(OTU="OTU"))
  return(
    selectInput("heatmapyaxis", "Select How to Label Yaxis:", 
                ranks, selected = "OTU", multiple = FALSE)
  )
})
output$heatmapaction <- renderUI({
  if(is.null(phyloseqobj()))return(NULL)
  actionButton("heatmaprender", "Render Heatmap", width = "100%")
})
phylosequse <- reactive({
  if(!is.null(updatedphyloseq())){
    updatedphyloseq()
  } else {
    phyloseqobj()
  }
})
heatmapplot <- eventReactive(input$heatmaprender, {
  withProgress(message = "Applying Filters:", {
    if(!is.null(av(input$heatmapmethod)) && !is.null(av(input$heatmapdistance))){
      plot <- plot_heatmap(phylosequse(), method = input$heatmapmethod,
                          distance = input$heatmapdistance,
                          sample.label = input$heatmapxaxis,
                          taxa.label = input$heatmapyaxis)
    }else if(!is.null(av(input$heatmapmethod)) && is.null(av(input$heatmapdistancec))){
      plot <- plot_heatmap(phylosequse(), method = input$heatmapmethod,
                           sample.label = input$heatmapxaxis,
                           taxa.label = input$heatmapyaxis)
    }else if(is.null(av(input$heatmapmethod)) && !is.null(av(input$heatmapdistance))){
      plot <- plot_heatmap(phylosequse(),
                           distance = input$heatmapdistance,
                           sample.label = input$heatmapxaxis,
                           taxa.label = input$heatmapyaxis)
    }else if(is.null(av(input$heatmapmethod)) && is.null(av(input$heatmapdistance))){
      plot <- plot_heatmap(phylosequse(),
                           sample.label = input$heatmapxaxis,
                           taxa.label = input$heatmapyaxis)
    }
    # plot <- plot_heatmap(phyloseqobj(), method = input$heatmapmethod,
    #                 distance = input$heatmapdistance,
    #                 sample.label = input$heatmapxaxis,
    #                 taxa.label = input$heatmapyaxis)
  })
  return(plot)
})
output$heatmapplotoutput <- renderPlot({
  withProgress(message = "Making Heatmap:", {
  heatmapplot()
  })
})
downloadPlot(id = "heatmapplotoutputdownload",plotid = heatmapplot())


###Normalization-----
#choice of rarefying data or converting to absolute counts
output$normalization <- renderUI({
  if(is.null(otufile()))return(NULL)
  radioButtons("normalize", "Select how you would like to normalize your data",
                choices = c("Relative Counts" = "absolute",
                            "No Normalization" = "none"),
               selected = "none")
})
output$rarefycounts <- renderUI({
  if(is.null(otufile()))return(NULL)
  conditionalPanel(condition = "input.normalize == 'rarefy'",
  numericInput("rarefiedcounts", "Select Number of Counts to Rarefy To:",
               value = 0, min = 0)
  )
})
output$normalizego <- renderUI({
  if(is.null(otufile()))return(NULL)
  actionButton("gonorm", "Apply", width = "100%")
})
#converts to absolute counts and filters otu table based on mapping filters
objupdatedabsolute <- reactive({
  req(otufile())
  req(input$gonorm)
  if(input$normalize == "rarefy"){
    withProgress(message = "Rarefying Counts",
                 detail = "This may take a while...", {
                   obj1 <- objupdated()
                   obj1[["data"]][["tax_data"]][, c(-1,-2,-3)] <- obj1[["data"]][["tax_data"]][, c(-1,-2,-3)] %>% round()
                   obj1[["data"]][["tax_data"]] <- rarefy_obs(obj1, "tax_data",
                                                              sample_size = input$rarefiedcounts,
                                                                  other_cols = TRUE)
                   obj1[["data"]][["tax_data"]] <- obj1[["data"]][["tax_data"]][, c("taxon_id", "otu_id",
                                                                                    "ConsensusLineage",
                                                                                    mappingfilefiltered()$sampleID)] 
                 })
    obj1 
  }else if(input$normalize == "none"){
                   obj1 <- objupdated()
                   obj1[["data"]][["tax_data"]][, c(-1,-2,-3)] <- obj1[["data"]][["tax_data"]][, c(-1,-2,-3)] %>% round()
                   obj1[["data"]][["tax_data"]] <- obj1[["data"]][["tax_data"]][, c("taxon_id", "otu_id",
                                                                                    "ConsensusLineage",
                                                                                    mappingfilefiltered()$sampleID)] 
    obj1
  }
})
##will show new table with normalized counts
output$taxmaptable2 <- renderDataTable({
  if (is.null(mappingfile())) return(NULL)
  objupdatedabsolute()[["data"]][["tax_data"]]
})
output$taxmaptable3 <- renderUI({
  if (is.null(mappingfile())) return(NULL)
    dataTableOutput("taxmaptable2")
})

#####Beta diversity /OrdinationPhyloseq-----

output$ordinationdataoption <- renderUI({
  if(is.null(phyloseqobj()))return(NULL)
  radioButtons("ordinationdataset", "Select Dataset to Use",
               choices = c("Filtered" = "filtered",
                           "Original" = "original"),
               selected = "original")
})
ordinationdatasetuse <- reactive({
  if(is.null(phyloseqobj()))return(NULL)
  switch(input$ordinationdataset,
         "filtered" = updatedphyloseq(),
         "original" = phyloseqobj())
})

output$phyloseqdistanceoptions <- renderUI({
  if(is.null(phyloseqobj()))return(NULL)
 output <- tagList(
   selectInput("phyloseqdistanceoptions1", "Select Distance Method:",
              choices = list(
                Standard = c("Bray" = "bray",
                             "Jaccard" = "jaccard",
                             "Euclidean" = "euclidean",
                             "DPCoA" = "dpcoa",
                             "JSD" = "jsd"),
                Require_Phylogenetic_Tree = c(
                  "Unweighted Unifrac" = "uunifrac",
                  "Weighted Unifrac" = "wunifrac"
                )),
              selected = "bray")
   ,
   conditionalPanel(condition = "input.phyloseqdistanceoptions1 == 'uunifrac' || 
                    input.phyloseqdistanceoptions1 == 'wunifrac'",
                    tags$div(tags$h5(tags$b("NOTE:"),"For this distance option, we recommend a filtered
                            dataset to avoid lengthy running times."),
                    align = "left"))
 )
 return(output)
})
output$makedistancematrixtable <- renderUI({
  if(is.null(phyloseqobj()))return(NULL)
  actionButton("renderdistancematrix", "Create Distance Matrix")
})
distancematrix <- eventReactive(input$renderdistancematrix, {
  withProgress(message = "Creating Distance Matrix", 
               detail= "This may take a while", {
  phyloseq::distance(physeq = ordinationdatasetuse(), method = input$phyloseqdistanceoptions1)
  })
})
output$distancematrixtable <- renderDataTable({
  if(is.null(distancematrix()))return(NULL)
  as.matrix(distancematrix())
})
downloadTable(id = "distancematrixtabledownload", tableid = as.matrix(distancematrix()))

output$dispersionUI <- renderUI({
  if(is.null(distancematrix()))return(NULL)
  output<- tagList(
    tags$div(tags$h4("You are Viewing the", paste(input$ordinationdataset), "Dataset"),
             align = "center")
    ,
    selectInput("betadispersionoptions1", "Select Factors to View Dispersion:",
                choices = sample_variables(ordinationdatasetuse()),
                multiple = FALSE)
    ,
    actionButton("renderbetadispersion", "Visualize Dispersion:")
    ,
    hr()
    ,
    downloadPlotUI(id = "betadispersionplot2download")
  )
  return(output)
})
# output$betadispersionoptions <- renderUI({
#   if(is.null(distancematrix()))return(NULL)
#   selectInput("betadispersionoptions1", "Select Factors to View Dispersion:",
#               choices = sample_variables(ordinationdatasetuse()),
#               multiple = FALSE)
# })
# output$betadispersionplotrender <- renderUI({
#   if(is.null(distancematrix()))return(NULL)
#   actionButton("renderbetadispersion", "Visualize Dispersion:")
# })
betadispersionplot <- eventReactive(input$renderbetadispersion,{
  withProgress(message = "Constructing Beta Dispersion Plot", {
  mod <- betadisper(distancematrix(), sample_data(ordinationdatasetuse())[[input$betadispersionoptions1]])
  centroids<- data.frame(grps=rownames(mod$centroids),data.frame(mod$centroids))
  vectors<- data.frame(group=mod$group,data.frame(mod$vectors))
  seg.data<- cbind(vectors[,1:3],centroids[rep(1:nrow(centroids),as.data.frame(table(vectors$group))$Freq),2:3])
  names(seg.data)<-c("grps","v.PCoA1","v.PCoA2","PCoA1","PCoA2")
  ggplot() + 
    geom_point(data=centroids, aes(x=PCoA1,y=PCoA2),size=4,colour="red",shape=16) + 
    geom_point(data=seg.data, aes(x=v.PCoA1,y=v.PCoA2),size=2,shape=16) +
    labs(title="Dispersion Plot",x="",y="") + facet_grid(~grps) + stat_ellipse(type = "t")
  })
  })
output$betadispersionplot1 <- renderPlot({
  if(is.null(betadispersionplot()))return(NULL)
  betadispersionplot()
})
output$betadispersionplot2 <- renderUI({
  plotOutput("betadispersionplot1")
})
downloadPlot(id = "betadispersionplot2download", plotid = betadispersionplot())
betadispstat <- eventReactive(input$renderbetadispersion, {
  if(is.null(distancematrix()))return(NULL)
  withProgress(message = "Performing Beta Dispersion", {
  #file <- mappingfile() %>% select(-"sampleID", - "plantID")
  # anovaresult <- list()
  # for(i in sample_variables(phyloseqobj())){
  #     #colnames(select_if(file, is.character))){  
  #   anovaresult[[i]] <- anova(betadisper(distancematrix(), sample_data(phyloseqobj())[[i]]))
  # }
  # return(anovaresult)
  # })
    anova(betadisper(distancematrix(), sample_data(ordinationdatasetuse())[[input$betadispersionoptions1]]))
})
    })
output$betadisptable <- renderPrint({
  if(is.null(betadispstat()))return(NULL)
  betadispstat()
})

output$adonisUI <- renderUI({
  if(is.null(ordinationdatasetuse()))return(NULL)
  output <- tagList(
    tags$div(tags$h4("You are Viewing the", paste(input$ordinationdataset), "Dataset"),
             align = "center")
    ,
    textInput("adonisoptions1", "Write Formula",
              placeholder = "A + B*C", width = "100%")
    ,
    # selectInput("adonisoptions1", "Select Variable to Compare",
    #             choices = sample_variables(ordinationdatasetuse()),
    #             multiple = FALSE)
    # ,
    selectInput("adonisstrata", "Groups Within Which to Constrain Permutations",
                choices = c("NULL", sample_variables(ordinationdatasetuse())),
                selected = "NULL")
    ,
    numericInput("adonispermutations", "Select Number of Permutations",
                 value = 999, min =1, width = "100%")
    ,
    actionButton("adonisrender1", "Perform Adonis:")
  )
  return(output)
})

# output$adonisoptions <- renderUI({
# # varSelectInput("adonisoptions1", "Select Variable to Compare:",
# #                  data = mappingfile(),
# #                  multiple = FALSE)
#   selectInput("adonisoptions1", "Select Variable to Compare",
#               choices = sample_variables(ordinationdatasetuse()),
#               multiple = FALSE)
#   })
# output$adonisrender <- renderUI({
# actionButton("adonisrender1", "Perform Adonis:")
# })
ordinationadonis <- eventReactive(input$adonisrender1, {
  if(!is.null(av(input$adonisstrata))){
  withProgress(message = "Performing Adonis", {
      adonis(as.formula(paste("distancematrix() ~", paste(input$adonisoptions1))), 
             strata = sample_data(ordinationdatasetuse())[[input$adonisstrata]], permutations = input$adonispermutations,
             data = as(sample_data(ordinationdatasetuse()), "data.frame"))
  })
  }else{
    withProgress(message = "Performing Adonis", {
    adonis(as.formula(paste("distancematrix() ~", paste(input$adonisoptions1))), 
           permutations = input$adonispermutations,
           data = as(sample_data(ordinationdatasetuse()), "data.frame"))
    })
  }
})
output$adonisphyloseq <- renderPrint({
  if(is.null(phyloseqobj()))return(NULL)
  ordinationadonis()
})  

pcoaobj <- eventReactive(input$makeordinationplot1, {
  #if(is.null(distancematrix()))return(NULL)
  if(input$phyloseqordinateoptions1 == "CCA" | input$phyloseqordinateoptions1 == "RDA" |input$phyloseqordinateoptions1 == "CAP"){
  isolate(
    ordinate(
    physeq = ordinationdatasetuse(), 
    method = input$phyloseqordinateoptions1, 
    distance = distancematrix(),
    formula = as.formula(paste("~", paste(input$formulaoptions1, collapse = "+")))
  ))
  }else if(input$phyloseqordinateoptions1 == "PCoA" | input$phyloseqordinateoptions1 == "NMDS"){
    isolate(ordinate(
      physeq = ordinationdatasetuse(), 
      method = input$phyloseqordinateoptions1, 
      distance = distancematrix()
  ))
  }
})
output$ordinationplotoptions <- renderUI({
  if(is.null(phyloseqobj()))return(NULL)
  output <- tagList(
    tags$div(tags$h4("You are Viewing the", paste(input$ordinationdataset), "Dataset"),
             align = "center")
    ,
    selectInput("phyloseqordinateoptions1", "Select Ordination Method:",
                choices = list(
                  Unconstrained = c("PCoA" = "PCoA",
                                    "NMDS" = "NMDS"),
                  Constrained = c("CCA" = "CCA",
                                  "RDA" = "RDA",
                                  "CAP" = "CAP")),
                selected = "PCoA")
    ,
    conditionalPanel(condition = 
                       "input.phyloseqordinateoptions1 == 'CCA'|| input.phyloseqordinateoptions1 == 'RDA' || input.phyloseqordinateoptions1 == 'CAP'",
                     selectInput("formulaoptions1", "Select Factors to Include:",
                                 choices = sample_variables(ordinationdatasetuse()),
                                 multiple = TRUE)
    )
    ,
    textInput(inputId = "ordinationplottitle1", 
              label = "Create Title for Plot",
              placeholder = "Title")
    ,
    selectInput("ordinationcoloroptions1", "Select Variable to Color:",
                choices = c("NULL", sample_variables(ordinationdatasetuse())),#names(mappingfile())),
                selected = "NULL",
                multiple = FALSE)
    ,
    selectInput("ordinationshapeoptions1", "Select Variable to Shape:",
                choices = c("NULL", sample_variables(ordinationdatasetuse())),#names(mappingfile())),
                selected = "NULL",
                multiple = FALSE)
    ,
    radioButtons("ordinationellipse1", "Add Ellipse?",
                 choices = c("Yes" = "yes",
                             "No" = "no"),
                 selected = "no")
    ,
    actionButton("makeordinationplot1", "Render Plot", width = "100%")
    ,
    hr()
    ,
    downloadPlotUI("ordinationplotoutputdownload")
  )
  return(output)
})

ordinationplot <- eventReactive(input$makeordinationplot1, {
  req(phyloseqobj())
  if(input$ordinationellipse1 == "yes"){
  plot_ordination(
    physeq = ordinationdatasetuse(),
    ordination = pcoaobj(),
    color = input$ordinationcoloroptions1,
    shape = input$ordinationshapeoptions1,
    title = input$ordinationplottitle1) + stat_ellipse(type = "t")
    }else
      plot_ordination(
        physeq = ordinationdatasetuse(),
        ordination = pcoaobj(),
        color = input$ordinationcoloroptions1,
        shape = input$ordinationshapeoptions1,
        title = input$ordinationplottitle1)
})

ordinationplotoutput1 <- eventReactive(input$makeordinationplot1, {
  withProgress(message = "Making Ordination Plot",
               detail = "This may take a while...", {
  if(input$phyloseqordinateoptions1 == "CCA"){
    arrowmat <- vegan::scores(pcoaobj(), display = "bp")
    arrowdf <- data.frame(labels = rownames(arrowmat), arrowmat)
    arrow_map <- aes(xend = CCA1, 
                     yend = CCA2, 
                     x = 0, 
                     y = 0, 
                     shape = NULL, 
                     color = NULL, 
                     label = labels)
    
    label_map <- aes(x = 1.3 * CCA1, 
                     y = 1.3 * CCA2, 
                     shape = NULL, 
                     color = NULL, 
                     label = labels)
    
    arrowhead = arrow(length = unit(0.02, "npc"))
    isolate(ordinationplot() + 
      geom_segment(
        mapping = arrow_map, 
        size = .5, 
        data = arrowdf, 
        color = "gray", 
        arrow = arrowhead
      ) + 
      geom_text(
        mapping = label_map, 
        size = 4,  
        data = arrowdf, 
        show.legend = FALSE
      ))
  }else if(input$phyloseqordinateoptions1 == "RDA"){
    arrowmat <- vegan::scores(pcoaobj(), display = "bp")
    arrowdf <- data.frame(labels = rownames(arrowmat), arrowmat)
    arrow_map <- aes(xend = RDA1, 
                     yend = RDA2, 
                     x = 0, 
                     y = 0, 
                     shape = NULL, 
                     color = NULL, 
                     label = labels)
    
    label_map <- aes(x = 1.3 * RDA1, 
                     y = 1.3 * RDA2, 
                     shape = NULL, 
                     color = NULL, 
                     label = labels)
    
    arrowhead = arrow(length = unit(0.02, "npc"))
    
    isolate(ordinationplot() + 
      geom_segment(
        mapping = arrow_map, 
        size = .5, 
        data = arrowdf, 
        color = "gray", 
        arrow = arrowhead
      ) + 
      geom_text(
        mapping = label_map, 
        size = 4,  
        data = arrowdf, 
        show.legend = FALSE
      ))
  }else if(input$phyloseqordinateoptions1 == "CAP"){
    arrowmat <- vegan::scores(pcoaobj(), display = "bp")
    arrowdf <- data.frame(labels = rownames(arrowmat), arrowmat)
    arrow_map <- aes(xend = CAP1, 
                     yend = CAP2, 
                     x = 0, 
                     y = 0, 
                     shape = NULL, 
                     color = NULL, 
                     label = labels)
    
    label_map <- aes(x = 1.3 * CAP1, 
                     y = 1.3 * CAP2, 
                     shape = NULL, 
                     color = NULL, 
                     label = labels)
    
    arrowhead = arrow(length = unit(0.02, "npc"))
    isolate(ordinationplot() + 
      geom_segment(
        mapping = arrow_map, 
        size = .5, 
        data = arrowdf, 
        color = "gray", 
        arrow = arrowhead
      ) + 
      geom_text(
        mapping = label_map, 
        size = 4,  
        data = arrowdf, 
        show.legend = FALSE
      ))
  }else
  ordinationplot()
               })
})
output$ordinationplotoutput <- renderPlot({
  ordinationplotoutput1()
})
output$ordinationplotoutputUI <- renderUI({
  if(is.null(phyloseqobj()))return(NULL)
  if(input$makeordinationplot1 == 0){
    output <- tags$h3("Please Create a Distance Matrix First")
  }else {
    req(distancematrix())
    output <- plotOutput("ordinationplotoutput")
  }
  return(output)
})

downloadPlot(id = "ordinationplotoutputdownload", plotid = )

####Beta Div vegan -----
output$betaindices <- renderUI({
  if(is.null(objupdated())) return(NULL)
  radioButtons(inputId = "betaindex", label = "Select Dissimilarity Index to Use",
               choices = c("Bray-Curtis" = "bray",
                           "Euclidean" = "euclidean",
                           "Jaccard" = "jaccard"))
})
beta_dist <- eventReactive(input$betarender, {
  withProgress(message = "Calculating Beta Diversity", {  
  vegdist(t(objupdatedabsolute()$data$tax_data[, mappingfilefiltered()$sampleID]),
            index = input$betaindex)
  })
})
mds <- reactive({
  if(is.null(beta_dist())) return(NULL)
  withProgress(message = "Converting Table", {
  mds <- metaMDS(beta_dist())
  mds_data <- as.data.frame(mds$points)
  mds_data$sampleID <- rownames(mds_data)
  mds_data <- left_join(mds_data, mappingfileupdated())
  })
  mds_data
}) 
output$betacolor <- renderUI({
  if (is.null(mds())) return(NULL)
  selectInput("beta", "Select Variable to Color:",
              choices = names(mds()),
              selected = "",
              multiple = FALSE)
})
output$betaxaxis <- renderUI({
  if(is.null(mds())) return(NULL)
  textInput("betaxaxis1", "Label X-Axis")
})
output$betayaxis <- renderUI({
  if(is.null(mds())) return(NULL)
  textInput("betayaxis1", "Label Y-Axis")
})
output$betaellipse <- renderUI({
  if(is.null(mds())) return(NULL)
  radioButtons("betacustom", "Add Additional Attributes to Graph:",
                choices = c("Ellipsoid" = "ellipse",
                          "None" = "none"),
                selected = "none")
})
output$betaplot <- renderPlotly({
  req(input$betarender2)
  obj1 <- mds()
  if(input$betacustom == "none"){
  isolate(
  ggplot(obj1, aes_string(color = input$beta)) + 
    geom_point(aes(x = MDS1, y = MDS2)) +
    labs(x = input$betaxaxis1, y = input$betayaxis1))
  }else if(input$betacustom == "ellipse"){
    isolate(
      ggplot(obj1, aes_string(color = input$beta)) + 
        geom_point(aes(x = MDS1, y = MDS2)) +
        stat_ellipse(aes(x = MDS1, y = MDS2),
                     type = "t") +
        labs(x = input$betaxaxis1, y = input$betayaxis1))
  }else return(NULL)
  
})
output$betatable <- renderTable({
  if(is.null(mds())) return(NULL)
  mds()
})
output$betaplotui <- renderUI({
  if(is.null(input$betarender2))return(NULL)
  isolate(
    plotlyOutput("betaplot")
  )
})
  
####Differential Abundance -----
output$diffabundance <- renderUI({
  #if(is.null(phyloseqobj()))return(NULL)
  output <- tagList(
    varSelectInput("diffabundoptions", "Select Design Condition to Compare:",
                data = as.data.frame(sample_data(phyloseqobj())),
                multiple = FALSE)
    ,
    actionButton("performdeseq", "Perform Deseq:")
    ,
    conditionalPanel("input.performdeseq",
                     hr(),
    tags$h4("Based on Results Table, View Distribution of Individual ASV's")
    ,
    textInput("asvid", "Select ASV to View Count Distribution:",
              value = "ASV ID")
    ,
    actionButton("asvplotrender", "Render ASV Count Plot:"))
  )
  return(output)
})
deseqobj <- eventReactive(input$performdeseq, {
  withProgress(message = "Performing Deseq Analysis",
               detail = "This may take a while...", {
  deseqtest <- phyloseq_to_deseq2(phyloseqobj(), as.formula(paste("~", paste(input$diffabundoptions))))
  deseqtest@assays@data@listData[["counts"]] <- deseqtest@assays@data@listData[["counts"]] + 1 %>% 
    round() %>% as.integer() 
  deseqtest1 <- DESeq(deseqtest, test= "Wald", fitType="parametric")
  
               })
})
deseqresults <- reactive({
  if(is.null(deseqobj()))return(NULL)
  withProgress(message = "Getting Deseq Results",
               detail = "This may take a while...", {
                 deseqresult1 <- results(deseqobj(), alpha = 0.05,
                                         format = "DataFrame")
                 deseqresult1 <- cbind(as(deseqresult1, "data.frame"), as(tax_table(phyloseqobj())[rownames(deseqresult1), ], "matrix"))
                 deseqresult1$asvID <- rownames(deseqresult1)
                 deseqresult1 <- deseqresult1 %>% mutate('-log10(padj)' = -log10(padj))
                 return(deseqresult1)
                 })
})
output$deseqresultprint <- renderPrint({
  if(is.null(deseqresults()))return(NULL)
  resultsNames(deseqobj())
})
output$deseqsummary <- renderPrint({
  if(is.null(deseqresults()))return(NULL)
  summary(deseqresults())
})
output$deseqpvalue <- renderText({
  if(is.null(deseqresults()))return(NULL)
  paste(sum(deseqresults()[["padj"]] < 0.05, na.rm = TRUE), "ASV's Have A Padj Value Less Than 0.05")
  })

output$deseqpvaluelist <- renderDataTable({
  if(is.null(deseqobj()))return(NULL)
  as.data.frame(deseqresults()) 
})
downloadTable(id = "deseqpvaluelistdownload", tableid = deseqresults())

deseqasvplot <- eventReactive(input$asvplotrender, {
    plotCounts(dds= deseqobj(), gene = input$asvid, intgroup = paste(input$diffabundoptions))
})
output$deseqasvplot1 <- renderPlot({
  if(is.null(deseqasvplot()))return(NULL)
  deseqasvplot()
})
downloadPlot(id = "deseqasvplot1download", plotid = deseqasvplot())

output$volcanoplotui <- renderUI({
  if(is.null(phyloseqobj()) && is.null(deseqresults()))return(NULL)
    output <- tagList(
      selectInput("volcanotaxrank1", "Select Taxonomic Rank to Depict:",
                  choices = c(rank_names(phyloseqobj()), "threshold"),
                  selected = 2)
      ,
      radioButtons("volcanothreshlines", label = "Show Threshold Lines?",
                   choices = c("Yes", "No"),
                   selected = "Yes")
      ,
      radioButtons("volcanoplotlabels", "Label Enriched Points?",
                   choices = c("Yes", "No"),
                   selected = "No")
      ,
      conditionalPanel(condition = "input.volcanoplotlabels == 'Yes'",
                       numericInput("volcanoplotlabelsthresh", "Select Numeric Threshold for Label:",
                                    2.5, min = 1.5, max = 10))
      ,
      actionButton("volcanoplotrender1", "Render Volcano Plot:")
      ,
      hr()
      ,
      downloadPlotUI("volcanoplotdownload")
    )
})

volcanoplot <- eventReactive(input$volcanoplotrender1, {
  withProgress(message = "Making Plot", {
  obj <- as.data.frame(deseqresults())
  obj <- obj %>% mutate(threshold = padj < 0.05)
  if(input$volcanothreshlines == "Yes"){
    plot <- ggplot(obj, aes(x = log2FoldChange, y = -log10(padj), color = !!as.symbol(input$volcanotaxrank1))) + 
      geom_point() + 
      xlab("log2 fold change") + 
      ylab("-log10 adjusted p-value") + 
      geom_vline(xintercept = c(-1,0,1))+
      geom_hline(yintercept = -log10(0.05))
    if(input$volcanoplotlabels == "Yes"){
    plot <- plot + geom_text(aes(label=ifelse(log2FoldChange> isolate(input$volcanoplotlabelsthresh),as.character(asvID),'')),color = "black",hjust=0,vjust=0) +
      theme(plot.title = element_text(size = rel(1.5), hjust = 0.5), 
            axis.title = element_text(size = rel(1.25)))
    } else {
        plot <- plot
    }
  }else if(input$volcanothreshlines=="No"){
      plot <- ggplot(obj, aes(x = log2FoldChange, y = -log10(padj), color = !!as.symbol(input$volcanotaxrank1))) + 
        geom_point() + 
        xlab("log2 fold change") + 
        ylab("-log10 adjusted p-value")
      if(input$volcanoplotlabels == "Yes"){
        plot <- plot + geom_text(aes(label=ifelse(log2FoldChange> isolate(input$volcanoplotlabelsthresh),as.character(asvID),'')),color = "black",hjust=0,vjust=0) +
          theme(plot.title = element_text(size = rel(1.5), hjust = 0.5), 
                axis.title = element_text(size = rel(1.25)))
      }else {
        plot <- plot
      }
  }else {
      return(NULL)}
  
  return(plot)
  })
})
downloadPlot(id = "volcanoplotdownload", plotid = volcanoplot())

###volcano plot interactive portion ------
output$volcanoplotprint <- renderPrint({
  if(is.null(deseqresults()))return(NULL)
  brushedPoints(deseqresults(), input$volcanobrush)
})
output$volcanosaveselection <- renderUI({
  actionButton("volcanosaveselection1", "Save Current Selection:")
})
output$volcanoseparateselection <- renderUI({
  actionButton("volcanoseparateselection1", "Save With Different Grouping")
})
output$volcanoresetselection <- renderUI({
  actionButton("volcanoresetselection1", "Reset Current Selection:")
})
output$volcanoselectionName <- renderUI({
  textInput("volcanoselectionName1", "Create Label for Selection 1:",
            value = "Selection 1")
})
output$volcanonotext <- renderUI({
  textInput("volcanonotext1", "Name for Samples not Included in Selections",
            value = "Not Grouped")
})
output$volcanocolumnName <- renderUI({
  textInput("volcanocolumnName1", "Create Name for Column to be Added",
            value = "New Column")
})
output$volcanoactionbutton <- renderUI({
  actionButton("volcanoactionbutton1", "Add Column:")
})
####Dynamically select multiple points 
volcanoselections <- reactiveValues()
volcanoselections$samples <- data.frame()
#add selection to dataframe
observeEvent(input$volcanosaveselection1, {
  newLine <- brushedPoints(deseqresults(), input$volcanobrush)["asvID"]
  volcanoselections$samples <- rbindPad(data = volcanoselections$samples, selections = newLine)
  return(volcanoselections$samples)
})
#add selection as different grouping 
observeEvent(input$volcanoseparateselection1, {
  if(ncol(volcanoselections$samples) >= 1 && ncol(volcanoselections$samples) < 7){
    newGrouping <- brushedPoints(deseqresults(), input$volcanobrush)["asvID"]
    volcanoselections$samples <- cbindPad(volcanoselections$samples, newGrouping)
  }else if(ncol(volcanoselections$samples) == 0 && ncol(volcanoselections$samples) >=7){
    volcanoselections$samples
  }
})
#reset selection and remove created text inputs
observeEvent(input$volcanoresetselection1, {
  volcanoselections$samples <- data.frame()
})

observeEvent(input$volcanoresetselection1, {
  removeUI(
    selector = '#volcano2, #volcano3, #volcano4, #volcano5, #volcano6',
    multiple = TRUE
  )
})
observeEvent(input$volcanoresetselection1, {
  counter(1)
})

#make dynamic number of UI elements for column naming
volcanocounter <- reactiveVal(1)
observeEvent(input$volcanoseparateselection1, {
  if(ncol(volcanoselections$samples) >= 1 && ncol(volcanoselections$samples) < 7){
    volcanocounter1 <<- volcanocounter() + 1
    volcanocounter(volcanocounter1)
    insertUI(
      selector = '#volcanocontainer',
      where = "beforeEnd",
      ui = tags$div(textInput(paste("volcanoselectionName", paste(volcanocounter()), sep = ""), paste("Create Label for Selection", paste(volcanocounter())),
                              value = paste("Selection", paste(volcanocounter()))),
                    id = paste0("volcano", paste(volcanocounter())))
    )
  } else if(ncol(volcanoselections$samples) == 0){
    showNotification(ui = "You Must First Make A Preliminary Selection",
                     type = "error")
  }
})
observe({
  if(volcanocounter() >= 6){
    showNotification(ui= "You Have Made the Maximum Number of Selections",
                     action = a(href = "javascript:location.reload();", "Reload page"),
                     duration = NULL, 
                     type = "error")
  }
})

#this produces the table to view selected points
output$volcanotable1 <- renderDataTable({
  volcanoselections$samples
})
#dynamically name selections and update the table with the new names
volcanotest <- reactiveValues()
volcanotest$list <- c()
observe({
  if(volcanocounter() == 1){
    volcanoname1 <- input$volcanoselectionName1
    volcanotest$list <- c(volcanoname1)
  }else if(volcanocounter() == 2){
    volcanoname1 <- input$volcanoselectionName1
    volcanoname2 <- input$volcanoselectionName2
    volcanotest$list <- c(volcanoname1, volcanoname2)
  }else if(volcanocounter() == 3){
    volcanoname1 <- input$volcanoselectionName1
    volcanoname2 <- input$volcanoselectionName2
    volcanoname3 <- input$volcanoselectionName3
    volcanotest$list <- c(volcanoname1, volcanoname2, volcanoname3)
  }else if(volcanocounter() == 4){
    volcanoname1 <- input$volcanoselectionName1
    volcanoname2 <- input$volcanoselectionName2
    volcanoname3 <- input$volcanoselectionName3
    volcanoname4 <- input$volcanoselectionName4
    volcanotest$list <- c(volcanoname1, volcanoname2, volcanoname3, volcanoname4)
  }else if(volcanocounter() == 5){
    volcanoname1 <- input$volcanoselectionName1
    volcanoname2 <- input$volcanoselectionName2
    volcanoname3 <- input$volcanoselectionName3
    volcanoname4 <- input$volcanoselectionName4
    volcanoname5 <- input$volcanoselectioname5
    volcanotest$list <- c(volcanoname1, volcanoname2, volcanoname3, volcanoname4, volcanoname5)
  }else if(volcanocounter() == 6){
    volcanoname1 <- input$volcanoselectionName1
    volcanoname2 <- input$volcanoselectionName2
    volcanoname3 <- input$volcanoselectionName3
    volcanoname4 <- input$volcanoselectionName4
    volcanoname5 <- input$volcanoselectionName5
    volcanoname6 <- input$volcanoselectionName6
    volcanotest$list <- c(volcanoname1, volcanoname2, volcanoname3, volcanoname4, volcanoname5, volcanoname6)
  }else if(volcanocounter() >= 7){
    volcanotest$list <- NULL
  }
  return(volcanotest$list)
})
observe({
  if(ncol(volcanoselections$samples != 0)){
    colnames(volcanoselections$samples) <- volcanotest$list
  }else return(NULL)
})
###Add column to mapping table
volcanoupdatedtable <- reactiveValues()
volcanoupdatedtable$table <- data.frame()
observe({
  if(is.null(deseqresults()))return(NULL)
  volcanoupdatedtable$table <- deseqresults()
})

observeEvent(input$volcanoactionbutton1, {
  columnadd <- pivot_longer(volcanoselections$samples, everything(), names_to = input$volcanocolumnName1, values_to = "asvID")
  variables <- data.frame("asvID" = deseqresults()[["asvID"]])
  columnadd <- right_join(columnadd, variables)
  volcanoupdatedtable$table <- left_join(volcanoupdatedtable$table, columnadd)
  volcanoupdatedtable$table[is.na(volcanoupdatedtable$table)] <- input$volcanonotext1
})
#Make Updated table
output$volcanotesttable <- renderDataTable({
  volcanoupdatedtable$table
})
downloadTable(id = "volcanoselectionstable", tableid = volcanoselections$samples)
downloadTable(id = "volcanodesequpdated", volcanoupdatedtable$table)


###log2foldchange plot -----
output$log2foldchangeui <- renderUI({
  if(is.null(deseqresults()))return(NULL)
  output<- tagList(
    selectInput("log2foldchangexaxis", "Select Tax Rank for X Axis:",
                choices = rank_names(phyloseqobj()))
    ,
    selectInput("log2foldchangecolor", "Select Tax Rank to Color:",
                choices = c(rank_names(phyloseqobj()), "none"),
                selected = "none")
    ,
    actionButton("log2foldchangeplotrender", "Make Plot:")
    ,
    hr()
    ,
    downloadPlotUI(id = "log2foldchangegraphdownload")
  )
  return(output)
})
log2foldchangeplot <- eventReactive(input$log2foldchangeplotrender, {
  withProgress(message = "Making Plot", {
  if(!is.null(deseqresults())){
    if(input$log2foldchangecolor == "none"){
      obj <- deseqresults()
      plot <- ggplot(obj, aes(x = reorder(!!as.symbol(input$log2foldchangexaxis), log2FoldChange), y = log2FoldChange)) + 
        geom_point() +
        theme(axis.text.x = element_text(angle = -90, hjust = 0, vjust=0.5))+ 
        xlab("log2 fold change") 
    }else{
      data <- deseqresults()
      plot <- ggplot(data, aes(x = reorder(!!(as.symbol(input$log2foldchangexaxis)), -log2FoldChange), y = log2FoldChange, color = !!as.symbol(input$log2foldchangecolor))) + 
        geom_point() +
        theme(axis.text.x = element_text(size = 8,angle = -90, hjust = 0, vjust=0.5))
    }
    return(plot)
  }else {
    return(NULL)
  }
  })
})
output$log2foldchangegraph <- renderPlot({
  if(is.null(log2foldchangeplot()))return(NULL)
  log2foldchangeplot()
})
downloadPlot(id = "log2foldchangegraphdownload", plotid = log2foldchangeplot())

output$scatter1 <- renderPlot({
  if(is.null(volcanoplot()))return(NULL)
  volcanoplot()
})

###Geochemistry ---------
output$geochemistryfileupload <- renderUI({
  output <- tagList(
    fileInput("geochemistrydata", "Choose CSV File",
              multiple = FALSE,
              accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv"))
    ,
    tags$hr(),
    checkboxInput("header", "Header", TRUE),
    radioButtons("sep", "Separator",
                 choices = c(Comma = ",",
                             Semicolon = ";",
                             Tab = "\t"),
                 selected = ",")
    ,
    radioButtons("quote", "Quote",
                 choices = c(None = "",
                             "Double Quote" = '"',
                             "Single Quote" = "'"),
                 selected = '"')
    ,
    tags$hr(),
    radioButtons("disp", "Display",
                 choices = c(Head = "head",
                             All = "all"),
                 selected = "head"))
  return(output)
})
geochemistrydata <- reactiveValues(path = NULL)
observeEvent(input$geochemistrydata, {
  req(input$geochemistrydata)
  if(tolower(tools::file_ext(input$geochemistrydata$datapath)) == "csv"){
    if(!is.null(phenotypedata$table)){
      geochemistrydata$table <- read.csv(input$geochemistrydata$datapath, sep = input$sep, quote = input$quote, 
                                      header = input$header)
      if(length(intersect(names(geochemistrydata$table), names(phenotypedata$table))) >= 1){
        geochemistrydata$table <- read.csv(input$geochemistrydata$datapath, sep = input$sep, quote = input$quote, 
                                        header = input$header)
        geochemistrydata$table <- left_join(geochemistrydata$table, phenotypedata$table) %>% na.omit()
      } else {
        geochemistrydata$table <- read.csv(input$geochemistrydata$datapath, sep = input$sep, quote = input$quote, 
                                        header = input$header)
      }
    }else if(is.null(phenotypedata$table)){
      geochemistrydata$table <- read.csv(input$geochemistrydata$datapath, sep = input$sep, quote = input$quote, 
                                      header = input$header)
    }
  }else{
    showNotification("File Type Not Recognized", duration = 5, type = "error")
  }
})

observe({
  req(geochemistrydata$table)
  names <- names(dplyr::select_if(geochemistrydata$table, is.numeric))
  geochemistrydata$melt <- geochemistrydata$table %>% pivot_longer(cols = all_of(names),
                                                             names_to = "Measure",
                                                             values_to = "Value")
})
output$geochemistryvariableclassUI <- renderUI({
  req(geochemistrydata$table)
  output <- tagList(
    hr()
    ,
    tags$h5("If R recognizes a column variable incorrectly, you may 
            alter it here")
    ,
    selectInput("geochemistrycolnames", "Select Variable to Alter",
                choices = c("NULL", 
                            as.list(colnames(geochemistrydata$table))),
                selected = "NULL")
    ,
    selectInput("geochemistryclassoptions", "Desired Variable Class",
                choices = c("NULL" = "NULL",
                            "Numeric" = "numeric",
                            "Factor" = "factor",
                            "Character" = "character",
                            "Logical" = "logical"),
                selected = "NULL")
    ,
    actionButton("geochemistrychangeclass", "Change Class", width = "100%")
  )
  return(output)
})
#code to change class
observeEvent(input$geochemistrychangeclass,{
  geochemistrydata$table <- eval(parse(text = paste0("geochemistrydata$table %>% mutate(",
                                                  input$geochemistrycolnames,
                                                  " = as.",
                                                  input$geochemistryclassoptions,
                                                  "(",
                                                  input$geochemistrycolnames,
                                                  "))")))
})
#View Table of file
output$geochemistrydatatable <- renderDataTable({
  req(geochemistrydata$table)
  geochemistrydata$table
})
#print summary of file
output$geochemistrydatasummary <- renderPrint({
  req(geochemistrydata$table)
  summary(geochemistrydata$table)
})
output$geochemistryuploadmain <- renderUI({
  req(geochemistrydata$table)
  output <- tagList(
    splitLayout(dataTableOutput("geochemistrydatatable"))
    ,
    verbatimTextOutput("geochemistrydatasummary")
  )
  return(output)
})
#this will filter the dataset in order to calculate the C to N to P ratio
geochemCNP <- reactive({
  req(geochemistrydata$table)
  select(geochemistrydata$table,"sampleID", "Nitrate", "Phosphorus", "Total_Carbon", "Potassium")
})
output$geochemtable <- renderTable({
  req(geochemistrydata$table)
  head(geochemistrydata$table)
})
output$CNP <- renderText({
  req(geochemistrydata$table)
  minimum <- min(c(geochemistrydata$table$Nitrate, geochemistrydata$table$Phosphorus, geochemistrydata$table$Total_Carbon))
  A <- ceiling(mean(geochemistrydata$table$Total_Carbon)/minimum)
  B <- ceiling(mean(geochemistrydata$table$Nitrate)/minimum)
  C <- ceiling(mean(geochemistrydata$table$Phosphorus)/minimum)
  paste(A, B, C, sep = ":")
})
output$NPK <- renderText({
  req(geochemistrydata$table)
  minimum <- min(c(geochemistrydata$table$Nitrate, geochemistrydata$table$Phosphorus, geochemistrydata$table$Potassium))
  A1 <- ceiling(mean(geochemistrydata$table$Nitrate)/minimum)
  B1 <- ceiling(mean(geochemistrydata$table$Phosphorus)/minimum)
  C2 <- ceiling(mean(geochemistrydata$table$Potassium)/minimum)
  paste(A1,B1,C2, sep = ":")
})
output$ssc <- renderTable({
  req(geochemistrydata$table)
  sand <- mean(geochemistrydata$table$Sand)
  silt <- mean(geochemistrydata$table$Silt)
  clay <- mean(geochemistrydata$table$Clay)
  ssc <- data.frame(name = c("Sand", "Silt", "Clay"), 
                    grain_size = c(sand, silt, clay))
  ssc
})

output$texturetriangle <- renderUI({
  includeHTML("www/sand_silt_clay_index.html")
})
output$geochemistryplotUI <- renderUI({
  req(geochemistrydata$table)
  output <- tagList(
    shiny::selectInput("geochemistryplottype", "Select Plot Type",
                       choices = c("Histogram" = "histogram",
                                   "Scatter" = "scatter",
                                   "Boxplot" = "boxplot"),
                       selected = "histogram")
    ,
    conditionalPanel(condition = "input.geochemistryplottype == 'histogram'",
                     radioButtons("geochemistryhistplottype", "Select View",
                                  choices = c("Count" = "count",
                                              "Density" = "density"),
                                  selected = "count"))
    ,
    conditionalPanel(condition = "input.geochemistryplottype == 'histogram' ||
                     input.geochemistryplottype == 'scatter'",
                     selectInput("geochemistryx", "Select Variable to Graph Along X-Axis:", 
                                 choices = c("NULL", colnames(dplyr::select_if(geochemistrydata$table, is.numeric))),
                                 selected = "NULL",
                                 multiple = FALSE))
    ,
    conditionalPanel(condition = "input.geochemistryplottype == 'boxplot'",
                     selectInput("geochemistryx1", "Select Variable(s) to Graph Along X-Axis:",
                                 choices = c("NULL", colnames(dplyr::select_if(geochemistrydata$table, is.numeric))),
                                 selected = "NULL",
                                 multiple = TRUE))
    ,
    conditionalPanel(condition = "input.geochemistryplottype == 'scatter'",
                     selectInput("geochemistryy", "Select Variable to Graph Along Y Axis:",
                                 choices = c("NULL", colnames(dplyr::select_if(geochemistrydata$table, is.numeric))),
                                 selected = "NULL",
                                 multiple = FALSE))
    ,
    conditionalPanel(condition = "input.geochemistryplottype == 'histogram'",
                     numericInput("geochemistrybinwidth", "Select Bin Width",
                                  value = 1, min = 0, max = 100))
    ,
    textInput("geochemistrytitle", "Create Title for Plot",
              placeholder = "Plot Title")
    ,
    conditionalPanel(condition= "input.geochemistryplottype == 'histogram'",
                     textInput("geochemistryxaxislabel", "Create X Axis Label",
                               placeholder = "X Axis Label"))
    ,
    conditionalPanel(condition = "input.geochemistryplottype == 'scatter' || 
                     input.geochemistryplottype=='boxplot'",
                     textInput("geochemistryxaxislabel1", "Create X Axis Label",
                               placeholder = "X Axis Label"),
                     textInput("geochemistryyaxislabel1", "Create Y Axis Label",
                               placeholder = "Y Axis Label"))
    ,
    conditionalPanel(condition= "input.geochemistrycoloroption != 'NULL'",
                     textInput("geochemistrylegendlabel1", "Create Title For Legend",
                               placeholder = "Legend Title"))
    ,
    conditionalPanel(condition = "input.geochemistryplottype == 'histogram'",
                     colourpicker::colourInput("geochemistrycolor", "Select Bar Color",
                                 value = "white"))
    ,
    conditionalPanel(condition = "input.geochemistryplottype == 'scatter' ||
                     input.geochemistryplottype == 'boxplot'",
                     selectInput("geochemistrycoloroption", "Select Factor to Color",
                                 choices = c("NULL", names(dplyr::select_if(geochemistrydata$table, is.character)),
                                             names(dplyr::select_if(geochemistrydata$table, is.factor))),
                                 selected = "NULL"))
    ,
    conditionalPanel(condition= "input.geochemistryplottype == 'scatter'",
                     radioButtons("geochemistryregressionline", "Add Linear Regression?",
                                  choices = c("Yes",
                                              "No"),
                                  selected = "No"))
    ,
    conditionalPanel(condition = "input.geochemistryplottype == 'boxplot'",
                     radioButtons("geochemistryfreeyaxis", "Free Y Axis?",
                                  choices = c("Yes",
                                              "No"),
                                  selected = "Yes"))
    ,
    conditionalPanel(condition = "input.geochemistryplottype == 'scatter' 
                     && input.geochemistrycoloroption == 'NULL'",
                     colourpicker::colourInput("geochemistrycolor1", "Select Color",
                                 value = "black"))
    ,
    downloadPlotUI("geochemistryplotdownload")
  )
  return(output)
})
downloadPlot("geochemistryplotdownload", geochemistryplot())
geochemistryplot <- reactive({
  req(geochemistrydata$table)
  if(input$geochemistryplottype == "histogram"){
    if(!is.null(av(input$geochemistryx))){
      if(input$geochemistryhistplottype == "count"){
        plot <- ggplot(geochemistrydata$table, aes(x = !!as.symbol(input$geochemistryx))) +
          geom_histogram(fill = input$geochemistrycolor, color = "black",
                         binwidth = input$geochemistrybinwidth) +
          labs(title = input$geochemistrytitle, y = "Sample Count",
               x = input$geochemistryxaxislabel)
      }else{
        plot <- ggplot(geochemistrydata$table, aes(x = !!as.symbol(input$geochemistryx))) +
          geom_histogram(stat = "density",fill = input$geochemistrycolor, color = "black") +
          labs(title = input$geochemistrytitle, y = "Sample Count",
               x = input$geochemistryxaxislabel)
      }
    }else {
      plot <- NULL
    }
  }else if(input$geochemistryplottype == "scatter"){
    if(!is.null(av(input$geochemistryx)) && !is.null(av(input$geochemistryy))){
      if(!is.null(av(input$geochemistrycoloroption))){
        plot <- ggplot(geochemistrydata$table, aes(x = !!as.symbol(input$geochemistryx),
                                                y = !!as.symbol(input$geochemistryy),
                                                color = !!as.symbol(input$geochemistrycoloroption))) + 
          geom_point() + 
          labs(title= input$geochemistrytitle, x = input$geochemistryxaxislabel1,
               y = input$geochemistryyaxislabel1, color = input$geochemistrylegendlabel1) 
        if(input$geochemistryregressionline == "Yes"){
          plot <- plot + geom_smooth(method = lm, se = FALSE)
        }else {
          plot
        }
      }else{
        plot <- ggplot(geochemistrydata$table, aes(x = !!as.symbol(input$geochemistryx),
                                                y = !!as.symbol(input$geochemistryy))) +
          geom_point(color = input$geochemistrycolor1) + 
          labs(title= input$geochemistrytitle, x = input$geochemistryxaxislabel1,
               y = input$geochemistryyaxislabel1)
        if(input$geochemistryregressionline == "Yes"){
          plot <- plot + geom_smooth(method = lm, se = FALSE)
        }else {
          plot
        }
      }
    }else{
      plot <- NULL
    }
  }else if(input$geochemistryplottype == "boxplot"){
    if(!is.null(av(input$geochemistryx1))){
      filtered_data <- geochemistrydata$melt %>% filter(Measure %in% input$geochemistryx1)
      if(!is.null(av(input$geochemistrycoloroption))){
        plot <- ggplot(filtered_data, aes(x = Measure, y = Value, fill = !!as.symbol(input$geochemistrycoloroption))) +
          geom_boxplot() + 
          labs(title= input$geochemistrytitle, x = input$geochemistryxaxislabel1,
               y = input$geochemistryyaxislabel1, fill = input$geochemistrylegendlabel1) 
      }else {
        plot <- ggplot(filtered_data, aes(x = Measure, y = Value)) +
          geom_boxplot() + 
          labs(title= input$geochemistrytitle, x = input$geochemistryxaxislabel1,
               y = input$geochemistryyaxislabel1) 
      }
      if(input$geochemistryfreeyaxis == "Yes"){
        plot <- plot + facet_wrap(~Measure, scales = "free") +
          theme(axis.text.x = element_blank())
      }else{
        plot <- plot
      }
    }else {
      plot <- NULL
    }
  }
  return(plot)
})
output$geochemistryplot1 <- renderPlot({
  req(geochemistrydata$table)
  geochemistryplot()
})
output$geochemistryplotmainUI <- renderUI({
  req(geochemistrydata$table)
  plotOutput("geochemistryplot1")
})



###Statistics----
output$statselect <- renderUI({
  if(is.null(mappingfile()))return(NULL)
  varSelectInput("statselect1", "Select Variable to Compare:",
                 data = mappingfile(),
                 multiple = FALSE)
})
output$stataction <- renderUI({
  actionButton("stataction1", "Perform Statistics:")
})
#make merged file composed of all uploaded files
mergedfile <- reactive({
  if(is.null(mappingfile())){
    return(NULL)
  }else if(!is.null(mappingfile()) && !is.null(geochemD()) && is.null(phenotypedata())){
    mergeddata <- left_join(geochemD(), mappingfile())
    mergeddata
  }else if(!is.null(mappingfile()) && !is.null(geochemD()) && !is.null(phenotypedata())){
    mergeddata <- left_join(geochemD(), mappingfile())
    mergeddata <- left_join(mergeddata, phenotypedata())
    mergeddata
  }else return(NULL)
})
output$stattable <- renderDataTable({
  if(is.null(mergedfile()))return(NULL)
  mergedfile()
})

statresult <- eventReactive(input$stataction1, {
  results <- list()
  withProgress(message = "Rendering Plot" , {
  for(variables in colnames(select_if(mergedfile(), is.numeric))){  
    results[[variables]] <- kruskal.test(formula(paste(variables, "~", paste(input$statselect1))), 
                                 data = mergedfile())
  }
    results_df1 <- data.frame(matrix(unlist(results), nrow=length(results), byrow=T))%>% 
      select(X1, X3, X5) %>% dplyr::rename(Chi_Squared = X1, P_Value = X3, Variables = X5)
    results_df1
  })
  
})
output$stattest <- renderPrint({
  if(is.null(mergedfile()))return(NULL)
  str(mergedfile())
})
output$statresulttable <- renderDataTable({
  statresult()
})


})



