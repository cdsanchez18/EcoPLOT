#Updates radio buttons to display filtered dataset after filters are applied
observeEvent(input$phenotypefilterrender, {
  updateRadioButtons(session, "phenotypedatasource", "Select Dataset to Use:",
                     choices = c("Original" = "Original",
                                 "Filtered" = "Filtered"),
                     selected = "Original", inline = TRUE)
}, once = TRUE)
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
  isolate({
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
      conditionalPanel("input.phenotypeplottype == 'scatter'",
                       radioButtons("phenotypescatterplottype", "Select Scatter Type",
                                    choices = c("Default (exact values will stack)" = "default",
                                                "Jitter (exact value will be offset)" = "jitter"),
                                    selected = "default",
                                    inline = TRUE))
      ,
      conditionalPanel("input.phenotypeplottype == 'scatter' && input.phenotypescatterplottype == 'jitter'",
                       fluidRow(
                         column(6,
                                numericInput("phenotypescatterplotheight", "Select Vertical Jitter", value = 0.1, min = 0,max = 10,step = 0.1)),
                         column(6,
                                numericInput("phenotypescatterplotwidth", "Select Hortizontal Jitter", value = 0.1, min = 0,max = 10,step = 0.1))))
      ,
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
                                    selected = "stacked", inline = TRUE))
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
                                   choices = c("NULL", colnames(phenotypedata$use)),
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
      numericInput("phenotypeplotheight", "Select Plot Height",
                   value = 800)
      ,
      downloadPlotUI("phenotypeplotdownload")
    )
  })
  return(output)
})
#download phenotype plot
downloadPlot("phenotypeplotdownload", phenotypeplot())
#Phenotype plot
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
          #geom_point() + 
          labs(title= input$phenotypetitle, x = input$phenotypexaxislabel1,
               y = input$phenotypeyaxislabel1, color = input$phenotypelegendlabel1) #+
        if(input$phenotypescatterplottype == "jitter"){
          plot <- plot + geom_jitter(width = input$phenotypescatterplotwidth, height = input$phenotypescatterplotheight)
        }else{
          plot <- plot + geom_point()
        }
        if(input$phenotyperegressionline == "Yes"){
          plot <- plot + geom_smooth(method = lm, se = FALSE)
        }else {
          plot
        }
      }else{
        plot <- ggplot(phenotypedata$use, aes(x = !!as.symbol(input$phenotypex1),
                                              y = !!as.symbol(input$phenotypey))) +
          #geom_point(color = input$phenotypecolor1) + 
          labs(title= input$phenotypetitle, x = input$phenotypexaxislabel1,
               y = input$phenotypeyaxislabel1)
        if(input$phenotypescatterplottype == "jitter"){
          plot <- plot + geom_jitter(width = input$phenotypescatterplotwidth, height = input$phenotypescatterplotheight)
        }else{
          plot <- plot + geom_point(color = input$phenotypecolor1)
        }
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
#prints correlation coefficient when viewing a scatter plot
output$phenotypecorrelation <- renderPrint({
  req(phenotypedata$table)
  if(input$phenotypeplottype == "scatter"){
    if(!is.null(av(input$phenotypex1)) && !is.null(av(input$phenotypey))){
      if(is.numeric(input$phenotypex1) && is.numeric(input$phenotypey)){
      paste("Pearson's Correlation Coefficient:", cor(phenotypedata$use[[input$phenotypex1]], phenotypedata$use[[input$phenotypey]]))
      }else{
        "Pearson's Correlation Coefficient: NA"
      }
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
  } else if(input$phenotypeplottype != "scatter"){
    NULL
  }
})


output$phenotypeplot1 <- renderPlot({
  req(phenotypedata$table)
  phenotypeplot()
})
##ui options for phenotype plot brush
output$phenotypeplotmainUI <- renderUI({
  req(phenotypedata$table)
  plotOutput("phenotypeplot1", brush = "phenotypebrush", height = input$phenotypeplotheight)
})
#tells you which points you have selected 
output$phenotypebrushtest <- renderPrint({
  req(phenotypedata$table)
  req(input$phenotypeplottype == "scatter")
  brushedPoints(phenotypedata$use, input$phenotypebrush)
})
#dynamic selection UI
output$phenotypedynamicselectbuttons <- renderUI({
  req(phenotypedata$table)
  req(input$phenotypeplottype == "scatter")
  output <- tagList(
    checkboxInput("phenotypesidebarhide", label = "Hide Sidebar Panel?", value = FALSE)
    ,
    fluidRow(
      column(4,
             hr()),
      column(4,
             tags$div(tags$h4(tags$b("Dynamic Selection")), align = "center")),
      column(4,
             hr())
    ),
    tags$div(tags$h4("Dynamic Selection allows users to create new variables within their dataset that capture unique patterns or trends 
    not explained within their experimental design. EcoPLOT allows for the creation of up to 10 unique groupings within a created variable.
                     To get started, create a name for you new variable and drag your mouse to select points of interest. 
                     Clicking 'Save Selection' will group those points together under a name of your choosing. This process can be repeated
                     to distinguish different groupings under the same variable heading. All created variables can be used in all graphical and 
                     statistical analyses within EcoPLOT."), align = "center")
    ,
    tags$div(style = "padding:10px")
    ,
    fluidRow(
      column(8,
             column(4,
                    textInput("phenotypecolumnName", "Create Name for Variable",
                              value = "New Variable"))
             ,
             conditionalPanel("input.phenotypesaveselection",
             column(4,
                    textInput("phenotypeselectionName1", "Name for Group 1",
                              value = "Group 1")
             )
             ,
             uiOutput("phenotypecontainer")
             ,
             #fluidRow(
               column(4,
                      textInput("phenotypenotext", "Name for Points Not Grouped",
                                value = "Not Grouped"))
             #)
      )
      ),
      column(4,
             actionButton("phenotypesaveselection", "Save Selection", width = "100%")
             ,
             conditionalPanel(condition = "input.phenotypesaveselection",
                              hr()
                              ,
                              actionButton("phenotypeseparateselection", "Save Selection with New Grouping", width = "100%")
             
             ,
             hr()
             ,
             actionButton("phenotypeactionbutton", "Save Variable", width = "100%")
             ,
             hr()
             ,
             actionButton("phenotyperesetselection", "Reset Groupings", width = "100%")
             )
      )
    )
    ,
    hr()
    ,
    fluidRow(
      column(6,
             tags$h4("Points Currently Selected"),
             verbatimTextOutput("phenotypebrushtest")
      ),
      column(6,
             tags$h4("Group Summary"),
             splitLayout(verbatimTextOutput("phenoypetable1")))
    ),
    hr(),
    tags$h4("View Newly Created Variable in Your Data"),
    splitLayout(dataTableOutput("phenotypetesttable"))
  )
})

observeEvent(input$phenotypesaveselection, {
  updateActionButton(
  inputId = "phenotypesaveselection",
  label = "Save Selected to Current Grouping")
})
observeEvent(input$phenotyperesetselection, {
  updateActionButton(
    inputId = "phenotypesaveselection",
    label = "Save Selected")
})
observeEvent(input$phenotypeseparateselection, {
  updateActionButton(
    inputId = "phenotypesaveselection",
    label = "Save Selected to Current Grouping")
})
observeEvent(input$phenotyperesetselection, {
  shinyjs::hide("phenotypeseparateselection")
})
observeEvent(input$phenotypesaveselection, {
  shinyjs::show("phenotypeseparateselection")
})
####Dynamically select multiple points 
phenotypeselections <- reactiveValues()
phenotypeselections$samples <- data.frame()
#add selection to dataframe
observeEvent(input$phenotypesaveselection, {
  IDpos <- which(grepl("ID", colnames(phenotypedata$use)))[1]
  newLine <- brushedPoints(phenotypedata$use, input$phenotypebrush)[IDpos]
  phenotypeselections$samples <- rbindPad(data = phenotypeselections$samples, selections = newLine)
  return(phenotypeselections$samples)
})
#add selection as different grouping 
observeEvent(input$phenotypeseparateselection, {
  if(ncol(phenotypeselections$samples) == 1 || ncol(phenotypeselections$samples) < 10 && ncol(phenotypeselections$samples >1)){
    IDpos <- which(grepl("ID", colnames(phenotypedata$use)))[1]
    newGrouping <- brushedPoints(phenotypedata$use, input$phenotypebrush)[IDpos]
    phenotypeselections$samples <- cbindPad(phenotypeselections$samples, newGrouping)
    phenotypeselections$samples[do.call(order, phenotypeselections$samples),]
  }else{
    NULL
  }
})
observeEvent(input$phenotyperesetselection, {
  phenotypeselections$samples <- data.frame()
})
observeEvent(input$phenotyperesetselection, {
  removeUI(
    selector = '#phenotypeselection2, #phenotypeselection3, #phenotypeselection4, #phenotypeselection5, 
    #phenotypeselection6, #phenotypeselection7, #phenotypeselection8, #phenotypeselection9, #phenotypeselection10',
    multiple = TRUE
  )
})
observeEvent(input$phenotyperesetselection, {
  phenotypecounter(1)
})
#make dynamic number of UI elements for column naming
phenotypecounter <- reactiveVal(1)
observeEvent(input$phenotypeseparateselection, {
  if(ncol(phenotypeselections$samples) == 1 || ncol(phenotypeselections$samples) < 11 && ncol(phenotypeselections$samples >1)){
    phenotypecounter1 <<- phenotypecounter() + 1
    phenotypecounter(phenotypecounter1)
    if(phenotypecounter() < 11){
      insertUI(
        selector = '#phenotypecontainer',
        where = "beforeEnd",
        ui = column(4,
                    tags$div(textInput(paste("phenotypeselectionName", paste(phenotypecounter()), sep = ""), paste("Name for Group", paste(phenotypecounter())),
                                       value = paste("Group", paste(phenotypecounter()))),
                             id = paste0("phenotypeselection", paste(phenotypecounter())))
        )
      )
    }else{NULL}
  } else if(ncol(phenotypeselections$samples) == 0){
    showNotification(ui = "You Must First Make A Preliminary Selection",
                     type = "error")
  } else if(ncol(phenotypeselections$samples) >=11){
    NULL
  }
})
observeEvent(input$phenotypeseparateselection, {
  if(phenotypecounter() >= 10){
    showNotification(ui= "You Have Made the Maximum Number of Selections",
                     action = a(href = "javascript:location.reload();", "Reload page"),
                     duration = NULL, 
                     type = "error")
  }else {
    NULL
  }
})
#this produces the table to view selected points
output$phenoypetable1 <- renderPrint({
  req(phenotypedata$table)
  req(input$phenotypeplottype == "scatter")
  print(as.list(phenotypeselections$samples), na.print = "")
})

#dynamically name selections and update the table with the new names
phenotypetest <- reactiveValues()
phenotypetest$list <- c()
observe({
  if(phenotypecounter() == 1){
    name1 <- input$phenotypeselectionName1
    phenotypetest$list <- c(name1)
  }else if(phenotypecounter() == 2){
    name1 <- input$phenotypeselectionName1
    name2 <- input$phenotypeselectionName2
    phenotypetest$list <- c(name1, name2)
  }else if(phenotypecounter() == 3){
    name1 <- input$phenotypeselectionName1
    name2 <- input$phenotypeselectionName2
    name3 <- input$phenotypeselectionName3
    phenotypetest$list <- c(name1, name2, name3)
  }else if(phenotypecounter() == 4){
    name1 <- input$phenotypeselectionName1
    name2 <- input$phenotypeselectionName2
    name3 <- input$phenotypeselectionName3
    name4 <- input$phenotypeselectionName4
    phenotypetest$list <- c(name1, name2, name3, name4)
  }else if(phenotypecounter() == 5){
    name1 <- input$phenotypeselectionName1
    name2 <- input$phenotypeselectionName2
    name3 <- input$phenotypeselectionName3
    name4 <- input$phenotypeselectionName4
    name5 <- input$phenotypeselectionName5
    phenotypetest$list <- c(name1, name2, name3, name4, name5)
  }else if(phenotypecounter() == 6){
    name1 <- input$phenotypeselectionName1
    name2 <- input$phenotypeselectionName2
    name3 <- input$phenotypeselectionName3
    name4 <- input$phenotypeselectionName4
    name5 <- input$phenotypeselectionName5
    name6 <- input$phenotypeselectionName6
    phenotypetest$list <- c(name1, name2, name3, name4, name5, name6)
  }else if(phenotypecounter() == 7){
    name1 <- input$phenotypeselectionName1
    name2 <- input$phenotypeselectionName2
    name3 <- input$phenotypeselectionName3
    name4 <- input$phenotypeselectionName4
    name5 <- input$phenotypeselectionName5
    name6 <- input$phenotypeselectionName6
    name7 <- input$phenotypeselectionName7
    phenotypetest$list <- c(name1, name2, name3, name4, name5, name6, name7)
  }else if(phenotypecounter() == 8){
    name1 <- input$phenotypeselectionName1
    name2 <- input$phenotypeselectionName2
    name3 <- input$phenotypeselectionName3
    name4 <- input$phenotypeselectionName4
    name5 <- input$phenotypeselectionName5
    name6 <- input$phenotypeselectionName6
    name7 <- input$phenotypeselectionName7
    name8 <- input$phenotypeselectionName8
    phenotypetest$list <- c(name1, name2, name3, name4, name5, name6, name7, name8)
  }else if(phenotypecounter() == 9){
    name1 <- input$phenotypeselectionName1
    name2 <- input$phenotypeselectionName2
    name3 <- input$phenotypeselectionName3
    name4 <- input$phenotypeselectionName4
    name5 <- input$phenotypeselectionName5
    name6 <- input$phenotypeselectionName6
    name7 <- input$phenotypeselectionName7
    name8 <- input$phenotypeselectionName8
    name9 <- input$phenotypeselectionName9
    phenotypetest$list <- c(name1, name2, name3, name4, name5, name6, name7, name8, name9)
  }else if(phenotypecounter() == 10){
    name1 <- input$phenotypeselectionName1
    name2 <- input$phenotypeselectionName2
    name3 <- input$phenotypeselectionName3
    name4 <- input$phenotypeselectionName4
    name5 <- input$phenotypeselectionName5
    name6 <- input$phenotypeselectionName6
    name7 <- input$phenotypeselectionName7
    name8 <- input$phenotypeselectionName8
    name9 <- input$phenotypeselectionName9
    name10 <- input$phenotypeselectionName10
    phenotypetest$list <- c(name1, name2, name3, name4, name5, name6, name7, name8, name9, name10)
  }
  return(phenotypetest$list)
})
observe({
  if(ncol(phenotypeselections$samples) == 1 || ncol(phenotypeselections$samples) < 11 && ncol(phenotypeselections$samples >1)){
    colnames(phenotypeselections$samples) <- phenotypetest$list
  }else return(NULL)
})
observeEvent(input$phenotypeactionbutton, {
  req(phenotypedata$table)
  #adds column to original dataset
  IDpos <- which(grepl("ID", colnames(phenotypedata$table1)))[1]
  IDposname <- names(phenotypedata$table1[which(grepl("ID", colnames(phenotypedata$table1)))[1]])
  columnadd <- pivot_longer(phenotypeselections$samples, everything(), names_to = input$phenotypecolumnName, values_to = IDposname) %>% unique()
  columnadd[[2]][duplicated(columnadd[[2]])] <- NA
  columnadd <- na.omit(columnadd)
  variables <- data.frame(phenotypedata$table1[[IDpos]])
  names(variables)[1] <- IDposname
  columnadd <- right_join(x = columnadd, y = variables, by = IDposname) %>% unique()
  columnadd[is.na(columnadd)] <- input$phenotypenotext
  phenotypedata$table1 <- left_join(x = phenotypedata$table1, y = columnadd, by = IDposname)
  
  #adds column to filtered dataset
  IDpos2 <- which(grepl("ID", colnames(phenotypedata$filter)))[1]
  IDposname2 <- names(phenotypedata$filter[which(grepl("ID", colnames(phenotypedata$filter)))[1]])
  columnadd2 <- pivot_longer(phenotypeselections$samples, everything(), names_to = input$phenotypecolumnName, values_to = IDposname) %>% unique()
  columnadd2[[2]][duplicated(columnadd2[[2]])] <- NA
  columnadd2 <- na.omit(columnadd2)
  variables2 <- data.frame(phenotypedata$filter[[IDpos]])
  names(variables2)[1] <- IDposname2
  columnadd2 <- right_join(x = columnadd2, y = variables2, by = IDposname2) %>% unique()
  columnadd2[is.na(columnadd2)] <- input$phenotypenotext
  phenotypedata$filter <- left_join(x = phenotypedata$filter, y = columnadd2, by = IDposname2)
  #phenotypedata$filter[is.na(phenotypedata$filter)] <- input$phenotypenotext
})
#Make Updated table
output$phenotypetesttable <- renderDataTable({
  req(phenotypedata$table)
  req(input$phenotypeplottype == "scatter")
  phenotypedata$use
})
