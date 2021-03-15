##environment plot UI Options-----
output$environmentplotUI <- renderUI({
  req(environmentdata$table)
  isolate({
    output <- tagList(
      shiny::selectInput("environmentplottype", "Select Plot Type",
                         choices = c("Histogram" = "histogram",
                                     "Scatter" = "scatter",
                                     "Boxplot" = "boxplot",
                                     "Barplot" = "barplot"),
                         selected = "histogram")
      ,
      conditionalPanel("input.environmentplottype == 'barplot'",
                       radioButtons("environmentbarplottype", "Select View",
                                    choices = c("Count of Cases" = "bin",
                                                "Values of Column" = "identity"),
                                    selected = "bin",
                                    inline = TRUE))
      ,
      conditionalPanel("input.environmentplottype == 'scatter'",
                       radioButtons("environmentscatterplottype", "Select Scatter Type",
                                    choices = c("Default (exact values will stack)" = "default",
                                                "Jitter (exact value will be offset)" = "jitter"),
                                    selected = "default",
                                    inline = TRUE))
      ,
      conditionalPanel("input.environmentplottype == 'scatter' && input.environmentscatterplottype == 'jitter'",
                       fluidRow(
                         column(6,
                       numericInput("environmentscatterplotheight", "Select Vertical Jitter", value = 0.1, min = 0,max = 10,step = 0.1)),
                       column(6,
                       numericInput("environmentscatterplotwidth", "Select Hortizontal Jitter", value = 0.1, min = 0,max = 10,step = 0.1))))
      ,
      conditionalPanel("input.environmentplottype == 'barplot'",
                       selectInput("environmentbarplotxaxis", "Select X Axis Variable",
                                   choices = c("NULL", colnames(dplyr::select_if(environmentdata$use, is.factor)),
                                               colnames(dplyr::select_if(environmentdata$use, is.character))
                                   ),
                                   selected = "NULL"))
      ,
      conditionalPanel("input.environmentplottype == 'barplot' && input.environmentbarplottype == 'identity'",
                       selectInput("environmentbarplotyaxis", "Select Y Axis Variable",
                                   choices = c("NULL", colnames(environmentdata$use)),
                                   selected = "NULL"))
      ,
      conditionalPanel("input.environmentplottype == 'barplot'",
                       selectInput("environmentbarplotfill", "Select Variable to Fill",
                                   choices = c("NULL", colnames(environmentdata$use)),
                                   selected = "NULL"))
      ,
      conditionalPanel("input.environmentplottype == 'barplot' && input.environmentbarplotfill != 'NULL'",
                       radioButtons("environmentbarplotpos", "Bar Plot Type",
                                    choices = c("Stacked" = "stacked",
                                                "Dodged" = "dodge"),
                                    selected = "stacked", inline = TRUE))
      ,
      conditionalPanel("input.environmentplottype == 'barplot' && input.environmentbarplottype == 'identity'",
                       radioButtons("environmentbarploterror", "Show Error Bars?",
                                    choices = c("Yes" = "yes",
                                                "No" = "no"),
                                    selected = "no",
                                    inline = TRUE))
      ,
      conditionalPanel(condition = "input.environmentplottype == 'histogram'",
                       radioButtons("environmenthistplottype", "Select View",
                                    choices = c("Count" = "count",
                                                "Density" = "density"),
                                    selected = "count",
                                    inline = TRUE))
      ,
      conditionalPanel(condition = "input.environmentplottype == 'histogram'",
                       selectInput("environmentx", "Select Variable to Graph Along X-Axis:", 
                                   choices = c("NULL", colnames(dplyr::select_if(environmentdata$use, is.numeric))),
                                   selected = "NULL",
                                   multiple = FALSE))
      ,
      conditionalPanel(condition =  "input.environmentplottype == 'scatter'",
                       selectInput("environmentx1", "Select Variable to Graph Along X-Axis:", 
                                   choices = c("NULL", colnames(environmentdata$use)),
                                   selected = "NULL",
                                   multiple = FALSE))
      ,
      conditionalPanel(condition = "input.environmentplottype == 'boxplot'",
                       selectInput("environmentx2", "Select Variable(s) to Graph Along X-Axis:",
                                   choices = c("NULL", colnames(dplyr::select_if(environmentdata$use, is.numeric))),
                                   selected = "NULL",
                                   multiple = TRUE))
      ,
      conditionalPanel(condition = "input.environmentplottype == 'scatter'",
                       selectInput("environmenty", "Select Variable to Graph Along Y Axis:",
                                   choices = c("NULL", colnames(dplyr::select_if(environmentdata$use, is.numeric))),
                                   selected = "NULL",
                                   multiple = FALSE))
      ,
      conditionalPanel(condition = "input.environmentplottype == 'histogram' || input.environmentplottype == 'scatter'",
                       selectInput("environmentfacet", "Select Variable to Facet Around",
                                   choices = c("NULL", colnames(dplyr::select_if(environmentdata$use, is.character)),
                                               colnames(dplyr::select_if(environmentdata$use, is.factor))),
                                   selected = "NULL",
                                   multiple = FALSE))
      ,
      conditionalPanel(condition = "input.environmentplottype == 'histogram'",
                       numericInput("environmentbinwidth", "Select Bin Width",
                                    value = 1, min = 0, max = 100))
      ,
      textInput("environmenttitle", "Create Title for Plot",
                placeholder = "Plot Title")
      ,
      conditionalPanel(condition= "input.environmentplottype == 'histogram'",
                       textInput("environmentxaxislabel", "Create X Axis Label",
                                 placeholder = "X Axis Label"))
      ,
      conditionalPanel(condition = "input.environmentplottype == 'scatter' || 
                     input.environmentplottype=='boxplot'",
                       textInput("environmentxaxislabel1", "Create X Axis Label",
                                 placeholder = "X Axis Label"),
                       textInput("environmentyaxislabel1", "Create Y Axis Label",
                                 placeholder = "Y Axis Label"))
      ,
      conditionalPanel(condition= "input.environmentcoloroption != 'NULL'",
                       textInput("environmentlegendlabel1", "Create Title For Legend",
                                 placeholder = "Legend Title"))
      ,
      conditionalPanel(condition = "input.environmentplottype == 'histogram'",
                       colourpicker::colourInput("environmentcolor", "Select Bar Color",
                                                 value = "white"))
      ,
      conditionalPanel(condition = "input.environmentplottype == 'scatter' ||
                     input.environmentplottype == 'boxplot'",
                       selectInput("environmentcoloroption", "Select Factor to Color",
                                   choices = c("NULL", names(dplyr::select_if(environmentdata$use, is.character)),
                                               names(dplyr::select_if(environmentdata$use, is.factor))),
                                   selected = "NULL"))
      ,
      conditionalPanel(condition= "input.environmentplottype == 'scatter'",
                       radioButtons("environmentregressionline", "Add Linear Regression?",
                                    choices = c("Yes",
                                                "No"),
                                    selected = "No", inline=TRUE))
      ,
      conditionalPanel(condition = "input.environmentplottype == 'boxplot'",
                       radioButtons("environmentfreeyaxis", "Free Y Axis?",
                                    choices = c("Yes",
                                                "No"),
                                    selected = "Yes", inline=TRUE))
      ,
      conditionalPanel(condition = "input.environmentplottype == 'scatter' 
                     && input.environmentcoloroption == 'NULL'",
                       colourpicker::colourInput("environmentcolor1", "Select Color",
                                                 value = "black"))
      ,
      numericInput("environmentplotheight", "Select Plot Height",
                   value = 500)
      ,
      downloadPlotUI("environmentplotdownload")
    )
  })
  return(output)
})
#download environment plot
downloadPlot("environmentplotdownload", environmentplot())
#Environment plot
environmentplot <- reactive({
  req(environmentdata$table)
  if(input$environmentplottype == "histogram"){
    if(!is.null(av(input$environmentx))){
      if(input$environmenthistplottype == "count"){
        plot <- ggplot(environmentdata$use, aes(x = !!as.symbol(input$environmentx))) +
          geom_histogram(fill = input$environmentcolor, color = "black",
                         binwidth = input$environmentbinwidth) +
          labs(title = input$environmenttitle, y = "Sample Count",
               x = input$environmentxaxislabel)
      }else{
        plot <- ggplot(environmentdata$use, aes(x = !!as.symbol(input$environmentx))) +
          geom_histogram(stat = "density",fill = input$environmentcolor, color = "black") +
          labs(title = input$environmenttitle, y = "Sample Count",
               x = input$environmentxaxislabel)
      }
      if(!is.null(av(input$environmentfacet))){
        environmentdata$use[[input$environmentfacet]] %>% sort()
        plot <- plot + facet_wrap(paste("~", input$environmentfacet))
      }else{
        plot <- plot
      }
    }else {
      plot <- NULL
    }
  }else if(input$environmentplottype == "scatter"){
    if(!is.null(av(input$environmentx1)) && !is.null(av(input$environmenty))){
      if(!is.null(av(input$environmentcoloroption))){
        plot <- ggplot(environmentdata$use, aes(x = !!as.symbol(input$environmentx1),
                                                y = !!as.symbol(input$environmenty),
                                                color = !!as.symbol(input$environmentcoloroption))) + 
          #geom_point() + 
          labs(title= input$environmenttitle, x = input$environmentxaxislabel1,
               y = input$environmentyaxislabel1, color = input$environmentlegendlabel1)
        if(input$environmentscatterplottype == "jitter"){
          plot <- plot + geom_jitter(width = input$environmentscatterplotwidth, height = input$environmentscatterplotheight)
        }else{
          plot <- plot + geom_point()
        }
        if(input$environmentregressionline == "Yes"){
          plot <- plot + geom_smooth(method = lm, se = FALSE)
        }else {
          plot
        }
        if(!is.null(av(input$environmentfacet))){
          plot <- plot + facet_wrap(paste("~", input$environmentfacet))
        }else{
          plot
        }
      }else{
        plot <- ggplot(environmentdata$use, aes(x = !!as.symbol(input$environmentx1),
                                                y = !!as.symbol(input$environmenty))) +
          #geom_point(color = input$environmentcolor1) + 
          labs(title= input$environmenttitle, x = input$environmentxaxislabel1,
               y = input$environmentyaxislabel1)
        if(input$environmentscatterplottype == "jitter"){
          plot <- plot + geom_jitter(width = input$environmentscatterplotwidth, height = input$environmentscatterplotheight)
        }else{
          plot <- plot + geom_point(color = input$environmentcolor1)
        }
        if(input$environmentregressionline == "Yes"){
          plot <- plot + geom_smooth(method = lm, se = FALSE)
        }else {
          plot
        }
      }
      if(!is.null(av(input$environmentfacet))){
        environmentdata$use[[input$environmentfacet]] %>% sort()
        plot <- plot + facet_wrap(paste("~", input$environmentfacet))
      }else{
        plot <- plot
      }
    }else{
      plot <- NULL
    }
  }else if(input$environmentplottype == "boxplot"){
    if(!is.null(av(input$environmentx2))){
      filtered_data <- environmentdata$melt %>% filter(Measure %in% input$environmentx2)
      if(!is.null(av(input$environmentcoloroption))){
        plot <- ggplot(filtered_data, aes(x = Measure, y = Value, fill = !!as.symbol(input$environmentcoloroption))) +
          geom_boxplot() + 
          labs(title= input$environmenttitle, x = input$environmentxaxislabel1,
               y = input$environmentyaxislabel1, fill = input$environmentlegendlabel1) 
      }else {
        plot <- ggplot(filtered_data, aes(x = Measure, y = Value)) +
          geom_boxplot() + 
          labs(title= input$environmenttitle, x = input$environmentxaxislabel1,
               y = input$environmentyaxislabel1) 
      }
      if(input$environmentfreeyaxis == "Yes"){
        plot <- plot + facet_wrap(~Measure, scales = "free") +
          theme(axis.text.x = element_blank())
      }else{
        plot <- plot
      }
    }else {
      plot <- NULL
    }
  }else if(input$environmentplottype == "barplot"){
    if(input$environmentbarplottype == "identity"){
      if(!is.null(av(input$environmentbarplotxaxis)) && !is.null(av(input$environmentbarplotyaxis))){
        if(!is.null(av(input$environmentbarplotfill))){
          data1 <- data_summary(data = environmentdata$use, varname = input$environmentbarplotyaxis,
                                groupnames = c(input$environmentbarplotfill,
                                               input$environmentbarplotxaxis))
          plot <- ggplot(data1, aes(x = !!as.symbol(input$environmentbarplotxaxis), 
                                    y = !!as.symbol(input$environmentbarplotyaxis),
                                    fill = !!as.symbol(input$environmentbarplotfill)))
        }else {
          data1 <- EcoPLOT::data_summary(data = environmentdata$use, varname = input$environmentbarplotyaxis,
                                         groupnames = input$environmentbarplotxaxis)
          
          plot <- ggplot(data1, aes(x = !!as.symbol(input$environmentbarplotxaxis), 
                                    y = !!as.symbol(input$environmentbarplotyaxis)))
        }
        if(input$environmentbarplotpos == "dodge"){
          plot <- plot + geom_bar(stat = "identity", position = position_dodge())
        }else {
          plot <- plot + geom_bar(stat = "identity")
        }
        if(input$environmentbarploterror == "yes"){
          plot <- plot + geom_errorbar(aes(ymin= !!as.symbol(input$environmentbarplotyaxis) - sd,
                                           ymax = !!as.symbol(input$environmentbarplotyaxis) + sd),
                                       position = "dodge")
        }else{
          plot <- plot
        }
      }else {
        plot <- NULL
      }
    }else if(input$environmentbarplottype == "bin"){
      if(!is.null(av(input$environmentbarplotxaxis))){
        if(!is.null(av(input$environmentbarplotfill))){
          plot <- ggplot(environmentdata$use, aes(x = !!as.symbol(input$environmentbarplotxaxis),
                                                  fill = !!as.symbol(input$environmentbarplotfill)))
        }else {
          plot <- ggplot(environmentdata$use, aes(x = !!as.symbol(input$environmentbarplotxaxis)))
        }
        if(input$environmentbarplotpos == "dodge"){
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
  return(plot + theme_bw())
})
#prints correlation coefficient when viewing a scatter plot
output$environmentcorrelation <- renderPrint({
  req(environmentdata$table)
  if(input$environmentplottype == "scatter"){
    if(!is.null(av(input$environmentx1)) && !is.null(av(input$environmenty))){
      if(is.numeric(input$environmentx1) && is.numeric(input$environmenty)){
      paste("Pearson's Correlation Coefficient:", cor(environmentdata$use[[input$environmentx1]], environmentdata$use[[input$environmenty]]))
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
output$environmentcorrelationoutput <- renderUI({
  req(environmentdata$table)
  if(input$environmentplottype == "scatter"){
    verbatimTextOutput("environmentcorrelation")
  } else if(input$environmentplottype != "scatter"){
    NULL
  }
})
output$environmentplot1 <- renderPlot({
  req(environmentdata$table)
  environmentplot()
})
##ui options for environment plot
output$environmentplotmainUI <- renderUI({
  req(environmentdata$table)
  plotOutput("environmentplot1", brush = "environmentbrush", height = input$environmentplotheight)
})
#tells you which points you have selected 
output$environmentbrushtest <- renderPrint({
  req(environmentdata$table)
  req(input$environmentplottype == "scatter")
  brushedPoints(environmentdata$use, input$environmentbrush)
})
#dynamic selection UI
output$environmentdynamicselectbuttons <- renderUI({
  req(environmentdata$table)
  req(input$environmentplottype == "scatter")
  output <- tagList(
      checkboxInput("environmentsidebarhide", label = "Hide Sidebar Panel?", value = FALSE)
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
                      textInput("environmentcolumnName", "Create Name for Variable",
                                value = "New Variable"))
               ,
               conditionalPanel("input.environmentsaveselection",
                                column(4,
                                       textInput("environmentselectionName1", "Name for Group 1",
                                                 value = "Group 1")
                                )
                                ,
                                uiOutput("environmentcontainer")
                                ,
                                column(4,
                                       textInput("environmentnotext", "Name for Points Not Grouped",
                                                 value = "Not Grouped"))
               )
        ),
        column(4,
               actionButton("environmentsaveselection", "Save Selection", width = "100%")
               ,
               conditionalPanel(condition = "input.environmentsaveselection",
                                hr()
                                ,
                                actionButton("environmentseparateselection", "Save Selection with New Grouping", width = "100%")
                                
                                ,
                                hr()
                                ,
                                actionButton("environmentactionbutton", "Save Variable", width = "100%")
                                ,
                                hr()
                                ,
                                actionButton("environmentresetselection", "Reset Groupings", width = "100%")
               )
        )
      )
      ,
      hr()
      ,
      fluidRow(
        column(6,
               tags$h4("Points Currently Selected"),
               verbatimTextOutput("environmentbrushtest")
        ),
        column(6,
               tags$h4("Group Summary"),
               splitLayout(verbatimTextOutput("environmenttable1")))
      ),
      hr(),
      tags$h4("View Newly Created Variable in Your Data"),
      splitLayout(dataTableOutput("environmenttesttable"))
  )
})
observeEvent(input$environmentsaveselection, {
  updateActionButton(
    inputId = "environmentsaveselection",
    label = "Save Selected to Current Grouping")
})
observeEvent(input$environmentresetselection, {
  updateActionButton(
    inputId = "environmentsaveselection",
    label = "Save Selected")
})
observeEvent(input$environmentseparateselection, {
  updateActionButton(
    inputId = "environmentsaveselection",
    label = "Save Selected to Current Grouping")
})
observeEvent(input$environmentresetselection, {
  shinyjs::hide("environmentseparateselection")
  shinyjs::hide("environmentactionbutton")
  shinyjs::hide("environmentresetselection")
  shinyjs::hide("environmentselectionName1")
  shinyjs::hide("environmentnotext")
})
observeEvent(input$environmentsaveselection, {
  shinyjs::show("environmentseparateselection")
  shinyjs::show("environmentactionbutton")
  shinyjs::show("environmentresetselection")
  shinyjs::show("environmentselectionName1")
  shinyjs::show("environmentnotext")
})
####Dynamically select multiple points 
environmentselections <- reactiveValues()
environmentselections$samples <- data.frame()
#add selection to dataframe
observeEvent(input$environmentsaveselection, {
  IDpos <- which(grepl("ID", colnames(environmentdata$use)))[1]
  newLine <- brushedPoints(environmentdata$use, input$environmentbrush)[IDpos]
  environmentselections$samples <- rbindPad(data = environmentselections$samples, selections = newLine)
  return(environmentselections$samples)
})
#add selection as different grouping 
observeEvent(input$environmentseparateselection, {
  if(ncol(environmentselections$samples) == 1 || ncol(environmentselections$samples) < 10 && ncol(environmentselections$samples >1)){
    IDpos <- which(grepl("ID", colnames(environmentdata$use)))[1]
    newGrouping <- brushedPoints(environmentdata$use, input$environmentbrush)[IDpos]
    environmentselections$samples <- cbindPad(environmentselections$samples, newGrouping)
    environmentselections$samples[do.call(order, environmentselections$samples),]
  }else{
    NULL
  }
})
observeEvent(input$environmentresetselection, {
  environmentselections$samples <- data.frame()
})
observeEvent(input$environmentresetselection, {
  removeUI(
    selector = '#environmentselection2, #environmentselection3, #environmentselection4, #environmentselection5, 
    #environmentselection6, #environmentselection7, #environmentselection8, #environmentselection9, #environmentselection10',
    multiple = TRUE
  )
})
observeEvent(input$environmentresetselection, {
  environmentcounter(1)
})
#make dynamic number of UI elements for column naming
environmentcounter <- reactiveVal(1)
observeEvent(input$environmentseparateselection, {
  if(ncol(environmentselections$samples) == 1 || ncol(environmentselections$samples) < 11 && ncol(environmentselections$samples >1)){
    environmentcounter1 <<- environmentcounter() + 1
    environmentcounter(environmentcounter1)
    if(environmentcounter() < 11){
      insertUI(
        selector = '#environmentcontainer',
        where = "beforeEnd",
        ui = column(4,
                    tags$div(textInput(paste("environmentselectionName", paste(environmentcounter()), sep = ""), paste("Name for Group", paste(environmentcounter())),
                                       value = paste("Group", paste(environmentcounter()))),
                             id = paste0("environmentselection", paste(environmentcounter())))
        )
      )
    }else{NULL}
  } else if(ncol(environmentselections$samples) == 0){
    showNotification(ui = "You Must First Make A Preliminary Selection",
                     type = "error")
  } else if(ncol(environmentselections$samples) >=11){
    NULL
  }
})
observeEvent(input$environmentseparateselection, {
  if(environmentcounter() >= 10){
    showNotification(ui= "You Have Made the Maximum Number of Selections",
                     action = a(href = "javascript:location.reload();", "Reload page"),
                     duration = NULL, 
                     type = "error")
  }else {
    NULL
  }
})
#this produces the table to view selected points
output$environmenttable1 <- renderPrint({
  req(environmentdata$table)
  req(input$environmentplottype == "scatter")
  print(as.list(environmentselections$samples), na.print = "")
})
#dynamically name selections and update the table with the new names
environmenttest <- reactiveValues()
environmenttest$list <- c()
observe({
  if(environmentcounter() == 1){
    name1 <- input$environmentselectionName1
    environmenttest$list <- c(name1)
  }else if(environmentcounter() == 2){
    name1 <- input$environmentselectionName1
    name2 <- input$environmentselectionName2
    environmenttest$list <- c(name1, name2)
  }else if(environmentcounter() == 3){
    name1 <- input$environmentselectionName1
    name2 <- input$environmentselectionName2
    name3 <- input$environmentselectionName3
    environmenttest$list <- c(name1, name2, name3)
  }else if(environmentcounter() == 4){
    name1 <- input$environmentselectionName1
    name2 <- input$environmentselectionName2
    name3 <- input$environmentselectionName3
    name4 <- input$environmentselectionName4
    environmenttest$list <- c(name1, name2, name3, name4)
  }else if(environmentcounter() == 5){
    name1 <- input$environmentselectionName1
    name2 <- input$environmentselectionName2
    name3 <- input$environmentselectionName3
    name4 <- input$environmentselectionName4
    name5 <- input$environmentselectionName5
    environmenttest$list <- c(name1, name2, name3, name4, name5)
  }else if(environmentcounter() == 6){
    name1 <- input$environmentselectionName1
    name2 <- input$environmentselectionName2
    name3 <- input$environmentselectionName3
    name4 <- input$environmentselectionName4
    name5 <- input$environmentselectionName5
    name6 <- input$environmentselectionName6
    environmenttest$list <- c(name1, name2, name3, name4, name5, name6)
  }else if(environmentcounter() == 7){
    name1 <- input$environmentselectionName1
    name2 <- input$environmentselectionName2
    name3 <- input$environmentselectionName3
    name4 <- input$environmentselectionName4
    name5 <- input$environmentselectionName5
    name6 <- input$environmentselectionName6
    name7 <- input$environmentselectionName7
    environmenttest$list <- c(name1, name2, name3, name4, name5, name6, name7)
  }else if(environmentcounter() == 8){
    name1 <- input$environmentselectionName1
    name2 <- input$environmentselectionName2
    name3 <- input$environmentselectionName3
    name4 <- input$environmentselectionName4
    name5 <- input$environmentselectionName5
    name6 <- input$environmentselectionName6
    name7 <- input$environmentselectionName7
    name8 <- input$environmentselectionName8
    environmenttest$list <- c(name1, name2, name3, name4, name5, name6, name7, name8)
  }else if(environmentcounter() == 9){
    name1 <- input$environmentselectionName1
    name2 <- input$environmentselectionName2
    name3 <- input$environmentselectionName3
    name4 <- input$environmentselectionName4
    name5 <- input$environmentselectionName5
    name6 <- input$environmentselectionName6
    name7 <- input$environmentselectionName7
    name8 <- input$environmentselectionName8
    name9 <- input$environmentselectionName9
    environmenttest$list <- c(name1, name2, name3, name4, name5, name6, name7, name8, name9)
  }else if(environmentcounter() == 10){
    name1 <- input$environmentselectionName1
    name2 <- input$environmentselectionName2
    name3 <- input$environmentselectionName3
    name4 <- input$environmentselectionName4
    name5 <- input$environmentselectionName5
    name6 <- input$environmentselectionName6
    name7 <- input$environmentselectionName7
    name8 <- input$environmentselectionName8
    name9 <- input$environmentselectionName9
    name10 <- input$environmentselectionName10
    environmenttest$list <- c(name1, name2, name3, name4, name5, name6, name7, name8, name9, name10)
  }
  return(environmenttest$list)
})
observe({
  if(ncol(environmentselections$samples) == 1 || ncol(environmentselections$samples) < 11 && ncol(environmentselections$samples >1)){
    colnames(environmentselections$samples) <- environmenttest$list
  }else return(NULL)
})
observeEvent(input$environmentactionbutton, {
  req(environmentdata$table)
  #adds column to original table
  IDpos <- which(grepl("ID", colnames(environmentdata$table1)))[1]
  IDposname <- names(environmentdata$table1[which(grepl("ID", colnames(environmentdata$table1)))[1]])
  columnadd <- pivot_longer(environmentselections$samples, everything(), names_to = input$environmentcolumnName, values_to = IDposname) %>% unique()
  columnadd[[2]][duplicated(columnadd[[2]])] <- NA
  columnadd <- na.omit(columnadd)
  variables <- data.frame(environmentdata$table1[[IDpos]])
  names(variables)[1] <- IDposname
  columnadd <- right_join(x = columnadd, y = variables, by = IDposname)
  columnadd[is.na(columnadd)] <- input$environmentcolumnName
  environmentdata$table1 <- left_join(x = environmentdata$table1, y = columnadd, by = IDposname) %>% unique()
  
  #adds column to filtered table 
  environmentdata$table1[is.na(environmentdata$table1)] <- input$environmentnotext
  IDpos2 <- which(grepl("ID", colnames(environmentdata$filter)))[1]
  IDposname2 <- names(environmentdata$filter[which(grepl("ID", colnames(environmentdata$filter)))[1]])
  columnadd2 <- pivot_longer(environmentselections$samples, everything(), names_to = input$environmentcolumnName, values_to = IDposname) %>% unique()
  columnadd2[[2]][duplicated(columnadd2[[2]])] <- NA
  columnadd2 <- na.omit(columnadd2)
  variables2 <- data.frame(environmentdata$filter[[IDpos]])
  names(variables2)[1] <- IDposname2
  columnadd2 <- right_join(x = columnadd2, y = variables2, by = IDposname2) %>% unique()
  columnadd2[is.na(columnadd2)] <- input$environmentcolumnName
  environmentdata$filter <- left_join(x = environmentdata$filter, y = columnadd2, by = IDposname2)
})

#Make Updated table
output$environmenttesttable <- renderDataTable({
  req(environmentdata$table)
  req(input$environmentplottype == "scatter")
  environmentdata$use
})

environmentcurrentselectionx1 <- reactiveVal(NULL)
observeEvent(input$environmentx1, {
  environmentcurrentselectionx1(input$environmentx1)
})
observeEvent(input$environmentactionbutton, {
  updateSelectInput(session, "environmentx1", "Select Variable to Graph Along X-Axis:", 
                    choices = c("NULL", colnames(environmentdata$table1)),
                    selected = environmentcurrentselectionx1()
  )
})
environmentcurrentselectiony <- reactiveVal(NULL)
observeEvent(input$environmenty, {
  environmentcurrentselectiony(input$environmenty)
})
observeEvent(input$environmentactionbutton, {
  updateSelectInput(session, "environmenty", "Select Variable to Graph Along Y Axis:",
                    choices = c("NULL", colnames(environmentdata$table1)),
                    selected = environmentcurrentselectiony())
})
environmentcurrentselectioncolor <- reactiveVal(NULL)
observeEvent(input$environmentcoloroption, {
  environmentcurrentselectioncolor(input$environmentcoloroption)
})
observeEvent(input$environmentactionbutton, {
  updateSelectInput(session, "environmentcoloroption", "Select Factor to Color",
                    choices = c("NULL", names(dplyr::select_if(environmentdata$table1, is.character)),
                                names(dplyr::select_if(environmentdata$table1, is.factor))),
                    selected = environmentcurrentselectioncolor())
})

