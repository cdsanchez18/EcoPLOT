####Differential Abundance -----
output$diffabundance <- renderUI({
  if(is.null(phyloseqobj()))return(NULL)
  output <- tagList(
    varSelectInput("diffabundoptions", "Select Design Condition to Compare:",
                   data = as.data.frame(sample_data(phyloseqobj())),
                   multiple = FALSE)
    ,
    actionButton("performdeseq", "Perform Deseq:", width = "100%")
    ,
    conditionalPanel("input.performdeseq",
                     hr(),
                     tags$div(
                     tags$h4("Using the Results table below, this section will allow you to view the distribution of individual
                             ASV counts based on the previously selected DESeq design condition."), align = "center")
                     ,
                     textInput("asvid", "Select ASV to View Count Distribution:",
                               value = "ASV ID")
                     ,
                     actionButton("asvplotrender", "Render ASV Count Plot:", width = "100%"))
  )
  return(output)
})
deseqobj <- eventReactive(input$performdeseq, {
  withProgress(message = "Performing Deseq Analysis",
               detail = "This may take a while...", {
                 deseqtest <- phyloseq_to_deseq2(ampliconuse(), as.formula(paste("~", paste(input$diffabundoptions))))
                 deseqtest@assays@data@listData[["counts"]] <- deseqtest@assays@data@listData[["counts"]] + 1 %>% 
                   round() %>% as.integer() 
                 deseqtest1 <- tryCatch(DESeq(deseqtest, test= "Wald", fitType="parametric"),
                                        error = function(cond){
                                          message("Error")
                                          return(NULL)
                                        },
                                        warning = function(cond){
                                          message("Warning")
                                          return(NULL)
                                        })
                 
               })
})
observe({
  if(!is.null(deseqobj()))return(NULL)
  showNotification("Error in performing DESeq. Please try again using another variable.",
                   type = "error")
})
deseqresults <- reactive({
  if(is.null(deseqobj()))return(NULL)
  withProgress(message = "Getting Deseq Results",
               detail = "This may take a while...", {
                 deseqresult1 <- results(deseqobj(), alpha = 0.05,
                format = "DataFrame")
                deseqresult1 <- cbind(as(deseqresult1, "data.frame"), as(tax_table(ampliconuse())[rownames(deseqresult1), ], "matrix"))
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
  #as.data.frame(deseqresults()) 
  deseqresults()
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
  req(deseqresults())
  output <- tagList(
    selectInput("volcanotaxrank1", "Select Taxonomic Rank to Depict:",
                choices = c(rank_names(phyloseqobj()), "threshold"),
                selected = 2)
    ,
    radioButtons("volcanothreshlines", label = "Show Threshold Lines?",
                 choices = c("Yes", "No"),
                 selected = "Yes", inline = TRUE)
    ,
    radioButtons("volcanoplotlabels", "Label Enriched Points?",
                 choices = c("Yes", "No"),
                 selected = "No", inline = TRUE)
    ,
    conditionalPanel(condition = "input.volcanoplotlabels == 'Yes'",
                     numericInput("volcanoplotlabelsthresh", "Select Numeric Threshold for Label:",
                                  2.5, min = 1.5, max = 10))
    ,
    radioButtons("volcanoplotlegendpos", "Select Legend Position",
                 choices = c("Right" = "right",
                             "Top" = "top",
                             "Bottom"= "bottom"))
    ,
    numericInput("volcanoplotheight", "Select Height for Plot",
                 min = 0, value = 800)
    ,
    actionButton("volcanoplotrender1", "Render Volcano Plot:", width = "100%")
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
        geom_vline(xintercept = c(-1,0,1)) +
        geom_hline(yintercept = -log10(0.05))
      if(input$volcanoplotlabels == "Yes"){
        plot <- plot + geom_text(aes(label=ifelse(log2FoldChange > isolate(input$volcanoplotlabelsthresh),as.character(asvID),'')),color = "black",hjust=0,vjust=0) +
          geom_text(aes(label=ifelse(log2FoldChange < -isolate(input$volcanoplotlabelsthresh),as.character(asvID),'')),color = "black",hjust=0,vjust=0) + 
          theme(legend.position = input$volcanoplotlegendpos, 
                plot.title = element_text(size = rel(1.5), hjust = 0.5), 
                axis.title = element_text(size = rel(1.25)))
      } else {
        plot <- plot +
          theme(legend.position = input$volcanoplotlegendpos,
                plot.title = element_text(size = rel(1.5), hjust = 0.5), 
                axis.title = element_text(size = rel(1.25)))
      }
    }else if(input$volcanothreshlines=="No"){
      plot <- ggplot(obj, aes(x = log2FoldChange, y = -log10(padj), color = !!as.symbol(input$volcanotaxrank1))) + 
        geom_point() + 
        xlab("log2 fold change") + 
        ylab("-log10 adjusted p-value")
      if(input$volcanoplotlabels == "Yes"){
        plot <- plot + geom_text(aes(label=ifelse(log2FoldChange> isolate(input$volcanoplotlabelsthresh),as.character(asvID),'')),color = "black",hjust=0,vjust=0) +
          geom_text(aes(label=ifelse(log2FoldChange < -isolate(input$volcanoplotlabelsthresh),as.character(asvID),'')),color = "black",hjust=0,vjust=0) +
          theme(legend.position = input$volcanoplotlegendpos,
                plot.title = element_text(size = rel(1.5), hjust = 0.5), 
                axis.title = element_text(size = rel(1.25)))
      }else {
        plot <- plot +
          theme(legend.position = input$volcanoplotlegendpos,
                plot.title = element_text(size = rel(1.5), hjust = 0.5), 
                axis.title = element_text(size = rel(1.25)))
      }
    }else {
      return(NULL)}
    
    return(plot)
  })
})


downloadPlot(id = "volcanoplotdownload", plotid = volcanoplot())

###volcano plot interactive portion ------
#tells you which points you have selected 
output$volcanobrushtest <- renderPrint({
  if(is.null(volcanoplot()))return(NULL)
  brushedPoints(deseqresults(), input$volcanobrush)
})
#dynamic selection UI
output$volcanodynamicselectbuttons <- renderUI({
  if(is.null(volcanoplot()))return(NULL)
  output <- tagList(
    fluidRow(
      column(6,
             actionButton("volcanosaveselection", "Save Current Selection:", width = "100%")
             ,
             conditionalPanel(condition = "input.volcanosaveselection",
                              hr()
                              ,
                              actionButton("volcanoseparateselection", "Save With Different Grouping", width = "100%")
             )
             ,
             hr()
             ,
             actionButton("volcanoresetselection", "Reset Current Selection:", width = "100%")
             ,
             hr()
             ,
             actionButton("volcanoactionbutton", "Add Column:", width = "100%")
      )
      ,
      column(6,
             column(6,
                    textInput("volcanoselectionName1", "Create Label for Selection 1:",
                              value = "Selection 1")
             )
             ,
             uiOutput("volcanocontainer")
             ,
             fluidRow(
               column(6,
                      textInput("volcanonotext", "Name Rows Not in Selections",
                                value = "Not Grouped"))
               ,
               column(6,
                      textInput("volcanocolumnName", "Create Name for Column",
                                value = "New Column"))
             )
      )
    )
  )
})
observeEvent(input$volcanoresetselection, {
  shinyjs::hide("volcanoseparateselection")
})
observeEvent(input$volcanosaveselection, {
  shinyjs::show("volcanoseparateselection")
})
####Dynamically select multiple points 
volcanoselections <- reactiveValues()
volcanoselections$samples <- data.frame()
#add selection to dataframe
observeEvent(input$volcanosaveselection, {
  IDpos <- which(grepl("ID", colnames(deseqresults())))[1]
  newLine <- brushedPoints(deseqresults(), input$volcanobrush)[IDpos]
  volcanoselections$samples <- rbindPad(data = volcanoselections$samples, selections = newLine)
  return(volcanoselections$samples)
})
#add selection as different grouping 
observeEvent(input$volcanoseparateselection, {
  if(ncol(volcanoselections$samples) == 1 || ncol(volcanoselections$samples) < 10 && ncol(volcanoselections$samples >1)){
    IDpos <- which(grepl("ID", colnames(deseqresults())))[1]
    newGrouping <- brushedPoints(deseqresults(), input$volcanobrush)[IDpos]
    volcanoselections$samples <- cbindPad(volcanoselections$samples, newGrouping)
  }else{
    NULL
  }
})
observeEvent(input$volcanoresetselection, {
  volcanoselections$samples <- data.frame()
})
observeEvent(input$volcanoresetselection, {
  removeUI(
    selector = '#volcanoselection2, #volcanoselection3, #volcanoselection4, #volcanoselection5, 
    #volcanoselection6, #volcanoselection7, #volcanoselection8, #volcanoselection9, #volcanoselection10',
    multiple = TRUE
  )
})
observeEvent(input$volcanoresetselection, {
  volcanocounter(1)
})
#make dynamic number of UI elements for column naming
volcanocounter <- reactiveVal(1)
observeEvent(input$volcanoseparateselection, {
  if(ncol(volcanoselections$samples) == 1 || ncol(volcanoselections$samples) < 11 && ncol(volcanoselections$samples >1)){
    volcanocounter1 <<- volcanocounter() + 1
    volcanocounter(volcanocounter1)
    if(volcanocounter() < 11){
      insertUI(
        selector = '#volcanocontainer',
        where = "beforeEnd",
        ui = column(6,
                    tags$div(textInput(paste("volcanoselectionName", paste(volcanocounter()), sep = ""), paste("Create Label for Selection", paste(volcanocounter())),
                                       value = paste("Selection", paste(volcanocounter()))),
                             id = paste0("volcanoselection", paste(volcanocounter())))
        )
      )
    }else{NULL}
  } else if(ncol(volcanoselections$samples) == 0){
    showNotification(ui = "You Must First Make A Preliminary Selection",
                     type = "error")
  } else if(ncol(volcanoselections$samples) >=11){
    NULL
  }
})
observeEvent(input$volcanoseparateselection, {
  if(volcanocounter() >= 10){
    showNotification(ui= "You Have Made the Maximum Number of Selections",
                     action = a(href = "javascript:location.reload();", "Reload page"),
                     duration = NULL, 
                     type = "error")
  }else {
    NULL
  }
})
#this produces the table to view selected points
output$volcanotable1 <- renderDataTable({
  if(is.null(volcanoplot()))return(NULL)
  volcanoselections$samples
})
#dynamically name selections and update the table with the new names
volcanotest <- reactiveValues()
volcanotest$list <- c()
observe({
  if(volcanocounter() == 1){
    name1 <- input$volcanoselectionName1
    volcanotest$list <- c(name1)
  }else if(volcanocounter() == 2){
    name1 <- input$volcanoselectionName1
    name2 <- input$volcanoselectionName2
    volcanotest$list <- c(name1, name2)
  }else if(volcanocounter() == 3){
    name1 <- input$volcanoselectionName1
    name2 <- input$volcanoselectionName2
    name3 <- input$volcanoselectionName3
    volcanotest$list <- c(name1, name2, name3)
  }else if(volcanocounter() == 4){
    name1 <- input$volcanoselectionName1
    name2 <- input$volcanoselectionName2
    name3 <- input$volcanoselectionName3
    name4 <- input$volcanoselectionName4
    volcanotest$list <- c(name1, name2, name3, name4)
  }else if(volcanocounter() == 5){
    name1 <- input$volcanoselectionName1
    name2 <- input$volcanoselectionName2
    name3 <- input$volcanoselectionName3
    name4 <- input$volcanoselectionName4
    name5 <- input$volcanoselectionName5
    volcanotest$list <- c(name1, name2, name3, name4, name5)
  }else if(volcanocounter() == 6){
    name1 <- input$volcanoselectionName1
    name2 <- input$volcanoselectionName2
    name3 <- input$volcanoselectionName3
    name4 <- input$volcanoselectionName4
    name5 <- input$volcanoselectionName5
    name6 <- input$volcanoselectionName6
    volcanotest$list <- c(name1, name2, name3, name4, name5, name6)
  }else if(volcanocounter() == 7){
    name1 <- input$volcanoselectionName1
    name2 <- input$volcanoselectionName2
    name3 <- input$volcanoselectionName3
    name4 <- input$volcanoselectionName4
    name5 <- input$volcanoselectionName5
    name6 <- input$volcanoselectionName6
    name7 <- input$volcanoselectionName7
    volcanotest$list <- c(name1, name2, name3, name4, name5, name6, name7)
  }else if(volcanocounter() == 8){
    name1 <- input$volcanoselectionName1
    name2 <- input$volcanoselectionName2
    name3 <- input$volcanoselectionName3
    name4 <- input$volcanoselectionName4
    name5 <- input$volcanoselectionName5
    name6 <- input$volcanoselectionName6
    name7 <- input$volcanoselectionName7
    name8 <- input$volcanoselectionName8
    volcanotest$list <- c(name1, name2, name3, name4, name5, name6, name7, name8)
  }else if(volcanocounter() == 9){
    name1 <- input$volcanoselectionName1
    name2 <- input$volcanoselectionName2
    name3 <- input$volcanoselectionName3
    name4 <- input$volcanoselectionName4
    name5 <- input$volcanoselectionName5
    name6 <- input$volcanoselectionName6
    name7 <- input$volcanoselectionName7
    name8 <- input$volcanoselectionName8
    name9 <- input$volcanoselectionName9
    volcanotest$list <- c(name1, name2, name3, name4, name5, name6, name7, name8, name9)
  }else if(volcanocounter() == 10){
    name1 <- input$volcanoselectionName1
    name2 <- input$volcanoselectionName2
    name3 <- input$volcanoselectionName3
    name4 <- input$volcanoselectionName4
    name5 <- input$volcanoselectionName5
    name6 <- input$volcanoselectionName6
    name7 <- input$volcanoselectionName7
    name8 <- input$volcanoselectionName8
    name9 <- input$volcanoselectionName9
    name10 <- input$volcanoselectionName10
    volcanotest$list <- c(name1, name2, name3, name4, name5, name6, name7, name8, name9, name10)
  }
  return(volcanotest$list)
})
observe({
  if(ncol(volcanoselections$samples) == 1 || ncol(volcanoselections$samples) < 11 && ncol(volcanoselections$samples >1)){
    colnames(volcanoselections$samples) <- volcanotest$list
  }else return(NULL)
})
observeEvent(input$performdeseq,{
  if(is.null(deseqresults()))return(NULL)
  volcanotest$table <- as.data.frame(deseqresults())
})
observeEvent(input$volcanoactionbutton, {
  if(is.null(volcanoplot()))return(NULL)
  IDpos <- which(grepl("ID", colnames(deseqresults())))[1]
  IDposname <- names(deseqresults()[which(grepl("ID", colnames(deseqresults())))[1]])
  columnadd <- pivot_longer(volcanoselections$samples, everything(), names_to = input$volcanocolumnName, values_to = IDposname)
  variables <- data.frame(deseqresults()[[IDpos]])
  names(variables)[1] <- IDposname
  columnadd <- right_join(x = columnadd, y = variables, by = IDposname)
  volcanotest$table <- left_join(x = volcanotest$table, y = columnadd, by = IDposname)
  volcanotest$table[is.na(volcanotest$table)] <- input$volcanonotext
})
#Make Updated table
output$volcanotesttable <- renderDataTable({
  if(is.null(volcanoplot()))return(NULL)
  volcanotest$table
})
downloadTable(id = "volcanoselectionstable", tableid = volcanoselections$samples)
downloadTable(id = "volcanodesequpdated", volcanotest$table)


###log2foldchange plot -----
output$log2foldchangeui <- renderUI({
  if(is.null(deseqresults()))return(NULL)
  output<- tagList(
    numericInput("log2foldchangethreshold", "Select Log 2 Fold Change Threshold: (As Absolute Value)", value = 0, min = 0, max = 10, step = 0.25)
    ,
    selectInput("log2foldchangexaxis", "Select Tax Rank for X Axis:",
                choices = rank_names(phyloseqobj()))
    ,
    selectInput("log2foldchangecolor", "Select Tax Rank to Color:",
                choices = c(rank_names(phyloseqobj()), "none"),
                selected = "none")
    ,
    radioButtons("log2foldplotlegendpos", "Select Legend Position",
                 choices = c("Default" = "right",
                             "Top" = "top",
                             "Bottom"= "bottom"))
    ,
    numericInput("log2foldplotheight", "Select Height for Plot",
                 min = 0, value = 800)
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
        plot <- ggplot(obj %>% filter(!between(log2FoldChange, -input$log2foldchangethreshold, input$log2foldchangethreshold)), 
          aes(x = reorder(!!as.symbol(input$log2foldchangexaxis), log2FoldChange), y = log2FoldChange)) + 
          geom_point() + labs(x = input$log2foldchangexaxis) + 
          theme(axis.text.x = element_text(angle = -90, hjust = 0, vjust=0.5),
                legend.position = input$log2foldplotlegendpos) + 
          xlab("log2 fold change") 
      }else{
        data <- deseqresults()
        plot <- ggplot(data %>% filter(!between(log2FoldChange, -input$log2foldchangethreshold, input$log2foldchangethreshold)),
                       aes(x = reorder(!!(as.symbol(input$log2foldchangexaxis)), -log2FoldChange), 
                           y = log2FoldChange, color = !!as.symbol(input$log2foldchangecolor))) + 
          geom_point() +labs(x = input$log2foldchangexaxis) + 
          theme(axis.text.x = element_text(size = 8,angle = -90, hjust = 0, vjust=0.5),
                legend.position = input$log2foldplotlegendpos)
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
output$log2foldchangegraphoutput <- renderUI({
  if(is.null(log2foldchangeplot()))return(NULL)
  plotOutput("log2foldchangegraph", height = input$log2foldplotheight)
})
downloadPlot(id = "log2foldchangegraphdownload", plotid = log2foldchangeplot())

output$scatter1 <- renderPlot({
  if(is.null(volcanoplot()))return(NULL)
  volcanoplot()
})
output$scatter12 <- renderUI({
  if(is.null(volcanoplot()))return(NULL)
  plotOutput("scatter1", brush = "volcanobrush", height = input$volcanoplotheight)
})