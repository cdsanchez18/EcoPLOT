####Differential Abundance -----
output$diffabundance <- renderUI({
  if(is.null(phyloseqobj()))return(NULL)
  output <- tagList(
    varSelectInput("diffabundoptions", "Select Design Condition to Compare:",
                   data = as.data.frame(sample_data(amplicondata$original)),
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
                 deseqtest <- phyloseq_to_deseq2(amplicondata$use, as.formula(paste("~", paste(input$diffabundoptions))))
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
                deseqresult1 <- cbind(as(deseqresult1, "data.frame"), as(tax_table(amplicondata$use)[rownames(deseqresult1), ], "matrix"))
                deseqresult1$asvID <- rownames(deseqresult1)
                deseqresult1 <- deseqresult1 %>% mutate('-log10(padj)' = -log10(padj))
                return(deseqresult1)
               })
})
output$deseqresultprint <- renderPrint({
  validate(
    need(input$makefile, "Please Upload a Dataset")
  )
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
                choices = c("NULL", rank_names(amplicondata$original), "threshold"),
                selected = "NULL")
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
    #,
    #actionButton("volcanoplotrender1", "Render Volcano Plot:", width = "100%")
    ,
    hr()
    ,
    downloadPlotUI("volcanoplotdownload")
  )
})

volcanoplot <- reactive({
  #eventReactive(input$volcanoplotrender1, {
  if(!is.null(av(input$volcanotaxrank1))){
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
    }else{
      plot <- ggplot(obj, aes(x = log2FoldChange, y = -log10(padj), color = !!as.symbol(input$volcanotaxrank1))) + 
        geom_point() + 
        xlab("log2 fold change") + 
        ylab("-log10 adjusted p-value")
    }
    if(input$volcanoplotlabels == "Yes"){
        plot <- plot + geom_text(aes(label=ifelse(log2FoldChange > input$volcanoplotlabelsthresh, asvID,'')), color = "black",hjust=0,vjust=0) +
          geom_text(aes(label=ifelse(log2FoldChange < -input$volcanoplotlabelsthresh, asvID,'')), color = "black",hjust=0,vjust=0) + 
          theme(#legend.position = input$volcanoplotlegendpos, 
                plot.title = element_text(size = rel(1.5), hjust = 0.5), 
                axis.title = element_text(size = rel(1.25)))
      } else {
        plot <- plot +
          theme(#legend.position = input$volcanoplotlegendpos,
                plot.title = element_text(size = rel(1.5), hjust = 0.5), 
                axis.title = element_text(size = rel(1.25)))
      }
  })
    }else {
      return(NULL)
      }
    
    return(plot + theme_bw() + theme(legend.position = input$volcanoplotlegendpos))
})


downloadPlot(id = "volcanoplotdownload", plotid = volcanoplot())

###volcano plot interactive portion ------
#tells you which points you have selected 
output$volcanobrushtest <- renderPrint({
  if(is.null(volcanoplot()))return(NULL)
  brushedPoints(deseqresults(), input$volcanobrush)
})

###log2foldchange plot -----
output$log2foldchangeui <- renderUI({
  if(is.null(deseqresults()))return(NULL)
  output<- tagList(
    numericInput("log2foldchangethreshold", "Select Log 2 Fold Change Threshold: (As Absolute Value)", value = 0, min = 0, max = 10, step = 0.25)
    ,
    selectInput("log2foldchangexaxis", "Select Tax Rank for X Axis:",
                choices = c("NULL", rank_names(amplicondata$original)),
                selected = "NULL")
    ,
    selectInput("log2foldchangecolor", "Select Tax Rank to Color:",
                choices = c(rank_names(amplicondata$original), "none"),
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
    numericInput("log2foldplotangle", "Select Angle for X Axis",
                 min = 0, max = 90, value = 45, step = 5)
    ,
    hr()
    ,
    downloadPlotUI(id = "log2foldchangegraphdownload")
  )
  return(output)
})
log2foldchangeplot <- reactive({
  #eventReactive(input$log2foldchangeplotrender, {
  if(!is.null(av(input$log2foldchangexaxis))){
  withProgress(message = "Making Plot", {
    if(!is.null(deseqresults())){
      if(input$log2foldchangecolor == "none"){
        obj <- deseqresults()
        plot <- ggplot(obj %>% filter(!between(log2FoldChange, -input$log2foldchangethreshold, input$log2foldchangethreshold)), 
          aes(x = reorder(!!as.symbol(input$log2foldchangexaxis), log2FoldChange), y = log2FoldChange)) + 
          geom_point() + labs(x = input$log2foldchangexaxis)
      }else{
        data <- deseqresults()
        plot <- ggplot(data %>% filter(!between(log2FoldChange, -input$log2foldchangethreshold, input$log2foldchangethreshold)),
                       aes(x = reorder(!!(as.symbol(input$log2foldchangexaxis)), -log2FoldChange), 
                           y = log2FoldChange, color = !!as.symbol(input$log2foldchangecolor))) + 
          geom_point() + labs(x = input$log2foldchangexaxis) #+ 
          #theme(axis.text.x = element_text(size = 8,angle = 45, hjust = 0, vjust=0.5))
      }
      return(plot + theme_bw() + theme(legend.position = input$log2foldplotlegendpos, axis.text.x = element_text(angle = input$log2foldplotangle)))
    }else {
      return(NULL)
    }
  })
  }else {
    return(NULL)
  }
})
output$log2foldchangegraph <- renderPlot({
  if(is.null(log2foldchangeplot()))return(NULL)
  log2foldchangeplot()
})
output$log2foldchangegraphoutput <- renderUI({
  validate(
    need(input$makefile, "Please Upload a Dataset")
  )
  if(is.null(log2foldchangeplot()))return(NULL)
  plotOutput("log2foldchangegraph", height = input$log2foldplotheight)
})
downloadPlot(id = "log2foldchangegraphdownload", plotid = log2foldchangeplot())

output$scatter1 <- renderPlot({
  if(is.null(volcanoplot()))return(NULL)
  volcanoplot()
})
output$scatter12 <- renderUI({
  validate(
    need(input$makefile, "Please Upload a Dataset")
  )
  if(is.null(volcanoplot()))return(NULL)
  plotOutput("scatter1", brush = "volcanobrush", height = input$volcanoplotheight)
})