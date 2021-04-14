##view stacked barplot of data---- 
#render UI options for barplot
output$bpplotui <- renderUI({
  #if(is.null(ampliconuse()))return(NULL)
  req(amplicondata$use)
  data <- c(as.list(unique(sample_variables(amplicondata$use))))
  names <- list("NULL"="NULL")
  names <- c(names, as.list(rank_names(amplicondata$use)))
  
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
    numericInput("bpplotsize", "Select Height of Plot:", value = 800)
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
    #,
    #actionButton("bptaxrender1", "Make Barplot", width = '100%')
  )
  return(output)
})

barplotfilter <- reactive({
  #eventReactive(input$bptaxrender1,{
  #if(is.null(ampliconuse()))return(NULL)
  req(amplicondata$use)
  if(!is.null(av(input$bptaxrank1))){
  withProgress(message = "Applying Filters:", {
    glom1 <- tax_glom(amplicondata$use, taxrank = input$bptaxrank1) %>% 
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
  }else{
    dataset <- NULL
  }
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
                   theme(legend.position= "right", axis.text.x = element_text(color = "black", size = input$bpaxistextsize, 
                                                                              angle = input$bpaxisangle1),
                         axis.text.y = element_text(color = "black", size = input$bpyaxistextsize,
                                                    angle = input$bpyaxisangle1),
                         axis.title.x = element_text(size = input$bpxaxislabelsize), axis.title.y = element_text(size = input$bpyaxislabelsize),
                         legend.text = element_text(face = "italic")) + theme_bw()
                 
                 if(!is.null(av(input$bpfacetoption)) && is.null(av(input$bpfacetoption2))){
                   plot <- plot + facet_grid(paste("~", paste(input$bpfacetoption)), scales = "free_x", drop = TRUE)
                 }else if(!is.null(av(input$bpfacetoption)) && !is.null(av(input$bpfacetoption2))){
                   plot <- plot + facet_grid(paste(input$bpfacetoption, "~", paste(input$bpfacetoption2)), scales = "free_x", drop = TRUE)
                 }else if(is.null(av(input$bpfacetoption)) && is.null(input$bpfacetoption2)){
                   plot 
                 }
               })
  return(plot)
})

output$barplotplot <- renderPlot({
  barplotplot1()
})
output$stackedbarplotgraph <- renderUI({
  #if(is.null(ampliconuse())) return(NULL)
  req(amplicondata$use)
  plotOutput("barplotplot", height = input$bpplotsize)
})
downloadPlot(id = "barplotdownload", plotid = barplotplot1())
