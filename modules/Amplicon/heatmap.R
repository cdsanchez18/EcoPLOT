#Heatmap -----
output$heatmapoptions <- renderUI({
  if(is.null(phyloseqobj()))return(NULL)
  selectInput("heatmapmethod", "Select Method:",
              choices = c("DCA", "CCA", "RDA", 
                          "CAP", "DPCoA", "NMDS", 
                          "MDS", "PCoA"),
              selected = "NMDS")
})
output$heatmapoptions1 <- renderUI({
  if(is.null(phyloseqobj()))return(NULL)
  selectInput("heatmapdistance", "Select Distance Method:",
              choices = list(
                Standard = c("Bray" = "bray",
                             "Jaccard" = "jaccard",
                             "Euclidean" = "euclidean",
                             "DPCoA" = "dpcoa",
                             "JSD" = "jsd"),
                Require_Phylogenetic_Tree = c(
                  "Unweighted Unifrac" = "unifrac",
                  "Weighted Unifrac" = "wunifrac"
                )),
              selected = "bray")
})
output$heatmapoptions2 <- renderUI({
  if(is.null(phyloseqobj()))return(NULL)
  #samplevars <-  list()
  #samplevars <- c(samplevars, as.list(sample_variables(amplicondata$original, errorIfNULL=FALSE)))
  #samplevars <- c(samplevars, list(Sample="Sample", NULL = "NULL"))
  return(
    textInput("heatmapxaxis", label = "X Axis Label", placeholder = "Sample")
    #selectInput("heatmapxaxis", "X Axis Label", samplevars, "NULL", multiple = FALSE)
  )
})
output$heatmapoptions3 <- renderUI({
  if(is.null(phyloseqobj()))return(NULL)
  ranks <- list()
  ranks <- c(ranks, as.list(rank_names(amplicondata$original, errorIfNULL=FALSE)))
  ranks <- c(ranks, list(OTU="OTU", NULL = "NULL"))
  return(
    selectInput("heatmapyaxis", "Y Axis Label", 
                ranks, selected = "NULL", multiple = FALSE)
  )
})
output$heatmapaction <- renderUI({
  if(is.null(phyloseqobj()))return(NULL)
  output <- tagList(
    numericInput("heatmapplotheight", "Select Plot Height:", value = 800, min = 200, max = 1500, step = 25)
    ,
    numericInput("heatmapphyloseqtextsize", "Select Size of Text", value = 12, step = 2, width = "100%")
    ,
    hr()
    ,
    fluidRow(
      column(6,
             textInput(inputId = "heatmapphyloseqxaxis1", label = "Create X axis Label",
                       placeholder = "X Axis"))
      ,
      column(6,
             numericInput("heatmapphyloseqxaxislabelsize", "Select Size of X Axis Label Text",
                          value = 10, min = 3, max = 30)))
    ,
    numericInput("heatmapphyloseqxaxistextsize", label = "Select Size of X Axis Text",
                 value = 10, min = 3, max = 30)
    ,
    sliderInput(inputId = "heatmapphyloseqxaxisangle", label = "Select Angle of X Axis Text",
                min = 0, max = 90, value = 0, step = 5)
    ,
    hr()
    ,
    fluidRow(
      column(6,
             textInput(inputId = "heatmapphyloseqyaxis1", label = "Create Y axis Label",
                       placeholder = "Y Axis"))
      ,
      column(6,
             numericInput("heatmapphyloseqyaxislabelsize", "Select Size of Y Axis Label Text",
                          value = 10, min = 3, max = 30)))
    ,
    numericInput("heatmapphyloseqayaxistextsize", label = "Select Size of Y Axis Text",
                 value = 10, min = 3, max = 30)
    ,
    sliderInput(inputId = "heatmapphyloseqyaxisangle", label = "Select Angle of Y Axis Text",
                min = 0, max = 90, value = 0, step = 5)
    ,
  actionButton("heatmaprender", "Render Heatmap", width = "100%")
  ,
  downloadPlotUI("heatmapplotoutputdownload")
  )
  return(output)
})

heatmapplot <- eventReactive(input$heatmaprender, {
  withProgress(message = "Applying Filters:", {
    if(#!is.null(av(input$heatmapxaxis)) && 
      !is.null(av(input$heatmapyaxis))){
      plot <- plot_heatmap(amplicondata$use, method = input$heatmapmethod,
                           distance = input$heatmapdistance,
                           sample.label = input$heatmapxaxis,
                           taxa.label = input$heatmapyaxis)
    }else if(#!is.null(av(input$heatmapxaxis)) && 
      is.null(av(input$heatmapyaxis))){
      plot <- plot <- plot_heatmap(amplicondata$use)
        #plot_heatmap(amplicondata$use, method = input$heatmapmethod, 
         #                  distance = input$heatmapdistance,
          #                 sample.label = input$heatmapxaxis)
    }#else if(is.null(av(input$heatmapxaxis)) && !is.null(av(input$heatmapyaxis))){
      #plot <- plot_heatmap(amplicondata$use,
      #                     distance = input$heatmapdistance,
      #                     taxa.label = input$heatmapyaxis)
    #} else if(is.null(av(input$heatmapxaxis)) && is.null(av(input$heatmapyaxis))){
      #plot <- plot_heatmap(amplicondata$use)
    #}
  })
  return(plot)
})
output$heatmapplotoutput <- renderPlot({
  withProgress(message = "Making Heatmap:", {
    heatmapplot() + theme_bw() +
      theme(legend.position= "right", 
            text = element_text(size = input$heatmapphyloseqtextsize),
            axis.text.x = element_text(color = "black", 
                                       size = input$heatmapphyloseqxaxistextsize, 
                                       angle = input$heatmapphyloseqxaxisangle),
            axis.text.y = element_text(color = "black", 
                                       size = input$heatmapphyloseqyaxistextsize, 
                                       angle = input$heatmapphyloseqyaxisangle),
            axis.title.x = element_text(size = input$heatmapphyloseqxaxislabelsize), 
            axis.title.y = element_text(size = input$heatmapphyloseqyaxislabelsize))
  })
})
output$heatmapplotoutputui <- renderUI({
  validate(
    need(input$makefile, "Please Upload a Dataset")
  )
  plotOutput("heatmapplotoutput", height = input$heatmapplotheight)
})


downloadPlot(id = "heatmapplotoutputdownload",plotid = heatmapplot())