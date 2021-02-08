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
  samplevars <-  list()
  samplevars <- c(samplevars, as.list(sample_variables(phyloseqobj(), errorIfNULL=FALSE)))
  samplevars <- c(samplevars, list(Sample="Sample", NULL = "NULL"))
  return(
    selectInput("heatmapxaxis", "Select How to Label Xaxis:", samplevars, "NULL", multiple = FALSE)
  )
})
output$heatmapoptions3 <- renderUI({
  if(is.null(phyloseqobj()))return(NULL)
  ranks <- list()
  ranks <- c(ranks, as.list(rank_names(phyloseqobj(), errorIfNULL=FALSE)))
  ranks <- c(ranks, list(OTU="OTU", NULL = "NULL"))
  return(
    selectInput("heatmapyaxis", "Select How to Label Yaxis:", 
                ranks, selected = "NULL", multiple = FALSE)
  )
})
output$heatmapaction <- renderUI({
  if(is.null(phyloseqobj()))return(NULL)
  output <- tagList(
  actionButton("heatmaprender", "Render Heatmap", width = "100%")
  ,
  downloadPlotUI("heatmapplotoutputdownload")
  )
  return(output)
})

heatmapplot <- eventReactive(input$heatmaprender, {
  withProgress(message = "Applying Filters:", {
    if(!is.null(av(input$heatmapxaxis)) && !is.null(av(input$heatmapyaxis))){
      plot <- plot_heatmap(ampliconuse(), method = input$heatmapmethod,
                           distance = input$heatmapdistance,
                           sample.label = input$heatmapxaxis,
                           taxa.label = input$heatmapyaxis)
    }else if(!is.null(av(input$heatmapxaxis)) && is.null(av(input$heatmapyaxis))){
      plot <- plot_heatmap(ampliconuse(), method = input$heatmapmethod,
                           sample.label = input$heatmapxaxis)
    }else if(is.null(av(input$heatmapxaxis)) && !is.null(av(input$heatmapyaxis))){
      plot <- plot_heatmap(ampliconuse(),
                           distance = input$heatmapdistance,
                           taxa.label = input$heatmapyaxis)
    } else if(is.null(av(input$heatmapxaxis)) && is.null(av(input$heatmapyaxis))){
      plot <- plot_heatmap(ampliconuse())
    }
  })
  return(plot)
})
output$heatmapplotoutput <- renderPlot({
  withProgress(message = "Making Heatmap:", {
    heatmapplot()
  })
})
downloadPlot(id = "heatmapplotoutputdownload",plotid = heatmapplot())