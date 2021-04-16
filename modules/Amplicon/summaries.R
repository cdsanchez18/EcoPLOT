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
#downloadPlot(id = "originalsamplecounthistplot", originalsamplehistplot())
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

output$originalphyloseqsummary <- renderPrint({
  if(is.null(phyloseqobj()))return(NULL)
  phyloseqobj()
})
output$updatedphyloseqsummary <- renderPrint({
  if(is.null(updatedphyloseq()))return(NULL)
  updatedphyloseq()
})
