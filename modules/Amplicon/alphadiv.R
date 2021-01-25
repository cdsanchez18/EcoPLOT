#alpha div using phyloseq options-----
output$alphadivoptions <- renderUI({
  if(is.null(ampliconuse()))return(NULL)
  output <- tagList(
    tags$div(tags$h4("You are Viewing the", paste(input$amplicondatasource), "Dataset"),
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
    selectInput("phyloxaxis", "Select Which Factor to Compare on the X-Axis:",
                choices = c("NULL", as.list(sample_variables(phyloseqobj()))),
                multiple = FALSE,
                selected = "NULL")
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
                 phyloseq::estimate_richness(ampliconuse())
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
  for(i in sample_variables(ampliconuse())[!grepl(pattern = "ID",x = sample_variables(ampliconuse()))]){  
    alphastats[[i]] <- pairwise.wilcox.test(phyloseqalpharichness()[[input$alphastatoptions]], sample_data(ampliconuse())[[i]], p.adjust.method = "bonf")
  }
  alphastats
})
output$alphadivstatprint <- renderPrint({
  if(is.null(phyloseqobj()))return(NULL)
  alphadivstatresult()
})
###Alpha Diversity Plot 
phyloseqplot <- eventReactive(input$phyloseqplotrender1, {
  req(input$phyloseqplotrender1)
  if(is.null(ampliconuse()))return(NULL)
  withProgress(message = "Making Plot",
               detail = "This may take a while...", {
                 if(is.null(av(input$phylocolor))){
                   plot <- phyloseq::plot_richness(ampliconuse(), x = input$phyloxaxis,
                                                   measures = input$phyloseqalphaoptions1,
                                                   scales = "free_y")
                   #plot$layers <- plot$layers[-1]
                 }else {
                   plot <- phyloseq::plot_richness(ampliconuse(), color = input$phylocolor, x = input$phyloxaxis,
                                                   measures = input$phyloseqalphaoptions1,
                                                   scales = "free_y")
                   #plot$layers <- plot$layers[-1]
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
               })
  return(plot)
})
output$phyloseqplot1 <- renderPlot({
  #if(is.null(phyloseqplot()))
  phyloseqplot()
})
downloadPlot(id = "alphadiversityplotdownload", plotid = phyloseqplot())
