#alpha div using phyloseq options-----
output$alphadivoptions <- renderUI({
  #if(is.null(ampliconuse()))return(NULL)
  req(amplicondata$use)
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
                choices = c("NULL", as.list(sample_variables(amplicondata$use))),
                multiple = FALSE,
                selected = "NULL")
    ,
    selectInput("phylocolor", "Select What Factor to Color:",
                choices = c( "NULL", as.list(sample_variables(amplicondata$use))),
                multiple = FALSE,
                selected = "NULL")
    ,
    tags$div(tags$h5(tags$b("Note:"),"If multiple methods selected, use 'variable' option to maintain separation."), align = "center")
    ,
    selectInput("phylofacet", "Select Facet Option:",
                choices = c("NULL", "variable",as.list(sample_variables(amplicondata$use))),
                multiple = FALSE,
                selected = "NULL")
    ,
    conditionalPanel(condition = "input.phylofacet != 'NULL'",
                     selectInput("phylofacet2", "Select Second Facet Option:",
                                 choices = c("NULL", "variable", as.list(sample_variables(amplicondata$use))),
                                 multiple = FALSE,
                                 selected = "NULL"))
    ,
    numericInput("alphaplotheight", "Select Plot Height:", value = 800, min = 200, max = 1500, step = 25)
    ,
    fluidRow(
      column(6,
    textInput(inputId = "alphaphyloseqxaxis1", label = "Create X axis Label",
              placeholder = "X Axis"))
    ,
    column(6,
    numericInput("alphaphyloseqxaxislabelsize", "Select Size of X Axis Label Text",
                 value = 10, min = 3, max = 30)))
    ,
    numericInput("alphaphyloseqxaxistextsize", label = "Select Size of X Axis Text",
                 value = 10, min = 3, max = 30)
    ,
    sliderInput(inputId = "alphaphyloseqxaxisangle", label = "Select Angle of X Axis Text",
                min = 0, max = 90, value = 0, step = 5)
    ,
    fluidRow(
      column(6,
    textInput(inputId = "alphaphyloseqyaxis1", label = "Create Y axis Label",
              placeholder = "Y Axis"))
    ,
    column(6,
    numericInput("alphaphyloseqyaxislabelsize", "Select Size of Y Axis Label Text",
                 value = 10, min = 3, max = 30)))
    ,
    numericInput("alphaphyloseqayaxistextsize", label = "Select Size of Y Axis Text",
                 value = 10, min = 3, max = 30)
    ,
    sliderInput(inputId = "alphaphyloseqyaxisangle", label = "Select Angle of Y Axis Text",
                min = 0, max = 90, value = 0, step = 5)
  )
  return(output)
})
##Alpha Diversity stats table and mann whitney
phyloseqalpharichness <- eventReactive(input$renderalphastattable, {
  if(is.null(phyloseqobj()))return(NULL)
  withProgress(message = "Performing Diversity Metrics",
               detail = "This may take a while...", {
                 phyloseq::estimate_richness(amplicondata$use)
               })
})
output$phyloseqalphatable <- renderDataTable({
  phyloseqalpharichness()
})
output$phyloseqalphatableui <- renderUI({
  validate(
    need(input$makefile, message =  "Please Upload a Dataset"),
    need(input$renderalphastattable, message =  "Table Will Appear Here")
  )
  output <- tagList(
    splitLayout(dataTableOutput("phyloseqalphatable"))
    ,
    downloadTableUI("alphadiversitystattable"))
  return(output)
})
downloadTable(id = "alphadiversitystattable",tableid = phyloseqalpharichness())

output$alphadivstatoptions <- renderUI({
  if(is.null(phyloseqobj()))return(NULL)
  output <- tagList(
    actionButton("renderalphastattable", "Make Table", width = "100%")
    ,
    tags$div(tags$h5(tags$b("Note:"), "This will produce a table of standard alpha diversity 
            estimates."), align = "center")
    ,
    #conditionalPanel("input.renderalphastattable",
    
    #,
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
                     selectInput("alphaexclude", "Select Variables to Include in Statistics:",
                                 choices = c(sample_variables(amplicondata$use)),
                                 multiple = TRUE)
                     ,
                     actionButton("performalphastats", "Perform Statistics", width = "100%"))
  )
  return(output)
})
alphadivstatresult <- eventReactive(input$performalphastats, {
  if(is.null(phyloseqobj()))return(NULL)
  alphastats <- list()
  for(i in input$alphaexclude){#sample_variables(ampliconuse())[!grepl(pattern = "ID",x = sample_variables(ampliconuse()))]){  
    alphastats[[i]] <- pairwise.wilcox.test(phyloseqalpharichness()[[input$alphastatoptions]], sample_data(amplicondata$use)[[i]], p.adjust.method = "bonf")
  }
  alphastats
})
output$alphadivstatprint <- renderPrint({
  if(is.null(phyloseqobj()))return(NULL)
  alphadivstatresult()
})
###Alpha Diversity Plot 
phyloseqplot <- reactive({
  #eventReactive(input$phyloseqplotrender1, {
  req(amplicondata$use)
  #if(is.null(ampliconuse()))return(NULL)
  #withProgress(message = "Making Plot",
  #             detail = "This may take a while...", {
  if(!is.null(input$phyloseqalphaoptions1) && !is.null(av(input$phyloxaxis))){
                # if(is.null(av(input$phylocolor))){
                   plot <- phyloseq::plot_richness(amplicondata$use, x = input$phyloxaxis,
                                                   measures = input$phyloseqalphaoptions1,
                                                   scales = "free_y")
                   #plot$layers <- plot$layers[-1]
                 #}else {
                 #  plot <- phyloseq::plot_richness(amplicondata$use, color = input$phylocolor, x = input$phyloxaxis,
                 #                                  measures = input$phyloseqalphaoptions1,
                 #                                  scales = "free_y")
                   #plot$layers <- plot$layers[-1]
                 #}
                 #if(input$phyloseqplottype1 == "box"){
                 plot <- plot + geom_boxplot(aes(#group = !!as.symbol(input$phyloxaxis), 
                   fill = if(!is.null(av(input$phylocolor))){!!as.symbol(input$phylocolor)}else{NULL})) +
                   labs(x = paste(input$alphaphyloseqxaxis1), y = paste(input$alphaphyloseqyaxis1),
                        title = "Alpha Diversity", fill = if(!is.null(av(input$phylocolor))){input$phylocolor}else{NULL}) +
                   theme(legend.position= "right", axis.text.x = element_text(color = "black", size = isolate(input$alphaphyloseqxaxistextsize), 
                                                                              angle = isolate(input$alphaphyloseqxaxisangle)),
                         axis.text.y = element_text(color = "black", size = input$alphaphyloseqyaxistextsize,
                                                    angle = input$alphaphyloseqyaxisangle),
                         axis.title.x = element_text(size = input$alphaphyloseqxaxislabelsize), axis.title.y = element_text(size = input$alphaphyloseqyaxislabelsize)) + theme_bw()
                 if(!is.null(av(input$phylofacet)) && !is.null(av(input$phylofacet2))){
                   plot <- plot + facet_wrap(paste(input$phylofacet, paste("~", paste(input$phylofacet2))), scales = "free_y")#paste("~", paste(input$phylofacet, "+", paste(input$phylofacet2)))) 
                 }else if(!is.null(av(input$phylofacet))){
                   plot <- plot + facet_grid(paste("~", paste(input$phylofacet))) 
                 }
  }else{
    plot <- NULL
  }
              # })
  return(plot)
})
output$phyloseqplot1 <- renderPlot({
  if(is.null(phyloseqplot()))return(NULL)
  phyloseqplot()
})
output$phyloseqplot2 <- renderUI({
  validate(
    need(input$makefile, "Please Upload a Dataset")
  )
  if(is.null(phyloseqplot()))return(NULL)
  output <- tagList(
  plotOutput("phyloseqplot1", height = input$alphaplotheight)
  ,
  downloadPlotUI(id = "alphadiversityplotdownload")
  )
})
downloadPlot(id = "alphadiversityplotdownload", plotid = phyloseqplot())
