#Beta diversity
output$phyloseqdistanceoptions <- renderUI({
  if(is.null(phyloseqobj()))return(NULL)
  output <- tagList(
    selectInput("phyloseqdistanceoptions1", "Select Distance Method:",
                choices = list(
                  Standard = c("Bray" = "bray",
                               "Jaccard" = "jaccard",
                               "Euclidean" = "euclidean",
                               "JSD" = "jsd"),
                  Require_Phylogenetic_Tree = c(
                    "DPCoA" = "dpcoa",
                    "Unweighted Unifrac" = "uunifrac",
                    "Weighted Unifrac" = "wunifrac"
                  )),
                selected = "bray")
    ,
    conditionalPanel(condition = "input.phyloseqdistanceoptions1 == 'uunifrac' || 
                    input.phyloseqdistanceoptions1 == 'wunifrac'",
                     tags$div(tags$h5(tags$b("NOTE:"),"For this distance option, we recommend a filtered
                            dataset to avoid lengthy running times."),
                              align = "left"))
  )
  return(output)
})
output$makedistancematrixtable <- renderUI({
  if(is.null(phyloseqobj()))return(NULL)
  actionButton("renderdistancematrix", "Create Distance Matrix")
})
distancematrix <- eventReactive(input$renderdistancematrix, {
  withProgress(message = "Creating Distance Matrix", 
               detail= "This may take a while", {
                 phyloseq::distance(physeq = ampliconuse(), method = input$phyloseqdistanceoptions1)
               })
})
output$distancematrixtable <- renderDataTable({
  if(is.null(distancematrix()))return(NULL)
  as.matrix(distancematrix())
})
downloadTable(id = "distancematrixtabledownload", tableid = as.matrix(distancematrix()))

output$dispersionUI <- renderUI({
  if(is.null(distancematrix()))return(NULL)
  output<- tagList(
    tags$div(tags$h4("You are Viewing the", paste(input$ordinationdataset), "Dataset"),
             align = "center")
    ,
    selectInput("betadispersionoptions1", "Select Factors to View Dispersion:",
                choices = sample_variables(ampliconuse()),
                multiple = FALSE)
    ,
    actionButton("renderbetadispersion", "Visualize Dispersion:")
    ,
    hr()
    ,
    downloadPlotUI(id = "betadispersionplot2download")
  )
  return(output)
})
# output$betadispersionoptions <- renderUI({
#   if(is.null(distancematrix()))return(NULL)
#   selectInput("betadispersionoptions1", "Select Factors to View Dispersion:",
#               choices = sample_variables(ordinationdatasetuse()),
#               multiple = FALSE)
# })
# output$betadispersionplotrender <- renderUI({
#   if(is.null(distancematrix()))return(NULL)
#   actionButton("renderbetadispersion", "Visualize Dispersion:")
# })
betadispersionplot <- eventReactive(input$renderbetadispersion,{
  withProgress(message = "Constructing Beta Dispersion Plot", {
    mod <- betadisper(distancematrix(), sample_data(ampliconuse())[[input$betadispersionoptions1]])
    centroids<- data.frame(grps=rownames(mod$centroids),data.frame(mod$centroids))
    vectors<- data.frame(group=mod$group,data.frame(mod$vectors))
    seg.data<- cbind(vectors[,1:3],centroids[rep(1:nrow(centroids),as.data.frame(table(vectors$group))$Freq),2:3])
    names(seg.data)<-c("grps","v.PCoA1","v.PCoA2","PCoA1","PCoA2")
    ggplot() + 
      geom_point(data=centroids, aes(x=PCoA1,y=PCoA2),size=4,colour="red",shape=16) + 
      geom_point(data=seg.data, aes(x=v.PCoA1,y=v.PCoA2),size=2,shape=16) +
      labs(title="Dispersion Plot",x="",y="") + facet_grid(~grps) + stat_ellipse(type = "t")
  })
})
output$betadispersionplot1 <- renderPlot({
  if(is.null(betadispersionplot()))return(NULL)
  betadispersionplot()
})
output$betadispersionplot2 <- renderUI({
  plotOutput("betadispersionplot1")
})
downloadPlot(id = "betadispersionplot2download", plotid = betadispersionplot())
betadispstat <- eventReactive(input$renderbetadispersion, {
  if(is.null(distancematrix()))return(NULL)
  withProgress(message = "Performing Beta Dispersion", {
    anova(betadisper(distancematrix(), sample_data(ampliconuse())[[input$betadispersionoptions1]]))
  })
})

output$betadisptable <- renderPrint({
  if(is.null(betadispstat()))return(NULL)
  betadispstat()
})

output$adonisUI <- renderUI({
  if(is.null(ampliconuse()))return(NULL)
  output <- tagList(
    tags$div(tags$h4("You are Viewing the", paste(input$amplicondatasource), "Dataset"),
             align = "center")
    ,
    textInput("adonisoptions1", "Write Formula",
              placeholder = "A + B*C", width = "100%")
    ,
    selectInput("adonisstrata", "Groups Within Which to Constrain Permutations",
                choices = c("NULL", sample_variables(ampliconuse())),
                selected = "NULL")
    ,
    numericInput("adonispermutations", "Select Number of Permutations",
                 value = 999, min =1, width = "100%")
    ,
    actionButton("adonisrender1", "Perform Adonis:")
  )
  return(output)
})

# output$adonisoptions <- renderUI({
# # varSelectInput("adonisoptions1", "Select Variable to Compare:",
# #                  data = mappingfile(),
# #                  multiple = FALSE)
#   selectInput("adonisoptions1", "Select Variable to Compare",
#               choices = sample_variables(ordinationdatasetuse()),
#               multiple = FALSE)
#   })
# output$adonisrender <- renderUI({
# actionButton("adonisrender1", "Perform Adonis:")
# })
output$adonissamplevars1 <- renderPrint({
  if(is.null(ampliconuse()))return(NULL)
  
  sample_variables(ampliconuse())
})
output$adonissamplevars <- renderUI({
  if(is.null(ampliconuse()))return(NULL)
  output <- tagList(
    tags$div(
      tags$h3("Select Which Sample Variables to Include in Formula"), align = "center"
    )
    ,
    conditionalPanel("input.adonisrender1",
    verbatimTextOutput("adonissamplevars1"))
  )
  return(output)
})
ordinationadonis <- eventReactive(input$adonisrender1, {
  if(!is.null(av(input$adonisstrata))){
    withProgress(message = "Performing Adonis", {
      adonis(as.formula(paste("distancematrix() ~", paste(input$adonisoptions1))), 
             strata = sample_data(ampliconuse())[[input$adonisstrata]], permutations = input$adonispermutations,
             data = as(sample_data(ampliconuse()), "data.frame"))
    })
  }else{
    withProgress(message = "Performing Adonis", {
      adonis(as.formula(paste("distancematrix() ~", paste(input$adonisoptions1))), 
             permutations = input$adonispermutations,
             data = as(sample_data(ampliconuse()), "data.frame"))
    })
  }
})
output$adonisphyloseq <- renderPrint({
  if(is.null(phyloseqobj()))return(NULL)
  ordinationadonis()
})  

pcoaobj <- eventReactive(input$makeordinationplot1, {
  #if(is.null(distancematrix()))return(NULL)
  if(input$phyloseqordinateoptions1 == "CCA" | input$phyloseqordinateoptions1 == "RDA" |input$phyloseqordinateoptions1 == "CAP"){
    isolate(
      ordinate(
        physeq = ampliconuse(), 
        method = input$phyloseqordinateoptions1, 
        distance = distancematrix(),
        formula = as.formula(paste("~", paste(input$formulaoptions1, collapse = "+")))
      ))
  }else if(input$phyloseqordinateoptions1 == "PCoA" | input$phyloseqordinateoptions1 == "NMDS"){
    isolate(ordinate(
      physeq = ampliconuse(), 
      method = input$phyloseqordinateoptions1, 
      distance = distancematrix()
    ))
  }
})
output$ordinationplotoptions <- renderUI({
  if(is.null(phyloseqobj()))return(NULL)
  output <- tagList(
    tags$div(tags$h4("You are Viewing the", paste(input$amplicondatasource), "Dataset"),
             align = "center")
    ,
    selectInput("phyloseqordinateoptions1", "Select Ordination Method:",
                choices = list(
                  Unconstrained = c("PCoA" = "PCoA",
                                    "NMDS" = "NMDS"),
                  Constrained = c("CCA" = "CCA",
                                  "RDA" = "RDA",
                                  "CAP" = "CAP")),
                selected = "PCoA")
    ,
    conditionalPanel(condition = 
                       "input.phyloseqordinateoptions1 == 'CCA'|| input.phyloseqordinateoptions1 == 'RDA' || input.phyloseqordinateoptions1 == 'CAP'",
                     selectInput("formulaoptions1", "Select Factors to Include:",
                                 choices = sample_variables(ampliconuse()),
                                 multiple = TRUE)
    )
    ,
    textInput(inputId = "ordinationplottitle1", 
              label = "Create Title for Plot",
              placeholder = "Title")
    ,
    selectInput("ordinationcoloroptions1", "Select Variable to Color:",
                choices = c("NULL", sample_variables(ampliconuse())),#names(mappingfile())),
                selected = "NULL",
                multiple = FALSE)
    ,
    selectInput("ordinationshapeoptions1", "Select Variable to Shape:",
                choices = c("NULL", sample_variables(ampliconuse())),#names(mappingfile())),
                selected = "NULL",
                multiple = FALSE)
    ,
    radioButtons("ordinationellipse1", "Add Ellipse?",
                 choices = c("Yes" = "yes",
                             "No" = "no"),
                 selected = "no", inline = TRUE)
    ,
    actionButton("makeordinationplot1", "Render Plot", width = "100%")
    ,
    hr()
    ,
    downloadPlotUI("ordinationplotoutputdownload")
  )
  return(output)
})

ordinationplot <- eventReactive(input$makeordinationplot1, {
  req(phyloseqobj())
  if(input$ordinationellipse1 == "yes"){
    plot_ordination(
      physeq = ampliconuse(),
      ordination = pcoaobj(),
      color = input$ordinationcoloroptions1,
      shape = input$ordinationshapeoptions1,
      title = input$ordinationplottitle1) + stat_ellipse(type = "t")
  }else
    plot_ordination(
      physeq = ampliconuse(),
      ordination = pcoaobj(),
      color = input$ordinationcoloroptions1,
      shape = input$ordinationshapeoptions1,
      title = input$ordinationplottitle1)
})

ordinationplotoutput1 <- eventReactive(input$makeordinationplot1, {
  withProgress(message = "Making Ordination Plot",
               detail = "This may take a while...", {
                 if(input$phyloseqordinateoptions1 == "CCA"){
                   arrowmat <- vegan::scores(pcoaobj(), display = "bp")
                   arrowdf <- data.frame(labels = rownames(arrowmat), arrowmat)
                   arrow_map <- aes(xend = CCA1, 
                                    yend = CCA2, 
                                    x = 0, 
                                    y = 0, 
                                    shape = NULL, 
                                    color = NULL, 
                                    label = labels)
                   
                   label_map <- aes(x = 1.3 * CCA1, 
                                    y = 1.3 * CCA2, 
                                    shape = NULL, 
                                    color = NULL, 
                                    label = labels)
                   
                   arrowhead = arrow(length = unit(0.02, "npc"))
                   plot <- ordinationplot() + 
                     geom_segment(
                       mapping = arrow_map, 
                       size = 1, 
                       data = arrowdf, 
                       color = "black", 
                       arrow = arrowhead
                     ) + 
                     geom_text(
                       mapping = label_map, 
                       size = 4,  
                       data = arrowdf, 
                       show.legend = FALSE
                     )
                 }else if(input$phyloseqordinateoptions1 == "RDA"){
                   arrowmat <- vegan::scores(pcoaobj(), display = "bp")
                   arrowdf <- data.frame(labels = rownames(arrowmat), arrowmat)
                   arrow_map <- aes(xend = RDA1, 
                                    yend = RDA2, 
                                    x = 0, 
                                    y = 0, 
                                    shape = NULL, 
                                    color = NULL, 
                                    label = arrowdf$labels)
                   
                   label_map <- aes(x = 1.3 * RDA1, 
                                    y = 1.3 * RDA2, 
                                    shape = NULL, 
                                    color = NULL, 
                                    label = arrowdf$labels)
                   
                   arrowhead = arrow(length = unit(0.02, "npc"))
                   
                   plot <- isolate(ordinationplot() + 
                                     geom_segment(
                                       mapping = arrow_map, 
                                       size = 1, 
                                       data = arrowdf, 
                                       color = "black", 
                                       arrow = arrowhead
                                     ) + 
                                     geom_text(
                                       mapping = label_map, 
                                       size = 4,  
                                       data = arrowdf, 
                                       show.legend = FALSE
                                     ))
                 }else if(input$phyloseqordinateoptions1 == "CAP"){
                   arrowmat <- vegan::scores(pcoaobj(), display = "bp")
                   arrowdf <- data.frame(labels = rownames(arrowmat), arrowmat)
                   arrow_map <- aes(xend = CAP1, 
                                    yend = CAP2, 
                                    x = 0, 
                                    y = 0, 
                                    shape = NULL, 
                                    color = NULL, 
                                    label = arrowdf$labels)
                   
                   label_map <- aes(x = 1.3 * CAP1, 
                                    y = 1.3 * CAP2, 
                                    shape = NULL, 
                                    color = NULL, 
                                    label = arrowdf$labels)
                   
                   arrowhead = arrow(length = unit(0.02, "npc"))
                   plot <- isolate(ordinationplot() + 
                                     geom_segment(
                                       mapping = arrow_map, 
                                       size = 1, 
                                       data = arrowdf, 
                                       color = "black", 
                                       arrow = arrowhead
                                     ) + 
                                     geom_text(
                                       mapping = label_map, 
                                       size = 4,  
                                       data = arrowdf, 
                                       show.legend = FALSE
                                     ))
                 }else
                   plot <- ordinationplot()
               })
  return(plot)
})
output$ordinationplotoutput <- renderPlot({
  ordinationplotoutput1()
})
output$ordinationplotoutputUI <- renderUI({
  if(is.null(phyloseqobj()))return(NULL)
  if(input$renderdistancematrix == 0){
    output <- tags$h3("Please Create a Distance Matrix First")
  }else {
    req(distancematrix())
    output <- plotOutput("ordinationplotoutput")
  }
  return(output)
})

downloadPlot(id = "ordinationplotoutputdownload", plotid = )
