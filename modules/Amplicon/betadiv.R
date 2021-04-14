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
    ,
    actionButton("renderdistancematrix", "Create Distance Matrix", width = "100%")
    ,
    conditionalPanel("input.renderdistancematrix",
                     hr(),
                     downloadTableUI(id = "distancematrixtabledownload"))
  )
  return(output)
})

distancematrix <- eventReactive(input$renderdistancematrix, {
  withProgress(message = "Creating Distance Matrix", 
               detail= "This may take a while", {
                 phyloseq::distance(physeq = amplicondata$use, method = input$phyloseqdistanceoptions1)
               })
})
output$distancematrixtable <- renderDataTable({
  if(is.null(distancematrix()))return(NULL)
  as.matrix(distancematrix())
})
downloadTable(id = "distancematrixtabledownload", tableid = as.matrix(distancematrix()))

# output$dispersionUI <- renderUI({
#   if(is.null(distancematrix()))return(NULL)
#   output<- tagList(
#     tags$div(tags$h4("You are Viewing the", paste(input$ordinationdataset), "Dataset"),
#              align = "center")
#     ,
#     selectInput("betadispersionoptions1", "Select Factors to View Dispersion:",
#                 choices = sample_variables(ampliconuse()),
#                 multiple = FALSE)
#     ,
#     actionButton("renderbetadispersion", "Visualize Dispersion:")
#     ,
#     hr()
#     ,
#     downloadPlotUI(id = "betadispersionplot2download")
#   )
#   return(output)
# })

# betadispersionplot <- reactive({
#   if(is.null(distancematrix()))return(NULL)
#   #eventReactive(input$renderbetadispersion,{
#   #withProgress(message = "Constructing Beta Dispersion Plot", {
#     mod <- betadisper(distancematrix(), sample_data(ampliconuse())[[input$betadispersionoptions1]])
#     centroids<- data.frame(grps=rownames(mod$centroids),data.frame(mod$centroids))
#     vectors<- data.frame(group=mod$group,data.frame(mod$vectors))
#     seg.data<- cbind(vectors[,1:3],centroids[rep(1:nrow(centroids),as.data.frame(table(vectors$group))$Freq),2:3])
#     names(seg.data)<-c("grps","v.PCoA1","v.PCoA2","PCoA1","PCoA2")
#     ggplot() + 
#       geom_point(data=centroids, aes(x=PCoA1,y=PCoA2),size=4,colour="red",shape=16) + 
#       geom_point(data=seg.data, aes(x=v.PCoA1,y=v.PCoA2),size=2,shape=16) +
#       labs(title="Dispersion Plot",x="",y="") + facet_grid(~grps) + stat_ellipse(type = "t")
#   #})
# })
# output$betadispersionplot1 <- renderPlot({
#   if(is.null(betadispersionplot()))return(NULL)
#   betadispersionplot()
# })
# output$betadispersionplot2 <- renderUI({
#   plotOutput("betadispersionplot1")
# })
#downloadPlot(id = "betadispersionplot2download", plotid = betadispersionplot())
# betadispstat <- eventReactive(input$renderbetadispersion, {
#   if(is.null(distancematrix()))return(NULL)
#   withProgress(message = "Performing Beta Dispersion", {
#     anova(betadisper(distancematrix(), sample_data(ampliconuse())[[input$betadispersionoptions1]]))
#   })
# })
# 
# output$betadisptable <- renderPrint({
#   if(is.null(betadispstat()))return(NULL)
#   betadispstat()
# })

output$adonisUI <- renderUI({
  #if(is.null(ampliconuse()))return(NULL)
  req(amplicondata$use)
  output <- tagList(
    tags$div(tags$h4("You are Viewing the", paste(input$amplicondatasource), "Dataset"),
             align = "center")
    ,
    textInput("adonisoptions1", "Write Formula",
              placeholder = "A + B*C", width = "100%")
    ,
    tags$h5(tags$b("NOTE:"), "For assistance in writing a model formula, consult the Amplicon guide at the beginning of the module.")
    ,
    selectInput("adonisstrata", "Groups Within Which to Constrain Permutations",
                choices = c("NULL", sample_variables(amplicondata$use)),
                selected = "NULL")
    ,
    numericInput("adonispermutations", "Select Number of Permutations",
                 value = 999, min =1, width = "100%")
    ,
    actionButton("adonisrender1", "Perform Adonis:", width = "100%")
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
  #if(is.null(ampliconuse()))return(NULL)
  req(amplicondata$use)
  sample_variables(amplicondata$use)
})
output$adonissamplevars <- renderUI({
  #if(is.null(ampliconuse()))return(NULL)
  output <- tagList(
    tags$div(
      tags$h3("Select Which Sample Variables to Include in Adonis"), align = "center"
    )
    ,
    verbatimTextOutput("adonissamplevars1")
  )
  return(output)
})
ordinationadonis <- eventReactive(input$adonisrender1, {
  req(distancematrix())
  if(!is.null(av(input$adonisstrata))){
    withProgress(message = "Performing Adonis", {
      adonis(as.formula(paste("distancematrix() ~", paste(input$adonisoptions1))), method = input$phyloseqdistanceoptions1,
             strata = sample_data(amplicondata$use)[[input$adonisstrata]], permutations = input$adonispermutations,
             data = as(sample_data(amplicondata$use), "data.frame"))
    })
  }else{
    withProgress(message = "Performing Adonis", {
      adonis(as.formula(paste("distancematrix() ~", paste(input$adonisoptions1))), method = input$phyloseqdistanceoptions1,
             permutations = input$adonispermutations,
             data = as(sample_data(amplicondata$use), "data.frame"))
    })
  }
})
output$adonisphyloseq <- renderPrint({
  if(is.null(phyloseqobj()))return(NULL)
  ordinationadonis()
})  

output$ordinationplotoptions <- renderUI({
  req(amplicondata$use)
  isolate({
  output <- tagList(
    tags$div(tags$h4("You are Viewing the", paste(input$amplicondatasource), "Dataset"),
             align = "center")
    ,
    selectInput("phyloseqordinateoptions1", "Select Ordination Method:",
                choices = list(
                  "NULL",
                  Unconstrained = c("PCoA" = "PCoA",
                                    "NMDS" = "NMDS"),
                  Constrained = c("CCA" = "CCA",
                                  "RDA" = "RDA",
                                  "CAP" = "CAP")),
                selected = "NULL")
    ,
    conditionalPanel(condition = 
                       "input.phyloseqordinateoptions1 == 'CCA'|| input.phyloseqordinateoptions1 == 'RDA' || input.phyloseqordinateoptions1 == 'CAP'",
                     selectInput("formulaoptions1", "Select Factors to Include:",
                                 choices = c("NULL", sample_variables(amplicondata$original)),
                                 selected = "NULL",
                                 multiple = TRUE)
    )
    ,
    textInput(inputId = "ordinationplottitle1", 
              label = "Create Title for Plot",
              placeholder = "Title")
    ,
    selectInput("ordinationcoloroptions1", "Select Variable to Color:",
                choices = c("NULL", sample_variables(amplicondata$original)),
                selected = "NULL",
                multiple = FALSE)
    ,
    selectInput("ordinationshapeoptions1", "Select Variable to Shape:",
                choices = c("NULL", sample_variables(amplicondata$original)),
                selected = "NULL",
                multiple = FALSE)
    ,
    radioButtons("ordinationellipse1", "Add Ellipse?",
                 choices = c("Yes" = "yes",
                             "No" = "no"),
                 selected = "no", inline = TRUE)
    ,
    numericInput("betaheight", "Select Plot Height:", value = 800, min = 200, max = 1600, step = 25)
    ,
    hr()
    ,
    downloadPlotUI("ordinationplotoutputdownload")
  )
  })
  return(output)
})
output$threeDordinationplotoptions <- renderUI({
  req(amplicondata$use)
  isolate({
    output <- tagList(
      tags$div(tags$h4("You are Viewing the", paste(input$amplicondatasource), "Dataset"),
               align = "center")
      ,
      selectInput("threedphyloseqordinateoptions1", "Select Ordination Method:",
                  choices = list(
                    "NULL",
                    Unconstrained = c("PCoA" = "PCoA")),
                  selected = "NULL")
      ,
      textInput(inputId = "threedordinationplottitle1", 
                label = "Create Title for Plot",
                placeholder = "Title")
      ,
      selectInput("threedordinationcoloroptions1", "Select Variable to Color:",
                  choices = c("NULL", sample_variables(amplicondata$original)),
                  selected = "NULL",
                  multiple = FALSE)
      ,
      numericInput("threedbetaheight", "Select Plot Height:", value = 800, min = 200, max = 1600, step = 25)
    )
  })
  return(output)
})
threedplot <- reactive({
  if(!is.null(av(input$threedphyloseqordinateoptions1))){
    ordinationobject <- ordinate(
      physeq = isolate(amplicondata$use), 
      method = input$threedphyloseqordinateoptions1, 
      distance = distancematrix()
    )
    ordinationdata <- data.frame(ordinationobject$vectors) 
    ordinationdata$Sample_ID <- rownames(ordinationdata)
    ordinationdata <- left_join(ordinationdata, data.frame(sample_data(amplicondata$use)))
    plot <- plot_ly(ordinationdata, x = ~Axis.1, y = ~Axis.2, z = ~Axis.3,
                    type="scatter3d", mode = "markers", 
                    color = if(!is.null(av(input$threedordinationcoloroptions1))){~get(input$threedordinationcoloroptions1)}else{NULL}
                    ) #ifelse(!is.null(av(input$threedordinationcoloroptions1)), ~get(input$threedordinationcoloroptions1),#paste("~", paste(input$threedordinationcoloroptions1)), 
                                               #      "NULL"))
    #if(!is.null(av(input$threedordinationcoloroptions1)))
  }else{
    plot <- NULL
  }
  return(plot)
})
output$testordinationoutput <- renderPlotly({
  if(is.null(threedplot()))return(NULL)
  threedplot() %>% layout(title = input$threedordinationplottitle1,
                          annotations = list(yref = 'paper', xref = "paper", y =1.05, x = 1.1, text = input$threedordinationcoloroptions1,
                                             showarrow = F),
                          scene = list(xaxis = list(title = 'Axis 1'),
                                       yaxis = list(title = 'Axis 2'),
                                       zaxis = list(title = 'Axis 3')))
})
output$threedordinationplotoutput <- renderUI({
  if(is.null(threedplot()))return(NULL)
  plotlyOutput("testordinationoutput", height = input$threedbetaheight)
})
ordinationobject <- reactiveValues()
ordinationobject$pco <- data.frame()

#pcoaobj <- reactive({
observe({
  #eventReactive(input$makeordinationplot1, {
  #if(is.null(distancematrix()))return(NULL)
  #req(amplicondata$use)
  if(!is.null(av(input$phyloseqordinateoptions1))){
  if(input$phyloseqordinateoptions1 == "CCA" | input$phyloseqordinateoptions1 == "RDA" |input$phyloseqordinateoptions1 == "CAP"){
    if(!is.null(input$formulaoptions1)){
      ordinationobject$pco <- ordinate(
        physeq = isolate(amplicondata$use), 
        method = input$phyloseqordinateoptions1, 
        distance = distancematrix(),
        formula = as.formula(paste("~", paste(input$formulaoptions1, collapse = "+")))
      )
    }else{
      NULL
    }
  }else if(input$phyloseqordinateoptions1 == "PCoA" | input$phyloseqordinateoptions1 == "NMDS"){
    #isolate(
    ordinationobject$pco <- ordinate(
      physeq = isolate(amplicondata$use), 
      method = input$phyloseqordinateoptions1, 
      distance = distancematrix()
    )
    #)
  }
  }else{
    NULL
  }
})

#ordinationplot <- reactive({
observe({
  req(amplicondata$use)
  req(ordinationobject$pco)
  if(is.null(av(input$phyloseqordinateoptions1)))return(NULL)
  #req(input$phyloseqordinateoptions1 != "NULL")
  #if(is.null(pcoaobj()))return(NULL)
  #eventReactive(input$makeordinationplot1, {
  #req(phyloseqobj())
  if(input$ordinationellipse1 == "yes"){
    ordinationobject$plot <- plot_ordination(
      physeq = isolate(amplicondata$use),
      ordination = ordinationobject$pco,
        #pcoaobj(),
      color = input$ordinationcoloroptions1,
      shape = input$ordinationshapeoptions1,
      title = input$ordinationplottitle1) + stat_ellipse(type = "t")
  }else
    ordinationobject$plot <-plot_ordination(
      physeq = isolate(amplicondata$use),
      ordination = ordinationobject$pco,
        #pcoaobj(),
      color = input$ordinationcoloroptions1,
      shape = input$ordinationshapeoptions1,
      title = input$ordinationplottitle1)
})

#ordinationplotoutput1 <- reactive({
observe({
  req(amplicondata$use)
  req(ordinationobject$plot)
  #if(is.null(ordinationplot()))return(NULL)
  #eventReactive(input$makeordinationplot1, {
  withProgress(message = "Making Ordination Plot",
               detail = "This may take a while...", {
                 if(input$phyloseqordinateoptions1 == "CCA"){
                   arrowmat <- vegan::scores(ordinationobject$pco,#pcoaobj(), 
                                             display = "bp")
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
                   ordinationobject$updateplot <- ordinationobject$plot + # ordinationplot() + 
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
                     ) + theme_bw()
                 }else if(input$phyloseqordinateoptions1 == "RDA"){
                   arrowmat <- vegan::scores(ordinationobject$pco,#pcoaobj(), 
                                             display = "bp")
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
                   
                   ordinationobject$updateplot <- isolate(ordinationobject$plot +#ordinationplot() + 
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
                                     )) + theme_bw()
                 }else if(input$phyloseqordinateoptions1 == "CAP"){
                   arrowmat <- vegan::scores(ordinationobject$pco,#pcoaobj(), 
                                             display = "bp")
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
                   ordinationobject$updateplot <- isolate(ordinationobject$plot +#ordinationplot() + 
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
                                     )) + theme_bw()
                 }else
                   ordinationobject$updateplot <- ordinationobject$plot +#ordinationplot() + 
                     theme_bw()
               })
  return(ordinationobject$updateplot)
})

downloadPlot(id = "ordinationplotoutputdownload", plotid = ordinationobject$updateplot)

output$ordinationplotoutput <- renderPlot({
  #req(amplicondata$use)
  #ordinationplotoutput1()
  ordinationobject$updateplot
})
output$ordinationplotoutputUI <- renderUI({
  if(is.null(phyloseqobj()))return(NULL)
  if(input$renderdistancematrix == 0){
    output <- tags$h3("Please Create a Distance Matrix First")
  }else {
    output <- tagList(
      plotOutput("ordinationplotoutput", height = input$betaheight, brush = "ordinationbrush")
      #,
      #uiOutput("ordinationdynamicselectbuttons")
      #verbatimTextOutput("ordinationbrushtest")
    )
  }
  return(output)
})
output$ordinationbrushtest <- renderPrint(
  brushedPoints(ordinationobject$updateplot[["data"]], input$ordinationbrush)
)

output$ordinationdynamicselectbuttons <- renderUI({
  req(input$phyloseqordinateoptions1 != "NULL")
  #if(is.null(ordinationplot()))return(NULL)
  output <- tagList(
    checkboxInput("ordinationsidebarhide", label = "Hide Sidebar Panel?", value = FALSE)
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
                     Clicking 'Save Selection' will group those points together under a name of your choosing within your created variable. This process can be repeated
                     to distinguish different groupings under the same new variable. All created variables can be used in all graphical and 
                     statistical analyses within EcoPLOT."), align = "center")
    ,
    tags$div(style = "padding:10px")
    ,
    fluidRow(
      column(8,
             column(4,
                    textInput("ordinationcolumnName", "Create Name for Variable",
                              value = "New_Variable"))
             ,
             conditionalPanel("input.ordinationsaveselection",
                              column(4,
                                     textInput("ordinationselectionName1", "Name for Group 1",
                                               value = "Group_1")
                              )
                              ,
                              uiOutput("ordinationcontainer")
                              ,
                              column(4,
                                     textInput("ordinationnotext", "Name for Points Not Grouped",
                                               value = "Not_Grouped"))
             )
      ),
      column(4,
             actionButton("ordinationsaveselection", "Save Selection", width = "100%")
             ,
             conditionalPanel(condition = "input.ordinationsaveselection",
                              hr()
                              ,
                              actionButton("ordinationseparateselection", "Save Selection to New Group", width = "100%")
                              
                              ,
                              hr()
                              ,
                              actionButton("ordinationactionbutton", "Save Variable", width = "100%")
                              ,
                              hr()
                              ,
                              actionButton("ordinationresetselection", "Reset Groupings", width = "100%")
             )
      )
    )
    ,
    hr()
    ,
    fluidRow(
      column(6,
             tags$h4("Points Currently Selected"),
             verbatimTextOutput("ordinationbrushtest")
      ),
      column(6,
             tags$h4("Group Summary"),
             splitLayout(verbatimTextOutput("ordinationtable1")))
    ),
    hr(),
    tags$h4("View Newly Created Variable in Your Data"),
    splitLayout(dataTableOutput("ordinationtesttable"),
                verbatimTextOutput("ordinationtestprint"))
  )
})
observeEvent(input$ordinationsaveselection, {
  updateActionButton(
    session = getDefaultReactiveDomain(),
    inputId = "ordinationsaveselection",
    label = "Save Selected to Current Group")
})
observeEvent(input$ordinationresetselection, {
  updateActionButton(
    session = getDefaultReactiveDomain(),
    inputId = "ordinationsaveselection",
    label = "Save Selected")
})
observeEvent(input$ordinationseparateselection, {
  updateActionButton(
    session = getDefaultReactiveDomain(),
    inputId = "ordinationsaveselection",
    label = "Save Selected to Current Group")
})
observeEvent(input$ordinationresetselection, {
  shinyjs::hide("ordinationseparateselection")
  shinyjs::hide("ordinationactionbutton")
  shinyjs::hide("ordinationresetselection")
  shinyjs::hide("ordinationselectionName1")
  shinyjs::hide("ordinationnotext")
})

observeEvent(input$ordinationsaveselection, {
  shinyjs::show("ordinationseparateselection")
  shinyjs::show("ordinationactionbutton")
  shinyjs::show("ordinationresetselection")
  shinyjs::show("ordinationselectionName1")
  shinyjs::show("ordinationnotext")
})
####Dynamically select multiple points 
ordinationselections <- reactiveValues()
ordinationselections$samples <- data.frame()
#add selection to dataframe
observeEvent(input$ordinationsaveselection, {
  #IDpos <- which(grepl("ID", colnames(phenotypedata$use)))[1]
  #newLine <- brushedPoints(phenotypedata$use, input$phenotypebrush)[IDpos]
  newLine <- brushedPoints(ordinationobject$plot[["data"]], input$ordinationbrush)["Row_ID"]
  ordinationselections$samples <- rbindPad(data = ordinationselections$samples, selections = newLine)
  ordinationselections$samples[do.call(order, ordinationselections$samples),]
  return(ordinationselections$samples)
})
#add selection as different grouping 
observeEvent(input$ordinationseparateselection, {
  if(ncol(ordinationselections$samples) == 1 || ncol(ordinationselections$samples) < 10 && ncol(ordinationselections$samples >1)){
    #IDpos <- which(grepl("ID", colnames(phenotypedata$use)))[1]
    #newGrouping <- brushedPoints(phenotypedata$use, input$phenotypebrush)[IDpos]
    newLine <- brushedPoints(ordinationobject$plot[["data"]], input$ordinationbrush)["Row_ID"]
    ordinationselections$samples <- cbindPad(ordinationselections$samples, newLine)#newGrouping)
    ordinationselections$samples[do.call(order, ordinationselections$samples),]
  }else{
    NULL
  }
})
observeEvent(input$ordinationresetselection, {
  ordinationselections$samples <- data.frame()
})
observeEvent(input$ordinationresetselection, {
  removeUI(
    selector = '#ordinationselection2, #ordinationselection3, #ordinationselection4, #ordinationselection5, 
    #ordinationselection6, #ordinationselection7, #ordinationselection8, #ordinationselection9, #ordinationselection10',
    multiple = TRUE
  )
})
observeEvent(input$ordinationresetselection, {
  ordinationcounter(1)
})
#make dynamic number of UI elements for column naming
ordinationcounter <- reactiveVal(1)
observeEvent(input$ordinationseparateselection, {
  if(ncol(ordinationselections$samples) == 1 || ncol(ordinationselections$samples) < 11 && ncol(ordinationselections$samples >1)){
    ordinationcounter1 <<- ordinationcounter() + 1
    ordinationcounter(ordinationcounter1)
    if(ordinationcounter() < 11){
      insertUI(
        selector = '#ordinationcontainer',
        where = "beforeEnd",
        ui = column(4,
                    tags$div(textInput(paste("ordinationselectionName", paste(ordinationcounter()), sep = ""), paste("Name for Group", paste(ordinationcounter())),
                                       value = paste("Group", paste(ordinationcounter()))),
                             id = paste0("ordinationselection", paste(ordinationcounter())))
        )
      )
    }else{NULL}
  } else if(ncol(ordinationselections$samples) == 0){
    showNotification(ui = "You Must First Make A Preliminary Selection",
                     type = "error")
  } else if(ncol(ordinationselections$samples) >=11){
    NULL
  }
})
observeEvent(input$ordinationseparateselection, {
  if(ordinationcounter() >= 10){
    showNotification(ui= "You Have Made the Maximum Number of Selections",
                     action = a(href = "javascript:location.reload();", "Reload page"),
                     duration = NULL, 
                     type = "error")
  }else {
    NULL
  }
})
#this produces the table to view selected points
output$ordinationtable1 <- renderPrint({
  print(as.list(ordinationselections$samples), na.print = "")
})

#dynamically name selections and update the table with the new names
ordinationtest <- reactiveValues()
ordinationtest$list <- c()
observe({
  if(ordinationcounter() == 1){
    name1 <- input$ordinationselectionName1
    ordinationtest$list <- c(name1)
  }else if(ordinationcounter() == 2){
    name1 <- input$ordinationselectionName1
    name2 <- input$ordinationselectionName2
    ordinationtest$list <- c(name1, name2)
  }else if(ordinationcounter() == 3){
    name1 <- input$ordinationselectionName1
    name2 <- input$ordinationselectionName2
    name3 <- input$ordinationselectionName3
    ordinationtest$list <- c(name1, name2, name3)
  }else if(ordinationcounter() == 4){
    name1 <- input$ordinationselectionName1
    name2 <- input$ordinationselectionName2
    name3 <- input$ordinationselectionName3
    name4 <- input$ordinationselectionName4
    ordinationtest$list <- c(name1, name2, name3, name4)
  }else if(ordinationcounter() == 5){
    name1 <- input$ordinationselectionName1
    name2 <- input$ordinationselectionName2
    name3 <- input$ordinationselectionName3
    name4 <- input$ordinationselectionName4
    name5 <- input$ordinationselectionName5
    ordinationtest$list <- c(name1, name2, name3, name4, name5)
  }else if(ordinationcounter() == 6){
    name1 <- input$ordinationselectionName1
    name2 <- input$ordinationselectionName2
    name3 <- input$ordinationselectionName3
    name4 <- input$ordinationselectionName4
    name5 <- input$ordinationselectionName5
    name6 <- input$ordinationselectionName6
    ordinationtest$list <- c(name1, name2, name3, name4, name5, name6)
  }else if(ordinationcounter() == 7){
    name1 <- input$ordinationselectionName1
    name2 <- input$ordinationselectionName2
    name3 <- input$ordinationselectionName3
    name4 <- input$ordinationselectionName4
    name5 <- input$ordinationselectionName5
    name6 <- input$ordinationselectionName6
    name7 <- input$ordinationselectionName7
    ordinationtest$list <- c(name1, name2, name3, name4, name5, name6, name7)
  }else if(ordinationcounter() == 8){
    name1 <- input$ordinationselectionName1
    name2 <- input$ordinationselectionName2
    name3 <- input$ordinationselectionName3
    name4 <- input$ordinationselectionName4
    name5 <- input$ordinationselectionName5
    name6 <- input$ordinationselectionName6
    name7 <- input$ordinationselectionName7
    name8 <- input$ordinationselectionName8
    ordinationtest$list <- c(name1, name2, name3, name4, name5, name6, name7, name8)
  }else if(ordinationcounter() == 9){
    name1 <- input$ordinationselectionName1
    name2 <- input$ordinationselectionName2
    name3 <- input$ordinationselectionName3
    name4 <- input$ordinationselectionName4
    name5 <- input$ordinationselectionName5
    name6 <- input$ordinationselectionName6
    name7 <- input$ordinationselectionName7
    name8 <- input$ordinationselectionName8
    name9 <- input$ordinationselectionName9
    ordinationtest$list <- c(name1, name2, name3, name4, name5, name6, name7, name8, name9)
  }else if(ordinationcounter() == 10){
    name1 <- input$ordinationselectionName1
    name2 <- input$ordinationselectionName2
    name3 <- input$ordinationselectionName3
    name4 <- input$ordinationselectionName4
    name5 <- input$ordinationselectionName5
    name6 <- input$ordinationselectionName6
    name7 <- input$ordinationselectionName7
    name8 <- input$ordinationselectionName8
    name9 <- input$ordinationselectionName9
    name10 <- input$ordinationselectionName10
    ordinationtest$list <- c(name1, name2, name3, name4, name5, name6, name7, name8, name9, name10)
  }
  return(ordinationtest$list)
})
observe({
  if(ncol(ordinationselections$samples) == 1 || ncol(ordinationselections$samples) < 11 && ncol(ordinationselections$samples >1)){
    colnames(ordinationselections$samples) <- ordinationtest$list
  }else return(NULL)
})


observeEvent(input$ordinationactionbutton, {
  #take selected points and turn them into data frame
  columnadd <- pivot_longer(ordinationselections$samples, everything(), names_to = input$ordinationcolumnName, values_to = "Row_ID") %>% unique()
  columnadd[[2]][duplicated(columnadd[[2]])] <- NA
  columnadd <- na.omit(columnadd)
  
  #add column to mapping file of amplicon dataset
  updatedmapping <- sample_data(amplicondata$original) %>% data.frame()
  updatedmapping <- left_join(x = updatedmapping, y = columnadd, by = "Row_ID")
  updatedmapping[is.na(updatedmapping)] <- input$ordinationnotext
  rownames(updatedmapping) <- updatedmapping[["Sample"]]
  
  phyloseq::sample_data(amplicondata$original) <- updatedmapping

  
  #update PCO data file so that new variable can be accessed in color and shape options
  ordinationobject$updateplot[["data"]] <- left_join(ordinationobject$updateplot[["data"]], columnadd, by = "Row_ID")
  ordinationobject$updateplot[["data"]][is.na(ordinationobject$updateplot[["data"]])] <- input$ordinationnotext
  
  #if a filtered dataset is present, allows for created variable to be added to filtered plot as well
  if(!is.null(updatedphyloseq())){
    updatedmapping <- sample_data(amplicondata$filtered) %>% data.frame()
    updatedmapping <- left_join(x = updatedmapping, y = columnadd, by = "Row_ID")
    updatedmapping[is.na(updatedmapping)] <- input$ordinationnotext
    rownames(updatedmapping) <- updatedmapping[["Sample"]]
    phyloseq::sample_data(amplicondata$filtered) <- updatedmapping
  }else{
    NULL
  }
})
output$ordinationtesttable <- renderDataTable({
  #req(phenotypedata$table)
  #req(input$phenotypeplottype == "scatter")
  #data.frame(phyloseq::sample_data(phyloseqobj()))
  #ordinationselections$testplot
  #data.frame(phyloseq::sample_data(amplicondata$use))
  ordinationobject$updateplot[["data"]]
})
output$ordinationtestprint <- renderPrint({
  amplicondata$use
})


################
# ordinationcurrentselectiontype <- reactiveVal(NULL)
# observeEvent(input$phyloseqordinateoptions1, {
#   ordinationcurrentselectiontype(input$phyloseqordinateoptions1)
# })
# observeEvent(input$ordinationactionbutton, {
#   updateSelectInput(session, "phyloseqordinateoptions1", "Select Ordination Method:",
#               choices = list(
#                 "NULL",
#                 Unconstrained = c("PCoA" = "PCoA",
#                                   "NMDS" = "NMDS"),
#                 Constrained = c("CCA" = "CCA",
#                                 "RDA" = "RDA",
#                                 "CAP" = "CAP")),
#               selected = ordinationcurrentselectiontype()
#               )
# })

ordinationcurrentselectionformula <- reactiveVal(NULL)
observeEvent(input$ordinationactionbutton, {
  ordinationcurrentselectionformula(input$formulaoptions1)
})
observeEvent(input$ordinationactionbutton, {
  updateSelectInput(session, "formulaoptions1", "Select Factors to Include:",
              choices = c("NULL", sample_variables(amplicondata$original)),
              selected = ordinationcurrentselectionformula()
              )
})


ordinationcurrentselectioncolor <- reactiveVal(NULL)
observeEvent(input$ordinationactionbutton, {
  ordinationcurrentselectioncolor(input$ordinationcoloroptions1)
})
observeEvent(input$ordinationactionbutton, {
  updateSelectInput(session, "ordinationcoloroptions1", "Select Variable to Color:",
              choices = c("NULL", sample_variables(amplicondata$original)),
              selected = ordinationcurrentselectioncolor()
              )
})

ordinationcurrentselectionshape <- reactiveVal(NULL)
observeEvent(input$ordinationactionbutton, {
  ordinationcurrentselectionshape(input$ordinationshapeoptions1)
})
observeEvent(input$ordinationactionbutton, {
  updateSelectInput(session, "ordinationshapeoptions1", "Select Variable to Shape:",
              choices = c("NULL", sample_variables(amplicondata$original)),
              selected = ordinationcurrentselectionshape()
              )
})






