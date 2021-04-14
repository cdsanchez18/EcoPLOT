#create tree plot-----
output$treeoptions <- renderUI({
  if(is.null(phy_tree(phyloseqobj()))){
    output <- tags$h3("Phylogenetic Tree Required")
  }else{
    withProgress(message = "Creating Plot Options",
                 detail = "This may take a while...", {
                   output <- tagList(
                     selectInput("treestyle1", "Select Tree Style:",
                                 choices = c("Rectangular" = "rectangular",
                                             "Slanted" = "slanted",
                                             "Fan" = "fan",
                                             "Circular" = "circular",
                                             "Radial" = "radial",
                                             "Equal Angle" = "equal_angle",
                                             "Daylight" = "daylight"), 
                                 selected = "rectangular")
                     ,
                     selectInput("treenode1", "Select Node to View:",
                                 choices = as_tibble(phy_tree(amplicondata$use)) %>% 
                                   select(label) %>% 
                                   arrange(label) %>% 
                                   pull(label))
                     ,
                     numericInput("treetaxonomy1", "Select Ancestral Levels:",
                                  min = 1,
                                  value = 10)
                     ,
                     numericInput("phylotreeplotheight1", "Select Height of Plot:",
                                  value = 1250)
                     ,
                     numericInput("phylotreelabelsize1", "Select Label Size:",
                                  min = 2,
                                  value = 3)
                     ,
                     actionButton("phylotreeplotrender1", "Render Tree Plot:")
                     ,
                     hr()
                     ,
                     conditionalPanel(condition = "input.phylotreeplotrender1",
                     EcoPLOT::downloadPlotUI(id = "phylotreeplotdownload")
                     )
                   )
                 })
  }
  return(output)
})
treeuse <- reactive({
  if(is.null(phyloseqobj()))return(NULL)
  switch(input$bpdataset, 
         "original" = treedf(),
         "filtered" = updatedtreedf())
  
})
phylotreeplotrender <- eventReactive(input$phylotreeplotrender1, {
  if(is.null(phy_tree(phyloseqobj())))return(NULL)
  withProgress(message = "Making Tree",
               detail = "This may take a while...", {
                 # phylotreesubset <- tree_subset(phy_tree(ampliconuse()),
                 #                                node = input$treenode1,
                 #                                levels_back = input$treetaxonomy1)
                 # if (isS4(phylotreesubset)) {
                 #   labels <- phylotreesubset@phylo$tip.label
                 # } else {
                 #   labels <- phylotreesubset$tip.label
                 # }
                 
                 # labels_df <- tibble(
                 #   label = labels,
                 #   genus = str_extract(label, "[^;]+;[^;]+$") %>% str_replace(";[^;]+$", ""),
                 #   species = str_extract(label, "[^;]+$")
                 # )  %>%
                 #   mutate(
                 #     species = if_else(is.na(genus), "", str_replace(species, "s__", "")),
                 #     genus = if_else(is.na(genus), label, str_replace(genus, "g__", ""))
                 #   )
                 
                 # ggtree(phylotreesubset, layout = input$treestyle1) + #%<+% labels_df +
                 #   geom_tiplab(size = input$phylotreelabelsize1) + geom_nodelab() 
                 plot_tree(amplicondata$use, shape="Family", label.tips="Genus", size="Abundance") + 
                   ggtitle("tree annotation using phyloseq") + theme(legend.position = "none")
                 
                 
               })
})
output$phylotreeplot <- renderPlot({
  req(input$phylotreeplotrender1)
  if(is.null(phylotree()))return(NULL)
  isolate(phylotreeplotrender())
})
output$phylotreeplotui <- renderUI({
  req(input$phylotreeplotrender1)
  if(is.null(phylotree()))return(NULL)
  isolate(plotOutput("phylotreeplot", height = input$phylotreeplotheight1))
})
EcoPLOT::downloadPlot(id = "phylotreeplotdownload", plotid = phylotreeplotrender())
