output$ampliconsampledata <- renderUI({
  output <- tagList(
    tags$h4("If Using Example Data, Click 'Upload' After Checking Box", align = "center")
    ,
    checkboxInput("ampliconexampledata", "Use Example Data", value = FALSE)
    ,
    fluidRow(
      column(5, hr()),
      column(2,tags$div(tags$h4("OR"), align="center")),
      column(5, hr())
    )
  )
  return(output)
})
output$fileformatoptions <- renderUI({
  output <- tagList(
    tags$h4("Upload Data:")
    ,
    radioButtons("fileformat", "How is Your Data Formatted?",
                 choices = c("QIIME1 (.biom)"= "qiime1",
                             "QIIME2 (.qza)" = "qiime2",
                             "No Format (.csv/.txt/.tsv)" = "none"),
                 selected = "none")
  )
  return(output)
})
output$fileuploadoptions <- renderUI({
  if(is.null(input$fileformat))return(NULL)
  if(input$fileformat == "none"){
    output <- tagList(
      tags$hr(),
      radioButtons("mappingformat", "How Are Your Files Formatted?",
                   choices = c("Tab" = "\t",
                               "Comma ( , )" = ",",
                               "Semicolon ( ; )" = ";"),
                   selected = "\t"),
      fileInput("otufile1", "Upload OTU File:",
                multiple = FALSE,
                accept = c(
                  ".csv",
                  ".txt",
                  ".tsv",
                  "text/csv",
                  "text/comma-separated-values,text/plain"
                )),
      fileInput("taxfile1", "Upload Taxonomy File:",
                multiple = FALSE,
                accept = c(
                  ".csv",
                  "text/csv",
                  "text/comma-separated-values,text/plain"
                )),
      fileInput("mappingfile1", "Upload Mapping File:",
                multiple = FALSE,
                accept = c(
                  ".csv",
                  "text/csv",
                  "text/comma-separated-values,text/plain"
                )),
      hr(),
      tags$div(tags$h5(tags$b("NOTE:"), "EcoPLOT only accepts", tags$b("Newick"),
                       "and", tags$b("Nexus"),"file formats for trees"),
               align = "center"),
      fileInput("treefile1", "Upload Tree File (optional):",
                multiple = FALSE,
                accept = c(
                  ".newick",".nex", ".nxs", ".tree"
                ))
    )
  }else if(input$fileformat == "qiime2"){
    output <- tagList(
      fileInput("otufile1", "Upload OTU File:",
                multiple = FALSE,
                accept = c(
                  ".qza"
                )),
      fileInput("taxfile1", "Upload Taxonomy File:",
                multiple = FALSE,
                accept = c(
                  ".qza")),
      fileInput("mappingfile1", "Upload Mapping File:",
                multiple = FALSE,
                accept = c(
                  ".tsv", 
                  "text/csv",
                  "text/comma-separated-values,text/plain")),
      hr(),
      tags$div(tags$h5(tags$b("NOTE:"), "EcoPLOT only accepts", tags$b("Newick"),
                       "and", tags$b("Nexus"),"file formats for trees"),
               align = "center"),
      fileInput("treefile1", "Upload Tree File (optional):",
                multiple = FALSE,
                accept = c(
                  ".newick",".nex", ".nxs", ".tree"
                ))
    )
  }else if(input$fileformat == "qiime1"){
    output <- tagList(
      fileInput("otufile1", "Upload OTU File (.biom):",
                multiple = FALSE,
                accept = c(
                  ".biom"
                )),
      radioButtons("mappingformat", "How is your Mapping File Formatted?",
                   choices = c("Tab" = "\t",
                               "Comma" = ",",
                               "Semicolon" = ";"),
                   selected = "\t"),
      fileInput("mappingfile1", "Upload Mapping File:",
                multiple = FALSE,
                accept = c(
                  ".tsv",
                  ".csv",
                  "text/csv",
                  "text/comma-separated-values,text/plain"
                )),
      tags$div(tags$h5(tags$b("NOTE:"), "EcoPLOT only accepts", tags$b("Newick"),
                       "and", tags$b("Nexus"),"file formats for trees"),
               align = "center"),
      hr(),
      fileInput("treefile1", "Upload Tree File (optional):",
                multiple = FALSE,
                accept = c(
                  ".newick", ".nex", ".nxs", ".tree"
                ))
    )
  }
  return(output)
})
otufile <- eventReactive(input$makefile, {
  if(input$ampliconexampledata == TRUE){
    file <- import_biom(BIOMfilename = "data/testotu.biom", parseFunction = parse_taxonomy_greengenes)
  }else {
    inFile <- input$otufile1
    if(!is.null(inFile)){
      if(input$fileformat == "none"){
        obj <- read.csv(file = inFile$datapath, row.names = 1) %>% na.omit()
        if(is.matrix(obj)){
          file <- phyloseq::otu_table(obj, taxa_are_rows = TRUE)
        }else {
          obj <- data.matrix(obj)
          file <- phyloseq::otu_table(obj, taxa_are_rows = TRUE)
        }
      }else if(input$fileformat == "qiime2"){
        file <- inFile$datapath
      }else if(input$fileformat == "qiime1"){
        file <- import_biom(BIOMfilename = inFile$datapath, parseFunction = parse_taxonomy_greengenes)
      }
    }else {
      NULL
    }
  }
  return(file)
})
taxonomyfile <- eventReactive(input$makefile, {
  if(input$ampliconexampledata == TRUE){
    NULL
  }else {
    inFile <- input$taxfile1
    if(!is.null(inFile)){
      if(input$fileformat == "none"){
        obj <- read.csv(file = inFile$datapath, row.names = 1) %>% na.omit()
        file <- obj
        file <- as.matrix(file)
        file <- tax_table(file) 
      }else if(input$fileformat == "qiime2"){
        file <- inFile$datapath
      }else if(input$fileformat == "qiime1"){
        NULL
      }
    }else {
      NULL
    }
  }
  return(file)
})

mappingfile <- eventReactive(input$makefile, {
  if(input$ampliconexampledata == TRUE){
    obj <- read.csv("data/testmapping.txt", sep = "\t")
    rownames(obj) <- obj[[1]] %>% na.omit
    if(!is.null(phenotypedata$table) && !is.null(environmentdata$table)){
      if(length(intersect(names(obj), names(phenotypedata$table))) >= 1 && length(intersect(names(obj), names(environmentdata$table))) >= 1){
        obj <- left_join(obj, phenotypedata$table1)
        obj <- left_join(obj, environmentdata$table1) %>% na.omit()
        rownames(obj) <- obj[[1]]
      }
    }else if(!is.null(phenotypedata$table) && is.null(environmentdata$table)){
      if(length(intersect(names(obj), names(phenotypedata$table))) >= 1){
        obj <- left_join(obj, phenotypedata$table1) %>% na.omit()
        rownames(obj) <- obj[[1]]
      }
    }else if(is.null(phenotypedata$table) && !is.null(environmentdata$table)){
      if(length(intersect(names(obj), names(environmentdata$table))) >= 1){
        obj <- left_join(obj, environmentdata$table1) %>% na.omit()
        rownames(obj) <- obj[[1]]
      }
    }
    if(is.data.frame(obj)){
      obj$Sample <- rownames(obj)
      file <- phyloseq::sample_data(obj)
    } else {
      obj$Sample <- rownames(obj)
      obj <- as.data.frame(obj)
      file <- phyloseq::sample_data(obj)
    }
  }else {
    inFile <- input$mappingfile1
    if(!is.null(inFile)){
      if(input$fileformat == "none" || input$fileformat == "qiime1"){
        obj <- read.csv(file = inFile$datapath, sep = input$mappingformat) %>% na.omit()
        rownames(obj) <- obj[[1]]
        #file <- sample_data(obj)
        if(!is.null(phenotypedata$table) && !is.null(environmentdata$table)){
          if(length(intersect(names(obj), names(phenotypedata$table))) >= 1 && length(intersect(names(obj), names(environmentdata$table))) >= 1){
            obj <- left_join(obj, phenotypedata$table1)
            obj <- left_join(obj, environmentdata$table1) %>% na.omit()
            rownames(obj) <- obj[[1]]
          }
        }else if(!is.null(phenotypedata$table) && is.null(environmentdata$table)){
          if(length(intersect(names(obj), names(phenotypedata$table))) >= 1){
            obj <- left_join(obj, phenotypedata$table1) %>% na.omit()
            rownames(obj) <- obj[[1]]
          }
        }else if(is.null(phenotypedata$table) && !is.null(environmentdata$table)){
          if(length(intersect(names(obj), names(environmentdata$table))) >= 1){
            obj <- left_join(obj, environmentdata$table1) %>% na.omit()
            rownames(obj) <- obj[[1]]
          }
        }
        if(is.data.frame(obj)){
          obj$Sample <- rownames(obj)
          file <- phyloseq::sample_data(obj)
        } else {
          obj$Sample <- rownames(obj)
          obj <- as.data.frame(obj)
          file <- phyloseq::sample_data(obj)
        }
      }else if(input$fileformat == "qiime2"){
        file <- inFile$datapath
        
      }#else if(input$fileformat == "qiime1"){
      #   obj <- read.csv(file = inFile$datapath,
      #                   sep = input$mappingformat,
      #                   row.names = 1)
      #   file <- sample_data(obj)
      # }
    }else {
      NULL
    }
  }
  return(file)
})

phylotree <- eventReactive(input$makefile, {
  if(input$ampliconexampledata == TRUE){
    withProgress(message = "Reading Tree File",
                 detail = "This may take a while...", {
                   tree <- read.tree("data/testtree.newick")
                 })
  } else {
    inFile <- input$treefile1
    if (!is.null(inFile)){
      withProgress(message = "Reading Tree File",
                   detail = "This may take a while...", {
                     tree <- read_tree(inFile$datapath)
                   })
    }else if(is.null(inFile)){
      tree <- NULL
    }
  }
  return(tree)
})
output$phyloaction <- renderUI({
  actionButton("makefile", "Upload", width = "100%")
})
output$phyloreset <- renderUI({
  actionButton("resetfile", "Reset Uploaded Files", width = "100%")
})
observeEvent(input$resetfile, {
  reset("otufile1")
  reset("taxfile1")
  reset("mappingfile1")
  reset("treefile1")
})
phyloseqobj <- eventReactive(input$makefile, {
  withProgress(message = "Reading Files",
               detail = "This may take a while...", {
                 if(input$ampliconexampledata == TRUE){
                   file <- tryCatch(merge_phyloseq(otufile(), mappingfile(), phylotree()),
                                    error = function(cond){
                                      message("Error")
                                      return(NULL)
                                    },
                                    warning = function(cond){
                                      message("Warning")
                                      return(NULL)
                                    })
                 }else {
                   if(!is.null(phylotree())){
                     if(input$fileformat == "none"){
                       file <- tryCatch(phyloseq(otufile(), taxonomyfile(), mappingfile(), phylotree()),
                                        error = function(cond){
                                          message("Error")
                                          return(NULL)
                                        },
                                        warning = function(cond){
                                          message("Warning")
                                          return(NULL)
                                        })
                     } else if(input$fileformat == "qiime2"){
                       file <- tryCatch(qiime2R::qza_to_phyloseq(features = otufile(), 
                                                        taxonomy = taxonomyfile(), 
                                                        metadata = mappingfile(), 
                                                        tree = phylotree()),
                                        error = function(cond){
                                          message("Error")
                                          return(NULL)
                                        },
                                        warning = function(cond){
                                          message("Warning")
                                          return(NULL)
                                        })
                     } else if(input$fileformat == "qiime1"){
                       file <- tryCatch(merge_phyloseq(otufile(), mappingfile(), phylotree()),
                                        error = function(cond){
                                          message("Error")
                                          return(NULL)
                                        },
                                        warning = function(cond){
                                          message("Warning")
                                          return(NULL)
                                        })
                     }
                   }else if(is.null(phylotree())){
                     if(input$fileformat == "none"){
                       file <- tryCatch(phyloseq(otufile(), taxonomyfile(), mappingfile()),
                                        error = function(cond){
                                          message("Error")
                                          return(NULL)
                                        },
                                        warning = function(cond){
                                          message("Warning")
                                          return(NULL)
                                        })
                     } else if(input$fileformat == "qiime2"){
                       file <- tryCatch(qza_to_phyloseq(features = otufile(), 
                                               taxonomy = taxonomyfile(), 
                                               metadata = mappingfile()),
                                        error = function(cond){
                                          message("Error")
                                          return(NULL)
                                        },
                                        warning = function(cond){
                                          message("Warning")
                                          return(NULL)
                                        })
                     } else if(input$fileformat == "qiime1"){
                       file <- tryCatch(merge_phyloseq(otufile(), mappingfile()),
                                        error = function(cond){
                                          message("Error")
                                          return(NULL)
                                        },
                                        warning = function(cond){
                                          message("Warning")
                                          return(NULL)
                                        })
                     }
                   }
                 }
               })
  return(file)
})
observe({
  if(!is.null(phyloseqobj()))return(NULL)
    showNotification("Error Uploading Files. Please make sure files are formatted correctly.",
                     type = "error")
})
output$phyloseqprint <- renderPrint({
  if(is.null(phyloseqobj()))return(NULL)
  phyloseqobj()
})
output$phyloseqprint1 <- renderUI({
  output <- tagList(
    
  )
})
## Produce Initial Tables -----
output$otutable <- renderDataTable({
  if(is.null(phyloseqobj()))return(NULL)
  withProgress(message = "Rendering OTU Table:", {
    otu_table(phyloseqobj())
  })
})
output$otutableoutput <- renderUI({
  if(input$makefile == 0){
    tags$h3("Please Upload Files")
  }else {
    splitLayout(dataTableOutput("otutable"))
  }
})
output$taxtable <- renderDataTable({
  if(is.null(phyloseqobj()))return(NULL)
  withProgress(message = "Rendering Taxonomy Table:", {
    tax_table(phyloseqobj())
  })
})
output$taxtableoutput <- renderUI({
  if(input$makefile == 0){
    tags$h3("Please Upload Files")
  }else{
    splitLayout(dataTableOutput("taxtable"))
  }
})
output$mappingtableoutput <- renderDataTable({
  if(is.null(phyloseqobj()))return(NULL)
  withProgress(message = "Rendering Mapping Table:", {
    sample_data(phyloseqobj())
  })
})
output$mappingtablesummary <- renderPrint({
  if(is.null(phyloseqobj()))return(NULL)
  Hmisc::describe(sample_data(phyloseqobj()))
})
output$mappingtableoutputdisplay <- renderUI({
  if(input$makefile == 0){
    tags$h3("Please Upload Files")
  }else{
    output <- tagList(
    splitLayout(dataTableOutput("mappingtableoutput")),
    verbatimTextOutput("mappingtablesummary")
    )
  }
})
treedf <- reactive({
  if(is.null(phylotree()))return(NULL)
  tibble::as_tibble(phylotree())
})
output$treedftable <- renderDataTable({
  if(is.null(treedf())) return(NULL)
  #treedf()
  tibble::as_tibble(phylotree())
})
output$treedftableoutput <- renderUI({
  if(is.null(phylotree())){
    output <- tags$h3("No Tree File Uploaded")
  }else {
    output <- tagList(
      splitLayout(dataTableOutput("treedftable"))
      ,
      tags$h3("To View Your Uploaded Tree Graphically, Proceed to 
              the Community Composition Tab")
    )
  }
  return(output)
})