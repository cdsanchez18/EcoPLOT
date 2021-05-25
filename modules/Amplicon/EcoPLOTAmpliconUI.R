





EcoPLOTAmpliconUI<- tabPanel("Amplicon Data",
                             useShinyjs(),
         tabsetPanel(
           tabPanel("Guide",
                    includeMarkdown("amplicon_guide.Rmd")),
           tabPanel("Upload Files",
                    fluidPage(
                      titlePanel("Menu"),
                      sidebarLayout(
                        sidebarPanel(id = "homesidebar", "",
                                     conditionalPanel(condition = "input.tabstart == 1",
                                                      uiOutput("ampliconsampledata"),
                                                      uiOutput("fileformatoptions"),
                                                      uiOutput("fileuploadoptions"),
                                                      uiOutput("phyloaction"),
                                                      hr(),
                                                      uiOutput("phyloreset"),
                                                      uiOutput("phylomerge"))),
                        mainPanel("",
                                  tabsetPanel(id = "tabstart",
                                              tabPanel(value = 1, title = "Upload Files",
                                                       uiOutput("uploaddisplayui")
                                                       # tags$div(tags$h3("Data Summary")),
                                                       # verbatimTextOutput("phyloseqprint")
                                              ),
                                              tabPanel(value = 2, title = "ASV Table",
                                                       uiOutput("otutableoutput")),
                                              tabPanel(value = 3, title = "Taxonomy Table",
                                                       uiOutput("taxtableoutput")),
                                              tabPanel(value = 4, title = "Mapping Table",
                                                       uiOutput("mappingtableoutputdisplay")),
                                              tabPanel(value = 5, title = "Tree Table",
                                                       uiOutput("treedftableoutput"))
                                  ), id ="homemain")
                      ))),
           tabPanel("Filter Data",
                    fluidPage(
                      titlePanel("Filter Menu"),
                      sidebarLayout(
                        sidebarPanel(id = "filtersidebar", "",
                                     uiOutput("sidebarfilteroutputui")
                                     ),
                        mainPanel("",
                                  tabsetPanel(id = "filtertabset",
                                              tabPanel(value = 1, title ="Summary",
                                                       uiOutput("mainpanelfilteroutputui"),
                                                       uiOutput("mainpanelfilteroutputui2")
                                                       ),
                                              tabPanel(value= 2 , title = "Filtered ASV Table",
                                                       uiOutput("updatedphyloseqtableoutput")),
                                              tabPanel(value = 3, title = "Filtered Taxonomy Table",
                                                       uiOutput("updatedtaxtableoutput")),
                                              tabPanel(value = 4, title = "Filtered Mapping Table",
                                                       uiOutput("updatedmappingtableoutput")),
                                              tabPanel(value = 5, title = "Filtered Tree Table",
                                                       uiOutput("updatedtreetableoutput"))
                                  ), id = "filtermain"))#,
                      #conditionalPanel("input.filtertabset == 1",
                      #                 uiOutput("filtertabledownloadsidebar"))
                      )),
           tabPanel("Community Composition",
                    fluidPage(
                      titlePanel("Menu"),
                      sidebarLayout(
                        sidebarPanel("",
                                     shiny::radioButtons("amplicondatasource", "Select Dataset to Use:",
                                                         choices = c("Original"),
                                                         selected = "Original", inline = TRUE),
                                     conditionalPanel(condition = "input.bpstart == 1",
                                                      uiOutput("bpplotui")),
                                     conditionalPanel(condition = "input.bpstart == 2",
                                                      uiOutput("treeoptions"))),
                        mainPanel("",
                                  tabsetPanel(id = "bpstart",
                                              tabPanel(value = 1, title = "Phylogenetic Bar Plot",
                                                       uiOutput("stackedbarplotgraph")),
                                              tabPanel(value = 2, title = "Phylogenetic Tree",
                                                       uiOutput("phylotreeplotui"))
                                  )
                        ))
                      ,
                      conditionalPanel("input.bpstart == 1",
                                       uiOutput("barplotdownloadUI"))
                      )),
           tabPanel("Alpha Diversity", 
                    fluidPage(
                      titlePanel(""),
                      sidebarPanel("",
                                   conditionalPanel(condition = "input.alphadiversity == 1",
                                                    shiny::radioButtons("amplicondatasource1", "Select Dataset to Use:",
                                                                        choices = c("Original"),
                                                                        selected = "Original", inline = TRUE),
                                                    uiOutput("alphadivstatoptions"),
                                                    hr()),
                                   conditionalPanel(condition = "input.alphadiversity == 2",
                                                    uiOutput("alphadivoptions"))),
                      mainPanel("",
                                tabsetPanel(id ="alphadiversity",
                                            tabPanel(value = 1, "Statistics",
                                                     uiOutput("phyloseqalphatableui"),
                                                     verbatimTextOutput("alphadivstatprint")),
                                            tabPanel(value = 2, "Plot",
                                                     uiOutput("phyloseqplot2"))
                                            
                                ))    
                    )),
           tabPanel("Beta Diversity",
                    fluidPage(
                      titlePanel("Menu"),
                      sidebarLayout(
                        sidebarPanel(id = "ordinationplotsidebar",
                                     conditionalPanel(condition="input.ordinationstart == 1",
                                                      shiny::radioButtons("amplicondatasource2", "Select Dataset to Use:",
                                                                          choices = c("Original"),
                                                                          selected = "Original", inline = TRUE),
                                                      uiOutput("phyloseqdistanceoptions"),
                                                      uiOutput("makedistancematrixtable")),
                                     conditionalPanel(condition= "input.ordinationstart == 3",
                                                      uiOutput("adonisUI")),
                                     conditionalPanel(condition= "input.ordinationstart == 2",
                                                      uiOutput("ordinationplotoptions")),
                                     conditionalPanel(condition = "input.ordinationstart == 4",
                                                      uiOutput("threeDordinationplotoptions"))
                        ),
                        mainPanel(id = "ordinationplotmainpanel",
                                  tabsetPanel(id = "ordinationstart",
                                              tabPanel(value = 1, "Step 1: Create Distance Matrix",
                                                       splitLayout(dataTableOutput("distancematrixtable"))),
                                              tabPanel(value = 2, "Step 2: Plot Ordination",
                                                       uiOutput("ordinationplotoutputUI"),
                                                       uiOutput("ordinationdynamicselectbuttons")),
                                              tabPanel(value = 3, "Step 3: Perform Statistics",
                                                       uiOutput("adonissamplevars"),
                                                       verbatimTextOutput("adonisphyloseq")),
                                              tabPanel(value = 4, "Optional: 3D Ordination Plot",
                                                       uiOutput("threedordinationplotoutput"))
                                  )
                        )
                      )
                    )),
           tabPanel("Heatmap",
                    fluidPage(
                      titlePanel("Menu"),
                      sidebarLayout(
                        sidebarPanel(id = "heatmapsidebar",
                                     conditionalPanel("input.heatmaptab == 1",
                                     shiny::radioButtons("amplicondatasource3", "Select Dataset to Use:",
                                                         choices = c("Original"),
                                                         selected = "Original", inline = TRUE),
                                     uiOutput("heatmapoptions"),
                                     uiOutput("heatmapoptions1"),
                                     uiOutput("heatmapoptions2"),
                                     uiOutput("heatmapoptions3"),
                                     uiOutput("heatmapaction"))),
                        mainPanel("",
                                  tabsetPanel(id = "heatmaptab",
                                              tabPanel(value = 1, "Heatmap",
                                                       uiOutput("heatmapplotoutputui")))
                                  , id = "heatmapmain")
                      )
                    )),
           tabPanel("Differential Abundance",
                    fluidPage(
                      titlePanel("Menu"),
                      sidebarLayout(
                        sidebarPanel("",
                                     conditionalPanel(condition = "input.diffabundance == 1 ",
                                                      shiny::radioButtons("amplicondatasource4", "Select Dataset to Use:",
                                                                          choices = c("Original"),
                                                                          selected = "Original", inline = TRUE),
                                                      uiOutput("diffabundance")),
                                     conditionalPanel(condition = "input.diffabundance == 2",
                                                      uiOutput("volcanoplotui")),
                                     conditionalPanel(condition="input.diffabundance == 3",
                                                      uiOutput("log2foldchangeui"))),
                        mainPanel("",
                                  tabsetPanel(id = "diffabundance",
                                              tabPanel(value = 1, "Perform DESeq",
                                                       verbatimTextOutput("deseqresultprint"),
                                                       textOutput("deseqpvalue"),
                                                       verbatimTextOutput("deseqsummary"),
                                                       conditionalPanel("input.performdeseq",
                                                       splitLayout(
                                                         tags$h4("Results Table"),
                                                         tags$h4("ASV Plot")
                                                       ),
                                                       splitLayout(
                                                         dataTableOutput("deseqpvaluelist"),
                                                         plotOutput("deseqasvplot1")),
                                                       splitLayout(
                                                         downloadTableUI(id = "deseqpvaluelistdownload"),
                                                         downloadPlotUI(id = "deseqasvplot1download")
                                                       ))),
                                              tabPanel(value = 2, "Volcano Plot",
                                                       uiOutput("scatter12"),
                                                       verbatimTextOutput("volcanobrushtest"),
                                                       splitLayout(dataTableOutput("volcanotable1")),
                                                       #uiOutput("volcanodynamicselectbuttons"),
                                                       #splitLayout(dataTableOutput("volcanotesttable")),
                                                       #splitLayout(downloadTableUI("volcanoselectionstable"),
                                                       #            downloadTableUI("volcanodesequpdated"))
                                              )#,
                                              #tabPanel(value=3, "Log2FoldChange Plot",
                                              #         uiOutput("log2foldchangegraphoutput"))
                                              ))
                      )
                    ))
         ))