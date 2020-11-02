#packages----------
library(shiny)
library(phyloseq)
library(readxl)
library(treeio)
library(colourpicker)
library(ggtree)
library(shinyjs)
#library(htmltools)
library(ggplot2)
library(metacoder)
library(taxa)
library(vegan)
library(crosstalk)
library(tidyr)
library(lubridate)
library(dplyr)
library(reshape2)
library(stringr)
library(shinythemes)
library(plotly)
library(shinycssloaders)
library(RColorBrewer)
library(DT)
library(DESeq2)
library(EcoPLOT)

shinyUI(navbarPage("Singer Lab",
  useShinyjs(),
  tabPanel("Instructions"),
#Geochemistry-----------------------------------------------------------------------
    tabPanel("Geochemistry",
      tabsetPanel(
        tabPanel("Upload File",
          titlePanel(""),
          fluidPage(theme = shinytheme("yeti"),
            titlePanel("Menu"),
              sidebarLayout(
                sidebarPanel("",
                             uiOutput("geochemistryfileupload"),
                             uiOutput("geochemistryvariableclassUI")),
                mainPanel("",
                          uiOutput("geochemistryuploadmain"))))),
        tabPanel("Filter"),
        tabPanel("Soil Index Ratios",
          titlePanel(""),
            fluidPage(
              fluidRow(
                column(width = 12,
              splitLayout(tableOutput("geochemtable")))),
              fluidRow(
              column(width = 6,
              h5("This is your C to N to P Ratio:"),
              wellPanel(textOutput("CNP"))),
              column(width = 6,
              h5("This is your N to P to K Ratio:"),
              wellPanel(textOutput("NPK")))),
              fluidRow(
              column(width = 6, offset = 5,
              h5("Sand, Silt, Clay Particle Sizes"),
              splitLayout(tableOutput("ssc")))))),
        tabPanel("Soil Texture Triangle",
          titlePanel(""),
          tags$div(
                uiOutput("texturetriangle"),
                align = "center")),
        tabPanel("Create Plot",
                 fluidPage(
                   titlePanel("Menu"),
                   sidebarLayout(
                     sidebarPanel("",
                               uiOutput("geochemistryplotUI")),
                     mainPanel("",
                               uiOutput("geochemistryplotmainUI"))))),
        tabPanel("Statistics",
                 fluidPage(
                   titlePanel("Menu"),
                   sidebarLayout(
                     sidebarPanel(),
                     mainPanel("",
                               tabsetPanel(id ="geochemistrystats",
                                           tabPanel("Parametric"),
                                           tabPanel("Non-Parametric"))))))
      )),
  #Phenotype data---------------------------------------------------------------------
             tabPanel("Phenotype Data",
                   tabsetPanel(
                   tabPanel("Upload File",
                       titlePanel(""),
                       fluidPage(
                         tags$head(
                           tags$style(
                             HTML(".shiny-notification {
                                  position:fixed;
                                  top: calc(25%);
                                  font-size:2vmin;
                                  text-align:center;
                                  left: calc(25%);
                                  width: 50%;
                             }
                                  #sidebar {
                                  background-color: #4C9F93;

                                  }          
                                            ")
                             # padding:5px;
                             # border: 1px;
                             # border-style: solid;
                           )
                         ),
                           titlePanel("Menu"),
                           sidebarLayout(
                               sidebarPanel("",
                                            uiOutput("plantfileupload"),
                                            hr(),
                                            tags$div(id = "sidebar",
                                            uiOutput("plantvariableclassUI"))),
                               mainPanel("",
                                            uiOutput("plantuploadmain"))))),
                   tabPanel("Filter",
                            titlePanel(""),
                            fluidPage(
                              titlePanel("Menu"),
                              sidebarLayout(
                                sidebarPanel("",
                                             uiOutput("phenotypefilteringoptionsUI"),
                                             uiOutput("phenotypefilteringoptionsUI1")),
                                mainPanel("",
                                          uiOutput("phenotypefiltertableUI"))
                              )
                            )),
                   tabPanel("Create Plot",
                            fluidPage(
                              titlePanel("Menu"),
                              sidebarLayout(
                                sidebarPanel("",
                                            #uiOutput("phenotypedatasourceUI"),
                                            shiny::radioButtons("phenotypedatasource", "Select Dataset to Use:",
                                                                choices = c("Original"),
                                                                selected = "Original", inline = TRUE),
                                            uiOutput("phenotypeplotUI")),
                                mainPanel("",
                                            uiOutput("correlationoutput"),
                                            #verbatimTextOutput("phenotypecorrelation"),
                                            uiOutput("phenotypeplotmainUI"))))),
                   tabPanel("Statistics",
                            fluidPage(
                              titlePanel("Menu"),
                              sidebarLayout(
                                sidebarPanel(
                                  shiny::radioButtons("phenotypedatasource1", "Select Dataset to Use:",
                                                      choices = c("Original"),
                                                      selected = "Original", inline = TRUE),
                                  #uiOutput("phenotypedatasource1UI"),
                                  conditionalPanel("input.plantstats == 1",
                                                   uiOutput("phenotypeparametricUI")),
                                  conditionalPanel("input.plantstats == 2",
                                                   uiOutput("phenotypenonparametricUI"))
                                ),
                                mainPanel("",
                                          tabsetPanel(id ="plantstats",
                                                      tabPanel(value = 1, "Parametric",
                                                        uiOutput("phenotypeparametricMain")),
                                                      tabPanel(value = 2, "Non-Parametric",
                                                        uiOutput("phenotypenonparametricMain"))))
                              )
                            ))
                   )),
  #Microbiome 2--------
  tabPanel("Amplicon Data",
    tabsetPanel(
      tabPanel("Upload Files",
        fluidPage(
        titlePanel("Menu"),
          sidebarLayout(
            sidebarPanel(id = "homesidebar", "",
                conditionalPanel(condition = "input.tabstart == 1",
                         uiOutput("fileformatoptions"),
                         uiOutput("fileuploadoptions"),
                         uiOutput("phyloaction"))),
            mainPanel("",
              tabsetPanel(id = "tabstart",
                tabPanel(value = 1, title = "Please Read",
                  tags$div(tags$h2("This is where a copy of the Read.me will be"),
                           align = "center"),
                  hr(),
                  tags$div(tags$h3("Data Summary")),
                  verbatimTextOutput("phyloseqprint")),
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
                                tags$div(tags$h5("Filter by Taxonomy or ASV:"),
                                         align = "center"),
                                uiOutput("phyloseq_tax_ranks"),
                                uiOutput("unique_taxa"),
                                hr(),
                                tags$div(tags$h5("Filter by Sample ID:"),
                                         align = "center"),
                                uiOutput("phyloseq_sample_variables"),
                                hr(),
                                tags$div(tags$h5("Filter by Experimental Design:"),
                                         align = "center"),
                                uiOutput("samplefilter"),
                                uiOutput("samplecontainer"),
                                uiOutput("addsamplefilter"),
                                hr(),
                                tags$div(tags$h5("Filter by Presence:"),
                                         align = "center"),
                                uiOutput("phyloseqfilteroptions")),
                   mainPanel("",
                             tabsetPanel(id = "filtertabset",
                               tabPanel(value = 1, title ="Summary",
                                        tags$h3("Original Data Summary"),
                                        verbatimTextOutput("originalphyloseqsummary"),
                                        splitLayout(
                                          plotOutput("originalsamplecounthist"),
                                          plotOutput("originaltaxacounthist")
                                        ),
                                        splitLayout(
                                        uiOutput("counthistsummaryorigsample"),
                                        uiOutput("counthisttaxa")
                                        ),
                                        splitLayout(
                                          downloadPlotUI("originalsamplecounthistplot"),
                                          downloadPlotUI("originaltaxacounthistplot")
                                        )
                                        ,
                                        hr(),
                                        tags$h3("Filtered Data Summary"),
                                        verbatimTextOutput("updatedphyloseqsummary"),
                                        splitLayout(
                                          plotOutput("updatedsamplecounthist"),
                                          plotOutput("updatedtaxacounthist")
                                        ),
                                        splitLayout(
                                          uiOutput("counthistsummaryorigsampleupdated"),
                                          uiOutput("counthisttaxaupdated")
                                        ),
                                        splitLayout(
                                          downloadPlotUI("updatedsamplecounthistplot"),
                                          downloadPlotUI("updatedtaxacounthistplot")
                                        )),
                               tabPanel(value= 2 , title = "Filtered ASV Table",
                                        uiOutput("updatedphyloseqtableoutput")),
                               tabPanel(value = 3, title = "Filtered Taxonomy Table",
                                        uiOutput("updatedtaxtableoutput")),
                               tabPanel(value = 4, title = "Filtered Mapping Table",
                                        uiOutput("updatedmappingtableoutput")),
                               tabPanel(value = 5, title = "Filtered Tree Table",
                                        uiOutput("updatedtreetableoutput"))
                             ), id = "filtermain"))
               )),
      tabPanel("Community Composition",
               fluidPage(
                 titlePanel("Menu"),
                 sidebarLayout(
                   sidebarPanel("",
                                uiOutput("bpdatasetchoice"),
                                conditionalPanel(condition = "input.bpstart == 1",
                                                 uiOutput("bpplotui")),
                                conditionalPanel(condition = "input.bpstart == 2",
                                                 uiOutput("treeoptions"))),
                   mainPanel("",
                             tabsetPanel(id = "bpstart",
                                         tabPanel(value = 1, title = "Phylogenetic Bar Plot",
                                                  uiOutput("stackedbarplotgraph"),
                                                  hr(),
                                                  downloadPlotUI("barplotdownload")),
                                         tabPanel(value = 2, title = "Phylogenetic Tree",
                                                  uiOutput("phylotreeplotui"))
                             )
                   ))
               )),
      tabPanel("Alpha Diversity", 
                          fluidPage(
                            titlePanel(""),
                            sidebarPanel("",
                                         conditionalPanel(condition = "input.alphadiversity == 1",
                                          uiOutput("alphadivdatasetoption"),
                                          uiOutput("alphadivstatoptions"),
                                          hr(),
                                          downloadTableUI("alphadiversitystattable")),
                                         conditionalPanel(condition = "input.alphadiversity == 2",
                                          uiOutput("alphadivoptions"),
                                          hr(),
                                          downloadPlotUI(id = "alphadiversityplotdownload"))),
                            mainPanel("",
                              tabsetPanel(id ="alphadiversity",
                                tabPanel(value = 1, "Statistics",
                                        uiOutput("phyloseqalphatableui"),
                                        #splitLayout(dataTableOutput("phyloseqalphatable")),
                                        verbatimTextOutput("alphadivstatprint")),
                                tabPanel(value = 2, "Plot",
                                       # uiOutput("phyloseqalphatableui"))
                                      plotOutput("phyloseqplot1"))
                                
                                ))    
                          )),
      tabPanel("Beta Diversity",
        fluidPage(
          titlePanel("Menu"),
          sidebarLayout(
            sidebarPanel("",
              conditionalPanel(condition="input.ordinationstart == 1",
                               uiOutput("ordinationdataoption"),
                               uiOutput("phyloseqdistanceoptions"),
                               uiOutput("makedistancematrixtable"),
                               hr(),
                               downloadTableUI(id = "distancematrixtabledownload")),
              #conditionalPanel(condition = "input.ordinationstart == 2",
               #                uiOutput("dispersionUI")),
              conditionalPanel(condition= "input.ordinationstart == 3",
                              uiOutput("adonisUI")),
              conditionalPanel(condition= "input.ordinationstart == 2",
                uiOutput("ordinationplotoptions"))
              ),
            mainPanel("",
                      tabsetPanel(id = "ordinationstart",
                        tabPanel(value = 1, "Step 1: Create Distance Matrix",
                                 splitLayout(dataTableOutput("distancematrixtable"))),
                        tabPanel(value = 2, "Step 2: Plot Ordination",
                                 uiOutput("ordinationplotoutputUI")),
                        #tabPanel(value = 2, "Dispersion",
                        #         uiOutput("betadispersionplot2"),
                        #         verbatimTextOutput("betadisptable")),
                        tabPanel(value = 3, "Step 3: Perform Statistics",
                                 verbatimTextOutput("adonisphyloseq"))
                      )
            )
          )
        )),
      tabPanel("Heatmap",
        fluidPage(
          titlePanel("Menu"),
          sidebarLayout(
            sidebarPanel("",
                         uiOutput("heatmapoptions"),
                         uiOutput("heatmapoptions1"),
                         uiOutput("heatmapoptions2"),
                         uiOutput("heatmapoptions3"),
                         uiOutput("heatmapaction"),
                         downloadPlotUI("heatmapplotoutputdownload")),
            mainPanel("",
                      plotOutput("heatmapplotoutput"))
          )
        )),
      tabPanel("Differential Abundance",
        fluidPage(
          titlePanel("Menu"),
          sidebarLayout(
            sidebarPanel("",
                         conditionalPanel(condition = "input.diffabundance == 1 ",
                         uiOutput("diffabundance")),
                         conditionalPanel(condition = "input.diffabundance == 2",
                         uiOutput("volcanoplotui")),
                         conditionalPanel(condition="input.diffabundance == 3",
                         uiOutput("log2foldchangeui"))),
            mainPanel("",
                      tabsetPanel(id = "diffabundance",
                        tabPanel(value = 1, "Perform deseq2",
                                 verbatimTextOutput("deseqresultprint"),
                                 textOutput("deseqpvalue"),
                                 verbatimTextOutput("deseqsummary"),
                                 splitLayout(
                                 dataTableOutput("deseqpvaluelist"),
                                 plotOutput("deseqasvplot1")),
                                 splitLayout(
                                   downloadTableUI(id = "deseqpvaluelistdownload"),
                                   downloadPlotUI(id = "deseqasvplot1download")
                                 )),
                      tabPanel(value = 2, "Volcano Plot",
                               plotOutput("scatter1", brush = "volcanobrush", width = "100%"),
                               uiOutput("volcanosaveselection"),
                               uiOutput("volcanoseparateselection"),
                               uiOutput("volcanoresetselection"),
                               hr(),
                               uiOutput("volcanoselectionName"),
                               uiOutput("volcanocontainer"),
                               uiOutput("volcanonotext"),
                               uiOutput("volcanocolumnName"),
                               hr(),
                               uiOutput("volcanoactionbutton"),
                               splitLayout(dataTableOutput("volcanotable1"),
                               dataTableOutput("volcanotesttable")),
                               splitLayout(downloadTableUI("volcanoselectionstable"),
                                           downloadTableUI("volcanodesequpdated"))
                               ),
                      tabPanel(value=3, "Log2FoldChange Plot",
                               plotOutput("log2foldchangegraph"))))
          )
        ))
      # tabPanel("Normalize",
      #          fluidPage(
      #            titlePanel("Normalize"),
      #            sidebarLayout(
      #              sidebarPanel("",
      #                           uiOutput("normalization"),
      #                           uiOutput("rarefycounts"),
      #                           uiOutput("normalizego")),
      #              mainPanel(
      #                tabsetPanel(
      #                tabPanel("Updated Table",
      #                   splitLayout(uiOutput("taxmaptable3"))))),
      #              )
      #            )),
    )),
tabPanel("IRF",
                    fluidPage(
                      titlePanel("Menu"),
                      sidebarLayout(
                        sidebarPanel(""),
                        mainPanel()
                      )
                    )
                    )
         ))

# 

#   
# #Microbiome-------------------------------------------------------------------------
#   tabPanel("Microbiome", 
#            tabsetPanel(
#              tabPanel("Upload CSV File",
#                 titlePanel(""),
#                 fluidPage(
#                   titlePanel("Menu"),
#                   sidebarLayout(
#                   sidebarPanel("",
#                       fileInput("file1", "Upload ASV File:",
#                                 multiple = FALSE,
#                                 accept = c("text/csv",
#                                            "text/comma-separated-values,text/plain",
#                                            ".csv")),
#               tags$hr(),
#                   checkboxInput("header", "Header", TRUE)),
#                 mainPanel("",
#                   uiOutput("tabs")))
#                         )
#                       ),
#              tabPanel("Plot Data",
#                 titlePanel(""), 
#                 fluidPage(
#                   titlePanel("Menu"),
#                   sidebarLayout(
#                   sidebarPanel("",
#                       selectInput("data", "Select Taxonomic Rank to Plot:", 
#                           choices = c("Kingdom", "Phylum", "Class", "Order", 
#                                       "Family", "Genus", "Species"),
#                           selected = "Kingdom",
#                               multiple = FALSE),
#                   uiOutput("selectfactors"),
#           hr(),
#                 h5("Manipulate Your Dataframe:"),
#                   uiOutput("treatment"),
#                   uiOutput("timepoint"),
#                   uiOutput("compartment"),
#                   uiOutput("depth"),
#                   uiOutput("threshold"),
#                   actionButton("render", "Click to render dataframe:"),
#           hr(),
#                 h5("Select What to Plot:"),
#                   uiOutput("xaxis"),
#                   uiOutput("facet"),
#           hr(),
#                 h5("Create Labels for Plot"),
#                   textInput("xaxis5", "Label for X-Axis", ""),
#                   textInput("yaxis5", "Label for Y- Axis", ""),
#                   textInput("title5", "Create Title for Graph", "")),
#             mainPanel(
#               tabsetPanel(type = "tabs",
#                 tabPanel("Filtered Data",
#                   splitLayout(tableOutput("Filter")),
#                   downloadButton("downloaddata", "Download Filtered Dataset")),
#                 tabPanel("finalfilter",
#                   splitLayout(tableOutput("finalfilter"))),
#                 tabPanel("Plot",
#                   splitLayout(plotlyOutput("OTUplot")))
#            ))
#   )
#         )
#   )
#   )), 

# 
# 
# ))
#              
#            
# 
#   
#             
# 
#                
#   
# 
