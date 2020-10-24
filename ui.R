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

#EcoPOD data----------------------------------------------------------------------
shinyUI(navbarPage("Singer Lab",
  useShinyjs(),
  tabPanel("Instructions"),
  navbarMenu("Environment",
    tabPanel("EcoPOD Sensors", 
            tabsetPanel(
              tabPanel("Upload",
              titlePanel(""),
              fluidPage(theme = shinytheme("yeti"),
              titlePanel("Menu"),
              sidebarLayout(
                sidebarPanel("",
                  fileInput("ecopoddata", "Browse for Excel File:",
                          multiple = FALSE,
                          accept = c(".xlsx"))),
                mainPanel("",
                  tabsetPanel(
                    tabPanel("Uploaded Data",
                      splitLayout(tableOutput("ecopoddatatable"))),
                    tabPanel("Melted Data",
                      splitLayout(tableOutput("ecopodmelt1")))))))),
              tabPanel("Plot Data",
                fluidPage(
                  titlePanel("Menu"),
                    sidebarLayout(
                      sidebarPanel("",
                        uiOutput("ecopodx"),
                        numericInput("size0", "Point size", 1, min = 1),
                        uiOutput("slider0"),
                        textInput("ecopodspace", "Tick Spacing", "6 hours"),
                        h5("NOTE: You may use hours, days, or years"),
                        textInput("title0", "Graph Title", ""),
                        textInput("ecopodxaxis", "Label X-Axis", ""),
                        textInput("ecopodyaxis", "Label Y-Axis", ""),
                        actionButton("ecopodrender", "Render:"),
                        h5("NOTE: This button will create both the graph and
                            the fitered dataset.")),
                      mainPanel(
                        tabsetPanel(
                          tabPanel("EcoPod Graph",
                            plotlyOutput("ecopodplot")),
                          tabPanel("Filtered Data Table",
                            tableOutput("ecopodfiltertable"),
                            downloadButton("downloadecopoddata",
                                          "Download Filtered Dataset"))))))))),
#Additional Sensors ----------------------------------------------------------------
    tabPanel("Additional Sensors",
      tabsetPanel(
        tabPanel("Upload CSV File",
          titlePanel(""),
          fluidPage(
            titlePanel("Menu"),
            sidebarLayout(
              sidebarPanel("",
                fileInput("sensordata", "Choose CSV/Excel File",
                          multiple = FALSE,
                          accept = c(".xlsx",
                                    "text/csv",
                                    "text/comma-separated-values,text/plain",
                                    ".csv")),
                tags$hr(),
                checkboxInput("header", "Header", TRUE),
                radioButtons("sep", "Separator",
                            choices = c(Comma = ",",
                                        Semicolon = ";",
                                        Tab = "\t"),
                                        selected = ","),
                radioButtons("quote", "Quote",
                            choices = c(None = "",
                                        "Double Quote" = '"',
                                        "Single Quote" = "'"),
                                        selected = '"'),
                tags$hr(),
                radioButtons("disp", "Display",
                            choices = c(Head = "head",
                                        All = "all"),
                                        selected = "head")),
              mainPanel("",
                tabsetPanel(
                  tabPanel("Uploaded Data",
                    splitLayout(tableOutput("sensordatatable"))),
                  tabPanel("Melted Data",
                    splitLayout(tableOutput("sensormelt1")))))))),
          tabPanel("Plot Data",
            fluidPage(
              titlePanel("Menu"),
                sidebarLayout(
                  sidebarPanel("",
                    uiOutput("sensorx"),
                    textInput("title1", "Graph Title", ""),
                    numericInput("size1", "Point size", 1, min = 1),
                    uiOutput("sensorrange"),
                    uiOutput("slider1"),
                    textInput("sensorspace", "Tick Spacing", "6 hours"),
                    h5("NOTE: You may use hours, days, or years"),
                    textInput("sensorxaxis", "Label X-Axis", ""),
                    actionButton("sensorrender", "Render:"),
                    h5("NOTE: This button will create both the graph and
                        the fitered dataset.")),
                  mainPanel(
                    tabsetPanel(
                      tabPanel("Sensor Graph",
                        plotlyOutput("sensorplot")),
                      tabPanel("Filtered Data Table",
                        tableOutput("sensorfiltertable"),
                        downloadButton("downloadsensordata",
                                      "Download Filtered Dataset"))))))))),
#Geochemistry-----------------------------------------------------------------------
    tabPanel("Geochemistry",
      tabsetPanel(
        tabPanel("Upload File",
          titlePanel(""),
          fluidPage(
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
      ))),
  #Phenotype data---------------------------------------------------------------------
  navbarMenu("Plant",
             tabPanel("Phenotype Data",
                   tabsetPanel(
                   tabPanel("Upload File",
                       titlePanel(""),
                       fluidPage(
                           titlePanel("Menu"),
                           sidebarLayout(
                               sidebarPanel("",
                                            uiOutput("plantfileupload"),
                                            uiOutput("plantvariableclassUI")),
                               mainPanel("",
                                         verbatimTextOutput("testprint"),
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
                                            uiOutput("phenotypeplotUI")),
                                mainPanel("",
                                            verbatimTextOutput("phenotypecorrelation"),
                                            uiOutput("phenotypeplotmainUI"))))),
                   tabPanel("Statistics",
                            fluidPage(
                              titlePanel("Menu"),
                              sidebarLayout(
                                sidebarPanel(
                                  uiOutput("phenotypeparametricUI")
                                ),
                                mainPanel("",
                                          tabsetPanel(id ="plantstats",
                                                      tabPanel("Parametric",
                                                        uiOutput("phenotypeparametricMain")
                                                               ),
                                                      tabPanel("Non-Parametric")))
                              )
                            ))
                   ))),
  #Microbiome 2--------
  navbarMenu("Microbiome",
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
                               tabPanel(value= 2 , title = "Updated ASV Table",
                                        uiOutput("updatedphyloseqtableoutput")),
                               tabPanel(value = 3, title = "Updated Taxonomy Table",
                                        uiOutput("updatedtaxtableoutput")),
                               tabPanel(value = 4, title = "Updated Mapping Table",
                                        uiOutput("updatedmappingtableoutput")),
                               tabPanel(value = 5, title = "Updated Tree Table",
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
              conditionalPanel(condition = "input.ordinationstart == 2",
                               uiOutput("dispersionUI")),
              conditionalPanel(condition= "input.ordinationstart == 3",
                              uiOutput("adonisUI")),
              conditionalPanel(condition= "input.ordinationstart == 4",
                uiOutput("ordinationplotoptions"))
              ),
            mainPanel("",
                      tabsetPanel(id = "ordinationstart",
                        tabPanel(value = 1, "Step 1: Create Distance Matrix",
                                 splitLayout(dataTableOutput("distancematrixtable"))),
                        tabPanel(value = 2, "Dispersion",
                                 uiOutput("betadispersionplot2"),
                                 verbatimTextOutput("betadisptable")),
                        tabPanel(value = 3, "Statistics",
                                 verbatimTextOutput("adonisphyloseq")),
                        tabPanel(value = 4, "Plot",
                                 uiOutput("ordinationplotoutputUI"))
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
        )),
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
      tabPanel("IRF",
               fluidPage(
                 titlePanel("Menu"),
                 sidebarLayout(
                   sidebarPanel(""),
                   mainPanel()
                 )
               ))
      )),
  tabPanel("Shotgun Data")),
tabPanel("Statistics",
         fluidPage(
           titlePanel(""),
           sidebarLayout(
             sidebarPanel("Menu",
                          uiOutput("statselect"),
                          uiOutput("stataction")),
             mainPanel(
               tabsetPanel(
                 tabPanel("Data Table",
                          splitLayout(dataTableOutput("stattable")),
                          verbatimTextOutput("stattest")),
                 tabPanel("Stat Table",
                          splitLayout(dataTableOutput("statresulttable")))))
             ))
           )
         ))
#))



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
