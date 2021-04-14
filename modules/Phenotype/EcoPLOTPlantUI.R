





EcoPLOTPlantUI <- tabPanel("Phenotype Data",
                           tabsetPanel(id = "phenotypeui",
                             tabPanel("Guide",
                                      includeMarkdown("plant_guide.rmd")),
                             tabPanel("Upload File",
                                      titlePanel(""),
                                      fluidPage(
                                        tags$head(
                                          tags$style(
                                            HTML(".shiny-notification-error {
                                  position:fixed;
                                  top: calc(25%);
                                  font-size:2vmin;
                                  background: red;
                                  color: rgba(0, 0, 0, 1);
                                  border:solid;
                                  border-color:black;
                                  border-size: 3px;
                                  text-align:center;
                                  left: calc(40%);
                                  width: 35%;
                             }
                             .shiny-notification-warning {
                               position:fixed;
                               top: calc(25%);
                               font-size:2vmin;
                               background: orange;
                               color: rgba(0, 0, 0, 1);
                               border:solid;
                               border-color:black;
                               border-size: 3px;
                               text-align:center;
                               left: calc(40%);
                               width: 35%;
                             }
                                  "
                                            )
                                          )
                                        ),
                                        titlePanel("Menu"),
                                        sidebarLayout(
                                          sidebarPanel("",
                                                       #uiOutput("plantfileupload")
                                                       checkboxInput("phenotypeexampledata", "Use Example Data", value = FALSE, width = "100%")
                                                       ,
                                                       fluidRow(
                                                         column(5, hr()),
                                                         column(2,tags$div(tags$h4("OR"), align="center")),
                                                         column(5, hr())
                                                       )
                                                       ,
                                                       h4(tags$u("Upload File:")," EcoPLOT accepts",tags$b(".csv, .txt, .xlsx "),"file formats.")
                                                       ,
                                                       fileInput("phenotypedata", "Select File",
                                                                 multiple = FALSE,
                                                                 accept = c("text/csv",
                                                                            "text/comma-separated-values,text/plain",
                                                                            ".csv"))
                                                       ,
                                                       tags$hr(),
                                                       fluidRow(
                                                         column(4,
                                                                checkboxInput("header", "Header", TRUE)),
                                                         column(4,
                                                                radioButtons("sep", "Separator",
                                                                             choices = c(Comma = ",",
                                                                                         Semicolon = ";",
                                                                                         Tab = "\t"),
                                                                             selected = ","))
                                                         ,
                                                         column(4,
                                                                radioButtons("quote", "Quote",
                                                                             choices = c(None = "",
                                                                                         "Double Quote" = '"',
                                                                                         "Single Quote" = "'"),
                                                                             selected = '"')))
                                                       ,
                                                       uiOutput("phenotypemergeUI")
                                                       # conditionalPanel("input.environmentdata || input.environmentexampledata",
                                                       #                  conditionalPanel("input.phenotypedata || input.phenotypeexampledata",
                                                       #                  hr()
                                                       #                  ,
                                                       #                  tags$h4("Merge Datasets")
                                                       #                  ,
                                                       #                  checkboxInput("phenotypemergefiles", "Merge Environment and Phenotype Datasets?", value = FALSE, width = "100%")))
                                          #)
                                                       ),
                                          mainPanel("",
                                                    uiOutput("plantuploadmain")))),
                                      uiOutput("changeclassUI")
                             ),
                             tabPanel("Filter",
                                      titlePanel(""),
                                      fluidPage(
                                        titlePanel("Menu"),
                                        sidebarLayout(
                                          sidebarPanel("",
                                                       uiOutput("phenotypefilteringoptionsUI"),
                                                       uiOutput("phenotypefilteringoptionsUI1"),
                                                       uiOutput("phenotypefilteringoptionsUI2")),
                                          mainPanel("",
                                                    uiOutput("phenotypefiltertableUI"))
                                        )
                                      )),
                             tabPanel("Create Plot",
                                      fluidPage(
                                        titlePanel("Menu"),
                                        sidebarLayout(
                                          sidebarPanel(id = "plantplotsidebar",
                                                       shiny::radioButtons("phenotypedatasource", "Select Dataset to Use:",
                                                                           choices = c("Original"),
                                                                           selected = "Original", inline = TRUE),
                                                       uiOutput("phenotypeplotUI")),
                                          mainPanel(id = "plantplotmainpanel",
                                                    uiOutput("correlationoutput"),
                                                    uiOutput("phenotypeplotmainUI"),
                                                    uiOutput("phenotypedynamicselectbuttons")
                                          )))),
                             tabPanel("Statistics",
                                      fluidPage(
                                        titlePanel("Menu"),
                                        sidebarLayout(
                                          sidebarPanel(id = "plantstatssidebar",
                                                       shiny::radioButtons("phenotypedatasource1", "Select Dataset to Use:",
                                                                           choices = c("Original"),
                                                                           selected = "Original", inline = TRUE),
                                                       conditionalPanel("input.plantstats == 2",
                                                                        uiOutput("phenotypeparametricUI")),
                                                       conditionalPanel("input.plantstats == 3",
                                                                        uiOutput("phenotypenonparametricUI"))
                                          ),
                                          mainPanel("",
                                                    tabsetPanel(id ="plantstats",
                                                                tabPanel(value = 1, "Guide",
                                                                         includeMarkdown("stats_guide.Rmd")),
                                                                tabPanel(value = 2, "Parametric",
                                                                         uiOutput("phenotypeparametricMain")),
                                                                tabPanel(value = 3, "Non-Parametric",
                                                                         uiOutput("phenotypenonparametricMain"))), id = "plantstats1")
                                        )
                                      ))
                           ))


