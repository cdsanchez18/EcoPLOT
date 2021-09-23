



EcoPLOTGeochemUI <- tabPanel("Environmental Data",
                                 tabsetPanel(
                                   tabPanel("Guide",
                                            includeMarkdown("modules/Geochemistry/Guides/environment_guide.rmd")),
                                   tabPanel("Upload File",
                                            titlePanel(""),
                                            fluidPage(
                                              tags$head(
                                                tags$style(
                                                )
                                              ),
                                              titlePanel("Menu"),
                                              sidebarLayout(
                                                sidebarPanel("",
                                                             #uiOutput("environmentfileupload")
                                                             checkboxInput("environmentexampledata", "Use Example Data", value = FALSE, width = "100%")
                                                             ,
                                                             fluidRow(
                                                               column(5, hr()),
                                                               column(2,tags$div(tags$h4("OR"), align="center")),
                                                               column(5, hr())
                                                             )
                                                             ,
                                                             h4(tags$u("Upload File:")," EcoPLOT accepts",tags$b(".csv, .txt, .xlsx "),"file formats.")
                                                             ,
                                                             fileInput("environmentdata", "Select File",
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
                                                             uiOutput("environmentmergeUI")
                                                             #conditionalPanel("input.phenotypedata || input.phenotypeexampledata",
                                                              #                conditionalPanel("input.environmentdata || input.environmentexampledata",
                                                              #                                 hr()
                                                              #                                 ,
                                                              #                                 tags$h4("Merge Datasets")
                                                              #                                 ,
                                                               #                                checkboxInput("environmentmergefiles", "Merge Environment and Phenotype Datasets?", value = FALSE, width = "100%")))
                                                ),
                                                #),
                                                mainPanel("",
                                                          uiOutput("environmentuploadmain")))),
                                            uiOutput("environmentchangeclassUI")
                                   ),
                                   tabPanel("Filter",
                                            titlePanel(""),
                                            fluidPage(
                                              titlePanel("Menu"),
                                              sidebarLayout(
                                                sidebarPanel("",
                                                             uiOutput("environmentfilteringoptionsUI"),
                                                             uiOutput("environmentfilteringoptionsUI1"),
                                                             uiOutput("environmentfilteringoptionsUI2")),
                                                mainPanel("",
                                                          uiOutput("environmentfiltertableUI"))
                                              )
                                            )),
                                   tabPanel("Soil Index Ratios",
                                            titlePanel(""),
                                            fluidPage(
                                              fluidRow(
                                                column(width = 12,
                                                       shiny::radioButtons("environmentdatasource2", "Select Dataset to Use:",
                                                                           choices = c("Original"),
                                                                           selected = "Original", inline = TRUE)
                                                       ,
                                                       hr()
                                                )),
                                              uiOutput("soilindexuioutput")
                                              #fluidRow(
                                              #   column(width = 12,
                                              #          shiny::radioButtons("environmentdatasource2", "Select Dataset to Use:",
                                              #                              choices = c("Original"),
                                              #                              selected = "Original", inline = TRUE)
                                              #          ,
                                              #          hr()
                                              #   )),
                                              # fluidRow(
                                              #   column(width = 12,
                                              #          splitLayout(dataTableOutput("environmenttable")))),
                                              # fluidRow(
                                              #   column(width = 6,
                                              #          h5("This is your C to N to P Ratio:"),
                                              #          wellPanel(textOutput("CNP"))),
                                              #   column(width = 6,
                                              #          h5("This is your N to P to K Ratio:"),
                                              #          wellPanel(textOutput("NPK")))),
                                              # fluidRow(
                                              #   column(width = 6, offset = 5,
                                              #          h5("Sand, Silt, Clay Particle Sizes"),
                                              #          splitLayout(tableOutput("ssc"))))
                                            )),
                                   tabPanel("Soil Texture Triangle",
                                            titlePanel(""),
                                            tags$div(
                                              uiOutput("texturetriangle"),
                                              align = "center")
                                   ),
                                   tabPanel("Create Plot",
                                            fluidPage(
                                              titlePanel("Menu"),
                                              sidebarLayout(
                                                sidebarPanel(id = "environmentplotsidebar",
                                                             shiny::radioButtons("environmentdatasource", "Select Dataset to Use:",
                                                                                 choices = c("Original"),
                                                                                 selected = "Original", inline = TRUE),
                                                             uiOutput("environmentplotUI")),
                                                mainPanel(id = "environmentplotmainpanel",
                                                          uiOutput("environmentcorrelationoutput"),
                                                          uiOutput("environmentplotmainUI"),
                                                          #verbatimTextOutput("environmentbrushtest"),
                                                          #splitLayout(dataTableOutput("environmenttable1")),
                                                          uiOutput("environmentdynamicselectbuttons"),
                                                          #splitLayout(dataTableOutput("environmenttesttable")
                                                                      )))),
                                   tabPanel("Statistics",
                                            fluidPage(
                                              titlePanel("Menu"),
                                              sidebarLayout(
                                                sidebarPanel(id = "environmentstatssidebar",
                                                             shiny::radioButtons("environmentdatasource1", "Select Dataset to Use:",
                                                                                 choices = c("Original"),
                                                                                 selected = "Original", inline = TRUE),
                                                             conditionalPanel("input.environmentstats == 2",
                                                                              uiOutput("environmentparametricUI")),
                                                             conditionalPanel("input.environmentstats == 3",
                                                                              uiOutput("environmentnonparametricUI"))
                                                ),
                                                mainPanel("",
                                                          tabsetPanel(id ="environmentstats",
                                                                      tabPanel(value = 1, "Guide",
                                                                               includeMarkdown("Guides/stats_guide.Rmd")),
                                                                      tabPanel(value = 2, "Parametric",
                                                                               uiOutput("environmentparametricMain")),
                                                                      tabPanel(value = 3, "Non-Parametric",
                                                                               uiOutput("environmentnonparametricMain"))), id = "environmentstats1")
                                              )
                                            ))
                                 ))





