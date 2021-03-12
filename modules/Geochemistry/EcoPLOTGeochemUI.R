



EcoPLOTGeochemUI <- tabPanel("Environmental Data",
                                 tabsetPanel(
                                   tabPanel("Guide",
                                            includeMarkdown("environment_guide.rmd")),
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
                                                             uiOutput("environmentfileupload")),
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
                                              fluidRow(
                                                column(width = 12,
                                                       splitLayout(dataTableOutput("environmenttable")))),
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
                                                       splitLayout(tableOutput("ssc"))))
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
                                                                               includeMarkdown("stats_guide.Rmd")),
                                                                      tabPanel(value = 2, "Parametric",
                                                                               uiOutput("environmentparametricMain")),
                                                                      tabPanel(value = 3, "Non-Parametric",
                                                                               uiOutput("environmentnonparametricMain"))), id = "environmentstats1")
                                              )
                                            ))
                                 ))





