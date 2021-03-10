





EcoPLOT_IRF <- tabPanel("IRF",
                        fluidPage(
                          titlePanel("Menu"),
                          sidebarLayout(
                            sidebarPanel(id = "IRFsidebar",
                                         conditionalPanel("input.IRF ==2",
                                         tags$div(
                                           tags$h5("It is recommended to use a filtered dataset when performing IRF."),
                                           align = "center"),
                                           shiny::radioButtons("irfDatasetselect", "Select Dataset to Use:",
                                                             choices = c("Original"),
                                                             selected = "Original")),
                                         uiOutput("irfUIoptions")),
                            mainPanel(id = "IRF1",
                              tabsetPanel(id="IRF", 
                                          tabPanel(value = 1, "Guide",
                                                   includeMarkdown("iRF_guide.RMD")),
                              tabPanel(value = 2, "Step 1: Format Data",
                              splitLayout(
                                uiOutput("IRFdatasetoutputUI"))
                              #DT::DTOutput("IRFdatasetoutput"))#,
                              #verbatimTextOutput("testoutput4")
                              ),
                              tabPanel(value = 3, "Step 2: Create Test/Train Datasets",
                                       uiOutput("testtest1")
                              ),
                              tabPanel(value = 4, "Step 3: Run IRF",
                                       #verbatimTextOutput("testtest"),
                                       #verbatimTextOutput("testtest2"),
                                       uiOutput("IRFoutputui")
                                       )
                              ,
                              tabPanel(value = 5, "View Plots",
                                       uiOutput("IRFplotui")
                            ),
                            tabPanel(value = 6, "View Tables")
                            )
                          )
                        )
),
conditionalPanel("input.IRF == 3",
                 uiOutput("iRFdatasets")),
conditionalPanel("input.IRF == 5",
                 hr(),
                 uiOutput("irfinteractionoutput2"),
                 uiOutput("partdepplot1")),
conditionalPanel("input.IRF == 6",
                 uiOutput("irftableui"))
)




