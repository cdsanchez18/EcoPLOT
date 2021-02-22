





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
                                       uiOutput("iRFdatasets")
                              ),
                              tabPanel(value = 4, "Step 3: Run IRF",
                                       verbatimTextOutput("testtest"),
                                       verbatimTextOutput("testtest1"),
                                       verbatimTextOutput("testtest2"),
                                       verbatimTextOutput("IRFoutput")
                                       )
                              ,
                              tabPanel(value = 5, "Step 4: View Results"
                            )
                            )
                          )
                        )
))




