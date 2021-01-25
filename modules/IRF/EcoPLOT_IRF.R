





EcoPLOT_IRF <- tabPanel("IRF",
                        fluidPage(
                          titlePanel("Menu"),
                          sidebarLayout(
                            sidebarPanel("",
                                         tags$div(
                                           tags$h5("It is recommended to use a filtered dataset when performing IRF."),
                                           align = "center"),
                                           shiny::radioButtons("irfDatasetselect", "Select Dataset to Use:",
                                                             choices = c("Original"),
                                                             selected = "Original"),
                                         uiOutput("irfUIoptions")),
                            mainPanel(
                              tabsetPanel(id="IRF", 
                              tabPanel(value = 1, "Formatting",
                              splitLayout(
                              dataTableOutput("IRFdatasetoutput")),
                              verbatimTextOutput("testoutput4")
                              ),
                              tabPanel(value = 2, "Tables",
                              splitLayout(
                              dataTableOutput("testoutput"),
                              dataTableOutput("testoutput1")),
                              splitLayout(
                                dataTableOutput("testoutput2"),
                                dataTableOutput("testoutput3"))
                              ),
                              tabPanel(value = 3, "Encoded Tables",
                                       splitLayout(
                                         dataTableOutput("Xtrainencodedoutput"),
                                         dataTableOutput("Xtestencodedoutput")
                                       )
                                       ),
                              tabPanel(value = 4, "Analysis",
                                       verbatimTextOutput("IRFoutput")
                                       )
                            )
                            )
                          )
                        )
)