library(shiny)
source("global.R", local = TRUE)
source('EcoPLOTUI.R', local = TRUE)
source('EcoPLOTServer.R', local = TRUE)


shinyApp(
  ui = EcoPLOTUI,
  server = EcoPLOTServer
)
