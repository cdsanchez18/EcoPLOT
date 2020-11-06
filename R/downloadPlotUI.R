#' One line command to create UI options for plot download
#'
#' @param id input identifier
#' @export

downloadPlotUI <- function(id) {
  ns <- shiny::NS(id)

  htmltools::tagList(
    hr()
    ,
    shiny::textInput(ns("plotfilename"), "Create Name For File",
              value = "EcoPLOT")
    ,
    shiny::selectInput(ns("plotfiletype"), "File type:",
                 choices = c("png", "pdf", "jpeg", "tiff", "svg"),
                 selected = "png", width = "100%")
    ,
    shiny::splitLayout(shiny::numericInput(ns("plotwidth"), "Specify Width:",
                             value = 5.72),
                numericInput(ns("plotheight"), "Specify Height:",
                             value = 6.92))
    ,
    shiny::selectInput(ns("plotdimunits"), "Choose Unit:",
                choices = c("inches"= "in",
                            "centimeters" = "cm",
                            "millimeters" = "mm"),
                selected = "in")
    ,
    shiny::downloadButton(ns("downloadPlot"), "Download Plot:", width = "100%")
  )
}
