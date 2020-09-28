#' One line command to create UI options for plot download
#'
#' @param id input identifier
#' @export

downloadPlotUI <- function(id) {
  ns <- shiny::NS(id)

  htmltools::tagList(
    shiny::textInput(ns("plotfilename"), "Create Name For File",
              value = "EcoPLOT")
    ,
    shiny::radioButtons(ns("plotfiletype"), "File type:",
                 choices = c("png", "pdf", "jpeg", "tiff", "svg"))
    ,
    shiny::downloadButton(ns("downloadPlot"), "Download Plot:")
  )
}
