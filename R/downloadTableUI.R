#' One line command to create UI options for table download
#'
#' @param id input identifier
#' @export

downloadTableUI <- function(id) {
  ns <- shiny::NS(id)

  htmltools::tagList(
    shiny::textInput(ns("tablefilename"), "Create Name For File",
              value = "EcoPLOT")
    ,
    shiny::radioButtons(ns("tablefiletype"), "File type:",
                 choices = c("csv", "tsv"),
                 inline = TRUE)
    ,
    shiny::downloadButton(ns("downloadTable"), "Download File:")
  )
}
