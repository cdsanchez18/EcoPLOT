#' One line command to create server options for table download
#'
#' @param id input identifier
#' @param plotid name of the table to be saved
#' @export

downloadTable <- function(id, tableid) {
  shiny::moduleServer(
    id,
    function(input,output,session){
      output$downloadTable <- shiny::downloadHandler(
        filename = function() {
          paste(input$tablefilename, input$tablefiletype, sep = ".")
        },
        content = function(file) {
          sep <- switch(input$tablefiletype, "csv" = ",", "tsv" = "\t")
          utils::write.table(x = tableid, file, sep = sep,
                      row.names = TRUE)
        }
      )
    }
  )
}
