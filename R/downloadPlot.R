#' One line command to create server options for plot download
#'
#' @param id input identifier
#' @param plotid name of the plot to be saved
#' @export

downloadPlot <- function(id, plotid) {
  shiny::moduleServer(
    id,
    function(input,output,session) {
      output$downloadPlot <- shiny::downloadHandler(
        filename = function() {
          paste(input$plotfilename, input$plotfiletype, sep = ".")
        },
        content = function(file) {
          ggplot2::ggsave(file, #plot = plotid, 
                          device = input$plotfiletype,
                          width = input$plotwidth, height = input$plotheight,
                          units = input$plotdimunits)
        }
      )
    }
  )
}
