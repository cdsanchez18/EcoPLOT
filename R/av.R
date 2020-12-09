#' av
#' Allows you to set "NULL" as an option in inputs. Shiny will recognize it as NULL
#'
#' @param x input option
#' @export

# Define generic function to access/clean variables
# This especially converts "NULL" to NULL
av <- function(x){
  if( isTRUE(all.equal(x, "")) | isTRUE(all.equal(x, "NULL")) ){
    return(NULL)
  }
  return(x)
}
