#' Allows you to add new rows of data to a column. Will automatically add NA values to rows in other columns.
#'
#' @param data Existing data table
#' @param selections vector of new elements to be added to column
#' @export

rbindPad <- function(data, selections){
  args <- base::list(data,selections)
  n <- base::sapply(args, ncol)
  mx <- base::max(n)
  if(base::ncol(selections) == base::ncol(data)){
    base::colnames(selections) <- base::colnames(data)
    newtable <- base::rbind(data, selections)
  }else if(base::ncol(selections) != base::ncol(data)){
    emptymatrix <- base::matrix(NA, base::nrow(selections), mx - base::ncol(selections))
    emptymatrix <- base::cbind(emptymatrix, selections)
    base::colnames(emptymatrix) <- base::colnames(data)
    newtable <- base::unique(base::rbind(data, emptymatrix))
  }
  return(newtable)
}
