#' Add new column to a dataframe. If lengths differ, NA values will be used to fill.
#'
#' @export

cbindPad <- function(...){
  args <- base::list(...)
  n <- base::sapply(args,nrow)
  mx <- base::max(n)
  pad <- function(x, mx){
    if (base::nrow(x) < mx){
      nms <- base::colnames(x)
      padTemp <- base::matrix(NA, mx - base::nrow(x), base::ncol(x))
      base::colnames(padTemp) <- nms
      if (base::ncol(x)==0) {
        return(padTemp)
      } else {
        return(base::rbind(x,padTemp))
      }
    }
    else{
      return(x)
    }
  }
  rs <- base::lapply(args,pad,mx)
  return(do.call(base::cbind,rs))
}
