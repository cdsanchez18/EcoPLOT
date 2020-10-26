#' data_sunmary
#' One line command to create summary of mean and SD
#'
#' @param data input identifier
#' @param varname y axis variable
#' @param groupnames vector of grouping variables to include
#' @export


data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}