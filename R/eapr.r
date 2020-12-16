#' Convert given edu distribution into eapr
#'
#' This function returns EAPR for given education distribution
#' @param prop distribution data 'edu' (sorted) 'value'
#' @return eapr
#' @keywords read
#' @export
#' @examples

eapr <- function(prop) {
  # prop <<- prop
  # stop()
  x <- rev(cumsum(rev(prop$value)))
  eapr.x <- prop[-nrow(prop),]
  eapr.x$value<- x[2:6]/x[1:5]
  return(eapr.x)
}