#' Convert given edu distribution into eapr within data.table
#'
#' This function returns EAPR for given education distribution
#' @param prop distribution data 'edu' (sorted) 'value' for single age-group
#' @return eapr
#' @keywords read
#' @export
#' @examples

eapr1 <- function (prop) {
  prop <<- prop

  x <- rev(cumsum(rev(prop$value)))
  n <- length(x)

  eapr.x <- prop[-nrow(prop), ]

  eapr.x$value <- x[2:n]/x[1:(n - 1)]
  eapr.x[,edut:=paste0(edu,as.numeric(substr(edu,2,2))+1)]
  return(eapr.x)
}
