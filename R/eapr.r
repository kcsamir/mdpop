#' Convert given edu distribution into eapr
#'
#' This function returns EAPR for given education distribution
#' @param prop distribution data 'edu' (sorted) 'value' as proportion for single age-group
#' @return eapr datatable
#' @keywords read
#' @export
#' @examples

eapr <- function(prop,edutcol=F) {
   prop <<- prop
   # stop("Hello")
  # print("Hello")
  x <- rev(cumsum(rev(prop$value)))
  n <- length(x)
  eapr.x <- prop[-nrow(prop), ]
  eapr.x$value <- x[2:n]/x[1:(n-1)]
  if(edutcol) eapr.x[,edut:=paste0(edu,as.numeric(substr(edu,2,2))+1)]
  return(eapr.x)
}
