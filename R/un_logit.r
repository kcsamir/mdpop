#' unlogit
#'
#' This function allows you to unlogit (back transform)
#' @param logit numeric value
#' @return y unlogit value
#' @keywords logit
#' @export
#' @examples
#' un_logit(1)

un_logit <- function(logit){
  y = exp(logit)/(1 + exp(logit))
  return(y)
}
