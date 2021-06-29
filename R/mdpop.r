#' mdpop
#'
#' This package allows you to work with mdpop and has the list of functions:
#' cctoname, eapr, fread_all, fsaveVarCntry, funlinkcctext, loadx, cntocc, popinterpkc, popkc, propkc, pyramid, pyredu
#' @param cctoname xxx
#' @param eapr xxx
#' @param fread_all xxx
#' @param fsaveVarCntry xxx
#' @param funlinkcctext xxx
#' @param loadx  xxx
#' @param cntocc  xxx
#' @param popinterpkc  xxx
#' @param popkc  xxx
#' @param propkc  xxx
#' @param pyramid  xxx
#' @param pyredu  xxx
#' @param un_logit  xxx
#' @return y unlogit value
#' @keywords logit
#' @export
#' @examples
#' un_logit(1)

mdpop <- function(logit){
  y = exp(logit)/(1 + exp(logit))
  return(y)
}
