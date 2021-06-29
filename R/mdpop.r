#' mdpop
#'
#' This package allows you to work with mdpop and has the list of functions:
#' cctoname, eapr, fread_all, fsaveVarCntry, funlinkcctext, loadx, cntocc, popinterpkc, popkc, propkc, pyramid, pyredu
#' @param cctoname
#' @param eapr
#' @param fread_all
#' @param fsaveVarCntry
#' @param funlinkcctext
#' @param loadx.r
#' @param cntocc.r
#' @param popinterpkc
#' @param popkc
#' @param propkc
#' @param pyramid
#' @param pyredu
#' @param un_logit
#' @return y unlogit value
#' @keywords logit
#' @export
#' @examples
#' un_logit(1)

mdpop <- function(logit){
  y = exp(logit)/(1 + exp(logit))
  return(y)
}
