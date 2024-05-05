#' mdpop
#'
#' This package allows you to work with mdpop and has the list of functions:
#' cctoname, eapr, eapr1, fread_all, fsaveVarCntry, funlinkcctext, loadx, cntocc, popinterpkc, popkc, propkc, pyramid, pyredu
#' @param cctoname xxx
#' @param eapr eapr calc for give value (prop)
#' @param eapr1 eapr calc for give value (prop) within data.table
#' @param fread_all xxx
#' @param fsaveVarCntry xxx
#' @param funlinkcctext xxx
#' @param funlines xxx
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
#' mdpop(1)

mdpop <- function(logit){
  y = exp(logit)/(1 + exp(logit))
  return("testing")
}
