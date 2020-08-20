#' save file
#'
#' This function saves a given value to a file name
#' @param x value or df to be saved
#' @param ivar id (without region)
#' @return df with 1
#' @keywords save file
#' @export
#' @examples
#' save.var.cntry.file("value or df", "id without region")
save.var.cntry.file <- function(x,ivar) {
  # x<<-x
  save(x,file=paste("../../data/wic_country/",ivar,x$region[1],".Rdata",sep = ""))
  return(x[1,1])
}
