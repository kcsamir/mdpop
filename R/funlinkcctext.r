#' delete specific file(s)
#'
#' This function finds and deletes file(s)
#' @param cc region
#' @param ipatt pattern to find withing the folder
#' @return
#' @keywords read file
#' @export
#' @examples
#' load.x("filename")


f.unlink.cc.text <- function(cc,ipatt){
  # i = 642
  cc.dir.val = paste("../../data/wic_country/val",cc,sep = "")
  delete_files =c(grep(pattern = ipatt,x = dir(cc.dir.val,full.names = T),value = T))
  unlink(delete_files)
}
