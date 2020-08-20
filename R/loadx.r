#' load a file named x
#'
#' This function load a given file name
#' @param ifile
#' @return file as x
#' @keywords read file
#' @export
#' @examples
#' load.x("filename")
loadx <- function(ifile){
  load(ifile$value)
  if(is.data.frame(x)) {
        return(x)}
  else {
    return(data.frame(value=x))
  }
}
