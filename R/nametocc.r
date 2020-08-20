#' UN country name to country code
#'
#' This function allows you to extract country code for give UN country name
#' @param cn UN country code
#' @param wpp which WPP to use
#' @return country code
#' @keywords read
#' @export
#' @examples
#' cntocc("World", "wpp2019")

cntocc <- function(cn,wpp = "wpp2019"){
   data(UNlocations,package = wpp)
   return(as.character(UNlocations$country_code[match(cn,UNlocations$name)]))
   }
