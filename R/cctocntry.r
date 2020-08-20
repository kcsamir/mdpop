#' UN cc to country name
#'
#' This function allows you to extract country name for give UN country code
#' @param cc UN country code
#' @param wpp which WPP to use
#' @return country name
#' @keywords read
#' @export
#' @examples
#' cctoname(900, "wpp2019")

cctoname <- function(cc,wpp = "wpp2019"){
   data(UNlocations,package = wpp)
   head(UNlocations)
   return(as.character(UNlocations$name[match(cc,UNlocations$country_code)]))
   }
