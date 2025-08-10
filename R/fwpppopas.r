#' Population past only for the moment by age and sex from WPP
#'
#' This function extracts population from WPP and prepare it to match the msdem
#' @param iwpp "wpp2022" as default
#' @return popas region, Time, sex, agest, pop
#' @keywords UN population
#' @export
#' @examples
#' @importFrom data.table
fwpppopA5G <- function(iwpp = "wpp2022"){
  data("popAge5dt",package = iwpp)
  popas <- popAge5dt[,.(country_code,year,age,popM,popF)][
    ,setnames(.SD,c("country_code","age"),c("region","agest"))][
      ,agest:=(tstrsplit(x = agest,split = "[+-]",keep=1,fixed=F))][
        ,agest:=as.numeric(agest)]
  popas <- melt(data = popas, measure.vars = c("popM","popF"), variable.name = "sex", value.name = "pop")
  popas[,sex:=tolower(gsub("pop","",sex))]
  popas <- popas[,setnames(.SD,c("year"),c("Time"))][,region:=paste0("reg",region)]
  popas[pop==0,pop:=0.0001]
  return(popas)
}
