#' lines by scenarios (or any other variables) 
#'
#' This function allows you to generate line graphs
#' @param res1 data supplied with three columns scen Time value
#' @param var1 variable to plot
#' @param ititle title
#' @param iby .BY from datatable group by
#' @param iylab ylab
#' @param ixlab xlab
#' @param iregions regions.all this is file saved in the mdepop 
#' @return a line graph
#' @keywords line graph
#' @export
#' @examples



funlines <- function(res1,ivar,ititle,iby,iylab,ixlab,iregions = regions.all){ #,ivar,iage,isex,iregions,icnt,itob,iiscen,ipropgraph=F,iscale=1,ireg=ireg
  res1 <<- res1
  # iage <<- iage
  # isex <<- isex
  # iregions <<- iregions
  # icnt <<- icnt
  # itob <<- itob
  # ipropgraph <<- ipropgraph
  # iscale <<- iscale
  ivar <<- ivar
  ititle <<- ititle
  iby <<- iby
  iylab <<- iylab
  ixlab <<- ixlab
  iregions <<- iregions
  
  # stop("..")
  
  #convert ivar name to value
  inames <- names(res1)
  names(res1)[match(ivar,inames)] <- "value"
  #get country name
  iregion = iby$region
  icnt <- names(iregions)[match(iregion,iregions)]
  print(icnt)
  #create title
  iititle <- paste0(icnt,": ",ititle)
  
  #plot
  # scens.label = sort(unique(params$figdt$scen))
  ggline <-    res1%>%
    ggplot(aes(x=Time,y=value,group=scen,col=scen,shape=scen,linetype=scen))+
    geom_line()+geom_point()+
    ylab(iylab)+xlab(ixlab)+
    ggtitle(iititle)+
    theme(axis.text.x = element_text(angle = 35, vjust = 0.5, hjust=1))
  print(ggline)
  #control height and weight
  
  return(1)
  
}

