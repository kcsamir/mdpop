#' Population Pyramid in MSDem
#'
#' This function generates a pyramid for a cntry x Time
#' @param df with four colums age, sex (male/female), edu, value
#' @param year year
#' @param area region
#' @param scen specific scenario
#' @popunit unit of value (default '000)
#' @caption Full list else year area scen will be pasted
#' @nmlegend  legends for 'edu' else variable's unique values will be used
#' @return pyramid
#' @keywords pyramid
#' @export
#' @examples
#' pyredu(df=df,year=2015,area="ARM",scen="Baseyear",nmlegend="Education")



pyredu <- function(df,year,area,scen,popunit = "Thousands",caption=NULL,nmlegend,ipal=1) {
  # df <<- df
  # stop("")
  if(is.null(caption)) caption = paste(area,scen,year,sep="-")

  gg1 <-
    df%>%
    ggplot() +
    geom_bar(aes(x=age,y=value,group=edu,fill=edu),
             stat = "identity",subset(df,df$sex=="female"))+
    geom_bar(aes(x=age,y=-value,group=edu,fill=edu),
             stat = "identity",subset(df,df$sex=="male"))+
    coord_flip()+
    ggtitle(caption)+# labs(fill = nmlegend)#https://www.datanovia.com/en/blog/ggplot-legend-title-position-and-labels/
    #https://aosmith.rbind.io/2018/01/19/reversing-the-order-of-a-ggplot2-legend/
    scale_fill_brewer(name=nmlegend,type="seq",palette=ipal,guide = guide_legend(reverse = TRUE))+
    xlab("Age") +# scale_fill_hue(l=90,c=90) +
    ylab(paste("Males     Population in ",popunit,"    Females",sep=""))+
    geom_hline(yintercept = 0,color="black")+
    theme_bw()#+theme(legend.position = "none")#+#what does this mean

  #fixing the limits of pop
  breaks <- ggplot_build(gg1)$layout$panel_params[[1]]$x$breaks
  breaks <- breaks[!is.na(breaks)]

  absbreaks = abs(breaks)
  lim.max = max(absbreaks)


  lim.main = max(abs(ggplot_build(gg1)$layout$panel_params[[1]]$x.range))
  lim.main  = ceiling(lim.main/max(unique(diff(breaks))))*max(unique(diff(breaks)))#had to add max as it was giving same value many times


  if(length(absbreaks)%%2==0) {
    if(lim.max==absbreaks[1]){
      breaks = c(breaks,lim.max)
      absbreaks = abs(breaks)
    }else{
      breaks = c(-lim.max,breaks)
      absbreaks = abs(breaks)
    }
  }

  limits = c(-lim.main,lim.main)


  # if(length(absbreaks)%%2==0) {if(grep(lim.max,absbreaks)==1){
  #    breaks = c(breaks,lim.max)
  #    absbreaks = abs(breaks)
  #   }else{
  #     breaks = c(-lim.max,breaks)
  #     absbreaks = abs(breaks)
  #   }
  # }

  # limits = c(-lim.max,lim.max)
  print(gg1+scale_y_continuous(limits=limits,breaks=breaks,labels = absbreaks))

}
