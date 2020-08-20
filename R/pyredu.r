#' Population Pyramid in MSDem
#'
#' This function generates a pyramid for a cntry x Time
#' @param df with four colums age, sex (male/female), edu, value
#' @param year year
#' @param area region
#' @param scen specific scenario
#' @param edu.nmlegend name of the education names from lower to higher
#' @param popunit unit of value (default '000)
#' @param caption Full list else year area scen will be pasted
#' @param nmlegend  legends for 'edu' else variable's unique values will be used
#' @param edu.nmlegend names of education categories
#' @return pyramid
#' @keywords pyramid
#' @export
#' @examples
#' pyredu(df=df,year=2015,area="ARM",scen="Baseyear",nmlegend="Education")



pyredu<- function(df1,Time,area,scen,popunit = "Thousands",
                        caption=NULL,nmlegend,edu.nmlegend,ipal=1) {

  # df1 <<- df1
  # Time<<-Time
  # area<<-area
  # scen<<-scen
  # popunit <<- popunit
  # caption <<- caption
  # nmlegend<<-nmlegend
  # edu.nmlegend <<- edu.nmlegend
  # ipal<<-ipal
  # stop("")

  if(is.null(caption)) caption = paste(area,scen,Time,sep="-")

  sex.names =  unique(df1$sex)
  female_nm = grep("f",sex.names,value = T)
  male_nm =   grep("^m",sex.names,value = T)

  edu.nm <- unique(df1$edu)

  gg1 <-
    df1%>%arrange(age)%>%
    ggplot(mapping = aes(x=age,
                         y=ifelse(sex==female_nm,value,-value),
                         fill=edu)) +
    geom_bar(stat = "identity",position= "stack")+
    coord_flip()+
    ggtitle(caption)+# labs(fill = nmlegend)#https://www.datanovia.com/en/blog/ggplot-legend-title-position-and-labels/
    #https://aosmith.rbind.io/2018/01/19/reversing-the-order-of-a-ggplot2-legend/
    labs(x = "Age group",
         y = paste("Males       Population in",popunit,"      Females",sep=" "),
         fill = "Educational attainment") +
    # scale_y_continuous(labels = abs) +
    scale_fill_brewer(name=nmlegend,breaks=edu.nm,
                      labels=edu.nmlegend,type="seq",
                      palette=ipal,guide = guide_legend(reverse = TRUE))+
    geom_hline(yintercept = 0,color="black")+
    theme_bw()


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
