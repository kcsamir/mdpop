#' Population Pyramid in MSDem ??
#'
#' This function generates a pyramid for a cntry x Time
#' @param df1 with five colums age, sex (male/female), edu, varfacet,value
#' @param year year
#' @param area region
#' @param scen specific scenario
#' @param varfacet variable to be faceted
#' @popunit unit of value (default '000)
#' @caption Full list else year area scen will be pasted
#' @nmlegend  legends for 'edu' else variable's unique values will be used
#' @return pyramid
#' @keywords pyramid
#' @export
#' @examples
#' pyredufacet(df1=df1,year=2015,area="ARM",scen="Baseyear",nmlegend="Education")


pyredufacet <- function(df1,year,
                        area,
                        scen,
                        popunit = "Thousands",
                        caption=NULL,nmlegend,ipal=1,
                        ixlab="Age",
                        iylab="Population",
                        female_nm="female") {
  # df1 <<- df1
  # year
  # area
  # scen
  # popunit = "Thousands"
  # caption=NULL
  # nmlegend
  # ipal=1
  # ixlab="Age"
  # iylab="Population"
  # female_nm=female_nm

  # stop("")
  if(is.null(caption)) caption = paste(area,scen,year,sep="-")

  gg1 <-df1%>%arrange(age) %>%
    ggplot(mapping = aes(x = age,
                         y = ifelse(sex == female_nm, value, -value),
                         fill = edu)) +
    geom_bar(stat = "identity", position = "stack")+
    coord_flip()+facet_wrap(~varfacet)+
    ggtitle(caption) +
    labs(x = ixlab,
         y = iylab,
         fill = "Educational attainment")  +
    scale_fill_manual(name=legend.title,
                      breaks=edu.nm,
                      labels=legend.labels,
                      values=ipal,
                      guide = guide_legend(reverse = TRUE))+
    # scale_fill_brewer(name=nmlegend,type="seq",palette=ipal,guide = guide_legend(reverse = TRUE))+
    scale_y_continuous(labels = abs)+
    geom_hline(yintercept = 0, color = "black") + theme_bw()

  # #fixing the limits of pop
  # breaks <- ggplot_build(gg1)$layout$panel_params[[1]]$x$breaks
  # breaks <- breaks[!is.na(breaks)]
  #
  # absbreaks = abs(breaks)
  # lim.max = max(absbreaks)
  #
  #
  # lim.main = max(abs(ggplot_build(gg1)$layout$panel_params[[1]]$x.range))
  # lim.main  = ceiling(lim.main/max(unique(diff(breaks))))*max(unique(diff(breaks)))#had to add max as it was giving same value many times
  #
  #
  # if(length(absbreaks)%%2==0) {
  #   if(lim.max==absbreaks[1]){
  #     breaks = c(breaks,lim.max)
  #     absbreaks = abs(breaks)
  #   }else{
  #     breaks = c(-lim.max,breaks)
  #     absbreaks = abs(breaks)
  #   }
  # }
  #
  # limits = c(-lim.main,lim.main)
  #
  #
  # # if(length(absbreaks)%%2==0) {if(grep(lim.max,absbreaks)==1){
  # #    breaks = c(breaks,lim.max)
  # #    absbreaks = abs(breaks)
  # #   }else{
  # #     breaks = c(-lim.max,breaks)
  # #     absbreaks = abs(breaks)
  # #   }
  # # }
  #
  # # limits = c(-lim.max,lim.max)
  # print(gg1+scale_y_continuous(limits=limits,breaks=breaks,labels = absbreaks))

  print(gg1)
}
