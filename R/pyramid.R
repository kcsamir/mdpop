#' Population Pyramid in MSDem ??
#'
#' This function generates a pyramid for a cntry x Time
#' @param res1 with four colums age, sex (male/female), edu (optional, will be aggregated), value (agest will be replaced by age; ivar will be replaced by value)
#' @param iTime year, this is needed for caption
#' @param ireg region, this is needed for caption
#' @param iiscen specific scenario, empirical (then no scenario is needed)
#' @param ivar variable to be plotted (default pop)
#' @param iscale unit of value (default 1 = '000, 1000 = millions, 1000000 - billions)
#' @param facet.scale facet scale (default fixed)
#' @param ititle.text full title text (default NULL)
#' @caption Full list else year area scen will be pasted
#' @nmlegend  legends for 'edu' else variable's unique values will be used
#' @return pyramid
#' @keywords pyramid
#' @export
#' @examples

pyr_ag <- function(res1,
                   ireg,
                   ivar="pop",
                   iTime,
                   iiscen = NULL,
                   iscale=1,
                   facet.scale="fixed",
                   ititle.text = NULL,
                   icol="xxx"){
  res1 <<- res1
  ireg <<- ireg
  iTime <<- iTime
  iiscen <<- iiscen
  ivar <<- ivar
  icol <<- icol
  iscale <<- iscale
  facet.scale<<-facet.scale
  ititle.text <<- ititle.text
  # stop()

  iscale.nm = ifelse(iscale == 1000,"Millions",ifelse(iscale == 1000000, "Billions","Thousands"))

  if(length(ireg)==1){
    if(ireg == "World"){ #this is for mcbm - can be removed
     if(length(iiscen) >1){#here the columns = 12. if it is a real list, it will be less than that
        df1 = res1[,region:="World"][Time%in%iTime][,by=.(Time,sex,agest,scen),.(pop=sum(pop,na.rm=T))
        ][,setnames(.SD,c("agest",ivar),c("age","value"))][age<=100&age>=0]
      } else {
        df1 = res1[,region:="World"][Time%in%iTime][,by=.(Time,sex,agest),.(pop=sum(pop,na.rm=T))
        ][,setnames(.SD,c("agest",ivar),c("age","value"))][age<=100&age>=0][,scen:=iiscen]
      }

    } else {
      df1 = copy(res1)[region%in%ireg & Time%in%iTime & scen%in%iiscen
      ][, setnames(.SD, c(ivar,"agest"),c("value","age"))][age<=100&age>=0]
    }
  } else { #already selected here scen has cnt name so be careful
    df1 = copy(res1)[Time%in%iTime&scen%in%iiscen
    ][, setnames(.SD, c(ivar,"agest"),c("value","age"))][age<=100&age>=0]

  }
  if(ivar!=ivar) df1 <- df1%>%mutate(Time = paste(Time,Time+5,sep="-"))

  iage <- unique(df1$age)

  sex.names = unique(df1$sex)
  female_nm = grep("f", sex.names, value = T)
  male_nm = grep("^m", sex.names, value = T)

  #icnt should be supplied from outside OR
  if(length(iTime)==1 && iiscen ==  "baseyear") ititle = ireg else  ititle = paste(ireg, iiscen)

  #adding alternative text
  if(!is.null(ititle.text)){
    #total population
    iscale.nm.tot = ifelse(iscale == 1000,"Billions",ifelse(iscale == 1000000, "Trillions","Millions"))

    if(ititle.text == "size") ititle <- paste0(ititle, ": Population ",round(df1[,sum(value)]/1000/iscale,2)," ",
                                               iscale.nm.tot)
  }

  #mcbm specific
  if(length(iiscen)>1){
    if(length(ireg)>1) {
      ititle = paste("Population SSP2")
    } else {
      ititle = paste(icnt,"Population SSPs")
    }
  }

  nTime = unique(df1$Time)


  gg1<-df1%>%arrange(age) %>%
    ggplot(mapping = aes(x = age,
                         y = ifelse(sex == female_nm, value, -value)/iscale)) +
    geom_bar(stat = "identity", position = "stack")+
    coord_flip()+
    facet_grid(rows=vars(Time),cols=vars(scen),scales = facet.scale) +
    ggtitle(ititle) +
    labs(x = "Age group",
         y = paste("Population in",paste("'", iscale.nm, sep = ""),
                   "", sep = " ")) +
    scale_y_continuous(labels = abs)+
    geom_hline(yintercept = 0, color = "black") + theme_bw()

  ymax=max(ggplot_build(gg1)$layout$panel_params[[1]]$x$minor_breaks)

  if(length(nTime) >= 3) {iht = 3; iwd=8.5;textsize =4} else {iht = 5.5; iwd=8.5;textsize =4}
  gg1 <-gg1+geom_text(y = c(-ymax*.75), x = c(100), size = textsize, hjust = "inward",
                      parse = TRUE, check_overlap = TRUE,
                      label = c("M"))+
    geom_text(y = c(ymax*.75), x = c(100), size = textsize, hjust = "inward",
              parse = TRUE, check_overlap = TRUE,
              label = c("F")) +
    theme(panel.spacing.x = unit(1.5, "lines"))



  # addpop <- df1[,.(label=paste0(round(sum(value)/iscale,0),substr(iscale.nm,1,3))),.(Time)]
  # gg1+geom_text(data = addpop, aes(Inf, Inf, label = label),
  #                         col = "red",
  #                         hjust =10.5,
  #                         vjust = 1)
  #   geom_text(#data=addpop,
  #             y = c(-ymax*.9), x = c(100),label = addpop$value[1],
  #             size = textsize, hjust = "inward",
  #             parse = TRUE, check_overlap = TRUE)
  return(gg1)
}
