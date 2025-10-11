#' Population size in MSDem
#'
#' This function allows you to calculate population size by sex or by regions using the MSDem output
#' @param res1 data region Time agest sex edu pop emi....
#' @param ivar default "pop"
#' @param iTime choose Time one or more
#' @param iiscen scenario name
#' @param iscale 1 (thousand) 1000 (millions)
#' @param ireg specific region(s)
#' @param facet.scale default "fixed"
#' @param ititle.text provide the text else internally provided
#' @param icol col or grey for black and white and nick for specific ones
#' @param ilegend.lables NULL, else provide the name of the edu categories
#' @param ipal provide the color else default for 6 is available
#' @return ggplot pop pyramid by edu
#' @keywords plot pyramid
#' @export
#' @examples



pyrage <- function(res1,ivar="pop",iTime,
                   iiscen,
                   iscale=1,
                   ireg,
                   facet.scale="fixed",
                   ititle.text = NULL,
                   icol="xxx",
                   ilegend.labels = NULL,
                   ipal=NULL){
  # res1 <<- res1
  # iTime <<- iTime
  # ireg <<- ireg
  # iiscen <<- iiscen
  # ivar <<- ivar
  # icol <<- icol
  # iscale <<- iscale
  # facet.scale<<-facet.scale
  # ititle.text <<- ititle.text
  # ipal <<- ipal


  iscale.nm = ifelse(iscale == 1000,"Millions",ifelse(iscale == 1000000, "Billions","Thousands"))

  df1 = copy(res1)[Time%in%iTime&scen%in%iiscen][, setnames(.SD, c("pop","agest"),c("value","age"))][age<=100&age>=0]


  if(ivar!="pop") df1 <- df1%>%mutate(Time = paste(Time,Time+5,sep="-"))

  iage <- unique(df1$age)

  if(any(iage<15) ){

    if( is.null(ilegend.labels)){# 6 categories WIC
      legend.labels = c("Under 15yr","No Education","Some Primary","Primary","Lower Secondary","Upper Secondary","Post Secondary")
    } else{
      legend.labels = ilegend.labels
    }

    df1<-df1[age<15,edu:="e0"][,by=.(Time,sex,edu,age,scen),.(value=sum(value))]


  } else {
    # legend.labels =c("NoEdu",'Some Prim','Prim','Low Sec','Upp Sec','Post Sec')
    if( is.null(ilegend.labels)){# 6 categories WIC
      legend.labels = c("No Education","Some Primary","Primary","Lower Secondary","Upper Secondary","Post Secondary")
    } else{
      legend.labels = ilegend.labels
    }

  }


  #color
  if(is.null(ipal)){
    if(any(iage<15) ){

      if( icol == "grey" ){
        ipal =  rep(icol,length(legend.labels))
      } else if(icol == "nick"){
        ipal =  c("grey","darkred","red4","red3","tomato3","orange1","dodgerblue3")#,"darkslateblue")
      } else {
        ipal =  c("grey","darkred","tomato3","orange1",  "tan2","steelblue1","dodgerblue3")#,"darkslateblue")
      }


    } else {

      ipal =  c("darkred","tomato3","orange1",  "tan2","steelblue1","dodgerblue3")#,"darkslateblue")
    }
  }



  edu.nm = unique(df1$edu)
  sex.names = unique(df1$sex)
  female_nm = grep("f", sex.names, value = T)
  male_nm = grep("^m", sex.names, value = T)


  if(length(iTime)==1 && iTime == 2020) ititle = icnt else  ititle = paste(icnt, iiscen)

  #adding alternative text
  if(!is.null(ititle.text)){
    #total population
    iscale.nm.tot = ifelse(iscale == 1000,"Billions",ifelse(iscale == 1000000, "Trillions","Millions"))

    if(ititle.text == "size") ititle <- paste0(ititle, ": Population ",round(df1[,sum(value)]/1000/iscale,2)," ",
                                               iscale.nm.tot)
  }

  if(length(iiscen)>1){
    if(length(ireg)>1) {
      ititle = paste("Population SSP2")
    } else {
      ititle = paste(icnt,"Population SSPs")
    }
  }

  nTime = unique(df1$Time)

  legend.title = "Education"


  gg1<-df1%>%arrange(age) %>%
    ggplot(mapping = aes(x = age,
                         y = ifelse(sex == female_nm, value, -value)/iscale,
                         fill = edu)) +
    geom_bar(stat = "identity", position = "stack")+
    coord_flip()+
    facet_grid(rows=vars(Time),cols=vars(scen),scales = facet.scale) +
    ggtitle(ititle) +
    labs(x = "Age group",
         y = paste("Population in",paste("'", iscale.nm, sep = ""),
                   "", sep = " "), fill = "Educational attainment") +
    scale_fill_manual(name=legend.title,
                      breaks=edu.nm,
                      labels=legend.labels,
                      values=ipal,
                      guide = guide_legend(reverse = TRUE))+
    # scale_fill_brewer(name = legend.title, breaks = edu.nm, labels = legend.labels,
    #                   type = "seq", palette = ipal, guide = guide_legend(reverse = TRUE)) +
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
