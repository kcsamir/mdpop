#' Population size in MSDem
#'
#' This function allows you to calculate population size by sex or by regions using the MSDem output
#' @param res1 data
#' @param agel lower limit - default is 0
#' @param ageu upper limit - default it 'max'
#' @param period specific periods - default is NULL
#' @param sex specific sex - default is NULL
#' @param bysex group by sex - default is FALSE
#' @param reg specific region(s)
#' @param byreg group by regions - default is FALSE
#' @param resi specific residence(s)
#' @param byresi group by residences - default is FALSE
#' @return R list with five data.table 1) var_def 2) state_space 3) mig_dom 4) mig_int 5) results
#' @keywords read
#' @export
#' @examples



funpyrwrapper <- function(res1,ivar="pop",iTime,
                          iiscen,iscale=1,
                          facet.scale="fixed",
                          ititle.text = NULL,
                          icol="xxx"){
  res1 <<- res1
  iTime <<- iTime
  iiscen <<- iiscen
  ivar <<- ivar
  icol <<- icol
  iscale <<- iscale
  facet.scale<<-facet.scale
  ititle.text <<- ititle.text


  iscale.nm = ifelse(iscale == 1000,"Millions",ifelse(iscale == 1000000, "Billions","Thousands"))

  if(length(ireg)==1){
    if(ireg == "World"){

      if(length(iiscen) >1){#here the columns = 12. if it is a real list, it will be less than that
        df1 = res1[,region:="World"][Time%in%iTime][,by=.(Time,sex,edu,agest,scen),.(pop=sum(pop,na.rm=T))
        ][,setnames(.SD,c("agest","pop"),c("age","value"))][age<=100&age>=0]

      } else {
        df1 = res1[,region:="World"][Time%in%iTime][,by=.(Time,sex,edu,agest),.(pop=sum(pop,na.rm=T))
        ][,setnames(.SD,c("agest","pop"),c("age","value"))][age<=100&age>=0][,scen:=iiscen]
      }

    } else {
      df1 = copy(res1)[region%in%ireg&Time%in%iTime&scen%in%iiscen
      ][, setnames(.SD, c("pop","agest"),c("value","age"))][age<=100&age>=0]
    }
  } else { #already selected here scen has cnt name so be careful
    df1 = copy(res1)[Time%in%iTime&scen%in%iiscen
    ][, setnames(.SD, c("pop","agest"),c("value","age"))][age<=100&age>=0]

  }
  if(ivar!="pop") df1 <- df1%>%mutate(Time = paste(Time,Time+5,sep="-"))

  iage <- unique(df1$age)

  if(ivar == "pop" & any(iage<15)){
    legend.labels = c("Under 15yr","No Education","Some Primary","Primary","Lower Secondary","Upper Secondary","Post Secondary")

    if( icol == "grey" ){
      ipal =  rep(icol,length(legend.labels))
    } else if(icol == "nick"){
      ipal =  c("grey","darkred","red4","red3","tomato3","orange1","dodgerblue3")#,"darkslateblue")
    }else {
      ipal =  c("grey","darkred","tomato3","orange1",  "tan2","steelblue1","dodgerblue3")#,"darkslateblue")
    }

    df1<-df1[age<15,edu:="e0"][,by=.(Time,sex,edu,age,scen),.(value=sum(value))]


  } else {
    # legend.labels =c("NoEdu",'Some Prim','Prim','Low Sec','Upp Sec','Post Sec')
    legend.labels = c("No Education","Some Primary","Primary","Lower Secondary","Upper Secondary","Post Secondary")
    ipal =  c("darkred","tomato3","orange1",  "tan2","steelblue1","dodgerblue3")#,"darkslateblue")
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
