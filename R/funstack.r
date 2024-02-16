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

funstack <- function(res1,ivar,iage,isex,iregions,icnt,itob,iiscen,ipropgraph=F,iscale=1,ireg=ireg){
  res1 <<- res1
  iage <<- iage
  isex <<- isex
  iregions <<- iregions
  icnt <<- icnt
  itob <<- itob
  ipropgraph <<- ipropgraph
  iscale <<- iscale
  ivar <<- ivar


  ivars = c("pop","births","deaths","emi","imm")#add more and ensure that they are arranged same as for ivar_titles
  ivar_titles = c("Population","Births","Deaths","Emigration","Immigration")
  var_title = ifelse(ipropgraph, "Population share", ivar_titles[match(ivar,ivars)])

  #for the title
  age.rng <- ifelse(max(iage)>=100, paste(min(iage),"+",sep=""),age.rng <- paste(range(iage),collapse = "-"))
  #SD = subset of datatable
  # figdt[age %in%iage & sex%in%isex][,.(value = sum(rowSums(.SD),na.rm = T)), .SDcols = regions[iregions],by=.(Time,age,edu)]%>%spread(Time,value)

  #prepare file for figure
  if(itob<9999){
    df1 <- res1[,tob:=Time-agest-5][tob %in%itob & sex%in%isex & region%in%regions[iregions]][,.(value = sum(get(ivar),na.rm = T)),by=.(Time,edu)]
    ititle = paste(var_title,icnt,iiscen,ifelse(length(isex)>1,"",isex),"cohort born",paste(itob,itob+5,sep="-"))
  }else{

    if(F){if(length(res1) <12){#here the columns = 12. if it is a real list, it will be less than that
      df1 <- NULL
      for(list.nm in names(res1)) df1 <- rbind(df1,res1[[list.nm]][,scen:=list.nm])
    } else {
      df1 <- res1[,scen:=iscenX]
    }
    }
    df1 <- res1

    if(any(iage<15)) df1[agest<15,edu:="e0"]

    if(icnt == "World") df1 <- df1[agest %in%iage & sex%in%isex][,.(value = sum(get(ivar),na.rm = T)),by=.(Time,edu,scen)] else
      df1 <- df1[agest %in%iage & sex%in%isex & region%in%regions[iregions]][,.(value = sum(get(ivar),na.rm = T)),by=.(Time,edu,scen)]

    ititle = paste(var_title,icnt,ifelse(length(iiscen)>1,"SSPs",res1[,unique(scen)]),
                   ifelse(length(isex)>1,"",isex),
                   ifelse(diff(range(iage))==120,"",paste("aged",age.rng,"years")))
  }

  # df1[,scen:=iscen.nm[scen]]

  if(any(iage<15)){
    edus.plot = unique(df1$edu)
    legend.labels = c("Under 15yr","No Education","Some Primary","Primary","Lower Secondary","Upper Secondary","Post Secondary")
    edu.colors =  c("grey","darkred","tomato3","orange1",  "tan2","steelblue1","dodgerblue3")#,"darkslateblue")


  } else {

    # legend.labels =c("NoEdu",'Some Prim','Prim','Low Sec','Upp Sec','Post Sec')
    edus.plot = unique(res1$edu)
    legend.labels = c("No Education","Some Primary","Primary","Lower Secondary","Upper Secondary","Post Secondary")
    edu.colors =  c("darkred","tomato3","orange1",  "tan2","steelblue1","dodgerblue3")#,"darkslateblue")
  }

  df1[,.(value=sum(value)),by=.(Time,edu,scen)]#??population of the world too slow growth compared to what we have in WIC3. Why?

  iscale.nm = ifelse(iscale == 1000,"Millions",ifelse(iscale == 1000000, "Billions","Thousands"))

  if(ipropgraph) {
    df1 <- df1[,.(edu=.SD$edu,value=prop.table(.SD$value)),by=.(Time,scen)]
  }

  #end Year only for pop
  if(ivar !="pop") df1 <- df1[Time<2100]


  #clean names
  if(F){df1[,scen:=iscen.nm[scen]]}

  gg1 <- df1[Time>2015]%>%mutate(edu=forcats::fct_rev(edu))%>% #reverse factor levels
    ggplot(aes(x=Time,y=value/iscale,fill=edu))+
    geom_area()  +scale_fill_manual(name=NULL,
                                    values=edu.colors,
                                    breaks=edus.plot,
                                    labels=legend.labels)+
    xlab("Year")+ ylab(if(ipropgraph) "Proportion" else paste("Population in",iscale.nm))+
    ggtitle(ititle)+
    facet_wrap(~scen,scales = "fixed")+
    guides(fill = guide_legend(reverse=T))#reverse the legend
  return(gg1)

}#area

