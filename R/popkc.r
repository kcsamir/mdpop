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

popkc <- function(resX=res,
                  period = NULL,
                  agel=0,ageu="max",byage=FALSE,
                  sex=NULL, bysex=FALSE,
                  reg=NULL, byreg=FALSE,
                  resi=NULL,byresi=FALSE) {
          #Try this for with residence
          #need to try all possible combinations

            # #need res data
            # if(length(grep("res",ls()))==0) print("Run fread_all() first")

            var_def <<- resX$var_def
            # head(var_def)
            # unique(var_def$variables)
            ages = var_def[variables=="age"][,values:=as.numeric(values)]

            if(ageu == "max") ageu = max(ages$values)

            periods = var_def[variables=="period"][,values:=as.numeric(values)]$values
            periods = c(periods,max(periods)+unique(diff(periods)))
            if(!is.null(period)) periods = period

            sexes = var_def[variables=="sex"]$values
            if(!is.null(sex)) sexes = sex

            regions = var_def[variables=="region"]$values
            if(!is.null(reg)) regions = reg

            group.var = "period"
            if(bysex) group.var = c(group.var,"sex")
            if(byreg) group.var = c(group.var,"region")

            resX <- resX$results

            if(any(grep("resi",unique(var_def$variables)))) {
              residences = var_def[variables=="resi"]$values
              if(!is.null(resi)) residences = resi
              if(byres) key.cols = c(group.var,"residence")
              res.pop = resX[period%in%periods&age%in%seq(agel,ageu)&sex%in%sexes&reg%in%regions&resi%in%residences
                                  ,.(pop=sum(pop)),by=group.var]
            } else {

              res.pop <- resX[period%in%periods & age%in%seq(agel,ageu,by=5) & sex%in%sexes & region%in%regions,
                   .(pop=sum(pop)),by=group.var]

            }
        return(res.pop)
  }
