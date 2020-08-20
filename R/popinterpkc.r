#' Population interpolation annual and single age in MSDem
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

# 1. The initial data as you have seen are for 5-year periods and 5-year intervals (on 1 july of each year),
#
# 2. so the interpolation is done first to create a cross-sectional annual time series of 5-year age groups
# (1-BeersOInterpol.r and IND_Pop2015B5_1.7.csv as sample input test data for India population both sexes for 2015 WPP revision),
#
# 3. and then it gets transformed into single ages (e.g., IND_Pop2015B1x1-step1-sprague.csv),
#
# 4. and interpolated by cohort for the pivotal years ending in 0 and 5 for which the estimates/projections are available
#     (2-SpragueSplit.r).
#
# At very old ages some alternative method with additional constraint needs to be added
# to avoid implausible negative populations (e.g., IND_Pop2015B1x1-step2-pchipGE90.csv).
#
# A special provision is needed to deal with the youngest age group and oldest age groups which cannot be
# interpolated by cohort.
#
# The cohort interpolation can be done using linear or exponential interpolation.
# In previous WPP revisions the interpolation used was linear, but exponential interpolation alleviates
# some of the artifact fluctuations.

popinterpkc <- function(x){
  x<<-x
  head(x)
  # stop()
  #1. The initial data as you have seen are for
  #5-year periods and 5-year intervals (on 1 july of each year),

  iage <- sort(unique(x$age))
  iper <- unique(x$period)
  x1<-x%>%select(period,age,pop)%>%spread(period,pop)%>%select(-age)

  # xx_8_90 <- x1[-19,8]*(x1[-1,8]/x1[-19,7])
  # xx_age0_4 <- (x1[1,8]/sum(x1[5:8,8]))*sum(xx_8_90[4:7,1])
  # x1[,9] <- rbind(xx_age0_4,xx_8_90)
  #colnames(x1)[6] <- 2036
  x1 <- as.matrix(x1)
  row.names(x1) <- iage

  # 2. so the interpolation is done first to create a cross-sectional annual time series of 5-year age groups
  # (1-BeersOInterpol.r and IND_Pop2015B5_1.7.csv as sample input test data for India population both sexes for 2015 WPP revision),
  tp <- t(x1)
  YEAR <- as.numeric(substr(rownames(tp),1,4))
  FYEAR <- min(YEAR) #2011
  LYEAR <- max(YEAR) #2021
  ## dimensions
  NY <- 5                        # Number of years in period
  NCOL <- dim(tp)[2]             # number of data columns/number of countries/time series
  NYEAR5 <- dim(tp)[1]           # number of five year period
  NYEAR1 <- (NYEAR5 - 1) * 5 + 1 # number of single years
  ## Beers Ordinary Interpol
  bc <- c(
    1.0000,  0.0000,  0.0000,  0.0000,  0.0000,  0.0000,
    0.6667,  0.4969, -0.1426, -0.1006,  0.1079, -0.0283,
    0.4072,  0.8344, -0.2336, -0.0976,  0.1224, -0.0328,
    0.2148,  1.0204, -0.2456, -0.0536,  0.0884, -0.0244,
    0.0819,  1.0689, -0.1666, -0.0126,  0.0399, -0.0115,
    0.0000,  1.0000,  0.0000,  0.0000,  0.0000,  0.0000,
    -0.0404,  0.8404,  0.2344, -0.0216, -0.0196,  0.0068,
    -0.0497,  0.6229,  0.5014, -0.0646, -0.0181,  0.0081,
    -0.0389,  0.3849,  0.7534, -0.1006, -0.0041,  0.0053,
    -0.0191,  0.1659,  0.9354, -0.0906,  0.0069,  0.0015,
    0.0000,  0.0000,  1.0000,  0.0000,  0.0000,  0.0000,
    0.0117, -0.0921,  0.9234,  0.1854, -0.0311,  0.0027,
    0.0137, -0.1101,  0.7194,  0.4454, -0.0771,  0.0087,
    0.0087, -0.0771,  0.4454,  0.7194, -0.1101,  0.0137,
    0.0027, -0.0311,  0.1854,  0.9234, -0.0921,  0.0117,
    0.0000,  0.0000,  0.0000 , 1.0000,  0.0000,  0.0000,
    0.0015,  0.0069, -0.0906 , 0.9354,  0.1659, -0.0191,
    0.0053, -0.0041, -0.1006 , 0.7534,  0.3849, -0.0389,
    0.0081, -0.0181, -0.0646 , 0.5014,  0.6229, -0.0497,
    0.0068, -0.0196, -0.0216 , 0.2344,  0.8404, -0.0404,
    0.0000,  0.0000,  0.0000,  0.0000,  1.0000,  0.0000,
    -0.0115,  0.0399, -0.0126, -0.1666,  1.0689,  0.0819,
    -0.0244,  0.0884, -0.0536, -0.2456,  1.0204,  0.2148,
    -0.0328,  0.1224, -0.0976, -0.2336,  0.8344,  0.4072,
    -0.0283,  0.1079, -0.1006, -0.1426,  0.4969,  0.6667,
    0.0000,  0.0000,  0.0000,  0.0000,  0.0000,  1.0000
  )


  ## Format for Beers ordinary coefficients 26 x 6 fixed
  ##first part is added
  bm <- matrix(bc,26,6,byrow = T)

  # number of middle panels
  MP <- NYEAR5 - 5 #Issue, I have NYEARS = 3

  ## creating a beers coefficient matrix for single years
  bcm <- array(0, dim = c(NYEAR1,NYEAR5))

  ## inserting first two panels
  bcm[1:10,1:6] <- bm[1:10,]

  # inserting middle panels
  for (i in (1:MP))
  {
    # calculate the slices and add middle panels accordingly
    bcm[((i + 1)*NY + 1):((i + 2)*NY), i:(i + NY)] <- bm[11:15,]
  }
  ## insert last two panels
  bcm[((NYEAR5 - 3)*NY + 1):(NYEAR1),MP:(MP + NY)]<- bm[16:26,]
  options(max.print = 10000000)
  pop <- bcm %*% as.matrix(tp)
  colnames(pop) <- colnames(tp)
  rownames(pop) <- c(FYEAR:LYEAR)

  tp <-t(pop)
  Age5 <- as.numeric(row.names(tp))

  return(data.frame(age=Age5,tp))

}
# At very old ages some alternative method with additional constraint needs to be added
# to avoid implausible negative populations (e.g., IND_Pop2015B1x1-step2-pchipGE90.csv).
#
# A special provision is needed to deal with the youngest age group and oldest age groups which cannot be
# interpolated by cohort.
#
# The cohort interpolation can be done using linear or exponential interpolation.
# In previous WPP revisions the interpolation used was linear, but exponential interpolation alleviates
# some of the artifact fluctuations.




# births.interp <-function(x) {
#   ########################################################################
#   ## Interpolation of of event data (five year periods)
#   ## fifth/single years by Beers Modified six-term formula
#   ## (Siegel and Swanson, 2004, p. 729)
#   ## This formula applies some smoothing to the interpolant, which is
#   ## recommended for time series of events with some dynamic
#   ## R implementation by Thomas Buettner (21 Oct. 2015)
#   ########################################################################
#
#
#
#   x <<- x
#   tp <- x$births
#   #stop("..")
#   ## interpolation period
#   YEAR1 <- (x$year)
#   YEAR2 <- (x$year)+5
#   YEAR <- 0.5 + (YEAR1 + YEAR2)/2
#
#   FYEAR <- min(YEAR1) #1950
#   LYEAR <- max(YEAR2) #2100
#   LYEAR1 <- LYEAR - 1
#
#   ## dimensions
#   NCOL =1
#   #NCOL <- dim(tp)[2]   ## countries or trajectories
#   #NAG5 <- dim(tp)[1]   ## age or time
#   NAG5 = length(YEAR1)
#   # number of single year or single age groups
#   NAG1 <- NAG5 * 5
#
#   ## Beers Modified Split
#   bc <- c(
#     0.3332, -0.1938,  0.0702, -0.0118,  0.0022 ,
#     0.2569, -0.0753,  0.0205, -0.0027,  0.0006 ,
#     0.1903,  0.0216, -0.0146,  0.0032, -0.0005 ,
#     0.1334,  0.0969, -0.0351,  0.0059, -0.0011 ,
#     0.0862,  0.1506, -0.0410,  0.0054, -0.0012 ,
#
#     0.0486,  0.1831, -0.0329,  0.0021, -0.0009 ,
#     0.0203,  0.1955, -0.0123, -0.0031, -0.0004 ,
#     0.0008,  0.1893,  0.0193, -0.0097,  0.0003 ,
#     -0.0108,  0.1677,  0.0577, -0.0153,  0.0007 ,
#     -0.0159,  0.1354,  0.0972, -0.0170,  0.0003 ,
#
#     -0.0160,  0.0973,  0.1321, -0.0121, -0.0013 ,
#     -0.0129,  0.0590,  0.1564,  0.0018, -0.0043 ,
#     -0.0085,  0.0260,  0.1650,  0.0260, -0.0085 ,
#     -0.0043,  0.0018,  0.1564,  0.0590, -0.0129 ,
#     -0.0013, -0.0121,  0.1321,  0.0973, -0.0160 ,
#
#     0.0003, -0.0170,  0.0972,  0.1354, -0.0159 ,
#     0.0007, -0.0153,  0.0577,  0.1677, -0.0108 ,
#     0.0003, -0.0097,  0.0193,  0.1893,  0.0008 ,
#     -0.0004, -0.0031, -0.0123,  0.1955,  0.0203 ,
#     -0.0009,  0.0021, -0.0329,  0.1831,  0.0486 ,
#
#     -0.0012,  0.0054, -0.0410,  0.1506,  0.0862 ,
#     -0.0011,  0.0059, -0.0351,  0.0969,  0.1334 ,
#     -0.0005,  0.0032, -0.0146,  0.0216,  0.1903 ,
#     0.0006, -0.0027,  0.0205, -0.0753,  0.2569 ,
#     0.0022, -0.0118,  0.0702, -0.1938,  0.3332
#   )
#   ## standard format for Beers coefficients
#   bm <- matrix(bc,25,5,byrow = T)
#
#   ## Age vector
#   a <- seq(0, (NAG5 - 1)*5, by = 5)
#
#   # number of middle panels
#   MP <- NAG5 - 5 + 1
#
#   ## creating a beers coefficient matrix for 18 5-year age groups
#   bcm <- array(0, dim = c(NAG1,NAG5))
#
#   ## insert first two panels
#   bcm[1:10,1:5] <- bm[1:10,]
#
#   for (i in (1:MP))
#   {
#     # calculate the slices and add middle panels accordingly
#     bcm[((i + 1)*5 + 1):((i + 2)*5), i:(i + 4)] <- bm[11:15,]
#   }
#
#   ## insert last two panels
#   bcm[((NAG5 - 2)*5 + 1):(NAG5*5),MP:(MP + 4)] <- bm[16:25,]
#
#   options(max.print = 10000000)
#
#   pop <- bcm %*% as.matrix(tp)
#
#   ## write un-shifted results
#   ## write.csv(pops, ofn)
#
#   ## shifting the interpolant by half a year forward
#   ## In general, each interval is halved and the the halves are recombined to form a calendar year.
#   ## The first missing half year is assumed to be equal the half of the first interval.
#   ## This means simply that the first year is equal to the first (shifted) interval
#   ## The implementation used vector arithmetic by constructing a parallel data array
#   ## with the first element equal to the original first interval,
#   ## and filling the rest with the original data up to n-1 elements
#   ## Adding the two data structures and dividing the result by two
#   ## produces the shifted time reference.
#
#   pop2 <- array(0, dim = c(NAG1,NCOL))
#   pop2[1,] <- pop[1,]
#   pop2[2:NAG1,] <- pop[1:(NAG1 - 1),]
#   pops <- (pop + pop2)*0.5
#
#   return(data.frame(value=pops)%>%mutate(year=seq(FYEAR,LYEAR-1,by=1)))
#
# }
