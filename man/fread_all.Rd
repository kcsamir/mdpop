% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fread_all.r
\name{fread_all}
\alias{fread_all}
\title{Read the output from MSDem}
\usage{
fread_all(input.dir = NULL, output.dir = NULL, model.patt = NULL)
}
\arguments{
\item{input.dir}{as input directory}

\item{output.dir}{as folder the projections are saved}

\item{model.patt}{is as in fread.data function of msproj}
}
\value{
R list with five data.table 1) var_def 2) state_space 3) mig_dom 4) mig_int 5) results
}
\description{
This function allows you to read all files for a projection
}
\keyword{read}
