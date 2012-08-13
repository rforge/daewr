\name{hardwood}
\alias{hardwood}
\docType{data}
\title{low grade hardwood conjoint study}
\description{
Data from the low grade hardwood conjoint study in chapter 6 of Design and Analysis
of Experiments with R
}
\usage{data(hardwood)}
\format{
  A data frame with 12 observations on the following 5 variables.
  \describe{
    \item{\code{Design}}{a factor with levels \code{"RC"} \code{"AC"} \code{"OCI"} \code{"OCII"}}
    \item{\code{Price}}{a numeric variable}
	\item{\code{Density}}{a factor with levels \code{"Clear"} \code{"Heavy"} \code{"Medium"}}	
	\item{\code{Guarantee}}{a factor with levels \code{"1y"} \code{"Un"} }
    \item{\code{Rating}}{a numeric vector}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
Design and Analysis of Experiments with R, by John Lawson, CRC/Chapman Hall
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(hardwood)
}
\keyword{datasets}
