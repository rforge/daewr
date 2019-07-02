\name{MPn}
\alias{MPn}\title{This function calulates the maximum allowable proprtion non-conforming.}
\description{This function calulates the maximum allowable proprtion non-conforming with sigma unknown or known using the standardized distribution.
}
\usage{
MPn(k,n,stype)
}
\arguments{
  \item{k}{k is the acceptance constant for the variables sampling plan.}
  \item{stype}{ stype is 'known' or 'unknown' depending on whether the standard deviation is known. The default is 'unknown'.}
  \item{n}{n is number of items in the sample required by the variables sampling plan.} 
}
\examples{
MPn(k=1.6094,n=10,stype="known")
MPn(k=1.905285,n=42,stype="unknown")
}
\references{Lawson, J. "An Introduction to Acceptance Sampling and SPC with R" pp 45=48.
}
\author{ John S. Lawson \email{lawson@byu.edu}}

\keyword{file}