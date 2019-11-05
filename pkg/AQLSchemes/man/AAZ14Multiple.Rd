\name{AAZ14Multiple}
\alias{AAZ14Multiple}
\title{ This function queries the user for the specs and recalls MIL-STD-105E - ANSI/ASQ Z1.4 Multiple Sampling Plans from the published tables.  
}
\description{
This function is called by the function AAMultiple function. 
}
\usage{
AAZ14Multiple(PLAN, INSL, LOTS, AQL)
}
\arguments{
  \item{PLAN}{ PLAN is the plan type 1=normal, 2=Tightened, 3=reduced.}
  \item{INSL}{ INSL is the inspection level, 1-7.}
  \item{LOTS}{ LOTS is the lot size, 1-15.}
  \item{AQL}{ AQL is the Acceptance Quality Level, 1-26.}
}

\references{
The Statistics Subcommittee of the Accredited Standards Committee Z1 on Quality Environment Dependability and Statistics "American National Standard Sampling Procedures and Tables for Inspection by Attributes" 
American Society for Quality, Milwaukee, Wisconsin, 2008.
}
\note{ This function is called by the function AAMultiple.}
\author{ John S. Lawson \email{lawson@byu.edu}}

\keyword{file}
