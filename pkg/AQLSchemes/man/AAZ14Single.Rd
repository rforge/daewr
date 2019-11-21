\name{AAZ14Single}
\alias{AAZ14Single}
\title{ This function queries the user for the specs and recalls MIL-STD - ANSI/ASQ Z1.4 Double Sampling Plans from the published tables.  
}
\description{
This function is called by the function AASingle function.
}
\usage{
AAZ14Single(PLAN, INSL, LOTS, AQL)
}
\value{
returns a data frame with one row and three columns
}
\arguments{
  \item{PLAN}{ PLAN is the plan type 1=normal, 2=Tightened, 3=reduced.}
  \item{INSL}{ INSL is the inspection level, 1-7.}
  \item{LOTS}{ LOTS is the lot size, 1-15.}
  \item{AQL}{ AQL is the Acceptance Quality Level, 1-26.}
}
\note{ This function is called by the function AASingle.}
\author{ John S. Lawson \email{lawson@byu.edu}}

\keyword{file }
