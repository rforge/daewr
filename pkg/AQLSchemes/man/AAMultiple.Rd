\name{AAMultiple}
\alias{AAMultiple}
\title{ This function recalls MIL-STD-105E - ANSI/ASQ Z1.4 Multiple Sampling Plans from the published tables.
}
\description{
This function calls the function AAZ14Multiple function that queries the user for the inspection level, lotsize,
and AQL, and then it recalls the appropriate sample sizes, acceptance numbers and rejection numbers and returns a data frame.
}
\usage{
AAMultiple(type)
}
\value{
returns a data frame with seven rows and three columns
}
\arguments{
  \item{type}{ type is either 'Normal', 'Tightened' or 'Reduced' to specify which type sampling plan is desired.}
}

\references{
<https://archive.org/details/MIL-STD-105E_1>
}
\note{ This function calls the function AAZ14Multiple to get the specifications and recall and print the plan.}
\author{ John S. Lawson \email{lawson@byu.edu}}

\keyword{file}
