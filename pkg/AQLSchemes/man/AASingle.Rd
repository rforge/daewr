\name{AASingle}
\alias{AASingle}
\title{ This function recalls MIL-STD-105E - ANSI/ASQ Z1.4 Single Sampling Plans from the published tables.
}
\description{
This function calls the function AAZ14Single function that queries the user for the , inspection level, lotsize,
and AQL, and then recalls and prints the appropriate sample sizes, acceptance numbers and rejection numbers and returns a data frame.
}
\usage{
AASingle(type)
}
\value{
returns a data frame with one row and three columns
}
\arguments{
  \item{type}{  type is either 'Normal', 'Tightened' or 'Reduced' to specify which type sampling plan is desired.}
}


\references{
<https://archive.org/details/MIL-STD-105E_1>
}
\note{ This function calls the function AAZ14Single to get the specifications and recall and print the plan.}
\author{ John S. Lawson \email{lawson@byu.edu}}

\keyword{sampscheme }
