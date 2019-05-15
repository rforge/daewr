\name{AASingle}
\alias{AASingle}
\title{ This function recalls ANSI/ASQ Z1.4 Single Sampling Plans from the published tables.
}
\description{
This function calls the function AAZ14Single function that queries the user for the , inspection level, lotsize,
and AQL, and then recalls and prints the appropriate sample sizes, acceptance numbers and rejection numbers and returns a data frame.
}
\usage{
AASingle(type)
}
\arguments{
  \item{type}{  type is either 'Normal', 'Tightened' or 'Reduced' to specify which type sampling plan is desired.}
}

\examples{
## To create a normal sampling plan issue the call:AASingle('Normal')
## the user will then be queried interactively to supply the inspection level
## lot size, and AQL.
## create a tightened sampling plan issue the call:AASingle('Tightened')
## the user will then be queried interactively to supply the inspection level
## lot size, and AQL.
## create a reduced sampling plan issue the call:AASingle('Reduced')
## the user will then be queried interactively to supply the inspection level
## lot size, and AQL.
}

\references{
1. The Statistics Subcommittee of the Accredited Standards Committee Z1 on Quality Environment Dependability and Statistics "American National Standard Sampling Procedures and Tables for AInspection by Attributes"
American Society for Quality, Milwaukee, Wisconsin, 2008.
}
\note{ This function calls the function AAZ14Single to get the specifications and recall and print the plan.}
\author{ John S. Lawson \email{lawson@byu.edu}}

\keyword{sampscheme }
