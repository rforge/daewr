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
\arguments{
  \item{type}{ type is either 'Normal', 'Tightened' or 'Reduced' to specify which type sampling plan is desired.}
}
\examples{
## To create a normal sampling plan issue the call: AAMultiple('Normal'),
## the user will then be queried interactively to supply the inspection level
## lot size, and AQL. 
## To create a tightened sampling plan issue the call: AAMultiple('Tightened'),
## the user will then be queried interactively to supply the inspection level
## lot size, and AQL.
## To create a Reduced sampling plan issue the call: AAMultiple('Reduced'),
## the user will then be queried interactively to supply the inspection level
## lot size, and AQL. 
## This example illustrates the use of the AAMultiple function to retrieve the normal 
##  sampling plan from the ANSI/ASQ Z1.4 Standard.
##planM<-AAMultiple('Normal')
##[1] "MIL-STD-105E ANSI/ASQ Z1.4"
##
## What is the Inspection Level?
#
##   1: S-1
##   2: S-2
##   3: S-3
##   4: S-4
##   5: I
##   6: II
##   7: III
#
## Selection: 6
#
## What is the Lot Size?
#
##  1: 2-8                2: 9-15               3: 16-25              4: 26-50
##  5: 51-90              6: 91-150             7: 151-280            8: 281-500
##  9: 501-1200          10: 1201-3200         11: 3201-10,000       12: 10,001-35,000
##  13: 35,001-150,000    14: 150,001-500,000   15: 500,001 and over
#
## Selection: 11
#
## What is the AQL in percent nonconforming per 100 items?
#
##  1: 0.010   2: 0.015   3: 0.025   4: 0.040   5: 0.065   6: 0.10    7: 0.15    8: 0.25
##  9: 0.40   10: 0.65   11: 1.0    12: 1.5    13: 2.5    14: 4.0    15: 6.5    16: 10
## 17: 15     18: 25     19: 40     20: 65     21: 100    22: 150    23: 250    24: 400
## 25: 650    26: 1000
#
## Selection: 12
#
##> planM
##          n  c  r
## first   50  0  4
## second  50  1  6
## third   50  3  8
## fourth  50  5 10
## fifth   50  7 11
## sixth   50 10 12
## seventh 50 13 14
}

\references{
1. The Statistics Subcommittee of the Accredited Standards Committee Z1 on Quality Environment Dependability and Statistics "American National Standard Sampling Procedures and Tables for Inspection by Attributes"
American Society for Quality, Milwaukee, Wisconsin, 2008.
}
\note{ This function calls the function AAZ14Multiple to get the specifications and recall and print the plan.}
\author{ John S. Lawson \email{lawson@byu.edu}}

\keyword{file}
