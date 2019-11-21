\name{OCASNZ4S}
\alias{OCASNZ4S}
\title{ This function creates a data frame containing three columns 1)proportion defective=pd, 2)OC=prob accept, and 3)ASN=average sample no.
}
\description{
This function takes the data frame=plan created by  the AAZ14Single function and and a vector of proportion defectives=pd. It calculates OC and ASN values and creates a data frame.
}
\usage{
OCASNZ4S(plan,pd)
}
\value{
returns a data frame containing three columns 1)proportion defective=pd, 2)OC=prob accept, and 3)ASN=average sample no.
}
\arguments{
  \item{plan}{plan is a data frame with two rows containing sample no's=n,acceptance no's=c, and rejection no's=r for first and second samples}
  \item{pd}{pd is a vector of values of the poprtion defective to b used in the OC ASN Curves}
}
\note{ This function calls the function AAZ14Double to get the specifications and recall and print the plan.}
\author{ John S. Lawson \email{lawson@byu.edu}}
\keyword{file}
