\name{OCASNZ4M}
\alias{OCASNZ4M}
\title{ This function creates a data frame containing three columns 1)proportion defective=pd, 2)OC=prob accept, and 3)ASN=average sample no.
}
\description{
This function takes the data frame=plan created by  the AAZ14Multiple function and and a vector of proportion defectives=pd. It calculates OC and ASN values and creates a data frame.
}
\usage{
OCASNZ4M(plan,pd)
}
\value{
returns a data frame containing three columns 1)proportion defective=pd, 2)OC=prob accept, and 3)ASN=average sample no.
}
\arguments{
  \item{plan}{plan is a data frame with seven rows containing sample no's=n,acceptance no's=c, and rejection no's=r for first through seventh samples}
  \item{pd}{pd is a vector of values of the poprtion defective to b used in the OC ASN Curves}
}
\note{ This function calls the function AAZ14Double to get the specifications and recall and print the plan.}
\examples{
planm<-data.frame(n=c(50,50,50,50,50,50,50),c=c(0,1,3,5,7,10,13),r=c(4,6,8,10,11,12,14))
Pnc<-seq(0,.08,.005)
OCASNM<-OCASNZ4M(planm,Pnc)
OCASNM$OC
OCASNM$ASN
}
\author{ John S. Lawson \email{lawson@byu.edu}}

\keyword{file}
