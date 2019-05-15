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
\arguments{
  \item{plan}{plan is a data frame with two rows containing sample no's=n,acceptance no's=c, and rejection no's=r for first and second samples}
  \item{pd}{pd is a vector of values of the poprtion defective to b used in the OC ASN Curves}
}
\examples{
# library(AQLSchemes)
## planS<-AASingle('Normal')
# What is the Inspection Level? 
#  
#   1: S-1
#   2: S-2
#   3: S-3
#   4: S-4
#   5: I
#   6: II
#   7: III
#
# Selection: 6
#
# What is the Lot Size? 
#  
#  1: 2-8                2: 9-15               3: 16-25              4: 26-50           
#  5: 51-90              6: 91-150             7: 151-280            8: 281-500         
#  9: 501-1200          10: 1201-3200         11: 3201-10,000       12: 10,001-35,000   
#  13: 35,001-150,000    14: 150,001-500,000   15: 500,001 and over  
#
# Selection: 11
#
# What is the AQL in percent nonconforming per 100 items? 
#  
#  1: 0.010   2: 0.015   3: 0.025   4: 0.040   5: 0.065   6: 0.10    7: 0.15    8: 0.25 
#  9: 0.40   10: 0.65   11: 1.0    12: 1.5    13: 2.5    14: 4.0    15: 6.5    16: 10   
# 17: 15     18: 25     19: 40     20: 65     21: 100    22: 150    23: 250    24: 400  
# 25: 650    26: 1000   
#
# Selection: 12
#
#> planS
#     n c r
# 1 200 7 8
# Pnc<-seq(0,.08,.001)
# OCASNZ4S(planS,Pnc)
}

\references{
1. The Statistics Subcommittee of the Accredited Standards Committee Z1 on Quality Environment Dependability and Statistics "American National Standard Sampling Procedures and Tables for Inspection by Attributes"
American Society for Quality, Milwaukee, Wisconsin, 2008.
}
\note{ This function calls the function AAZ14Double to get the specifications and recall and print the plan.}
\author{ John S. Lawson \email{lawson@byu.edu}}

\keyword{file}
