\name{AAZ19}
\alias{AAZ19}
\title{ This function queries the user for the specs and recalls ANSI/ASQ Z1.9 Variable Sampling Plans from the published tables.
}
\description{
This function recalls Normal, Tightened and Reduced Plans including the sample size n, and maximum prportion nonconforming allowable M.
}
\usage{
AAZ19(type,stype,INSL,LOTS,AQL)
}
\arguments{
  \item{type}{type is the plan type 'Normal', 'Tightened', or 'Reduced', Normal is Default.}
  \item{stype}{stype is "known" or "unknown", unknown is Default}
  \item{INSL}{INSL is 1-5}
  \item{LOTS}{LOTS is 1-16}
  \item{AQL}{AQL is 1-11}
}
\examples{
## create normal sampling plan
# AAZ19('Normal',stype='unknown')
#[1] "MIL-STD-414 ANSI/ASQ Z1.9"
#
# What is the Inspection Level? 
#
# 1: S-3
# 2: S-4
# 3: I
# 4: II
# 5: III
#
# Selection: 4
# What is the Lot Size? 
#
#  1: 2-8                2: 9-15            
#  3: 16-25              4: 26-50           
#  5: 51-90              6: 91-150          
#  7: 151-280            8: 281-400         
#  9: 401-500           10: 501-1200        
# 11: 1201-3200         12: 3201-10,000     
# 13: 10,001-35,000     14: 35,001-150,000  
# 15: 150,001-500,000   16: 500,001 and over
#
# Selection: 4
#
# What is the AQL in percent nonconforming per 100 items? 
#
# 1: 0.10   2: 0.15   3: 0.25   4: 0.40   5: 0.65   6: 1.0 
# 7: 1.5    8: 2.5    9: 4.0   10: 6.5   11: 10    
#
# Selection: 4
# Sample size  n =  5 
# Acceptability constant k =  1.774969 
# Maximum proportion non-conforming M =  0.00041

## create tightened sampling plan
# AAZ19('Tightened',stype='known')
#
# What is the Inspection Level? 
#
# 1: S-3
# 2: S-4
# 3: I
# 4: II
# 5: III
#
# Selection: 4
# 
# What is the Lot Size? 
# 
#  1: 2-8                2: 9-15            
#  3: 16-25              4: 26-50           
#  5: 51-90              6: 91-150          
#  7: 151-280            8: 281-400         
#  9: 401-500           10: 501-1200        
# 11: 1201-3200         12: 3201-10,000     
# 13: 10,001-35,000     14: 35,001-150,000  
# 15: 150,001-500,000   16: 500,001 and over
# 
# Selection: 4
# 
# What is the AQL in percent nonconforming per 100 items? 
# 
#  1: 0.10   2: 0.15   3: 0.25   4: 0.40   5: 0.65   6: 1.0 
#  7: 1.5    8: 2.5    9: 4.0   10: 6.5   11: 10    
# 
# Selection: 4
# Acceptability constant k =  1.93536 
# Sample size  n =  2 
# Maximum proportion non-conforming M =  0.0031


## create reduced sampling plan
# AAZ19('Reduced',stype='unknown')
# 
# What is the Inspection Level? 
# 
# 1: S-3
# 2: S-4
# 3: I
# 4: II
# 5: III
# 
# Selection: 4
# 
# What is the Lot Size? 
# 
#  1: 2-8                2: 9-15            
#  3: 16-25              4: 26-50           
#  5: 51-90              6: 91-150          
#  7: 151-280            8: 281-400         
#  9: 401-500           10: 501-1200        
# 11: 1201-3200         12: 3201-10,000     
# 13: 10,001-35,000     14: 35,001-150,000  
# 15: 150,001-500,000   16: 500,001 and over
# 
# Selection: 4
# 
# What is the AQL in percent nonconforming per 100 items? 
# 
#  1: 0.10   2: 0.15   3: 0.25   4: 0.40   5: 0.65   6: 1.0 
#  7: 1.5    8: 2.5    9: 4.0   10: 6.5   11: 10    
# 
# Selection: 4
# Sample size  n =  5 
# Acceptability constant k =  1.645868 
# Maximum proportion non-conforming M =  0.0134
}
\references{
The Statistics Subcommittee of the Accredited Standards Committee Z1 on Quality Environment Dependability and Statistics "American National Standard Sampling Procedures and Tables for Inspection by Attributes"
American Society for Quality, Milwaukee, Wisconsin, 2008.
}
\author{ John S. Lawson \email{lawson@byu.edu}}

\keyword{file}
