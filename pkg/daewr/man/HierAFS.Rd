\name{HierAFS}
\alias{HierAFS}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{RSM forward regression keeping model hierarchy
}
\description{This function performs a hierarchical forward stepwise regression. If an interaction or quadratic term is entered in the model, the parent main effects are also entered into the model.
}
\usage{
HierAFS(y,x,m,c,step) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{
input - this is a vector containing a single numeric column of response data.
}
  \item{x}{
input - this is a data frame containing the numeric columns of the candidate independent variables. The m three-level factors always preceed the c two-level factors in the design. The factor names or colnames(x)
should always be of length (for example letters of the alphabet "A", "B", etc.)
}
  \item{m}{
input - this is an integer equal to the number of three-level factors in the design
}
  \item{c}{
input - this is an integer equal to the number of two-level factors in the design. Note m+c must be equal to the number of columns of des.
}
  \item{step}{
input - this is a single numeric value containing the n umber of steps requested.
}
}
\value{
returned data frame the first column is a factor variable containing the formula for the model fit at each step, the second numeric column is the R-square statistic for the model fit with each formula.
}
\author{
Gerhard Krennrich, and modified by John Lawson
}

\examples{
#Definitive Screening Design
library(daewr)
set.seed(1234)
x <- DefScreen(m=5,c=0)
x$y <- 3*x$A + 2*x$B + 3*x$A*x$B + 2*x$A^2 + 2*x$C+rnorm(nrow(x),0,.5) 
HierAFS(x$y,x[,-6],m=5,c=0,step=3)
# Example p. 240 Design and Analysis of Experiments with R PB Design
library(BsMD)
data(PB12Des,package="BsMD")
colnames(PB12Des)<-c("c11","c10","c9","c8","G","F","E","D","C","B","A")
#Reorder the columns to match Table 6.11
castf<-PB12Des[c(11,10,9,8,7,6,5,4,3,2,1)]
y<-c(4.733,4.625,5.899,7.0,5.752,5.682,6.607,5.818,5.917,5.863,6.058,4.809)
castf<-cbind(castf,y)
castfr <- castf[ , c(1:7, 12)]
library(daewr)
des<-castfr[ ,c(1:7)]
y<-castfr[ , 8]
HierAFS(y,des,m=0,c=7,step=4)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{htest}

