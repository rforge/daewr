\name{MEWMA}
\alias{MEWMA}
\title{Multivariate EWMA Control Chart
}
\description{Computes a MEWMA using the method of Lowry, Woodall, Champ  and Rigdon. The number of variables p must be between 2 and 10, r is fixed at .1
}
\usage{
MEWMA(X,Sigma=NULL,mu=NULL,Sigma.known=TRUE)
}

\arguments{
\item{X}{
input - this is a matrix or data frame containing the multivariate data. One line for each observation and one column for each variable or quality characteristic being monitored.
}
\item{Sigma}{
input this is the known (or estimate from a Phase I study) covariance matrix of the variables
}
\item{mu}{
input this is the known (or estimate from a Phase I study) mean vector of the variables
}
\item{Sigma.known}{
input this is a logical variable, if TRUE, Sigma, and mu must be supplied, if FALSE the function will estimate them from the data in X
}
}
\value{returned list containing the upper control limit, the covariance matrix and the mean vector.
}
\references{
Lowry, Woodall, Champ  and Rigdon(1992)<https://www.tandfonline.com/doi/abs/10.1080/00401706.1992.10485232.>
}
\author{
John Lawson
}
\examples{
data(Lowry)
Sigma<-matrix(c(1, .5, .5, 1), nrow=2, ncol=2)
mu<-c(0,0)
MEWMA(Lowry,Sigma,mu,Sigma.known=TRUE)
MEWMA(Lowry,Sigma.known=FALSE)
mu5<-c(-.314,.32)
Sig5<-matrix(c(1.16893, -.3243, -.3243, 1.16893), nrow=2, ncol=2)
MEWMA(Lowry,Sig5,mu5,Sigma.known=TRUE)

## The function is currently defined as
function(X,Sigma=NULL,mu=NULL,Sigma.known=TRUE)
{
  }
}
\keyword{ htest }% use one of  RShowDoc("KEYWORDS")

