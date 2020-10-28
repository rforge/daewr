\name{arl}
\alias{arl}
\title{ARL for Lucas's Cusum Chart for Attribute Data
}
\description{Calculates ARL for Lucas's Cusum Chart for Attribute Data
}  
\usage{
arl(h=2,k=2,lambda=1,shift=.5)
}
\arguments{
\item{h}{
input - this is the decision limit. It should be an even number, so that h/2 for the FIR feature will also be an integer.
}
\item{k}{
input - this is the reference value. It should be calculated as (mu_d-mu_a)/ln(mu_d-mu_a), where mu_a is the in-control Poisson mean and mu_d mean to detect. k should be rounded to an integer. 
}
\item{lambda}{
input - this is the in-control Poisson mean. 
}
\item{shift}{
input - this is the number of standard deviation shift from the in-control mean to the mean to detect , i.e.,
lambda+shift*sqrt(lambda)=mu_d. 
}
}
\value{returned list containing the ARL and the ARL with FIR.
}
\references{
Lucas, J.M.(1985) "Counted data cusums", Technometrics, Vol. 27, No. 2, pp129-143. 
}
\author{
John Lawson
}
\examples{
library(IAcsSPCR)
arl(h=6,k=2,lambda=1.88,shift=0)
arl(h=6,k=2,lambda=1.88,shift=.9627)
{
  }
}
\keyword{ htest }% use one of  RShowDoc("KEYWORDS")