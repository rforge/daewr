\name{GVcontrol}
\alias{GVcontrol}
\title{Control Chart for the generalized variance |S|
}
\description{This function makes a control chart of the generalized variance, |S|.
}
\usage{
GVcontrol(DF,m,n,p)
}

\arguments{
\item{DF}{
input - this is dataframe containing the subgrouped multivariate data. One line for each observation and one column for each variable or quality characteristic being monitored. The first column is a subgroup indicator numbered from 1 to m, with n repeats of each. There should be m x n rows and p + 1 columns.
}
\item{m}{
input this is the number of observations in each subgroup
}
\item{n}{
input this is the known (or estimate from a Phase I study) mean vector of the variables
}
\item{p}{
input this is the number of quality characteristics
}
}
\value{returned list containing the upper control limit, the covariance matrix (S), the generalized variance (|S|), the mean vector (mu), and a vector of the generalized variances (|Si|, i=1,2,...m) within each subgroup.
}
\references{
Alt, F. B. (1985) "Multivariate Quality Control", Encyclopedia of Statistical Sciences, Vol. 6 Editors N. L. Johnson and S. Kotz, John Wiley and Sons, N. Y.
}
\author{
John Lawson
}
\examples{
library(IAcsSPCR)
data(Ryan92)
GVcontrol(Ryan92,20,4,2)
{
  }
}
\keyword{ htest }% use one of  RShowDoc("KEYWORDS")

