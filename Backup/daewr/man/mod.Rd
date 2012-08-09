\name{mod}
\alias{mod}%- Also NEED an '\alias' for EACH other topic documented here.
\title{Mod function}
\description{ Gets mod of a to base b
}
\usage{
mod(a,b)
}
\arguments{
  \item{a}{ input- an integer
}
  \item{b}{ input - an integer
}
}
\details{
}
\value{ remainder of a/b or mod(a,b)
}
\references{
}
\author{
John Lawson
}
\note{
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
}
\examples{
mod(5,3)
## The function is currently defined as
mod<-function(a,b)
{a-b*floor(a/b)}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.


