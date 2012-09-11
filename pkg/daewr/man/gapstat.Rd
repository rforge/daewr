\name{gapstat}
\alias{gapstat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{This function computes the gap statistic which is used to test for an outlier using Daniels method
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
gapstat(beta, pse)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{beta}{
input - vector of coefficients from saturated model fit to the data
}
  \item{pse}{
input - Lenth's PSE statistic calculated from the elements of beta
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
returned gap statistic
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
John Lawson
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

## The function is currently defined as
function (beta, pse) 
{
    p <- length(beta)
    psehe <- pse
    sel <- beta >= 0
    betap <- beta[sel]
    betap <- sort(betap)
    betas <- betap[1]
    sel <- beta < 0
    betan <- beta[sel]
    nn <- length(betan)
    betan <- sort(betan)
    betal <- betan[nn]
    zl <- qnorm((nn - 0.375)/(p + 0.25))
    zs <- qnorm((nn + 1 - 0.375)/(p + 0.25))
    gap <- ((betas - betal)/psehe)/(zs - zl)
    return(gap)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
