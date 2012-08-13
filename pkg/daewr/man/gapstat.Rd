\name{gapstat}
\alias{gapstat}
\title{Gap Statistic}
\description{This function calculates the gap statistic used in the automated Daniel method of detecting an outlier in an 8, 16 or 32 run \eqn{2^{k-p}} design.
}
\usage{
gapstat<-function(beta,pse)
}
\arguments{
  \item{beta}{
input - This is the the vector of effects or regression coefficients calculated from the design where an outlier is suspected. This vector of coefficients should not include the intercept. This function is called by the function Gaptest at the first and second pass through the data.
}
  \item{pse}{
input - This Lenth's pse statistic calculated on the vector beta befor the call to this function.  
}
}
\details{
}
\value{
The result is the gap statistic \eqn{gap=((\beta_s-\beta_l)/pse)/(z_s-z_l)}.

}
\references{
 Lawson, J. and Gatlin, J.  "Finding bad values in factorials - revisited" \emph{Quality Engineering} Vol 18, pages 491-501., 2006.
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
Beta<-c(.06,.25,-.01,.5,0,-.02,0,.14,.03,-.01,.02,.04,.02,.01,.02)
names(Beta)<-c("A","B","C","D","AB","AC","AD","BC","BD","CD","ABC","ABD","ACD","BCD","ABCD")
gapstat(Beta,.06)

## The function is currently defined as
gapstat<-function(beta,pse) {
# computes the standardized gap score
p<-length(beta)
psehe<-pse
# gets positive coefficients
sel<-beta >= 0
betap<-beta[sel]
# sorts positive elements
betap<-sort(betap)
# gets Beta_s
betas<-betap[1]
#gets negative coefficients
sel<-beta < 0
betan<-beta[sel]
nn<-length(betan)
# sorts negative coefficients
betan<-sort(betan)
#gets Beta_L
betal<-betan[nn]
# gets Z_L and Z_S
zl<-qnorm((nn-.375)/(p+.25))
zs<-qnorm((nn+1-.375)/(p+.25))
# calculates gap statistic
gap<-((betas-betal)/psehe)/(zs-zl)
return(gap)
                      }


}
\keyword{ }

