\name{EPn}
\alias{EPn}
\title{This function calulates the estimated proprtion non-conforming.}

\description{This function calulates the estimated proprtion non-conforming with sigma unknown or known using the standardized distribution.
}
\usage{
EPn(sample,sided,stype,LSL,USL,sigma,xbar,s,n)
}
\arguments{
  \item{sample}{ sample is a numeric vector of sampled values.}
  \item{sided}{ sided is 'one' or 'two' depending on whether there is one or two specification limits. The default is 'one'.}
  \item{stype}{ stype is 'known' or 'unknown' depending on whether the standard deviation is known. The default is 'unknown'.}
  \item{LSL}{LSL is the lower spoecification limit, leave it out if there is no lower specification limit.}
  \item{USL}{USL is the upper spoecification limit, leave it out if there is no upper specification limit.}
  \item{sigma}{sigma is the known standard deviation, leave it out if it is unknown an stype is 'unknown'.}
  \item{xbar}{sample mean. Leave it out if the vector sample is supplied.}
  \item{s}{sample standard deviation. Leave it out if the vector sample is supplied.} 
  \item{n}{number of items in the sample. Leave it out if the vector sample is supplied.} 
}
\examples{
# one sided lower limit
EPn(sided="one",stype="known",LSL=100,sigma=8,xbar=110,n=10)
EPn(sided="one",stype="unknown",LSL=225,xbar=255,s=15,n=42)
# one sided upper limit
sample<-c(197,188,184,205,201)
EPn(sample,sided="one",USL=209)
# two sided 
sample<-c(197,188,184,205,201)
EPn(sample,sided="two",LSL=180,USL=209)
EPn(sided="two",stype="known",sigma=2,LSL=90,USL=100,xbar=96.68,n=21)

}
\references{Lawson, J. "An Introduction to Acceptance Sampling and SPC with R" pp 45=48.
}
\author{ John S. Lawson \email{lawson@byu.edu}}

\keyword{file}
