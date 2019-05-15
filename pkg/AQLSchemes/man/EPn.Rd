\name{EPn}
\alias{EPn}
\title{ This function calulates the estimated proprtion non-conforming.}

\description{This function calulates the estimated proprtion non-conforming with sigma unknown or known using the standardized distribution.
}
\usage{
EPn(x,sided,stype,LSL,USL,sigma)
}
\arguments{
  \item{x}{ x is a numeric vector of sampled values.}
  \item{sided}{ sided is 'one' or 'two' depending on whether there is one or two specification limits. The default is 'one'.}
  \item{stype}{ stype is 'known' or 'unknown' depending on whether the standard deviation is known. The default is 'unknown'.}
  \item{LSL}{LSL is the lower spoecification limit, leave it out if there is no lower specification limit.}
  \item{USL}{USL is the upper spoecification limit, leave it out if there is no upper specification limit.}
  \item{sigma}{sigma is the value of the standard deviation when the argument stype='known'}
}
\examples{
# two sided 
x<-c(197,188,184,205,201)
EPn(x,sided="two",LSL=180,USL=209)
EPn(x,sided="two",stype="known",LSL=180,USL=209,sigma=9)
EPn(x,sided="two",stype="known",LSL=180,USL=209)
# one sided lower limit
x<-c(643,651,619,627,658,670,673,641,638,650)
EPn(x,sided="one",LSL=620)
# one sided upper limit
x<-c(197,188,184,205,201)
EPn(x,sided="one",USL=209)
EPn(x,sided="one",stype="known",USL=209,sigma=9)
}
\references{Lawson, J. "An Introduction to Acceptance Sampling and SPC with R" pp 45=48.
}
\author{ John S. Lawson \email{lawson@byu.edu}}

\keyword{file}
