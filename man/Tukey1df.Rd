\name{Tukey1df}
\alias{Tukey1df}
\title{Tukey's single degree of freedom test for additivity}
\description{This function performs Tukey's single degree of freedom test for additivity in an unreplicated two-factor factorial.
}
\usage{
Tukey1df<-function(data)
}
\arguments{
  \item{data}{
input - This is a data frame containing an unreplicated two-factor factorial. The first column should be an numeric response, the second and third columns should be factors. At least one of the factors should have more than 2 levels.
}
}
\details{
}
\value{
}
\references{
 Tukey, J.   "One degree of freedom test for non-additivity" \emph{Biometrics} Vol 5, pages 232-242., 1949.
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
Dilution<-rep(c(3,4,5),6)
Sample<-rep(c(1,2,3,4,5,6),each=3)
 y<-c(1.87506,1.38021,0.60206,1.74036,1.36173,0.90309,1.79934,1.25527,0.95424,2.02119,1.39794,1.000,1.79934,1.20412,0.60206,1.59106,1.25527,0.60206)
 virus<-data.frame(y=y, Sample=factor(Sample),Dilution=factor(Dilution))

Tukey1df(virus)


## The function is currently defined as
Tukey1df<-function(data) {
y<-data[,1]
Afactor<-data[,2]
Bfactor<-data[,3]
tst1<-is.factor(Afactor)
tst2<-is.factor(Bfactor)
tst3<-is.numeric(y)
if (tst1&tst2&tst3) {
a<-nlevels(Afactor)
b<-nlevels(Bfactor)
  }  else {stop("The first column of the data frame is the numeric response, the 2nd and 3rd columns should be coded as factors") }
tst4<-max(a,b)>2
tst5<-length(y)==a*b
if (tst4&tst5) {
ybb<-with(data, tapply(y, Bfactor, mean))
yba<-with(data, tapply(y, Afactor, mean))
sbb<-with(data, tapply(y, Bfactor, sum))
sba<-with(data, tapply(y, Afactor, sum))
ybardd<-mean(y)
CT<-(sum(y)^2)/(a*b)
ssA<-sum(sba^2/b)-CT
ssB<-sum(sbb^2/a)-CT
ssE<-sum(y^2)-CT-ssA-ssB
ybdj<-rep(ybb,6)
prody<-y*ybdj
sumprod<-tapply(prody,Afactor,sum)
leftsum<-sum(sumprod*yba)
ssAB<-(a*b*(leftsum-(ssA+ssB+a*b*ybardd^2)*ybardd)^2/(ssA*ssB))
ssR<-ssE-ssAB
F<-ssAB/(ssR/((a-1)*(b-1)-1))
Pval<-1-pf(1,((a-1)*(b-1)-1),F)
cat("Source           df     SS        MS        F     Pr>F","\n")
cat("A            ",paste(format(a-1, width=6)," ", format(round(ssA,4),justify="right"),"  ",format(round(ssA/(a-1),4), justify="right"),"\n"),sep="")
cat("B            ",paste(format(b-1, width=6)," ", format(round(ssB,4),justify="right"),"  ",format(round(ssB/(b-1),4), justify="right"),"\n"),sep="")
cat("Error        ",paste(format((b-1)*(a-1), width=6)," ", format(round(ssE,4),justify="right"),"  ",format(round(ssE/(a-1)*(b-1),4), justify="right"),"\n"),sep="")
cat("NonAdditivity",paste(format(1, width=6)," ", format(round(ssAB,4),justify="right"),"  ",format(round(ssAB,4),justify="right"),"  ",format(round(F,2),justify="right"),"  ",format(round(Pval,4),justify="right"),"\n"),sep="")
cat("Residual     ",paste(format((b-1)*(a-1)-1, width=6)," ", format(round(ssR,4),justify="right"),"  ",format(round(ssR/((a-1)*(b-1)-1),4), justify="right"),"\n"),sep="")
   } else {stop("This function only works for unreplicated 2-factor factorials with >2 levels for one of the factors")}
}

}
\keyword{ }

