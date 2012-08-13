\name{fullnormal}
\alias{fullnormal}
\title{Nornal Plot of Effects}
\description{This function makes a full normal plot of effects labeling those significant and by default adds the reference line (refline). 
}
\usage{
fullnormal<-function(effects,labs,alpha=.05,refline="TRUE")
}
\arguments{
  \item{effects}{
input - This is the numeric vector of effects or regression coefficients to be plotted; it should not include the intercept.
}
  \item{labs}{
input - This is the character vector of effect labels usually obtained from the lm function.
}
  \item{alpha}{
input - The significance level of the test. The default is .05 if alpha is not specified.  
}
  \item{refline}{
input - logical, the default value is TRUE which indicates a reference line will be added to the plot.
}
}
\details{
}
\value{
}
\references{

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
data(plasma)
sol<-lm(y~A*B*C*D*E,data=plasma)
# get whole plot effects and split plot effects
effects<-coef(sol)
effects<-effects[c(2:32)]
Wpeffects<-effects[c(1:4, 6:11, 16:19, 26)]
Speffects<-effects[c(5,12:15,20:25,27:31)]
#make separate normal plots
fullnormal(Wpeffects,names(Wpeffects),alpha=.10)
fullnormal(Speffects,names(Speffects),alpha=.05)
# make normal plot of all effects
fullnormal(effects,names(effects),alpha=.01,refline=FALSE)



## The function is currently defined as
fullnormal<-function(effects,labs,alpha=.05,refline="TRUE") {
crit<-LenthPlot(effects,alpha=alpha,plt=FALSE)["ME"]
names<-names(effects)
names<-gsub(':','',names)
names<-gsub('1','',names)
le<-length(effects)
 for (i in 1:le) {
     logc<-(abs(effects[i])<=crit)
     if (logc) {names[i]<-" "}
                  }
qqnorm(effects, ylab="Estimated Effects", xlab="Normal Scores")
x<-qqnorm(effects,plot=FALSE)
zscr<-(x$x)
# Splits effects into positive and negative for labeling
effp<-effects[zscr>0]
zp<-zscr[zscr>0]
namep<-names[zscr>0]
effn<-effects[zscr<0]
zn<-zscr[zscr<0]
namen<-names[zscr<0]
text(zp,effp,namep,pos=1) 
text(zn,effn,namen,pos=3)  
# calculate pse statistic
ahe<-abs(effects)
s0<-1.5*median(ahe)
selhe<-ahe<(2.5*s0)
pse=1.5*median(ahe[selhe])
if (refline) {
# add reference line to plot
abline(0,pse)
  }

}


