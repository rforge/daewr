EPn<-function( sample=c(1), sided="one", stype="unknown", LSL=-1, USL=-1, sigma=-1 ,xbar=1E9, s=1E9, n=1E9 )
{
  # Calculate the estimated proportion non-conforming
  # using the standardized Beta CDF as shown on
  # pages 45-48 Acceptance Sampling and SPC

  if(sigma<0 && stype=="known") {stop("When stype='known', a known value of sigma must be supplied")}

  ns<-length(sample)

  case<-2

  # First case is where sigma is unknown
  if (stype=="unknown") {case<-1}
  # Second case is where sigma is known
  if (stype=="known") {case<-2}


  while (case==1)  {
    if(ns==1 && stype=="unknown") {if(xbar>.9E9) stop("You must supply either a vector of sample values or xbar, s, and n")}
    if(ns==1 && stype=="unknown") {if(s>.9E9) stop("You must supply either a vector of sample values or xbar, s, and n")}
    if(ns==1 && stype=="unknown") {if(n>.9E9) stop("You must supply either a vector of sample values or xbar, s, and n")}
    if(ns>1 && stype=="unknown") {xb<-mean(sample)}
    if(ns>1 && stype=="unknown") {sdev<-sd(sample)}
    if(ns>1 && stype=="unknown") {n<-ns}
    if(ns==1 && stype=="unknown") {xb<-xbar}
    if(ns==1 && stype=="unknown") {sdev<-s}
    a<-(n/2)-1
    b<-(n/2)-1
    P1<-0
    P2<-0
    P<-0
    # Calculate the proportion below the LSL if there is one
    if(LSL>=0) {

      Q1<-(abs(xb-LSL)/sdev)
      x1<-max(0,.5-.5*Q1*(sqrt(n)/(n-1)))
      P1<-pbeta(x1,a,b)
              }
      P<-P1
    # Calculate the proportion above the USL if there is one
    if(USL>=0) {
      Q2<-(abs(USL-xb)/sdev)
      x2<-max(0,.5-.5*Q2*(sqrt(n)/(n-1)))
      P2<-pbeta(x2,a,b)
               }
    if(sided=="two") {P<-P+P2} else
    {P<-max(P1,P2)}
    format(P,digits=8)
    case<-3
                  }


  if(ns==1 && stype=="known") {if(xbar>.9E9) stop("You must supply either a vector of sample values or xbar and n")}
  if(ns==1 && stype=="known") {if(n>.9E9) stop("You must supply either a vector of sample values or xbar and n")}
  if(ns==1 && stype=="known"){xb<-xbar} else {xb<-mean(sample)}
  if(ns==1 && stype=="known") {sdev<-s} else {sdev<-sd(sample)}
  if(ns==1 && stype=="known") {n<-n} else {n<-ns}

while (case==2)           {
  # Second case is where sigma is known

  a<-(n/2)-1
  b<-(n/2)-1
  P1<-0
  P2<-0
  P<-0
  if(LSL>=0) {
  QL<-((LSL-xb)/sigma)*sqrt(n/(n-1))
  P1<-pnorm(QL,lower.tail=T)
  P<-P1      }
  # Calculate the proportion above the USL if there is one
  if(USL>=0) {
    ZU<-(USL-xbar)/sigma
    QU<-ZU*sqrt(n/(n-1))
    P2<-pnorm(QU,lower.tail=F)
             }
    if(sided=="two")
    {P<-P+P2}
  else
  {P<-max(P1,P2)}
  format(P,digits=8)
  case<-3
                          }

  return(P)
}


