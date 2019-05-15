EPn<-function(x,sided="one",stype="unknown",LSL=-1,USL=-1,sigma=-1){
  # Calculate the estimated proportion non-conforming
  # using the standardized Beta CDF as shown on
  # pages 45-48 Acceptance Sampling and SPC
  case<-2
  n<-length(x)
  xbar<-mean(x)
  s<-sd(x)
  a<-(n/2)-1
  b<-(n/2)-1
  P1<-0
  P2<-0
  P<-0
  # First case is where sigma is unknown
  if (stype=="unknown") {case<-1}
while (case==1)  { 
  # Calculate the proportion below the LSL if there is one
  if(LSL>=0) {
    Q1<-(abs(xbar-LSL)/s)
    x1<-max(0,.5-.5*Q1*(sqrt(n)/(n-1)))
    P1<-pbeta(x1,a,b)}
  P<-P1
  # Calculate the proportion above the USL if there is one
  if(USL>=0) {
    Q2<-(abs(USL-xbar)/s)
    x2<-max(0,.5-.5*Q2*(sqrt(n)/(n-1)))
    P2<-pbeta(x2,a,b)}
  if(sided=="two")
  {P<-P+P2}
  else
  {P<-max(P1,P2)}
  format(P,digits=8)
  options(scipen=999)
    case<-3                }
while (case==2)           {
  # Second case is where sigma is known  
  if(LSL>=0) {
  QL<-((xbar-LSL)/sigma)*sqrt(n/(n-1))
  P1<-pnorm(QL,lower.tail=F) 
  P<-P1 }
  # Calculate the proportion above the USL if there is one
  if(USL>=0) {
    ZU<-(USL-xbar)/sigma
    QU<-ZU*sqrt(n/(n-1))
    P2<-pnorm(QU,lower.tail=F) }
    if(sided=="two")
    {P<-P+P2}
  else
  {P<-max(P1,P2)}
  format(P,digits=8)
  options(scipen=999)
  
   case<-3                }
  
  if (sigma<=0 && stype=="known") {cat("When stype='known', a known value of sigma must be supplied", "\n")}
  else
  {cat("Estimated proportion non-conforming = ", P) } 
  
}

  
