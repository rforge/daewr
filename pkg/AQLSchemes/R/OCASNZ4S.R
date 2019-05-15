OCASNZ4S<-function(plan,pd) {
  # Here is where the function OCASN starts
  x<-length(pd)
  n<-plan[ ,1]
  c<-plan[ ,2]
  r<-plan[ ,3]
  ns<-length(n)
  
  # First Sample
  pr1<-array(rep(0,r[1]-c[1]+1*x),dim=c((r[1]-c[1]+1),x))
  nd1<-array(rep(0,r[1]-c[1]+1*x),dim=c((r[1]-c[1]+1),x))
  nd1[1]<-c[1]
  for (j in 2:(r[1]-c[1]+1)) {nd1[j]<-nd1[j-1]+1}
  if(c[1]<0) pr1[1,1:x]<-rep(0,x) else pr1[1,1:x]<-pbinom(c[1],n[1],pd)
  for (i in 2:r[1]-c[1]) {pr1[i,1:x]<-dbinom(c[1]+i-1,n[1],pd)}
  pr1[r[1]-c[1]+1,1:x]<-1-pbinom(r[1]-1,n[1],pd)
 
  #prob accept OC
  OC<-pbinom(c[1],n[1],pd)
  ASN<-rep(n[1], x)
  data.frame(pd,OC,ASN)
}


