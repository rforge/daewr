arl<-function(h=2,k=2,lambda=1,shift=.5) {
  if((h %%2)!=0){
    stop("h should be an even integer")
  }
  k<-floor(k)
  # Initialize transition matrix
  P<-matrix(0,h+1,h+1)
  # index m for row number(1st state), index n for column number (2nd state)
  for (m in 1:h) {
    for (n in 0:h+1) {
      if (n == 1) { P[m,n]<-ppois(k-(m-1),lambda+shift*sqrt(lambda)) }
      else { P[m,n]<-dpois(k-m+n,lambda+shift*sqrt(lambda))}
      if (n==h+1) {P[m,n]<-1-ppois(k+n-(m+1) ,lambda+shift*sqrt(lambda))}
    }
  }  
  P[h+1,h+1]<-1
  Q<-P[1:h,1:h]
  IQ<-diag(h)-Q
  N<-solve(IQ)
  c<-rep(1,h)
  t<-N%*%c
  t
  ARL<-round(t[1], digits=2)
  ARLFIR<-round(t[floor(h/2)+1], digits=2)
  r1<-list("ARL=",ARL)
  r2<-list(" ARL(FIR)=",ARLFIR," Valid if h is a even integer ")
  result<-c(r1,r2)
  return(result)
}