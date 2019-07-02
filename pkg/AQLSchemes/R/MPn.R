MPn<-function(k=-1,n=-1,stype="unknown") {
if(k<0){stop("You must supply a value for k","\n")}
if(n<0){stop("You must supply a value for n","\n")}
  if(stype=="known") {M<-1-pnorm(k*sqrt(n/(n-1)))}
  else
  {BM<-.5*(1 - k* sqrt(n)/(n-1));
    M<-pbeta(BM,((n-2)/2),((n-2)/2)) }
  return(M)
}
