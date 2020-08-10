MEWMA<-function(X,Sigma=NULL,mu=NULL,Sigma.known=TRUE)
{
  #check for mean and covariance
  if(Sigma.known==TRUE && is.null(Sigma)){
    stop("When Sigma.know=TRUE you must supply mu and Sigma")
  }
  if(Sigma.known==TRUE && is.null(mu)){
    stop("When Sigma.known=TRUE you must supply mu and Sigma")
  }
  # Convert dataframe to matrix
  if(class(X)=="data.frame"){X<-data.matrix(X)}
  nrowx<-nrow(X)
  ncolx<-ncol(X)
  p<-ncolx
  r<-.1
  # calculate h4 for ARL0=200 by quadratic interpolation
  # store limit constants
  h4<-c(8.66,10.79,12.73,14.56,22.67)
  pp<-c(2,3,4,5,10)
  xv<-(pp-4.8)
  xv2<-xv*xv
  ch4<-lm(h4~xv+xv2)
  beta0 <- ch4[["coefficients"]][1]
  beta1 <- ch4[["coefficients"]][2]
  beta2 <- ch4[["coefficients"]][3]
  xp<-c(6,7,8,9)-c(4.8,4.8,4.8,4.8)
  ph4<-round(beta0+beta1*xp+beta2*xp*xp,digits=2)
  limits<-data.frame(c(pp[1:4],c(6,7,8,9),pp[5]),c(h4[1:4],ph4,h4[5]))
  colnames(limits)<-c("p","h4")
  h4<-limits[p-1,2]
  # get mu and sigma
  if(Sigma.known!=TRUE){
    Sigma<-cov(X)
    mu<-colMeans(X)
  }
  # initialize Z
  Z<-matrix(c(rep(0,2*nrowx)),nrow=nrowx,ncol=ncolx)
  # Calculate Zi
  Z[1,]<-r*(X[1,]-mu)
  for(i in 2:nrowx){
    Z[i,]<-r*(X[i,]-mu)+(1-r)*Z[i-1, ] # (2.4)
  }
  # calculate SigmaZ using (2.6)
  SigmaZ<-(r/(2-r))*Sigma
  # Calculate T2
  T2<-c(rep(0,nrowx))
  for (i in 1:nrowx) {
    #  T2[i]<-t(Z[i,])%*%solve(SigmaZ)%*%Z[i,]
    T2[i]<-Z[i,]%*%solve(SigmaZ)%*%Z[i,]
  }
  # make the plot
  mt2<-max(T2)
  my<-1.1*max(h4,mt2)
  plot(c(1:nrowx), T2, type='b',ylim=c(0,my),xlab="i",ylab="T^2", main="MEWMA with ARL(0)=200")
  abline(h=h4)
  text(1.5,h4+1,'h4=UCL')
  # return result
  result1<-list(name="UCL=",value=h4)
  result2<-list(name="Covariance matrix=",value=Sigma)
  result3<-list(name="mean vector",value=mu)
  result<-c(result1,result2,result3)
  return(result)
}
