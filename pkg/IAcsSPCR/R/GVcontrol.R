GVcontrol<-function(DF,m,n,p)
{
  # function for creating control charts of the generalized variance
  names<-colnames(DF)
  if(names[1]!='subgroup'){
    stop("The first column number in the data frame must be called subgroup")
  }
  if(n<=p){
    stop("m must be greater than p")
  }
  nrd<-nrow(DF)
  if(nrd!=n*m){
    stop("The number of rows in the data frame must equal n*m")
  }
  ncd<-ncol(DF)
  if(ncd!=p+1){
    stop("The number of columns in the dataframe must be equal to p+1")
  }
  df<-subset(DF,DF$subgroup==1)
  nrdf<-nrow(df)
  if(nrdf!=n){
    stop("The number of samples in each subgroup must be equal to n")
  }
  # make dataframe into an array of matricies for one for each subgroup
  X<-data.matrix(df[-1])
  # get subgroup mean vectors and covariance matricies
  S<-cov(X)
  GV<-det(S)
  mu<-apply(X,2,mean)
  gr<-2
  while (gr<=m)
  {
    df<-subset(DF,DF$subgroup==gr)
    nrdf<-nrow(df)
    if(nrdf==0){
      stop("The subgroup numbers must be in sequential order from 1 to m")
    }
    if(nrdf!=n){
      stop("The number of samples in each subgroup must be equal to n")
    }
    Y<-data.matrix(df[-1])
    S2<-cov(Y)
    GV2<-det(S2)
    #cat("gr=",gr," GV2=",GV2,"S2=",S2,"\n")
    mu2<-apply(Y,2,mean)
    X<-abind(X,Y,along=3)
    # subgroup mean vectors in rows of mu
    mu<-rbind(mu,mu2)
    S<-abind(S,S2,along=3)
    GV<-c(GV,GV2)
    gr<-gr+1
  }
  rownames(mu)<-c(1:m)
  # get grand mean vector and covariance matrix
  Mu<-apply(mu,2,mean)
  Cov<-apply(S,1:2,mean)
  GVM<-det(Cov)
  # calculate b1
  prod<-n-1
  for (i in 2:p) {
    prod<-prod*(n-i)
  }
  b1<-prod/((n-1)^p)
  # calculate b2
  prod2<-n-1+2
  for (j in 2:p){
    prod2<-prod2*(n-j+2)
  }
  b2<-(prod*(prod2-prod))/((n-1)^(2*p))
  # calculate centerline and control limits
  CL<-det(Cov)
  UCL<-(det(Cov)/b1)*(b1+3*b2^.5)
  LCL<-max(0,(det(Cov)/b1)*(b1-3*b2^.5))
  # plot control chart
  yl<-1.10*max(UCL,max(GV))
  plot(c(1:m),GV,type='b',ylim=c(0,yl),xlab='Subgroup Number',ylab='|S|',main="Control Chart for sample generalized variance")
  abline(h=CL,lty=1,col='black')
  abline(h=UCL,lty=3,col='red')
  text(1,1.05*UCL,"UCL")
  # return result
  result1<-list(name="UCL=",value=UCL)
  result2<-list(name="Covariance matrix=",value=Cov)
  result3<-list(name="Generalized Variance |S|",value=GVM)
  result4<-list(name="mean vector=",value=Mu)
  result5<-list(name="Subgroup Generalized Variances=",value=GV)
  result<-c(result1,result2,result3,result4,result5)
  return(result)
}
