FitDefSc<-function(y,design,steps=1)
{
  modme<-lm(y~(.),data=design)
  Smry<-summary(modme)
  # main effect p-values
  Smry$coefficients[,4]
  # main effect labels
  menames<-rownames(Smry$coefficients)[-1]
  #effects with p-values less than .2
  sige<-Smry$coefficients[Smry$coefficients[,4]<.2,4]
  # names of main effects with p-values less than .2
  nsigme<-names(sige)[-1]
  # reduced design matrix containing effects with p-values less than .2
  subdes<-design[,nsigme]
  # call fhstep to get add largest 2nd order effect
  trm<-fhstep(y,subdes,m=length(nsigme),c=0,prvm=nsigme)
  # continue calling fhstep 
  if(steps>1) { for (i in 2:steps-1){
    trm<-fhstep(y,subdes,m=length(nsigme),c=0,prvm=trm)}  
  }
}