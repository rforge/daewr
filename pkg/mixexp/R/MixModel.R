MixModel <- function(frame, response, mixcomps=NULL,model,procvars=NULL)  {
# Get number of mixture components and process variables
   n.mxcmpts<-length(mixcomps)
   n.prvars<-length(procvars)
   if(length(mixcomps) == 0) stop("No mixture variable names supplied")
# Fits Scheffe Linear Model 
   Linmod<-NULL
   if (model == 1) {
   mixmod<-paste(response,"~")
    for (i in 2:n.mxcmpts) {
    mixmod<-paste(mixmod,"+",mixcomps[i])
       }
 Linmod<-lm(mixmod, data=frame)
# Re-labels coefficients in the output
 for (i in 2:n.mxcmpts) {
   Linmod$coefficients[i]<-Linmod$coefficients[1]+Linmod$coefficients[i]
 }
names(Linmod$coefficients)[1]<-mixcomps[1]
ModelF<-Linmod

                      }

# Fits Scheffe Quadratic Model
  if (model == 2) {
  mixmod<-paste(response,"~")
# Adds linear terms
  for (i in 2:n.mxcmpts) {
    mixmod<-paste(mixmod,"+",mixcomps[i])
  }
# Adds quadratic terms
  for (i in 1:(n.mxcmpts-1)){    
    for (j in (i+1):n.mxcmpts) {
    mixmod<-paste(mixmod,"+",mixcomps[i],":",mixcomps[j])
   }
  } 

  Quadmod<-lm(mixmod, data=frame)
# Re-labels coefficients in the output
  for (i in 2:n.mxcmpts) {
    Quadmod$coefficients[i]<-Quadmod$coefficients[1]+Quadmod$coefficients[i]
  }
names(Quadmod$coefficients)[1]<-mixcomps[1]
ModelF<-Quadmod    
                   }

# Fits Scheffe Cubic Model
if (model == 3) {
  mixmod<-paste(response,"~")
  # Adds linear terms
  for (i in 2:n.mxcmpts) {
    mixmod<-paste(mixmod,"+",mixcomps[i])
  }
  # Adds quadratic terms
  for (i in 1:(n.mxcmpts-1)){  
   for (j in (i+1):n.mxcmpts){
    mixmod<-paste(mixmod,"+",mixcomps[i],":",mixcomps[j])
  }
  }
# Adds cubic terms
  for ( i in 1:(n.mxcmpts-1)) {
    for (j in (i+1):n.mxcmpts){
      mixmod<-paste(mixmod,"+","cubic(",mixcomps[i],",",mixcomps[j],")")
    } 
  }
  
  # Adds special cubic terms
  for (i in 1:(n.mxcmpts-2)){    
    for (j in (i+1):(n.mxcmpts-1)) {
      for (k in (i+2):n.mxcmpts) {
        mixmod<-paste(mixmod,"+",mixcomps[i],":",mixcomps[j],":",mixcomps[k])
      }
    }
  }
  print(mixmod)
  Cubicmod<-lm(mixmod, data=frame)
  # Re-labels coefficients in the output
  for (i in 2:n.mxcmpts) {
    Cubicmod$coefficients[i]<-Cubicmod$coefficients[1]+Cubicmod$coefficients[i]
  }
  names(Cubicmod$coefficients)[1]<-mixcomps[1]
  ModelF<-Cubicmod   
}


# Fits Scheffe Special Cubic Model
if (model == 4) {
  mixmod<-paste(response,"~")
  # Adds linear terms
  for (i in 2:n.mxcmpts) {
    mixmod<-paste(mixmod,"+",mixcomps[i])
  }
# Adds quadratic terms
  for (i in 1:(n.mxcmpts-1)){    
    for (j in (i+1):n.mxcmpts) {
      mixmod<-paste(mixmod,"+",mixcomps[i],":",mixcomps[j])
    }
  }
# Adds special cubic terms
for (i in 1:(n.mxcmpts-2)){    
  for (j in (i+1):(n.mxcmpts-1)) {
    for (k in (i+2):n.mxcmpts) {
    mixmod<-paste(mixmod,"+",mixcomps[i],":",mixcomps[j],":",mixcomps[k])
    }
  }
}
  SpecCmod<-lm(mixmod, data=frame)
  # Re-labels coefficients in the output
  for (i in 2:n.mxcmpts) {
    SpecCmod$coefficients[i]<-SpecCmod$coefficients[1]+SpecCmod$coefficients[i]
  }
  names(SpecCmod$coefficients)[1]<-mixcomps[1]
  ModelF<-SpecCmod    
}

# Fits Mixture Process Variable that is a cross of Scheffe quadratic model in mixture components 
# and linear plus linear by linear interactions in process variables
if (model == 5) {
  if(length(procvars) == 0) stop("No Process variable names supplied")
  mixmod<-paste(response,"~")
  # Adds linear terms in mixture components
  for (i in 2:n.mxcmpts) {
    mixmod<-paste(mixmod,"+",mixcomps[i])
  }
  # Adds quadratic terms in mixture components
  for (i in 1:(n.mxcmpts-1)){    
    for (j in (i+1):n.mxcmpts) {
      mixmod<-paste(mixmod,"+",mixcomps[i],":",mixcomps[j])
    }
  } 
  # Adds linear mixture by linear process variable terms
  for (i in 1:(n.mxcmpts)){
    for (j in 1:(n.prvars)) {
      mixmod<-paste(mixmod,"+",mixcomps[i],":",procvars[j])  
    }
  }
  
  # Adds quadratic mixture by linear process variable terms
  for (i in 1:(n.mxcmpts-1)){    
    for (j in (i+1):n.mxcmpts) {
      for (k in 1:n.prvars){
        mixmod<-paste(mixmod,"+",mixcomps[i],":",mixcomps[j],":",procvars[k])    
      }
    }
  }
  
  # Adds linear mixture by linear by linear interactions in process variable terms
  for (i in 1:(n.mxcmpts)){
    for (j in 1:(n.prvars-1)) {
      for (k in (j+1):n.prvars){
        mixmod<-paste(mixmod,"+",mixcomps[i],":",procvars[j],":",procvars[k])          
      }
    }
  }
  # Adds linear by linear mixture interactions by linear by linear interactions in process variable terms
  for (i in 1:(n.mxcmpts-1)){
    for (l in (i+1):n.mxcmpts){
     for (j in 1:(n.prvars-1)) {
      for (k in (j+1):n.prvars){
        mixmod<-paste(mixmod,"+",mixcomps[i],":",mixcomps[l],":",procvars[j],":",procvars[k])          
      }
    }
  }  
  }
  Mixproc5<-lm(mixmod, data=frame)
  # Re-labels coefficients in the output
  for (i in 2:n.mxcmpts) {
    Mixproc5$coefficients[i]<-Mixproc5$coefficients[1]+Mixproc5$coefficients[i]
  }
  names(Mixproc5$coefficients)[1]<-mixcomps[1]
  ModelF<-Mixproc5   
}

# Fits Kowalski, Cornell and Vining's Reduced Modelin  Mixture and Process Variables
if (model == 6) {
  if(length(procvars) == 0) stop("No Process variable names supplied")
  mixmod<-paste(response,"~")
  # Adds linear terms in mixture components
  for (i in 2:n.mxcmpts) {
    mixmod<-paste(mixmod,"+",mixcomps[i])
  }
  # Adds quadratic terms in mixture components
  for (i in 1:(n.mxcmpts-1)){    
    for (j in (i+1):n.mxcmpts) {
      mixmod<-paste(mixmod,"+",mixcomps[i],":",mixcomps[j])
    }
  } 
  # Adds linear mixture by linear process variable terms
  for (i in 1:(n.mxcmpts)){
    for (j in 1:(n.prvars)) {
      mixmod<-paste(mixmod,"+",mixcomps[i],":",procvars[j])  
    }
  }
  
  # Adds interactions of process variable terms
  for (i in 1:(n.prvars-1)){
    for (j in (i+1):n.prvars){
      mixmod<-paste(mixmod,"+",procvars[i],":",procvars[j])
    }
  }
  
  # Adds Quadratic terms in process variables
  for (i in 1:n.prvars){
    mixmod<-paste(mixmod,"+","I(",procvars[i],"^2)")
  }
  
  Mixproc6<-lm(mixmod, data=frame)
  # Re-labels coefficients in the output
  for (i in 2:n.mxcmpts) {
    Mixproc6$coefficients[i]<-Mixproc6$coefficients[1]+Mixproc6$coefficients[i]
  }
  names(Mixproc6$coefficients)[1]<-mixcomps[1]
  ModelF<-Mixproc6  
}


print(mixmod)

summary(ModelF)
     
                                                                   }