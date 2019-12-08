AASingle <- function(type="Normal") {
  if(type == "Normal"){
    iplan <- 1
  } else if (type == "Tightened"){
    iplan <- 2
  } else if (type == "Reduced"){
    iplan <- 3
  } else {
    iplan<-4
  }
  if(iplan==4) {stop("type must be equal to 'Normal' 'Reduced' or 'Tightened'")} else
  {plan<-AAZ14Single(PLAN=iplan,INSL=1,LOTS=1,AQL=1)
  return(plan)}
}
