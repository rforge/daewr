OCASNZ4D<-function(plan,pd) {
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
 pr1[1,1:x]<-pbinom(c[1],n[1],pd)
 for (i in 2:(r[1]-c[1]) ) {pr1[i,1:x]<-dbinom(c[1]+i-1,n[1],pd)
 #print(i)
 #print(c[1]+i-1)
 #print(dbinom(c[1]+i-1,n[1],pd))
   }
 pr1[r[1]-c[1]+1,1:x]<-1-pbinom(r[1]-1,n[1],pd)
 
# Second Sample
 pr2<-array(rep(0,(r[2]-c[2]+1)*x),dim=c((r[2]-c[2]+1),x))
 nd2<-array(rep(0,(r[2]-c[2]+1)*x),dim=c((r[2]-c[2]+1),x))
 nd2[1]<-c[2]
 for (j in 2:(r[2]-c[2]+1)) {nd2[j]<-nd2[j-1]+1}
 pr2[1]<-0
 # Case where accept on 2 
 ## Note if r[2]>C[2]+1 using reduced samplng 
 ## then accept for any nc count less than or 
 ## equal to r[2]-1, but return to normal inspection 
 ## for the next lot
 ce<-r[2]-1
 for (j in 2:(r[1]-c[1])) {
#   bin<-pbinom(c[2]-nd1[j],n[2],pd)
   bin<-pbinom(ce-nd1[j],n[2],pd)
   pr2[1,]<-pr2[1,]+pr1[j,]*bin 
 }
 
 # Case where reject on 2  
 pr2[r[2]-c[2]+1]<-0
 for (j in 2:(r[1]-c[1])) {
   bin<-1-(pbinom(r[2]-nd1[j]-1,n[2],pd))
   pr2[r[2]-c[2]+1,]<-pr2[r[2]-c[2]+1,]+pr1[j,]*bin
 } 

#prob accept OC
OC<-pr1[1, ]+pr2[1, ]
#Prob reject 
Rej<-pr1[r[1]-c[1]+1, ]+pr2[r[2]-c[2]+1, ]
# Prob of Decision 
P1<-pr1[1, ]+pr1[r[1]-c[1]+1, ]
P2<-pr2[1, ]+pr2[r[2]-c[2]+1, ]
ASN<-P1*n[1]+P2*(n[1]+n[2])
data.frame(pd,OC,ASN)
}


