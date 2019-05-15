OCASNZ4M<-function(plan,pd) {
# Here is where the function OCASN starts
x<-length(pd)
n<-plan[ ,1]
c<-plan[ ,2]
r<-plan[ ,3]
ns<-length(n)

# First Sample
if (c[1]<0) {c1<-0} else {c1<-c[1]}
 pr1<-array(rep(0,r[1]-c1+1*x),dim=c((r[1]-c1+1),x))
 nd1<-array(rep(0,r[1]-c1+1*x),dim=c((r[1]-c1+1),x))
 if (c[1]<0) {nd1[1]<-c1 } else {nd1[1]<-c[1]}
 for (j in 2:(r[1]-c1+1)) {nd1[j]<-nd1[j-1]+1}
 for (i in 2:r[1]-c1) {pr1[i,1:x]<-dbinom(c1+i-1,n[1],pd)}
 pr1[r[1]-c1+1,1:x]<-1-pbinom(r[1]-1,n[1],pd)
 pr1[1,1:x]<-dbinom(c1,n[1],pd)
# Second Sample
 if (c[2]<0) {c2<-0} else {c2<-c[2]}
 pr2<-array(rep(0,(r[2]-c2+1)*x),dim=c((r[2]-c2+1),x))
 nd2<-array(rep(0,(r[2]-c2+1)*x),dim=c((r[2]-c2+1),x))
 nd2[1]<-c2
 for (j in 2:(r[2]-c2+1)) {nd2[j]<-nd2[j-1]+1}
 pr2[1]<-0
 # Case where accept on 2  
 for (j in 2:(r[1]-c1)) {
   bin<-pbinom(c2-nd1[j],n[2],pd)
   pr2[1,]<-pr2[1,]+pr1[j,]*bin 
 }
 if (c[1]<0) pr2[1,]<-pr2[1,]+pr1[1, ]*pbinom(c2-nd1[1],n[2],pd)
 # Case where no decision on 2 
 for (i in 2:(r[2]-c2)) {
   pr2[i]<-0
   for (j in 2:(r[1]-c[1])) {
     if (c[1]<0) {jnew<-j-1} else {jnew<-j}
     bin<-dbinom(nd2[i]-nd1[jnew],n[2],pd)
     pr2[i,]<-pr2[i,]+pr1[jnew,]*bin
   }
 }
 # Case where reject on 2  
 pr2[r[2]-c2+1]<-0
 for (j in 2:(r[1]-c[1])) {
   if (c[1]<0) {jnew<-j-1} else {jnew<-j}
   bin<-1-(pbinom(r[2]-nd1[jnew]-1,n[2],pd))
   pr2[r[2]-c2+1,]<-pr2[r[2]-c2+1,]+pr1[jnew,]*bin
 } 

# Third Sample
v<-r[3]-c[3]+1
pr3<-array(rep(0,(r[3]-c[3]+1)*x),dim=c((r[3]-c[3]+1),x))
nd3<-array(rep(0,(r[3]-c[3]+1)*x),dim=c((r[3]-c[3]+1),x))
nd3[1]<-c[3]
for (j in 2:(r[3]-c[3]+1)) {nd3[j]<-nd3[j-1]+1}
# Case where accept on 3
for (j in 2:(r[2]-c2)) {
  bin<-pbinom(c[3]-nd2[j],n[3],pd)
  pr3[1,]<-pr3[1,]+pr2[j,]*bin
}
if (c[2]<0) pr3[1,]<-pr3[1,]+pr2[1, ]*pbinom(c[3]-nd2[1],n[2],pd)
# Case where no decision on 3
for (i in 2:(r[3]-c[3])) {
  pr3[i]<-0
  for (j in 2:(r[2]-c[2])) {
    if (c[2]<0) {jnew<-j-1} else {jnew<-j}
    bin<-dbinom(nd3[i]-nd2[jnew],n[3],pd)
    pr3[i,]<-pr3[i,]+pr2[jnew,]*bin
  }
}
# Case where reject on 3 #### 
pr3[r[3]-c[3]+1]<-0
for (j in 2:(r[2]-c[2])) {
  if (c[2]<0) {jnew<-j-1} else {jnew<-j}
  bin<-1-(pbinom(r[3]-nd2[jnew]-1,n[3],pd))
  pr3[r[3]-c[3]+1,]<-pr3[r[3]-c[3]+1,]+pr2[jnew,]*bin
}

# Forth sample
pr4<-array(rep(0,(r[4]-c[4]+1)*x),dim=c((r[4]-c[4]+1),x))
nd4<-array(rep(0,(r[4]-c[4]+1)*x),dim=c((r[4]-c[4]+1),x))
nd4[1]<-c[4]
for (j in 2:(r[4]-c[4]+1)) {nd4[j]<-nd4[j-1]+1}
# Case where accept on 4
for (j in 2:(r[3]-c[3])) {
  bin<-pbinom(c[4]-nd3[j],n[4],pd) #### This loop works
  pr4[1,]<-pr4[1,]+pr3[j,]*bin
}
# Case where no decision on 4  
for (i in 2:(r[4]-c[4])) {
  pr4[i]<-0
  for (j in 2:(r[3]-c[3])) {
    bin<-dbinom(nd4[i]-nd3[j],n[4],pd)
    pr4[i,]<-pr4[i,]+pr3[j,]*bin
  }
}
# Case where reject on 4 
pr4[r[4]-c[4]+1]<-0
for (j in 2:(r[3]-c[3])) {
  bin<-1-(pbinom(r[4]-nd3[j]-1,n[4],pd))
  pr4[r[4]-c[4]+1,]<-pr4[r[4]-c[4]+1,]+pr3[j,]*bin
}

# Fifth sample
pr5<-array(rep(0,(r[5]-c[5]+1)*x),dim=c((r[5]-c[5]+1),x))
nd5<-array(rep(0,(r[5]-c[5]+1)*x),dim=c((r[5]-c[5]+1),x))
nd5[1]<-c[5]
for (j in 2:(r[5]-c[5]+1)) {nd5[j]<-nd5[j-1]+1}
# Case where accept on 5
for (j in 2:(r[4]-c[4])) {
  bin<-pbinom(c[5]-nd4[j],n[5],pd) 
  pr5[1,]<-pr5[1,]+pr4[j,]*bin
}
# Case where no decision on 5  
for (i in 2:(r[5]-c[5])) {
  pr5[i]<-0
  for (j in 2:(r[4]-c[4])) {
    bin<-dbinom(nd5[i]-nd4[j],n[5],pd)
    pr5[i,]<-pr5[i,]+pr4[j,]*bin
  }
}
# Case where reject on 5 
pr5[r[5]-c[5]+1]<-0
for (j in 2:(r[4]-c[4])) {
  bin<-1-(pbinom(r[5]-nd4[j]-1,n[5],pd))
  pr5[r[5]-c[5]+1,]<-pr5[r[5]-c[5]+1,]+pr4[j,]*bin
}

# Sixth sample
pr6<-array(rep(0,(r[6]-c[6]+1)*x),dim=c((r[6]-c[6]+1),x))
nd6<-array(rep(0,(r[6]-c[6]+1)*x),dim=c((r[6]-c[6]+1),x))
nd6[1]<-c[6]
for (j in 2:(r[6]-c[6]+1)) {nd6[j]<-nd6[j-1]+1}
# Case where accept on 6
for (j in 2:(r[5]-c[5])) {
  bin<-pbinom(c[6]-nd5[j],n[6],pd) 
  pr6[1,]<-pr6[1,]+pr5[j,]*bin
}
# Case where no decision on 6 
for (i in 2:(r[6]-c[6])) {
  pr6[i]<-0
  for (j in 2:(r[5]-c[5])) {
    bin<-dbinom(nd6[i]-nd5[j],n[6],pd)
    pr6[i,]<-pr6[i,]+pr5[j,]*bin
  }
}
# Case where reject on 6 
pr6[r[6]-c[6]+1]<-0
for (j in 2:(r[5]-c[5])) {
  bin<-1-(pbinom(r[6]-nd5[j]-1,n[6],pd))
  pr6[r[6]-c[6]+1,]<-pr6[r[6]-c[6]+1,]+pr5[j,]*bin
}

# Seventh sample
pr7<-array(rep(0,(r[7]-c[7]+1)*x),dim=c((r[7]-c[7]+1),x))
nd7<-array(rep(0,(r[7]-c[7]+1)*x),dim=c((r[7]-c[7]+1),x))
nd7[1]<-c[7]
for (j in 2:(r[7]-c[7]+1)) {nd7[j]<-nd7[j-1]+1}
# Case where accept on 7
## Note if r[7]>C[7]+1 using reduced samplng 
## then accept for any nc count less than or 
## equal to r[7]-1, but return to normal inspection 
## for the next lot
  ce<-r[7]-1
for (j in 2:(r[6]-c[6])) {
#  bin<-pbinom(c[7]-nd6[j],n[7],pd) 
  bin<-pbinom(ce-nd6[j],n[7],pd)
  pr7[1,]<-pr7[1,]+pr6[j,]*bin
}
# Case where reject on 7 
pr7[r[7]-c[7]+1]<-0
for (j in 2:(r[6]-c[6])) {
  bin<-1-(pbinom(r[7]-nd6[j]-1,n[7],pd))
  pr7[r[7]-c[7]+1,]<-pr7[r[7]-c[7]+1,]+pr6[j,]*bin
}

#prob accept OC
OC<-pr1[1, ]+pr2[1, ]+pr3[1, ]+pr4[1, ]+pr5[1, ]+pr6[1, ]+pr7[1, ]
if (c[1]<0) OC<-OC-pr1[1, ]
if (c[2]<0) OC<-OC-pr2[1, ]
#Prob reject 
Rej<-pr1[r[1]-c1+1, ]+pr2[r[2]-c2+1, ]+pr3[r[3]-c[3]+1, ]+pr4[r[4]-c[4]+1, ]+pr5[r[5]-c[5]+1, ]+pr6[r[6]-c[6]+1, ]+pr7[r[7]-c[7]+1, ]
# Prob of Decision 
P1<-pr1[1, ]+pr1[r[1]-c1+1, ]
if (c[1]<0) P1<-P1-pr1[1, ]
P2<-pr2[1, ]+pr2[r[2]-c2+1, ]
if (c[2]<0) P2<-P2-pr2[1, ]
P3<-pr3[1, ]+pr3[r[3]-c[3]+1, ]
P4<-pr4[1, ]+pr4[r[4]-c[4]+1, ]
P5<-pr5[1, ]+pr5[r[5]-c[5]+1, ]
P6<-pr6[1, ]+pr6[r[6]-c[6]+1, ]
P7<-pr7[1, ]+pr7[r[7]-c[7]+1, ]
ASN<-P1*n[1]+P2*(n[1]+n[2])+P3*(n[1]+n[2]+n[3])+P4*(n[1]+n[2]+n[3]+n[4])+P5*(n[1]+n[2]+n[3]+n[4]+n[5])+P6*(n[1]+n[2]+n[3]+n[4]+n[5]+n[6])+P7*(n[1]+n[2]+n[3]+n[4]+n[5]+n[6]+n[7])
data.frame(pd,OC,ASN)
}


