pkgname <- "daewr"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('daewr')

assign(".oldSearch", search(), pos = 'CheckExEnv')
cleanEx()
nameEx("Apo")
### * Apo

flush(stderr()); flush(stdout())

### Name: Apo
### Title: apolipoprotein survey varaince component study
### Aliases: Apo
### Keywords: datasets

### ** Examples

data(Apo)



cleanEx()
nameEx("BIBsize")
### * BIBsize

flush(stderr()); flush(stdout())

### Name: BIBsize
### Title: Balanced incomplete blocksize
### Aliases: BIBsize

### ** Examples

BIBsize(6,3)

## The function is currently defined as
BIBsize<-function(t,k)
{
  b<-t
  r<-0
  lambda<-0
  check<-0
  while (check==0) {
   while (r==0) {
     #cat("r=",r)
     testr<-(b*k)/t
     #cat("testr=",testr,"b=",b)
     if (testr==floor(testr)) {
       r<-testr
       } else {
       b<-b+1
       }
     }
      #cat("b=",b, "r=",r)
      testl<-(r*(k-1))/(t-1)
      #cat("testl=",testl,"b=",b)
      if (testl==floor(testl)) {
       lambda<-testl
       check=1
       } else {
       r<-0
       b<-b+1
      #cat("b=",b, "r=",r)
        }
}





cleanEx()
nameEx("BPmonitor")
### * BPmonitor

flush(stderr()); flush(stdout())

### Name: BPmonitor
### Title: blood pressure monitor experiment
### Aliases: BPmonitor
### Keywords: datasets

### ** Examples

data(BPmonitor)



cleanEx()
nameEx("Bdish")
### * Bdish

flush(stderr()); flush(stdout())

### Name: Bdish
### Title: Confounded Block Dishwashing Experiment
### Aliases: Bdish
### Keywords: datasets

### ** Examples

data(Bdish)



cleanEx()
nameEx("Bff")
### * Bff

flush(stderr()); flush(stdout())

### Name: Bff
### Title: Confounded block fractional mouse growth experiment
### Aliases: Bff
### Keywords: datasets

### ** Examples

data(Bff)



cleanEx()
nameEx("BoxM")
### * BoxM

flush(stderr()); flush(stdout())

### Name: BoxM
### Title: Box and Meyer's unreplicated 2^4 from Chapter 3
### Aliases: BoxM
### Keywords: datasets

### ** Examples

data(BoxM)



cleanEx()
nameEx("COdata")
### * COdata

flush(stderr()); flush(stdout())

### Name: COdata
### Title: CO emmisions experiment data from Chapter 3
### Aliases: COdata
### Keywords: datasets

### ** Examples

data(COdata)



cleanEx()
nameEx("Fcrit")
### * Fcrit

flush(stderr()); flush(stdout())

### Name: Fcrit
### Title: F-Distribution critical values
### Aliases: Fcrit

### ** Examples

Fcrit(.05,2,15)
## The function is currently defined as
function(alpha,nu1,nu2) qf(1-alpha,nu1,nu2)



cleanEx()
nameEx("Fpower")
### * Fpower

flush(stderr()); flush(stdout())

### Name: Fpower
### Title: F-Distribution Power Calculation
### Aliases: Fpower

### ** Examples

Fpower(0.05,2,15,6.428)

## The function is currently defined as
function(alpha,nu1,nu2,nc) 1-pf(Fcrit(alpha,nu1,nu2),nu1,nu2,nc)



cleanEx()
nameEx("Gaptest")
### * Gaptest

flush(stderr()); flush(stdout())

### Name: Gaptest
### Title: Automated Daniel Method
### Aliases: Gaptest

### ** Examples

# Lawson Gatlin Example
library(FrF2)
lawg<-FrF2(16,11,generators=c("ABC","BCD","ACD","ABD","ABCD","AB","AC"),randomize=FALSE)
y<-c(31,48,43,32,54,52,60,34,47,43,30,26,67,49,80,41)
lawg<-cbind(lawg,y)
Gaptest(lawg)

# eight run example
library(FrF2)
des<-FrF2(8,6,generators=c("AB","AC","BC"),randomize=FALSE)
y<-c(1.299,1.601,1.359,1.461,1.338,1.486,1.330,1.470)
Des8<-cbind(des,y)
Gaptest(Des8)
#tes<-lm(y~(.)^3,data=Des8)
#summary(tes)
#c<-tes$coef
#cn<-names(c)
#ccn<-gsub("[^A-Z]","",cn)




## The function is currently defined as
Gaptest<-function(DesY) {
# function to compute gap statistic
ncheck<-dim(DesY)
ncheck<-ncheck[1]
 tcnd=TRUE
 if (ncheck==8) {tcnd=FALSE}
 if (ncheck==16) {tcnd=FALSE}
 if (ncheck==32) {tcnd=FALSE}
 if (tcnd) {stop("This function only works for 8, 16, or 32 run designs","\n")
    } else {
if (ncheck==8) ncheck=16
#####################################
# 50th and 99th percentiles of the gap statistic ##
critg16<-c(1.7884,5.1009)
critg32<-c(1.7297,5.8758)

### First Pass through the data ####

###### Step 1  #######
#fit model to saturated design
modf<-lm(y~(.)^4,x=TRUE,data=DesY)


#extract the regression coefficients
nbeta<-dim(DesY)
nbeta<-nbeta[1]
he<-modf$coef
# This extracts the coefficients that are not NA
selcol<-which(!is.na(he))
he<-he[selcol]
he<-he[-1]
#number of coefficients
p<-length(he)
#number of runs
n<-p+1
# This trims unnecessary characters from coefficient names
cn1<-names(he)
ccn1<-gsub("[^A-Z]","",cn1)
names(he)<-ccn1

##### End of Step 1 ########

###### Steps 2 and 3  #######
#calculate the pse statistic
ahe<-abs(he)
s0<-1.5*median(ahe)
selhe<-ahe<(2.5*s0)
pse=1.5*median(ahe[selhe])
#library(BsMD)
#pse<-LenthPlot(modf,plt=FALSE)
#pse<-pse[2]
#calculate the gap statistic
gap<-gapstat(he,pse)
# checks to see if gap statistic exceeds 50th percentile
if (ncheck==16) {test=(gap>critg16[1])
 } else  {test=(gap>critg32[1])}
##### End Step 2 and 3  #####

if (test) {
##### Step 4  #####
#extract the model X matrix
X<-modf$x
# This selects columns of the X matrix that correspond to non-missing
# coefficients
X<-X[,selcol]
X<-X[,-1]
#gets signs of regression coefficients
se<-as.matrix(sign(he),nrow=1)
# find signigicant effects using LGB
sigef<-LGB(he,rpt=FALSE,plt=FALSE)
# make signs of significant effects zero
 for (i in 1:length(he)) {
     if (sigef[i]=="yes")   {se[i]=0 }
                         }
#gets sum of products of signed effects and rows of X matrix
sp<-X

#finds index of largest sum of products as index of potential outlier
asp<-abs(sp)
oo<-max.col(t(asp))

### End Step 4 ####

###### Step 5  ######

# calculates the bias
 # first get absolute regression coefficients
ae<-abs(he)
 # next sort absolute effects
sae<-sort(ae)
 #get the number of effects in smallest half
nsmall<-round(length(he)/2)
 # sum the smallest half absolute effects to get bias
bias<-2*sum(sae[1:nsmall])

##### Step 6 ######
 # gets corrected response vector
y<-DesY$y
ycorr<-DesY$y
ycorr[oo]<-ycorr[oo]+(-1*sign(sp[oo]))*bias
 # makes vector of indicators for outlier
detect<-c(rep("no",n))
detect[oo]<-"yes"
cat("Initial Outlier Report","\n")
cat("Standardized-Gap = ",gap, "Significant at 50th percentile","\n")
### End of first pass throught the data #######

### Second Pass throught the data ###########
### Step 1 ####
# augment DesY with corrected data
DesYc<-cbind(DesY[,1:(dim(DesY)[2]-1)],ycorr)
# fit saturated model to corrected data
modf<-lm(ycorr~(.)^4,x=TRUE,data=DesYc)

#extract the regression coefficients
che<-modf$coef
# This extracts the coefficients that are not NA
che<-che[!is.na(che)]
che<-che[-1]
#number of coefficients
p<-length(che)
#number of runs
n<-p+1
# This trims unnecessary characters from coefficient names
cn<-names(che)
ccn<-gsub("[^A-Z]","",cn)
names(che)<-ccn
### End of Step 1 ####

###### Steps 2 and 3  #######
#calculate the pse statistic
ache<-abs(che)
s0<-1.5*median(ache)
selche<-ache<(2.5*s0)
psec=1.5*median(ache[selche])

#psec<-LenthPlot(modf,plt=FALSE)
#psec<-psec[2]
#calculate the gap statistic
gap<-gapstat(he,psec)
# checks to see if gap statistic exceeds 99th percentile
if (ncheck==16) test2=(gap>critg16[2]) else  test2=(gap>critg32[2])
##### End Step 2 and 3  #####

if (test2) {
cat("Final Outlier Report","\n")
cat("Standardized-Gap = ",gap, "Significant at 99th percentile","\n")
cat("   ","\n")
cat("    Corrrected Data Report  ","\n")
cat("Response  Corrected Response   Detect Outlier","\n")
cat(paste(format(DesY$y, width=8), format(DesYc$ycorr, width=13),
"           ", format(detect, width=10),"\n"),sep="")

# use LGB to test significance of effects calculated from corrected data

tce<-LGB(che)
  } else {
cat("Final Outlier Report","\n")
cat("No significant outlier detected in second pass","\n" )
# use LGB to test significance of effects calculated from corrected data
LGB(he)
cat("    ","\n")
         }

### End of second pass through the data #####
  }
}
# end of function Gaptest
 }
 


cleanEx()
nameEx("LGB")
### * LGB

flush(stderr()); flush(stdout())

### Name: LGB
### Title: Lawson, Grimshaw, Burt Test
### Aliases: LGB

### ** Examples

Beta<-c(.06,.25,-.01,.5,0,-.02,0,.14,.03,-.01,.02,.04,.02,.01,.02)
names(Beta)<-c("A","B","C","D","AB","AC","AD","BC","BD","CD","ABC","ABD","ACD","BCD","ABCD")
LGB(Beta,alpha=.05)


## The function is currently defined as
LGB<-function(Beta,alpha=.05,rpt=TRUE,plt=TRUE,pltl=TRUE) {
# function to compute the LGB statistic
siglev<-c(.1,.05,.025,.01)
df<-c(7,8,11,15,16, 17, 26, 31, 32, 35, 63, 127)
crittab<-matrix(c(1.265,1.196,1.161,1.122,1.110, 1.106, 1.072, 1.063, 1.060, 1.059, 1.037, 1.023,
1.534, 1.385, 1.291, 1.201, 1.186, 1.178, 1.115, 1.099, 1.093, 1.091, 1.056, 1.034,
1.889, 1.606, 1.449, 1.297, 1.274, 1.260, 1.165, 1.140, 1.130, 1.127, 1.074, 1.043,
2.506, 2.026, 1.74, 1.447, 1.421, 1.377, 1.232, 1.197, 1.185, 1.178, 1.096, 1.058),ncol=4,byrow=FALSE)
# get the critical value
colind<-which(siglev==alpha,arr.ind=TRUE)

if (length(colind)==0) {stop("this function works only when alpha= .1, .05, .025 or .01") }
rowind<-which(df==length(Beta),arr.ind=TRUE)
if (length(rowind)==0) {stop("this function works only for coefficent vectors of length 
7,8,11,15,16,26,31,32,35,63,or 127")}

critL<-crittab[rowind,colind]

# calculate Beta1, Beta2 and the Rn statistic
acj<-abs(Beta)
ranks<-rank(acj,ties.method="first")
s0<-1.5*median(acj)
p<-(ranks-.5)/length(Beta)
z<-qnorm((p+1)/2)
moda<-lm(acj~-1+z)
beta1<-moda$coef
sel<-acj<2.5*s0
modi<-lm(acj[sel]~-1+z[sel])
beta2<-modi$coef
Rn<-beta1/beta2
# finds prediction limits for values in sorted absolute Beta
pred<-beta2*z
n<-length(acj[sel])
df<-n-1
sig<-sqrt(sum(modi$residuals^2)/df)
se.pred<-sig*(1+1/n+(z^2)/sum(z[sel]^2))^.5
pred.lim<-pred+qt(.975,df)*se.pred
# gets significance indicators
sigi<-c(rep("no",length(Beta)))
sel2<-acj>pred.lim
sigi[sel2]<-"yes"
 if (plt) {
plot(z,acj,xlab="Half Normal Scores", ylab="Absoulute Effects")
lines(sort(z),sort(pred),lty=1)
         
 for (i in 1:length(Beta)) {
   if (sigi[i]=="yes") text(z[i],acj[i],names(Beta)[i],pos=1)
                           }
 if (pltl) {
lines(sort(z),sort(pred.lim),lty=3)
           }           
          } 
 if (rpt) {
cat("Effect Report","\n")
cat("  ","\n")
cat("Label     Half Effect    Sig(.05)","\n")
cat(paste(format(names(Beta), width=8), format(Beta, width=8),
 "      ",format(sigi, width=10),"\n"),sep="")
cat("  ","\n")
cat("Lawson, Grimshaw & Burt Rn Statistic = ",Rn,"\n")
cat("95th percentile of Rn = ",critL,"\n")
          }
return(sigi)
                    }




cleanEx()
nameEx("MPV")
### * MPV

flush(stderr()); flush(stdout())

### Name: MPV
### Title: mixture process variable experiment with mayonnaise
### Aliases: MPV
### Keywords: datasets

### ** Examples

data(MPV)



cleanEx()
nameEx("Naph")
### * Naph

flush(stderr()); flush(stdout())

### Name: Naph
### Title: Yields of naphthalene black
### Aliases: Naph
### Keywords: datasets

### ** Examples

data(Naph)



cleanEx()
nameEx("SPMPV")
### * SPMPV

flush(stderr()); flush(stdout())

### Name: SPMPV
### Title: Split-plot mixture process variable experiment with vinyl
### Aliases: SPMPV
### Keywords: datasets

### ** Examples

data(SPMPV)



cleanEx()
nameEx("Smotor")
### * Smotor

flush(stderr()); flush(stdout())

### Name: Smotor
### Title: Single array for starting motor experiment
### Aliases: Smotor
### Keywords: datasets

### ** Examples

data(Smotor)



cleanEx()
nameEx("Tet")
### * Tet

flush(stderr()); flush(stdout())

### Name: Tet
### Title: Tetracycline concentration in plasma
### Aliases: Tet
### Keywords: datasets

### ** Examples

data(Tet)



cleanEx()
nameEx("Treb")
### * Treb

flush(stderr()); flush(stdout())

### Name: Treb
### Title: Box-Behnken design for trebuchet experiment
### Aliases: Treb
### Keywords: datasets

### ** Examples

data(Treb)



cleanEx()
nameEx("Tukey1df")
### * Tukey1df

flush(stderr()); flush(stdout())

### Name: Tukey1df
### Title: Tukey's single degree of freedom test for additivity
### Aliases: Tukey1df

### ** Examples

Dilution<-rep(c(3,4,5),6)
Sample<-rep(c(1,2,3,4,5,6),each=3)
 y<-c(1.87506,1.38021,0.60206,1.74036,1.36173,0.90309,1.79934,1.25527,0.95424,2.02119,1.39794,1.000,1.79934,1.20412,0.60206,1.59106,1.25527,0.60206)
 virus<-data.frame(y=y, Sample=factor(Sample),Dilution=factor(Dilution))

Tukey1df(virus)


## The function is currently defined as
Tukey1df<-function(data) {
y<-data[,1]
Afactor<-data[,2]
Bfactor<-data[,3]
tst1<-is.factor(Afactor)
tst2<-is.factor(Bfactor)
tst3<-is.numeric(y)
if (tst1&tst2&tst3) {
a<-nlevels(Afactor)
b<-nlevels(Bfactor)
  }  else {stop("The first column of the data frame is the numeric response, the 2nd and 3rd columns should be coded as factors") }
tst4<-max(a,b)>2
tst5<-length(y)==a*b
if (tst4&tst5) {
ybb<-with(data, tapply(y, Bfactor, mean))
yba<-with(data, tapply(y, Afactor, mean))
sbb<-with(data, tapply(y, Bfactor, sum))
sba<-with(data, tapply(y, Afactor, sum))
ybardd<-mean(y)
CT<-(sum(y)^2)/(a*b)
ssA<-sum(sba^2/b)-CT
ssB<-sum(sbb^2/a)-CT
ssE<-sum(y^2)-CT-ssA-ssB
ybdj<-rep(ybb,6)
prody<-y*ybdj
sumprod<-tapply(prody,Afactor,sum)
leftsum<-sum(sumprod*yba)
ssAB<-(a*b*(leftsum-(ssA+ssB+a*b*ybardd^2)*ybardd)^2/(ssA*ssB))
ssR<-ssE-ssAB
F<-ssAB/(ssR/((a-1)*(b-1)-1))
Pval<-1-pf(1,((a-1)*(b-1)-1),F)
cat("Source           df     SS        MS        F     Pr>F","\n")
cat("A            ",paste(format(a-1, width=6)," ", format(round(ssA,4),justify="right"),"  ",format(round(ssA/(a-1),4), justify="right"),"\n"),sep="")
cat("B            ",paste(format(b-1, width=6)," ", format(round(ssB,4),justify="right"),"  ",format(round(ssB/(b-1),4), justify="right"),"\n"),sep="")
cat("Error        ",paste(format((b-1)*(a-1), width=6)," ", format(round(ssE,4),justify="right"),"  ",format(round(ssE/(a-1)*(b-1),4), justify="right"),"\n"),sep="")
cat("NonAdditivity",paste(format(1, width=6)," ", format(round(ssAB,4),justify="right"),"  ",format(round(ssAB,4),justify="right"),"  ",format(round(F,2),justify="right"),"  ",format(round(Pval,4),justify="right"),"\n"),sep="")
cat("Residual     ",paste(format((b-1)*(a-1)-1, width=6)," ", format(round(ssR,4),justify="right"),"  ",format(round(ssR/((a-1)*(b-1)-1),4), justify="right"),"\n"),sep="")
   } else {stop("This function only works for unreplicated 2-factor factorials with >2 levels for one of the factors")}
}




cleanEx()
nameEx("antifungal")
### * antifungal

flush(stderr()); flush(stdout())

### Name: antifungal
### Title: Two-period crossover study of antifungal agent
### Aliases: antifungal
### Keywords: datasets

### ** Examples

data(antifungal)



cleanEx()
nameEx("apple")
### * apple

flush(stderr()); flush(stdout())

### Name: apple
### Title: Confounded apple slice browning experiment
### Aliases: apple
### Keywords: datasets

### ** Examples

data(apple)



cleanEx()
nameEx("arso")
### * arso

flush(stderr()); flush(stdout())

### Name: arso
### Title: 2^{(7-3)} arsenic removal experiment
### Aliases: arso
### Keywords: datasets

### ** Examples

data(arso)



cleanEx()
nameEx("augm")
### * augm

flush(stderr()); flush(stdout())

### Name: augm
### Title: 2^{(7-3)} arsenic removal experiment augmented with mirror image
### Aliases: augm
### Keywords: datasets

### ** Examples

data(augm)



cleanEx()
nameEx("bha")
### * bha

flush(stderr()); flush(stdout())

### Name: bha
### Title: mouse liver enzyme experiment
### Aliases: bha
### Keywords: datasets

### ** Examples

data(bha)



cleanEx()
nameEx("bioequiv")
### * bioequiv

flush(stderr()); flush(stdout())

### Name: bioequiv
### Title: Extra-period crossover bioequivalence study
### Aliases: bioequiv
### Keywords: datasets

### ** Examples

data(bioequiv)



cleanEx()
nameEx("bioeqv")
### * bioeqv

flush(stderr()); flush(stdout())

### Name: bioeqv
### Title: Latin Square bioequivalence experiment
### Aliases: bioeqv
### Keywords: datasets

### ** Examples

data(bioeqv)



cleanEx()
nameEx("blood")
### * blood

flush(stderr()); flush(stdout())

### Name: blood
### Title: Variance component study of calcium in blood serum
### Aliases: blood
### Keywords: datasets

### ** Examples

data(blood)



cleanEx()
nameEx("bread")
### * bread

flush(stderr()); flush(stdout())

### Name: bread
### Title: Bread rise experiment data from Chapter 2
### Aliases: bread
### Keywords: datasets

### ** Examples

data(bread)



cleanEx()
nameEx("cake")
### * cake

flush(stderr()); flush(stdout())

### Name: cake
### Title: Split-Plot response surface for cake baking experiment
### Aliases: cake
### Keywords: datasets

### ** Examples

data(cake)



cleanEx()
nameEx("castf")
### * castf

flush(stderr()); flush(stdout())

### Name: castf
### Title: cast fatigue experiment
### Aliases: castf
### Keywords: datasets

### ** Examples

data(castf)



cleanEx()
nameEx("cement")
### * cement

flush(stderr()); flush(stdout())

### Name: cement
### Title: CCD design for cement workability experiment
### Aliases: cement
### Keywords: datasets

### ** Examples

data(cement)



cleanEx()
nameEx("chem")
### * chem

flush(stderr()); flush(stdout())

### Name: chem
### Title: Chemical process experiment data from Chapter 3
### Aliases: chem
### Keywords: datasets

### ** Examples

data(chem)



cleanEx()
nameEx("chipman")
### * chipman

flush(stderr()); flush(stdout())

### Name: chipman
### Title: Williams' crossover design for sprinting experiment
### Aliases: chipman
### Keywords: datasets

### ** Examples

data(chipman)



cleanEx()
nameEx("cont")
### * cont

flush(stderr()); flush(stdout())

### Name: cont
### Title: Control factor array and summary statistics for controller
###   circuit design experiment
### Aliases: cont
### Keywords: datasets

### ** Examples

data(cont)



cleanEx()
nameEx("cpipe")
### * cpipe

flush(stderr()); flush(stdout())

### Name: cpipe
### Title: Split-plot response surface for ceramic pipe experiment
### Aliases: cpipe
### Keywords: datasets

### ** Examples

data(cpipe)



cleanEx()
nameEx("culture")
### * culture

flush(stderr()); flush(stdout())

### Name: culture
### Title: paecilomyces variotii culture experiment
### Aliases: culture
### Keywords: datasets

### ** Examples

data(culture)



cleanEx()
nameEx("daewr-package")
### * daewr-package

flush(stderr()); flush(stdout())

### Name: daewr-package
### Title: Data frames and functions for Design and Analysis of Experiments
###   with R
### Aliases: daewr-package daewr
### Keywords: package

### ** Examples

Fcrit(.05,2,15)
Fpower(0.05,2,15,6.428)
BIBsize(6,3)



cleanEx()
nameEx("dairy")
### * dairy

flush(stderr()); flush(stdout())

### Name: dairy
### Title: Repeated measures study with dairy cow diets
### Aliases: dairy
### Keywords: datasets

### ** Examples

data(dairy)



cleanEx()
nameEx("drug")
### * drug

flush(stderr()); flush(stdout())

### Name: drug
### Title: Data from rat behavior experiment in Chapter 4
### Aliases: drug
### Keywords: datasets

### ** Examples

data(drug)



cleanEx()
nameEx("eptaxr")
### * eptaxr

flush(stderr()); flush(stdout())

### Name: eptaxr
### Title: Single array and raw response for silicon layer growth
###   experiment
### Aliases: eptaxr
### Keywords: datasets

### ** Examples

data(eptaxr)



cleanEx()
nameEx("eptaxs2")
### * eptaxs2

flush(stderr()); flush(stdout())

### Name: eptaxs2
### Title: Control array and variance of response for silicon layer growth
###   experiment
### Aliases: eptaxs2
### Keywords: datasets

### ** Examples

data(eptaxs2)



cleanEx()
nameEx("eptaxyb")
### * eptaxyb

flush(stderr()); flush(stdout())

### Name: eptaxyb
### Title: Control array and mean response for silicon layer growth
###   experiment
### Aliases: eptaxyb
### Keywords: datasets

### ** Examples

data(eptaxyb)



cleanEx()
nameEx("fullnormal")
### * fullnormal

flush(stderr()); flush(stdout())

### Name: fullnormal
### Title: Nornal Plot of Effects
### Aliases: fullnormal

### ** Examples

data(plasma)
sol<-lm(y~A*B*C*D*E,data=plasma)
# get whole plot effects and split plot effects
effects<-coef(sol)
effects<-effects[c(2:32)]
Wpeffects<-effects[c(1:4, 6:11, 16:19, 26)]
Speffects<-effects[c(5,12:15,20:25,27:31)]
#make separate normal plots
fullnormal(Wpeffects,names(Wpeffects),alpha=.10)
fullnormal(Speffects,names(Speffects),alpha=.05)
# make normal plot of all effects
fullnormal(effects,names(effects),alpha=.01,refline=FALSE)



## The function is currently defined as
fullnormal<-function(effects,labs,alpha=.05,refline="TRUE") {
crit<-LenthPlot(effects,alpha=alpha,plt=FALSE)["ME"]
names<-names(effects)
names<-gsub(':','',names)
names<-gsub('1','',names)
le<-length(effects)
 for (i in 1:le) {
     logc<-(abs(effects[i])<=crit)
     if (logc) {names[i]<-" "}
                  }
qqnorm(effects, ylab="Estimated Effects", xlab="Normal Scores")
x<-qqnorm(effects,plot=FALSE)
zscr<-(x$x)
# Splits effects into positive and negative for labeling
effp<-effects[zscr>0]
zp<-zscr[zscr>0]
namep<-names[zscr>0]
effn<-effects[zscr<0]
zn<-zscr[zscr<0]
namen<-names[zscr<0]
text(zp,effp,namep,pos=1) 
text(zn,effn,namen,pos=3)  
# calculate pse statistic
ahe<-abs(effects)
s0<-1.5*median(ahe)
selhe<-ahe<(2.5*s0)
pse=1.5*median(ahe[selhe])
if (refline) {
# add reference line to plot
abline(0,pse)
  }

}





cleanEx()
nameEx("gagerr")
### * gagerr

flush(stderr()); flush(stdout())

### Name: gagerr
### Title: Gauge R&R Study
### Aliases: gagerr
### Keywords: datasets

### ** Examples

data(gagerr)



cleanEx()
nameEx("gapstat")
### * gapstat

flush(stderr()); flush(stdout())

### Name: gapstat
### Title: Gap Statistic
### Aliases: gapstat

### ** Examples

Beta<-c(.06,.25,-.01,.5,0,-.02,0,.14,.03,-.01,.02,.04,.02,.01,.02)
names(Beta)<-c("A","B","C","D","AB","AC","AD","BC","BD","CD","ABC","ABD","ACD","BCD","ABCD")
gapstat(Beta,.06)

## The function is currently defined as
gapstat<-function(beta,pse) {
# computes the standardized gap score
p<-length(beta)
psehe<-pse
# gets positive coefficients
sel<-beta >= 0
betap<-beta[sel]
# sorts positive elements
betap<-sort(betap)
# gets Beta_s
betas<-betap[1]
#gets negative coefficients
sel<-beta < 0
betan<-beta[sel]
nn<-length(betan)
# sorts negative coefficients
betan<-sort(betan)
#gets Beta_L
betal<-betan[nn]
# gets Z_L and Z_S
zl<-qnorm((nn-.375)/(p+.25))
zs<-qnorm((nn+1-.375)/(p+.25))
# calculates gap statistic
gap<-((betas-betal)/psehe)/(zs-zl)
return(gap)
                      }





cleanEx()
nameEx("gear")
### * gear

flush(stderr()); flush(stdout())

### Name: gear
### Title: Unreplicated split-plot fractional-factorial experiment on
###   geometric distortion of drive gears
### Aliases: gear
### Keywords: datasets

### ** Examples

data(gear)



cleanEx()
nameEx("hardwood")
### * hardwood

flush(stderr()); flush(stdout())

### Name: hardwood
### Title: low grade hardwood conjoint study
### Aliases: hardwood
### Keywords: datasets

### ** Examples

data(hardwood)



cleanEx()
nameEx("inject")
### * inject

flush(stderr()); flush(stdout())

### Name: inject
### Title: Single array for injection molding experiment
### Aliases: inject
### Keywords: datasets

### ** Examples

data(inject)



cleanEx()
nameEx("mod")
### * mod

flush(stderr()); flush(stdout())

### Name: mod
### Title: Mod function
### Aliases: mod

### ** Examples

mod(5,3)
## The function is currently defined as
mod<-function(a,b)
{a-b*floor(a/b)}






cleanEx()
nameEx("pastry")
### * pastry

flush(stderr()); flush(stdout())

### Name: pastry
### Title: Blocked response surface design for pastry dough experiment
### Aliases: pastry
### Keywords: datasets

### ** Examples

data(pastry)



cleanEx()
nameEx("pest")
### * pest

flush(stderr()); flush(stdout())

### Name: pest
### Title: Pesticide formulation experiment
### Aliases: pest
### Keywords: datasets

### ** Examples

data(pest)



cleanEx()
nameEx("pesticide")
### * pesticide

flush(stderr()); flush(stdout())

### Name: pesticide
### Title: pesticide application experiment
### Aliases: pesticide
### Keywords: datasets

### ** Examples

data(pesticide)



cleanEx()
nameEx("plasma")
### * plasma

flush(stderr()); flush(stdout())

### Name: plasma
### Title: Unreplicated split-plot 2^5 experiment on plasma treatment of
###   paper
### Aliases: plasma
### Keywords: datasets

### ** Examples

data(plasma)



cleanEx()
nameEx("polvdat")
### * polvdat

flush(stderr()); flush(stdout())

### Name: polvdat
### Title: Polvoron mixture experiment
### Aliases: polvdat
### Keywords: datasets

### ** Examples

data(polvdat)



cleanEx()
nameEx("polymer")
### * polymer

flush(stderr()); flush(stdout())

### Name: polymer
### Title: polymerization strength variability study
### Aliases: polymer
### Keywords: datasets

### ** Examples

data(polymer)



cleanEx()
nameEx("prodstd")
### * prodstd

flush(stderr()); flush(stdout())

### Name: prodstd
### Title: Complete control factor array and noise factor array for
###   connector experiment
### Aliases: prodstd
### Keywords: datasets

### ** Examples

data(prodstd)



cleanEx()
nameEx("qsar")
### * qsar

flush(stderr()); flush(stdout())

### Name: qsar
### Title: Library of substituted hydroxyphenylurea compounds
### Aliases: qsar
### Keywords: datasets

### ** Examples

data(qsar)



cleanEx()
nameEx("rcb")
### * rcb

flush(stderr()); flush(stdout())

### Name: rcb
### Title: generalized RCB golf driving experiment
### Aliases: rcb
### Keywords: datasets

### ** Examples

data(rcb)



cleanEx()
nameEx("residue")
### * residue

flush(stderr()); flush(stdout())

### Name: residue
### Title: Herbicide degradation experiment
### Aliases: residue
### Keywords: datasets

### ** Examples

data(residue)



cleanEx()
nameEx("rubber")
### * rubber

flush(stderr()); flush(stdout())

### Name: rubber
### Title: Rubber Elasticity data
### Aliases: rubber
### Keywords: datasets

### ** Examples

data(rubber)



cleanEx()
nameEx("sausage")
### * sausage

flush(stderr()); flush(stdout())

### Name: sausage
### Title: Split-plot experiment on sausage casing with RCB in whole plot
### Aliases: sausage
### Keywords: datasets

### ** Examples

data(sausage)



cleanEx()
nameEx("soup")
### * soup

flush(stderr()); flush(stdout())

### Name: soup
### Title: dry mix soup experiment
### Aliases: soup
### Keywords: datasets

### ** Examples

data(soup)



cleanEx()
nameEx("soupmx")
### * soupmx

flush(stderr()); flush(stdout())

### Name: soupmx
### Title: dry soup mix variance component study
### Aliases: soupmx
### Keywords: datasets

### ** Examples

data(soupmx)



cleanEx()
nameEx("splitPdes")
### * splitPdes

flush(stderr()); flush(stdout())

### Name: splitPdes
### Title: Split-plot cookie baking experiment
### Aliases: splitPdes
### Keywords: datasets

### ** Examples

data(splitPdes)



cleanEx()
nameEx("strung")
### * strung

flush(stderr()); flush(stdout())

### Name: strung
### Title: Repeated measures study with dairy cow diets
### Aliases: strung
### Keywords: datasets

### ** Examples

data(strung)



cleanEx()
nameEx("strungtile")
### * strungtile

flush(stderr()); flush(stdout())

### Name: strungtile
### Title: Strung out control factor array and raw response data for Ina
###   tile experiment
### Aliases: strungtile
### Keywords: datasets

### ** Examples

data(strungtile)



cleanEx()
nameEx("sugarbeet")
### * sugarbeet

flush(stderr()); flush(stdout())

### Name: sugarbeet
### Title: Sugarbeet data from Chapter 2
### Aliases: sugarbeet
### Keywords: datasets

### ** Examples

data(sugarbeet)



cleanEx()
nameEx("taste")
### * taste

flush(stderr()); flush(stdout())

### Name: taste
### Title: taste test panel experiment
### Aliases: taste
### Keywords: datasets

### ** Examples

data(taste)



cleanEx()
nameEx("teach")
### * teach

flush(stderr()); flush(stdout())

### Name: teach
### Title: Teaching experiment data from Chapter 2
### Aliases: teach
### Keywords: datasets

### ** Examples

data(teach)



cleanEx()
nameEx("tile")
### * tile

flush(stderr()); flush(stdout())

### Name: tile
### Title: Control factor array and summary statistics for Ina tile
###   experiment
### Aliases: tile
### Keywords: datasets

### ** Examples

data(tile)



cleanEx()
nameEx("vci")
### * vci

flush(stderr()); flush(stdout())

### Name: vci
### Title: confidence limits for method of moments estimators of variance
###   components
### Aliases: vci

### ** Examples

vci(.90,.05,.014852,2,.05,.026885,18)
## The function is currently defined as
vci<-function(confl,c1,ms1,nu1,c2,ms2,nu2){
  delta<-c1*ms1-c2*ms2
  alpha<-1-confl
  Falpha1<-qf(confl,nu1,10000000)
  Falpha12<-qf(confl,nu1,nu2)
  Fconf2<-qf(alpha,nu2,10000000)
  Fconf12<-qf(alpha,nu1,nu2)
  Falpha2<-qf(confl,nu2,10000000)
  Fconf1<-qf(alpha,nu1,10000000)
  Fconf12<-qf(alpha,nu1,nu2)
  G1<-1-(1/Falpha1)
  H2<-(1/Fconf2)-1
  G12<-((Falpha12-1)**2-G1**2*Falpha12**2-H2**2)/Falpha12
  VL<-G1**2*c1**2*ms1**2+H2**2*c2**2*ms2**2+G12*c1*c2*ms1*ms2
  H1<-(1/Fconf1)-1
  G2<-1-(1/Falpha2)
  H12<-((1-Fconf12)**2-H1**2*Fconf12**2-G2**2)/Fconf12
  VU<-H1**2*c1**2*ms1**2+G2**2*c2**2*ms2**2
  L<-delta-sqrt(VL)
  U<-delta+sqrt(VU)
  cat("delta=",delta," Lower Limit=",L," Upper Limit=",U,"\n")
}




cleanEx()
nameEx("virus")
### * virus

flush(stderr()); flush(stdout())

### Name: virus
### Title: Assay of Viral Contamination experiment data from Chapter 3
### Aliases: virus
### Keywords: datasets

### ** Examples

data(virus)



cleanEx()
nameEx("volt")
### * volt

flush(stderr()); flush(stdout())

### Name: volt
### Title: Volt meter experiment data from Chapter 3
### Aliases: volt
### Keywords: datasets

### ** Examples

data(volt)



cleanEx()
nameEx("web")
### * web

flush(stderr()); flush(stdout())

### Name: web
### Title: Web page design experiment data from Chapter 3
### Aliases: web
### Keywords: datasets

### ** Examples

data(web)



### * <FOOTER>
###
cat("Time elapsed: ", proc.time() - get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
