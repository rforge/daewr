pkgname <- "daewr"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('daewr')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("Altscreen")
### * Altscreen

flush(stderr()); flush(stdout())

### Name: Altscreen
### Title: Alternate 16 run screening designs
### Aliases: Altscreen
### Keywords: datagen

### ** Examples

Altscreen(6)
Altscreen(6, randomize=TRUE)
     


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
### Keywords: datagen

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

    #cat("lambda=",lambda)
    }
  cat("Posible BIB design with b=",b," and r=",r," lambda=",lambda,"\n") 
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
nameEx("DefScreen")
### * DefScreen

flush(stderr()); flush(stdout())

### Name: DefScreen
### Title: Definitive Screening Designs
### Aliases: DefScreen
### Keywords: datagen

### ** Examples

DefScreen(m=8,c=2)
DefScreen(12)
DefScreen(m=4,c=4, randomize=TRUE)
     


cleanEx()
nameEx("EEw1s1")
### * EEw1s1

flush(stderr()); flush(stdout())

### Name: EEw1s1
### Title: D-efficient Estimation Equivalent Response Surface Designs
### Aliases: EEw1s1
### Keywords: datagen

### ** Examples

EEw1s1()
EEw1s1('EE8R4WP')
EEw1s1('EE8R4WP', randomize=TRUE)
     


cleanEx()
nameEx("EEw1s2")
### * EEw1s2

flush(stderr()); flush(stdout())

### Name: EEw1s2
### Title: D-efficient Estimation Equivalent Response Surface Designs
### Aliases: EEw1s2
### Keywords: datagen

### ** Examples

EEw1s2( )
EEw1s2('EE12R4WP')
EEw1s2('EE12R4WP', randomize=TRUE)
EEw1s2('EE12R6WP')
EEw1s2('EE12R6WP', randomize=TRUE)
EEw1s2('EE14R7WP')
EEw1s2('EE14R7WP', randomize=TRUE)
EEw1s2('EE15R5WP')
EEw1s2('EE15R5WP', randomize=TRUE)
EEw1s2('EE16R4WP')
EEw1s2('EE16R4WP', randomize=TRUE)
EEw1s2('EE18R6WP')
EEw1s2('EE18R6WP', randomize=TRUE)
EEw1s2('EE20R4WP')
EEw1s2('EE20R4WP', randomize=TRUE)
EEw1s2('EE20R5WP')
EEw1s2('EE20R5WP', randomize=TRUE)
EEw1s2('EE21R7WP')
EEw1s2('EE21R7WP', randomize=TRUE)
EEw1s2('EE24R4WP')
EEw1s2('EE24R4WP', randomize=TRUE)
EEw1s2('EE24R6WP')
EEw1s2('EE24R6WP', randomize=TRUE)
EEw1s2('EE25R5WP')
EEw1s2('EE25R5WP', randomize=TRUE)
EEw1s2('EE28R7WP')
EEw1s2('EE28R7WP', randomize=TRUE)
EEw1s2('EE30R6WP')
EEw1s2('EE30R6WP', randomize=TRUE)
EEw1s2('EE30R5WP')
EEw1s2('EE30R5WP', randomize=TRUE)
EEw1s2('EE35R7WP')
EEw1s2('EE35R7WP', randomize=TRUE)
EEw1s2('EE36R6WP')
EEw1s2('EE36R6WP', randomize=TRUE)
EEw1s2('EE42R7WP')
EEw1s2('EE42R7WP', randomize=TRUE)
     


cleanEx()
nameEx("EEw1s3")
### * EEw1s3

flush(stderr()); flush(stdout())

### Name: EEw1s3
### Title: D-efficient Estimation Equivalent Response Surface Designs
### Aliases: EEw1s3
### Keywords: datagen

### ** Examples

EEw1s3()
EEw1s3('EE16R4WP')
EEw1s3('EE16R4WP', randomize=TRUE)
     


cleanEx()
nameEx("EEw2s1")
### * EEw2s1

flush(stderr()); flush(stdout())

### Name: EEw2s1
### Title: D-efficient Estimation Equivalent Response Surface Designs
### Aliases: EEw2s1
### Keywords: datagen

### ** Examples

EEw2s1()
EEw2s1('EE21R7WP')
EEw1s1('EE21R7WP', randomize=TRUE)
     


cleanEx()
nameEx("EEw2s2")
### * EEw2s2

flush(stderr()); flush(stdout())

### Name: EEw2s2
### Title: D-efficient Estimation Equivalent Response Surface Designs
### Aliases: EEw2s2
### Keywords: datagen

### ** Examples

EEw2s2()
EEw2s2('EE21R7WP')
EEw1s2('EE21R7WP', randomize=TRUE)
     


cleanEx()
nameEx("EEw2s3")
### * EEw2s3

flush(stderr()); flush(stdout())

### Name: EEw2s3
### Title: D-efficient Estimation Equivalent Response Surface Designs
### Aliases: EEw2s3
### Keywords: datagen

### ** Examples

EEw2s3()
EEw2s3('EE24R8WP')
EEw1s3('EE24R8WP', randomize=TRUE)
     


cleanEx()
nameEx("EEw3")
### * EEw3

flush(stderr()); flush(stdout())

### Name: EEw3
### Title: D-efficient Estimation Equivalent Response Surface Designs
### Aliases: EEw3
### Keywords: datagen

### ** Examples

EEw3()
EEw3('EE22R11WP')
EEw3('EE22R11WP', randomize=TRUE)
EEw3('EE48R12WP')
EEw3('EE48R12WP', randomize=TRUE)
     


cleanEx()
nameEx("Fcrit")
### * Fcrit

flush(stderr()); flush(stdout())

### Name: Fcrit
### Title: F-Distribution critical values
### Aliases: Fcrit
### Keywords: datagen

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
nameEx("Fpower1")
### * Fpower1

flush(stderr()); flush(stdout())

### Name: Fpower1
### Title: F-Distribution Power Calculation
### Aliases: Fpower1
### Keywords: datagen

### ** Examples

Fpower1(alpha=.05,nlev=3,nreps=4,Delta=3,sigma=sqrt(2.1))


rmin <-2 #smallest number of replicates considered
rmax <-6 # largest number of replicates considered
alpha <- rep(0.05, rmax - rmin +1) 
sigma <-rep(sqrt(2.1), rmax - rmin +1)
nreps <-c(rmin:rmax)
nlev <- rep(3,rmax - rmin +1)
nreps <- rmin:rmax
Delta <- rep(3,rmax - rmin +1)
power <- Fpower1(alpha,nlev,nreps,Delta,sigma)
data.frame(r=nreps,Power=power)


## The function is currently defined as
Fpower1<-function(alpha=NULL, nlev=NULL,nreps=NULL, Delta=NULL, sigma=NULL)
{
##### Power Calculation for one way ANOVA ###########
# Argument list
# alpha the significance level of the test
# nlev the number of levels of the factor 
# nreps the number of replicates in each level of the factor
# Delta the size of a practical difference in two cell means
# sigma the standard deviation of the experimental error
#####################################################
if (is.null(alpha)|is.null(nlev)|is.null(nreps)|is.null(Delta)|is.null(sigma))
  stop("you must supply alpha, nlev, nreps, Delta and sigma")
css<-(Delta^2)/2
nc<- (nreps*css)/(sigma^2)
df1<-nlev-1
df2<-(nreps-1)*nlev
power <- 1-pf(Fcrit(alpha,df1,df2),df1,df2,nc)
return(power)
}



cleanEx()
nameEx("Fpower2")
### * Fpower2

flush(stderr()); flush(stdout())

### Name: Fpower2
### Title: F-Distribution Power Calculation
### Aliases: Fpower2
### Keywords: datagen

### ** Examples

power <- Fpower2(.05, nlev = c(4,4), nreps=2, Delta= 1, sigma=.32)

rmin <- 2 # smallest number of replicates
rmax <- 4 # largest number of replicates
alpha <- .05
sigma <- .32
Delta <- 1.0
nlev <- c(4,4)
nreps <- c(rmin:rmax)
result <- Fpower2(alpha, nlev, nreps, Delta, sigma)
options(digits = 5)
result


## The function is currently defined as
Fpower2<-function(alpha=NULL, nlev=NULL,nreps=NULL, Delta=NULL, sigma=NULL)
{
##### Power Calculation for two way ANOVA ###########
# Argument list
# alpha the significance level of the test.
# nlev vector containing the number of levels of the factors. 
# nreps the number of replicates in each combination of factor levels.
# Delta the size of a practical difference in two marginal factor level means.
# sigma the standard deviation of the experimental error.
############################################################
if (is.null(alpha)|is.null(nlev)|is.null(nreps)|is.null(Delta)|is.null(sigma))
  stop("you must supply alpha, nlev, nreps, Delta and sigma")
if(length(nlev)<2)
  stop ("nlev must be a two component vecto containing levels of the 1st and 2nd factors")
a <- nlev[1]
b <- nlev[2]
cssb <- (Delta^2)/2
ncb <- a*(nreps*cssb)/(sigma^2)
cssa<-(Delta^2)/2
nca<- b*(nreps*cssa)/(sigma^2)
dfa<- a-1
dfb<- b-1
df2<-(nreps-1)*b*a
powera <- 1-pf(Fcrit(alpha,dfa,df2),dfa,df2,nca)
powerb <- 1-pf(Fcrit(alpha,dfb,df2),dfa,df2,nca)
result <-cbind(nreps,df2,powera,powerb)
}



cleanEx()
nameEx("Gaptest")
### * Gaptest

flush(stderr()); flush(stdout())

### Name: Gaptest
### Title: This function uses Daniel's Method to find an outlier in an
###   unreplicated 2^{(k-p)} design.
### Aliases: Gaptest
### Keywords: htest

### ** Examples

# Example from Box(1991)
data(BoxM)
Gaptest(BoxM)


## The function is currently defined as
function (DesY) 
{
    ncheck <- dim(DesY)
    ncheck <- ncheck[1]
    tcnd = TRUE
    if (ncheck == 8) {
        tcnd = FALSE
    }
    if (ncheck == 16) {
        tcnd = FALSE
    }
    if (ncheck == 32) {
        tcnd = FALSE
    }
    if (tcnd) {
        stop("This function only works for 8, 16, or 32 run designs", 
            "\n")
    }
    else {
        if (ncheck == 8) 
            ncheck = 16
        critg16 <- c(1.7884, 5.1009)
        critg32 <- c(1.7297, 5.8758)
        modf <- lm(y ~ (.)^4, x = TRUE, data = DesY)
        nbeta <- dim(DesY)
        nbeta <- nbeta[1]
        he <- modf$coef
        selcol <- which(!is.na(he))
        he <- he[selcol]
        he <- he[-1]
        p <- length(he)
        n <- p + 1
        cn1 <- names(he)
        ccn1 <- gsub("[^A-Z]", "", cn1)
        names(he) <- ccn1
        ahe <- abs(he)
        s0 <- 1.5 * median(ahe)
        selhe <- ahe < (2.5 * s0)
        pse = 1.5 * median(ahe[selhe])
        gap <- gapstat(he, pse)
        if (ncheck == 16) {
            test = (gap > critg16[1])
        }
        else {
            test = (gap > critg32[1])
        }
        if (test) {
            X <- modf$x
            X <- X[, selcol]
            X <- X[, -1]
            se <- as.matrix(sign(he), nrow = 1)
            sigef <- LGB(he, rpt = FALSE, plt = FALSE)
            for (i in 1:length(he)) {
                if (sigef[i] == "yes") {
                  se[i] = 0
                }
            }
            sp <- X %*% se
            asp <- abs(sp)
            oo <- max.col(t(asp))
            ae <- abs(he)
            sae <- sort(ae)
            nsmall <- round(length(he)/2)
            bias <- 2 * sum(sae[1:nsmall])
            y <- DesY$y
            ycorr <- DesY$y
            ycorr[oo] <- ycorr[oo] + (-1 * sign(sp[oo])) * bias
            detect <- c(rep("no", n))
            detect[oo] <- "yes"
            cat("Initial Outlier Report", "\n")
            cat("Standardized-Gap = ", gap, "Significant at 50th percentile", 
                "\n")
            DesYc <- cbind(DesY[, 1:(dim(DesY)[2] - 1)], ycorr)
            modf <- lm(ycorr ~ (.)^4, x = TRUE, data = DesYc)
            che <- modf$coef
            che <- che[!is.na(che)]
            che <- che[-1]
            p <- length(che)
            n <- p + 1
            cn <- names(che)
            ccn <- gsub("[^A-Z]", "", cn)
            names(che) <- ccn
            ache <- abs(che)
            s0 <- 1.5 * median(ache)
            selche <- ache < (2.5 * s0)
            psec = 1.5 * median(ache[selche])
            gap <- gapstat(he, psec)
            if (ncheck == 16) 
                test2 = (gap > critg16[2])
            else test2 = (gap > critg32[2])
            if (test2) {
                cat("Final Outlier Report", "\n")
                cat("Standardized-Gap = ", gap, "Significant at 99th percentile", 
                  "\n")
                cat("   ", "\n")
                cat("    Corrrected Data Report  ", "\n")
                cat("Response  Corrected Response   Detect Outlier", 
                  "\n")
                cat(paste(format(DesY$y, width = 8), format(DesYc$ycorr, 
                  width = 13), "           ", format(detect, 
                  width = 10), "\n"), sep = "")
                tce <- LGB(che)
            }
            else {
                cat("Final Outlier Report", "\n")
                cat("No significant outlier detected in second pass", 
                  "\n")
                LGB(he)
                cat("    ", "\n")
            }
        }
    }
  }



cleanEx()
nameEx("LGB")
### * LGB

flush(stderr()); flush(stdout())

### Name: LGB
### Title: This function uses the LGB Method to detect significant effects
###   in unreplicated fractional factorials.
### Aliases: LGB
### Keywords: htest

### ** Examples

data(chem)
modf<-lm(y~A*B*C*D,data=chem)
LGB(coef(modf)[-1],rpt=FALSE)


## The function is currently defined as
LGB <- function(Beta, alpha=.05,rpt=TRUE, plt=TRUE, pltl=TRUE) {
sigLGB<-LGBc(Beta,alpha,rpt,plt,pltl)
}



cleanEx()
nameEx("LGBc")
### * LGBc

flush(stderr()); flush(stdout())

### Name: LGBc
### Title: This function does the calculations for the LGB Method to detect
###   significant effects in unreplicated fractional factorials.
### Aliases: LGBc
### Keywords: htest

### ** Examples

data(chem)
modf<-lm(y~A*B*C*D,data=chem)
sig<-LGBc(coef(modf)[-1],rpt=FALSE)


## The function is currently defined as
function (Beta, alpha = 0.05, rpt = TRUE, plt = TRUE, pltl = TRUE) 
{
    siglev <- c(0.1, 0.05, 0.025, 0.01)
    df <- c(7, 8, 11, 15, 16, 17, 26, 31, 32, 35, 63, 127)
    crittab <- matrix(c(1.265, 1.196, 1.161, 1.122, 1.11, 1.106, 
        1.072, 1.063, 1.06, 1.059, 1.037, 1.023, 1.534, 1.385, 
        1.291, 1.201, 1.186, 1.178, 1.115, 1.099, 1.093, 1.091, 
        1.056, 1.034, 1.889, 1.606, 1.449, 1.297, 1.274, 1.26, 
        1.165, 1.14, 1.13, 1.127, 1.074, 1.043, 2.506, 2.026, 
        1.74, 1.447, 1.421, 1.377, 1.232, 1.197, 1.185, 1.178, 
        1.096, 1.058), ncol = 4, byrow = FALSE)
    colind <- which(siglev == alpha, arr.ind = TRUE)
    if (length(colind) == 0) {
        stop("this function works only when alpha= .1, .05, .025 or .01")
    }
    rowind <- which(df == length(Beta), arr.ind = TRUE)
    if (length(rowind) == 0) {
        stop("this function works only for coefficent vectors of 
		length 7,8,11,15,16,26,31,32,35,63,or 127")
    }
    critL <- crittab[rowind, colind]
    acj <- abs(Beta)
    ranks <- rank(acj, ties.method = "first")
    s0 <- 1.5 * median(acj)
    p <- (ranks - 0.5)/length(Beta)
    z <- qnorm((p + 1)/2)
    moda <- lm(acj ~ -1 + z)
    beta1 <- moda$coef
    sel <- acj < 2.5 * s0
    modi <- lm(acj[sel] ~ -1 + z[sel])
    beta2 <- modi$coef
    Rn <- beta1/beta2
    pred <- beta2 * z
    n <- length(acj[sel])
    df <- n - 1
    sig <- sqrt(sum(modi$residuals^2)/df)
    se.pred <- sig * (1 + 1/n + (z^2)/sum(z[sel]^2))^0.5
    pred.lim <- pred + qt(0.975, df) * se.pred
    sigi <- c(rep("no", length(Beta)))
    sel2 <- acj > pred.lim
    sigi[sel2] <- "yes"
    if (plt) {
        plot(z, acj, xlab = "Half Normal Scores", ylab = "Absoulute Effects")
        lines(sort(z), sort(pred), lty = 1)
        for (i in 1:length(Beta)) {
            if (sigi[i] == "yes") 
                text(z[i], acj[i], names(Beta)[i], pos = 1)
        }
        if (pltl) {
            lines(sort(z), sort(pred.lim), lty = 3)
        }
    }
    if (rpt) {
        cat("Effect Report", "\n")
        cat("  ", "\n")
        cat("Label     Half Effect    Sig(.05)", "\n")
        cat(paste(format(names(Beta), width = 8), format(Beta, 
            width = 8), "      ", format(sigi, width = 10), "\n"), 
            sep = "")
        cat("  ", "\n")
        cat("Lawson, Grimshaw & Burt Rn Statistic = ", Rn, "\n")
        cat("95th percentile of Rn = ", critL, "\n")
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
nameEx("ModelRobust")
### * ModelRobust

flush(stderr()); flush(stdout())

### Name: ModelRobust
### Title: Model Robust Factorial Designs
### Aliases: ModelRobust
### Keywords: datagen

### ** Examples

ModelRobust()
ModelRobust('MR8m4g3')
ModelRobust('MR8m4g3', randomize=TRUE)
     


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
nameEx("OptPB")
### * OptPB

flush(stderr()); flush(stdout())

### Name: OptPB
### Title: Optimum Plackett-Burman Designs
### Aliases: OptPB
### Keywords: datagen

### ** Examples

OptPB(12,8)
     


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
### Title: This function performs Tukey's single degree of freedom test for
###   interaction in an unreplicated two-factor design
### Aliases: Tukey1df
### Keywords: htest

### ** Examples

library(daewr)
data(virus)
Tukey1df(virus)

## The function is currently defined as
function (data) 
{
    y <- data[, 1]
    Afactor <- data[, 2]
    Bfactor <- data[, 3]
    tst1 <- is.factor(Afactor)
    tst2 <- is.factor(Bfactor)
    tst3 <- is.numeric(y)
    if (tst1 & tst2 & tst3) {
        a <- nlevels(Afactor)
        b <- nlevels(Bfactor)
    }
    else {
        stop("The first column of the data frame is the numeric response, 
		the 2nd and 3rd columns should be coded as factors")
    }
    tst4 <- max(a, b) > 2
    tst5 <- length(y) == a * b
    if (tst4 & tst5) {
        ybb <- with(data, tapply(y, Bfactor, mean))
        yba <- with(data, tapply(y, Afactor, mean))
        sbb <- with(data, tapply(y, Bfactor, sum))
        sba <- with(data, tapply(y, Afactor, sum))
        ybardd <- mean(y)
        CT <- (sum(y)^2)/(a * b)
        ssA <- sum(sba^2/b) - CT
        ssB <- sum(sbb^2/a) - CT
        ssE <- sum(y^2) - CT - ssA - ssB
        ybdj <- rep(ybb, 6)
        prody <- y * ybdj
        sumprod <- tapply(prody, Afactor, sum)
        leftsum <- sum(sumprod * yba)
        ssAB <- (a * b * (leftsum - (ssA + ssB + a * b * ybardd^2) * 
            ybardd)^2/(ssA * ssB))
        ssR <- ssE - ssAB
        F <- ssAB/(ssR/((a - 1) * (b - 1) - 1))
        Pval <- 1 - pf(1, ((a - 1) * (b - 1) - 1), F)
        cat("Source           df     SS        MS        F     Pr>F", 
            "\n")
        cat("A            ", paste(format(a - 1, width = 6), 
            " ", format(round(ssA, 4), justify = "right"), "  ", 
            format(round(ssA/(a - 1), 4), justify = "right"), 
            "\n"), sep = "")
        cat("B            ", paste(format(b - 1, width = 6), 
            " ", format(round(ssB, 4), justify = "right"), "  ", 
            format(round(ssB/(b - 1), 4), justify = "right"), 
            "\n"), sep = "")
        cat("Error        ", paste(format((b - 1) * (a - 1), 
            width = 6), " ", format(round(ssE, 4), justify = "right"), 
            "  ", format(round(ssE/(a - 1) * (b - 1), 4), justify = "right"), 
            "\n"), sep = "")
        cat("NonAdditivity", paste(format(1, width = 6), " ", 
            format(round(ssAB, 4), justify = "right"), "  ", 
            format(round(ssAB, 4), justify = "right"), "  ", 
            format(round(F, 2), justify = "right"), "  ", format(round(Pval, 
                4), justify = "right"), "\n"), sep = "")
        cat("Residual     ", paste(format((b - 1) * (a - 1) - 
            1, width = 6), " ", format(round(ssR, 4), justify = "right"), 
            "  ", format(round(ssR/((a - 1) * (b - 1) - 1), 4), 
                justify = "right"), "\n"), sep = "")
    }
    else {
        stop("This function only works for unreplicated 2-factor 
		factorials with >2 levels for one of the factors")
    }
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
nameEx("bstep")
### * bstep

flush(stderr()); flush(stdout())

### Name: bstep
### Title: This function performs Tukey's single degree of freedom test for
###   interaction in an unreplicated two-factor design
### Aliases: bstep
### Keywords: htest

### ** Examples


library(daewr)
des <- DefScreen( m = 8 )
pd<-c(5.35,4.4,12.91,3.79,4.15,14.05,11.4,4.29,3.56,11.4,10.09,5.9,9.54,4.53,3.919,8.1,5.35)
trm<-ihstep(pd,des)
trm<-fhstep(pd,des,trm)
trm<-fhstep(pd,des,trm)
trm<-fhstep(pd,des,trm)
trm<-bstep(pd,des,trm)
trm<-bstep(pd,des,trm)



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
nameEx("colormap")
### * colormap

flush(stderr()); flush(stdout())

### Name: colormap
### Title: This function makes a colormap of correlations in a design
###   matrix
### Aliases: colormap
### Keywords: hplot

### ** Examples

# color map of 2^(4-1) design
library(FrF2)
design <- FrF2(8, 4, randomize = FALSE)
colormap(design, mod=3)

# Makes color map for saturated 2^(7-4) design in Figure 6.14 p. 197
library(FrF2)
design <-FrF2( 8, 7)
colormap(design, mod=2)

# Makes colormap of an Alternate Screening Design 
library(daewr)
ascr<-Altscreen(7)
colormap(ascr, mod=2)

# Makes colormap of a Model Robust Design 
library(daewr)
MR16 <- ModelRobust('MR16m7g5', randomize = FALSE)
colormap(MR16, mod=2)
          
## The function is currently defined as
function(design, mod) {
##################### Inputs ###########################################
# design - a data frame containing columns of the numeric factor levels
# mod - the model for the color plot of correlations
#    1 = Linear model containing only the terms in the data frame
#    2 = Linear model plus two factor interactions
#    3 = Linear model plus 2 and 3 factor interactions
#    4 = Linear model plus 2, 3 and 4 factor interactions
########################################################################
y<-runif(nrow(design),0,1)
if(mod==1) {test <- model.matrix(lm(y~(.),data=design))}
if(mod==2) {test <- model.matrix(lm(y~(.)^2,data=design))}
if(mod==3) {test <- model.matrix(lm(y~(.)^3,data=design))}
if(mod==4) {test <- model.matrix(lm(y~(.)^4,data=design))}
names<-colnames(test)
names<-gsub(':','',names)
names<-gsub('1','',names)
colnames(test)<-names
cmas<-cor(test[,ncol(test):2])
cmas<-cmas[c((ncol(cmas)):1), ]
rgb.palette <- colorRampPalette(c("white", "black"), space = "rgb")
levelplot(cmas, main="Color map of correlations", xlab="", ylab="", col.regions=rgb.palette(120), cuts=100, at=seq(0,1,0.01),scales=list(x=list(rot=90)))                          }



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
Fpower1(alpha=.05,nlev=3,nreps=4,Delta=3,sigma=sqrt(2.1))
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
nameEx("fhstep")
### * fhstep

flush(stderr()); flush(stdout())

### Name: fhstep
### Title: This function performs Tukey's single degree of freedom test for
###   interaction in an unreplicated two-factor design
### Aliases: fhstep
### Keywords: htest

### ** Examples


library(daewr)
des <- DefScreen( m = 8 )
pd<-c(5.35,4.4,12.91,3.79,4.15,14.05,11.4,4.29,3.56,11.4,10.09,5.9,9.54,4.53,3.919,8.1,5.35)
trm<-ihstep(pd,des)
trm<-fhstep(pd,des,trm)
trm<-fhstep(pd,des,trm)
trm<-fhstep(pd,des,trm)




cleanEx()
nameEx("fullnormal")
### * fullnormal

flush(stderr()); flush(stdout())

### Name: fullnormal
### Title: This function makes a full normal plot of the elements of the
###   vector called effects
### Aliases: fullnormal
### Keywords: hplot

### ** Examples

# Example Separate Normal plots of whole and split plot effects from an unreplicated split-plot 
data(plasma)
sol<-lm(y~A*B*C*D*E,data=plasma)
summary(sol)
# get whole plot effects and split plot effects
effects<-coef(sol)
effects<-effects[c(2:32)]
Wpeffects<-effects[c(1:4, 6:11, 16:19, 26)]
Speffects<-effects[c(5,12:15,20:25,27:31)]

#make separate normal plots
library(BsMD)
fullnormal(Wpeffects,names(Wpeffects),alpha=.10)
fullnormal(Speffects,names(Speffects),alpha=.05)


## The function is currently defined as
function (effects, labs, alpha = 0.05, refline = "TRUE") 
{
    crit <- LenthPlot(effects, alpha = alpha, plt = FALSE)["ME"]
    names <- names(effects)
    names <- gsub(":", "", names)
    names <- gsub("1", "", names)
    le <- length(effects)
    for (i in 1:le) {
        logc <- (abs(effects[i]) <= crit)
        if (logc) {
            names[i] <- " "
        }
    }
    qqnorm(effects, ylab = "Estimated Effects", xlab = "Normal Scores")
    x <- qqnorm(effects, plot = FALSE)
    zscr <- (x$x)
    effp <- effects[zscr > 0]
    zp <- zscr[zscr > 0]
    namep <- names[zscr > 0]
    effn <- effects[zscr < 0]
    zn <- zscr[zscr < 0]
    namen <- names[zscr < 0]
    text(zp, effp, namep, pos = 1)
    text(zn, effn, namen, pos = 3)
    ahe <- abs(effects)
    s0 <- 1.5 * median(ahe)
    selhe <- ahe < (2.5 * s0)
    pse = 1.5 * median(ahe[selhe])
    if (refline) {
        abline(0, pse)
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
### Title: This function computes the gap statistic which is used to test
###   for an outlier using Daniels method
### Aliases: gapstat
### Keywords: htest

### ** Examples


## The function is currently defined as
function (beta, pse) 
{
    p <- length(beta)
    psehe <- pse
    sel <- beta >= 0
    betap <- beta[sel]
    betap <- sort(betap)
    betas <- betap[1]
    sel <- beta < 0
    betan <- beta[sel]
    nn <- length(betan)
    betan <- sort(betan)
    betal <- betan[nn]
    zl <- qnorm((nn - 0.375)/(p + 0.25))
    zs <- qnorm((nn + 1 - 0.375)/(p + 0.25))
    gap <- ((betas - betal)/psehe)/(zs - zl)
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
nameEx("halfnorm")
### * halfnorm

flush(stderr()); flush(stdout())

### Name: halfnorm
### Title: This function makes a half normal plot of the elements of the
###   vector called effects
### Aliases: halfnorm
### Keywords: hplot

### ** Examples

# Example Separate Normal plots of whole and split plot effects from an unreplicated split-plot 
data(plasma)
sol<-lm(y~A*B*C*D*E,data=plasma)
# get whole plot effects and split plot effects
effects<-coef(sol)
effects<-effects[c(2:32)]
Wpeffects<-effects[c(1:4, 6:11, 16:19, 26)]
Speffects<-effects[c(5,12:15,20:25,27:31)]

#make separate half normal plots
library(BsMD)
halfnorm(Wpeffects,names(Wpeffects),alpha=.10)
halfnorm(Speffects,names(Speffects),alpha=.05)
        


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
nameEx("ihstep")
### * ihstep

flush(stderr()); flush(stdout())

### Name: ihstep
### Title: This function performs Tukey's single degree of freedom test for
###   interaction in an unreplicated two-factor design
### Aliases: ihstep
### Keywords: htest

### ** Examples


library(daewr)
des <- DefScreen( m = 8 )
pd<-c(5.35,4.4,12.91,3.79,4.15,14.05,11.4,4.29,3.56,11.4,10.09,5.9,9.54,4.53,3.919,8.1,5.35)
trm<-ihstep(pd,des)




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
### Keywords: datagen

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
### Keywords: htest

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
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
