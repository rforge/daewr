pkgname <- "mixexp"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('mixexp')

assign(".oldSearch", search(), pos = 'CheckExEnv')
cleanEx()
nameEx("DesignPoints")
### * DesignPoints

flush(stderr()); flush(stdout())

### Name: DesignPoints
### Title: This function plots design points and or constraints in the
###   simplex mixture space.
### Aliases: DesignPoints

### ** Examples

dat<-SCD(3)
DesignPoints(des=dat)

x1<-c(1,0,0,.5,.5, 0,.33333)
x2<-c(0,1,0,.5,0,.5,.33333)
x3<-c(0,0,1,0,.5,.5,.33333)
DesignPoints(x=x3,y=x2,z=x1)

dat<-data.frame(x1,x2,x3)
DesignPoints(des=dat)

DesignPoints(x1lower=0,x1upper=.8,x2lower=.10,x2upper=.95,x3lower=.05,x3upper=.50)  



cleanEx()
nameEx("Effplot")
### * Effplot

flush(stderr()); flush(stdout())

### Name: Effplot
### Title: This function creates mixture effect plots
### Aliases: Effplot

### ** Examples





#Example from Li, Tolley, Lee(2010) response is perm
x1<-c(.572,.358,.286,.286,.286,.143,.357)
x2<-c(.214,.428,.500,.357,.214,.500,.500)
x3<-c(.214,.214,.214,.357,.500,.357,.143)
y<-c(7.7,18.4,24.2,9.8,5.9,23.0,19.4)
des<-data.frame(x1,x2,x3,y)
Effplot(des,2,2)


#Example from Snee, Marquart(1976)
x1<-c(.1,.1,.1,.15,.1,.1,.1,.4,.35,.30,.1,.45,.45,.45,.45,.45,.259,.259,.259,.259)
x2<-c(.5,.05,.5,.05,.05,.5,.05,.05,.05,.5,.5,.05,.2,.15,.25,.1,.222,.222,.222,.222)
x3<-c(0,0,0,0,.1,.1,.1,.1,.1,0,.1,0,0,0,.1,.1,.05,.05,.05,.05)
x4<-c(0,0,.1,.1,0,.1,.1,.1,.1,0,0,0,.1,.1,0,0,.05,.05,.05,.05)
x5<-c(.1,.55,.1,.6,.55,.1,.55,.1,.1,.1,.2,.45,.1,.1,.1,.1,.244,.244,.244,.244)
x6<-c(.2,.2,.2,.05,.2,.05,.05,.2,.2,.05,.05,.05,.05,.2,.05,.2,.125,.125,.125,.125)
x7<-c(.05,.05,0,.05,0,0,0,.05,.05,0,.05,0,.05,0,.05,0,.025,.025,.025,.025)
x8<-c(.05,.05,0,0,0,.05,.05,0,.05,.05,0,0,.05,0,0,.05,.025,.025,.025,.025)
y<-c(30,113,17,94,89,18,90,20,21,15,28,48,18,7,16,19,38,30,35,40)
des<-data.frame(x1,x2,x3,x4,x5,x6,x7,x8,y)
Effplot(des,1,2)


# Weed control example from Lawson & Erjavec
x1<-c(1,0,0,.5,.5,0,.33333,.33333,.33333)
x2<-c(0,1,0,.5,0,.5,.33333,.33333,.33333)
x3<-c(0,0,1,0,.5,.5,.33333,.33333,.33333)
y<-c(73,68,80,77,86,75,92,93,88)
des<-data.frame(x1,x2,x3,y)
Effplot(des,3)



# Polvoron Example from Lawson
des<-Xvert(x1=c(0,.8),x2=c(.10,.95),x3=c(.05,.50),ndm=1)
dat<-as.matrix(des)
# remove the edge centroid at the top
dat<-dat[c(1:6,8:11), ]
# add two more centroids
dat<-rbind(dat,dat[10, ],dat[10,])
# response vector
y<-c(5.75,3.69,5.33,5.68,3.85,3.83,5.88,5.87,5.23,6.54,6.82,6.41)
# make the data frame for plotting
des<-data.frame(dat[,1:3],y)
Effplot(des,3)





cleanEx()
nameEx("MixturePlot")
### * MixturePlot

flush(stderr()); flush(stdout())

### Name: MixturePlot
### Title: This function makes contour plots in the simplex mixture space.
### Aliases: MixturePlot

### ** Examples


##Usage and Examples - Example from page 458 DAE with SAS
dat = data.frame(
          "x1"=c(1,.8,.6,.5,.5,.33333,.3,.3,.1,.1,0,0,0),
          "x2"=c(0,.1,.2,0,.5,.33333,.2,.5,.1,.8,0,.5,1),
          "x3"=c(0,.1,.2,.5,0,.33333,.5,.2,.8,.1,1.0,.5,0),
          "y"=c(48.7,49.5,50.2,52.8,49.3,51.1,52.7,50.3,60.7,49.9,64.9,53.5,50.6) 
                 )
MixturePlot(dat$x3,dat$x2,dat$x1,dat$y, x3lab="Fraction x3",
  x2lab="Fraction x2", x1lab="Fraction x1", corner.labs=c("x3","x2","x1"),
  constrts=FALSE,contrs=TRUE,cols=TRUE, mod=2,n.breaks=9)

# Weed control example from Lawson & Erjavec
x1<-c(1,0,0,.5,.5,0,.33333,.33333,.33333)
x2<-c(0,1,0,.5,0,.5,.33333,.33333,.33333)
x3<-c(0,0,1,0,.5,.5,.33333,.33333,.33333)
y<-c(73,68,80,77,86,75,92,93,88)
des<-data.frame(x1,x2,x3,y)
MixturePlot(des=des,x3lab="Fraction C",x2lab="Fraction B",
   x1lab="Fraction A",corner.labs=c("C","B","A"),mod=3,n.breaks=5,cols=TRUE)




cleanEx()
nameEx("SCD")
### * SCD

flush(stderr()); flush(stdout())

### Name: SCD
### Title: This function creates simplex centroid mixture designs
### Aliases: SCD

### ** Examples

SCD(3)

des<-SCD(5)

des<-SCD(12)




cleanEx()
nameEx("SLD")
### * SLD

flush(stderr()); flush(stdout())

### Name: SLD
### Title: This function creates simplex lattice mixture designs
### Aliases: SLD

### ** Examples

des<-SLD(3,2)

des<-SLD(4,3)



cleanEx()
nameEx("Xvert")
### * Xvert

flush(stderr()); flush(stdout())

### Name: Xvert
### Title: This function creates an extreme vertices design in a
###   constrained mixture space.
### Aliases: Xvert

### ** Examples


# Polvoron Example from Lawson
des<-Xvert(x1=c(0,.8),x2=c(.10,.95),x3=c(.05,.50),ndm=1)

# Exercise 11.3 DAE with SAS
Xvert(x1=c(.2,.8),x2=c(.1,.4),x3=c(.1,.5))

#Snee Marquardt(1976) example
Xvert(x1=c(.1,.45),x2=c(.05,.50),x3=c(0,.10),x4=c(0,.4),x5=c(.1,.6),x6=c(.05,.2),
      x7=c(0,.05),x8=c(0,.05),ndm=0)




cleanEx()
nameEx("crvtave")
### * crvtave

flush(stderr()); flush(stdout())

### Name: crvtave
### Title: This function creates an extreme vertices design
### Aliases: crvtave

### ** Examples


data(conmx)
crvtave(1,conmx)




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
