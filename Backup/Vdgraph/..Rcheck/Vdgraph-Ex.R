pkgname <- "Vdgraph"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('Vdgraph')

assign(".oldSearch", search(), pos = 'CheckExEnv')
cleanEx()
nameEx("Compare2Vdg")
### * Compare2Vdg

flush(stderr()); flush(stdout())

### Name: Compare2Vdg
### Title: this function compares Variance Dispersion Graph of two response
###   surface designs with the same number of factors on the same scale
### Aliases: Compare2Vdg

### ** Examples


data(SCDH5)
data(SCDDL5)
Compare2Vdg("Hartley's Small Composite-5 fac",SCDH5,"Draper and Lin's Small Composite-5 fac",SCDDL5)


library(rsm)
BB.des3<-bbd(3)
CCD.des3<-ccd(3, n0=5)
CCD.des3<-CCD.des3[,2:4]
Compare2Vdg("Box Behnken 3 Factors",BB.des3,"Central Composite 3 Factors",CCD.des3)





cleanEx()
nameEx("Vdgraph")
### * Vdgraph

flush(stderr()); flush(stdout())

### Name: Vdgraph
### Title: this function makes a Variance Dispersion Graph of a response
###   surface design
### Aliases: Vdgraph

### ** Examples


data(D310)
Vdgraph(D310)

library(rsm)
bbhrsm<-bbd(4,n0=3)
des<-bbhrsm[,2:5]
des<-as.matrix(des)
Vdgraph(des)

dat<-ccd(~A+B,generators=B~A,n0=c(1,1),alpha=1.212,wbreps=1)
des2<-dat[,2:3]
Vdgraph(des2)




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
