# Multivariate Regression Trees (MRT) in R:
#https://sites.ualberta.ca/~lkgray/uploads/7/3/6/2/7362679/slides-cart.pdf
#https://wiki.qcbs.ca/r_workshop10

#devtools::install_github("cran/mvpart")
#devtools::install_github("cran/MVPARTwrap")


library(mvpart)
library(MVPARTwrap)#MRT
library(labdsv) #inaval
#Prepare the data: remove “distance from source”
env <- subset(Cover.data.gruposfuncionales, select = c("reef.area","Depth"))
spe.hel <- Cover.data.gruposfuncionales[,-(0:20)]


# Create the regression tree
doubs.mrt <- mvpart(as.matrix(spe.hel) ~. ,env,legend=T, margin=0, cp=0, xv="pick",xval=nrow(spe.hel), xvmult=100, which=4)

doubs.mrt <- mvpart(as.matrix(spe.hel) ~. ,env,legend=F, margin=0,bars = F,xval=nrow(spe.hel), xvmult=100, which=4)



# Find discriminant species with MRT results
doubs.mrt.wrap<-MRT(doubs.mrt,percent=10,species=colnames(spe.hel))
summary(doubs.mrt.wrap)

# Extract indval p-values
doubs.mrt.indval<-indval(spe.hel,doubs.mrt$where)
doubs.mrt.indval$pval

# Extract indicator species of each node, with its indval
doubs.mrt.indval$maxcls[which(doubs.mrt.indval$pval<=0.05)]
doubs.mrt.indval$indcls[which(doubs.mrt.indval$pval<=0.05)]




