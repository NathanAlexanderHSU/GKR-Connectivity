#############MANTEL TEST############
##Use Dps from memgene package and PS  codominant marker distance from Genalex
##This code is used for testing the parameterization values as well
##The GeoDist assignment changes based on LCP or IBR testing

install.packages("ecodist")
require(ecodist)

IndCostMat<-list.files("H:\\THESISDATA\\IndIBRDist\\")
setwd("H:\\THESISDATA\\IndIBRDist\\")
cost<-read.csv(IndCostMat[1])
dim(cost)
cost<-cost[,-1]
head(cost)

GeoDist<-read.csv("H:\\THESISDATA\\IndIBDAll.csv")
head(GeoDist)
GeoDist<-(GeoDist[,-1])
GeoDist<-as.dist(GeoDist)


MantelMatrix<-matrix(NA,ncol=7)

for(i in 1:length(IndCostMat)){
  x<-read.csv(IndCostMat[i])
  x<-x[,-1] 
  GenDist<-as.dist(Gen)
  CostDist<-as.dist(x)
  Man<-mantel(GenDist~CostDist+GeoDist, nperm=1000000,mrank=FALSE,nboot=1000,pboot=.9,cboot=.95) ##mrank=FALSE; Pearson better than Spear for linear
  Man1<-cbind(IndCostMat[i],Man[1],Man[2],Man[3],Man[4],Man[5],Man[6])
  MantelMatrix<- rbind(Man1,MantelMatrix)
}


colnames(MantelMatrix)<-c("Model","mantelr","pval1","pval2","pval3","llim.2.5%","ulim.97.5%")

write.csv(MantelMatrix,"H:\\THESISDATA\\Mantel\\PartialMantelPearson14Loci.csv")

IndCostMat
