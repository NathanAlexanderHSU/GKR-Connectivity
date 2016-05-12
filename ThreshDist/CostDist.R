
#####################
##Subset data into less than 3km, as identified as correlated##
##genetics from GenAlEx##
########################


##In Excel, using an "IF" function, use "IF(cell>3000,"NA",cell)

###Read in Cost distance Euc matrix with NAs##

##Get rid of the NAs and leave a list of pairwise distance vectors##

Euc<-read.csv(choose.files())
dim(Euc)
SiteID<-Euc[,1]
Euc<-Euc[,-1]

Euc<-Euc[lower.tri(Euc,diag=F)]
length(Euc)


Euc.Low
IDs<-matrix(NA,ncol=121,nrow=121)
for(i in 1:121){	
	for(j in 1:121){
		IDs[j,i]<-paste(i,";",j)
}
}

Sites<-IDs[lower.tri(IDs,diag=F)]

Paired<-cbind(Sites, Euc)

write.table(Paired,"C:\\Users\\nba52\\Desktop\\3km2.txt")

####Now import Cost Distance Matrices, and resample based on the identified pairs##
###################################################################################

##Read in pairwise vector##

P<-read.csv(choose.files())
head(P)

setwd(choose.dir()) #set wd to pairwise cost dist matrices file location
dists<-list.files()
dists
dists<-dists[-1] #remove the non cost-dist folder(s)
dists<-dists[-74]

for(k in 73:length(dists)){
	IBR<-read.csv(dists[k])
	IBR<-IBR[,-1]


	submat<-matrix(NA,ncol=4,nrow=length(P$Site1))
	submat[,1]<-P[,1]
	submat[,2]<-P[,2]
	submat[,3]<-P[,3]
	

for(i in 1:length(P[,1])){
	submat[i,4]<-IBR[submat[i,1],submat[i,2]]
}

colnames(submat)<-c("Site1","Site2","Euc","Cost")

write.csv(submat, paste(c("H:\\THESISDATA\\3kmlessthan\\LCP\\",dists[k],".csv"),collapse="",sep=""))
}


##Resample IBR Null pairwise distances##
#######################################
IBR<-read.csv(choose.files()) #read in IBR Null cost dist matrix
	IBR<-IBR[,-1]


	submat<-matrix(NA,ncol=4,nrow=length(P$Site1))
	submat[,1]<-P[,1]
	submat[,2]<-P[,2]
	submat[,3]<-P[,3]
	

for(i in 1:length(P[,1])){
	submat[i,4]<-IBR[submat[i,1],submat[i,2]]
}

colnames(submat)<-c("Site1","Site2","Euc","IBRNULL")

write.csv(submat, paste(c("H:\\THESISDATA\\3kmlessthan\\","IBRNull",".csv"),collapse="",sep=""))



##Resample genetic pairwise distances##
#######################################

##Read in pairwise vector##

P<-read.csv(choose.files())
head(P)

setwd(choose.dir()) #set wd to pairwise cost dist matrices file location
distDps<-read.csv(choose.files())
distDps

distDps<-distDps[-1] #remove the non cost-dist folder(s)


	subgen<-matrix(NA,ncol=4,nrow=length(P$Site1))
	subgen[,1]<-P[,1]
	subgen[,2]<-P[,2]
	subgen[,3]<-P[,3]
	

for(i in 1:length(P[,1])){
	subgen[i,4]<-distDps[subgen[i,1],subgen[i,2]]
}

colnames(submat)<-c("Site1","Site2","Euc","Dps")

write.csv(submat,"H:\\THESISDATA\\3kmlessthan\\DpsSubset.csv")

distPS<-read.csv(choose.files())
distPS

distPS<-distPS[-1] #remove the non cost-dist folder(s)
distPS[upper.tri(distPS)] <- t(distPS)[upper.tri(distPS)]
head(distPS)

	subgen<-matrix(NA,ncol=4,nrow=length(P$Site1))
	subgen[,1]<-P[,1]
	subgen[,2]<-P[,2]
	subgen[,3]<-P[,3]
	

for(i in 1:length(P[,1])){
	subgen[i,4]<-distPS[subgen[i,1],subgen[i,2]]
}
subgen
colnames(submat)<-c("Site1","Site2","Euc","PS")

write.csv(submat,"H:\\THESISDATA\\3kmlessthan\\PSSubset.csv")

###Mantel Test##
################################################

##LCP PS##

install.packages("ecodist")
require(ecodist)
setwd(choose.dir())
IndCostMat<-list.files()
IndCostMat
cost<-read.csv(choose.files())

IndCostMat[1]
GeoDist<-cost[,4]


PS<-read.csv(choose.files())
head(PS)
PS<-PS[,5]
head(PS)
)

GenDist<-PS
CostDist<-cost[,5]

MantelMatrix<-matrix(NA,ncol=7)
for(i in 1:length(IndCostMat)){
  x<-read.csv(IndCostMat[i])
  CostDist<-x[,5]
Man<-mantel(GenDist~CostDist+GeoDist, nperm=1000000,mrank=FALSE,nboot=1000,pboot=.9,cboot=.95) ##mrank=FALSE; Pearson better than Spear for linear
Man1<-cbind(paste(IndCostMat[i]),Man[1],Man[2],Man[3],Man[4],Man[5],Man[6])
MantelMatrix<- rbind(Man1,MantelMatrix)
}
warnings()
Man
Man1
Man<-mantel(GenDist~GeoDist+CostDist,nperm=1000000,mrank=FALSE,nboot=1000,pboot=.9,cboot=.95)
Man<-matrix(Man,nrow=1)
head(MantelMatrix)
colnames(MantelMatrix)<-c("mantelr","model","pval1","pval2","pval3","llim.2.5%","ulim.97.5%")
head(MantelMatrix)
write.csv(MantelMatrix,"H:\\THESISDATA\\Mantel\\3kmPS.csv")


###LCP Dps###
install.packages("ecodist")
require(ecodist)
setwd(choose.dir())
IndCostMat<-list.files()
IndCostMat
cost<-read.csv(choose.files())

IndCostMat[1]
GeoDist<-cost[,4]


Dps<-read.csv(choose.files())
head(Dps)
Dps<-Dps[,5]
head(Dps)


GenDist<-Dps
CostDist<-cost[,5]

MantelMatrix<-matrix(NA,ncol=7)
for(i in 1:length(IndCostMat)){
  x<-read.csv(IndCostMat[i])
  CostDist<-x[,5]
Man<-mantel(GenDist~CostDist+GeoDist, nperm=1000000,mrank=FALSE,nboot=1000,pboot=.9,cboot=.95) ##mrank=FALSE; Pearson better than Spear for linear
Man1<-cbind(paste(IndCostMat[i]),Man[1],Man[2],Man[3],Man[4],Man[5],Man[6])
MantelMatrix<- rbind(Man1,MantelMatrix)
}
warnings()
Man
Man1
Man<-mantel(GenDist~GeoDist+CostDist,nperm=1000000,mrank=FALSE,nboot=1000,pboot=.9,cboot=.95)
Man<-matrix(Man,nrow=1)
head(MantelMatrix)
colnames(MantelMatrix)<-c("model","mantel r","pval1","pval2","pval3","llim.2.5%","ulim.97.5%")
head(MantelMatrix)
write.csv(MantelMatrix,"H:\\THESISDATA\\Mantel\\3kmDps.csv")


########IBR MANTEL TEST#########
################################


##IBR PS##

setwd(choose.dir())
IndCostMat<-list.files()
IndCostMat

cost<-read.csv(choose.files()) #Choose <3km IBR Null costs

GeoDist<-cost[,5]


PS<-read.csv(choose.files())
head(PS)
PS<-PS[,5]
head(PS)


GenDist<-PS


MantelMatrix<-matrix(NA,ncol=7)
for(i in 1:length(IndCostMat)){
  x<-read.csv(IndCostMat[i])
  CostDist<-x[,5]
Man<-mantel(GenDist~CostDist+GeoDist, nperm=1000000,mrank=FALSE,nboot=1000,pboot=.9,cboot=.95) ##mrank=FALSE; Pearson better than Spear for linear
Man1<-cbind(paste(IndCostMat[i]),Man[1],Man[2],Man[3],Man[4],Man[5],Man[6])
MantelMatrix<- rbind(Man1,MantelMatrix)
}
warnings()
Man
Man1
Man<-mantel(GenDist~GeoDist+CostDist,nperm=1000000,mrank=FALSE,nboot=1000,pboot=.9,cboot=.95)
Man<-matrix(Man,nrow=1)
head(MantelMatrix)
colnames(MantelMatrix)<-c("model","mantel r","pval1","pval2","pval3","llim.2.5%","ulim.97.5%")
head(MantelMatrix)
write.csv(MantelMatrix,"H:\\THESISDATA\\Mantel\\3kmPSIBR.csv")


###IBR Dps###

setwd(choose.dir())
IndCostMat<-list.files()
IndCostMat

cost<-read.csv(choose.files()) #Choose <3km IBR Null costs

GeoDist<-cost[,5]


Dps<-read.csv(choose.files()) #Choose <3km Dps file
head(Dps)
Dps<-Dps[,5]
head(Dps)


GenDist<-Dps


MantelMatrix<-matrix(NA,ncol=7)
for(i in 1:length(IndCostMat)){
  x<-read.csv(IndCostMat[i])
  CostDist<-x[,5]
Man<-mantel(GenDist~CostDist+GeoDist, nperm=1000000,mrank=FALSE,nboot=1000,pboot=.9,cboot=.95) ##mrank=FALSE; Pearson better than Spear for linear
Man1<-cbind(paste(IndCostMat[i]),Man[1],Man[2],Man[3],Man[4],Man[5],Man[6])
MantelMatrix<- rbind(Man1,MantelMatrix)
}
warnings()
Man
Man1
Man<-mantel(GenDist~GeoDist+CostDist,nperm=1000000,mrank=FALSE,nboot=1000,pboot=.9,cboot=.95)
Man<-matrix(Man,nrow=1)
head(MantelMatrix)
colnames(MantelMatrix)<-c("model","mantel r","pval1","pval2","pval3","llim.2.5%","ulim.97.5%")
head(MantelMatrix)
write.csv(MantelMatrix,"H:\\THESISDATA\\Mantel\\3kmDpsIBR.csv")









