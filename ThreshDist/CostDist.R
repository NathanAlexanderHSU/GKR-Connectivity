
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
Euc<-Euc[,-1] #remove row names

Euc<-Euc[lower.tri(Euc,diag=F)]



##Extract the individual coded value

IDs<-matrix(NA,ncol=121,nrow=121)
for(i in 1:121){	
	for(j in 1:121){
		IDs[j,i]<-paste(i,";",j)
}
}

Sites<-IDs[lower.tri(IDs,diag=F)]

Paired<-cbind(Sites, Euc)

write.table(Paired,"C:\\Users\\nba52\\Desktop\\3km2.txt")

####Now import Cost Distance Matrices, and resample based on the identified pairs

##Read in pairwise vector##

P<-read.csv(choose.files())
head(P)

setwd(choose.dir()) #set wd to pairwise cost dist matrices file location
dists<-list.files()
dists
dists<-dists[-1] #remove the non cost-dist folder(s)


for(k in 1:length(dists)){
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
