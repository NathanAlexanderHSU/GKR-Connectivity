######################################
##LCP resample
######################################
##Creating individual matrices from population distance matrices



##read in a file with the krat individuals and the krat sites

KratSite<-read.csv("H:\\THESISDATA\\KRAT_SITE_FILES\\Krat_Lat.csv")

##read in a file that has sites associated with circuitscape numeric values (order of nodes)

SiteNum<-read.csv("H:\\THESISDATA\\KRAT_SITE_FILES\\Lat_CS.csv")

head(SiteNum)
length(SiteNum$Cscode)
head(KratSite)


##merge the two datasets to allow each individual to have a population based Circuitscape code

KratNum<-merge(KratSite,SiteNum,by="Lat")
head(KratSite)
head(KratNum)

##rename the column names

names(KratNum)<-c("Lat","ID","CScode")

##Merging made the individuals get out of order. To use individuals, we need to re-order
##based on the genetic distance order



#rename row names to do pairwise

OrderedData <- KratNum[order(KratNum$ID, KratSite$Lat),1:3]
head(OrderedData)



x<-(OrderedData$CScode)
length(x)
##read in cost maps
setwd(choose.dir())
LCPmaps<-list.files("H:\\THESISDATA\\LCPDist\\")
LCPmaps<-LCPmaps[-1]
LCPmaps<-list.files()
LCPmaps
##read in file with the Circuitscape ID assignment

SiteNum<-read.table("H:\\THESISDATA\\GKR_LatLon\\GKR_Circuitscape_Site.txt")
colnames(SiteNum)<-c("CSCode","X","Y")

for(k in 1:length(LCPmaps)){
  model<-read.table(LCPmaps[k],header=F)
 	M<-matrix(NA,ncol=47,nrow=47)
	M[lower.tri(M,diag=F)]<-model[,3]
	M[upper.tri(M,diag=F)]<-t(M)[upper.tri(M)]
	M[is.na(M)]<-0

  row.names(M)<-SiteNum$CSCode
  colnames(M)<-SiteNum$CSCode
  
  Individual<-matrix(NA,ncol=121,nrow=121) ##Create a null matrix of length of individuals
  for(i in 1:121){
    
    for(j in 1:121){
      
      Individual[i,j]<-M[paste(c(x[i]),collapse="",sep=","),paste(c(x[j]),collapse="",sep=",")]
    }
  }
  Data<-data.frame(Individual)
 write.csv(Individual,paste(c("H:\\THESISDATA\\IndLCPDist\\NEED",LCPmaps[k],".csv"),sep="",collapse=""))
  
}
