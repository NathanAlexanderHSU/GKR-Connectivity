######################################
##LCP resample
######################################
##Creating individual matrices from population distance matrices



##read in a file with the krat individuals and the krat sites

KratSite<-read.table("C:\\Users\\Nathan\\Desktop\\2015_CS\\2015_occurrences_SITE.txt")

##read in a file that has sites associated with circuitscape numeric values (order of nodes)

SiteNum<-read.table("C:\\Users\\Nathan\\Desktop\\2015_CS\\2015_occurrences.txt")


head(SiteNum)
length(SiteNum$Cscode)
head(KratSite)


##merge the two datasets to allow each individual to have a population based Circuitscape code

KratNum<-merge(KratSite,SiteNum,by=c("V2","V3"))
head(KratSite)
head(KratNum)

##rename the column names

names(KratNum)<-c("Lon","Lat","CScode","IndID")

##Merging made the individuals get out of order. To use individuals, we need to re-order
##based on the genetic distance order



#rename row names to do pairwise

OrderedData <- KratNum[order(KratNum$ID, KratSite$Lat),1:3]
head(OrderedData)



x<-(KratNum$CScode)
length(x)
##read in cost maps
setwd(choose.dir())
LCPmaps<-list.files()
#LCPmaps<-LCPmaps[-1]
#LCPmaps<-list.files()
#LCPmaps
##read in file with the Circuitscape ID assignment

SiteNum<-read.table("H:\\THESISDATA\\GKR_LatLon\\GKR_Circuitscape_Site.txt")
colnames(SiteNum)<-c("CSCode","X","Y")

for(k in 1:length(LCPmaps)){
model<-read.table(LCPmaps[1],header=F)
  model<-read.table(LCPmaps[k],header=F)
 	M<-matrix(NA,ncol=72,nrow=72)
	M[lower.tri(M,diag=F)]<-model[,3]
	M[upper.tri(M,diag=F)]<-t(M)[upper.tri(M)]
	M[is.na(M)]<-0

  row.names(M)<-KratNum$CSCode
  colnames(M)<-KratNum$CSCode

  Individual<-matrix(NA,ncol=239,nrow=239) ##Create a null matrix of length of individuals
  for(i in 1:239){
    
    for(j in 1:239){
      
      Individual[i,j]<-M[x[i],x[j]]
    }
  }
  Data<-data.frame(Individual)
 write.csv(Individual,paste(c("C:\\Users\\Nathan\\Desktop\\2015THESISDATA\\2015_LCP\\LCP_Param_Ind\\",LCPmaps[k],".csv"),sep="",collapse=""))
  
}
