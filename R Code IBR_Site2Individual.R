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
KratNum
names(KratNum)<-c("Lat","ID","CScode")

##Merging made the individuals get out of order. To use individuals, we need to re-order
##based on the genetic distance order



#rename row names to do pairwise

OrderedData <- KratNum[order(KratNum$ID, KratSite$Lat),1:3]
head(OrderedData)

rownames(Gen)<-OrderedData$ID[1:121]

x<-(OrderedData$CScode)
length(x)

#########read in cost maps

IBRmaps<-list.files("H:\\THESISDATA\\IBRDistMatrices\\")
IBRmaps<-choose.files()

#######read in file with the Circuitscape ID assignment

SiteNum<-read.table("H:\\THESISDATA\\GKR_LatLon\\GKR_Circuitscape_Site.txt")
IBRmaps<-choose.files()
IBRmaps
for(k in 1:length(IBRmaps)){
  model<-read.csv(paste(c("H:\\THESISDATA\\IBRDistMatrices\\",IBRmaps[k]),sep="",collapse=""),header=T)
  model<-read.table(choose.files(),header=TRUE)
  model<-model[,-1] ##Get rid of list of names
KratNum
rownames(model)
  row.names(model)<-unique(KratNum[,3])
  colnames(model)<-unique(KratNum[,3])
  x<-(KratNum$CScode)

  Individual<-matrix(NA,ncol=239,nrow=239) ##Create a null matrix of length of individuals
  for(i in 1:239){
  
    for(j in 1:239){
      
      Individual[i,j]<-model[paste(c(x[i]),collapse="",sep=","),paste(c(x[j]),collapse="",sep=",")]
    }
  }
  Data<-data.frame(Individual)
  write.csv(Individual,paste(c("C:\\Users\\Nathan\\Desktop\\2015THESISDATA\\MaxEnt_IBR_Conductance_Dps.csv"),sep="",collapse=""))
  
}
