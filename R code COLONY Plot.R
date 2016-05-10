
###HalfSib
Sib<-read.csv(choose.files())
Sib<-Sib[,-1]
Sib<-Sib[,1:4]

head(Sib)

SiteLoc<-read.csv(choose.files(),header=T)
head(SiteLoc)

dim(SiteLoc)

IndSite<-cbind(SiteLoc[,1:2],SiteLoc[,32:33])

head(IndSite)
names(IndSite)<-c("ID","Pop","X","Y")
head(Sib)

names(Sib)<-c("ID","ID2","Probability","SD")

SibLoc<-merge(Sib,IndSite,by="ID")
names(SibLoc)<-c("Off1","ID","Probability","SD","Pop","X1","Y1")
head(SibLoc)
SibLocFull<-merge(SibLoc,IndSite,by="ID")
head(SibLocFull)
names(SibLocFull)<-c("Off2","Off1","Prob","SD","Pop1","X1","Y1","Pop2","X2","Y2")
head(SibLocFull)
write.csv(SibLocFull,"C:\\Users\\nba52\\Desktop\\SibPops.csv")

require(raster)
require(rgdal)

SibPops<-read.csv(choose.files())
SibPops<-SibLocFull
head(SibPops)

SibProb<-SibPops[which((SibPops$Prob-SibPops$SD)>=.95),]
& SibPops$SD<=.05),]  ##Constrain Prob to be greater than .95 and SD>.9
head(SibProb)
dim(SibPops)
dim(SibProb)
SibProb
dev.new()
SiteLoc
Coords<-cbind(SiteLoc$X,SiteLoc$Y)

Coords1<-cbind(SibProb$X1,SibProb$Y1)
Coords2<-cbind(SibProb$X2,SibProb$Y2)

plot(Coords,xlab="X",ylab="Y")


write.csv(IndSite,"C:\\Users\\nba52\\Desktop\\WTF.csv")
##weight=2, col=red if Lower Credible Interval >=.95
segments(SibProb$X1,SibProb$Y1,SibProb$X2,SibProb$Y2,lty=3,lwd=1,col="red")

###Full Sib
Fsib<-read.csv(choose.files())
head(Fsib)
dim(Fsib)
Fsib<-cbind(Fsib[,1:2],Fsib[,13:14])
colnames(Fsib)<-c("ID","ID2","Mean","SD")
FsibP1<-merge(Fsib,IndSite,by="ID")
head(FsibP1)
names(FsibP1)<-c("Off1","ID","P","SD","Pop1","X1","Y1")
Fsib2<-merge(FsibP1,IndSite,by="ID")


head(Fsib2)
names(Fsib2)<-c("Off2","Off1","Probability","SD","Pop1","X1","Y1","Pop2","X2","Y2")
head(Fsib2)
Fsibling<-Fsib2[which((Fsib2$Probability-Fsib2$SD)>=.95),]
Fsib2
Fsibling
Coords<-cbind(unique(Coords1),unique(SibProb$Pop1))
length(unique(SibProb$Pop1))
head(Coords)
dim(Coords1)
names(Fsibling)
segments(Fsibling$X1,Fsibling$Y1,Fsibling$X2,Fsibling$Y2,lty=1,lwd=2,col="red")
