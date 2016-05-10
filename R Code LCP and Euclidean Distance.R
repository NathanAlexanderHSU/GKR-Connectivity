
#LCP#


# Load libraries

install.packages("gdistance")
install.packages("rgdal")

require(gdistance)
require(raster)
require(rgdal)
require(sp)

patch.locs <- read.table("H:\\THESISDATA\\GKR_LatLon\\GKR_Circuitscape_Sites.txt")  #Text file of locs
names(patch.locs)<-c("ID","x","y")
head(patch.locs)
maps <- list.files(path="H:\\THESISDATA\\OutputCostMaps\\")    #prompt user for dir containing raster files
maps
for(k in 1:length(maps)){

	cost.surface <- raster(paste("H:\\THESISDATA\\OutputCostMaps\\",maps[k],collapse='',sep=""))
	tr <- transition(1/cost.surface, transitionFunction=sum, directions=8)
	tr.corr <- geoCorrection(tr, type="c")
	patch.dist <- NULL


for(i in 1:(nrow(patch.locs)-1)){
	from.patch <- patch.locs[i,]


	# For each new patch
	for(j in (i+1):nrow(patch.locs)){
		to.patch <- patch.locs[j,]
		from.loc <- c(from.patch$x, from.patch$y)
		to.loc <- c(to.p atch$x, to.patch$y)
		cost <- costDistance(tr.corr, from.loc, to.loc)
		patch.dist <- rbind(patch.dist, data.frame(from.patch$ID, to.patch$ID, cost))		
		print(paste(from.patch$ID, to.patch$ID, Sys.time()))
	}
}

write.table(patch.dist, paste("H:\\THESISDATA\\LCPDist\\","LCP",maps[k],".txt",collapse='',sep=""), row.names=FALSE, col.names=FALSE)

}

###Euclidean Distance

names(patch.locs)<-c("gridID","x","y")
head(patch.locs)
EucLCP<-patch.locs[order(patch.locs$gridID),]
head(EucLCP)
try<-apply(data.frame(patch.locs[,3:4]), 1, function(eachPoint) spDistsN1(as.matrix(data.frame(patch.locs[,3:4])), eachPoint))
head(try)
install.packages("gdata")
require(gdata)
Euclidean<-lowerTriangle(try, diag=FALSE)
head(Euclidean)
write.csv(try,"C:\\Users\\nba52\\Desktop\\EuclideanVectorTrial.csv")
