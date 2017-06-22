require(gdistance)
require(raster)
require(rgdal)
require(sp)
patch.locs<-read.table(choose.files())
patch.locs <- read.table("H:\\THESISDATA\\GKR_LatLon\\GKR_Circuitscape_Sites.txt")  #Text file of locs
names(patch.locs)<-c("ID","x","y")
head(patch.locs)
maps <- list.files(path="I:\\THESISDATA\\Parameterization\\ascMaps\\")    #prompt user for dir containing raster files
maps
for(k in 5:length(maps)){

	cost.surface <- raster(paste("I:\\THESISDATA\\Parameterization\\ascMaps\\",maps[k],collapse='',sep=""))
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

write.table(patch.dist, paste("C:\\Users\\Nathan\\Desktop\\2015ParameterizationLCP\\","LCP",maps[k],".txt",collapse='',sep=""), row.names=FALSE, col.names=FALSE)

}
