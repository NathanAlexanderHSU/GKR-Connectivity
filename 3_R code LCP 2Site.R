require(gdistance)
require(raster)
require(rgdal)
require(sp)

#read in the patch files. Each of these is a unique location
patch.locs<-read.table(choose.files())
patch.locs <- read.table("H:\\THESISDATA\\GKR_LatLon\\GKR_Circuitscape_Sites.txt")  #Text file of locs

#specify column names. Format is a text file with site ID, Longitude, Latitude
names(patch.locs)<-c("ID","x","y")
head(patch.locs)

#list all the raster files that will have costs calculated across them
maps <- list.files(path="I:\\THESISDATA\\Parameterization\\ascMaps\\")    #prompt user for dir containing raster files
maps

#cycle through calculating for each raster
for(k in 1:length(maps)){

	cost.surface <- raster(paste("I:\\THESISDATA\\Parameterization\\ascMaps\\",maps[k],collapse='',sep=""))
	tr <- transition(1/cost.surface, transitionFunction=sum, directions=8)
	tr.corr <- geoCorrection(tr, type="c")
	patch.dist <- NULL

#cycle trhough each site
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
