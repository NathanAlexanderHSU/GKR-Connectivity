######MAPPING LCP

install.packages("gdistance")
install.packages("rgdal")
install.packages("raster")
require(gdistance)
require(raster)
require(rgdal)
require(sp)
gkr.pts <- readOGR("F:\\GKRThesisCoding",layer="ocpt")
patch.locs <- as.data.frame(gkr.pts)
names(patch.locs)<-c("pointID","gridID","x","y")
head(patch.locs)
gkr.pts$ident <- as.integer(patch.locs$pointID)
maps <- list.files(path="H:\\R_CS\\CM")    #prompt user for dir containing raster files
e <- extent(685000,730000,4030000,4065000)
plot(e)
plot(raster(paste("H:\\R_CS\\CM\\model_2costmap_asc.asc")),legend=FALSE,xlim=c(684900,730000),ylim=c(4030000,4065000),add=TRUE)
head(gkr.pts)
plot(gkr.pts[1,],add=TRUE,pch=17)
plot(gkr.pts[6,],add=TRUE,pch=17)
plot(gkr.pts[104,],add=TRUE,pch=17)


for(k in 1:length(maps)){
  
  cost.surface <- raster(paste("H:\\R_CS\\CM\\",maps[k],collapse='',sep=""))
  tr <- transition(1/cost.surface, transitionFunction=sum, directions=8)
  tr.corr <- geoCorrection(tr, type="c")
  
  
  
  from.patch <- patch.locs[1,]
  
  to.patch <- patch.locs[6,]
  from.loc <- c(from.patch$x, from.patch$y)
  to.loc <- c(to.patch$x, to.patch$y)
  cost <- shortestPath(tr.corr, from.loc, to.loc,output="SpatialLines")
  lines(cost,col="red")
}

