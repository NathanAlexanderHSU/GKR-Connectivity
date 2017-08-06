######MAPPING LCP

install.packages("gdistance")
install.packages("rgdal")
install.packages("raster")
require(gdistance)
require(maptools)
require(raster)
require(rgdal)
require(sp)
require(ggplot2)
#gkr.pts <- readOGR("F:\\GKRThesisCoding",layer="ocpt")

patch.locs <- as.data.frame(read.table(choose.files()))
names(patch.locs)<-c("pointID","x","y")
head(patch.locs)
gkr.pts$ident <- as.integer(patch.locs$pointID)
maps <- list.files(choose.dir(),recursive=FALSE)    #prompt user for dir containing raster files
maps
e <- extent(raster(maps[1]))
setwd(choose.dir())
plot(e)
plot(raster(paste(maps[1])),legend=FALSE,xlim=c(684900,730000),ylim=c(4030000,4065000),add=TRUE)


points(patch.locs[,2:3],pch=25,fill="white")
plot(gkr.pts[1,],add=TRUE,pch=17)
plot(gkr.pts[6,],add=TRUE,pch=17)
plot(gkr.pts[104,],add=TRUE,pch=17)


for(k in 1:length(maps)){
  
  cost.surface <- raster(paste(maps[k]))
  tr <- transition(1/cost.surface, transitionFunction=sum, directions=8)
  tr.corr <- geoCorrection(tr, type="c")
  
  from.patch <- patch.locs[1,]
  
  to.patch <- patch.locs[2,]
  from.loc <- c(from.patch$x, from.patch$y)
  to.loc <- c(to.patch$x, to.patch$y)
  cost <- shortestPath(tr.corr, from.loc, to.loc,output="SpatialLines")
  LCP <- cost

  
  for(i in 1:(length(patch.locs[,1])-1)){
  
    for(z in (i+1):(length(patch.locs[,1]))) {
  from.patch <- patch.locs[i,]
  
  to.patch <- patch.locs[z,]
  
  
  
  from.loc <- c(from.patch$x, from.patch$y)
  to.loc <- c(to.patch$x, to.patch$y)
  cost <- shortestPath(tr.corr, from.loc, to.loc,output="SpatialLines")
  LCP <- rbind(LCP, cost)
  
    }
  }

  df<-data.frame(1:length(LCP))
  row.names(df)<-row.names(LCP)

  LCP_SLDF<-SpatialLinesDataFrame(LCP,data=df)
  
    writeOGR(LCP_SLDF, dsn="C:/Users//Nathan/Desktop/LCP_shapefiles" ,layer=paste0("LCP_",maps[k]),driver="ESRI Shapefile")
}


