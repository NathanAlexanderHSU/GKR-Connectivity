
###To Compare Isolation by Distance to Isolation by Resistance, the Euclidean
###Distance is calculated using Circuitscape for a map with no variation 
###cost/cell


require(raster)

setwd("H:\\THESISDATA\\GKR_LatLon\\CircuitscapeRasters\\CSBoundedCorrectly\\")



rain<-raster("rainresample.asc")
max(rain)
Reclass<-matrix(c(0,1000,50),ncol=3,nrow=1)
head(Reclass)
IBD<-reclassify(rain,Reclass)
plot(IBD)
writeRaster(IBD,"C:\\Users\\nba52\\Desktop\\IBD.asc")
