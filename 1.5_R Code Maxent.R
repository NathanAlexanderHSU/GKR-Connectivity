install.packages("spatstat")
install.packages("rgdal")
install.packages("raster")
install.packages("maptools")
install.packages("rgeos")
install.packages("rJava")
install.packages("sp")
install.packages("PresenceAbsence")
require(spatstat)
require(rgdal)
require(raster)
require(maptools)
require(rgeos)
require(dismo)
require(rJava)
require(sp)
require(PresenceAbsence)


setwd("H:\\GKRShapefiles\\")

#gkr2=shapefile of 2011-2013 occupancy merged with 2014 occupancy

#rand=Random points generated in ArcMap 10.1 from the DataManagement tool 
#bounded to the feature "BuffBound3."

#BuffBound3 is the northern CPNA study site buffered by 2000m and projected into NAD_1983_10_UTM


gkr<- readOGR(dsn=".",layer="GKRAllpts")
#abs<-readOGR(dsn='.',layer="rand")

Bound.poly<-readOGR(dsn=".",layer="boundary")
plot(Bound.poly)
plot(gkr,add=TRUE)

# Now let's load the environmental predictor layers

# Elevation from SRTM global data
#slope1 is from the D: drive GKR data clipped to BoundBuff3
#Precip1 is a clipped and re-scaled Prism layer clipped to BoundBuff3
#veg1 is from USFS vegetation files using WHRTYPE clipped to BoundBuff3
#Soil is from NRCS using "taxpartsize" clipped to BoundBuff3

slope <- raster("slope_img.asc")
precip <- raster("rainresample.asc") 
veg <- raster("vegetation_img.asc")
road<-raster("road_img.asc")


Road<-crop(road,Bound.poly,keepres=TRUE)
Veg<-crop(veg,Bound.poly,keepres=TRUE)
Precip<-crop(precip, Bound.poly, keepres=TRUE)
Slope<-crop(slope,Bound.poly,keepres=TRUE)

Elev<- mask(Elev, Bound.poly,keepres=TRUE)
Precip<- mask(Precip, Bound.poly,keepres=TRUE)
Veg<- mask(Veg, Bound.poly,keepres=TRUE)
Soil<- mask(Soil, Bound.poly,keepres=TRUE)

#slope<-writeRaster(Elev,"D:\\GKR\\NBA\\Rasters\\slope.tif")
#veg<-writeRaster(Elev,"D:\\GKR\\NBA\\Rasters\\Veg.tif")
#precip<-writeRaster(Elev,"D:\\GKR\\NBA\\Rasters\\Precip.tif")
#soil<-writeRaster(Elev,"D:\\GKR\\NBA\\Rasters\\soil.tif")

compareRaster(Slope,Veg,Precip,Road,extent=TRUE,res=TRUE)

plot(Soil)
plot(Veg,add=TRUE)
plot(Precip,add=TRUE)
plot(Elev,add=TRUE)
plot(gkr,add=TRUE)

length(gkr)


occ.pts <- as.data.frame(gkr)
head(occ.pts
)
##Presence/Absence data


occ.pts$occupied <- rep(1, length(gkr))
head(occ.pts)
occ.pts<-data.frame(occ.pts$coords.x1,occ.pts$coords.x2,occ.pts$occupied)
colnames(occ.pts)<-c("x","y","observed")

Occ<-na.omit(occ.pts)
Occ<-data.frame((Occ[1:384,],Occ[387:404,],Occ[413:418,],Occ[427:513,]))
Occ
occ.pts
#abs.pts <- as.data.frame(abs)
#abs.pts$observed <- rep(0, nrow(abs.pts))
#abs.pts<-cbind(abs.pts$coords.x1,abs.pts$coords.x2,abs.pts$observed)
#colnames(abs.pts)<-c("x","y","observed")


#all.data <- data.frame(rbind(occ.pts, abs.pts))
#head(all.data)

predictors <- stack(Veg,Slope,Precip,Road)

gkr.maxent <- maxent(predictors, occ.pts[,1:2], args=c("betamultiplier=1", "-J", "-P"),path="C:\\Users\\nba52\\Desktop\\Max")

gkr.raster <- predict(gkr.maxent, predictors)
plot(gkr.raster)
plot(gkr,add=TRUE,pch=17)
getwd()
#plot(gkr.raster)
#points(occ.pts)
occ.pts<-occ.pts[!0]
occ.pts
##Maxent Code

beta.parameters <- c(1,2,3,4) # Create a vector of all the beta paremeters

for(i in beta.parameters){
  print(paste("Run Maxent with", i, "as the beta parameter"))
}

all.combos <- list()
pred.num <- nlayers(predictors)
for(i in 2:pred.num){
  all.combos <- c(all.combos, combn(1:pred.num, i, simplify=FALSE))
}

all.combos
class(all.combos)
all.combos[[4]]
for(i in beta.parameters){ # Create a loop for each beta parameter
  for(j in 1:length(all.combos)){
output.dir <- paste("H:\\MaxentModels\\b", i, ".", 
                        paste(all.combos[[j]], collapse=''), sep="")   
print(output.dir)
model <- maxent(predictors[[all.combos[[j]]]], occ.pts[,1:2],
                    args=c(paste("betamultiplier=", i, sep=""), "outputformat=raw"),
                           path=output.dir)
    max.results <- predict(model, predictors[[all.combos[[j]]]])
    writeRaster(max.results, paste(output.dir, "/maxent.asc", sep=""),
                format="ascii")
    print(paste(j, " of ", length(all.combos), ": ", Sys.time(), sep=""))
  }
  
}
warnings()
occ.pts<-data.frame(occ.pts)
head(occ.pts)
name <- rep("gkr",nrow(occ.pts))
enmtools.occpts <- data.frame(name, occ.pts[,1], occ.pts[,2])
colnames(enmtools.occpts) <- c("Species", "X", "Y")
write.csv(enmtools.occpts, "H:\\MaxentModels\\GKRinputConnectivity.csv", row.names=FALSE)

head(enmtools.occpts)

setwd("H:\\MaxentModels\\")


getwd()
dirs <- list.files()
dirs

models <- NULL
for(i in 1:length(dirs)){
  models[i] <- paste("H:\\MaxentModels\\GKRinputConnectivity.csv,","H:\\MaxentModels\\",
                    dirs[i], "\\maxent.asc,H:\\MaxentModels\\", dirs[i],
                    "/species.lambdas", sep="")
}

write.csv(models, "maxent_models.csv", row.names=FALSE, quote=FALSE, col.names=FALSE)


