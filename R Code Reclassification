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
library(dismo)




setwd("H:\\THESISDATA\\GKR_LatLon\\CircuitscapeRasters\\CSBoundedCorrectly\\")



rain<-raster("rainresample.asc")
road<-raster("road_img.asc")
slope<-raster("Slope_img.asc")
veg<-raster("Vegetation_img.asc")


plot(veg)
names(veg)
plot(rain,add=TRUE)

#Veg cost assignments
VegTypes<-c(1,  2,  3,  5,  6,  7,  8,  9, 10, 11, 12,13, 14,15, 16, 17, 19, 20, 21,22, 23, 24, 25)

VegTypes
VegRange<-c(0,1,  2,  3,  5,  6,  7,  8,  9, 10, 11, 12,13, 14,15, 16, 17, 19, 20, 21,22, 23, 24)


From.To<-matrix(c(VegRange,VegTypes),ncol=2)
dim(From.To)
From.To

# Full Reclass values 
#WHRTYPE=(1 Annual Grassland, 2 Chapparal,3 BOW (Oak),4 COW (Oak),5 Urban,6 Crop, 7 Scrub,8 VRI Riparian,9 Chaparral,
	### 10 Sagebrush,11 Juniper,12 Alkali Desert Scrub,13 MHC (Hardwood),14 Pine,15 Conifer,16 MHW (hardwood)
	### 17 DRI riparian,18 VOW (Oak),19 Lacustrine,20 Wet Meadow,21 Barren,22 Irrigated Row Crop,23 Pasture,
	###24 Orchard,25  Riverine,26 FEW (wetland)-FEW IS NOT PRESENT IN ANY VEG
	
	###There is not the following in the cropped maps: 4. COW (oak),  18. VOW (Oak)-need to remove from Reclass
	
	##VegTypes to include: Grassland, Chapparal, BOW (Oak), Urban, Crop, Scrub, VRI Riparian, 
	##Chaparral, Sagebrush, Juniper, Alkali Desert Scrub, Pine, MHW (Hardwood), DRI Riparian, Lacustrine,
	##Wet Meadow, Barren, Irrigated Row Crop, Pasture, Orchard, Riverine

	##Cluster by types and assign same cost
	## Grassland<-.2
	## Chapparal, Scrub,, Sagebrush, Alkali Desert Scrub <-.3
	##BOW (oak), Juniper, MHC (hardwood) Pine, MHW (hardwood), conifer<-.6
	##Urban<-1
	##Crop<-.4
	##VRI Riparian, DRI Riparian, Wet Meadow<-.8
	##Lacustrine, Riverine<-1
	##Barren<-.001
	##Irrigated Row Crop<-.7
	##Pasture<-.1 
	##Orchard<-.5

	
length(unique(veg))
ReclassCosts<-matrix(c(.2,.3,.6,1,.4,.3,.8,.3,.3,.6,.3,.6,.6,.6,.6,.8,1,.8,.001,.7,.1,.5,1),ncol=1)
length(ReclassCosts)
dim(From.To)
ReclassValues<-matrix(c(From.To,ReclassCosts),ncol=3)
(unique(ReclassValues))
head(ReclassValues)
length(ReclassValues)

Veg.reclass<-reclassify(veg,ReclassValues)
unique(Veg.reclass)
plot(Veg.reclass)


#Slope cost assignments

plot(slope,add=TRUE)
max(unique(slope))
boxplot(Slope.reclass)
Slope.reclass<-(slope+1)/max(unique(slope+1))  ##Circuitscape cannot do 0 values, so add on minimal decimal
plot(Slope.reclass)
min(Slope.reclass)
#Rain cost assignments
max(unique(rain))

Rain.reclass<-rain/max(unique(rain))
plot(Rain.reclass)


plot(Rain.reclass,add=TRUE)
plot(Slope.reclass,add=TRUE)
plot(Veg.reclass,add=TRUE)
plot(road)
plot(Rain.reclass)


#check road values

##Roads are flat and open, predominantly dirt in the study area-Presuming that dirt roads aid in dispersal
##NEED TO RECLASSIFY PAVED AND HIGHWAY ROADS!  Potentially need to completely re-create road layer from digitization

unique(road)
#0 if there is a Rd, 1 if no road
ReclassRoadValues<-matrix(c((unique(road)-1),unique(road),.001,.3),ncol=3)
head(ReclassRoadValues)
Road.reclass<-reclassify(road,ReclassRoadValues)
unique(Road.reclass)
