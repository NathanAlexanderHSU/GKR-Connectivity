####Reclassification Matrices!
setwd("H:\\THESISDATA\\GKR_LatLon\\CircuitscapeRasters\\CSBoundedCorrectly\\")

##setwd(choose.dir())

##Vegetation Layer

veg<-raster("Vegetation_img.asc")
VegTypes<-c(1,  2,  3,  5,  6,  7,  8,  9, 10, 11, 12,13, 14,15, 16, 17, 19, 20, 21,22, 23, 24, 25)

VegTypes
VegRange<-c(0,1,  2,  3,  5,  6,  7,  8,  9, 10, 11, 12,13, 14,15, 16, 17, 19, 20, 21,22, 23, 24)


From.To<-matrix(c(VegRange,VegTypes),ncol=2)
dim(From.To)
From.To

# Full Reclass values 

#Veg1:  Used in All multi-parameter models
 
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
hist(rain.resample,col="red", main="Hypothesized Cost")

#Veg2:  Severe Veg
 
#WHRTYPE=(1 Annual Grassland, 2 Chapparal,3 BOW (Oak),4 COW (Oak),5 Urban,6 Crop, 7 Scrub,8 VRI Riparian,9 Chaparral,
	### 10 Sagebrush,11 Juniper,12 Alkali Desert Scrub,13 MHC (Hardwood),14 Pine,15 Conifer,16 MHW (hardwood)
	### 17 DRI riparian,18 VOW (Oak),19 Lacustrine,20 Wet Meadow,21 Barren,22 Irrigated Row Crop,23 Pasture,
	###24 Orchard,25  Riverine,26 FEW (wetland)-FEW IS NOT PRESENT IN ANY VEG
	
	###There is not the following in the cropped maps: 4. COW (oak),  18. VOW (Oak)-need to remove from Reclass
	
	##VegTypes to include: Grassland, Chapparal, BOW (Oak), Urban, Crop, Scrub, VRI Riparian, 
	##Chaparral, Sagebrush, Juniper, Alkali Desert Scrub, Pine, MHW (Hardwood), DRI Riparian, Lacustrine,
	##Wet Meadow, Barren, Irrigated Row Crop, Pasture, Orchard, Riverine

	##Cluster by types and assign same cost
	## Grassland<-.001
	## Chapparal, Scrub,, Sagebrush, Alkali Desert Scrub <-1
	##BOW (oak), Juniper, MHC (hardwood) Pine, MHW (hardwood), conifer<-1
	##Urban<-1
	##Crop<-1
	##VRI Riparian, DRI Riparian, Wet Meadow<-1
	##Lacustrine, Riverine<-1
	##Barren<-.001
	##Irrigated Row Crop<-1
	##Pasture<-.001 
	##Orchard<-1

	
plot(veg)
hist(veg,col="red",main="Veg Occurrence")
ReclassCosts<-matrix(c(.001,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,.001,1,.001,1,1),ncol=1)
length(ReclassCosts)
dim(From.To)
ReclassValues<-matrix(c(From.To,ReclassCosts),ncol=3)
(unique(ReclassValues))
head(ReclassValues)
length(ReclassValues)

Veg.reclass<-reclassify(veg,ReclassValues)
hist(Veg.reclass)
plot(Veg.reclass)



######Rain Reclassification
rain<-raster("rainresample.asc")

##Binary, Low Rain=Good
Rain.reclass.Low<-matrix(c(0,300,.001,300,600,1),nrow=2,byrow=T)
Rain.reclass.Low
Rain.LowGood<-reclassify(rain,Rain.reclass.Low)
hist(Rain.LowGood)
#Binary, Medium Rain=Good
Rain.reclass.Medium<-matrix(c(0,250,1,250,350,.001,350,600,1),ncol=3,byrow=T)
Rain.reclass.Medium
Rain.MediumGood<-reclassify(rain,Rain.reclass.Medium)
Rain.reclass<-rain/max(unique(rain))
min(Rain.LowGood)
min(Rain.MediumGood)

#Continuous Rain with Low=Good, High=Cost (Used in All combination Models
Rain.reclass<-rain/max(unique(rain))


###Road Reclassification

##Roads are flat and open, predominantly dirt in the study area-Presuming that dirt roads aid in dispersal
##NEED TO RECLASSIFY PAVED AND HIGHWAY ROADS!  Potentially need to completely re-create road layer from digitization

road<-raster("road_img.asc")

unique(road)
#Road=connectivity path
ReclassRoadValues<-matrix(c((unique(road)-1),unique(road),.001,1),ncol=3)
head(ReclassRoadValues)
Road.Conduct<-reclassify(road,ReclassRoadValues)
unique(Road.Conduct)

#Road=Cost
ReclassRoadValues<-matrix(c((unique(road)-1),unique(road),1,.001),ncol=3)
head(ReclassRoadValues)
Road.Cost<-reclassify(road,ReclassRoadValues)
unique(Road.reclass)

#Currently used in models: 0 if there is a Rd, .3 if no road
ReclassRoadValues<-matrix(c((unique(road)-1),unique(road),.001,.3),ncol=3)
head(ReclassRoadValues)
Road.reclass<-reclassify(road,ReclassRoadValues)
unique(Road.reclass)

########Slope

slope<-raster("Slope_img.asc")
hist(slope)
###Currently used Model, copntinuous cost
Slope.reclass<-(slope+1)/max(unique(slope+1))  ##Circuitscape cannot do 0 values, so add on minimal decimal


##10 Binary Model
ReclassSlopeValues<-matrix(c(-1,10,0.001,10,100,1),ncol=3,byrow=T)
head(ReclassSlopeValues)
Slope10Binary<-reclassify(slope,ReclassSlopeValues)
hist(slope)
hist(Slope.reclass)
hist(Slope10Binary)

##5 Binary Model
ReclassSlopeValues<-matrix(c(-1,5,.001,5,100,1),ncol=3,byrow=T)
head(ReclassSlopeValues)
Slope5Binary<-reclassify(slope,ReclassSlopeValues)
hist(slope)
hist(Slope.reclass)
plot(Slope10Binary)


########################Calculations for permutations
predictors<-stack(Slope10Binary,Slope5Binary,Rain.LowGood,Rain.MediumGood,Road.Conduct,Road.Cost )

predictors<-predictors*1000 ##Make non-decimal for power functions
names(predictors)<-c("Slope10Binary","Slope5Binary","Rain.LowGood","Rain.MediumGood","Road.Conduct","Road.Cost")
length(names(predictors))

pred.num <- nlayers(predictors)
names(predictors[[1]])
for (i in 1:pred.num){
model<-predictors[[i]]

output.dir <- paste(c("H:\\THESISDATA\\OutputCostMaps\\Power1",".", 
                        names(predictors[[i]]), collapse=''), sep="",collapse="")   
print(output.dir)
       writeRaster(model, paste(c(output.dir, "costmap.asc"), sep="",collapse=""),
                format="ascii")
    print(paste(i, " of ", length(pred.num), ": ", Sys.time(), sep=""))
  }











