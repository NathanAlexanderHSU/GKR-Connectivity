related<-read.csv(choose.files()) #read in csv of related individuals with trap locations

##file had some NA's at the botom, so constrained to the 19 related pairs

realted<-related[1:19,]

##create a duplicate file to assign coordinates to later

related2<-related

##Get the coordinates of individual 1 (offspring/sibling

Coords1<-cbind(related$Lon1,related$Lat1)

#get the coordinates of individual 2 (parent/sibling)
Coords2<-cbind(related$Lon2,related$Lat2)

#plot the points to visualize
plot(Coords1)
points(Coords2, pch=17)

##Create spatial points

require(sp)
#individual 1
coordinates(related)<-Coords1
#individual 2
coordinates(related2)<-Coords2

##Assign projection
proj4string(related)<-CRS("+proj=longlat +datum=WGS84")
proj4string(related2)<-CRS("+proj=longlat +datum=WGS84")

require(geosphere)

#calculate the geodesic distance between the 2 points relying on the earth as an ellipsoid
distances<-distGeo(Coords1,Coords2)

related_out<-cbind(related,distances)
#Check output file
related_out
write.csv(related_out,"C:/Users/Nathan/Desktop/Related_Distances.csv")
