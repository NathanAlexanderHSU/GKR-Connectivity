## Memgene analysis
##Can plot on Google maps if using Decimal Lat/Lon, but not UTMS

install.packages("memgene")
library(memgene)
library(adegenet)
## Read in genotypes and coordinates
genalex <- read.csv(choose.files(), header=TRUE)  ##Read in raw loci values, not genetic distances
genalex<-genalex[-1,]
head(genalex)
dim(genalex)
# Subset genotypes and coordinates
Genotypes <- genalex[1:121,3:32]
Coords <- genalex[1:121,34:35]
(Genotypes)

## Produce genetic distance matrix using proportion of shared alleles (Bowcock et al. 1994)

dm <- codomToPropShared(Genotypes)  

## Extract Moran's Eigen Vectors from distance matrix

if(!exists("Analysis"))
Analysis <- mgQuick(dm,Coords)


## Visualize results of the first two memgene variables 
mgMap(Coords, Analysis$memgene[,1],legend=TRUE)

Analysis$memgene[,1:2]

