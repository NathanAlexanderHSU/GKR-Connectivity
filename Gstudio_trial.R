install.packages("devtools")
require(devtools)

install_github("gstudio", "dyerlab", ref = "develop")
require(gstudio)
Gen<-read.csv(choose.files())
dim(Gen)
head(Gen)

Locs<-read.csv(choose.files())
head(Locs)
dim(Locs)
Locs<-cbind(Locs[,1],Locs[,33:34])
dim(Locs)
colnames(Locs)<-c("ID","Decimal Latitude","Decimal Longitude")
head(Locs)

file <- system.file("extdata", choose.files(), package = "gstudio")
data <- read_population(choose.files(), type = "separated", locus.columns = c(5:18))
head(data)

M<-merge(Locs,data,by="ID")
head(M)
dim(M)
GKR<-cbind(M[,1],M[,4],M[,2:3],M[,7:20])
head(GKR)
dim(GKR)
colnames(GKR)<-c("ID","Cluster","Latitude","Longitude",names(GKR[,5:18]))
head(GKR)

require(ggmap)

map <- population_map(GKR)
ggmap(map) + geom_point(aes(x = Longitude, y = Latitude, color = Pop), data = GKR, 
    size = 5)

head(GKR)
?frequencies
freqs.loci <- frequencies(GKR,stratum= "ID")
head(freqs.loci)
freqs.loci[1:10, ]

names(GKR)
f <- freqs.loci[freqs.loci$Locus %in% c("dst119",    "dst168",    "dst2601",
"dst2887",   "dst2793",   "dst1210",   "dst1230",   "dst1567",   "dst3268",   "dst3158",  
"dst3646",   "dst3666",   "dst3824",   "dst3315") , ]
summary(f)
head(f)
ggplot(f) + geom_frequencies(f)+ facet_grid(Locus ~ .)+theme(legend.position = "none")
coords<-cbind(GKR[,1],GKR[,3:4])
colnames(coords)<-c("ID","Latitude","Longitude")
head(coords)
head(GKR)
PopID<-GKR[,1:2]

colnames(f)<-c("ID","Locus","Allele","Frequency")
df<-merge(f,coords,by="ID")
head(df)
df<-merge(df,PopID,by="ID")
head(df)

ggplot(df, aes(x = Latitude, y = Frequency)) + geom_line(linetype = 2) + geom_point(size = 5, 
    aes(color = Cluster))

ggmap(map) + geom_point(aes(x = Longitude, y = Latitude, size = Frequency), 
    data = df)

genetic_diversity(GKR)

amova.dist <- genetic_distance(GKR, stratum="ID", mode = "AMOVA")

D <- dist_amova(GKR[5:18])
rownames(D) <- colnames(D) <- as.character(GKR[,1])
head(D)
DC<-dist_cavalli(GKR,stratum="ID")
head(DC)

dist.euc <- genetic_distance(GKR,stratum="ID", mode = "Euclidean")
plot(dist.euc~DC)

