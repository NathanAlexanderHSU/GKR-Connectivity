install.packages('devtools',dep=T)
library(devtools)

install_github('royfrancis/pophelper')
library(pophelper)

##STRUCTURE
setwd("H:\\Structure\\2014\\2014ST\\2014RWork\\2014RWork\\RTrial\\Results")
sfiles<-list.files()
head(sfiles)
stab<-tabulateRunsStructure(sfiles,writetable=TRUE)
ssum<-summariseRunsStructure(stab,writetable=TRUE)
Evan<-evannoMethodStructure(ssum,exportplot=T) 
DFStructure<-runsToDfStructure(sfiles)
clumppExportStructure(sfiles)

plotRuns(choose.files(),imgoutput="tab")


stab

###TESS EXPORT
setwd("H:\\Tess\\Merge1\\")
getwd()

collectRunsTess() 
Tabulated<-tabulateRunsTess(choose.files(),writetable=TRUE)
tfiles<-list.files(("H:\\Tess\\TessTrue\\1TessRuns"))
setwd("H:\\Tess\\TessTrue\\1TessRuns")
head(tfiles)
Summarized<-summariseRunsTess(Tabulated,writetable=TRUE)
Summarized
setwd(choose.dir())
clumppExportTess(tfiles) 
setwd(choose.dir())
mfiles<-list.files()
mfiles
PopLab<-read.delim("H:\\TESS\\PopAssign.txt",header=F)
str(PopLab)
head(PopLab)
plotRuns(choose.files(),imgoutput="tab")
summary(Summarized)
(Tabulated)
R<-read.table(choose.files()) 

Pop<-R
write.csv(Pop,"C:\\Users\\nba52\\Desktop\\PopAssignments.csv")
head(R)
analyseRuns(R)


