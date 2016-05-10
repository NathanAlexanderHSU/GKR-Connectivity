

predictors<-stack(Rain.reclass,Slope.reclass,Veg.reclass,Road.reclass)

predictors<-predictors*1000 ##Make non-decimal for power functions
predictors
###iterative combinations
all.combos <- list()
pred.num <- nlayers(predictors)
for(i in 2:pred.num){
  all.combos <- c(all.combos, combn(1:pred.num, i, simplify=FALSE))
}

all.combos
class(all.combos)

  for(j in 1:length(all.combos)){
output.dir <- paste("H:\\THESISDATA\\OutputCostMaps\\Model",".", 
                        paste(all.combos[[j]], collapse=''), sep="")   
print(output.dir)
model <- sum(predictors[[all.combos[[j]]]])/max(unique(sum(predictors[[all.combos[[j]]]])))
       writeRaster(model, paste(output.dir, "costmap.asc", sep=""),
                format="ascii")
    print(paste(j, " of ", length(all.combos), ": ", Sys.time(), sep=""))
  }


####single predictors

for (i in 1:pred.num){
model<-predictors[[i]]
output.dir <- paste("H:\\THESISDATA\\OutputCostMaps\\Model",".", 
                        paste(i, collapse=''), sep="")   
print(output.dir)
       writeRaster(model, paste(output.dir, "costmap.asc", sep=""),
                format="ascii")
    print(paste(i, " of ", length(all.combos), ": ", Sys.time(), sep=""))
  }


####Power models ####Use the non decimal predictors


Powers<-cbind(.25,.5,.75,1,1.25,1.5,1.75,2)

for(k in 1:length(Powers)){

  for(j in 1:length(all.combos)){
output.dir <- paste("H:\\THESISDATA\\OutputCostMaps\\PowerModel",".", 
                        paste(c(Powers[k],"_",all.combos[[j]]), collapse=''), sep="")   
print(output.dir)
model <- sum(predictors[[all.combos[[j]]]])^k/max(unique(sum(predictors[[all.combos[[j]]]])^k))
       writeRaster(model, paste(output.dir, "costmap.asc", sep=""),
                format="ascii")
    print(paste(Powers[k], j, " of ", length(all.combos), ": ", Sys.time(), sep=""))
  }
}

####single predictor Power Model

Powers<-c(.5,1,1.5,2)

for(k in 1:length(Powers)){

for (i in 1:pred.num){
model<-(predictors[[i]]^Powers[k])/max(unique((predictors[[i]])^Powers[k]))
output.dir <- paste("H:\\THESISDATA\\OutputCostMaps\\PowerModel",".", 
                        paste(c(Powers[k],"_",i), collapse=''), sep="")   
print(output.dir)
       writeRaster(model, paste(output.dir, "costmap.asc", sep=""),
                format="ascii",overwrite=TRUE)
    print(paste(Powers[k], i, " of ", length(all.combos), ": ", Sys.time(), sep=""))
  }

}


###Power functions Trumbo et al. 2013
  for(j in 1:length(all.combos)){
output.dir <- paste("H:\\THESISDATA\\OutputCostMaps\\TrumboModel",".", 
                        paste(all.combos[[j]], collapse=''), sep="")   
print(output.dir)
model <- 100^((sum(predictors[[all.combos[[j]]]]))/max(unique(sum(predictors[[all.combos[[j]]]]))))
       writeRaster(model, paste(output.dir, "costmap.asc", sep=""),
                format="ascii")
    print(paste( j, " of ", length(all.combos), ": ", Sys.time(), sep=""))
  }
  
####Power Functions Trumbo et al. 2013

for (i in 1:pred.num){
model<-100^(predictors[[i]]/max(unique(predictors[[i]])))
output.dir <- paste("H:\\THESISDATA\\OutputCostMaps\\TrumboModel",".", 
                        paste(i, collapse=''), sep="")   
print(output.dir)
       writeRaster(model, paste(output.dir, "costmap.asc", sep=""),
                format="ascii")
    print(paste(i, " of ", length(all.combos), ": ", Sys.time(), sep=""))
  }

###inverse Power functions Trumbo et al. 2013
  for(j in 1:length(all.combos)){
output.dir <- paste("H:\\THESISDATA\\OutputCostMaps\\TrumboINVModel",".", 
                        paste(all.combos[[j]], collapse=''), sep="")   
print(output.dir)
model <- 100-100^(1-((sum(predictors[[all.combos[[j]]]]))/max(unique(sum(predictors[[all.combos[[j]]]])))))
       writeRaster(model, paste(output.dir, "costmap.asc", sep=""),
                format="ascii")
    print(paste( j, " of ", length(all.combos), ": ", Sys.time(), sep=""))
  }
  
####inverse Power Functions Trumbo et al. 2013

for (i in 1:pred.num){
model<-100-100^(1-(predictors[[i]]/max(unique(predictors[[i]]))))
output.dir <- paste("H:\\THESISDATA\\OutputCostMaps\\TrumboINVModel",".", 
                        paste(i, collapse=''), sep="")   
print(output.dir)
       writeRaster(model, paste(output.dir, "costmap.asc", sep=""),
                format="ascii")
    print(paste(i, " of ", length(all.combos), ": ", Sys.time(), sep=""))
  }

############################################################
