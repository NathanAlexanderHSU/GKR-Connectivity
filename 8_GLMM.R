require(lme4)
require(MuMIn)





###Code adapted from LDG-2016 course week 7 supplementary materials



#####DistMat and IBD distances will be consistent across each time step (time step151-651)
	setwd(choose.dir())
	IBR<-list.files()
IBR
	IBDDist<-as.matrix(read.csv(choose.files(),header=TRUE))
	individuals<-IBDDist[,1]
individuals
	IBDDist<-IBDDist[,-1]
	diag(IBDDist)<-NA
	IBD<-as.vector(IBDDist)
	IBD<-na.omit(IBD)
IBD
	Gen<-as.matrix(read.csv(choose.files()),header=TRUE)
head(Gen[1:10,1:10])

###GENALEX CODE###
##	Gen<-Gen[,-240] 
##	GenA<-Gen
##	Gen<-GenA
##	Gen[upper.tri(Gen)]<-t(Gen)[upper.tri(Gen)]
head(Gen[1:10,1:10])


##3Dps CODE###
	Gen<-Gen[,-1]
	diag(Gen)<-NA
	Gen<-as.numeric(Gen)

	GenDist<-as.vector(Gen)
	GenDist<-na.omit(GenDist)

	pop1 <- t(matrix(c(individuals),length(individuals),length(individuals)))
		diag(pop1)<-NA
		pop1<-as.vector(pop1)
		pop1<-na.omit(pop1)

	pop2 <- (matrix(c(individuals),length(individuals),length(individuals)))
		diag(pop2)<-NA
		pop2<-as.vector(pop2)
		pop2 <- na.omit(pop2)

	GenDist <- scale(GenDist)      # Each predictor variable is centred around the mean.
	IndIBD<-scale(IBD)	

Model.Out <- vector(mode = "list", length = length(IBR)+1)

for(i in 1:length(IBR)){
	DistMat<-as.matrix(read.csv(paste(c(getwd(),"/",IBR[i]),sep="",collapse=""),header=TRUE))
head(DistMat)
	DistMat<-DistMat[,-1]
	diag(DistMat)<-NA
	IndDist<-as.vector(DistMat)
	IndDist<-na.omit(IndDist)

	IndIBR<-scale(IndDist)
	Distmat <- data.frame(pop1, pop2, IndIBR, IndIBD, GenDist)

		##I don't know what this part is for; I'm guessing it was needed for example data.
		##it inverts a pop1/pop2 column for our data so do not use 	  	
		#n <- nrow(Distmat)
		#Distmat[c(2,n-1),1:2] <- Distmat[c(2,n-1),2:1]
    

	lmer_form <- as.formula("GenDist ~ IndIBR + IndIBD + (1|pop1)")
    	Zl <- lapply(c("pop1","pop2"), function(nm) Matrix:::fac2sparse(Distmat[[nm]],"d", drop=FALSE))
    	ZZ <- Reduce("+", Zl[-1], Zl[[1]])

	mod <- lFormula(GenDist ~ IndIBR + IndIBD  + (1|pop1), data = Distmat, REML = TRUE)
	mod$reTrms$Zt <- ZZ
	dfun <- do.call(mkLmerDevfun,mod)
    	opt <- optimizeLmer(dfun)

    	Model.Out[[i+1]] <- mkMerMod(environment(dfun), opt, mod$reTrms,fr = mod$fr)

}



	###create a null model
	mod2 <- lFormula(GenDist ~ 1  + (1|pop1), data = Distmat, REML = TRUE)
	mod2$reTrms$Zt <- ZZ
	dfun2 <- do.call(mkLmerDevfun,mod2)
   	opt2 <- optimizeLmer(dfun2)
	Model.Out[[1]] <- mkMerMod(environment(dfun2), opt2, mod$reTrms,fr = mod2$fr)

##Model Selection

Mod.Sel<-data.frame()
	Mod.Aic<-AICc(Model.Out[[1]],REML=TRUE)	
	Model<-cbind("NULL",Mod.Aic)
	Mod.Sel<-rbind(Mod.Sel,Model)

for(i in 1:length(IBR)){	
	Mod.Aic<-AICc(Model.Out[[i+1]],REML=TRUE)	
	Model<-cbind(IBR[i],Mod.Aic)
	Mod.Sel<-rbind(Mod.Sel,Model)
}
colnames(Mod.Sel)<-c("Model","AICc")
Mod.Sel$AICc<-as.vector(Mod.Sel$AICc)
Mod.Sel
Mod.Sel<-Mod.Sel[order(Mod.Sel$AICc),]
Mod.Sel

write.csv(Mod.Sel,"C:\\Users\\Nathan\\Desktop\\GKR_ModelSelection_AICc_Dps_LCP.csv")

##Goodness of Fit

	##Row et al. (2015 Ecol and Evol) found that this r.squaredGLMM function better		
	##aligned with AICc output than the R-squared beta suggested by Van Strien et al. (2012 Mol. Ecol.)
	## and Edwards et al. (2008 Stat. Med.)

Mod.Fit<-data.frame()
	R2<-r.squaredGLMM(Model.Out[[1]])
	R<-cbind("NULL",R2[1],R2[2])
	Mod.Fit<-rbind(Mod.Fit,R)
Mod.Fit
for(i in 1:(length(IBR))){
	R2<-r.squaredGLMM(Model.Out[[i]])
	R<-cbind(IBR[i],R2[1],R2[2])
	Mod.Fit<-rbind(Mod.Fit,R)
}

colnames(Mod.Fit)<-c("Model","Marginal R2","Conditional R2")
Mod.Fit
write.csv(Mod.Fit,"C:\\Users\\Nathan\\Desktop\\GKR_ModelFit_R2_Dps_LCP.csv")
Model.Out
saveRDS(Model.Out,"C:\\Users\\Nathan\\Desktop\\GKR_REML_Models_Dps_LCP.rds")



	##Vaida and Blanchard (2005 Biometrika) argue that AIC can be used for REML
	##if the question is based on a population (not-genetic term) level question.
	##Because we are concerned with the marginal R-squared, we are concerned with
	##the population/marginal AIC.  This means we can account for the random-effects
	##based on populations as a single term.

	D.AICc<-AICc(mod_1)-AICc(mod_2)

	output<-cbind(R2[1],R2[2],D.AICc)
	colnames(output)<-c("R2m","R2c","Delta.AICc")
	rownames(output)<-genetics[i]

	OUT<-rbind(OUT,output)

	OUT<-na.omit(OUT)



############################################
############################################
####r-squared beta method from Van Strien et al. (2012 Mol. Ecol.) and Row et al. (2015 Ecol and Evol)
#a<-KRmodcomp(mod_1, mod_2, betaH=0, details=0)
#summary(a)
###This function can be used to extract the F-value if we want to proceed with the r-squared beta method
#getKR(a,"aux")#,"ndf", "ddf"))#, "Fstat", "p.value",
#"F.scaling", "FstatU", "p.valueU", "aux"))

