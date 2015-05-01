library(mgcv)
library(ape)
library(ncf)
library(ncdf)
library(ncdf4)
library(spdep)
require(maps)       # for map.where
require(mapdata)    # for worldHires
library(maptools)
require(ROCR)
data(wrld_simpl)

 plotROC <- function(truth, predicted, ...){
   pred <- prediction(abs(predicted), truth)    
   perf <- performance(pred,"tpr","fpr")

   plot(perf, ...)
}

  setwd('~/Dropbox/Documents/R/Blue_whales/Data/')
  wd<-getwd(); path<-wd
  out.dir = sprintf("%s/Predict/",wd)
  alldata<-read.csv(paste(out.dir,"xtract_sightings_SSHrms.csv",sep=""))
  #### THROW OUT PREVIOUS MODEL RUNS
  alldata<-alldata[1:34,]
  
  loadhistory(file=paste(getwd(),"WWatch.Rhistory",sep="/")) # default is ".Rhistory"
  load(file=paste(wd,"WWatch.RData",sep="/"))
  
  ####### ROCR comparison for track/pseudoabsence

AUCfull<-c(1:numruns); AUCred<-c(1:numruns)
for(l in 1:numruns) {

### THIS CODE COMPARES TWO MODELS - reduced vs. full
redmodel<-read.csv(paste(path,"/Predict/GAMdataRun_SSHrms",l,".csv",sep=''))
fullmodel<-read.csv(paste(path,"/Predict/GAMdataRun",l,".csv",sep=''))

#rednamesvect<-c(paste("RedFit_",l,sep=''),paste("RedTrue_",l,sep=''))
#fullnamesvect<-c(paste("FullFit_",l,sep=''),paste("FullTrue_",l,sep=''))
#redvect<-cbind(redmodel$fit,redmodel$presabs); names(redvect)<-rednamesvect
#fullvect<-cbind(fullmodel$fit,fullmodel$presabs); names(fullvect)<-fullnamesvect


if(l>2) {
	redallfit<-cbind(redallfit,redmodel$fit); fullallfit<-cbind(fullallfit,fullmodel$fit); redalltrue<-cbind(redalltrue,redmodel$presabs); fullalltrue<-cbind(fullalltrue,fullmodel$presabs)
	} else {		
	redallfit<-redmodel$fit; fullallfit<-fullmodel$fit; redalltrue<-redmodel$presabs; fullalltrue<-fullmodel$presabs}

jpeg(paste(path,"/Predict/SSHrms_ROCR",l,".jpg",sep=''))
pred <- prediction(abs(redmodel$fit), redmodel$presabs)    
perf <- performance(pred,"tpr","fpr",measure="auc")
#print(perf$y.values)
plotROC(redmodel$presabs,redmodel$fit, colorize = TRUE)
dev.off()

AUCred[l]<-as.numeric(perf@y.values)

jpeg(paste(path,"/Predict/Full_ROCR",l,".jpg",sep=''))
pred <- prediction(abs(fullmodel$fit), fullmodel$presabs)    
perf <- performance(pred,"tpr","fpr",measure="auc")
plotROC(fullmodel$presabs,fullmodel$fit, colorize = TRUE)
dev.off()

jpeg(paste(path,"/Predict/Both_ROCRv2",l,".jpg",sep=''))
   pred1 <- prediction(abs(redmodel$fit), redmodel$presabs)    
   perf1 <- performance(pred1,"tpr","fpr")
   pred2 <- prediction(abs(fullmodel$fit), fullmodel$presabs)    
   perf2 <- performance(pred2,"tpr","fpr")

	plot(perf1, col = "red")
	plot(perf2, add = TRUE, col = "blue")
#	plot(perf1, colorize = TRUE)
#	plot(perf2, add = TRUE, colorize = TRUE)

dev.off()


AUCfull[l]<-as.numeric(perf@y.values)

}

print(AUCfull-AUCred)
write.csv(cbind(AUCfull,AUCred),file = paste(path,"/Predict/AUCvalues_SSHrms.csv",sep=''))

meanredfit<-mean(redallfit, na.rm=T); meanfullfit<-mean(fullallfit, na.rm=T); meanredtrue<-mean(redalltrue, na.rm=T); meanfulltrue<-mean(fullalltrue, na.rm=T)

jpeg(paste(path,"/Predict/Mean_ROCR_SSHrms.jpg",sep=''))
   pred1 <- prediction(abs(redallfit), redalltrue)    
   perf1 <- performance(pred1,"tpr","fpr")
   pred2 <- prediction(abs(fullallfit), fullalltrue)    
   perf2 <- performance(pred2,"tpr","fpr")

	plot(perf1, col = "red")
	plot(perf2, add = TRUE, col = "blue")
#	plot(perf1, colorize = TRUE)
#	plot(perf2, add = TRUE, colorize = TRUE)

dev.off()

### PREDICT.GAM ON SIGHTINGS AND NULL----------------------------------

numruns = 40; AUCvals<-c(1:numruns)
#rsq<-c(1:numruns); AICl<-c(1:numruns); tabs<-c(1:numruns)
alldata<-data
for(l in 1:numruns) {

#bwhaleGAMM<-gamm(presabs~s(sst,bs="ts",k=5)+s(log(chl),bs="ts",k=5)+s(bathy,k=5,bs="ts")+s(sshrms,k=5,bs="ts"), random=list(ptt=~1),family=binomial, niterPQL=50, data=get(paste("GAMdataRun",l,sep='')))
assign(bwhaleGAMM,get(paste("bGAMM",l,sep='')))

### MAKE SURE data columns match sst,chl and bathy
newdata<-predict.gam(bwhaleGAMM$gam,data,se=TRUE, type="response")
names(newdata)<-c(paste("fit",l,sep=""),paste("se.fit",l,sep=""))
do.call("<-",list(paste("new_sight_data",l,sep=''),newdata))
alldata<-cbind(alldata,get(paste("new_sight_data",l,sep='')))

### ROCR SIGHTINGS AND NULL--------------------------------------------

jpeg(paste(path,"/Predict/Sightings_ROCR_SSHrms",l,".jpg",sep=''))
pred <- prediction(abs(as.vector(alldata[,33+l*2])), alldata$presabs)    
perf <- performance(pred,"tpr","fpr",measure="auc")
#print(perf$y.values)
plotROC(alldata$presabs,as.vector(alldata[,33+l*2]), colorize = TRUE)
dev.off()

AUCvals[l]<-as.numeric(perf@y.values)

}  

write.table(AUCvals,file=paste(out.dir,"AUC_sightings_SSHrms.csv",sep=""),sep=",", row.names=FALSE, col.names=TRUE)

save.image(file=paste(wd,"WWatch.RData",sep="/"))
rm(list=ls())

