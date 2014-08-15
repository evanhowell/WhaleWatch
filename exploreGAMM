library(mgcv)
library(ape)
library(ncf)
library(ncdf)
library(spdep)
require(maps)       # for map.where
require(mapdata)    # for worldHires
#require(ROCR)
#library(lattice)

# plotROC <- function(truth, predicted, ...){
#   pred <- prediction(abs(predicted), truth)    
#   perf <- performance(pred,"tpr","fpr")
#   plot(perf, ...)
#}

  setwd('~/Dropbox/Documents/R/Blue_whales/Data/')
  wd<-getwd(); path<-wd

  #loadhistory(file=paste(getwd(),"WWatch.Rhistory",sep="/")) # default is ".Rhistory"
  load(file=paste(wd,"WWatch.RData",sep="/"))

  Xtract.dir = sprintf('%s/Xtracto/', wd)                   # NEW (4ssm)

  in.csv <- sprintf('%sbluewhale_actual_xtrack_v3a.csv', Xtract.dir) 
#  sim.csv<- sprintf('%sww_crw_subsample_v3.csv', Xtract.dir) 
  sim.csv<- sprintf('%scrw_full_sim1.csv', Xtract.dir) 
  # NEW (4ssm)
in.csv <- sprintf('%sDistSampActual.csv', Xtract.dir) 
sim.csv<- sprintf('%sDistSampCRW.csv', Xtract.dir) 

 simdata<-read.csv(file=sim.csv,head=TRUE,sep=",")
 trackdata<-read.csv(file=in.csv,head=TRUE,sep=",")
colnames(trackdata)[27]<-"dist_shelf"
colnames(simdata)[24]<-"dist_shelf"
#trackshelf<-trackdata$dist_shelf
#simshelf<-simdata$dist_shelf

names(simdata)<-c(names(simdata)[1:9],"blendo_sst","pathfinder_sst","modis_chla","sw_chl","ssha","sshdev","sshrms","eke","qswekm","qsux10","qsuy10","bathy","bathy_rms","FPI","dist_shelf")

#### GAMMs

 alltags<-unique(simdata$ptt)
 #alltags<-c(41749,41752, 41767, 61918, 61919, 61920, 61922, 61923, 61924, 63978, 63980, 63983, 64270, 66884, 77160, 89297) #, 91973)
 rm(GAMdata)
 runs2do<-1
 numruns<-40
 for (l in runs2do:numruns){
 for (tagid in alltags){	
 	sim<-simdata[simdata$ptt==tagid,]
 	bwhale<-trackdata[trackdata$ptt==tagid,]
 	iternum<-sample(1:numruns, 1)
 	datapt<-sim$flag[sim$iteration==iternum]
 	while (datapt[1]> 50) {
	 	iternum<-sample(1:numruns, 1) 		
	 	datapt<-sim$flag[sim$iteration==iternum]
 	}
 	sim1<-sim[sim$iteration==iternum,]
	sim1$presabs<-0
 	bwhale$presabs<-1
 	bwhale$iteration<-0
 	bwhale$flag<-0
 	
 	#### Create GAMdata file with tracks and pseudotracks (randomly selected)
 	GAMdata1<-cbind(sim1$ptt,sim1$presabs,sim1$iteration,sim1$flag,sim1$lat,sim1$lon,sim1$year,sim1$month,sim1$day,sim1$pathfinder_sst,sim1$modis_chla,sim1$ssha,sim1$sshrms,sim1$eke,sim1$qswekm,sim1$qsux10,sim1$qsuy10,sim1$bathy,sim1$bathy_rms,sim1$FPI,sim1$dist_shelf)
 	GAMdata2<-cbind(bwhale$ptt,bwhale$presabs,bwhale$iteration,bwhale$flag,bwhale$lat,bwhale$lon,bwhale$year,bwhale$month,bwhale$day,bwhale$pathfinder,bwhale$modis_chla,bwhale$ssha,bwhale$sshrms,bwhale$eke,bwhale$qswekm,bwhale$qsux10,bwhale$qsuy10,bwhale$bathy,bwhale$bathy_rms,bwhale$FPI,bwhale$dist_shelf)
 	
 	for (x in 1:dim(GAMdata1)[1]){
 		if (is.na(GAMdata1[x,10])) GAMdata1[x,10]<-sim1$blendo_sst[x]    #blendo
 		if (is.na(GAMdata2[x,10])) GAMdata2[x,10]<-bwhale$blendo_sst[x]    #blendo
 		if (is.na(GAMdata1[x,11])) GAMdata1[x,11]<-sim1$sw_chl[x]     #seawifs
 		if (is.na(GAMdata2[x,11])) GAMdata2[x,11]<-bwhale$sw_chl[x]     #seawifs 		
 	}
 	GAMdata3<-rbind(GAMdata2,GAMdata1)
 	
 	if (exists('GAMdata')){
      GAMdata = rbind(GAMdata, GAMdata3)
    } else {
      GAMdata = GAMdata3
    }
 
 }
print(paste("Run #",l,sep=''))
assign(paste("GAMdataRun",l,sep=''),0)
GAMdata<-as.data.frame(GAMdata)
colnames(GAMdata)<-c("ptt","presabs","iteration","flag","lat","lon","year","month","day","sst","chl","ssh","sshrms","eke","wekm","ux10","uy10","bathy","bathyrms","FPI") #,"dist_shelf"
assign(paste("GAMdataRun",l,sep=''),GAMdata[GAMdata$bathy<0,])

rm(GAMdata)

### CURRENT GAMM to try. Could run multiple at once but am right now just trying 1 for each of the 40 GAMdataRuns.
try(bwhaleGAMM<-gamm(presabs~s(sst,bs="ts",k=5)+s(log(chl),bs="ts",k=5)+s(uy10,k=5,bs="ts")+s(bathy,k=5,bs="ts")+s(bathyrms,k=5,bs="ts")+s(lat,lon,k=5,bs="ts")+month, random=list(ptt=~1),family=binomial, niterPQL=40, data=get(paste("GAMdataRun",l,sep=''))))
do.call("<-",list(paste("bGAMM",l,sep=''),bwhaleGAMM))
AIC(bwhaleGAMM$lme)
summary(bwhaleGAMM$gam)
}

### Calculates summary statistics and writes data files
rsq<-c(1:numruns); AICl<-c(1:numruns); tabs<-c(1:numruns)
for(l in 1:numruns) {
assign(bwhaleGAMM,get(paste("bGAMM",l)))
#try(bwhaleGAMM<-gamm(presabs~s(sst,bs="ts",k=5)+s(log(chl),bs="ts",k=5)+s(uy10,k=5,bs="ts")+s(bathy,k=5,bs="ts")+s(bathyrms,k=5,bs="ts")+s(lat,lon,k=5,bs="ts")+month, random=list(ptt=~1),family=binomial, niterPQL=40, data=get(paste("GAMdataRun",l,sep=''))))
do.call("<-",list(paste("newdata",l,sep=''),cbind(get(paste("GAMdataRun",l,sep='')),newdata)))

AIC(bwhaleGAMM$lme)
rsq[l]<-summary(bwhaleGAMM$gam)[10]
AICl[l]<-AIC(bwhaleGAMM$lme)
tabs[l]<-summary(bwhaleGAMM$gam)[24]
newdata<-predict.gam(bwhaleGAMM$gam,get(paste("GAMdataRun",l,sep='')),se=TRUE, type="response")
do.call("<-",list(paste("newdata",l,sep=''),cbind(get(paste("GAMdataRun",l,sep='')),newdata)))
writefile<-get(paste("newdata",l,sep=''))
writefile<-writefile[!is.na(writefile$lon)&!is.na(writefile$lat),]
names(writefile)<-c(names(writefile)[1:21],"sefit")
write.csv(writefile,file = paste(path,"/PredictLatLon/GAMdataRun",l,".csv",sep=''))
save(bwhaleGAMM,file = paste(path,"/PredictLatLon/bwhaleGAMM",l,".RData",sep=''))
}

write.csv(tabs,file = paste(path,"/PredictLatLon/SummaryTables.csv",sep=''))
write.csv(rsq,file = paste(path,"/PredictLatLon/Rsquareds.csv",sep=''))
write.csv(AICl,file = paste(path,"/PredictLatLon/AICvals.csv",sep=''))

#### OLD MODEL ATTEMPTS HERE
#bwhaleGAMM<-gamm(presabs~s(sst,bs="ts",k=5)+s(log(chl),bs="ts",k=5)+s(log(eke),bs="ts", k=5)+s(wekm,bs="ts", k=5)+s(uy10,k=5,bs="ts")+s(bathy,k=5,bs="ts")+s(bathyrms,k=5,bs="ts")+s(log(FPI+0.0001),k=5,bs="ts"), random=list(ptt=~1),family=binomial, niterPQL=40, data=get(paste("GAMdataRun",l,sep='')))
#bwhaleGAMM<-gamm(presabs~s(sst,bs="ts",k=5)+s(log(chl),bs="ts",k=5)+s(log(eke),bs="ts", k=5)+s(wekm,bs="ts", k=5)+s(uy10,k=5,bs="ts")+s(bathy,k=5,bs="ts")+s(bathyrms,k=5,bs="ts")+s(log(FPI+0.0001),k=5,bs="ts"), random=list(ptt=~1),family=binomial, data=GAMdata)
#AIC(bwhaleGAMM$lme)
#bwhaleGAMM<-gamm(presabs~s(sst,bs="ts",k=5)+s(log(chl),bs="ts",k=5)+s(log(eke),bs="ts", k=5)+s(uy10,k=5,bs="ts")+s(bathy,k=5,bs="ts")+s(bathyrms,k=5,bs="ts")+s(log(FPI+0.0001),k=5,bs="ts"), random=list(ptt=~1),family=binomial, data=GAMdata)
#AIC(bwhaleGAMM$lme)
#bwhaleGAMM<-gamm(presabs~s(log(chl),bs="ts",k=5)+s(log(eke),bs="ts", k=5)+s(wekm,bs="ts", k=5)+s(uy10,k=5,bs="ts")+s(bathy,k=5,bs="ts")+s(bathyrms,k=5,bs="ts")+s(log(FPI+0.0001),k=5,bs="ts"), random=list(ptt=~1),family=binomial, data=GAMdata)
#AIC(bwhaleGAMM$lme)
#bwhaleGAMM<-gamm(presabs~s(sst,bs="ts",k=5)+s(log(chl),bs="ts",k=5)+s(log(eke),bs="ts", k=5)+s(bathy,k=5,bs="ts")+s(bathyrms,k=5,bs="ts")+s(log(FPI+0.0001),k=5,bs="ts"), random=list(ptt=~1),family=binomial, data=GAMdata)
#AIC(bwhaleGAMM$lme)

