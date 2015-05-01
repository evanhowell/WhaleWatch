#Start "cd C:\path\bin\Rgui.exe CMD BATCH C:\path\WhaleWatch_Run.r"

#setwd("H:/Aimee_H/2014_Whalewatch/WhaleWatchGAMMs")
  setwd('~/Dropbox/Documents/R/Blue_whales/Data/')
  wd<-getwd(); path<-wd; modeldir<-'/PredictCCSv1/'; modeldir<-'/PredictV2/'
  codedir<-'~/Dropbox/Documents/R/Blue_whales/Evan/'
  
if (file.exists("bathy.txt")){
	print("Bathymetry data present, moving to next command")
}else{
	source(paste(codedir,'bathymetry_grdfilter.R',sep=''))
}

source(paste(codedir,'RCurl_Chla_SST.R',sep=''),print.eval=TRUE)
source(paste(codedir,'Predict_GAMMv0.2.R',sep=''),print.eval=TRUE)
source(paste(codedir,'Plot_FitMean_GMT.R',sep=''),print.eval=TRUE)