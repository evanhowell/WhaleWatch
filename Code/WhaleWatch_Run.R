#Start "cd C:\path\bin\Rgui.exe CMD BATCH C:\path\WhaleWatch_Run.r"

#Set working directories
setwd('/Users/evan.howell/lib/git/WhaleWatch') #Change this when making live
path = getwd()
codedir = paste(path,'/Code',sep='')
modeldir = paste(path,'/ModelRuns',sep='')
datadir = paste(path, '/Data',sep='')
  
if (file.exists(paste(datadir,'/bathy.txt',sep=''))){
	print("Bathymetry data present, moving to next command")
}else{
	source(paste(codedir,'get_Bathymetry.R',sep=''))
}

source(paste(codedir,'RCurl_Chla_SST.R',sep=''),print.eval=TRUE)
source(paste(codedir,'Predict_GAMMv0.2.R',sep=''),print.eval=TRUE)
source(paste(codedir,'Plot_FitMean_GMT.R',sep=''),print.eval=TRUE)