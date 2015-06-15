#uncomment for batch mode. Comment for testing
#Start "cd C:\path\bin\Rgui.exe CMD BATCH C:\path\WhaleWatch_Run.r"

#Set working directories
setwd('/Users/evan.howell/lib/git/WhaleWatch') #Change this when making live
path = getwd()
codedir = paste(path,'/Code',sep='')
modeldir = paste(path,'/ModelRuns',sep='')
datadir = paste(path, '/Data',sep='')

#Load global functions used in process
source("Code/load_Functions.R")

#Set up logfile
logfile <- file("logs/templog", open="wt")
sink(logfile, type=c("output","message")) #set all output to templog

#set up initial log with helpful information
logprint('Starting operations to make WhaleWatch product')
  
if (file.exists(paste(datadir,'/bathy.txt',sep=''))){
	logprint("Bathymetry data present, moving to next command")
}else{
  logprint("Bathymetry data does not exist, creating bathymetry file")
  get_Bathymetry()
}

logprint("Running function get_EnvData.R")
get_EnvData()
logprint("Running file get_EnvData.R")
source(paste(codedir,'Predict_GAMMv0.2.R',sep=''),print.eval=TRUE)
logprint("Running file get_EnvData.R")
source(paste(codedir,'Plot_FitMean_GMT.R',sep=''),print.eval=TRUE)

newlogfile = paste('logs/log',Sys.time(),'.log',sep='')

logprint(paste("Closing temporary logfile and renaming to file ", newlogfile))
close(logfile)

file.rename(logfile, newlogfile)
sink(type = c("output", "message")) #reset all messages to STDOUT and STDERR