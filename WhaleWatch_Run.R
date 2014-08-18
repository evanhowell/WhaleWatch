
#Start "cd C:\path\bin\Rgui.exe CMD BATCH C:\path\WhaleWatch_Run.r"
#####################
#NOAA/JIMAR West Coast WhaleWatch script
#Aimee Hoover & Evan Howell, 06/23/2014
#This runs multiple scripts from the main command.
#####################

 
#Only get bathymetry data if file is not present (static variable)
if (file.exists("bathy.txt")){
	print("Bathymetry data present, moving to next command")
}else{
	source('bathymetry_grdfilter.R')
}

source('Environmental_Variables_Extract.R',print.eval=TRUE)
source('Predict_GAMM.R',print.eval=TRUE)
source('Plot_FitMean_GMT.R',print.eval=TRUE)
