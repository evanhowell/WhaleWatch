#This file installs the variables SST and Chla from ERDDAP

#Install and load libraries
is.installed <- function(x){
    is.element(x, installed.packages()[,1])
  } 

if (!is.installed("ncdf")){
    install.packages("ncdf")
  }
if (!is.installed("gmt")){
    install.packages("gmt")
  }
if (!is.installed("RCurl")){
    install.packages("RCurl")
  }
  
library(ncdf)
library(gmt)
library(RCurl)

#Download chl and change dataframe format for GMT to regrid chlorophyll to match SST
f = CFILE("chl.nc",mode="wb")
curlPerform(url="http://coastwatch.pfeg.noaa.gov/erddap/griddap/erdMWchlamday.nc?chlorophyll[(last)][(0.0):1:(0.0)][(30):1:(49)][(225):1:(245)]",writedata=f@ref) 
close(f)

#Download sst and change dataframe format for GMT to regrid chlorophyll to match SST
f = CFILE("sst.nc",mode="wb")
curlPerform(url="http://coastwatch.pfeg.noaa.gov/erddap/griddap/erdMWsstdmday.nc?sst[(last)][(0.0):1:(0.0)][(30):1:(49)][(225):1:(245)]",writedata=f@ref) 
close(f)

#Run a check for the same time frame
sstnc = open.ncdf("sst.nc",write=FALSE)
chlnc = open.ncdf("chl.nc",write=FALSE)

if((att.get.ncdf(sstnc,0,"time_coverage_start")$value == att.get.ncdf(chlnc,0,"time_coverage_start")$value)){
	print("SST and Chla have the same time period")
	print(att.get.ncdf(sstnc,0,"time_coverage_start")$value)
	}else{ 
	stop("Not the same year/month")
}

month = as.numeric(substring(att.get.ncdf(sstnc,0,"time_coverage_start")$value,6,7))

close.ncdf(sstnc)
close.ncdf(chlnc) #necessary?

#Filter to desired size. Delete if filtering chl values prior to grdfilter
gmt.system("grdfilter chl.nc?chlorophyll -D0 -Fm0.5 -R225/245/30N/49N -I0.25/0.25 -Gchlgridded_grdfilter.grd")
gmt.system('grd2xyz chlgridded_grdfilter.grd',file="chl.xyz",append=F)

gmt.system("grdfilter sst.nc?sst -D0 -Fm0.5 -R225/245/30N/49N -I0.25/0.25 -Gsstgridded_grdfilter.grd") #using a median filter. 
gmt.system('grd2xyz sstgridded_grdfilter.grd',file="sst.xyz",append=F)

#Combine sst, chl, bathy into one file
sst = read.table("sst.xyz")
chl = read.table("chl.xyz")
bathy = read.table("bathy.txt")
colnames(sst) = c("longitude","latitude","sst")
colnames(chl) = c("longitude","latitude","chl")
merged = merge(merge(sst,bathy,by=c("latitude","longitude")),chl,by=c("latitude","longitude"))
colnames(merged) = c("lat","lon","sst","bathy","chl")
write.csv(merged,"WhaleWatchFactors.csv",row.names=FALSE)

#Remove unnessary files
file.remove("chl.nc","chl.xyz","sst.nc","sst.xyz","sstgridded_grdfilter.grd","chlgridded_grdfilter.grd")