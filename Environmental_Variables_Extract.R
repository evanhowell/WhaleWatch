#This file downloads the environmental variables for the model. It names the variables properly to run in the GAMM prediction.
#This currently has CHLA, SST, and Y-Wind operational.

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
#Use [(last)] when obtaining real-time data from ERDDAP, e.g. [(2014-08-16T00:00:00Z):1:(2014-08-16T00:00:00Z)] = [(last)]
f = CFILE("chl.nc",mode="wb")
curlPerform(url="http://coastwatch.pfeg.noaa.gov/erddap/griddap/erdMWchlamday.nc?chlorophyll[(2009-12-16T00:00:00Z):1:(2009-12-16T00:00:00Z)][(0.0):1:(0.0)][(30):1:(49)][(225):1:(245)]",writedata=f@ref) 
close(f)

#Download SST and change dataframe format for GMT to regrid 
f = CFILE("sst.nc",mode="wb")
curlPerform(url="http://coastwatch.pfeg.noaa.gov/erddap/griddap/erdMWsstdmday.nc?sst[(2009-12-16T00:00:00Z):1:(2009-12-16T00:00:00Z)][(0.0):1:(0.0)][(30):1:(49)][(225):1:(245)]",writedata=f@ref) 
close(f)

#Download wind and change dataframe format for GMT to regrid to match other variables
#f = CFILE("wind.nc",mode="wb") #for older files
#curlPerform(url="http://coastwatch.pfeg.noaa.gov/erddap/griddap/erdQSwindmday.nc?y_wind[(2009-12-16T12:00:00Z):1:(2009-12-16T12:00:00Z)][(10.0):1:(10.0)][(30):1:(49)][(225):1:(245)]",writedata=f@ref) 
#close(f)
f = CFILE("wind.nc",mode="wb") #for new files
curlPerform(url="http://coastwatch.pfeg.noaa.gov/erddap/griddap/erdQAwindmday.nc?y_wind[(2009-12-16T12:00:00Z):1:(2009-12-16T12:00:00Z)][(10.0):1:(10.0)][(30):1:(49)][(225):1:(245)]",writedata=f@ref) 
close(f)

#Download Ekman Upwelling and change dataframe format for GMT to regrid the same way to match other variables
#Out of date
#f = CFILE("eke.nc",mode="wb") 
#curlPerform(url="http://coastwatch.pfeg.noaa.gov/erddap/griddap/erdQAstressmday.nc?upwelling[(2009-12-16T00:00:00Z):1:(2009-12-16T00:00:00Z)][(0.0):1:(0.0)][(30):1:(49)][(225):1:(245)]",writedata=f@ref) 
#close(f)

#Download SSH deviation and change dataframe format for GMT to regrid the same way to match other variables
#Out of date. Need to update source.
#Gridded at 0.25 so no regridding done
#f = CFILE("sshd.nc",mode="wb") 
#curlPerform(url="http://coastwatch.pfeg.noaa.gov/erddap/griddap/erdTAsshmday.nc?sshd[(2009-12-16T00:00:00Z):1:(2009-12-16T00:00:00Z)][(0.0):1:(0.0)][(29):1:(49)][(224):1:(245)]",writedata=f@ref)
#close(f)

#Run a check for the same time frame. Every file should be the same month.
sstnc = open.ncdf("sst.nc",write=FALSE)
chlnc = open.ncdf("chl.nc",write=FALSE)
windnc = open.ncdf("wind.nc",write=FALSE)
#ekenc = open.ncdf("eke.nc",write=FALSE)
#sshdnc = open.ncdf("sshd.nc",write=FALSE)

if((att.get.ncdf(sstnc,0,"time_coverage_start")$value == att.get.ncdf(chlnc,0,"time_coverage_start")$value) && (att.get.ncdf(sstnc,0,"time_coverage_start")$value == att.get.ncdf(windnc,0,"time_coverage_start")$value)){
	print("Same time period")
	print(att.get.ncdf(sstnc,0,"time_coverage_start")$value)
	}else{ 
	stop("Not the same year/month")
}

#Need to automate to variables input
#if((att.get.ncdf(sstnc,0,"time_coverage_start")$value == att.get.ncdf(chlnc,0,"time_coverage_start")$value) && (att.get.ncdf(sstnc,0,"time_coverage_start")$value == att.get.ncdf(windnc,0,"time_coverage_start")$value) && (att.get.ncdf(sstnc,0,"time_coverage_start")$value == att.get.ncdf(ekenc,0,"time_coverage_start")$value) && (att.get.ncdf(sstnc,0,"time_coverage_start")$value == att.get.ncdf(sshdnc,0,"time_coverage_start")$value)){
#	print("Same time period")
#	print(att.get.ncdf(sstnc,0,"time_coverage_start")$value)
#	}else{ 
#	stop("Not the same year/month")
#}

month = as.numeric(substring(att.get.ncdf(sstnc,0,"time_coverage_start")$value,6,7))
year = as.numeric(substring(att.get.ncdf(sstnc,0,"time_coverage_start")$value,1,4))

close.ncdf(sstnc)
close.ncdf(chlnc) 
close.ncdf(windnc)
#close.ncdf(ekenc)
#close.ncdf(sshdnc)

#Filter to desired size
gmt.system("grdfilter chl.nc?chlorophyll -D0 -Fm0.5 -R225/245/30N/49N -I0.25/0.25 -Gchlgridded_grdfilter.grd")
gmt.system('grd2xyz chlgridded_grdfilter.grd',file="chl.xyz",append=F)

gmt.system("grdfilter sst.nc?sst -D0 -Fm0.5 -R225/245/30N/49N -I0.25/0.25 -Gsstgridded_grdfilter.grd") #using a median filter. 
gmt.system('grd2xyz sstgridded_grdfilter.grd',file="sst.xyz",append=F)

#Wind is at 0.25  
gmt.system('grd2xyz wind.nc?y_wind',file="wind.xyz",append=F)
#gmt.system("grdfilter wind.nc?y_wind -D0 -Fm0.5 -R225/245/30N/49N -I0.25/0.25 -Gwindgridded_grdfilter.grd") #using a median filter. 
#gmt.system('grd2xyz windgridded_grdfilter.grd',file="wind.xyz",append=F)

#gmt.system("grdfilter eke.nc?upwelling -D0 -Fm0.5 -R225/245/30N/49N -I0.25/0.25 -Gekegridded_grdfilter.grd") #using a median filter. 
#gmt.system('grd2xyz ekegridded_grdfilter.grd',file="eke.xyz",append=F)

#Want to get SSH deviation
#gmt.system("grdfilter sshd.nc?sshd -D0 -Fm0.5 -R225/245/30N/49N -I0.25/0.25 -Gsshdgridded_grdfilter.grd") #using a median filter. 
#gmt.system('grd2xyz sshdgridded_grdfilter.grd',file="sshd.xyz",append=F)

#Combine sst, chl, bathy into one file
sst = read.table("sst.xyz")
chl = read.table("chl.xyz")
wind = read.table("wind.xyz")
bathy = read.table("bathy.txt")
#eke = read.table("eke.xyz")
#sshd = read.table("sshd.xyz")
colnames(sst) = c("longitude","latitude","sst")
colnames(chl) = c("longitude","latitude","chl")
colnames(wind) = c("longitude","latitude","wind")
#colnames(eke) = c("longitude","latitude","eke")
#colnames(sshd) = c("longitude","latitude","sshd")

merged = merge(merge(merge(sst,bathy,by=c("latitude","longitude")),chl,by=c("latitude","longitude")),wind,by=c("latitude","longitude"))
colnames(merged) = c("lat","lon","sst","bathy","bathyrms","chl","uy10")

#merged2 =  merge(merge(merged,eke,by=c("latitude","longitude")),sshd,by=c("latitude","longitude"))
#Names need to be in the same format/name as GAMM models
#colnames(merged2) = c("lat","lon","sst","bathy","bathyrms","chl","uy10","eke","sshd")
#write.csv(merged,sprintf("WhaleWatchFactors_%s_%s_Model4.csv",month,year),row.names=FALSE)
merged$month = month
write.csv(merged,sprintf("WhaleWatchFactors_%s_%s.csv",month,year),row.names=FALSE)

file.remove("chl.nc","chl.xyz","sst.nc","sst.xyz","sshd.nc","wind.nc","eke.nc","sstgridded_grdfilter.grd","chlgridded_grdfilter.grd","wind.xyz","windgridded_grdfilter.grd","ekegridded_grdfilter.grd","eke.xyz")

