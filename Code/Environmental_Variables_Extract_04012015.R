#This file downloads the environmental variables for the model. It names the variables properly to run in the GAMM prediction.
#This currently has CHLA, SST, and Y-Wind operational.

#Install and load libraries
setwd("/Users/elliotthazen/Dropbox/Documents/R/Blue_whales/Evan/")

is.installed <- function(x){
    is.element(x, installed.packages()[,1])
  } 
focalsd <- function(ncvals,xmin,xmax,ymin,ymax,xres=7,yres=7){
  if (!is.installed("raster")){
    install.packages("raster")
  }
#  ncvals<-get.var.ncdf(ncdffile)
#  v1<-ncdffile$var[[1]]
#  varsize <- v1$varsize
#  ndims <- v1$ndims
#  nt <- varsize[ndims]

#  nclats<-v1$dim[[1]]$vals
#  nclons<-v1$dim[[2]]$vals
  ncraster<-raster(ncvals,xmn=xmin,xmx=xmax,ymn=ymin,ymx=ymax)
  r<-ncraster
  #ncells<-1/res(sstraster)[1]
  #ncextent<-extent(min(nclats),max(nclats),min(nclons),max(nclons))
  
  #r<-setExtent(r,ncextent)
  #ncells<-1/res(r)
  
  rsd = focal(r, w=matrix(1,nrow=yres,ncol=xres), fun=sd,na.rm=TRUE)
  rsd = flip(t(rsd),2)
  extent(rsd)<-c(extent(rsd)@ymin,extent(rsd)@ymax,extent(rsd)@xmin,extent(rsd)@xmax)
  return(rsd)
}


if (!is.installed("gmt")){
    install.packages("gmt")
  }
if (!is.installed("SDMTools")){
    install.packages("SDMTools")
  }
if (!is.installed("ncdf")){
    install.packages("ncdf")
  }
  
if (!is.installed("RCurl")){
    install.packages("RCurl")
  }

if (!is.installed("raster")){
    install.packages("raster")
  }
  
library(gmt)
library(SDMTools)
library(ncdf)
library(RCurl)
library(raster)

#Download chl and change dataframe format for GMT to regrid chlorophyll to match SST
#Use [(last)] when obtaining real-time data from ERDDAP, e.g. [(2014-08-16T00:00:00Z):1:(2014-08-16T00:00:00Z)] = [(last)]
f = CFILE("chl.nc",mode="wb")
curlPerform(url="http://coastwatch.pfeg.noaa.gov/erddap/griddap/erdMWchlamday.nc?chlorophyll[(2008-11-16T00:00:00Z):1:(2008-11-16T00:00:00Z)][(0.0):1:(0.0)][(30):1:(49)][(225):1:(245)]",writedata=f@ref) 
close(f)

#Download SST and change dataframe format for GMT to regrid 
f = CFILE("sst.nc",mode="wb")
curlPerform(url="http://coastwatch.pfeg.noaa.gov/erddap/griddap/erdMWsstdmday.nc?sst[(2008-11-16T00:00:00Z):1:(2008-11-16T00:00:00Z)][(0.0):1:(0.0)][(30):1:(49)][(225):1:(245)]",writedata=f@ref) 
close(f)

#Download wind and change dataframe format for GMT to regrid to match other variables
f = CFILE("qswind.nc",mode="wb") #for older files
curlPerform(url="http://coastwatch.pfeg.noaa.gov/erddap/griddap/erdQSwindmday.nc?y_wind[(2008-11-16T12:00:00Z):1:(2008-11-16T12:00:00Z)][(10.0):1:(10.0)][(30):1:(49)][(225):1:(245)]",writedata=f@ref) 
close(f)
f = CFILE("qawind.nc",mode="wb") #for new files
curlPerform(url="http://coastwatch.pfeg.noaa.gov/erddap/griddap/erdQAwindmday.nc?y_wind[(2008-11-16T12:00:00Z):1:(2008-11-16T12:00:00Z)][(10.0):1:(10.0)][(30):1:(49)][(225):1:(245)]",writedata=f@ref) 
close(f)

#Download Ekman Upwelling and change dataframe format for GMT to regrid the same way to match other variables
#Out of date
f = CFILE("eke.nc",mode="wb") 
curlPerform(url="http://coastwatch.pfeg.noaa.gov/erddap/griddap/erdQAstressmday.nc?upwelling[(2008-11-16T00:00:00Z):1:(2008-11-16T00:00:00Z)][(0.0):1:(0.0)][(30):1:(49)][(225):1:(245)]",writedata=f@ref) 
close(f)

#Download SSH deviation and change dataframe format for GMT to regrid the same way to match other variables
#Out of date. Need to update source.
#Gridded at 0.25 so no regridding done
f = CFILE("sshd.nc",mode="wb") 
curlPerform(url="http://coastwatch.pfeg.noaa.gov/erddap/griddap/erdTAsshmday.nc?sshd[(2008-11-16T00:00:00Z):1:(2008-11-16T00:00:00Z)][(0.0):1:(0.0)][(29):1:(49)][(224):1:(245)]",writedata=f@ref)
close(f)

#Gridded at 0.25 so no regridding done
f = CFILE("ugeo.nc",mode="wb") 
curlPerform(url="http://coastwatch.pfeg.noaa.gov/erddap/griddap/erdTAgeomday.nc?u_current[(2008-11-16T00:00:00Z):1:(2008-11-16T00:00:00Z)][(0.0):1:(0.0)][(29):1:(49)][(224):1:(245)]",writedata=f@ref)
close(f)

#Gridded at 0.25 so no regridding done
f = CFILE("vgeo.nc",mode="wb") 
curlPerform(url="http://coastwatch.pfeg.noaa.gov/erddap/griddap/erdTAgeomday.nc?v_current[(2008-11-16T00:00:00Z):1:(2008-11-16T00:00:00Z)][(0.0):1:(0.0)][(29):1:(49)][(224):1:(245)]",writedata=f@ref)
close(f)

#Run a check for the same time frame. Every file should be the same month.
sstnc = open.ncdf("sst.nc",write=FALSE)
chlnc = open.ncdf("chl.nc",write=FALSE)
windnc = open.ncdf("qawind.nc",write=FALSE)
windnc = open.ncdf("qswind.nc",write=FALSE)
ekenc = open.ncdf("eke.nc",write=FALSE)
sshdnc = open.ncdf("sshd.nc",write=FALSE)
ugeonc = open.ncdf("ugeo.nc", write=FALSE) # Added by EAH for u component
vgeonc = open.ncdf("vgeo.nc", write=FALSE) # Added by EAH for v component

sstvals<-get.var.ncdf(sstnc)
sshvals<-get.var.ncdf(sshdnc)
sstlon=get.var.ncdf(sstnc,"longitude")
sstlat=get.var.ncdf(sstnc,"latitude")
sshlon=get.var.ncdf(sshdnc,"longitude")
sshlat=get.var.ncdf(sshdnc,"latitude")

v1<-sstnc$var[[1]]
varsize <- v1$varsize
ndims <- v1$ndims
ntsst <- varsize[ndims]

v1<-sshdnc$var[[1]]
varsize <- v1$varsize
ndims <- v1$ndims
ntssh <- varsize[ndims]


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
close.ncdf(ekenc)
close.ncdf(sshdnc)
close.ncdf(ugeonc)
close.ncdf(vgeonc)

#Filter to desired size
gmt.system("grdfilter chl.nc?chlorophyll -D0 -Fm0.5 -R225/245/30N/49N -I0.25/0.25 -Gchlgridded_grdfilter.grd")
gmt.system('grd2xyz chlgridded_grdfilter.grd',file="chl.xyz",append=F)

gmt.system("grdfilter sst.nc?sst -D0 -Fm0.5 -R225/245/30N/49N -I0.25/0.25 -Gsstgridded_grdfilter.grd") #using a median filter. 
gmt.system('grd2xyz sstgridded_grdfilter.grd',file="sst.xyz",append=F)

#Wind is at 0.25  
gmt.system('grd2xyz qawind.nc?y_wind',file="wind.xyz",append=F)
gmt.system('grd2xyz qswind.nc?y_wind',file="wind.xyz",append=F)
#gmt.system('grd2xyz sshd.nc?sshd',file="sshd.xyz",append=F)
#gmt.system("grdfilter wind.nc?y_wind -D0 -Fm0.5 -R225/245/30N/49N -I0.25/0.25 -Gwindgridded_grdfilter.grd") #using a median filter. 
#gmt.system('grd2xyz windgridded_grdfilter.grd',file="wind.xyz",append=F)

#ERRORS
gmt.system("grdfilter sshd.nc?sshd -D0 -Fm0.5 -R225/245/30N/49N -I0.25/0.25 -Gsshdgridded_grdfilter.grd") #using a median filter. 
gmt.system('grd2xyz sshdgridded_grdfilter.grd',file="sshd.xyz",append=F)

gmt.system("grdfilter eke.nc?upwelling -D0 -Fm0.5 -R225/245/30N/49N -I0.25/0.25 -Gekegridded_grdfilter.grd") #using a median filter. 
gmt.system('grd2xyz ekegridded_grdfilter.grd',file="eke.xyz",append=F)

gmt.system("grdfilter ugeo.nc?u_current -D0 -Fm0.5 -R225/245/30N/49N -I0.25/0.25 -Gugeogridded_grdfilter.grd") #using a median filter. 
gmt.system('grd2xyz ugeogridded_grdfilter.grd',file="ugeo.xyz",append=F)

gmt.system("grdfilter vgeo.nc?v_current -D0 -Fm0.5 -R225/245/30N/49N -I0.25/0.25 -Gvgeogridded_grdfilter.grd") #using a median filter. 
gmt.system('grd2xyz vgeogridded_grdfilter.grd',file="vgeo.xyz",append=F)

#Get right grids
sst = read.table("sst.xyz")
spsst<-sst
coordinates(spsst) <- ~ V1 + V2     #### check order here of V1 and V2
gridded(spsst) <- TRUE
# coerce to raster
rasterDF <- raster(spsst)

#calculate SD for sst
rsd<-focalsd(sstvals,min(sstlon),max(sstlon),min(sstlat),max(sstlat),29,29)
rsdregrid<-raster::resample(rsd, rasterDF, method="bilinear")
#rsdvals<-extract(rsdregrid,spsst)
rnc = writeRaster(rsdregrid,filename="sstsd-working.nc",format="CDF",overwrite=TRUE) # Write to netcdf file

#run grd2xyz through blockmean to regrid to 0.25x0.25 resolution.
gmt.system("grd2xyz sstsd-working.nc",file="sstsd-working.xyz")
gmt.system("blockmean sstsd-working.xyz -Rd225/245/30/49 -I0.25/0.25", file="sstsd-blockmean.xyz")
gmt.system("grdfilter sstsd-working.nc?layer -D0 -Fm0.5 -R225/245/30N/49N -I0.25/0.25 -Gsstsd_grdfilter.grd") #using a median filter. 
gmt.system('grd2xyz sstsd_grdfilter.grd',file="sstsd.xyz",append=F)

#calculate SD for ssh
rsd<-focalsd(sshvals,min(sshlon),max(sshlon),min(sshlat),max(sshlat),5,5)
#rsd<-focalsd(sshvals,min(sshlat),max(sshlat),min(sshlon),max(sshlon),5,5)
rsdregrid<-raster::resample(rsd, rasterDF, method="bilinear")
rnc = writeRaster(rsdregrid,filename="sshsd-working.nc",format="CDF",overwrite=TRUE) # Write to netcdf file

#run grd2xyz through blockmean to regrid to 0.25x0.25 resolution.
gmt.system("grd2xyz sshsd-working.nc",file="sshsd-working.xyz")
gmt.system("blockmean sshsd-working.xyz -Rd225/245/30/49 -I0.25/0.25", file="sshsd-blockmean.xyz")
gmt.system("grdfilter sshsd-working.nc?layer -D0 -Fm0.5 -R225/245/30N/49N -I0.25/0.25 -Gsshsd_grdfilter.grd") #using a median filter. 
gmt.system('grd2xyz sshsd_grdfilter.grd',file="sshsd.xyz",append=F)


#Combine sst, chl, bathy into one file
sst = read.table("sst.xyz")
chl = read.table("chl.xyz")
wind = read.table("wind.xyz")
bathy = read.table("bathy.txt")
sshd = read.table("sshd.xyz")
sstrms = read.table("sstsd.xyz")
sshrms = read.table("sshsd.xyz")
ugeo = read.table("ugeo.xyz")
vgeo = read.table("vgeo.xyz")
bathy = read.table("bathy.txt")
#bathyrms = read.table("bathyrms.txt")
#eke = read.table("eke.xyz")
eke = ugeo
eke[,3] = 1/2*(ugeo[,3]^2+vgeo[,3]^2)

colnames(sst) = c("longitude","latitude","sst")
colnames(chl) = c("longitude","latitude","chl")
colnames(wind) = c("longitude","latitude","wind")
colnames(eke) = c("longitude","latitude","eke")
colnames(sshd) = c("longitude","latitude","sshd")
colnames(sstrms) = c("longitude","latitude","sstrms")
colnames(sshrms) = c("longitude","latitude","sshrms")
colnames(ugeo) = c("longitude","latitude","ugeo")
colnames(vgeo) = c("longitude","latitude","vgeo")
#colnames(bathy) = c("longitude","latitude","bathy")
#colnames(bathyrms) = c("longitude","latitude","bathyrms")
colnames(eke) = c("longitude","latitude","eke")

merged = merge(merge(merge(merge(merge(merge(merge(sst,bathy,by=c("latitude","longitude")),chl,by=c("latitude","longitude")),wind,by=c("latitude","longitude")),eke,by=c("latitude","longitude")),sshd,by=c("latitude","longitude")),sstrms,by=c("latitude","longitude")),sshrms,by=c("latitude","longitude"))
colnames(merged) = c("lat","lon","sst","bathy","bathyrms","slope","aspect","chl","uy10","eke","sshd","sstrms","sshrms")

#merged2 =  merge(merge(merged,eke,by=c("latitude","longitude")),sshd,by=c("latitude","longitude"))
#Names need to be in the same format/name as GAMM models
#colnames(merged2) = c("lat","lon","sst","bathy","bathyrms","chl","uy10","eke","sshd")
#write.csv(merged,sprintf("WhaleWatchFactors_%s_%s_Model4.csv",month,year),row.names=FALSE)
merged$month = month
write.csv(merged,sprintf("WhaleWatchFactors_%s_%s.csv",month,year),row.names=FALSE)

file.remove("chl.nc","chl.xyz","sst.nc","sst.xyz","sshd.nc","wind.nc","eke.nc","sstgridded_grdfilter.grd","chlgridded_grdfilter.grd","wind.xyz","windgridded_grdfilter.grd","ekegridded_grdfilter.grd","eke.xyz")

