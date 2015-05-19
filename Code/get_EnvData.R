# This is an R script to download all environmental data used in the GAMM models. It names the variables properly to run in the GAMM prediction.
#This currently has CHLA, SST, and Y-Wind operational.

# IMPORTANT - The script assumes that it is in the correct parent directory, and then sets relative paths from there.

#Code validated to get env variables 5/18/2015 by EAH

##############HEADER########################
# Define functions used in script.

focalsd <- function(ncvals,xmin,xmax,ymin,ymax,xres=7,yres=7){
	r<-raster(ncvals,xmn=xmin,xmx=xmax,ymn=ymin,ymx=ymax)
	rsd = focal(r, w=matrix(1,nrow=yres,ncol=xres), fun=sd,na.rm=TRUE)
	rsd = flip(t(rsd),2)
	extent(rsd)<-c(extent(rsd)@ymin,extent(rsd)@ymax,extent(rsd)@xmin,extent(rsd)@xmax)
	return(rsd)
}

# Function to generalize grabbing data from ERDDAP. 
#Download chl and change dataframe format for GMT to regrid chlorophyll to match SST
#Use [(last)] when obtaining real-time data from ERDDAP, e.g. [(2014-08-16T00:00:00Z):1:(2014-08-16T00:00:00Z)] = [(last)]
curldap <- function(dapurl, outfile) {
	f = CFILE(outfile,mode="wb")
	id = curlPerform(url=dapurl,writedata=f@ref) 
	close(f)
	return(id)
}

#Load required libraries. Function pkgTest is in the file Code/load_Functions.R and should have been loaded. If not test here and load file.

if(exists("pkgTest")==FALSE) {
   print("Function pkgTest not found, loading file Code/load_Functions.R...")
   source("Code/load_Functions.R")
   }
   
pkgTest("gmt")
pkgTest("SDMTools")
pkgTest("ncdf")
pkgTest("RCurl")
pkgTest("raster")

# Get the current month and year
year = as.numeric(format(Sys.time(), "%Y"))
month = as.numeric(format(Sys.time(), "%m"))

#http://coastwatch.pfeg.noaa.gov/erddap/convert/time.txt?n=473472000&units=seconds%20since%201970-01-01T00:00:00Z

#Get Environmental variables. 
# First is to grab Chlorophyll data as this is the limiting factor. First step is to try from ERDDAP. If this fails can grab from other sources.

dapurl="http://coastwatch.pfeg.noaa.gov/erddap/griddap/erdMWchlamday.nc?chlorophyll[(last)][(0.0):1:(0.0)][(29):1:(49)][(224):1:(245)]"
outfile = "chl.nc"

idchl = curldap(dapurl, outfile)

#Check id to make sure it exits cleanly (e.g., id=0)
if(id!=0) {
	print(paste("Download failed, exit code =",id,"Trying secondary source"))
}

#Read in chl.nc file to get month and year to load other data files

chlnc = open.ncdf("chl.nc",write=FALSE)
chldate = as.Date(get.var.ncdf(chlnc,"time")/(60*60*24),origin="1970-01-01")
chllon=get.var.ncdf(chlnc,"longitude")
chllat=get.var.ncdf(chlnc,"latitude")

erdtime = format(chldate,'%Y-%m-%dT00:00:00Z')

#Download SST and change dataframe format for GMT to regrid 
outfile = "sst.nc"
dapurl = paste("http://coastwatch.pfeg.noaa.gov/erddap/griddap/erdMWsstdmday.nc?sst[(",erdtime,"):1:(",erdtime,")][(0.0):1:(0.0)][(29):1:(49)][(224):1:(245)]",sep='')

idsst = curldap(dapurl, outfile)

sstnc = open.ncdf("sst.nc",write=FALSE)
sstdate = as.Date(get.var.ncdf(sstnc,"time")/(60*60*24),origin="1970-01-01")
sstlon=get.var.ncdf(sstnc,"longitude")
sstlat=get.var.ncdf(sstnc,"latitude")

#Do some sanity checks on dates and locations
if(sstdate-chldate!=0) { print("Problem: Dates not identical")}
if(sum(sstlon-chllon)!=0) { print("Problem: Longitudes not identical")}
if(sum(sstlat-chllat)!=0) { print("Problem: Latitudes not identical")}

#STOPPING HERE>>>>> NEED TO DO SSH<<<<<<<<

#Download SSH deviation and change dataframe format for GMT to regrid the same way to match other variables
#Out of date. Need to update source.
#Gridded at 0.25 so no regridding done
outfile = "sshd.nc"
dapurl = paste("http://coastwatch.pfeg.noaa.gov/erddap/griddap/erdTAsshmday.nc?sshd[(",erdtime,"):1:(",erdtime,"2008-11-16T00:00:00Z)][(0.0):1:(0.0)][(29):1:(49)][(224):1:(245)]",sep='')

#Run a check for the same time frame. Every file should be the same month.
sstnc = open.ncdf("sst.nc",write=FALSE)
sshdnc = open.ncdf("sshd.nc",write=FALSE)

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


month = as.numeric(substring(att.get.ncdf(sstnc,0,"time_coverage_start")$value,6,7))
year = as.numeric(substring(att.get.ncdf(sstnc,0,"time_coverage_start")$value,1,4))

close.ncdf(sstnc)
close.ncdf(chlnc) 
close.ncdf(sshdnc)

#Filter to desired size
gmt.system("grdfilter chl.nc?chlorophyll -D0 -Fm0.5 -R225/245/30N/49N -I0.25/0.25 -Gchlgridded_grdfilter.grd")
gmt.system('grd2xyz chlgridded_grdfilter.grd',file="chl.xyz",append=F)

gmt.system("grdfilter sst.nc?sst -D0 -Fm0.5 -R225/245/30N/49N -I0.25/0.25 -Gsstgridded_grdfilter.grd") #using a median filter. 
gmt.system('grd2xyz sstgridded_grdfilter.grd',file="sst.xyz",append=F)

gmt.system("grdfilter sshd.nc?sshd -D0 -Fm0.5 -R225/245/30N/49N -I0.25/0.25 -Gsshdgridded_grdfilter.grd") #using a median filter. 
gmt.system('grd2xyz sshdgridded_grdfilter.grd',file="sshd.xyz",append=F)

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
bathy = read.table("bathy.txt")
sshd = read.table("sshd.xyz")
sshrms = read.table("sshsd.xyz")

colnames(sst) = c("longitude","latitude","sst")
colnames(chl) = c("longitude","latitude","chl")
colnames(sshd) = c("longitude","latitude","sshd")
colnames(sshrms) = c("longitude","latitude","sshrms")
colnames(bathy) = c("longitude","latitude","bathy")

merged = merge(merge(merge(merge(merge(merge(merge(sst,bathy,by=c("latitude","longitude")),chl,by=c("latitude","longitude")),wind,by=c("latitude","longitude")),eke,by=c("latitude","longitude")),sshd,by=c("latitude","longitude")),sstrms,by=c("latitude","longitude")),sshrms,by=c("latitude","longitude"))
colnames(merged) = c("lat","lon","sst","bathy","bathyrms","slope","aspect","chl","uy10","eke","sshd","sstrms","sshrms")

merged$month = month
write.csv(merged,sprintf("WhaleWatchFactors_%s_%s.csv",month,year),row.names=FALSE)

file.remove("chl.nc","chl.xyz","sst.nc","sst.xyz","sshd.nc","sstgridded_grdfilter.grd","chlgridded_grdfilter.grd")

