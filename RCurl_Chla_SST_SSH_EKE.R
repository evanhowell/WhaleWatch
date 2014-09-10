#Install and load libraries
is.installed <- function(x){
    is.element(x, installed.packages()[,1])
  } 

if (!is.installed("ncdf")){
    install.packages("ncdf")
  }
if (!is.installed("RCurl")){
    install.packages("RCurl")
  }

library(ncdf)
library(RCurl)

#Download chl and change dataframe format for GMT to regrid chlorophyll to match SST
f = CFILE("chl.nc",mode="wb")
curlPerform(url="http://coastwatch.pfeg.noaa.gov/erddap/griddap/erdMWchlamday.nc?chlorophyll[(last)][(0.0):1:(0.0)][(30):1:(49)][(225):1:(245)]",writedata=f@ref) 
close(f)

#downloadReturn<-geturl("http://coastwatch.pfeg.noaa.gov/erddap/griddap/erdMWchlamday.nc?chlorophyll[(last)][(0.0):1:(0.0)][(30):1:(49)][(225):1:(245)]",destfile='fileinfo.nc',cacheOK=TRUE,mode="wb",quiet=quiet)

#Download sst and change dataframe format for GMT to regrid chlorophyll to match SST
f = CFILE("sst.nc",mode="wb")
curlPerform(url="http://coastwatch.pfeg.noaa.gov/erddap/griddap/erdTAssh1day.nc?sst[(last)][(0.0):1:(0.0)][(30):1:(49)][(225):1:(245)]",writedata=f@ref) 
close(f)

#Download SSHa and change dataframe format for GMT to regrid chlorophyll to match SST
f = CFILE("ssha.nc",mode="wb")
curlPerform(url="http://coastwatch.pfeg.noaa.gov/erddap/griddap/erdTAssh1day.nc?ssh[(last)][(0.0):1:(0.0)][(30):1:(49)][(225):1:(245)]",writedata=f@ref) 
close(f)

#Download sst and change dataframe format for GMT to regrid chlorophyll to match SST
f = CFILE("ugeo.nc",mode="wb")
curlPerform(url="http://coastwatch.pfeg.noaa.gov/erddap/griddap/erdTAgeo1day.nc?u_current[(last)][(0.0):1:(0.0)][(30):1:(49)][(225):1:(245)]",writedata=f@ref) 
close(f)

#Download sst and change dataframe format for GMT to regrid chlorophyll to match SST
f = CFILE("vgeo.nc",mode="wb")
curlPerform(url="http://coastwatch.pfeg.noaa.gov/erddap/griddap/erdTAgeo1day.nc?v_current[(last)][(0.0):1:(0.0)][(30):1:(49)][(225):1:(245)]",writedata=f@ref) 
close(f)

#Run a check for the same time frame
sstnc = open.ncdf("sst.nc",write=FALSE)
chlnc = open.ncdf("chl.nc",write=FALSE)
ssha = open.ncdf("ssha.nc",write=FALSE)
ugeo = open.ncdf("ugeo.nc",write=FALSE)
vgeo = open.ncdf("vgeo.nc",write=FALSE)

if((att.get.ncdf(sstnc,0,"time_coverage_start")$value == att.get.ncdf(chlnc,0,"time_coverage_start")$value)){
	print("SST and Chla have the same time period")
	print(att.get.ncdf(sstnc,0,"time_coverage_start")$value)
	}else{ 
	stop("Not the same year/month")
}

month = as.numeric(substring(att.get.ncdf(sstnc,0,"time_coverage_start")$value,6,7))

close.ncdf(sstnc)
close.ncdf(chlnc) #necessary?
close.ncdf(ssha) #necessary?
close.ncdf(ugeo) #necessary?
close.ncdf(vgeo) #necessary?

#Filter to desired size. Delete if filtering chl values prior to grdfilter
gmt.system("grdfilter chl.nc?chlorophyll -D0 -Fm0.5 -R225/245/30N/49N -I0.25/0.25 -Gchlgridded_grdfilter.grd")
gmt.system('grd2xyz chlgridded_grdfilter.grd',file="chl.xyz",append=F)

gmt.system("grdfilter sst.nc?sst -D0 -Fm0.5 -R225/245/30N/49N -I0.25/0.25 -Gsstgridded_grdfilter.grd") #using a median filter. 
gmt.system('grd2xyz sstgridded_grdfilter.grd',file="sst.xyz",append=F)

gmt.system("grdfilter ssh.nc?ssh -D0 -Fm0.5 -R225/245/30N/49N -I0.25/0.25 -Gsshgridded_grdfilter.grd") #using a median filter. 
gmt.system('grd2xyz sshgridded_grdfilter.grd',file="ssh.xyz",append=F)

gmt.system("grdfilter ugeo.nc?u_current -D0 -Fm0.5 -R225/245/30N/49N -I0.25/0.25 -Gugeogridded_grdfilter.grd") #using a median filter. 
gmt.system('grd2xyz ugeogridded_grdfilter.grd',file="ugeo.xyz",append=F)

gmt.system("grdfilter vgeo.nc?v_current -D0 -Fm0.5 -R225/245/30N/49N -I0.25/0.25 -Gvgeogridded_grdfilter.grd") #using a median filter. 
gmt.system('grd2xyz vgeogridded_grdfilter.grd',file="vgeo.xyz",append=F)

#Calculate SD for SSH
gmt.system("blockmean ssh.xyz -Es -Rd-135/-115/30/49 -I0.25/0.25", file="sshrms_blockmean.xyz")
sd = read.table("sshrms_blockmean.xyz")
sd = data.frame(c(sd[1],sd[2],sd[4]))
write.table(sd,"sd_blockmean.xyz",row.names=FALSE,col.names=FALSE)
gmt.system("xyz2grd sd_blockmean.xyz -Rd-135/-115/30/49 -I0.25/0.25 -Gsd_blockmean.grd")
gmt.system("grdmath sd_blockmean.grd mask.grd OR = sd_blockmean_masked.grd")
gmt.system("grd2xyz sd_blockmean_masked.grd", file="sshrms.xyz")


#Combine sst, chl, bathy into one file
sst = read.table("sst.xyz")
chl = read.table("chl.xyz")
ssh = read.table("ssh.xyz")
sshrms = read.table("sshrms.xyz")
ugeo = read.table("ugeo.xyz")
vgeo = read.table("vgeo.xyz")
bathy = read.table("bathy.txt")
bathyrms = read.table("bathyrms.txt")
eke = 1/2*(ugeo^2+vgeo^2)

colnames(sst) = c("longitude","latitude","sst")
colnames(chl) = c("longitude","latitude","chl")
colnames(ssh) = c("longitude","latitude","ssh")
colnames(sshrms) = c("longitude","latitude","sshrms")
colnames(ugeo) = c("longitude","latitude","ugeo")
colnames(vgeo) = c("longitude","latitude","vgeo")
colnames(bathy) = c("longitude","latitude","bathy")
colnames(bathyrms) = c("longitude","latitude","bathyrms")
colnames(eke) = c("longitude","latitude","eke")

merged = merge(merge(sst,bathy,bathyrms,chl,ssh,sshrms,ugeo,vgeo,eke,by=c("latitude","longitude")))
colnames(merged) = c("lat","lon","sst","bathy","bathyrms","chl","ssh","sshrms","ugeo","vgeo","eke")
write.csv(merged,"WhaleWatchFactors.csv",row.names=FALSE)

#Remove unnessary files
file.remove("chl.nc","chl.xyz","sst.nc","sst.xyz","sstgridded_grdfilter.grd","chlgridded_grdfilter.grd")
file.remove("*.nc","*.xyz","*.grd")