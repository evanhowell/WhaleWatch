#This script downloads STRM_30 PLUS bathymetry and runs a median filter on the data over the desired grid size, 0.25 degrees. 
#It then gets the SD within each cell using a mean filter (as the only way to get the SD), outputting files with both depth and SD and just SD.
#Therefore, it is getting depth with a median filter and then the SD of the depth on the cell.
#Updated 08/14/2014

#Install and load libraries if absent
is.installed <- function(x){
    is.element(x, installed.packages()[,1])
  } 

if (!is.installed("RCurl")){
    install.packages("RCurl")
  }
if (!is.installed("gmt")){
    install.packages("gmt")
  }

library(gmt)
library(RCurl)

#Download bathymetry files from srtm30, which are separated
f = CFILE("depth1.nc",mode="wb")
curlPerform(url = "ftp://topex.ucsd.edu/pub/srtm30_plus/srtm30/grd/w140n40.nc",writedata=f@ref) 
close(f)

f = CFILE("depth2.nc",mode="wb")
curlPerform(url = "ftp://topex.ucsd.edu/pub/srtm30_plus/srtm30/grd/w140n90.nc",writedata=f@ref)
close(f)

#Create one file
gmt.system("grdpaste depth1.nc depth2.nc -Gdepth.nc -V")

#Cut file into desired size and void all land values before filtering
gmt.system("grd2xyz depth.nc -Rd-135/-115/30/49",file="depth.xyz")
depth = read.table("depth.xyz")
depth$V3[depth$V3>=0] = NaN
write.table(depth,"depthnan.xyz",row.names=FALSE,col.names=FALSE)

#Filter bathymetry to the desired cell size, 0.25deg
gmt.system("xyz2grd depthnan.xyz -Rd-135/-115/30/49 -I0.008333333 -Gdepth.grd")
gmt.system("grdfilter depth.grd -D0 -Fm0.5 -Rd-135/-115/30/49 -I0.25/0.25 -Gdepth_grdfilter_a.grd") #using a median filter

#Find all cells with land and turn into NaN values (requested)(-Np is not working in grdfilter)
gmt.system("grdlandmask -Gmask.grd -I0.25/0.25 -Rd-135/-115/30/49 -N0/NaN")
gmt.system("grdmath depth_grdfilter_a.grd mask.grd OR = depth_grdfilter.grd")
gmt.system("grd2xyz depth_grdfilter.grd",file="depth_filtered.xyz")

#Find the SD of the depth, put it through the same landmask grd as depth to match the data 
#grd files can only have lon, lat, variable
gmt.system("blockmean depthnan.xyz -Es -Rd-135/-115/30/49 -I0.25/0.25", file="bathyrms_blockmean.xyz")
sd = read.table("bathyrms_blockmean.xyz")
sd = data.frame(c(sd[1],sd[2],sd[4]))
write.table(sd,"sd_blockmean.xyz",row.names=FALSE,col.names=FALSE)
gmt.system("xyz2grd sd_blockmean.xyz -Rd-135/-115/30/49 -I0.25/0.25 -Gsd_blockmean.grd")
gmt.system("grdmath sd_blockmean.grd mask.grd OR = sd_blockmean_masked.grd")
gmt.system("grd2xyz sd_blockmean_masked.grd", file="bathyrms.xyz")

#Prepare text file for combining all variables
depthfiltered = read.table('depth_filtered.xyz')
sd = read.table("bathyrms.xyz")
depthfiltered = merge(depthfiltered,sd,by=c("V2","V1"))
depthfiltered$longitude = depthfiltered$V1+360
depthfiltered$latitude = depthfiltered$V2
bathy = depthfiltered[order(depthfiltered$latitude),]
colnames(bathy)[3] = "bathy"
colnames(bathy)[4] = "bathyrms"
bathy[1] = NULL
bathy[1] = NULL
write.table(bathy,"bathy.txt")

#Write a separate text file solely for bathyrms if that's desired
bathyrms = bathy[c("longitude","latitude","bathyrms")]
write.table(bathyrms,"bathyrms.txt")

#Remove unnecessary files
file.remove("depth1.nc","depth2.nc","depth.xyz","depthnan.xyz","bathyrms.xyz","bathyrms_blockmean.xyz","sd_blockmean_masked.grd","depth.grd","mask.grd","depth_grdfilter_a.grd","depth_grdfilter.grd","depth_filtered.xyz","sd_blockmean.xyz","sd_blockmean.grd")
