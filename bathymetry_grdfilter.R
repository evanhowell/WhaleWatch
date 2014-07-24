#This script downloads STRM_30 PLUS bathymetry and runs a median filter on the data over the desired grid size, 0.25 degrees

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

#Download bathymetry files, which are separated
f = CFILE("depth1.nc",mode="wb")
curlPerform(url = "ftp://topex.ucsd.edu/pub/srtm30_plus/srtm30/grd/w140n40.nc",writedata=f@ref) 
close(f)

f = CFILE("depth2.nc",mode="wb")
curlPerform(url = "ftp://topex.ucsd.edu/pub/srtm30_plus/srtm30/grd/w140n90.nc",writedata=f@ref)
close(f)

#Create one file
gmt.system("grdpaste depth1.nc depth2.nc -Gdepth.nc -V")

#Cut file into desired size and delete all land values before filtering
gmt.system("grd2xyz depth.nc -Rd-135/-115/30/49",file="depth.xyz")
depth = read.table("depth.xyz")
depth$V3[depth$V3>=0] = NaN
write.table(depth,"depthnan.xyz",row.names=FALSE,col.names=FALSE)

#Filter bathymetry to match chlorophyll and SST gridding using the same method 
gmt.system("xyz2grd depthnan.xyz -Rd-135/-115/30/49 -I0.008333333 -Gdepth.grd")
gmt.system("grdfilter depth.grd -D0 -Fm0.5 -Rd-135/-115/30/49 -I0.25/0.25 -Gdepth_grdfilter_a.grd") #using a median filter

#Find all cells with land and turn into NaN values (requested)(-Np does not work in grdfilter)
gmt.system("grdlandmask -Gmask.grd -I0.25/0.25 -Rd-135/-115/30/49 -N0/NaN")
gmt.system("grdmath depth_grdfilter_a.grd mask.grd OR = depth_grdfilter.grd")
gmt.system("grd2xyz depth_grdfilter.grd",file="depth_filtered.xyz")

#Prepare text file for combining all variables
depthfiltered = read.table('depth_filtered.xyz')
depthfiltered$longitude = depthfiltered$V1+360
depthfiltered$latitude = depthfiltered$V2
bathy = depthfiltered[order(depthfiltered$latitude),]
colnames(bathy)[3] = "bathy"
bathy[1] = NULL
bathy[1] = NULL
write.table(bathy,"bathy.txt")

#Remove unnessary files
file.remove("depth1.nc","depth2.nc","depth.xyz","depthnan.xyz","depth.grd","mask.grd","depth_grdfilter_a.grd","depth_grdfilter.grd","depth_filtered.xyz")