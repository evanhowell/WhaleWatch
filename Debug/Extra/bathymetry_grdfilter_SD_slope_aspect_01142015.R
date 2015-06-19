#This script loads the static STRM_30 PLUS bathymetry file and runs a median filter on the data over the desired grid size, 0.25 degrees. 
#It then gets the SD within each cell using a mean filter (as the only way to get the SD), outputting files with both depth and SD and just SD.
#Therefore, it is getting depth with a median filter and then the SD of the depth on the cell.
#Updated 08/14/2014
# Updated 01/14/2015 to reflect larger Topex file already pasted together and masked to 0.25x0.25. This file will need to exist in this directory (elliott-topex-NaNs.xyz)

#Install and load libraries if absent
is.installed <- function(x){
    is.element(x, installed.packages()[,1])
  } 

if (!is.installed("gmt")){
    install.packages("gmt")
  }
if (!is.installed("SDMTools")){
    install.packages("SDMTools")
  }
if (!is.installed("sp")){
    install.packages("sp")
  }
library(gmt)
library(SDMTools)
library(sp)

#Filter bathymetry, slope, and aspect to the desired cell size, 0.25km
#gmt.system("grdfilter depth.grd -D0 -Fm0.5 -Rd-170/-80/-15/75 -I0.25/0.25 -Gdepth_grdfilter_a.grd") #using a median filter

#Load xyz data and create SpatialGridDataFrame object
spdepth = read.table('elliott-topex-NaNs-p25.xyz')
colnames(spdepth)<-c("lon","lat","depth")
coordinates(spdepth)<-c("lon", "lat")
gridded(spdepth) <- TRUE
spdepth = as(spdepth, "SpatialGridDataFrame") # creates the full grid

#Calculate slope & aspect
dslope<-slope(spdepth, latlon=TRUE)
daspect<-aspect(spdepth, latlon=TRUE)
write.table(data.frame(dslope)[c("lon", "lat", "depth")],"slopenan.xyz",row.names=FALSE,col.names=FALSE)
write.table(data.frame(daspect)[c("lon", "lat", "depth")],"aspectnan.xyz",row.names=FALSE,col.names=FALSE)

## Plot aspect
pdf("Aspect.pdf",width=7,height=7)
 spplot(daspect,"depth")
dev.off()
pdf("Slope.pdf",width=7,height=7)
 spplot(dslope,"depth")
dev.off()

#slope
gmt.system("xyz2grd slopenan.xyz -Rd-170/-80/-15/75 -I0.25/0.25 -r -Gslope.grd")

#aspect
gmt.system("xyz2grd aspectnan.xyz -Rd-170/-80/-15/75 -I0.25/0.25 -r -Gaspect.grd")

#Find all cells with land and turn into NaN values (requested)(-Np is not working in grdfilter)
gmt.system("grdlandmask -Gmask.grd -I0.25/0.25 -Rd-170/-80/-15/75 -N0/NaN -r") # use pixel registration
gmt.system("grdmath elliott-topex-NaNs-p25.grd mask.grd OR = depth_masked.grd")
gmt.system("grd2xyz depth_masked.grd",file="depth_masked.xyz")

#slope
gmt.system("grdmath slope.grd mask.grd OR = slope_masked.grd")
gmt.system("grd2xyz slope_masked.grd",file="slope_masked.xyz")

#aspect
gmt.system("grdmath aspect.grd mask.grd OR = aspect_masked.grd")
gmt.system("grd2xyz aspect_masked.grd",file="aspect_masked.xyz")

#Find the SD of the depth, put it through the same landmask grd as depth to match the data 
#grd files can only have lon, lat, variable
gmt.system("blockmean elliott-topex-NaNs-p25.xyz -Es -Rd-170/-80/-15/75 -I0.25/0.25", file="bathyrms_blockmean.xyz")
bathyrms = read.table("bathyrms_blockmean.xyz")
bathyrms = data.frame(c(bathyrms[1],bathyrms[2],bathyrms[4]))
write.table(bathyrms,"sd_blockmean.xyz",row.names=FALSE,col.names=FALSE)
gmt.system("xyz2grd sd_blockmean.xyz -Rd-170/-80/-15/75 -I0.25/0.25 -r -Gsd_blockmean.grd")
gmt.system("grdmath sd_blockmean.grd mask.grd OR = sd_blockmean_masked.grd")
gmt.system("grd2xyz sd_blockmean_masked.grd", file="bathyrms.xyz")

#Prepare text file for combining all variables
depthmasked = read.table('depth_masked.xyz')
bathyrms = read.table("bathyrms.xyz")
slope = read.table("slope_masked.xyz")
aspect = read.table("aspect_masked.xyz")
depthmasked = merge(depthmasked,bathyrms,by=c("V2","V1"))
depthmasked = merge(depthmasked,slope,by=c("V2","V1"))
depthmasked = merge(depthmasked,aspect,by=c("V2","V1"))
depthmasked$longitude = depthmasked$V1+360
depthmasked$latitude = depthmasked$V2
bathy = depthmasked[order(depthmasked$latitude),]
colnames(bathy)[3] = "bathy"
colnames(bathy)[4] = "bathyrms"
colnames(bathy)[5] = "slope"
colnames(bathy)[6] = "aspect"
bathy[1] = NULL
bathy[1] = NULL
bathy2 = bathy[,c(5,6,1,2,3,4)]
write.table(bathy2,"bathy.txt")

#Write a separate text file solely for bathyrms if that's desired
bathyrms = bathy[c("longitude","latitude","bathyrms")]
write.table(bathyrms,"bathyrms.txt")

#Remove unnecessary files
file.remove("slope.grd","aspect.grd","slopenan.xyz","aspectnan.xyz","depth_masked.grd","slope_masked.grd","slope_masked.xyz","aspect_masked.grd","aspect_masked.xyz","bathyrms.xyz","bathyrms_blockmean.xyz","sd_blockmean_masked.grd","mask.grd","depth_masked.xyz","sd_blockmean.xyz","sd_blockmean.grd")