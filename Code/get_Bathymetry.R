#This function downloads STRM_30 PLUS bathymetry and runs a median filter on the data over the desired grid size, 0.25 degrees. 
#It then gets the SD within each cell using a mean filter (as the only way to get the SD), outputting files with both depth and SD and just SD.
#Therefore, it is getting depth with a median filter and then the SD of the depth on the cell.
#Updated 08/14/2014

# IMPORTANT - The script assumes that it is in the correct parent directory, and then sets relative paths from there.

#Code validated to recreate bathy.txt file on 5/11/2015 by EAH

get_Bathymetry <- function() {
  #Install and load libraries if absent
  if(exists("pkgTest")==FALSE) {
     print("Function pkgTest not found, loading file Code/load_Functions.R...")
     source("Code/load_Functions.R")
     }
     
  pkgTest("gmt")
  pkgTest("SDMTools")
  pkgTest("sp")
  pkgTest("RCurl")
  
  #Download bathymetry files from srtm30, which are separated
  f = CFILE("Data/w140n40.nc",mode="wb")
  curlPerform(url = "ftp://topex.ucsd.edu/pub/srtm30_plus/srtm30/grd/w140n40.nc",writedata=f@ref) 
  close(f)
  
  f = CFILE("Data/w140n90.nc",mode="wb")
  curlPerform(url = "ftp://topex.ucsd.edu/pub/srtm30_plus/srtm30/grd/w140n90.nc",writedata=f@ref)
  close(f)
  
  #Create one file
  gmt.system("grdpaste Data/w140n40.nc Data/w140n90.nc -Gdepth.nc -V")
  
  #Cut file into desired size and void all land values before filtering
  gmt.system("grd2xyz depth.nc -Rd-135/-115/30/49",file="depth.xyz")
  depth = read.table("depth.xyz")
  depth$V3[depth$V3>=0] = NaN
  write.table(depth,"depthnan.xyz",row.names=FALSE,col.names=FALSE)
  
  #Create SpatialGridDataFrame object
  spdepth<-depth
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
  
  #Filter bathymetry, slope, and aspect to the desired cell size, 0.25km
  gmt.system("xyz2grd depthnan.xyz -Rd-135/-115/30/49 -I0.008333333 -Gdepth.grd")
  gmt.system("grdfilter depth.grd -D0 -Fm0.5 -Rd-135/-115/30/49 -I0.25/0.25 -Gdepth_grdfilter_a.grd") #using a median filter
  
  #slope
  gmt.system("xyz2grd slopenan.xyz -Rd-135/-115/30/49 -I0.008333333 -Gslope.grd")
  gmt.system("grdfilter slope.grd -D0 -Fm0.5 -Rd-135/-115/30/49 -I0.25/0.25 -Gdepth_grdfilter_b.grd") #using a median filter
  
  #aspect
  gmt.system("xyz2grd aspectnan.xyz -Rd-135/-115/30/49 -I0.008333333 -Gaspect.grd")
  gmt.system("grdfilter aspect.grd -D0 -Fm0.5 -Rd-135/-115/30/49 -I0.25/0.25 -Gdepth_grdfilter_c.grd") #using a median filter
  
  #Find all cells with land and turn into NaN values (requested)(-Np is not working in grdfilter)
  gmt.system("grdlandmask -Gmask.grd -I0.25/0.25 -Rd-135/-115/30/49 -N0/NaN")
  gmt.system("grdmath depth_grdfilter_a.grd mask.grd OR = depth_grdfilter.grd")
  gmt.system("grd2xyz depth_grdfilter.grd",file="depth_filtered.xyz")
  
  #slope
  gmt.system("grdmath depth_grdfilter_b.grd mask.grd OR = depth_grdfilter.grd")
  gmt.system("grd2xyz depth_grdfilter.grd",file="slope_filtered.xyz")
  
  #aspect
  gmt.system("grdmath depth_grdfilter_c.grd mask.grd OR = depth_grdfilter.grd")
  gmt.system("grd2xyz depth_grdfilter.grd",file="aspect_filtered.xyz")
  
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
  depthfiltered = read.table("depth_filtered.xyz")
  sd = read.table("bathyrms.xyz")
  slope = read.table("slope_filtered.xyz")
  aspect = read.table("aspect_filtered.xyz")
  depthfiltered = merge(depthfiltered,sd,by=c("V2","V1"))
  depthfiltered = merge(depthfiltered,slope,by=c("V2","V1"))
  depthfiltered = merge(depthfiltered,aspect,by=c("V2","V1"))
  depthfiltered$longitude = depthfiltered$V1+360
  depthfiltered$latitude = depthfiltered$V2
  bathy = depthfiltered[order(depthfiltered$latitude),]
  colnames(bathy)[3] = "bathy"
  colnames(bathy)[4] = "bathyrms"
  colnames(bathy)[5] = "slope"
  colnames(bathy)[6] = "aspect"
  bathy[1] = NULL
  bathy[1] = NULL
  write.table(bathy,"Data/bathy.txt")
  
  #Write a separate text file solely for bathyrms if that's desired
  bathyrms = bathy[c("longitude","latitude","bathyrms")]
  write.table(bathyrms,"Data/bathyrms.txt")
  
  #Remove unnecessary files
  file.remove("aspect.grd","aspect_filtered.xyz","aspectnan.xyz","depth.nc","depth_grdfilter_b.grd","depth_grdfilter_c.grd","slope.grd","slope_filtered.xyz","slopenan.xyz","depth.xyz","depthnan.xyz","bathyrms.xyz","bathyrms_blockmean.xyz","sd_blockmean_masked.grd","depth.grd","mask.grd","depth_grdfilter_a.grd","depth_grdfilter.grd","depth_filtered.xyz","sd_blockmean.xyz","sd_blockmean.grd")
}
