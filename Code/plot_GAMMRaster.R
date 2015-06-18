plot_GAMMRaster <- function(predictvec) {
  if(exists("pkgTest")==FALSE) {
    print("Function pkgTest not found, loading file Code/load_Functions.R...")
    source("Code/load_Functions.R")
  }
  
  #Build date from month and year within predictvec
  #predictvec$year=2009 #DEBUGGING ONLY. COMMENT OUT FOR PRODUCTION!
  fileDate = as.Date(paste(predictvec$month[1],"1",predictvec$year[1],sep="/"),format="%m/%d/%Y")
  
  data("wrld_simpl") #Load WorldHires
  
  predictfit = data.frame(predictvec$lon, predictvec$lat, predictvec$percent)
  names(predictfit) = c("lon","lat","percent") #clean up col names
  coordinates(predictfit) = ~lon + lat # Set coordinates
  gridded(predictfit) = TRUE # make gridded
  predfitRaster = raster(predictfit) # turn into Raster Layer
  extent(predfitRaster)@xmin<-extent(predfitRaster)@xmin-360 #Change Longitude extant
  extent(predfitRaster)@xmax<-extent(predfitRaster)@xmax-360
  
  imageFile = paste("Images/WhaleWatch_", format(fileDate,"%Y_%m"),".png",sep="")
  imageText = paste("WhaleWatch Image for", format(fileDate,"%B %Y"))
  
  #Make new theme
  coltheme<-PuOrTheme(rev(brewer.pal(9,"Spectral")))
  
  png(filename=imageFile)
  plot(predfitRaster,legend=T,zlim=c(0,100),col=coltheme$regions$col,main=imageText,xlim=c(-135,-115),ylim=c(30,49))
  plot(wrld_simpl, add = T, col="light grey",border="dark grey",xlim=c(-135,-115),ylim=c(30,49))
  dev.off()
  
  #predictsdfit = cbind(predictvec$lon, predictvec$lat, predictvec$sdfit)
  
  
}