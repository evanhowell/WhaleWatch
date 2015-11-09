plot_GAMMRaster <- function(imagevec, titletext) {
  # Plots the GAMM prediction output
  #
  # Args:
  #   imagevec: The prediction output from the predict_GAMM() step
  #   titletext: To give the title for:
  #		Average Probability of Occurrence
  #		Standard Deviation
  #		Lower estimate
  #		Upper estimate
  #
  # Produces an image based on the parameter (e.g., mean, SD) chosen.
  #
  # Version 1.1 -> This was rewritten to create one image of all four plots, rather
  # than the single plots that were made in version 1.0. 11/9/2015 EAH

  if(exists("pkgTest")==FALSE) {
    print("Function pkgTest not found, loading file Code/load_Functions.R...")
    source("Code/load_Functions.R")
  }
  
  pkgTest("maptools")
  pkgTest("raster")
  pkgTest("rasterVis")
  
  #Build date from month and year within imagevec
  #imagevec$year=2009 #DEBUGGING ONLY. COMMENT OUT FOR PRODUCTION!
  fileDate = as.Date(paste(imagevec$month[1],"1",imagevec$year[1],sep="/"),format="%m/%d/%Y")
  
  data("wrld_simpl") #Load WorldHires
  
  #Make new theme
  coltheme<-PuOrTheme(rev(brewer.pal(9,"Spectral")))
  
  #We will make a RasterStack from the 
  #Do fit first
  imagevec = data.frame(longitude=predictvec$longitude,latitude=predictvec$latitude,Occurrence=predictvec$percent,month=predictvec$month,year=predictvec$year)
  fit = imagevec[,c(1,2,3)]
  coordinates(fit) = ~longitude + latitude # Set coordinates
  gridded(fit) = TRUE # make gridded
  OccurrenceRaster = raster(fit) # turn into Raster Layer
  extent(OccurrenceRaster)@xmin<-extent(OccurrenceRaster)@xmin-360 #Change Longitude extant
  extent(OccurrenceRaster)@xmax<-extent(OccurrenceRaster)@xmax-360
  
  imageFile = paste("Images/WhaleWatch_", format(fileDate,"%Y_%m"),".png",sep="")
  
  #setup layout
  png(filename=imageFile, res=150,width=25,height=6.9,units="in") #6.169491
  layout(matrix(c(1,2,3,4), 1, 4, byrow=T))
  plot(fitRaster,legend=F,las=1,zlim=c(0,100),col=coltheme$regions$col,main='Occurrence',xlim=c(-134.6,-115),ylim=c(30,48.7))
  plot(wrld_simpl, add = T, col="light grey",border="dark grey")
  
  plot(fitRaster,legend=F,las=1,zlim=c(0,100),col=coltheme$regions$col,main='Occurrence',xlim=c(-134.6,-115),ylim=c(30,48.7))
  plot(wrld_simpl, add = T, col="light grey",border="dark grey")
  
  plot(fitRaster,legend=T,las=1,zlim=c(0,100),col=coltheme$regions$col,main='Occurrence',xlim=c(-134.6,-115),ylim=c(30,48.7))
  plot(wrld_simpl, add = T, col="light grey",border="dark grey")
  
  plot(fitRaster,legend=T,las=1,zlim=c(0,100),col=coltheme$regions$col,main='Occurrence',xlim=c(-134.6,-115),ylim=c(30,48.7))
  plot(wrld_simpl, add = T, col="light grey",border="dark grey")
  dev.off()
  
  
}
