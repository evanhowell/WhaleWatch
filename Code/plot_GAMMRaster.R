plot_GAMMRaster <- function(imagevec) {
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
  
  #Test if we are dealing with percent (fitmean) or standard dev
  if ("percent" %in% names(imagevec)) {
    titletext = 'Mean of 40 WhaleWatch models'
  } else {
    titletext = 'SD of 40 WhaleWatch models'
  }
  
  #Do fit first
  fit = imagevec[,c(1,2,3)]
  coordinates(fit) = ~longitude + latitude # Set coordinates
  gridded(fit) = TRUE # make gridded
  fitRaster = raster(fit) # turn into Raster Layer
  extent(fitRaster)@xmin<-extent(fitRaster)@xmin-360 #Change Longitude extant
  extent(fitRaster)@xmax<-extent(fitRaster)@xmax-360
  
  imageFile = paste("Images/WhaleWatch_", names(imagevec)[3],'_', format(fileDate,"%Y_%m"),".png",sep="")
  
  #First plot fitmean
  png(filename=imageFile, width=495,height=549,units='px')
  plot(fitRaster,legend=T,zlim=c(0,100),col=coltheme$regions$col,main=titletext)
  plot(wrld_simpl, add = T, col="light grey",border="dark grey")
  dev.off()
  
  
}