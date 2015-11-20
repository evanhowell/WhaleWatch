plot_GAMMRaster <- function(predictvecvec) {
  # Plots the GAMM prediction output
  #
  # Args:
  #   predictvec: The prediction output from the predict_GAMM() step
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
  
  # Function to create raster from data to avoid repeating code. 
  rasterout <- function(rawdata) {
    coordinates(rawdata) = ~longitude + latitude # Set coordinates
    gridded(rawdata) = TRUE # make gridded
    rasterout = raster(rawdata) # turn into Raster Layer
    extent(rasterout)@xmin<-extent(rasterout)@xmin-360 #Change Longitude extant
    extent(rasterout)@xmax<-extent(rasterout)@xmax-360
    return(rasterout)
  }
  
  pkgTest("maptools")
  pkgTest("raster")
  pkgTest("rasterVis")
  
  #Build date from month and year within predictvec
  #predictvec$year=2009 #DEBUGGING ONLY. COMMENT OUT FOR PRODUCTION!
  fileDate = as.Date(paste(predictvec$month[1],"1",predictvec$year[1],sep="/"),format="%m/%d/%Y")
  
  data("wrld_simpl") #Load WorldHires
  
  #Make new theme
  coltheme<-PuOrTheme(rev(brewer.pal(9,"Spectral")))
  
  #Plot Rasters images in complex mulitple plot. Plot is 1 row by 4 columns, with first three 
  # plotted together (percentages) and density plot as last one.
  #Do Lower first
  imagevec = data.frame(longitude=predictvec$longitude,latitude=predictvec$latitude,Lower=predictvec$lower)
  rasterlower = rasterout(imagevec)
  #Now do Average
  imagevec = data.frame(longitude=predictvec$longitude,latitude=predictvec$latitude,Occurrence=predictvec$percent)
  rasteraverage = rasterout(imagevec)
  #Now do Upper
  imagevec = data.frame(longitude=predictvec$longitude,latitude=predictvec$latitude,Upper=predictvec$upper)
  rasterupper = rasterout(imagevec)
  #now do density
  imagevec = data.frame(longitude=predictvec$longitude,latitude=predictvec$latitude,Density=predictvec$density)
  rasterdensity = rasterout(imagevec)
  
    #setup layout
  png(filename=imageFile, res=150,width=23.0,height=6.9,units="in") #6.169491
  par(mar=c(5.5, 5.5, 5.5, 5.5), oma=c(0,0,4,0), ps=24)
  layout(matrix(c(1,2,3,4), 1, 4, byrow=T))
  plot(rasterlower,legend=F,las=1,zlim=c(0,100),col=coltheme$regions$col,main='Lower',xlim=c(-134.6,-115),ylim=c(30,48.7))
  plot(wrld_simpl, add = T, col="light grey",border="dark grey")
  
  plot(rasteraverage,legend=F,las=1,zlim=c(0,100),col=coltheme$regions$col,main='Average',xlim=c(-134.6,-115),ylim=c(30,48.7))
  plot(wrld_simpl, add = T, col="light grey",border="dark grey")
  mtext("Likelihood of Occurrence", side=3, line=6)
  
  plot(rasterupper,legend=T,las=1,zlim=c(0,100),col=coltheme$regions$col,main='Upper',xlim=c(-134.6,-115),ylim=c(30,48.7))
  plot(wrld_simpl, add = T, col="light grey",border="dark grey")
  
  plot(rasterdensity,legend=T,las=1,zlim=c(0,3),col=coltheme$regions$col,main='Average',xlim=c(-134.6,-115),ylim=c(30,48.7))
  plot(wrld_simpl, add = T, col="light grey",border="dark grey")
  mtext("Number of Whales", side=3, line=6)
  dev.off()
  
  
}
