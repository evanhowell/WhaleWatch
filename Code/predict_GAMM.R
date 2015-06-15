predict_GAMM <- function(factorfile) {
  #Load required libraries. Function pkgTest is in the file Code/load_Functions.R and should have been loaded. If not test here and load file.
  
  if(exists("pkgTest")==FALSE) {
     print("Function pkgTest not found, loading file Code/load_Functions.R...")
     source("Code/load_Functions.R")
     }
     
  pkgTest("mgcv")
  pkgTest("sp")
  pkgTest("rgdal")
  pkgTest("raster")
  
  
  #setwd("~/Dropbox/Documents/R/Blue_whales/Evan/")
  #Run all GAMs to average
  ####BE SURE TO HAVE THE RIGHT GAMM NAME BEFORE SENDING THIS OUT
  
  # IMPORTANT - The script assumes that it is in the correct parent directory, and then sets relative paths from there.
  
  filenm<-"Data/WhaleWatchFactors_1_2009.csv"
  predfactors = read.csv(filenm)
  wwvector = vector('list')
  
  #For SuFaindex<-GAMdataRunCCS$month>7 & GAMdataRunCCS$month<12 need to load bwhaleGAMM.sufa. Otheriwse bwhaleGAMM.wisp
  if(predfactors$month>7 & predfactors$month<12) {gammnm<-"bwhaleGAMM.sufa"} else {gammnm<-"bwhaleGAMM.wisp"}
  
  files = paste(gammnm,1:40,".RData",sep="")
  for(i in files){
  	load(i)
  	bwhaleGAMM<-get(gammnm)
  	wwvector[[i]] = predict.gam(bwhaleGAMM$gam,get("predfactors"),se=TRUE, type="response")
  	names(wwvector) = sub((i),"",names(wwvector))
  }
  
  #### Try BRT model
  pts=predfactors
  coordinates(pts)=~lon+lat
  proj4string(pts)=CRS("+init=epsg:4326") # set it to lat-long
  #pts = spTransform(pts,CRS("+proj=longlat +ellps=WGS84 +no_defs"))
  gridded(pts) = TRUE
  r1 = raster(subset(pts,select="sst"))
  projection(r1) = CRS("+proj=longlat +ellps=WGS84 +no_defs")
  r2 = raster(subset(pts,select="bathy"))
  projection(r2) = CRS("+proj=longlat +ellps=WGS84 +no_defs")
  r3 = raster(subset(pts,select="chl"))
  projection(r3) = CRS("+proj=longlat +ellps=WGS84 +no_defs")
  
  b <- brick(r1,r2,r3)
  
  # ### FOR BRT PREDICTIONS - NOT NOW
  # wwbrtvector = vector('list')
  # files = paste("bwhaleBRT",1:40,".RData",sep="")
  # for(i in files){
  # 	load(i)
  # 	p = predict(b, bwhaleBRT.lr005, n.trees=angaus.tc5.lr005$gbm.call$best.trees, type="response")
  # 	p = mask(p, raster(b, 1))
  
  # 	wwbrtvector[[i]] = predict.gam(bwhaleGAMM$gam,get("predfactors"),se=TRUE, type="response")
  # 	names(wwbrtvector) = sub((i),"",names(wwvector))
  # }
  
  gam = data.frame(wwvector)
  fit =gam[ ,!substr(colnames(gam),1,1)=="s"]
  #sefit =gam[ ,substr(colnames(gam),1,1)=="s"]
  fitmean = rowMeans(fit,na.rm=T)
  sd = apply(fit,1,sd)
  predict = cbind(predfactors,fitmean,sd)
  predict$percent = predict$fitmean*100
  
  #Write a .csv file with all data if desired for comparisons
  write.csv(predict,"WhaleWatchPredictions.csv")
  
  #Create text files for GMT  
  fitxyz = data.frame(predict[2],predict[1],predict[8])
  sdxyz = data.frame(predict[2],predict[1],predict[7])
  write.table(fitxyz,"fit.txt",col.names=FALSE,row.names=F,quote=FALSE) #must have no headers, no row names, no quotes
  write.table(sdxyz,"sd.txt",col.names=FALSE,row.names=F,quote=FALSE)
}

