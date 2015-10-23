predict_GAMM <- function(factorfile) {
  # Runs predictions for the 40 models from the correct winter-spring (wisp) or
  # summer-fall (sufa) time period
  #
  # Args:
  #   factorfile: the environmental data needed by the model to run predictions
  #
  # Returns:
  #   predictvec: the mean, SD, lower, and upper bound predictions from the 40 models.

  #Load required libraries. Function pkgTest is in the file Code/load_Functions.R and should have been loaded. If not test here and load file.
  
  if(exists("pkgTest")==FALSE) {
     print("Function pkgTest not found, loading file Code/load_Functions.R...")
     source("Code/load_Functions.R")
     }
     
  pkgTest("mgcv")
  pkgTest("sp")
  #pkgTest("rgdal") #nor needed unless using Excel?
  pkgTest("raster")
  pkgTest("gmt")
  
  
  #setwd("~/Dropbox/Documents/R/Blue_whales/Evan/")
  #Run all GAMs to average
  ####BE SURE TO HAVE THE RIGHT GAMM NAME BEFORE SENDING THIS OUT
  
  # IMPORTANT - The script assumes that it is in the correct parent directory, and then sets relative paths from there.
  
  #factorfile<-"Data/WhaleWatchFactors_9_2009.csv" #Only if you want to load specific file
  predictfile = sub("Data","Predictions",factorfile)
  predictfile = sub("Factors","Predictions",predictfile)
  
  logprint(paste("Loading factor file", factorfile))
  predfactors = read.csv(factorfile)
  wwvector = vector("list")
  
  #For SuFaindex<-GAMdataRunCCS$month>7 & GAMdataRunCCS$month<12 need to load bwhaleGAMM.sufa. Otheriwse bwhaleGAMM.wisp
  if(predfactors$month[1]>7 & predfactors$month[1]<12) {
      period<-"sufa"
      periodtext = "Summer-Fall"
  } else {
      period<-"wisp"
      periodtext = "Winter-Spring"
  }
  
  gammnm = paste("bwhaleGAMM.",period,sep="")
  #*******FOR DEBUGGING ONLY*****************
  #files = paste("ModelRuns/",gammnm,1:1,".RData",sep="") #Smaller number for debugging
  #files = c(files,files) #F
  #*******DEBUGGING END*****************
  files = paste("ModelRuns/",gammnm,1:40,".RData",sep="") #Uncomment this for production
  for(i in files){
  	logprint(paste("loading model",i))
    load(i)
  	bwhaleGAMM<-get(gammnm)
  	wwvector[[i]] = predict.gam(bwhaleGAMM$gam,get("predfactors"),se=TRUE, type="response")
  	names(wwvector) = sub((i),"",names(wwvector))
  }
  
  logprint("Making GAM data frame")
  gam = data.frame(wwvector)
  fit =gam[ ,substr(colnames(gam),1,1)=="f"]
  
  #Calculate density from predicted "presence". The equation is:
  # density = uP/(sum(uP)*E*S
  # uP = the mean predicted presence from the 40 models
  # E = the number of Blue Whales in the system (currently 1647)
  # S = the monthly scaling factor
  # Set up the variables
  E<-1647 #Scalar for converting to density
  Sarray<-c(0.03608,0.02821,0.04903,0.11700,0.22376,0.49830,0.67260,0.92464,0.91957,0.76547,0.34197,0.14138)
  S<-Sarray[predfactors$month[1]] #Scalar for converting to density
  
  fit2 = sweep(fit,2,colSums(fit,na.rm=T),`/`) #Apply dividing by column sum to each column
  dens = fit2*E*S #Get densities
  fitmean = rowMeans(fit,na.rm=T)
  sdfit = apply(fit,1,sd)
  predictvec = cbind(predfactors,fitmean,sdfit)
  predictvec$percent = predictvec$fitmean*100
  
  densmean = rowMeans(dens,na.rm=T) #Mean run from densities
  sddens = apply(dens,1,sd) #SD from densities
  
  predictvec$density<-densmean
  predictvec$sddens<-sddens
  
  #predictvec$density<-predictvec$fitmean/sum(predictvec$fitmean,na.rm=T)*E*S #Old calculation of density. Exactly 1:1 match with new calculation above 10/22/15 - EAH
  
  #Do upper and lower ranges for density
  predictvec$upper<-predictvec$density+predictvec$sddens
  predictvec$lower<-predictvec$density-predictvec$sddens
  predictvec$lower[predictvec$lower<0]<-0
  
  #Write a .csv file with all data if desired for comparisons
  logprint(paste("Writing predictions to file",predictfile))
  write.csv(predictvec,predictfile)
  
  #Make list of objects to return
  #out = list("tmpfile"=tmpfile,"predictvec"=predictvec)
  return(predictvec)
  
  #Now plot predict data as raster
  #plot_GAMMRaster(tmpfile, predictvec)
  
  #Do checks on files and then remove temp directory
  #logprint(paste("Cleaning up temp directory", tmpdir))
  #unlink(tmpdir,recursive=TRUE)
}
