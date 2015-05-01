library(mgcv)
library(ape)
library(ncf)
library(ncdf)
library(spdep)
library(sp)
require(maps)       # for map.where
require(mapdata)    # for worldHires
require(dismo)
require(gbm)
require(rasterVis)
#require(reshape)

##http://www.r-bloggers.com/plot-maps-like-a-boss/

plotland<-function(xmin,xmax,ymin,ymax){
map('worldHires', xlim=c(xmin,xmax), ylim=c(ymin,ymax), resolution=0)         # atlantic-centric projection
map.axes()
for (xl in xmin*10 : xmax*10){
  for (yl in ymin*10 : ymax*10){
    pT=SpatialPoints(matrix(c(xl/10,yl/10),nrow=1))
    proj4string(pT)<-proj4string(CC.sp)
        place = over(pT, CC.sp)
        if(is.na(place)) {
                points(xl/10, yl/10, col='blue', pch=20)
        } else {
                points(xl/10, yl/10, col='red', pch=20)
        }
  }
}
}

makeTransparent<-function(someColor, alpha=100)
{
  newColor<-col2rgb(someColor)
  apply(newColor, 2, function(curcoldata){rgb(red=curcoldata[1], green=curcoldata[2],
    blue=curcoldata[3],alpha=alpha, maxColorValue=255)})
}

addslopeaspect<-function(datafile,slope=NA,aspect=NA)
{
  datafile$slope[]<-NA
  datafile$aspect[]<-NA
  for (i in 1:length(datafile$lat)){
    nearpt<-spDistsN1(cbind(slope$lon,slope$lat),c(datafile$lon[i],datafile$lat[i]), longlat = TRUE)
    slopept<-slope[which(nearpt==min(nearpt)),]
    aspectpt<-aspect[which(nearpt==min(nearpt)),]
    datafile$slope[i]<-slopept
    datafile$aspect[i]<-aspectpt
  }

  return(datafile)
}

matchpts<-function(masterfile,tempfile){
  coordinates(masterfile) <- c("lon", "lat")
  coordinates(tempfile) <- c("lon", "lat")      

  closestSiteVecS <- vector(mode = "numeric",length = nrow(masterfile))
  minDistVecS     <- vector(mode = "numeric",length = nrow(masterfile))

  for (i in 1 : nrow(masterfile))
   {
      distVec <- spDistsN1(tempfile,masterfile[i,],longlat = TRUE)
      minDistVecS[i] <- min(distVec)
      closestSiteVecS[i] <- which.min(distVec)
   }
     return(closestSiteVecS)
}
rotatesshrms<-function(factors,mkplot=FALSE){
  fitraster<-factors[,c("lat","lon","sshrms")]
  #fitmelt<-melt(fitraster, id=c("lat", "lon"))

  coordinates(fitraster) <- ~ lon + lat
  gridded(fitraster) <- TRUE
  rasterDF2 <- raster(fitraster)
  rasterDF3 <- flip(t(rasterDF2),2)
#  sshrms.mat<-as.matrix(flip(t(rasterDF2),2))
#  factors$sshrms<-as.vector(melt(sshrms.mat)[3])
  #image(rasterDF2)
  par(mfrow=c(1,2))
  plot(rasterDF2,legend=T,col=heat.colors(100))
  plot(rasterDF3,legend=T,col=heat.colors(100))
  factors$sshrms<-as.vector(melt(as.matrix(rasterDF3))[3])[[1]]
  return(factors)

}

rasterfactor<-function(fitraster){
  #fitmelt<-melt(fitraster, id=c("lat", "lon"))
  names(fitraster)<-c("lon","lat","value")
  coordinates(fitraster) <- ~ lon + lat
  gridded(fitraster) <- TRUE
  rasterDF2 <- raster(fitraster)
#  rasterDF3 <- flip(t(rasterDF2),2)
#  sshrms.mat<-as.matrix(flip(t(rasterDF2),2))
#  factors$sshrms<-as.vector(melt(sshrms.mat)[3])
  #image(rasterDF2)
#  par(mfrow=c(1,2))
  plot(rasterDF2,legend=T,col=heat.colors(100))
#  plot(rasterDF3,legend=T,col=heat.colors(100))
#  factors$sshrms<-as.vector(melt(as.matrix(rasterDF3))[3])[[1]]
#  return(factors)

}


plothovmoller=function(extract){

  ymin<-min(as.numeric(unlist(strsplit(names(extract[1,,1]),"N"))))
  ymax<-max(as.numeric(unlist(strsplit(names(extract[1,,1]),"N"))))
  ymax<-40
  xmin<-min(as.numeric(unlist(strsplit(names(extract[,1,1]),"E"))))
  xmax<-max(as.numeric(unlist(strsplit(names(extract[,1,1]),"E"))))

  dates<-names(extract[1,1,])
  rbextract<-brick(extract,xmn=xmin,xmx=xmax,ymn=ymin,ymx=ymax, crs="+proj=longlat +datum=WGS84")

  dirLayer <- init(rbextract, v='y')
# dirLayerx <- init(rbextract, v='x')
  z <- zonal(rbextract, dirLayer, FUN='mean', digits=2)
  idx<-names(extract[1,1,])
  dat <- expand.grid(y=z[,1], x=idx)
  dat$z <- as.vector(z[,-1], mode='numeric')
  dat$z<-dat$z  
  dat$mdate<-paste(as.numeric(format(as.Date(dat$x), "%m")),as.numeric(format(as.Date(dat$x), "%Y"),sep=' '))
# spplot(subset(rbextract,1))    # PLOT LAYER 1
# coltheme<-simpleTheme(col=c("dark blue","white","yellow","orange"))
# coltheme<-BuRdTheme()
# coltheme<-PuOrTheme(brewer.pal(9, 'YlOrRd'))
  coltheme<-PuOrTheme(rev(brewer.pal(9,"Spectral")))
  levelplot(z ~ x*y, data=dat,
          xlab='Time', ylab='Latitude',
          panel=panel.levelplot.raster,     #          panel.lmline(fishx,fishy,lwd=2),
          interpolate=TRUE,
          scales=list(x=list(rot=90,at=seq(1,length(dates),by=6))),
          par.settings=coltheme,   
 #         par.settings=GrTheme(),   
      contour=F)

}


plotlevs=function(extract){

  ymin<-min(as.numeric(unlist(strsplit(names(extract[1,,1]),"N"))))
  ymax<-max(as.numeric(unlist(strsplit(names(extract[1,,1]),"N"))))
  ymax<-40
  xmin<-min(as.numeric(unlist(strsplit(names(extract[,1,1]),"E"))))
  xmax<-max(as.numeric(unlist(strsplit(names(extract[,1,1]),"E"))))

  dates<-names(extract[1,1,])
  rbextract<-brick(extract,xmn=xmin,xmx=xmax,ymn=ymin,ymx=ymax, crs="+proj=longlat +datum=WGS84")

  dirLayer <- init(rbextract, v='y')
# dirLayerx <- init(rbextract, v='x')
  z <- zonal(rbextract, dirLayer, FUN='mean', digits=2)
  idx<-names(extract[1,1,])
  dat <- expand.grid(y=z[,1], x=idx)
  dat$z <- as.vector(z[,-1], mode='numeric')
  dat$z<-dat$z  
  dat$mdate<-paste(as.numeric(format(as.Date(dat$x), "%m")),as.numeric(format(as.Date(dat$x), "%Y"),sep=' '))
# spplot(subset(rbextract,1))    # PLOT LAYER 1
# coltheme<-simpleTheme(col=c("dark blue","white","yellow","orange"))
# coltheme<-BuRdTheme()
# coltheme<-PuOrTheme(brewer.pal(9, 'YlOrRd'))
  coltheme<-PuOrTheme(rev(brewer.pal(9,"Spectral")))
  levelplot(z ~ x*y, data=dat,
          xlab='Time', ylab='Latitude',
          panel=panel.levelplot.raster,     #          panel.lmline(fishx,fishy,lwd=2),
          interpolate=TRUE,
          scales=list(x=list(rot=90,at=seq(1,length(dates),by=6))),
          par.settings=coltheme,   
 #         par.settings=GrTheme(),   
      contour=F)

}



  setwd('~/Dropbox/Documents/R/Blue_whales/Data/')
  wd<-getwd(); path<-wd

  #loadhistory(file=paste(getwd(),"WWatch.Rhistory",sep="/")) # default is ".Rhistory"
  #load(file=paste(wd,"WWatch.RData",sep="/"))
  load(file=paste(wd,"WWatchv3.RData",sep="/"))
  load(file="shippingraster.RData")
  ##### RERUN MODELS WITH DATAFRAMES

#startmodel<-12
nummodels<-40
numruns<-nummodels

rsq<-c(1:numruns); AICl<-c(1:numruns); tabs<-c(1:numruns); #AUCvals<-c(1:numruns)
for(l in 1:numruns) {
run<-l

GAMdataRunX<-get(paste("GAMdataRun",run,sep=''))

## Grab slope and aspect data
#slope = read.table("slopenan.xyz")
#aspect = read.table("aspectnan.xyz")
slope = read.table("slope_filtered.xyz")
aspect = read.table("aspect_filtered.xyz")
colnames(slope)<-c("lon","lat","slope")
colnames(aspect)<-c("lon","lat","aspect")

# NEED TO GET FULL RANGE OF SLOPES & ASPECTS IN bathymetry_grdfilter_SD
#GAMdataRunX<-addslopeaspect(GAMdataRunX,slope,aspect)

#### Reduce data to CCS domain
#CCSindex<-(GAMdataRunX$lat>32.533 & GAMdataRunX$lon<(-117.12076))&(GAMdataRunX$lat<49) & GAMdataRunX$lon<(-135)#|GAMdataRunX$presabs==0
CCSindex<-(GAMdataRunX$lat>30) & GAMdataRunX$lon<(-115.3) &(GAMdataRunX$lat<49) & GAMdataRunX$lon>(-135)#|GAMdataRunX$presabs==0
CCSindex<-rep(TRUE,length(GAMdataRunX$lat))
GAMdataRunCCS<-GAMdataRunX[CCSindex,]
GAMdataRunCCS$sst[GAMdataRunCCS$sst<1]<-NA
#SuFaindex<-GAMdataRunCCS$month>5 & GAMdataRunCCS$month<11
SuFaindex<-GAMdataRunCCS$month>7 & GAMdataRunCCS$month<12
WiSpindex<-!SuFaindex
inshindex<-GAMdataRunCCS$bathy<1001
offshindex<-GAMdataRunCCS$bathy>1000

# NEED TO GET FULL RANGE OF SLOPES & ASPECTS IN bathymetry_grdfilter_SD
#GAMdataRunCCS<-addslopeaspect(GAMdataRunCCS,slope,aspect)
GAMdataRunCCSsl<-matchpts(GAMdataRunCCS,slope)
GAMdataRunCCSas<-matchpts(GAMdataRunCCS,aspect)
GAMdataRunCCS$slope<-slope[GAMdataRunCCSsl,3]
GAMdataRunCCS$aspect<-aspect[GAMdataRunCCSsl,3]

try(bwhaleGAMM.sufa<-gamm(presabs~s(sst,bs="ts",k=5)+s(log(chl+0.0001),bs="ts",k=5)+s(sshrms,bs="ts", k=5)+s(bathy,k=5,bs="ts")+s(bathyrms,k=5,bs="ts")+month, random=list(ptt=~1),family=binomial, niterPQL=50, data=GAMdataRunCCS[SuFaindex,]))
try(bwhaleGAMM.wisp<-gamm(presabs~s(sst,bs="ts",k=5)+s(log(chl+0.0001),bs="ts",k=5)+s(sshrms,bs="ts", k=5)+s(bathy,k=5,bs="ts")+s(bathyrms,k=5,bs="ts")+month, random=list(ptt=~1),family=binomial, niterPQL=50, data=GAMdataRunCCS[WiSpindex,]))
try(bwhaleGAMM<-gamm(presabs~s(sst,bs="ts",k=5)+s(log(chl+0.0001),bs="ts",k=5)+s(sshrms,bs="ts", k=5)+s(bathy,k=5,bs="ts")+s(bathyrms,k=5,bs="ts")+month, random=list(ptt=~1),family=binomial, niterPQL=50, data=GAMdataRunCCS))

##try(bwhaleGAMM<-gamm(presabs~s(sst,bs="ts",k=5)+s(log(chl+0.0001),bs="ts",k=5)+s(log(eke+0.0001),bs="ts",k=5)+s(sshrms,bs="ts", k=5)+s(bathy,k=5,bs="ts")+s(bathyrms,k=5,bs="ts"), random=list(ptt=~1),family=binomial, niterPQL=50, data=GAMdataRunX))
### TRY adding monthxlon interaction
AIC(bwhaleGAMM$lme)

## PLOT ALL POINTS 
#plot(c(GAMdataRun1$lon, GAMdataRun2$lon, GAMdataRun3$lon, GAMdataRun4$lon, GAMdataRun5$lon, GAMdataRun6$lon, GAMdataRun7$lon, GAMdataRun8$lon, GAMdataRun9$lon, GAMdataRun10$lon, GAMdataRun11$lon, GAMdataRun12$lon, GAMdataRun13$lon, GAMdataRun14$lon, GAMdataRun15$lon, GAMdataRun16$lon, GAMdataRun17$lon, GAMdataRun18$lon, GAMdataRun19$lon, GAMdataRun20$lon, GAMdataRun21$lon, GAMdataRun22$lon, GAMdataRun23$lon, GAMdataRun24$lon, GAMdataRun25$lon, GAMdataRun26$lon, GAMdataRun27$lon, GAMdataRun28$lon, GAMdataRun29$lon, GAMdataRun30$lon, GAMdataRun31$lon, GAMdataRun32$lon, GAMdataRun33$lon, GAMdataRun34$lon, GAMdataRun35$lon, GAMdataRun36$lon, GAMdataRun37$lon, GAMdataRun38$lon, GAMdataRun39$lon, GAMdataRun40$lon),c(GAMdataRun1$lat, GAMdataRun2$lat, GAMdataRun3$lat, GAMdataRun4$lat, GAMdataRun5$lat, GAMdataRun6$lat, GAMdataRun7$lat, GAMdataRun8$lat, GAMdataRun9$lat, GAMdataRun10$lat, GAMdataRun11$lat, GAMdataRun12$lat, GAMdataRun13$lat, GAMdataRun14$lat, GAMdataRun15$lat, GAMdataRun16$lat, GAMdataRun17$lat, GAMdataRun18$lat, GAMdataRun19$lat, GAMdataRun20$lat, GAMdataRun21$lat, GAMdataRun22$lat, GAMdataRun23$lat, GAMdataRun24$lat, GAMdataRun25$lat, GAMdataRun26$lat, GAMdataRun27$lat, GAMdataRun28$lat, GAMdataRun29$lat, GAMdataRun30$lat, GAMdataRun31$lat, GAMdataRun32$lat, GAMdataRun33$lat, GAMdataRun34$lat, GAMdataRun35$lat, GAMdataRun36$lat, GAMdataRun37$lat, GAMdataRun38$lat, GAMdataRun39$lat, GAMdataRun40$lat),ylab="lat",xlab='lon')

## BRT try
GAMdataRunX<-subset(get(paste("GAMdataRun",l,sep='')),select=c(presabs,lat,lon,ptt,year,month,sst,chl,sshrms,eke,uy10,bathy,bathyrms))
GAMdataRunX$logchl<-log(GAMdataRunX$chl+0.0001)
GAMdataRunX$logeke<-log(GAMdataRunX$eke+0.0001)
GAMdataRunCCS$logchl<-log(GAMdataRunCCS$chl+0.0001)
GAMdataRunCCS$logeke<-log(GAMdataRunCCS$eke+0.0001)

do.call("<-",list(paste("BRTdataRun",l,sep=''),subset(GAMdataRunX,select=c(presabs,lat,lon,ptt,year,month,sst,logchl,sshrms,logeke,uy10,bathy,bathyrms))))  # could add other varibles in col 3-14
do.call("<-",list(paste("BRTdataRun",l,sep=''),na.omit(get(paste("BRTdataRun",l,sep='')))))  # could add other variables in col 3-14

	do.call("<-",list("GAMdataRunCCSsub",subset(GAMdataRunCCS[SuFaindex,],select=c(presabs,lat,lon,ptt,year,month,sst,logchl,sshrms,bathy,bathyrms))))  # could add other varibles in col 3-14
	do.call("<-",list("GAMdataRunCCSsub.sufa",na.omit(GAMdataRunCCSsub)))  # could add other variables in col 3-14
	do.call("<-",list("GAMdataRunCCSsub",subset(GAMdataRunCCS[WiSpindex,],select=c(presabs,lat,lon,ptt,year,month,sst,logchl,sshrms,bathy,bathyrms))))  # could add other varibles in col 3-14
	do.call("<-",list("GAMdataRunCCSsub.wisp",na.omit(GAMdataRunCCSsub)))  # could add other variables in col 3-14
  do.call("<-",list("GAMdataRunCCSsub",subset(GAMdataRunCCS,select=c(presabs,lat,lon,ptt,year,month,sst,logchl,sshrms,bathy,bathyrms))))  # could add other varibles in col 3-14
  do.call("<-",list("GAMdataRunCCSsub",na.omit(GAMdataRunCCSsub)))  # could add other variables in col 3-14
  #GAMdataRunCCSsub<-GAMdataRunCCSsub.sufa

nterms=dim(GAMdataRunCCSsub)[2]


#try(bwhaleBRT.lr005<-gbm.fixed(data=get(paste("BRTdataRun",l,sep='')),gbm.x=2:13, gbm.y=1, family="bernoulli",tree.complexity=5, learning.rate = 0.005, n.trees = 2000, bag.fraction=0.75))
try(bwhaleBRT.lr005.sufa<-gbm.fixed(data=GAMdataRunCCSsub.sufa,gbm.x=2:nterms, gbm.y=1, family="bernoulli",tree.complexity=5, learning.rate = 0.005, n.trees = 2000, bag.fraction=0.75))
try(bwhaleBRT.lr005.wisp<-gbm.fixed(data=GAMdataRunCCSsub.wisp,gbm.x=2:nterms, gbm.y=1, family="bernoulli",tree.complexity=5, learning.rate = 0.005, n.trees = 2000, bag.fraction=0.75))
try(bwhaleBRT.lr005<-gbm.fixed(data=GAMdataRunCCSsub,gbm.x=2:nterms, gbm.y=1, family="bernoulli",tree.complexity=5, learning.rate = 0.005, n.trees = 2000, bag.fraction=0.75))
#try(bwhaleBRT.lr005<-gbm.step(data=GAMdataRunCCSsub,gbm.x=2:nterms, gbm.y=1, family="bernoulli",tree.complexity=5, learning.rate = 0.005, n.trees = 2000, bag.fraction=0.75))

#gbm.plot(bwhaleBRT.lr005, n.plots=10, write.title = FALSE)
#quartz(); gbm.plot.fits(bwhaleBRT.lr005)
#summary(bwhaleBRT.lr005)
### ReRead JAE paper on BRTs to figure out troubleshooting / model fits

rsq[l]<-summary(bwhaleGAMM$gam)[10]
AICl[l]<-AIC(bwhaleGAMM$lme)
tabs[l]<-summary(bwhaleGAMM$gam)[24]
#newdata<-predict.gam(bwhaleGAMM$gam,get(paste("GAMdataRun",l,sep='')),se=TRUE, type="response")
newdata<-predict.gam(bwhaleGAMM$gam,GAMdataRunCCS,se=TRUE, type="response")
do.call("<-",list(paste("newdata",l,sep=''),cbind(GAMdataRunCCS,newdata)))
writefile<-get(paste("newdata",l,sep=''))
writefile<-writefile[!is.na(writefile$lon)&!is.na(writefile$lat),]
#names(writefile)<-c(names(writefile)[1:21],"sefit")
write.csv(writefile,file = paste(path,"/PredictV5/GAMdataRun",run,".csv",sep=''))
save(bwhaleGAMM,file = paste(path,"/PredictV5/bwhaleGAMM",l,".RData",sep=''))
save(bwhaleBRT.lr005,file = paste(path,"/PredictV5/bwhaleBRT",l,".RData",sep=''))
save(bwhaleGAMM.sufa,file = paste(path,"/PredictV5/bwhaleGAMM.sufa",l,".RData",sep=''))
save(bwhaleBRT.lr005.sufa,file = paste(path,"/PredictV5/bwhaleBRT.sufa",l,".RData",sep=''))
save(bwhaleGAMM.wisp,file = paste(path,"/PredictV5/bwhaleGAMM.wisp",l,".RData",sep=''))
save(bwhaleBRT.lr005.wisp,file = paste(path,"/PredictV5/bwhaleBRT.wisp",l,".RData",sep=''))

### PLOTS
pdf(paste(path,"/PredictV5/bwhaleBRT_lr005_",l,".pdf",sep=''))
plot.gbm(bwhaleBRT.lr005, n.plots=10, write.title = FALSE)
dev.off()

pdf(paste(path,"/PredictV5/bwhaleGAMM_",l,".pdf",sep=''))
par(mfrow=c(3,3))
plot(bwhaleGAMM$gam, shade=TRUE)
dev.off()

pdf(paste(path,"/PredictV5/bwhale_map_",l,".pdf",sep=''))
map('worldHires', xlim=c(-130,-110), ylim=c(20,50))         # pacific-centric projection
map.axes()
points(GAMdataRunX$lon[GAMdataRunX$presabs==0],GAMdataRunX$lat[GAMdataRunX$presabs==0], col=makeTransparent('red',85), pch=16, cex=.5)
points(GAMdataRunX$lon[GAMdataRunX$presabs==1],GAMdataRunX$lat[GAMdataRunX$presabs==1], col=makeTransparent('blue',85), pch=16, cex=.5)
rect(-135, 30, -115, 49, density = NULL, angle = 45, col = NA, border = "black")
dev.off()

pdf(paste(path,"/PredictV5/bwhale_map_CCS_",l,".pdf",sep=''))
map('worldHires', xlim=c(-140,-110), ylim=c(20,60))         # pacific-centric projection
map.axes()
points(GAMdataRunCCS$lon[GAMdataRunCCS$presabs==0],GAMdataRunCCS$lat[GAMdataRunCCS$presabs==0], col=makeTransparent('red',85), pch=16, cex=.5)
points(GAMdataRunCCS$lon[GAMdataRunCCS$presabs==1],GAMdataRunCCS$lat[GAMdataRunCCS$presabs==1], col=makeTransparent('blue',85), pch=16, cex=.5)
rect(-135, 30, -115, 49, density = NULL, angle = 45, col = NA, border = "black")
dev.off()


pdf(paste(path,"/PredictV5/bwhaleBRT_lr005_summary_",l,".pdf",sep=''))
summary(bwhaleBRT.lr005)
dev.off()

}

#write.table(tabs,file = paste(path,"/PredictCCSv3/SummaryTables.txt",sep=''))
sink(paste(path,"/PredictV5/SummaryTables.txt",sep=''))
print(tabs)
sink()

write.csv(rsq,file = paste(path,"/PredictV5/Rsquareds.csv",sep=''))
write.csv(AICl,file = paste(path,"/PredictV5/AICvals.csv",sep=''))



################################### PREDICT MODELS

setwd('~/Dropbox/Documents/R/Blue_whales/Data/')
wd<-getwd(); #modeldir<-'/PredictV2/'
data(wrld_simpl, package = "maptools")
#USA <- getData('GADM', country='USA', level=0)
#CAN <- getData('GADM', country='CAN', level=0)
#MEX<-getData('GADM', country='MEX', level=0)


#modeldir<-'/PredictCCSv1/'
## add more variables to v2
modeldir<-'/PredictV5/'

path<-wd
PRDsightings<-read.csv("/Users/elliotthazen/Dropbox/Documents/R/Blue_whales/PRD/CCE1991-2008_Bm_OnEffortSightings.csv")


#yrs<-c(2009,2009,2009,2009,2009,2009,2009)
#mos<-c(4,5,7,8,9,10,11)
yrs<-c(2009,2009,2009,2009,2009,2009,2009,2009,2009,2009,2009,2009,2008,2008,2008,2008)
mos<-c(1,2,3,4,5,6,7,8,9,10,11,12,8,9,10,11)
#yrs<-c(2009,2009,2008)
#mos<-c(11,12,9)
#yrs<-c(2008,2008,2008,2008)
#mos<-c(8,9,10,11)

for (lmno in 1:length(yrs)){
	yr<-yrs[lmno]
	mn<-mos[lmno]


filenm<-paste("WhaleWatchFactors_",mn,"_",yr,".csv",sep='')

#factors = read.csv("WhaleWatchFactors_6_2008.csv")
#factors = read.csv("WhaleWatchFactors_12_2008.csv")
factors = read.csv(filenm)
#factors = read.csv("WhaleWatchFactors_12_2009.csv")

#setwd(paste(wd,modeldir,sep=''))

factors$year = yr
factors$month = mn
factors$day = 16
factors$ptt<-3127773
factors$logchl<-log(factors$chl+0.0001)
#factors$logeke<-log(factors$eke+0.0001)
factors$logeke<-NA
factors$ssh<-factors$sshd
#if(is.na(factors$sshrms[factors$lat==35&factors$lon==240])){
#  factorsnew<-rotatesshrms(factors,TRUE)
#  factorscrap<-rotatesshrms(factorsnew,TRUE)
#} 
#factors$lon<-360-factors$lon

outdir<-paste(factors$month,factors$year,'/',sep='')[1]
myear<-paste(factors$month,factors$year,sep='')[1]

#names(factors)[12]<-"sshrms"
#names(factors2)[12]<-"sshrms"

wwvector = vector('list')
#files = paste("bwhaleGAMM_reduced",1:40,".RData",sep="")

files = paste(wd,modeldir,"bwhaleGAMM",1:nummodels,".RData",sep="")
whichfiles=c(1:nummodels) #c(7,9,11,12,13,14,15) # c(1:15)
files=files[whichfiles]

for(i in files){
	load(i)
	pregam = predict.gam(bwhaleGAMM$gam,get("factors"),se=TRUE, type="response")
	wwvector[[i]]<-pregam
	fit<-pregam$fit
	se<-pregam$se.fit
	predict = cbind(factors,fit,se)
	predict$percent = predict$fit*100
  predict$density = predict$fit/sum(predict$fit,na.rm=T)*1647
  # summary(GAMdataRun1[GAMdataRun1$presabs==1,])
  # GAMdataRun1$lat[GAMdataRun1$presabs==1&GAMdataRun1$month==mn]
  # Scale by population w/in the CCS
  Trues<-GAMdataRun1$lat[GAMdataRun1$presabs==1&GAMdataRun1$month==mn]<49 & GAMdataRun1$lat[GAMdataRun1$presabs==1&GAMdataRun1$month==mn]>30 & GAMdataRun1$lon[GAMdataRun1$presabs==1&GAMdataRun1$month==mn]<(-115.3) & GAMdataRun1$lon[GAMdataRun1$presabs==1&GAMdataRun1$month==mn]>(-135)
  perc.pop<-sum(Trues,na.rm=TRUE)/length(Trues)
  predict$perc.pop<-perc.pop
  predict$density<-perc.pop*predict$density
  predict$fit.up<-predict$fit+predict$se
  predict$fit.down<-predict$fit-predict$se

	write.csv(predict,paste(wd,modeldir,outdir,"WhaleWatchPredictionsCCS_",whichfiles[which(files==i)],".csv",sep=''))
	names(wwvector) = sub((i),"",names(wwvector))
	fitcol<-dim(predict)[2]-2
	sdcol<-dim(predict)[2]-1
	fitcol<-names(predict)=="fit"
  sdcol<-names(predict)=="se.fit"
  fitxyz = data.frame(predict[2],predict[1],predict[fitcol])
	sdxyz = data.frame(predict[2],predict[1],predict[sdcol])
	write.table(fitxyz,paste(wd,modeldir,outdir,"fit",whichfiles[which(files==i)],".txt",sep=''),col.names=FALSE,row.names=F,quote=FALSE) #must have no headers, no row names, no quotes
	write.table(sdxyz,paste(wd,modeldir,outdir,"sd",whichfiles[which(files==i)],".txt",sep=''),col.names=FALSE,row.names=F,quote=FALSE)

	fitraster<-predict[,c("lat","lon","fit")]
	coordinates(fitraster) <- ~ lon + lat
	gridded(fitraster) <- TRUE
	rasterDF2 <- raster(fitraster)

  fitraster<-predict[,c("lat","lon","fit.up")]
  coordinates(fitraster) <- ~ lon + lat
  gridded(fitraster) <- TRUE
  rasterDF2.up <- raster(fitraster)
  
  fitraster<-predict[,c("lat","lon","fit.down")]
  coordinates(fitraster) <- ~ lon + lat
  gridded(fitraster) <- TRUE
  rasterDF2.down <- raster(fitraster)

	#quartz()
	image(rasterDF2)
	coltheme<-PuOrTheme(rev(brewer.pal(9,"Spectral")))
  #plot(rasterDF2,legend=T,col=heat.colors(100))
  plot(rasterDF2,legend=T,col=coltheme$regions$col,main=paste(mn,yr,sep='-'))
  times<-PRDsightings$year==yr&PRDsightings$month==mn
  if(sum(times, na.rm=TRUE)>0) {
    par(new=T)
    plot(360+PRDsightings$slon[times],PRDsightings$slat[times],cex=1,axes=F,xlab="",ylab="",pch=16,xlim=c(extent(rasterDF2)@xmin,extent(rasterDF2)@xmax),ylim=c(extent(rasterDF2)@ymin,extent(rasterDF2)@ymax))
  }

  draster<-predict[,c("lat","lon","density")]
  coordinates(draster) <- ~ lon + lat
  gridded(draster) <- TRUE
  rasterDF3 <- raster(draster)
  rm(draster,fitraster)
  #quartz()
  #image(rasterDF3)
  
  #FIT
	pdf(paste(wd,modeldir,outdir,"bwhale_FIT_GAM_",myear,"_",whichfiles[which(files==i)],".pdf",sep=''))
	#pdf(paste(getwd(),"bwhale_FIT_BRT_122009.pdf",sep='/'))
	extent(rasterDF2)@xmin<-extent(rasterDF2)@xmin-360
	extent(rasterDF2)@xmax<-extent(rasterDF2)@xmax-360
	plot(rasterDF2,legend=T,col=coltheme$regions$col,zlim=c(0,1),main=paste(mn,yr,sep='-'))
	plot(wrld_simpl, add = T, col="white",border=NA)	
  if(sum(times, na.rm=TRUE)>0) {
    par(new=T)
    plot(PRDsightings$slon[times],PRDsightings$slat[times],cex=1,axes=F,xlab="",ylab="",pch=16,xlim=c(extent(rasterDF2)@xmin,extent(rasterDF2)@xmax),ylim=c(extent(rasterDF2)@ymin,extent(rasterDF2)@ymax))
  }
#	plot(USA, add = T, col="white",lwd=0.1,border=NA)	
#	plot(CAN, add = T, col="white",lwd=0.1,border=NA)	
#	plot(MEX, add = T, col="white",lwd=0.1,border=NA)	
	dev.off()

  #WhaleShipRisk
  whalerisk<-overlay(rasterDF2,rp.resamp,fun=function(x,y){return(x*y)})
  pdf(paste(wd,modeldir,outdir,"bwhale_shipping_GAM_",myear,"_",whichfiles[which(files==i)],".pdf",sep=''))
  #pdf(paste(getwd(),"bwhale_FIT_BRT_122009.pdf",sep='/'))
  plot(whalerisk,legend=T,col=coltheme$regions$col,zlim=c(0,1))#,main=paste(mn,yr,sep='-'))
  plot(wrld_simpl, add = T, col="white",border=NA)  
  dev.off()

#### TO CALCULATE ERRORS
  # #UPPER BOUNDS FROM SE
  # pdf(paste(wd,modeldir,outdir,"bwhale_FIT_GAM_",myear,"_",whichfiles[which(files==i)],"-upper.pdf",sep=''))
  # #pdf(paste(getwd(),"bwhale_FIT_BRT_122009.pdf",sep='/'))
  # extent(rasterDF2.up)@xmin<-extent(rasterDF2.up)@xmin-360
  # extent(rasterDF2.up)@xmax<-extent(rasterDF2.up)@xmax-360
  # plot(rasterDF2.up,legend=T,col=coltheme$regions$col,zlim=c(0,1),main=paste(mn,yr,sep='-'))
  # plot(wrld_simpl, add = T, col="white",border=NA)  
  # if(sum(times, na.rm=TRUE)>0) {
  #   par(new=T)
  #   plot(PRDsightings$slon[times],PRDsightings$slat[times],cex=1,axes=F,xlab="",ylab="",pch=16,xlim=c(extent(rasterDF2.up)@xmin,extent(rasterDF2.up)@xmax),ylim=c(extent(rasterDF2.up)@ymin,extent(rasterDF2.up)@ymax))
  # }
  # dev.off()

  # #LOWER BOUNDS FROM SE
  # pdf(paste(wd,modeldir,outdir,"bwhale_FIT_GAM_",myear,"_",whichfiles[which(files==i)],"-lower.pdf",sep=''))
  # #pdf(paste(getwd(),"bwhale_FIT_BRT_122009.pdf",sep='/'))
  # extent(rasterDF2.down)@xmin<-extent(rasterDF2.down)@xmin-360
  # extent(rasterDF2.down)@xmax<-extent(rasterDF2.down)@xmax-360
  # plot(rasterDF2.down,legend=T,col=coltheme$regions$col,zlim=c(0,1),main=paste(mn,yr,sep='-'))
  # plot(wrld_simpl, add = T, col="white",border=NA)  
  # if(sum(times, na.rm=TRUE)>0) {
  #   par(new=T)
  #   plot(PRDsightings$slon[times],PRDsightings$slat[times],cex=1,axes=F,xlab="",ylab="",pch=16,xlim=c(extent(rasterDF2.down)@xmin,extent(rasterDF2.down)@xmax),ylim=c(extent(rasterDF2.down)@ymin,extent(rasterDF2.down)@ymax))
  # }
  # dev.off()

  pdf(paste(wd,modeldir,outdir,"bwhale_dens_GAM_",myear,"_",whichfiles[which(files==i)],".pdf",sep=''))
  extent(rasterDF3)@xmin<-extent(rasterDF3)@xmin-360
  extent(rasterDF3)@xmax<-extent(rasterDF3)@xmax-360
  plot(rasterDF3,legend=T,col=coltheme$regions$col,zlim=c(0,3))
  plot(wrld_simpl, add = T, col="white",border=NA)  
  if(sum(times, na.rm=TRUE)>0) {
    par(new=T)
    plot(PRDsightings$slon[times],PRDsightings$slat[times],cex=1,axes=F,xlab="",ylab="",pch=16,xlim=c(extent(rasterDF3)@xmin,extent(rasterDF3)@xmax),ylim=c(extent(rasterDF3)@ymin,extent(rasterDF3)@ymax))
  }
  dev.off()

}

#gam = data.frame(wwvector)
#fit =gam[ ,!substr(colnames(gam),1,1)=="s"]
#sefit =gam[ ,substr(colnames(gam),1,1)=="s"]
#fitmean = rowMeans(fit,na.rm=T)
#sd = apply(fit,1,sd)
#predict = cbind(factors,fitmean,sd)
#predict$percent = predict$fitmean*100


#Write a .csv file with all data if desired for comparisons
#write.csv(predict,"WhaleWatchPredictionsCCS.csv")

#Create text files for GMT  
#fitcol<-dim(predict)[2]-3
#sdcol<-dim(predict)[2]-2
#fitxyz = data.frame(predict[2],predict[1],predict[fitcol])
#sdxyz = data.frame(predict[2],predict[1],predict[sdcol])
#write.table(fitxyz,"fit.txt",col.names=FALSE,row.names=F,quote=FALSE) #must have no headers, no row names, no quotes
#write.table(sdxyz,"sd.txt",col.names=FALSE,row.names=F,quote=FALSE)




##### BRT predictions

wwbrtvector = vector('list')
wwbrtAUC = vector('list')

filesbrt = paste(wd,modeldir,"bwhaleBRT",1:nummodels,".RData",sep="")
filesobs = paste(wd,modeldir,"GAMdataRun",run,".csv",sep="")

whichfiles=c(1:nummodels)
filesbrt=filesbrt[whichfiles]

for(i in filesbrt){
	load(i)
	brtobs<-read.csv(filesobs[1])
	brtobs$logchl<-log(brtobs$chl+0.0001)
	brtobs$logeke<-log(brtobs$eke+0.0001)

	pregbm = predict.gbm(bwhaleBRT.lr005, brtobs,n.trees=bwhaleBRT.lr005$gbm.call$best.trees, type="response")
	d <- cbind(brtobs$presabs, pregbm)
	pres <- d[d[,1]==1, 2]
	abs <- d[d[,1]==0, 2]
	e <- evaluate(p=pres, a=abs)

	
	fit = predict.gbm(bwhaleBRT.lr005, factors,n.trees=bwhaleBRT.lr005$gbm.call$best.trees, type="response")
	wwbrtvector[[i]] = fit
	wwbrtAUC[[i]] = e
	#pregbm<-wwbrtvector[[i]]
	predict = cbind(factors,fit)
	predict$percent = predict$fit*100
  predict$density = predict$fit/sum(predict$fit)*1647
  Trues<-GAMdataRun1$lat[GAMdataRun1$presabs==1&GAMdataRun1$month==mn]<49 & GAMdataRun1$lat[GAMdataRun1$presabs==1&GAMdataRun1$month==mn]>30 & GAMdataRun1$lon[GAMdataRun1$presabs==1&GAMdataRun1$month==mn]<(-115.3) & GAMdataRun1$lon[GAMdataRun1$presabs==1&GAMdataRun1$month==mn]>(-135)
  perc.pop<-sum(Trues,na.rm=TRUE)/length(Trues)
  predict$perc.pop<-perc.pop
  predict$density<-perc.pop*predict$density

	write.csv(predict,paste(wd,modeldir,outdir,"WhaleWatchPredictionsCCS-BRT_",whichfiles[which(filesbrt==i)],".csv",sep=""))
	fitxyz = data.frame(predict[2],predict[1],predict[dim(predict)[2]-1])
	#sdxyz = data.frame(predict[2],predict[1],predict[sdcol])
	write.table(fitxyz,paste(wd,modeldir,outdir,"fitBRT",whichfiles[which(filesbrt==i)],".txt",sep=""),col.names=FALSE,row.names=F,quote=FALSE) #must have no headers, no row names, no quotes
	#write.table(sdxyz,"sdBRT.txt",col.names=FALSE,row.names=F,quote=FALSE)


	#calc.deviance(obs=Anguilla_test$Angaus_obs, pred=preds, calc.mean=TRUE)
	#predict.gam(bwhaleGAMM$gam,get("factors"),se=TRUE, type="response")
	names(wwbrtvector) = sub((i),"",names(wwbrtvector))
	names(wwbrtAUC) = sub((i),"",names(wwbrtAUC))

	#pdf(paste(path,"/PredictCCSv3/bwhaleBRT_lr005_AUC_",which(filesbrt==i),".pdf",sep=''))
	#print(e)
	#dev.off()

	sink(file = paste(wd,modeldir,outdir,"bwhaleBRT_lr005_AUC_",whichfiles[which(filesbrt==i)],".txt",sep=''), type = "output")
	print(which(filesbrt==i))
	print(e)
	sink()

	pdf(paste(wd,modeldir,outdir,"bwhaleBRT_lr005_AUCplot_",whichfiles[which(filesbrt==i)],".pdf",sep=''))
	plot(e, 'ROC')
	dev.off()

	#predict = read.csv("WhaleWatchPredictionsCCS.csv")
	#predict = read.csv("WhaleWatchPredictionsCCS-BRT.csv")

	fitraster<-predict[,c("lat","lon","fit")]
	coordinates(fitraster) <- ~ lon + lat
	gridded(fitraster) <- TRUE
	rasterDF2 <- raster(fitraster)
	#quartz()
	image(rasterDF2)
	coltheme<-PuOrTheme(rev(brewer.pal(9,"Spectral")))
  plot(rasterDF2,legend=T,col=coltheme$regions$col)
  
  draster<-predict[,c("lat","lon","density")]
  coordinates(draster) <- ~ lon + lat
  gridded(draster) <- TRUE
  rasterDF3 <- raster(draster)
  rm(draster,fitraster)

	#pdf(paste(getwd(),"bwhale_FIT_GAM_122009.pdf",sep='/'))
	pdf(paste(wd,modeldir,outdir,"bwhale_FIT_BRT_",myear,"_",whichfiles[which(filesbrt==i)],".pdf",sep=''))
	extent(rasterDF2)@xmin<-extent(rasterDF2)@xmin-360
	extent(rasterDF2)@xmax<-extent(rasterDF2)@xmax-360
	plot(rasterDF2,legend=T,col=coltheme$regions$col,zlim=c(0,1))
	plot(wrld_simpl, add = T, col="white",border=NA)	
#	plot(USA, add = T, col="white",lwd=0.1,border=NA)	
#	plot(CAN, add = T, col="white",lwd=0.1,border=NA)	
#	plot(MEX, add = T, col="white",lwd=0.1,border=NA)	
	dev.off()

  pdf(paste(wd,modeldir,outdir,"bwhale_dens_BRT_",myear,"_",whichfiles[which(filesbrt==i)],".pdf",sep=''))
  extent(rasterDF3)@xmin<-extent(rasterDF3)@xmin-360
  extent(rasterDF3)@xmax<-extent(rasterDF3)@xmax-360
  plot(rasterDF3,legend=T,col=coltheme$regions$col,zlim=c(0,1.75))
  plot(wrld_simpl, add = T, col="white",border=NA)  
  dev.off()

}
}










###########SHIPPING
library(rgdal)
temp<-readOGR("/Users/elliotthazen/Dropbox/Documents/R/Blue_whales/Data/ship",layer="ship")
temp.ncol<-round((extent(temp)@ymax-extent(temp)@ymin)/.25)
temp.nrow<-round((extent(temp)@xmax-extent(temp)@xmin)/.25)
temp.ncol<-250
temp.nrow<-500

temp.ras<-raster(ncol=temp.ncol, nrow=temp.nrow)
extent(temp.ras)<-extent(temp)
rp <- rasterize(temp, temp.ras, 'classCode')
m <- c(0,0.1,0,0.1, 1.1, 25, 1.1, 2.1, 50,  2.1, 3.1, 75,  3.1, 4.1, 100,4.1,5.1,259,5.1,6.1,500,6.1,7.1,750,7.1,8.1,1500,8.1,9.1,5000)
rclmat <- matrix(m, ncol=3, byrow=TRUE)
rc <- reclassify(rp, rclmat)
rp.norm<-(rp-1)/8
rp.crop<-crop(rp.norm,extent(rasterDF2))
rp.resamp<-resample(rp.norm,(rasterDF2))
whalerisk<-overlay(rasterDF2,rp.resamp,fun=function(x,y){return(x*y)})
save(rp.norm,rp.crop,rp.resamp,file="shippingraster.RData")



#gbm = data.frame(wwbrtvector)
#fitbrt =gbm[ ,!substr(colnames(gbm),1,1)=="s"]
#sefit =gam[ ,substr(colnames(gam),1,1)=="s"]
#fitmean = rowMeans(fitbrt,na.rm=T)
#sd = apply(fitbrt,1,sd)
#predict = cbind(factors,fitmean,sd)
#predict$percent = predict$fitmean*100

#Write a .csv file with all data if desired for comparisons
#write.csv(predict,"WhaleWatchPredictionsCCS-BRT.csv")

#Create text files for GMT  
#fitxyz = data.frame(predict[2],predict[1],predict[fitcol])
#sdxyz = data.frame(predict[2],predict[1],predict[sdcol])
#write.table(fitxyz,"fitBRT.txt",col.names=FALSE,row.names=F,quote=FALSE) #must have no headers, no row names, no quotes
#write.table(sdxyz,"sdBRT.txt",col.names=FALSE,row.names=F,quote=FALSE)

##### PLOT CODE

#setwd('~/Dropbox/Documents/R/Blue_whales/Data/PredictCCSv3')
#predict = read.csv("WhaleWatchPredictionsCCS.csv")
#predict = read.csv("WhaleWatchPredictionsCCS-BRT.csv")

#fitraster<-predict[,c("lat","lon","fitmean")]
#coordinates(fitraster) <- ~ lon + lat
#gridded(fitraster) <- TRUE
#rasterDF2 <- raster(fitraster)
#quartz()
#image(rasterDF2)
#plot(rasterDF2,legend=T,col=heat.colors(100))

#pdf(paste(getwd(),"bwhale_FIT_GAM_122009.pdf",sep='/'))
#pdf(paste(getwd(),"bwhale_FIT_BRT_122009.pdf",sep='/'))
#plot(rasterDF2,legend=T,col=heat.colors(100))
#dev.off()

#predict = read.csv("WhaleWatchPredictionsCCS.csv")
#predict = read.csv("WhaleWatchPredictionsCCS-BRT.csv")

#fitraster<-predict[,c("lat","lon","fitmean")]
#coordinates(fitraster) <- ~ lon + lat
#gridded(fitraster) <- TRUE
#rasterDF2 <- raster(fitraster)
#quartz()
#image(rasterDF2)
#plot(rasterDF2,legend=T,col=heat.colors(100))

#pdf(paste(getwd(),"bwhale_FIT_GAM_122009.pdf",sep='/'))
#pdf(paste(getwd(),"bwhale_FIT_BRT_122009.pdf",sep='/'))
#plot(rasterDF2,legend=T,col=heat.colors(100))
#dev.off()


