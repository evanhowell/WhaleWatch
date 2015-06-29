# xtracto<-function(xpos, ypos, tpos, dtype, xlen, ylen,quiet=TRUE){
#
# Created by Cindy Bessey, Dec.10, 2007
# Purpose: To translate Dave Foley's Xtractomatic Matlab code to R
# This code will be used to extract requested data via the web using Bobdap
# This code will follow Dave Foley's code as closely as possible.
#
# Revisions:
#
# 20 Mar 2009    - fixed bug in which program grabbed data for all permutations of
# DGF              space and time points.  Now treats xpos, ypos and tpos as
#                  basis vectors.
#
#                - corrected program so it returns both the track point time
#                  and the centered time of the satellite pass
#
#                - added ability to read actual satellite data set IDs
#                  regardless of lower/upper case characters
#
#                - set up program so it automatically loads ncdf library,
#                  if available.
#
# 7 Feb 2010     - added median and median absolute deviation to provide more robust statistics
# DGF              these are recommended for data sets such as chlorophyll, which exhibit
#                  non-normal distributions in nature.
#
# 10 Aug 2010 DGF - handled case of constant or specified search box boundaries
#
#
# 2 June 2011 DGF - adjusted script to handle changes in server configuration
#
# 1 Sept 2013 RM - adjusted script to work with ERDDAP instead of BDAP
#
# 8 August 2014 ELH - added functions to reduce if then loops
#                   - grab dataset info in xtracto call and pass to xtractomatic code
#
                    ########## CODE ############## 
                    #for (n in 1:numdatasets){
                    #  dtype<-as.numeric(as.integer(datasets[n]))
                    #  datasetname<-XtractTableDF$datasetname[dtype]
                    #  varname<-XtractTableDF$varname[dtype]
                    #  assign(paste("datasetlist",n,sep=''),getallinfo(datasetname))
                    #  dates<-as.POSIXct(get(paste("datasetlist",n,sep=''))[[1]],origin='1970-01-01',tz= "GMT")
                    #  datasetstartdates[n]<-min(dates)
                    #  datasetenddates[n]<-max(dates)
                    #}
                    #  rm(dates)

                    #datasetstartdates<-as.POSIXct(as.numeric(datasetstartdates),origin='1970-01-01',tz= "GMT")
                    #datasetenddates<-as.POSIXct(as.numeric(datasetenddates),origin='1970-01-01',tz= "GMT")
#                   ########## END CODE ##############
#                   - added Xtract lookup table as function XtractTable()
#                     In xtract call: 
#                    ########## CODE ##############  XtractTableDF<-XtractTable()              

#                   - try 60 times to get data from ERDDAP in geturl() function
#
#                   - Added Xtracto_3d to xtractomatic code here
#
#                   - Added Xtractogon to xtractomatic code
#                       # polyfile = path of xy vector file (comma delimited lon lat) that defines
#                          the polygon.  If it is not closed, the program will do this
#                          automatically.
#                       ########## CODE ##############  polyfile="/home/user/polyfile.csv" in Xtracto_3D call
#
#
# 23 September 2014 ELH & Dana Wingfield - wind codes updates such that stress does not have altitude for QA or AS.
#
#
# INPUTS:
# xpos = longitude vector(in decimal degrees East, either 0-360 or -180 to 180)
# ypos = latitude vector(in decimal degrees N; -90 to 90)
# tpos = time vector(must be specified as 'YYYY-MM-DD'; quotes required)
# dtype = data ID Code (data types listed below)
# xlen = search box length in longitude (decimal degrees)
# ylen = search box in latitude (decimal degrees)

# OUTPUTS:
# column 1 = mean of data within search radius
# column 2 = standard deviation of data within search radius
# column 3 = number of points found within search radius
# column 4 = time of returned value
# column 5 = min longitude of call (decimal degrees)
# column 6 = max longitude of call (decimal degrees)
# column 7 = min latitude of call (decimal degrees)
# column 8 = max latitude of call (decimal degrees)
# column 9 = requested time in tag
# column 10 = median of data within search radius
# column 11 = median absolute deviation of data within search radius
#
# Sample Calls:
# to extract Seawifs 8-day Primary Productivity in 22 km x 22km boxes
#   surrounding a point(i.e. 230E, 40N, 2006-01-01).
########### CODE ##############  extract<-xtracto(xpos=230, ypos=40, tpos='2006-01-15', dtype='41', xlen=.5, ylen=.5)
#
#  to extract pathfinder SST 8-day mean data in 11km x 11km boxes
########### CODE ##############   extract<-xtracto(xpos=c(230,235), ypos=c(40,45), tpos=c('2006-01-15','2006-01-20'), dtype='18', xlen=.5, ylen=.5)
#
# similarly, the calls:
########### CODE ##############   extract<-xtracto(xpos=230, ypos=40, tpos='2006-01-15', dtype='sst', xlen=.5, ylen=.5)  and
########### CODE ##############   extract<-xtracto(xpos=230, ypos=40, tpos='2006-01-15', dtype='phssta8day', xlen=.5, ylen=.5)
#               would give the same result.
#
# #
# see the following link to get data codes and full data set information
# http://coastwatch.pfel.noaa.gov/coastwatch/CWBrowserWW360.jsp?get=griddata
#
#  This is version 3.2.8  Released Sep 2014.  DGF/RM/ELH
#
# 

buildurl<-function(urlbase="http://coastwatch.pfeg.noaa.gov/erddap/griddap/",dtype=as.character(dtloc),datasetname,varname,requesttime,xmin,xmax,ymin,ymax,hasAltitude){

#   myURL=paste(urlbase,datasetname,'.nc?',varname,'[(',time1,'):1:(',time2,')]',
#       '[(10):1:(10)]',
#       '[(',lat1,'):1:(',lat2,')]',
#       '[(',lon1,'):1:(',lon2,')]',sep="")

if (length(requesttime)==1) {
  time1<-requesttime
  time2<-requesttime
} else {
  time1<-requesttime[1]
  time2<-requesttime[2]
}
     iswind<-(length(grep("QA",datasetname))>0 | length(grep("QS",datasetname))>0)
     if (iswind) iswind<-!(length(grep("stress",datasetname))>0)
     #iswind<-(dtype == '55' | dtype == '56'| dtype == '57' | (dtype > 73 & dtype < 81) | dtype == '120' | dtype == '121' | (dtype > 127 & dtype < 132))
     if (iswind){
        myURL=paste(urlbase,datasetname,'.nc?',varname,'[(',time1,'):1:(',time2,')]',
                      '[(10):1:(10)]',
                      '[(',ymin,'):1:(',ymax,')]',
                      '[(',xmin,'):1:(',xmax,')]',sep="")
      } else if (dtype == "180" | dtype == "360") {
      
        myURL = paste(urlbase,datasetname,'.nc?',varname,'[(',time1,'):1:(',time2,')]',
       '[(0.0):1:(0.0)]',
       '[(',ymin,'):1:(',ymax,')]',
       '[(',xmin,'):1:(',xmax,')]',sep="")

       } else if (isTRUE(hasAltitude)) {
      myURL=paste(urlbase,datasetname,'.nc?',varname,'[(',time1,'):1:(',time2,')]',
       '[(0.0):1:(0.0)]',
       '[(',ymin,'):1:(',ymax,')]',
       '[(',xmin,'):1:(',xmax,')]',sep="")

#      myURL=paste(urlbase,datasetname,'.nc?',varname,'[(',time1,'):1:(',time2,')]',
#       '[(',lat1,'):1:(',lat2,')]',
#       '[(',lon1,'):1:(',lon2,')]',sep="")
       } else {
      myURL=paste(urlbase,datasetname,'.nc?',varname,'[(',time1,'):1:(',time2,')]',
       '[(',ymin,'):1:(',ymax,')]',
       '[(',xmin,'):1:(',xmax,')]',sep="")
       }
#      myURL=paste(urlbase,datasetname,'.nc?',varname,'[(',time1,'):1:(',time2,')]',
#       '[(',lat1,'):1:(',lat2,')]',
#       '[(',lon1,'):1:(',lon2,')]',sep="")
       
      return(myURL)

}

make180 <- function(lon){

  isnot360<-min(lon)<0

if (!isnot360) {
    ind<-which(lon>180)
    lon[ind]<-lon[ind]-360
  }
  return(lon)
}

make360 <- function(lon){

  isnot360<-min(lon)<0
 
  if(isnot360){
    ind<-which(lon<0)
    lon[ind]<-lon[ind]+360
  }  
  
  return(lon)
}

is.installed <- function(x){
    is.element(x, installed.packages()[,1])
  } 

inbounds <- function(datarange,min1,max1){
  return(as.numeric(datarange)>as.numeric(min1)&(as.numeric(datarange))<as.numeric(max1))
}

# Load ncdf if available,  give error message if not.
#require(ncdf4)
#Determine if ncdf package is installed on users computer
#geturl(myURL,destfile='coords.nc',cacheOK=TRUE,mode="wb",quiet=quiet)

geturl<-function(myURL,destfile,cacheOK=TRUE,mode="wb",quiet=FALSE){
  numtries<-10
  tryn<-1

  while(tryn<numtries){
  #  if(quiet){
        try(downloadReturn<-download.file(myURL,destfile=destfile,cacheOK=TRUE,mode="wb",quiet=quiet))
  #    } else)
  #    {
  #     try(downloadReturn<-download.file(myURL,destfile=destfile,cacheOK=TRUE,mode="wb",quiet=FALSE))
  #    }
  if(!exists('downloadReturn')){downloadReturn<-1}
  if(downloadReturn != 0){
    print(myURL)
    print(paste("Tried ",tryn," of ",numtries," times",sep=''))
    print("There was an error in the url call.  See message on screen and URL called")
    tryn<-tryn+1    
    Sys.sleep(20)
  } else {
    tryn<-numtries
  }
 


} #while
      return(downloadReturn)
} #function

getallinfo<-function(datasetname, STANFORD=FALSE){
  urlbase<-'http://coastwatch.pfeg.noaa.gov/erddap/griddap/'
  urlbase1<-'http://coastwatch.pfeg.noaa.gov/erddap/info/'
  if (STANFORD) {
    urlbase<-'http://scad.stanford.edu/erddap/griddap/'
    urlbase1<-'http://scad.stanford.edu/erddap/info/' 
  }
  
  hasAltitude<-getfileinfo(datasetname, urlbase1=urlbase1)
  listvals<-getfilecoords(datasetname,hasAltitude, urlbase=urlbase)
  time<-listvals[[1]]
  latitude<-listvals[[2]]
  longitude<-listvals[[3]]
  altitude<-listvals[[4]]
  returnlist<-list(time,latitude,longitude,altitude,hasAltitude)
}

getfileinfo<-function(datasetname, urlbase1='http://coastwatch.pfeg.noaa.gov/erddap/info/',quiet=FALSE){

  myURL<-paste(urlbase1,datasetname,'/index.nc',sep="")
  
  #if(quiet){
  #  downloadReturn<-download.file(myURL,destfile='fileinfo.nc',cacheOK=TRUE,mode="wb",quiet=TRUE)
  #} else
  #{
  #  downloadReturn<-download.file(myURL,destfile='fileinfo.nc',cacheOK=TRUE,mode="wb",quiet=FALSE)
  #}
  #if(downloadReturn != 0){
  #  print(myURL)
  #  print(paste("Tried ",tryn," of ",numtries," times",sep=''))
  #  print("There was an error in the url call.  See message on screen and URL called")
  #  tryn<-tryn+1    
  #} else tryn<-30
  downloadReturn<-geturl(myURL,destfile='fileinfo.nc',cacheOK=TRUE,mode="wb",quiet=quiet)
  
  fileinfo<-nc_open('fileinfo.nc')
  rowTypes<-ncvar_get(fileinfo,'Row_Type')
  ndim<-length(which(rowTypes=='dimension'))
  if(ndim==3){
    hasAltitude<-FALSE
  } else {
    hasAltitude<-TRUE
  }
  nc_close(fileinfo)
  file.remove('fileinfo.nc')
  return(hasAltitude)
} #function getfileinfo

getfilecoords<-function(datasetname, hasAltitude, urlbase='http://coastwatch.pfeg.noaa.gov/erddap/griddap/',quiet=FALSE){

 #get dimension info
  myURL<-paste(urlbase,datasetname,'.nc?time[1:1:last],latitude[1:1:last],longitude[1:1:last]',sep="")
  downloadReturn<-geturl(myURL,destfile='coords.nc',cacheOK=TRUE,mode="wb",quiet=quiet)
  #if(quiet){
  #  downloadReturn<-download.file(myURL,destfile='coords.nc',cacheOK=TRUE,mode="wb",quiet=TRUE)
  #} else
  #{
  #  downloadReturn<-download.file(myURL,destfile='coords.nc',cacheOK=TRUE,mode="wb",quiet=FALSE)
  #}
  #if(downloadReturn != 0){
  #  print(myURL)
  #  print("There was an error in the url call.  See message on screen and URL called")
  #}
  coords<-nc_open('coords.nc')
  time<-coords$dim$time$vals
  latitude<-coords$dim$latitude$vals
  longitude<-coords$dim$longitude$vals
  if(hasAltitude){
    altitude<-coords$dim$altitude$vals
  } else {
    altitude<-rep(NA,n=length(time))
  }
  nc_close(coords)
  file.remove('coords.nc')

  returnlist = list(time,latitude,longitude,altitude,hasAltitude)

} #function getfileinfo

xtracto<-function(xpos, ypos, tpos, dtype, xlen, ylen, datasetlist=999, quiet=FALSE, STANFORD=FALSE){
  
  if (!is.installed("ncdf4")){
    install.packages("ncdf4")
  }
  packs<-search()
  xx<-rep(NA,length(packs))
  for(i in 1:length(packs)){if(packs[i]=="package:ncdf4"){xx[i]<-1}else{xx[i]<-0}}
  xx<-as.numeric(xx)
  if(sum(xx)<1){stop("This program requires the use of the'ncdf4' package. 
                     For Mac: Use Package Manager or Download from http://cran.r-project.org/web/packages/ncdf4/index.html
                     For Windows download from http://cirrus.ucsd.edu/~pierce/ncdf/")
  }
  

  #Load ncdf package
  library('ncdf4', character.only = TRUE)
  
  # make sure data type input is a string and not a number
  #if(!is.character(dtype)){
  #  dtype = as.character(dtype)
  #}
  datasetname<-NA
  varname<-NA
  # deal with xlen = constant v. vector
  if (length(xlen)==1) {
    xrad <- c(rep(xlen,length(xpos)))
    yrad <- c(rep(ylen,length(ypos)))
  } else {
    xrad <- xlen
    yrad <- ylen
  }
  
  hasAltitude <-1

### XtractTable can be run outside of xtracto to speed up the data lookup.  
if(!exists('XtractTableDF')) XtractTableDF<-XtractTable()

   if(!is.na(as.numeric(dtype))) {
#  if(is.character(dtype)) {           # DKB Debug change (jul 7 2014) -----
    dtloc<-as.numeric(dtype)
  } else {
    dtloc<-pmatch(dtype,XtractTableDF$dtstring)
  }

  if (is.na(dtloc)) {stop("no matching dataset found. Please visit http://coastwatch.pfeg.noaa.gov/erddap/griddap/")}


#   datasetname<-XtractTableDF$datasetname[dtloc]
#   varname<-XtractTableDF$varname[dtloc]
  datasetname<-XtractTableDF$datasetname[as.numeric(dtloc)]    # DKB Debug change (jul 7 2014) ----
  varname<-XtractTableDF$varname[as.numeric(dtloc)]



  hasAltitude=TRUE
  
  
  # default URL for NMFS/SWFSC/ERD  ERDDAP server
  
  urlbase<-'http://coastwatch.pfeg.noaa.gov/erddap/griddap/'
  urlbase1<-'http://coastwatch.pfeg.noaa.gov/erddap/info/'
  
if (STANFORD) {
  urlbase<-'http://scad.stanford.edu/erddap/griddap/'
  urlbase1<-'http://scad.stanford.edu/erddap/info/' 
}
 # urlbase<-'http://scad.stanford.edu/erddap/griddap/'
 # urlbase1<-'http://scad.stanford.edu/erddap/info/'

  # get dimension information
  
  #  ncdf4 does not support DAP calls on Windows
  # to have uniform code will not use this
  #myURL<-paste(urlbase,datasetname,sep="")
  #coords<-nc_open(myURL)
  #if(coords$ndim==3){
  #  hasAltitude<-FALSE 
  #  }
  #time<-coords$dim$time$vals
  #latitude<-coords$dim$latitude$vals
  #longitude<-coords$dim$longitude$vals
  #if(hasAltitude){
  #  altitude<-coords$dim$altitude$vals
  #  }
  #nc_close(coords)
  
  # alternative kludge to get dimension info
  #get file info
  

  if (is.numeric(datasetlist)) { 
    ## FILEINFO
    hasAltitude<-getfileinfo(datasetname,urlbase1)
    
    ## DIMENSIONS
    listvals<-getfilecoords(datasetname,hasAltitude,urlbase=urlbase)
    time<-listvals[[1]]
    latitude<-listvals[[2]]
    longitude<-listvals[[3]]
    altitude<-listvals[[4]]
#    hasAltitude<-listvals[[5]]
 } else {
    time<-datasetlist[[1]]
    latitude<-datasetlist[[2]]
    longitude<-datasetlist[[3]]
    altitude<-datasetlist[[4]]
    hasAltitude<-datasetlist[[5]]
  }
  ## REST OF CODE

  alldates<-unique(na.omit(time))
  sattime<-as.Date(as.POSIXlt(alldates,origin='1970-01-01',tz= "GMT"))
  #if (!isTRUE(datasetlist1[[4]])) hasAltitude<-FALSE
  #reconcile longitude grids
  # check for negative values in satellite grid
  isnot360<-(min(longitude)<0)
  
  #grid is not 360, xpos is
  if (isnot360) xpos1<-make180(xpos)
  
  #grid is 360, xpos isn't
  if(!isnot360) xpos1<-make360(xpos)
  
  
  # convert tpos to date class
  tpos<-as.Date(tpos)
  
  time2<-max(tpos)
  time1<-min(tpos)

  gooddata<-tpos>min(sattime)&tpos<max(sattime)

  # now that we are set up do bounds checking
  # check longitudes
  if((min(xpos1) < min(longitude))  || (max(xpos1) > max(longitude))) {
    print('xpos  (longtude) has elements out of range of the dataset')
    print('longtiude range in xpos')
    print(paste(min(xpos1),',', max(xpos)))
    print  ('longitude range in ERDDAP data')
    print(paste(min(longitude),',', max(longitude)))
#    stop('execution stopped')
  }
  # check latitudes
  if((min(ypos) < min(latitude))  || (max(ypos) > max(latitude))) {
    print('ypos  (latitude) has elements out of range of the dataset')
    print('latitiude range in ypos')
    print(paste(min(ypos),',', max(ypos)))
    print  ('latitude range in ERDDAP data')
    print(paste(min(latitude),',', max(latitude)))
#    stop('execution stopped')
  }
  # check time
  if((max(tpos) < min(sattime))  || (min(tpos) > max(sattime))) { #|| sum(gooddata)<0
    print('tpos  (time) has elements out of range of the dataset')
    print('time range in tpos')
    print(paste(min(tpos),',', max(tpos)))
    print  ('time range in ERDDAP data')
    timeposix<-as.POSIXct(time,origin='1970-01-01',tz= "GMT")
    print(paste(min(timeposix),',', max(timeposix)))
#    stop('execution stopped')
  } else {
    tpos<-tpos[gooddata]
    ypos<-ypos[gooddata]
    xpos<-xpos1[gooddata]
    xrad<-xrad[gooddata]
    yrad<-yrad[gooddata]
  }
  
  
  
  # loop on points
  out.dataframe<-as.data.frame(matrix(ncol=11,nrow=length(xpos)))
  dimnames(out.dataframe)[[2]]<-c('mean','stdev','n','satellite date','requested lon min','requested lon max',
                                  'requested lat min','requested lat max','requested date','median','mad')
  for(i in 1:length(xpos)){
    
    # define bounding box
    xmax<-xpos1[i]+xrad[i]/2
    xmin<-xpos1[i]-xrad[i]/2
    if(latitude[1]<latitude[2]){
      ymax<-ypos[i]+yrad[i]/2
      ymin<-ypos[i]-yrad[i]/2
    }else
    {
      ymin<-ypos[i]+yrad[i]/2
      ymax<-ypos[i]-yrad[i]/2
    }
    
    
       # if position is within bounds, get time/space indices for extraction
    if (tpos[i] <= max(sattime) & tpos[i] >= min(sattime)){    
      
      # find closest time of available data
      requesttime<-sattime[which.min(abs(sattime-tpos[i]))]
      # text string for data retrieval call

      myURL<-buildurl(urlbase=urlbase,dtype=as.character(dtloc),datasetname,varname,requesttime,xmin,xmax,ymin,ymax,hasAltitude)
      
      #Download the data from the website to the current R directory
      xmean<-xpos[i]
      ymean<-ypos[i]
      fileout<-"tempExtract.nc"
      downloadReturn<-geturl(myURL,destfile=fileout,cacheOK=TRUE,mode="wb",quiet=quiet)
  
      #if(quiet){
      #  downloadReturn<-download.file(myURL,destfile=fileout,cacheOK=TRUE,mode="wb",quiet=TRUE)
      #} else
      #{
      #  downloadReturn<-download.file(myURL,destfile=fileout,cacheOK=TRUE,mode="wb",quiet=FALSE)
      #}
      #if(downloadReturn != 0){
      #  print(myURL)
      #  print("There was an error in the url call.  See message on screen and URL called")
      #}
      
      datafileID<-nc_open(fileout)
      
      # Store in a dataframe
      paramdata<-as.vector(ncvar_get(datafileID))
      
      # close netcdf file
      nc_close(datafileID)
      
      out.dataframe[i,1]<-mean(paramdata,na.rm=T)
      out.dataframe[i,2]<-sd(paramdata,na.rm=T)
      out.dataframe[i,3]<-length(na.omit(paramdata))
      out.dataframe[i,4]<-as.character(requesttime)
      out.dataframe[i,5]<-xmin
      out.dataframe[i,6]<-xmax
      out.dataframe[i,7]<-ymin
      out.dataframe[i,8]<-ymax
      out.dataframe[i,9]<-as.character(tpos[i])
      out.dataframe[i,10]<-median(paramdata,na.rm=T)
      out.dataframe[i,11]<-mad(paramdata,na.rm=T)
      
      # clean thing up
      # remove temporary file
      file.remove(fileout)
      # remove variable from workspace
      remove('paramdata')
      
    } else {
      print("Error in Input time:")
      print(tpos[i])
      print("First and Last time in Dataset")
      print(sattime[1],sattime[length(sattime)])
    }  
    
    
  } 
  return(out.dataframe)
  }  

xtracto_3D<-function(xpos, ypos, tpos, dtype, polyfile=NA, datasetlist=999, quiet=FALSE, STANFORD=FALSE){
  if (!is.installed("ncdf4")){
    install.packages("ncdf4")
  }
  if (!is.installed("sp")){
    install.packages("sp")
  }
  library(sp)
  library(ncdf4)
  hasAltitude=TRUE
  # default URL for NMFS/SWFSC/ERD  ERDDAP server
  urlbase<-'http://coastwatch.pfeg.noaa.gov/erddap/griddap/'
  urlbase1<-'http://coastwatch.pfeg.noaa.gov/erddap/info/'
  
  if (STANFORD){
  urlbase<-'http://scad.stanford.edu/erddap/griddap/'
  urlbase1<-'http://scad.stanford.edu/erddap/info/'
  }

  if(!exists('XtractTableDF')) XtractTableDF<-XtractTable()

  if(!is.na(as.numeric(dtype))) {
#  if(is.character(dtype)) {           # DKB Debug change (jul 7 2014) -----
    dtloc<-as.numeric(dtype)
  } else {
    dtloc<-pmatch(dtype,XtractTableDF$dtstring)
  }

#    dtype<-as.numeric(dtype)


  if (is.na(dtloc)) {stop("no matching dataset found. Please visit http://coastwatch.pfeg.noaa.gov/erddap/griddap/")}
    datasetname<-XtractTableDF$datasetname[dtloc]
    varname<-XtractTableDF$varname[dtloc]

   if (is.numeric(datasetlist)) { 
      ## FILEINFO
      hasAltitude<-getfileinfo(datasetname,urlbase1)
      
      ## DIMENSIONS
      listvals<-getfilecoords(datasetname,hasAltitude,urlbase=urlbase)
      time<-listvals[[1]]
      latitude<-listvals[[2]]
      longitude<-listvals[[3]]
      altitude<-listvals[[4]]
    } else {
      time<-datasetlist[[1]]
      latitude<-datasetlist[[2]]
      longitude<-datasetlist[[3]]
      altitude<-datasetlist[[4]]
      hasAltitude<-datasetlist[[5]]
    }

    alldates<-unique(na.omit(time))
    sattime<-as.Date(as.POSIXlt(alldates,origin='1970-01-01',tz= "GMT"))
    
    #reconcile longitude grids
    # check for negative values in satellite grid
    isnot360<-(min(longitude)<0)
  
  #grid is not 360, xpos is
  if (isnot360) xpos1<-make180(xpos)
  
  #grid is 360, xpos isn't
  if(!isnot360) xpos1<-make360(xpos)
  
    # convert tpos to date class
    tpos<-as.Date(tpos)
    
      # define spatial bounding box
    lon2<-max(xpos1)
    lon1<-min(xpos1)

    if(latitude[1]<latitude[2]){
      lat2<-max(ypos)
      lat1<-min(ypos)
      } else {
      lat2<-min(ypos)
      lat1<-max(ypos)
      }

    if(!is.na(polyfile)){
      poly <- read.csv(polyfile)
      names(poly) <- c("x","y")

      isnot360<-(min(longitude)<0)
      polylon<-poly[,1]
      if (isnot360) polylon<-make180(polylon)
      #grid is 360, xpos isn't
      if(!isnot360) polylon<-make360(polylon)
      poly[,1]<-polylon

      lon1 = min(polylon,lon1)
      lon2 = max(polylon,lon2)
      if(latitude[1]<latitude[2]){
            lat2<-max(poly[,2],lat2)
            lat1<-min(poly[,2],lat1)
            } else {
            lat2<-min(poly[,2],lat2)
            lat1<-max(poly[,2],lat1)
      }

#      lat1 = min(poly[,2],lat1)
#      lat2 = max(poly[,2],lat2)

  # make sure polygon is closed; if not, close it.
      if ((poly[length(poly[,1]),1] != poly[1,1]) | (poly[length(poly[,2]),2] != poly[1,2])){
         poly <- rbind(poly, c(poly[1,1], poly[1,2]))
      }
    }

    # find time points within time bounds
    time2<-max(tpos)
    time1<-min(tpos)

    myURL<-buildurl(urlbase=urlbase,dtype=as.character(dtloc),datasetname,varname,requesttime=c(time1,time2),lon1,lon2,lat1,lat2,hasAltitude)
      

 #     if (dtype == '55' | dtype == '56'| dtype == '57' | (dtype > 73 & dtype < 81)){
      
 #     myURL=paste(urlbase,datasetname,'.nc?',varname,'[(',time1,'):1:(',time2,')]',
 #      '[(10):1:(10)]',
 #      '[(',lat1,'):1:(',lat2,')]',
 #      '[(',lon1,'):1:(',lon2,')]',sep="")
 #      } else if (dtype == "180" | dtype == "360") {
      
 #       myURL = paste(urlbase,datasetname,'.nc?',varname,'[(',time1,'):1:(',time2,')]',
 #      '[(0.0):1:(0.0)]',
 #      '[(',lat1,'):1:(',lat2,')]',
 #      '[(',lon1,'):1:(',lon2,')]',sep="")

 #      } else
 #      {
 #     myURL=paste(urlbase,datasetname,'.nc?',varname,'[(',time1,'):1:(',time2,')]',
 #      '[(0.0):1:(0.0)]',
 #      '[(',lat1,'):1:(',lat2,')]',
 #      '[(',lon1,'):1:(',lon2,')]',sep="")

#      myURL=paste(urlbase,datasetname,'.nc?',varname,'[(',time1,'):1:(',time2,')]',
#       '[(',lat1,'):1:(',lat2,')]',
#       '[(',lon1,'):1:(',lon2,')]',sep="")
#       }
 
       fileout<-"tmpExtract.nc"

#       myURL<-"http://coastwatch.pfeg.noaa.gov/erddap/griddap/erdPHsstamday.nc?sst[(2003-01-01):1:(2008-12-31)][(0.0):1:(0.0)][(22):1:(47)][(229):1:(250)]"

       downloadReturn<-geturl(myURL,destfile=fileout,cacheOK=TRUE,mode="wb",quiet=quiet)
    
    datafileID<-nc_open(fileout)
    datalongitude<-ncvar_get(datafileID, varid="longitude")
    datalatitude<-ncvar_get(datafileID, varid="latitude")
    datatime<-ncvar_get(datafileID, varid="time")
    datatime<-as.Date(as.POSIXlt(datatime,origin='1970-01-01',tz= "GMT"))

    #param<-ncvar_get(datafileID,varid=varname)
    param<-as.vector(ncvar_get(datafileID))

    nc_close(datafileID)
    out.array<-array(param,dim=c(length(datalongitude),length(datalatitude),length(datatime)))
    rownames<-paste(datalongitude,'E',sep="")
    colnames<-paste(datalatitude,'N',sep="")
#    rownames<-as.numeric(datalongitude)
#    colnames<-as.numeric(datalatitude)
    laynames<-as.character(datatime)
    dimnames(out.array)<-list(rownames,colnames,laynames)
   
    #clean things up
    file.remove(fileout)

    if(!is.na(polyfile)){
      extract<-out.array
 #Parse grid lats and longs
      x.vals <- matrix(rep(as.numeric(substr(dimnames(extract)[[1]], 1, nchar(dimnames(extract)[[1]])-1)),length(dimnames(extract)[[2]])), ncol = length(dimnames(extract)[[2]]))
      y.vals <- matrix(sort(rep(as.numeric(substr(dimnames(extract)[[2]], 1, nchar(dimnames(extract)[[2]])-1)),length(dimnames(extract)[[1]]))), ncol = length(dimnames(extract)[[1]]))
      if(latitude[2]<latitude[1]){
        y.vals<-t(apply((y.vals),1,rev)) 
        #y.vals<-arev(y.vals) 
      }

# FOR BATHY: deal with polygon crossing 180
      ew.sign <- sign(poly$x)
      if(length(unique(ew.sign)) > 1){
        poly$x[poly$x < 0] <- poly$x[poly$x < 0] + 360
        x.vals[x.vals < 0] <- x.vals[x.vals < 0] + 360
        print("Polygon data cross 180. Converted to E longitudes")
      }
    
# FOR BATHY: create new array masked by polygon
      in.poly <- matrix(point.in.polygon(x.vals, y.vals, poly$x, poly$y), ncol = length(dimnames(extract)[[1]]))
      in.poly[in.poly > 1] <- 1
      in.poly[in.poly == 0] <- NA
      dim(in.poly) <- dim(extract[,,1])
      extract.in.poly <- apply(extract, 3, "*", in.poly)
      dim(extract.in.poly) <- dim(extract)
      dimnames(extract.in.poly) <- dimnames(extract)
      out.array<-extract.in.poly
    }
    return(out.array)
}



XtractTable<-function(){ 

# numdatasets<-131
numdatasets<-  147 #135    # DKB updated 9/19/2014


dataname<-c(1:numdatasets)
vartype<-c(1:numdatasets)
dtypevals<-c(1:numdatasets)
dtstr<-c(1:numdatasets)
alts<--c(1:numdatasets)

for(dtype in 1:numdatasets){
datasetname<-NA
varname<-NA
dtstring<-NA
alt<-FALSE
 if(dtype=='1'|| dtype=='atsstnhday'){
    datasetname <- 'erdATsstnhday'
    varname <- 'sst'
    dtstring<-'atsstnhday'
  }
  #  AVHRR HRPT 1.4 km daytime SST data for West coast
  if(dtype=='2'|| dtype=='atssdhday'){
    datasetname <- 'erdATsstnhday'
    varname <- 'sst'
    dtstring<-'atssdhday'
   }
  
  #  AVHRR HRPT 1,4 km night and day SST 1-day composite
  if(dtype=='3'|| dtype=='atssta1day'|| dtype=='hrpt'|| dtype=='avhrr hrpt'){
    datasetname <- 'erdATssta1day'
    varname <- 'sst'
    dtstring<-'hrpt'
  }
  
  # AVHRR HRPT 1.4 km night and day SST 3-day composite
  if(dtype=='4'|| dtype=='atssta3dayy'){
    datasetname <- 'erdATssta3day'
    varname <- 'sst'
    dtstring<-'atssta3dayy'
  }
  
  #  AVHRR HRPT 1.4 km night and day SST 8-day composite
  if(dtype=='5'|| dtype=='atssta8day'){
    datasetname <- 'erdATssta8day'
    varname <- 'sst';
    dtstring<-'atssta8day'
  }
  
  #  AVHRR HRPT 1.4 km night and day SST 14-day composite
  if(dtype=='6'|| dtype=='atssta14dayy'){
    datasetname <- 'erdATssta14day'
    varname <- 'sst';
    dtstring<-'atssta14dayy'
  }
  
  #  AVHRR HRPT 1.4km night and day SST monthly composite
  if(dtype=='7'|| dtype=='atsstamday'){
    datasetname <- 'erdATsstamday'
    varname <-'sst'
    dtstring<-'atsstamday'
  }
  
  #  AVHRR GAC SST 11km 1-day composite
  if(dtype=='8'|| dtype=='agssta1day'){
    datasetname <- 'erdAGssta1day'
    varname <- 'sst'
    dtstring<-'agssta1day'
  }
  
  #  AVHRR GAC SST 11km 3-day composite
  if(dtype=='9'|| dtype=='agssta3day'){
    datasetname <- 'erdAGssta3day'
    varname <- 'sst'
    dtstring<-'agssta3day'
  }
  
  #  AVHRR GAC SST 11km 8-day composite
  if(dtype=='10'|| dtype=='agssta8da'){
    datasetname <- 'erdAGssta8day'
    varname <- 'sst'
    dtstring<-'agssta8da'
  }
  
  #  AVHRR GAC SST 11km 14-day composite
  if(dtype=='11'|| dtype=='agssta14day'){
    datasetname <- 'erdAGssta14day'
    varname <- 'sst'
    dtstring<-'agssta14day'
  }
  
  #  AVHRR GAC SST 11km monthly composite
  if(dtype=='12'|| dtype=='agsstamday'){
    datasetname <-'erdAGsstamday'
    varname <- 'sst'
    dtstring<-'agsstamday'
  }
  
  #  GOES SST 5.5 km 1-day composite
  if(dtype=='13'|| dtype=='gassta1day'|| dtype=='goes sst'|| dtype=='goes'|| dtype=='geostationary'){
    datasetname <- 'erdGAssta1day'
    varname <- 'sst'
    dtstring<-'gassta1day goes sst geostationary'
  }
  
  #  GOES SST 5.5 km 3-day composite
  if(dtype=='14'|| dtype=='gassta3day'){
    datasetname <- 'erdGAssta3day'
    varname <- 'sst'
   dtstring<-'gassta3day'
   }
  
  #  GOES SST 5.5 km 8-day composite
  if(dtype=='15'|| dtype=='gassta8day'){
    datasetname <- 'erdGAssta8day'
    varname <- 'sst'
   dtstring<-'gassta8day'
   }
  
  #  GOES SST 5.5 km 14-day composite
  if(dtype=='16'|| dtype=='gassta14day'){
    datasetname <- 'erdGAssta14day'
    varname <- 'sst'
   dtstring<-'gassta14day'
   }
  
  #  Pathfinder v5 5.5km SST 1-day composite
  if(dtype=='17'|| dtype=='phssta1day'){
    datasetname <- 'erdPHssta1day'
    varname <- 'sst'
    dtstring<-'phssta1day'
  }
  
  #  Pathfinder v5 5.5km SST 8-day composite
  if(dtype=='18'|| dtype=='phssta8day'|| dtype=='pathfinder'|| dtype=='sst'|| dtype=='avhrr'|| dtype=='sea surface temperature'){
    datasetname <- 'erdPHssta8day'
    varname <- 'sst'
    dtstring<-'phssta8day pathfinder sst avhrr sea surface temperature'
  }
  
  # Pathfinder v5 5.5km SST monthly composite
  if(dtype=='19'|| dtype=='phsstmday'|| dtype=='monthly sst'){
    datasetname <- 'erdPHsstamday'
    varname <-'sst'
    dtstring<-'phsstmday monthly'
  }
  #  MODIS Aqua 2.5 km chla 1-day composite
  if(dtype=='20'|| dtype=='mbchla1day'){
    datasetname <- 'erdMBchla1day'
    varname <- 'chlorophyll'
    dtstring<-'mbchla1day'
  }
  
  #  MODIS Aqua 2.5 km chla 8-day composite
  if(dtype=='21'|| dtype=='mbchla8day'|| dtype=='chla'|| dtype=='chlorophyll'|| dtype=='modis chl'|| dtype=='modis aqua'){
    datasetname <- 'erdMBchla8day'
    varname <- 'chlorophyll'
    dtstring<-'mbchla8day chlorophyll modis aqua'
  }
  
  #  MODIS Aqua 2.5 km chla 14-day composite
  if(dtype=='22'|| dtype=='mbchla14day'){
    datasetname <- 'erdMBchla14day'
    varname <- 'chlorophyll'
   dtstring<-'mbchla14day'
   }
  
  #  Quikscat NRT 25 km zonal wind, 1-day composite   - probably can be removed
  if(dtype=='24'|| dtype=='qnux101day'){
    datasetname <- 'erdQSwind1day'
    varname <- 'x_wind'
    dtstring<-'qnux101day'
   }
  
  
  #  Primary productivity, 8-day, seawifs chl.
  if(dtype=='41'|| dtype=='ppbfp18day'|| dtype=='primary productivity'|| dtype=='seawifs productivity'){
    datasetname <- 'erdPPbfp18day'
    varname <- 'productivity'
    dtstring<-'ppbfp18day primary seawifs productivity'
  }
  
  #  Primary productivity, monthly, seawifs chl
  if(dtype=='42'|| dtype=='Tppbfp1mday'|| dtype=='monthly productivity'){
    datasetname <- 'erdPPbfp1mday'
    varname <- 'productivity'
    dtstring<-'Tppbfp1mday monthly productivity'
  }
  
  #  GOES frontal index 14-day
  if(dtype=='43'|| dtype=='gatfnt14day'|| dtype=='GOES fronts'|| dtype=='frontal index'|| dtype=='frontal probability'){
    datasetname <- 'erdGAtfnt14day'
    varname <- 'front'
    dtstring<-'gatfnt14day GOES fronts frontal index probability'
  }
  
  #  GOES frontal index monthly
  if(dtype=='44'|| dtype=='gatfntmday'){
    datasetname <- 'erdGAtfntmday'
    varname <- 'front'
    dtstring<-'gatfntmday'
  }

  #  AVISO Geostrophic (0.25 degrees) Daily Composite
  if(dtype=='46'|| dtype=='tageo1day_u'){
    datasetname <- 'erdTAgeo1day'
    varname <- 'u_current'
    dtstring<-'tageo1day_u'
  }
  if(dtype=='47'|| dtype=='tageo1day_v'){
    datasetname <- 'erdTAgeo1day'
    varname <- 'v_current'
    dtstring<-'tageo1day_v'
  }

  #  AVISO SSH (0.25 degrees) Science Quality
  if(dtype=='48'|| dtype=='tasshd1day'){
    datasetname <- 'erdTAssh1day'
    varname <- 'ssh'
    dtstring<-'tasshd1day'
  }
  
  #  Dave Blended SST (modis+amsre+goes+avhrr)
  if(dtype=='49'|| dtype=='bassta5day'){
    datasetname <- 'erdBAssta5day'
    varname <- 'sst'
    dtstring<-'bassta5day'
  }
  
  #  MODIS/Aqua 0.05 degree chlorophyll (oc3)
  if(dtype=='50'|| dtype=='mhchla8day'){
    if (STANFORD) datasetname <- 'erdMHchla8day' else datasetname <- 'erdMH1chla8day' 
    varname <- 'chlorophyll'
    dtstring<-'mhchla8day erdMHchla8day erdMH1chla8day'
  }
  #  MODIS/Aqua 0.05 degree K490
  if(dtype=='51'|| dtype=='mhk4908day'){
    datasetname <- 'erdMGk4908day'
    varname <- 'k490'
    dtstring<-'mhk4908day'
  }
  #  MODIS/Aqua 0.05 degree SST
  if(dtype=='52'|| dtype=='mhsstd8day'){
    datasetname <- 'erdMHsstd8day'
    varname <- 'sst'
    dtstring<-'mhsstd8day'
  }
  #  MODIS/Aqua 0.05 degree Chlorophyll fluoresence line height
  if(dtype=='53'|| dtype=='mhcflh8day'){
    datasetname <- 'erdMHcflh8day'
    varname <- 'fluorescence'
    dtstring<-'mhcflh8day'
  }
  
  #  quikscat 3-day Science-quality Curl of wind stress
  if(dtype=='54'|| dtype=='qscurl3day'){
    datasetname <- 'erdQSstress3day'
    varname <- 'curl'
    dtstring<-'qscurl3day'
    alt<-FALSE
  }
  #  quikscat 3-day Science-quality wind modulus
  if(dtype=='55'|| dtype=='qsumod3day'){
    datasetname <- 'erdQSdivmod3day'
    varname <- 'mod'
    dtstring<-'qsumod3day'
    alt<-TRUE
  }
  # Quikscat 3-day Science-Quality zonal wind
  if(dtype=='56'|| dtype=='qsux103day'){
    datasetname <- 'erdQSwind3day'
    varname <- 'x_wind'
    dtstring<-'qsux103day'
    alt<-TRUE
  }
  #  Quikscat 3-day Science Quality meridional wind
  if(dtype=='57'|| dtype=='qsuy103day'){
    datasetname <- 'erdQSwind3day'
    varname <- 'y_wind'
    dtstring<-'qsuy103day'
    alt<-TRUE
  }
 
if(dtype=='58'||dtype=='qswekm8day'){
  dtstring<-'qswekm8day'
  datasetname<-'erdQSstress8day'
  varname='upwelling'
  alt<-FALSE
 }
if(dtype=='59'||dtype=='qswekmmday'){
  dtstring<-'qswekmmday'
  datasetname<-'erdQSstressmday'
  varname='upwelling'
  alts-FALSE
}
 if(dtype=='60'||dtype=='qswekm1day'){
  dtstring<-'qswekm1mday'
  datasetname<-'erdQSstress1day'
  varname='upwelling'
  alt<-FALSE
}
  #  Seawifs 0.1 degree chlrophyll, 8-day
  if(dtype=='62'|| dtype=='swchla8day'){
    datasetname <- 'erdSWchla8day'
    varname <- 'chlorophyll'
    dtstring<-'swchla8day'
  }
  #  Seawifs 0,1 degree chlorophyll, monthly
  if(dtype=='63'|| dtype=='swchlamday'){
    datasetname <- 'erdSWchlamday'
    varname <- 'chlorophyll'
    dtstring<-'swchlamday'
  }
  #  MODIS/Aqua NRT chlorophyll a from OSU DB Station
  if(dtype=='64'|| dtype=='mychlahday'){
    datasetname <- 'erdMYchlahday'
    varname <- 'chlorophyll'
    dtstring<-'mychlahday'
  }
  
  #  MODIS/Aqua NRT chlorophyll a , 1-day composite, from OSU DB Station
  if(dtype=='65'|| dtype=='mychla1day'){
    datasetname <- 'erdMYchla1day'
    varname <- 'chlorophyll'
    dtstring<-'mychla1day'
  }
  
  #  MODIS/Aqua NRT chlorophyll a , 3-day composite, from OSU DB Station
  if(dtype=='66'|| dtype=='mychla3day'){
    datasetname <- 'erdMYchla3day'
    varname <- 'chlorophyll'
    dtstring<-'mychla3day'
  }
  
  #  Seawifs HRPT monthly chl
  if(dtype=='68'|| dtype=='shchl2mday'){
    datasetname <- 'erdSWchlamday'
    varname <-'chlorophyll'
    dtstring<-'shchl2mday'
  }
  #  AVISO Geostrophic (0.25 degrees) Monthly Composite
  if(dtype=='70'|| dtype=='tageomday_u'){
    datasetname <- 'erdTAgeomday'
    varname <- 'u_current'
    dtstring<-'tageomday_u'
  }
  if(dtype=='71'|| dtype=='tageomday_v'){
    datasetname <- 'erdTAgeomday'
    varname <- 'v_current'
    dtstring<-'tageomday_v'
  }

  
  #  quikscat 1-day science quality zonal wind
  if(dtype=='74'|| dtype=='qsux101day'){
    datasetname <- 'erdQSwind1day'
    varname <- 'x_wind'
    dtstring<-'qsux101day'
      alt<-TRUE
  }
  #  quikscat 1-day science quality meridional wind
  if(dtype=='75'|| dtype=='qsuy101day'){
    datasetname <- 'erdQSwind1day'
    varname <- 'y_wind'
    dtstring<-'qsuy101day'
      alt<-TRUE
  }
  #  quikscat 1-day science quality wind modulus
  if(dtype=='76'|| dtype=='qsumod1day'){
    datasetname <- 'erdQSdivmod1day'
    varname <- 'mod'
    dtstring<-'qsumod1day'
      alt<-TRUE
  }
  #  quikscat 1-day science quality wind stress curl
  if(dtype=='77'|| dtype=='qscurl1day'){
    datasetname <- 'erdQSstress1day'
    varname <- 'curl'
    dtstring<-'qscurl1day'
      alt<-FALSE
  }
  #  quikscat 1-day science quality zonal wind stress
  if(dtype=='78'|| dtype=='qstaux1day'){
    datasetname <- 'erdQSstress1day'
    varname <- 'taux'
    dtstring<-'qstaux1day'
      alt<-FALSE
  }
  #  quikscat 1-day science quality meridional wind stress
  if(dtype=='79'|| dtype=='qstauy1day'){
    datasetname <- 'erdQSstress1day'
    varname <- 'tauy'
    dtstring<-'qstauy1day'
      alt<-FALSE
  }
  #  quikscat 1-day science quality wind stress modulus
  if(dtype=='80'|| dtype=='qstmod1day'){
    datasetname <- 'erdQSstress1day'
    varname <- 'modStress'
    dtstring<-'qstmod1day'
      alt<-FALSE
  }
  
  #  Reynolds daily OI SST - AVHRR  + in situ
  if(dtype=='89'|| dtype=='nassta1day'){
    datasetname <- 'ncdcOisst2Agg'
    varname<-'sst'
    dtstring<-'nassta1day'
  }
  
  #  Reynolds daily OI SST - AVHRR / AMSR-E + insitu
  if(dtype=='90'|| dtype=='n2ssta1day'){
    datasetname <- 'ncdcOisst2AmsrAgg'
    varname <- 'sst'
    dtstring<-'n2ssta1day'
  }
  
  #  Blendomatic 8-day SST
  if(dtype=='91'|| dtype=='bassta8day'){
    datasetname <- 'erdBAssta8day'
    varname <- 'sst'
    dtstring<-'bassta8day'
  }
  
  #VIIRSN Level-3 Mapped, Global, 4km, Chlorophyll a, Daily
  if(dtype=='92'|| dtype=='erdVHchla1day'){
    datasetname <- 'erdVHchla1day'
    varname <- 'chla'
    dtstring<-'erdVHchla1day'
  }
  #VIIRSN Level-3 Mapped, Global, 4km, Chlorophyll a, 8-Day
  if(dtype=='93'|| dtype=='erdVHchla8day'){
    datasetname <- 'erdVHchla8day'
    varname <- 'chla'
    dtstring<-'erdVHchla8day'
  }
  #VIIRSN Level-3 Mapped, Global, 4km, Chlorophyll a, Monthly
  if(dtype=='94'|| dtype=='erdVHchlamday'){
    datasetname <- 'erdVHchlamday'
    varname <-'chla'
    dtstring<-'erdVHchlamday'
  }
  #VIIRSN Level-3 Mapped, Global, 4km, Diffuse Attenuation Coefficient K490, Daily
  if(dtype=='95'|| dtype=='erdVHk4901day'){
    datasetname <- 'erdVHk4901day'
    varname <-'k490'
    dtstring<-'erdVHk4901day'
  }
  #VIIRSN Level-3 Mapped, Global, 4km, Diffuse Attenuation Coefficient K490, 8-Day
  if(dtype=='96'|| dtype=='erdVHk4908day'){
    datasetname <- 'erdVHk4908day'
    varname <-'k490'
    dtstring<-'erdVHk4908day'
  }
  #VIIRSN Level-3 Mapped, Global, 4km, Diffuse Attenuation Coefficient K490, Monthly
  if(dtype=='97'|| dtype=='erdVHk490mday'){
    datasetname <-'erdVHk490mday'
    varname <- 'k490'
    dtstring<-'erdVHk490mday'
  }
  #VIIRSN Level-3 Mapped, Global, 4km, Remote Sensing Reflectance at 671 nm, Daily
  if(dtype=='98'|| dtype=='erdVHr6711day'){
    datasetname <- 'erdVHr6711day'
    varname <- 'r671'
    dtstring<-'erdVHr6711day'
  }
  #VIIRSN Level-3 Mapped, Global, 4km, Remote Sensing Reflectance at 671 nm, 8-Day
  if(dtype=='99'|| dtype=='erdVHr6718day'){
    datasetname <- 'erdVHr6718day'
    varname <- 'r671'
    dtstring<-'erdVHr6718day'
  }
  #VIIRSN Level-3 Mapped, Global, 4km, Remote Sensing Reflectance at 671 nm, Monthly  
  if(dtype=='100'|| dtype=='erdVHr671mday'){
    datasetname <- 'erdVHr671mday'
    varname <- 'r671'
    dtstring<-'erdVHr671mday'
  }
  # Aquarius Sea Surface Salinity, Version 2, Global, 3-Month
  if(dtype=='101'|| dtype=='jplAquariusSSS3MonthV3'){
    datasetname <- 'jplAquariusSSS3Month'
    varname <-'sss'
    dtstring<-'jplAquariusSSS3Month'
  }
  #Aquarius Sea Surface Salinity, Version 2, Global, 7-Day
  if(dtype=='102'|| dtype=='jplAquariusSSS7DayV3'){
    datasetname <- 'jplAquariusSSS7Day'
    varname <- 'sss'
    dtstring<-'jplAquariusSSS7Day'
  }
  #Aquarius Sea Surface Salinity, Version 2, Global, Daily
  if(dtype=='103'|| dtype=='jplAquariusSSSDailyV3'){
    datasetname <- 'jplAquariusSSSDaily'
    varname <- 'sss'
    dtstring<-'jplAquariusSSSDaily'
  }
  #Aquarius Sea Surface Salinity, Version 2, Global, Monthly  
  if(dtype=='104'|| dtype=='jplAquariusSSSMonthlyV3'){
    datasetname <- 'jplAquariusSSSMonthly'
    varname <- 'sss'
    dtstring<-'jplAquariusSSSMonthly'
  }
  #GHRSST Global 1-km Sea Surface Temperature (G1SST), Global, 0.01 Degree, Daily  
  if(dtype=='105'|| dtype=='jplG1SST'){
    datasetname <- 'jplG1SST'
    varname <-'SST'
    dtstring<-'jplG1SST'
  }
  #GHRSST Level 4 AVHRR_OI Global Blended Sea Surface Temperature Analysis, Global, 0.25 Degree, Daily 
  if(dtype=='106'|| dtype=='jplL4AvhrrOIv1fv2'){
    datasetname = 'jplL4AvhrrOIv1fv2'
    varname = 'analysed_sst'
    dtstring<-'jplL4AvhrrOIv1fv2'
  }
  #SST, GHRSST Blended, MW-IR-OI, Science Quality, Global (1 Day Composite)
  if(dtype=='107'|| dtype=='erdG1ssta1day'){
    datasetname <- 'erdG1ssta1day'
    varname <- 'analyzed_sst'
    dtstring<-'erdG1ssta1day'
  }
  #SST, GHRSST Blended, MW-IR-OI-RT, Near Real Time, Global (1 Day Composite)
  if(dtype=='108'|| dtype=='erdGRssta1day'){
    datasetname <- 'erdGRssta1day'
    varname <-'analyzed_sst'
    dtstring<-'erdGRssta1day'
  }
  #Multi-scale Ultra-high Resolution (MUR) SST analysis, Global, 0.011 Degree, Daily
  if(dtype=='109'|| dtype=='plMURSST'){
    datasetname <- 'plMURSST'
    varname <- 'analyzed_sst'
    dtstring<-'plMURSST'
  }
  if(dtype=='110'|| dtype=='etopo360'){
    datasetname <- 'etopo360'
    varname <- 'bathymetry'
    dtstring<-'etopo360'
  }

# ---------------------#####------------------- #
  
  # BELOW ADDED BY SARA MAXWELL 6/5/13
  
  # ---------------------#####------------------- #
  
  # SST, POES AVHRR, LAC, West US, Day and Night (1 Day Composite) 
  if(dtype=='111'|| dtype=='erdATssta1day'){
    datasetname <- 'erdATssta1day'
    varname <- 'sst'
    dtstring<-'erdATssta1day'
  }
  # SST, POES AVHRR, LAC, West US, Day and Night (3 Day Composite) 
  if(dtype=='112'|| dtype=='erdATssta3day'){
    datasetname <- 'erdATssta3day'
    varname <- 'sst'
    dtstring<-'erdATssta3day'
  }
  # SST, POES AVHRR, LAC, West US, Day and Night (8 Day Composite) 
  if(dtype=='113'|| dtype=='erdATssta8day'){
    datasetname <- 'erdATssta8day'
    varname <- 'sst'
    dtstring<-'erdATssta8day'
  }
  # SST, Pathfinder Ver 5.0, Day and Night, Global, Science Quality (1 Day Composite)
  if(dtype=='114'|| dtype=='erdPHssta1day'){
    datasetname <- 'erdPHssta1day'
    varname <- 'sst'
    dtstring<-'erdPHssta1day'
  }
  # SST, Pathfinder Ver 5.0, Day and Night, Global, Science Quality (5 Day Composite)
  if(dtype=='115'|| dtype=='erdPHssta5day'){
    datasetname <- 'erdPHssta5day'
    varname <- 'sst'
    dtstring<-'erdPHssta1day'
  }
  # SST, Pathfinder Ver 5.0, Day and Night, Global, Science Quality (8 Day Composite)
  if(dtype=='116'|| dtype=='erdPHssta8day'){
    datasetname <- 'erdPHssta8day'
    varname <- 'sst'
    dtstring<-'erdPHssta1day'
  }
#   # SST, Daily Optimum Interpolation (OI), AVHRR Only, Version 2, Final+Preliminary
#   if(dtype=='117'|| dtype=='ncdcOisst2Agg'){  #ncdcOisst2AmsrAgg
#     datasetname <- 'ncdcOisst2Agg'
#     varname <- 'sst'
#     dtstring<-'ncdcOisst2Agg'
#   }
    # SST, Daily Optimum Interpolation (OI), AVHRR Only, Version 2, Final+Preliminary  - DKB MOD!! NAME DIFF
    if(dtype=='117'|| dtype=='ncdcOisst2AmsrAgg'){  #ncdcOisst2Agg
      datasetname <- 'ncdcOisst2AmsrAgg'
      varname <- 'sst'
      dtstring<-'ncdcOisst2AmsrAgg'
    }
  # Chlorophyll-a, Aqua MODIS, NPP, West US (1 Day Composite)
  if(dtype=='118'|| dtype=='erdMWchla1day'){
    datasetname <- 'erdMWchla1day'
    varname <- 'chlorophyll'
    dtstring<-'erdMWchla1day'
  }
  # Chlorophyll-a, Orbview-2 SeaWiFS, West US (1 Day Composite)
  if(dtype=='119'|| dtype=='erdSHchla1day'){
    datasetname <- 'erdSHchla1day'
    varname <- 'chlorophyll'
    dtstring<-'erdSHchla1day'
  }
  # Wind, QuikSCAT, Global, Science Quality (1 Day Composite)
  if(dtype=='120'|| dtype=='erdQSwind1day'){
    datasetname <- 'erdQSwind1day'
    varname <- 'x_wind'
    dtstring<-'erdQSwind1day'
      alt<-TRUE
  }
  # Wind, METOP ASCAT, Global, Near Real Time (1 Day Composite)
  if(dtype=='121'|| dtype=='erdQAwind1day'){
    datasetname <- 'erdQAwind1day'
    varname <- 'x_wind'
    dtstring<-'erdQAwind1day'
      alt<-TRUE
  }
  
  # Currents, Geostrophic, Aviso, Global (1 Day Composite) - eke = u^2+v^2
  if(dtype=='122'|| dtype=='erdTAgeo1day'){
    datasetname <- 'erdTAgeo1day'
    varname <- 'u_current'
    dtstring<-'erdTAgeo1day'
  }
  # Sea Surface Height, Aviso, Global, Science Quality (1 Day Composite) - mean and sd
  if(dtype=='123'|| dtype=='erdTAssh1day'){
    datasetname <- 'erdTAssh1day'
    varname <- 'ssh'
    dtstring<-'erdTAssh1day'
  }
  # Chlorophyll-a, Aqua MODIS, NPP, West US (3 Day Composite)
  if(dtype=='124'|| dtype=='erdMWchla3day'){
    datasetname <- 'erdMWchla3day'
    varname <- 'chlorophyll'
    dtstring<-'erdMWchla3day'
  }  
  # Chlorophyll-a, Aqua MODIS, NPP, West US (8 Day Composite)
  if(dtype=='125'|| dtype=='erdMWchla8day'){
    datasetname <- 'erdMWchla8day'
    varname <- 'chlorophyll'
    dtstring<-'erdMWchla8day'
  } 
  # Chlorophyll-a, Orbview-2 SeaWiFS, West US (3 Day Composite)
  if(dtype=='126'|| dtype=='erdSHchla3day'){
    datasetname <- 'erdSHchla3day'
    varname <- 'chlorophyll'
    dtstring<-'erdSHchla3day'
  }
  # Chlorophyll-a, Orbview-2 SeaWiFS, West US (8 Day Composite)
  if(dtype=='127'|| dtype=='erdSHchla8day'){
    datasetname <- 'erdSHchla8day'
    varname <- 'chlorophyll'
    dtstring<-'erdSHchla8day'
  } 
  # Wind, QuikSCAT, Global, Science Quality (3 Day Composite)
  if(dtype=='128'|| dtype=='erdQSwind3day'){
    datasetname <- 'erdQSwind3day'
    varname <- 'x_wind'
    dtstring<-'erdQSwind3day'
      alt<-TRUE
  }
  # Wind, QuikSCAT, Global, Science Quality (8 Day Composite)
  if(dtype=='129'|| dtype=='erdQSwind8day'){
    datasetname <- 'erdQSwind8day'
    varname <- 'x_wind'
    dtstring<-'erdQSwind8day'
      alt<-TRUE
  } 
  # Wind, METOP ASCAT, Global, Near Real Time (3 Day Composite)
  if(dtype=='130'|| dtype=='erdQAwind3day'){
    datasetname <- 'erdQAwind3day'
    varname <- 'x_wind'
    dtstring<-'erdQAwind3day'
      alt<-TRUE
  } 
  # Wind, METOP ASCAT, Global, Near Real Time (8 Day Composite)
  if(dtype=='131'|| dtype=='erdQAwind8day'){
    datasetname <- 'erdQAwind8day'
    varname <- 'x_wind'
    dtstring<-'erdQAwind8day'
      alt<-TRUE
  }
## ---------------------#####------------------- #

# BELOW ADDED BY DANA K BRISCOE 9/15/2014 (and added to v3.2.8 on 9/26/2014)

# ---------------------#####------------------- #

#  MODIS/Aqua 0.05 degree chlorophyll (oc3)    #
if(dtype=='132'|| dtype=='mbchla3day'){
  datasetname <- 'erdMBchla3day'
  varname <- 'chlorophyll'
  dtstring<-'erdMBchla3day'
}
# Wind, METOP ASCAT, Global, Near Real Time (3 Day Composite)
if(dtype=='133'|| dtype=='erdQAwind3day'){
  datasetname <- 'erdQAwind3day'
  varname <- 'y_wind'
  dtstring <- 'erdQAwind3day'
      alt<-TRUE
  } 
# Wind, METOP ASCAT, Global, Near Real Time (1 Day Composite)
if(dtype=='134'|| dtype=='erdQAwind1day'){
  datasetname <- 'erdQAwind1day'
  varname <- 'y_wind'
  dtstring <- 'erdQAwind1day'
      alt<-TRUE
  } 
# Currents, Geostrophic, Aviso, Global (1 Day Composite) - eke = u^2+v^2
if(dtype=='135'|| dtype=='erdTAgeo1day'){
  datasetname <- 'erdTAgeo1day'
  varname <- 'v_current'
  dtstring <- 'erdTAgeo1day'
}
# Seawifs 0.1 degree chlrophyll, 1-day - Chlorophyll-a, Orbview-2 SeaWiFS, Global (1 Day Composite)
if(dtype=='136'|| dtype=='swchla1day'){
  datasetname <- 'erdSWchla1day'
  varname <- 'chlorophyll'
  dtstring <- 'swchla1day'
}
# Wind, METOP ASCAT, Global, Near Real Time (3 Day Composite)
if(dtype=='137'|| dtype=='erdQAstress3day'){
  datasetname <- 'erdQAstress3day'
#   varname <- 'taux'
  varname <- 'curl'                                          
  dtstring <- 'erdQAstress3day'
  #       alt<-TRUE                                        
  alt<-FALSE
} 
# Wind, METOP ASCAT, Global, Near Real Time (1 Day Composite)
if(dtype=='138'|| dtype=='erdQAstress1day'){
  datasetname <- 'erdQAstress1day'
  varname <- 'taux'
  dtstring <- 'erdQAstress1day'
  alt<-FALSE
} 

# Stanford ONLY, DT merged Global Ocean Gridded Geostrophic Velocities SSALTO/Duacs L4 product U (1 Day Composite)
if(dtype=='145'|| dtype=='aviso_Idelayed-time_msla_uv'){
  datasetname <- 'aviso_delayed-time_msla_uv'
  varname <- 'u'
  dtstring <- 'aviso_delayed-time_msla_uv'
  alt<-FALSE
} 
# Stanford ONLY, DT merged Global Ocean Gridded Geostrophic Velocities SSALTO/Duacs L4 product V (1 Day Composite)
if(dtype=='146'|| dtype=='aviso_delayed_time_msla_uv'){
  datasetname <- 'aviso_delayed_time_msla_uv'
  varname <- 'v'
  dtstring <- 'aviso_delayed_time_msla_uv'
  alt<-FALSE
} 
if(dtype=='147'|| dtype=='aviso_delayed_time_msla_h'){
  datasetname <- 'aviso_delayed_time_msla_h'
  varname <- 'sla'
  dtstring <- 'aviso_delayed_time_msla_h'
  alt<-FALSE
} 

# end dkb added products --------------------------------------------------------------


  if (dtype =='180' || dtype=='etopo180'){
    datasetname= 'etopo180'
    varname='altitude'
    dtstring<-'etopo180'
  }
  if (dtype =='360' || dtype=='etopo360'){
    datasetname= 'etopo360'
    varname='altitude'
    dtstring<-'etopo360'
  }


  #if(is.na(datasetname)){
    #print('no matching dataset found')
  #}

 # dtypen<-as.numeric(dtype)
  dtypevals[dtype]<-dtype
  dataname[dtype]<-datasetname
  vartype[dtype]<-varname
  dtstr[dtype]<-dtstring
  alts[dtype]<-alt
}

XtractFiles<-as.data.frame(cbind(dtypevals,dataname,vartype,dtstr,alts))
names(XtractFiles)<-c('dtype','datasetname','varname','dtstring','iswind')

return(XtractFiles)
#write.csv(cbind(dtypevals,dataname,vartype),file="XtractoFiles.csv")


}
