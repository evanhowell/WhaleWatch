#
#  Get_Seg_Data_ERDDAP
#
#  Script to obtain ERDDAP variable data for CCE line-transect segments. 
#  Reads ncdf files and processes CCE data to create segment files with RS variables.
#
#
#  Written by Elizabeth Becker 10/10/2014 based on nc.data indexing code by Karin Forney
#  Last edits:  10/14/2014 to add SD time code
#
#######################################################################################

# Clear work space  Reminder: to clear console in R ctrl(L)
  rm(list=ls())

# ---------------------------------------------------------------------------
#
  is.installed <- function(x){
    is.element(x, installed.packages()[,1])
  } 
# ---------------------------------------------------------------------------
#
# Check for and if needed, install required packages
#
  if (!is.installed("ncdf4")){
    install.packages("ncdf4")
  }
 library(ncdf4)
# ---------------------------------------------------------------------------
# Set path for nc/data files

  nc.path <- 'C:/Users/EABECKER/Documents/R_xtracto/ERDDAP_Data/'
 
# Read in segment data file and assign variable names 

#  CCEsegs <- read.csv("TEST1993.csv",header=TRUE) # TEST file for August 27-31, 1991
  
  infile <- "C:/Users/EABECKER/Documents/HabModels_CCE_2013/Datasets/EAB_CCE/Segment_Building_Data/CCE_SURVEYS/CCE_1991_2009Segs.csv"
  CCEsegs <- read.table(infile,sep=",",header=T)
   
  seglat <- CCEsegs$mlat
  seglon <- CCEsegs$mlon + 360  # The CCE segment files need to be converted!  Change as needed for other datasets.
  segyear <- CCEsegs$year
  segmonth <- CCEsegs$month
  segday <- CCEsegs$day

# get time vector as 'YYYY-MM-DD'
  date <- paste(segyear,segmonth,segday,sep='-')
  segtime <- strptime(date, "%Y-%m-%d")

# Create output dataset and dd columns for new SSTre variables   
  seg.data  <- CCEsegs
  seg.data$SSTre.mean   <- NA
  seg.data$SSTre.SDtime <- NA
  seg.data$SSTre.SDspace <- NA

 
  t1<-Sys.time()
  print(t1)

 startyr <- 1987  # Set bogus year to initiate variable

  for(y in 1:nrow(CCEsegs)) {               # Loop through the segment file 

    yr <- segyear[y] 
    SegDay <- segtime[y]    
                
#  For each year, open the appropriate nc file and process data as needed  

#  REYNOLDS SST -----------------------------------------------------------------------
#   SST: Reynolds Optimal Interpolation Final+Prelim (AVHRR only & in situ)
#   Daily, 25km - get value for each segment midpoint ls()
#
      
         if(yr != startyr){

            datafile <- paste(nc.path,'CCE_sst_ncdcOisst2Agg_',yr,'.nc',sep="")
            varname <- 'sst'
            nc.data <- nc_open(datafile)
  
            lat <- ncvar_get(nc.data,'latitude')
            lon <- ncvar_get(nc.data,'longitude')
            tim <- ncvar_get(nc.data,'time')
            day <- substr(as.POSIXlt(tim,origin='1970-01-01',tz= "GMT"),1,10)
            data.var <-  ncvar_get(nc.data,varname)
            nrows <- length(lon)
            ncols <- length(lat)
            startyr <- yr  
        }
#
# Check to make sure segment day is is in file

      index1 <- which(day==SegDay)
      if (length(index1)==0) print('Not OK! Missing dates in nc file.')
#  
# Get value for segment mid-point ("mean") and calculate daily SST.SDspace for
# up to 9 pixels around each segment midpoint.

      SSTre.mean <- matrix(data.var[,,index1], nrows, ncols, dimnames=list(lon,lat))

      SSTre.SDspace <- SSTre.mean
      SSTre.SDspace[,] <- NA

      for (r in 1:nrows){
        for (c in 1:ncols) {    
          row1 <- max(r-1,1)
          row2 <- min(r+1,nrows)
          col1 <- max(c-1,1)
          col2 <- min(c+1,ncols)
          SSTre.SDspace[r,c] <- sd(SSTre.mean[c(row1:row2),c(col1:col2)], na.rm=TRUE)
        }
      }

# Calculate "SD time" of SST for 'numdays', beginning on SegDay 
# Note that sighting is on last day of range.
#  
# Find starting date in file; check to make sure it is in file
#
      numdays <- 8
      index2 <- index1 - numdays+1
      if (length(index2)==0) print('Not OK! Missing dates in nc file.')
      days <-array(data.var[,,index2:index1],dim=c(nrows,ncols,numdays))
      SSTre.SDtime <- matrix(apply(days, c(1,2), sd,   na.rm=TRUE), 
                             nrows, ncols, dimnames=list(lon,lat))

#
# Find data closest to each segment midpoint and add to data.frame
#
        lat.indx<-which(abs(lat-seglat[y])==min(abs(lat-seglat[y])))
        lon.indx<-which(abs(lon-seglon[y])==min(abs(lon-seglon[y])))
        nearest.cells <- length(lat.indx) * length(lon.indx)
        num.vals <- which(!is.na(SSTre.mean[lon.indx,lat.indx]))
        if (length(num.vals)>0) {
          if ((length(num.vals)==nearest.cells) | (nearest.cells == 1)) {
            seg.data$SSTre.mean[y]    <- SSTre.mean[lon.indx[1],lat.indx[1]] 
            seg.data$SSTre.SDtime[y]  <- SSTre.SDtime[lon.indx[1],lat.indx[1]] 
            seg.data$SSTre.SDspace[y] <- SSTre.SDspace[lon.indx[1],lat.indx[1]] 
          } else {
            seg.data$SSTre.mean[y]    <- SSTre.mean[lon.indx,lat.indx][num.vals[1]]
            seg.data$SSTre.SDtime[y]  <- SSTre.SDspace[lon.indx,lat.indx][num.vals[1]]
            seg.data$SSTre.SDspace[y] <- SSTre.SDtime[lon.indx,lat.indx][num.vals[1]]
          }
        }    # end if ((length(num.vals)==nearest.cells) | (nearest.cells == 1))

    }        # end  for(y in 1:nrow(CCEsegs))   

  nc_close(nc.data)

# Print data to csv

    write.table(seg.data, file = "segment_data.csv", sep = ",", col.names = TRUE, row.names = FALSE) 
 

