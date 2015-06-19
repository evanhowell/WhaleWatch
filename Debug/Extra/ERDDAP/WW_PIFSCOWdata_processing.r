#--------------------------------------------------------------------------------------
# WW_PIFSCOWdata_processing.r -- Reads SSH NetCDF file for matching data
#
# Based on file CCE_ERDDAPdata_processin2g.r
#
# Originally written by Karin Forney and Elizabeth Becker, 9/24/2014
#
# Modified by Evan Howell, 1/14/2015
#
#
#
#--------------------------------------------------------------------------------------
# Clear workspace 
#
  rm(list=ls())

#-------------------------------SET UP FUNCTIONS---------------------------------------
# Function to check for libararies
#
 is.installed <- function(x){
    is.element(x, installed.packages()[,1])
  } 
#
# If needed install and load ncdf5 package
#
  if (!is.installed("ncdf4")){
    install.packages("ncdf4")
  }
  library(ncdf4)

  t1<-Sys.time()
  print(t1)

#
#  For each data type, open the appropriate nc file
#  and process data as needed  
#
#  AVISO SSH Science Quality  -----------------------------------------------------------------------
#
      nc.file <- 'http://oceanwatch.pifsc.noaa.gov/thredds/dodsC/altim_merged/weekly'

  	  varname <- 'ssh'
      nc.data <- nc_open(nc.file)
  
      lat <- ncvar_get(nc.data,'lat')
      lon <- ncvar_get(nc.data,'lon')
      tim <- ncvar_get(nc.data,'time')
      day <- substr(as.POSIXlt(tim*86400,origin='1800-01-01',tz= "GMT"),1,10) #convert day step to seconds
      data.var <-  ncvar_get(nc.data,varname)
      nrows <- length(lon)
      ncols <- length(lat)
#
# Find satellite date in file; product is served once every 7 days,
# so find date closest to the last day of the 8-day period. 
# Check to make sure a day within the period is in file.

      satDay <- which(day==BegDay + 7)

      if (length(satDay)==0) {
           satDay <- which(day==BegDay + 6)
          }
      if (length(satDay)==0) {
           satDay <- which(day==BegDay + 5)
          }
      if (length(satDay)==0) {
           satDay <- which(day==BegDay + 4)
          }
      if (length(satDay)==0) {
           satDay <- which(day==BegDay + 3)
          }
      if (length(satDay)==0) {
           satDay <- which(day==BegDay + 2)
          }
      if (length(satDay)==0) {
           satDay <- which(day==BegDay + 1)
          }

      if (length(satDay)==0) print('Not OK! Missing dates in nc file.')
#  
#
# Grab SSHd for the satellite day closest to the last day of the 8-day period 

      SSHd <- matrix(data.var[,,satDay], nrows, ncols, dimnames=list(lon,lat))

# Get SSHd SD based on up to 9 pixels around the data cell.

     SSHrms <- SSHd
     SSHrms [,] <- NA

    for (r in 1:nrows){
      for (c in 1:ncols) {    
        row1 <- max(r-1,1)
        row2 <- min(r+1,nrows)
        col1 <- max(c-1,1)
        col2 <- min(c+1,ncols)
        SSHrms[r,c] <- sd(SSHd[c(row1:row2),c(col1:col2)], na.rm=TRUE)
        }
      }

# Add columns for new SSH variables to grid data
#
      grid.data$SSHd <- NA
      grid.data$SSHrms <- NA

#
# Find data closest to each grid point and add to data.frame
#
      for (i in 1:num.pixels) {
        lat.indx<-which(abs(lat-gridlat[i])==min(abs(lat-gridlat[i])))
        lon.indx<-which(abs(lon-gridlon[i])==min(abs(lon-gridlon[i])))
        nearest.cells <- length(lat.indx) * length(lon.indx)
        num.vals <- which(!is.na(SSHd[lon.indx,lat.indx]))
        if (length(num.vals)>0) {
          if ((length(num.vals)==nearest.cells) | (nearest.cells == 1)) {
            grid.data$SSHd[i]  <- SSHd[lon.indx[1],lat.indx[1]] 
            grid.data$SSHrms[i] <- SSHrms[lon.indx[1],lat.indx[1]] 
          } else {
            grid.data$SSHd[i]  <- SSHd[lon.indx,lat.indx][num.vals[1]]
            grid.data$SSHrms[i] <- SSHrms[lon.indx,lat.indx][num.vals[1]]
          }
        }
      }

  nc_close(nc.data)

#
#
#--------------------------------------------------------------------------------------
#
# Print data to csv

      grid.datafile <- paste(out.path,'CCE_',gridsize[whichgrid],'_',BegDay,'.csv',sep="")
      write.table(grid.data, grid.datafile, sep = "," , col.names = TRUE, row.names = FALSE)

    }  # Get next 8-day period for this year

  }  # Next year

   t2<-Sys.time()
   print(t2-t1)


