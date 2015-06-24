# File load_Functions.R - file of all small functions used in code. These were put into a separate file so that they can be loaded at the beginning of a session and used for all other script files.
# Compiled from scripts 5/18/2015 by Evan Howell - Evan.Howell@noaa.gov

# Function pkgTest - An R function to test if a package is installed. If not, the package and all dependencies will be installed from the default CRAN mirror.
# Code taken from Stack Overflow - http://stackoverflow.com/questions/9341635/how-can-i-check-for-installed-r-packages-before-running-install-packages

pkgTest <- function(x)
  {
    if (!require(x,character.only = TRUE))
    {
      install.packages(x,dep=TRUE)
        if(!require(x,character.only = TRUE)) stop("Package not found")
    }
}

#Function logprint - An R function to print a message with a leading timestamp. Goes to STDOUT which should be set to the logfile in the main program using sink

logprint <- function(msg)
{
  print(paste(Sys.time(),msg, sep=': '))
}

source('Code/get_Bathymetry.R')
source('Code/get_EnvData.R')
source('Code/predict_GAMM.R')
source('Code/plot_GAMMRaster.R')