WhaleWatch
==========

Code to make the operational NASA WhaleWatch product

Authors: Aimee Hoover, Evan Howell, and Elliott Hazen 2013-

WhaleWatch_Run.R
	* This will be called to run the script start to finish, by running all the scripts from the command line

bathymetry_grdfilter.R 
	* This script downloads STRM_30 PLUS bathymetry and runs a median filter on the data over the desired grid size, 0.25 degrees
This sets the stage for the other scripts. It has an installer function, then installs RCurl and gmt, loading those needed libraries
	* It downloads the bathymetry files
		* These files covering the depth for the desired area are in two separate files. They are both dowloaded in .nc format and combined using GMT
	* All values at or above 0 are turned into NA
		* GMT is then used to filter the data to 0.25 degrees to match the desired file size 
			* MEDIAN FILTER
		* GMT landmask is used to remove all areas of the data where there is a value with land present
	* A text file is created and temporary files are removed to clear folder

bathymetry_grdfilter_SD.R
	* Same as bathymetry_grdfilter.R, except it also finds the bathymetry SD of a given cell
	* This script downloads STRM_30 PLUS bathymetry and runs a median filter on the data over the desired grid size, 0.25 degrees
	* R Packages gmt and RCurl are needed and installed if necessary
	* It downloads the bathymetry files
		* These files covering the depth for the desired area are in two separate files. They are both dowloaded in .nc format and combined using GMT
	* All values at or above 0 are turned into NaN to avoid being taken into account in the filtering process
		* GMT is then used to filter the data to 0.25 degrees to match the desired file size
			* MEDIAN FILTER
		* GMT landmask is used to remove all areas of the data where there is a value with land present
			* We don't want to analyze any cells that have even a portion of land present, and grdlandmask should process these cells out
	* Use grdblockmean to find the SD from the mean within each cell
		* Write this to a text file
	* Combines standard deviation and median bathymetry data files into one text file to be able to call later
	* Temporary files are removed to clear folder

Environmental_Variables_Extract.R
	* Formerly RCurl_Chla_SST.R
	* R Packages ncdf, gmt, and RCurl are needed and installed if necessary
	* This file downloads the variables needed for the GAMM predictions to be made. The variables need to be named the same as in the models created. They are not all currently up and running.
	* Install and load necessary packages (part of this step is unnecessary when all the files are run together, but it was added in when I was troubleshooting only 1 R file-eg added to all to source() run them individually)
	* Calls ERDDAP to download variables in netCDF format

               Functioning Variables:
		* Chl
			* Aqua MODIS monthly (NASA product)
			* Resolution: 0.0125 degrees (1.4 km)

		* SST
			* Aqua MODIS monthly (NASA product)
			* Resolution: 0.0125 degrees (1.4 km)

		* Wind
			* ASCAT monthly
			* Resolution: 0.25 degrees (28 km)
			
               Not Functioning Variables:
		* Eke
			* ASCAT monthly (NOAA/NESDIS)
			* Resolution: 0.25 degrees (28 km)

		* SSHd
			* Aviso
			* Resolution: 0.25 degrees (28 km)
			* Derived product

	* Checks if all data is from the same month (useful with [last] call) by looking at ncdf information for each file
	* Filters Chl and SST to 0.25 degrees with grdfilter
		* MEDIAN FILTER
	* Merges variables into one .csv file for use in prediction call
	* Temporary files are removed to clear folder

Predict_GAMM.R
	* R Package mgcv needed and installed if necessary
	* This file takes the .csv file from Environmental_Variables_Extract.R with the environmental variables and runs it through each blue whale GAMM in the predict.gam function
		* get("factors"),se=TRUE, type="response"
		* The mean of the fits at each lat/lon point is calculated, as well as the percent of this mean fit
	* Writes a .csv file if desired to look back over all the data
	* Write .txt files for mean and standard deviation from the mean (if desired) that GMT can read in the next step

Plot_FitMean_GMT.R
	* R Package gmt needed
	* This file plots the percent mean fit and SD of the fits of the predictions using GMT, outputting a .eps file
	* It creates .cpt files for use in the image file, which can be easily changed and rescaled to desired coloring
	* GMT grdimage is used to create the images, along with psscale

