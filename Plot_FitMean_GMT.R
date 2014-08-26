#This file uses GMT to create images for the mean fit of the predictions and the mean SD of the predictions for the models.

library(gmt)

#Create cpts
if (!file.exists("ww.cpt")){
	gmt.system("makecpt -Cjet -T0/100/0.1 -Z",file="ww.cpt")
}
if (!file.exists("ww2.cpt")){
	gmt.system("makecpt -Cjet -T0/0.25/0.01 -Z",file="ww2.cpt")
}

#Create a map using the manipulated data, percent of the mean fit
gmt(par=NULL, file="whalewatchfit.eps", style="u", quiet=TRUE)
gmt.system("xyz2grd fit.txt -Rg225/245/30/49 -I0.25 -Gwhalewatchfit.grd")
gmt.system("grdimage whalewatchfit.grd -Rg225/245/30N/49N -JM6 -P -Bf5a5/f2.5a5WeSn:.WhaleWatch: -X3 -Y5 -K -Cww.cpt", file="whalewatchfit.eps",append=FALSE) #Create EPS file
gmt.system("psscale -D2.65i/1.5i/7.4c/0.75c -P -B10:Percent_Fit: -Cww.cpt -K -O", file="whalewatchfit.eps",append=TRUE)
psclose(file="whalewatchfit.eps",trailer=TRUE)

#Create a map using the manipulated data, SD of the fit
gmt(par=NULL, file="whalewatchfitSD.eps", style="u", quiet=TRUE)
gmt.system("xyz2grd sd.txt -Rg225/245/30/49 -I0.25 -GwhalewatchfitSD.grd")
gmt.system("grdimage whalewatchfitSD.grd -Rg225/245/30N/49N -JM6 -P -Bf5a5/f2.5a5WeSn:.WhaleWatch_SD: -X3 -Y5 -K -Cww2.cpt", file="whalewatchfitSD.eps",append=FALSE) #Create EPS file
gmt.system("psscale -D2.65i/1.5i/7.4c/0.75c -P -B0.02:Percent_Fit: -Cww2.cpt -K -O", file="whalewatchfitSD.eps",append=TRUE)
psclose(file="whalewatchfitSD.eps",trailer=TRUE)

