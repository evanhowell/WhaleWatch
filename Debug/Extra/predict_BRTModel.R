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
