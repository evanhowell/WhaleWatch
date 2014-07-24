if (!is.installed("mgcv")){
    install.packages("mgcv")
  }

library(mgcv)

#Run all GAMs to average

factors = read.csv("WhaleWatchFactors.csv")

wwvector = vector('list')
files = paste("bwhaleGAMM_reduced",1:40,".RData",sep="")
for(i in files){
	load(i)
	wwvector[[i]] = predict.gam(bwhaleGAMM$gam,get("factors"),se=TRUE, type="response")
	names(wwvector) = sub((i),"",names(wwvector))
}

gam = data.frame(wwvector)
fit =gam[ ,!substr(colnames(gam),1,1)=="s"]
#sefit =gam[ ,substr(colnames(gam),1,1)=="s"]
fitmean = rowMeans(fit,na.rm=T)
sd = apply(fit,1,sd)
predict = cbind(factors,fitmean,sd)
predict$percent = predict$fitmean*100

#Write a .csv file with all data if desired for comparisons
write.csv(predict,sprintf("WhaleWatchPredictions_%s_%s.csv",month,year))

#Create text files for GMT  
fitxyz = data.frame(predict[2],predict[1],predict[8])
sdxyz = data.frame(predict[2],predict[1],predict[7])
write.table(fitxyz,"fit.txt",col.names=FALSE,row.names=F,quote=FALSE) #must have no headers, no row names, no quotes
write.table(sdxyz,"sd.txt",col.names=FALSE,row.names=F,quote=FALSE)

