#plot test data

par(mfrow=c(2,2))

plot(d$sst.x, d$sst.y,xlab='Elliott',ylab='Evan',main='SST')
plot(d$chl.x, d$chl.y,xlab='Elliott',ylab='Evan',main='Chl-a')
plot(d$sshd, d$ssh,xlab='Elliott',ylab='Evan',main='SSH')
plot(d$sshrms.x, d$sshrms.y,xlab='Elliott',ylab='Evan',main='SSHRMS')