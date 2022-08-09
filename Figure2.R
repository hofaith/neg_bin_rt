quartz()
par(mar=c(3,3,1,1),xpd=T,mfrow=c(2,2))
plot(NA,xlim=c(0,70),ylim=c(0,3),xlab="",ylab="",axes=F,main="")
axis(1, at=0:70, labels=NA, las=1,line=-0.2,tck=-0.01)
mtext(10*0:7, side=1, at=10*0:7, line=-0.2, cex=0.5,las=0)
axis(2,at=0:3,labels=NA,tck=-0.01,line=-0.5)
mtext(0:3,side=2,at=0:3,las=1,cex=0.5,line=-0.1)
for (i in 1:50) {
  lines(10:70,get(paste0("sim1.rt.ci.l",i))[10:70],col=alpha("dark blue",0.2))
  lines(10:70,get(paste0("sim1.rt.ci.u",i))[10:70],col=alpha("dark blue",0.2))
  lines(10:70,get(paste0("sim1.rt",i))[10:70],col=alpha("black",0.2))
}
lines(10:70,rowMeans(mapply(c,mget(paste0("sim1.rt",1:50))))[10:70],lwd=1.5,col="limegreen")
lines(c(10:70),rep(1.2,70-9),col="red")
text(38,3.3,"k = 10 to 1 to 0.1 to 2",font=2,cex=1.1)
mtext(side=2,"Effective reproductive number \nunder negative binomial distribution",cex=0.6,las=0,line=0.5)
text(-8,3.3,"A",font=2)

plot(NA,xlim=c(0,70),ylim=c(0,3),xlab="",ylab="",axes=F,main="")
axis(1, at=0:70, labels=NA, las=1,line=-0.2,tck=-0.01)
mtext(10*0:7, side=1, at=10*0:7, line=-0.2, cex=0.5,las=0)
axis(2,at=0:3,labels=NA,tck=-0.01,line=-0.5)
mtext(0:3,side=2,at=0:3,las=1,cex=0.5,line=-0.1)
for (i in 1:50) {
  lines(10:70,get(paste0("sim2.rt.ci.l",i))[10:70],col=alpha("dark blue",0.2))
  lines(10:70,get(paste0("sim2.rt.ci.u",i))[10:70],col=alpha("dark blue",0.2))
  lines(10:70,get(paste0("sim2.rt",i))[10:70],col=alpha("black",0.2))
}
lines(10:70,rowMeans(mapply(c,mget(paste0("sim2.rt",1:50))))[10:70],lwd=1.5,col="limegreen")
lines(c(10:35,35:70),c(rep(1.5,35-9),rep(0.8,36)),col="red")
text(38,3.3,"k = 10 to 1 to 0.1 to 2",font=2,cex=1.1)
text(-8,3.3,"B",font=2)

par(mar=c(3,3,1,1),xpd=T)
plot(NA,xlim=c(0,70),ylim=c(0,10),xlab="",ylab="",axes=F,main="")
axis(1, at=0:70, labels=NA, las=1,line=-0.2,tck=-0.01)
mtext(10*0:7, side=1, at=10*0:7, line=-0.2, cex=0.5,las=0)
axis(2,at=0:10,labels=NA,tck=-0.01,line=-0.5)
mtext(0:10,side=2,at=0:10,las=1,cex=0.5,line=-0.1)
mtext(side=2,"Overdispersion parameter (k)",cex=0.7,las=0,line=0.8)
for (i in 1:50) {
  lines(10:70,get(paste0("sim1.r.ci.l",i))[10:70],col=alpha("dark blue",0.2))
  lines(10:70,get(paste0("sim1.r.ci.u",i))[10:70],col=alpha("dark blue",0.2))
  lines(10:70,get(paste0("sim1.r",i))[10:70],col=alpha("black",0.2))
}
lines(10:70,rowMeans(mapply(c,mget(paste0("sim1.r",1:50))))[10:70],lwd=1.5,col="limegreen")
text(-8,10.5,"C",font=2)
lines(c(10:18,18:36,36:53,53:70),c(rep(10,18-9),rep(1,19),rep(0.1,18),rep(2,18)),col="red")
mtext(side=1,line=1.9,"Days since epidemic",cex=0.8)

par(mar=c(3,3,1,1),xpd=T)
plot(NA,xlim=c(0,70),ylim=c(0,10),xlab="",ylab="",axes=F,main="")
axis(1, at=0:70, labels=NA, las=1,line=-0.2,tck=-0.01)
mtext(10*0:7, side=1, at=10*0:7, line=-0.2, cex=0.5,las=0)
axis(2,at=0:10,labels=NA,tck=-0.01,line=-0.5)
mtext(0:10,side=2,at=0:10,las=1,cex=0.5,line=-0.1)
for (i in 1:50) {
  lines(10:70,get(paste0("sim2.r.ci.l",i))[10:70],col=alpha("dark blue",0.2))
  lines(10:70,get(paste0("sim2.r.ci.u",i))[10:70],col=alpha("dark blue",0.2))
  lines(10:70,get(paste0("sim2.r",i))[10:70],col=alpha("black",0.2))
}
lines(10:70,rowMeans(mapply(c,mget(paste0("sim2.r",1:50))))[10:70],lwd=1.5,col="limegreen")
text(-8,10.5,"D",font=2)
lines(c(10:18,18:36,36:53,53:70),c(rep(10,18-9),rep(1,19),rep(0.1,18),rep(2,18)),col="red")
mtext(side=1,line=1.9,"Days since epidemic",cex=0.8)

###########################################################################
