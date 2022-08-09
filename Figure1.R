##########Figure 1
library(dplyr)
case <- read.csv("/Users/faithho/hkcase_20210301.csv")
case$confirm.date <- as.Date(case$confirm.date,"%d/%m/%Y")
case$onset.date <- as.Date(case$onset.date,"%d/%m/%Y")
case$onset.date[is.na(case$onset.date)] <- case$confirm.date[is.na(case$onset.date)]
unique(case$classification)
case$unlinked.local <- ifelse(case$classification=="Possibly local case"|
                                case$classification=="Local case",1,0)
case$secondary <- ifelse(case$classification=="Epidemiologically linked with local case"|
                           case$classification=="Epidemiologically linked with possibly local case",1,0)
case$epi.date <- ifelse(is.na(case$onset.date), as.Date(case$confirm.date, origin = "1970-01-01"), as.Date(case$onset.date, origin = "1970-01-01"))
case$epi.date <-as.Date(case$epi.date, origin = "1970-01-01")

df2 <- data.frame(date=seq(as.Date("2020-06-01"),as.Date("2021-02-23"),"day"),imported=NA,unlinked.local=NA,secondary=NA)
for (i in 1:length(df2$date)){
  df2$unlinked.local[i] <- sum(case$unlinked.local[case$onset.date==df2$date[i]],na.rm=T)
  df2$secondary[i] <- sum(case$secondary[case$onset.date==df2$date[i]],na.rm=T)
}
df2$sunday <- ifelse(strftime(df2$date, "%A")=="Sunday",paste(df2$date),NA)

pdf(file="/Users/faithho/Desktop/Figure1_revision.pdf",
    width=12,
    height=8)
layout(matrix(c(1,2,3,4,5),5,1,byrow=TRUE),heights=c(0.2,rep(2,4)))
par(mar = c(0,0,0,0),xpd=T,cex=1)
plot(0,xlim=c(0,length(df2$date))+1,ylim=c(0,0.15),type="h",axes=F,ann=F,main=3)
legend(-6,0.18,c("Unlinked local cases","Linked local cases","(1) Ban on dine-in service from 6pm","(2) Restricted headcount in restaurants","(3) Ban on group gatherings","(4) Closure of bars","(5) Civil servants adopt working from home arrangements","(6) Ban on live performances and dancing activity"),col=c(grey(0.5),grey(0.8),alpha(c("orange","brown","red","darkmagenta","royal blue","dark green"),0.3)),cex=0.5, pch=rep(15,7),border=c("black","black",rep(NA,5)),horiz = TRUE,text.width=c(6,19.5,18.3,23.3,26.5,26.5,25,29.3),bty="n") #horiz = TRUE

par(mar=c(2.5,2,1,1),xpd=T)
plot(0,xlim=c(0,length(df2$date))+1,ylim=c(0,150),type="h",axes=F,ann=F,main=3)
for (i in 1:length(df2$date)){
  rect(i-1,0,i,df2$unlinked.local[i],col=grey(0.5))
  rect(i-1,df2$unlinked.local[i],i,df2$unlinked.local[i]+df2$secondary[i],col=grey(0.8))
}
axis(2,las=1,at=0:15*10, labels=NA,cex.axis=0.6,line=-1.7,tck=-0.01)
mtext(0:7*20,side=2,at=0:7*20,las=1,cex=0.4,line=-1.5)
mtext(side=2,"Number of Cases", cex=0.7, las=0)
axis(1, at=0:268, labels=NA, las=1,line=-0.1,tck=-0.01)
axis(1, at=c(31,61,92,123,153,184,214,245), labels=NA, lwd=0, lwd.ticks=1, tck=-0.1,line=-0.1)
axis(1, at=214, labels=NA, lwd=0, lwd.ticks=1, tck=-0.2,line=-0.1)
mtext(c("Jun","Jul","Aug","Sep","Oct","Nov","Dec","Jan","Feb"), side=1, at=c(31,61,92,123,153,184,214,245,273)-15, line=0.1, cex=0.65,las=0)
mtext(c("2020","2021"), side=1, at=c(109,245), line=1.2, cex=0.7,las=0)
mtext(side=1,line=1.9,"Date of symptom onset",cex=0.8)
#restaurant no dine-in service until 6pm
rect(which(df2$date=="2020-07-15"),0,which(df2$date=="2020-08-27"),150,col=alpha("orange",0.1),border=F)
rect(which(df2$date=="2020-12-10"),0,which(df2$date=="2021-02-17"),150,col=alpha("orange",0.1),border=F)
#Restricted headcount in restaurants
rect(which(df2$date=="2020-07-15"),0,which(df2$date=="2020-10-29"),140,col=alpha("brown",0.1),border=F) 
rect(which(df2$date=="2020-11-16"),0,which(df2$date==max(df2$date)),140,col=alpha("brown",0.1),border=F) 
#Ban on group gathering
rect(which(df2$date=="2020-07-29"),0,which(df2$date=="2020-09-10"),130,col=alpha("red",0.1),border=F)
rect(which(df2$date=="2020-12-02"),0,which(df2$date=="2021-02-21"),130,col=alpha("red",0.1),border=F)
#Closure of bars
rect(which(df2$date=="2020-07-15"),0,which(df2$date=="2020-09-17"),120,col=alpha("darkmagenta",0.1),border=F) #Closures of bars and premises selling liquor
rect(which(df2$date=="2020-12-02"),0,which(df2$date==max(df2$date)),120,col=alpha("darkmagenta",0.1),border=F) #Closures of bars and premises selling liquor
#WFH arrangement
rect(which(df2$date=="2020-07-15"),0,which(df2$date=="2020-09-14"),110,col=alpha("royal blue",0.1),border=F)
rect(which(df2$date=="2020-11-20"),0,which(df2$date=="2021-02-17"),110,col=alpha("royal blue",0.1),border=F)
#Ban on live performances and dancing activity
rect(which(df2$date=="2020-07-15"),0,which(df2$date=="2020-10-29"),100,col=alpha("dark green",0.1),border=F) #Closures of bars and premises selling liquor
rect(which(df2$date=="2020-11-22"),0,which(df2$date==max(df2$date)),100,col=alpha("dark green",0.1),border=F) 
text(-17,155,"A",font=2)

par(mar=c(3,2,1,1),xpd=T)
plot(NA,xlim=c(0,268),ylim=c(0,6),xlab="",ylab="",axes=F,main="")
axis(1, at=0:268, labels=NA, las=1,line=-0.1,tck=-0.01)
axis(1, at=c(31,61,92,123,153,184,214,245), labels=NA, lwd=0, lwd.ticks=1, tck=-0.1,line=-0.1)
axis(1, at=214, labels=NA, lwd=0, lwd.ticks=1, tck=-0.2,line=-0.1)
mtext(c("Jun","Jul","Aug","Sep","Oct","Nov","Dec","Jan","Feb"), side=1, at=c(31,61,92,123,153,184,214,245,273)-15, line=0.1, cex=0.65,las=0)
mtext(c("2020","2021"), side=1, at=c(109,245), line=1.2, cex=0.7,las=0)
axis(2,at=0:6,labels=NA,tck=-0.01,line=-1.9)
mtext(0:6,side=2,at=0:6,las=1,cex=0.6,line=-1.5)
polygon(c(36:267+0.5,rev(36:267)+0.5),c(pois.rt.ci.l[32:263],rev(pois.rt.ci.u[32:263])),col=alpha("royal blue",0.3),border=NA)
polygon(c(36:267+0.5,rev(36:267)+0.5),c(rt.ci.l[32:263],rev(rt.ci.u[32:263])),col=alpha("red",0.3),border=NA)
lines(36:267+0.5,as.numeric(unlist(pois.rt.fit2$summary("rt","mean")[32:263,2])),col="royal blue")
lines(36:267+0.5,rt[32:263],col="red")
lines(0:266+0.3,rep(1,267),lty=2)
mtext(side=2,"Effective reproductive \nnumber",cex=0.65,las=0,line=-1)
text(-17,6.5,"B",font=2)

par(mar=c(3,2,1,1),xpd=T)
plot(NA,xlim=c(0,268),ylim=c(0,2),xlab="",ylab="",axes=F,main="")
axis(1, at=0:268, labels=NA, las=1,line=-0.1,tck=-0.01)
axis(1, at=c(31,61,92,123,153,184,214,245), labels=NA, lwd=0, lwd.ticks=1, tck=-0.1,line=-0.1)
axis(1, at=214, labels=NA, lwd=0, lwd.ticks=1, tck=-0.2,line=-0.1)
mtext(c("Jun","Jul","Aug","Sep","Oct","Nov","Dec","Jan","Feb"), side=1, at=c(31,61,92,123,153,184,214,245,273)-15, line=0.1, cex=0.65,las=0)
mtext(c("2020","2021"), side=1, at=c(109,245), line=1.2, cex=0.7,las=0)
axis(2,at=0.5*0:4,labels=NA,tck=-0.01,line=-1.9)
mtext(0.5*0:4,side=2,at=0.5*0:4,las=1,cex=0.6,line=-1.5)
polygon(c(36:267+0.5,rev(36:267)+0.5),c(r.ci.l[32:263],rev(r.ci.u[32:263])),col=alpha("dark green",0.3),border=NA)
lines(36:267+0.5,r[32:263],col="dark green")
lines(0:266+0.3,rep(1,267),lty=2)
mtext(side=2,"Overdispersion \nparameter",cex=0.7,las=0,line=-0.8)
text(-17,2.1,"C",font=2)

par(mar=c(3,2,1,1),xpd=T)
plot(NA,xlim=c(0,268),ylim=c(0,0.5),xlab="",ylab="",axes=F,main="")
axis(1, at=0:268, labels=NA, las=1,line=-0.1,tck=-0.01)
axis(1, at=c(31,61,92,123,153,184,214,245), labels=NA, lwd=0, lwd.ticks=1, tck=-0.1,line=-0.1)
axis(1, at=214, labels=NA, lwd=0, lwd.ticks=1, tck=-0.2,line=-0.1)
mtext(c("Jun","Jul","Aug","Sep","Oct","Nov","Dec","Jan","Feb"), side=1, at=c(31,61,92,123,153,184,214,245,273)-15, line=0.1, cex=0.65,las=0)
mtext(c("2020","2021"), side=1, at=c(109,245), line=1.2, cex=0.7,las=0)
axis(2,at=0.1*0:5,labels=NA,tck=-0.01,line=-1.9)
mtext(0.1*0:5,side=2,at=0.1*0:5,las=1,line=-1.5,cex=0.6)
polygon(c(36:267+0.5,rev(36:267)+0.5),c(p80.ci.l[32:263],rev(p80.ci.u[32:263])),col=alpha("dark orange",0.3),border=NA)
lines(36:267+0.5,p80[32:263],col="dark orange")
lines(0:267,rep(0.2,268),lty=2)
mtext(side=1,line=1.9,"Date",cex=0.8)
mtext(side=2,"p80",cex=0.7,las=0,line=-0.4)
text(-17,0.51,"D",font=2)
dev.off()

###########################################################################

