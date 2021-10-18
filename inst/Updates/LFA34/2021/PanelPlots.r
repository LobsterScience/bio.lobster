#plots
require(bio.lobster)

##Commercial
ff = "LFA34Update"
fp1 = file.path(project.datadirectory('bio.lobster'),"analysis",ff)
fpf = fpf1 = file.path(project.figuredirectory('bio.lobster'),ff)


a = lobster.db('seasonal.landings')
a = subset(a,select=c(SYEAR,LFA34))
a$yr = as.numeric(substr(a$SYEAR,6,9))
write.csv(a,file=file.path(fpf1,'updatedlandings.csv') )

Sp = read.csv(file=file.path(fpf,paste('LFA34-NEFSCSpringCommercialB.csv',sep="-")))
DF = read.csv(file=file.path(fpf,paste('DFORelf34.csv',sep="-")))
Fa = read.csv(file=file.path(fpf,paste('LFA34-NEFSCFallCommercialB.csv',sep="-")))
IL = read.csv(file=file.path(fpf1,'ILTSCommB.csv'))
c34 = read.csv(file=file.path(fpf1,'updatedlandings.csv') )

RP = list(c(1539,80.1),c(3232, 223), c(686,107),c(4574,1975))
png(file=file.path(fpf,'commb.png'),units='in',width=12,height=9,pointsize=18, res=300,type='cairo')
par(mfrow=c(2,2), mar=c(4,4,2,1), mai=c(0.5,1,0.4,0.4))

with(Sp,plot(yr,w.Yst,pch=16,xlab=' ',ylab = 'NEFSC Spring', ylim=c(0,8000)))
Spr = rmed(Sp$yr, Sp$w.Yst)
lines(Spr$yr, Spr$x, lwd=2, col='salmon')
with(Sp, arrows(yr, y0=w.ci.Yst.l, y1=w.ci.Yst.u, length=0))
abline(h=RP[[1]][1], lwd=2, col='green',lty=2)
abline(h=RP[[1]][2], lwd=2, col='blue',lty=3)

with(Fa,plot(yr,w.Yst,pch=16,xlab=' ',ylab = 'NEFSC Fall', ylim=c(0,30000)))
Far = rmed(Fa$yr, Fa$w.Yst)
lines(Far$yr, Far$x, lwd=2, col='salmon')
with(Fa, arrows(yr, y0=w.ci.Yst.l, y1=w.ci.Yst.u, length=0))
abline(h=RP[[2]][1], lwd=2, col='green',lty=2)
abline(h=RP[[2]][2], lwd=2, col='blue',lty=3)

with(DF,plot(yr,w.Yst,pch=1,xlab='Year',ylab = 'DFO RV Survey', ylim=c(0,8000)))
with(subset(DF,yr>1998),points(yr,w.Yst,pch=16))

DFr = rmed(DF$yr, DF$w.Yst)
lines(DFr$yr, DFr$x, lwd=2, col='salmon')
with(DF, arrows(yr, y0=w.ci.Yst.l, y1=w.ci.Yst.u, length=0))
abline(h=RP[[3]][1], lwd=2, col='green',lty=2)
abline(h=RP[[3]][2], lwd=2, col='blue',lty=3)

with(IL,plot(Year,B,pch=16,xlab='Year',ylab = 'ILTS', ylim=c(0,30000)))
DFr = rmed(IL$Year, IL$B)
lines(DFr$yr, DFr$x, lwd=2, col='salmon')
with(IL, arrows(Year, y0=lB, y1=uB, length=0))
abline(h=RP[[4]][1], lwd=2, col='green',lty=2)
abline(h=RP[[4]][2], lwd=2, col='blue',lty=3)

dev.off()

#Relative F


RRs = list(.9113,10.5713,.97063,.83955)
png(file=file.path(fpf,'relf.png'),units='in',width=10,height=8,pointsize=18, res=300,type='cairo')
par(mfrow=c(2,2), mar=c(4,4,2,1), mai=c(0.5,1,0.4,0.4))

Sp = Sp[,c('yr','w.Yst','w.ci.Yst.l','w.ci.Yst.u')]
Sp  =merge(Sp,c34,by.x='yr',by.y='yr')

Sp$rL = Sp$LFA34/(Sp$w.ci.Yst.l+Sp$LFA34)
Sp$rU =Sp$LFA34/ (Sp$w.ci.Yst.u+Sp$LFA34)
Sp$rM = Sp$LFA34/(Sp$w.Yst+Sp$LFA34)

Sp[which(!is.finite(Sp[,7])),7] <- NA
Sp[which(!is.finite(Sp[,8])),8] <- NA
Sp[which(!is.finite(Sp[,9])),9] <- NA

rf = median(Sp$rM, an.rm=T)
rl = mean(subset(Sp,yr %in% 1970:1998,select=rM)[,1],na.rm=T)

rMM = rmed(Sp$yr, Sp$rM)
wMM = rmed(Sp$yr, Sp$w.Yst)

rMM$x[46] = rMM$x[45]
wMM$x[46] = wMM$x[45]
rMM$x[45] = wMM$x[45] = NA

Sp$rMM = rMM$x
Sp$wMM = wMM$x

with(Sp,plot(yr,rM,pch=16,xlab=' ',ylab='NEFSC Spring',ylim=c(0.6,1)))
with(Sp,arrows(yr,y0=rU,y1=rL, length=0))
xx = rmed(Sp$yr,Sp$rM)
with(xx,lines(yr,x,col='salmon',lwd=2))
abline(h=RRs[[1]],col='blue',lwd=2)


Fa = Fa[,c('yr','w.Yst','w.ci.Yst.l','w.ci.Yst.u')]
Fa  =merge(Fa,c34,by.x='yr',by.y='yr')

Fa$rL = Fa$LFA34/(Fa$w.ci.Yst.l)
Fa$rU =Fa$LFA34/ (Fa$w.ci.Yst.u)
Fa$rM = Fa$LFA34/(Fa$w.Yst)

Fa[which(!is.finite(Fa[,7])),7] <- NA
Fa[which(!is.finite(Fa[,8])),8] <- NA
Fa[which(!is.finite(Fa[,9])),9] <- NA

rf = median(Fa$rM, an.rm=T)
rl = mean(subset(Fa,yr %in% 1970:1998,select=rM)[,1],na.rm=T)

rMM = as.data.frame(do.call(cbind,rmed(Fa$yr, Fa$rM)))
wMM = as.data.frame(do.call(cbind,rmed(Fa$yr, Fa$w.Yst)))

names(rMM)[2] = 'rMM'
names(wMM)[2] = 'wMM'

Fa = merge(Fa,rMM, all.x=T)
Fa = merge(Fa,wMM,all.x=T)

with(Fa,plot(yr,rM,pch=16,xlab=' ',ylab='NEFSC Fall'))
with(Fa,arrows(yr,y0=rU,y1=rL, length=0))
xx = rmed(Fa$yr,Fa$rM)
with(xx,lines(yr,x,col='salmon',lwd=2))
abline(h=rl,col='blue',lwd=2)



DF = DF[,c('yr','w.Yst','w.ci.Yst.l','w.ci.Yst.u')]
DF  =merge(DF,c34,by.x='yr',by.y='yr')

DF$rL = DF$LFA34/(DF$w.ci.Yst.l+DF$LFA34)
DF$rU =DF$LFA34/ (DF$w.ci.Yst.u+DF$LFA34)
DF$rM = DF$LFA34/(DF$w.Yst+DF$LFA34)

DF[which(!is.finite(DF[,7])),7] <- NA
DF[which(!is.finite(DF[,8])),8] <- NA
DF[which(!is.finite(DF[,9])),9] <- NA

rf = median(DF$rM, an.rm=T)
rl = mean(subset(DF,yr %in% 1970:1998,select=rM)[,1],na.rm=T)

DF$rMM = rmed(DF$yr, DF$rM)[[2]]
DF$wMM = rmed(DF$yr, DF$w.Yst)[[2]]

DF$rMM = rmed(DF$yr, DF$rM)[[2]]
DF$wMM = rmed(DF$yr, DF$w.Yst)[[2]]

with(DF,plot(yr,rM,pch=16,xlab=' ',ylab='DFO RV Survey'))
with(DF,arrows(yr,y0=rU,y1=rL, length=0))
xx = rmed(DF$yr,DF$rM)
with(xx,lines(yr,x,col='salmon',lwd=2))
abline(h=rl,col='blue',lwd=2)


names(IL) = c('x','yr','w.Yst','w.ci.Yst.l','w.ci.Yst.u')

IL = IL[,c('yr','w.Yst','w.ci.Yst.l','w.ci.Yst.u')]
IL  =merge(IL,c34,by.x='yr',by.y='yr')

IL$rL = IL$LFA34/(IL$w.ci.Yst.l+IL$LFA34)
IL$rU =IL$LFA34/ (IL$w.ci.Yst.u+IL$LFA34)
IL$rM = IL$LFA34/(IL$w.Yst+IL$LFA34)

IL[which(!is.finite(IL[,7])),7] <- NA
IL[which(!is.finite(IL[,8])),8] <- NA
IL[which(!is.finite(IL[,9])),9] <- NA

rf = median(IL$rM, an.rm=T)
rl = mean(subset(IL,yr %in% 1970:1998,select=rM)[,1],na.rm=T)

IL$rMM = rmed(IL$yr, IL$rM)[[2]]
IL$wMM = rmed(IL$yr, IL$w.Yst)[[2]]

IL$rMM = rmed(IL$yr, IL$rM)[[2]]
IL$wMM = rmed(IL$yr, IL$w.Yst)[[2]]

with(IL,plot(yr,rM,pch=16,xlab=' ',ylab='ILTS',ylim=c(0.3,1)))
with(IL,arrows(yr,y0=rU,y1=rL, length=0))
xx = rmed(IL$yr,IL$rM)
with(xx,lines(yr,x,col='salmon',lwd=2))
abline(h=rl,col='blue',lwd=2)
dev.off()

png(file=file.path(fpf,'phaseplots.png'),units='in',width=12,height=9,pointsize=18, res=300,type='cairo')
par(mfrow=c(2,2), mar=c(5,4,2,1), mai=c(0.5,1,0.4,0.4),oma=c(3,3,0,0))


hcrPlot(B=Sp$wMM/1000,mF=Sp$rMM,USR=RP[[1]][1]/1000,LRP=RP[[1]][2]/1000,RR=RRs[[1]],labels=c(),yrs=c(rep('',length(Sp$yr)-3),2019:2021),ylim=c(0.5,1.1),area.cols=rev(gray.colors(3,start=.4,end=1)),xlab=' ',ylab='NEFSC Spring',yr.ends=T); box();
hcrPlot(B=Fa$wMM/1000,mF=Fa$rMM,USR=RP[[2]][1]/1000,LRP=RP[[2]][2]/1000,RR=RRs[[2]],labels=c(),yrs=c(rep('',length(Fa$yr)-5),2017,'',2019,'',''),area.cols=rev(gray.colors(3,start=.4,end=1)),xlab=' ',ylab='NEFSC Fall',yr.ends=T);box();
hcrPlot(B=DF$wMM/1000,mF=DF$rMM,USR=RP[[3]][1]/1000,LRP=RP[[3]][2]/1000,RR=RRs[[3]],labels=c(),yrs=c(rep('',length(DF$yr)-3),2018:2020),ylim=c(0.5,1.1),area.cols=rev(gray.colors(3,start=.4,end=1)),xlab=' ',ylab='DFO RV Survey',yr.ends=T);box();
hcrPlot(B=IL$wMM/1000,mF=IL$rMM,USR=RP[[4]][1]/1000,LRP=RP[[4]][2]/1000,RR=RRs[[4]],labels=c(),yrs=c(rep('',length(IL$yr)-3),2019:2021),ylim=c(0.5,1.1),area.cols=rev(gray.colors(3,start=.4,end=1)),xlab=' ',ylab='ILTS',yr.ends=T);box();

mtext("Commercial Biomass",side=1, line=1, outer=TRUE, cex=1.1)
mtext("Relative Fishing Mortality", side=2,line=1,outer=TRUE, cex=1.1, las=0)


graphics.off()


