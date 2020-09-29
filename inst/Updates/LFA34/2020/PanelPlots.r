#plots
require(bio.lobster)

##Commercial
ff = "LFA34Update"
fp1 = file.path(project.datadirectory('bio.lobster'),"analysis",ff)
fpf = fpf1 = file.path(project.figuredirectory('bio.lobster'),ff)

Sp = read.csv(file=file.path(fpf,paste('LFA34-NEFSCSpringCommercialB.csv',sep="-")))
DF = read.csv(file=file.path(fpf,paste('LFA34-DFOCommercialB.csv',sep="-")))
Fa = read.csv(file=file.path(fpf,paste('LFA34-NEFSCFallCommercialB.csv',sep="-")))
IL = read.csv(file=file.path(fpf1,'ILTSCommB.csv'))
c34 = read.csv(file=file.path(fpf1,'updatedlandings.csv') )
c34 = c34[,-1]
png(file=file.path(fpf,'commb.png'),units='in',width=10,height=8,pointsize=18, res=300,type='cairo')
par(mfrow=c(2,2), mar=c(4,4,2,1))

with(Sp,plot(yr,w.Yst,pch=16,xlab='Year',ylab = 'NEFSC Spring Commercial Biomass'))
Spr = rmed(Sp$yr, Sp$w.Yst)
lines(Spr$yr, Spr$x, lwd=2, col='salmon')
with(Sp, arrows(yr, y0=w.ci.Yst.l, y1=w.ci.Yst.u, length=0))
abline(h=1539, lwd=2, col='green',lty=2)
abline(h=80.1, lwd=2, col='blue',lty=3)

with(Fa,plot(yr,w.Yst,pch=16,xlab='Year',ylab = 'NEFSC Fall Commercial Biomass'))
Far = rmed(Fa$yr, Fa$w.Yst)
lines(Far$yr, Far$x, lwd=2, col='salmon')
with(Fa, arrows(yr, y0=w.ci.Yst.l, y1=w.ci.Yst.u, length=0))
abline(h=3232, lwd=2, col='green',lty=2)
abline(h=223, lwd=2, col='blue',lty=3)

with(DF,plot(yr,w.Yst,pch=1,xlab='Year',ylab = 'DFO Commercial Biomass'))
with(subset(DF,yr>1998),points(yr,w.Yst,pch=16))

DFr = rmed(DF$yr, DF$w.Yst)
lines(DFr$yr, DFr$x, lwd=2, col='salmon')
with(DF, arrows(yr, y0=w.ci.Yst.l, y1=w.ci.Yst.u, length=0))
abline(h=686, lwd=2, col='green',lty=2)
abline(h=107, lwd=2, col='blue',lty=3)

with(IL,plot(Year,B,pch=16,xlab='Year',ylab = 'ILTS Commercial Biomass'))
DFr = rmed(IL$Year, IL$B)
lines(DFr$yr, DFr$x, lwd=2, col='salmon')
with(IL, arrows(Year, y0=lB, y1=uB, length=0))
abline(h=4574, lwd=2, col='green',lty=2)
abline(h=1975, lwd=2, col='blue',lty=3)

dev.off()

#Relative F
png(file=file.path(fpf,'relf.png'),units='in',width=10,height=8,pointsize=18, res=300,type='cairo')
par(mfrow=c(2,2), mar=c(4,4,2,1))

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

Sp$rMM = rmed(Sp$yr, Sp$rM)[[2]]
Sp$wMM = rmed(Sp$yr, Sp$w.Yst)[[2]]

Sp$rMM = rmed(Sp$yr, Sp$rM)[[2]]
Sp$wMM = rmed(Sp$yr, Sp$w.Yst)[[2]]

with(Sp,plot(yr,rM,pch=16,xlab='Year',ylab='Relative F',ylim=c(0.6,1)))
with(Sp,arrows(yr,y0=rU,y1=rL, length=0))
xx = rmed(Sp$yr,Sp$rM)
with(xx,lines(yr,x,col='salmon',lwd=2))
abline(h=rl,col='blue',lwd=2)


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

Fa$rMM = rmed(Fa$yr, Fa$rM)[[2]]
Fa$wMM = rmed(Fa$yr, Fa$w.Yst)[[2]]

Fa$rMM = rmed(Fa$yr, Fa$rM)[[2]]
Fa$wMM = rmed(Fa$yr, Fa$w.Yst)[[2]]

with(Fa,plot(yr,rM,pch=16,xlab='Year',ylab='Relative F'))
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

with(DF,plot(yr,rM,pch=16,xlab='Year',ylab='Relative F'))
with(DF,arrows(yr,y0=rU,y1=rL, length=0))
xx = rmed(DF$yr,DF$rM)
with(xx,lines(yr,x,col='salmon',lwd=2))
abline(h=rl,col='blue',lwd=2)


with(subset(df,yr>1998),points(yr,w.Yst/1000,pch=16))
xx = rmed(df$yr,df$w.Yst/1000)
xx = as.data.frame(do.call(cbind,xx))
with(subset(xx,yr<1999),lines(yr,x,col='salmon',lwd=2))
with(subset(xx,yr>1998),lines(yr,x,col='salmon',lwd=3))


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

with(IL,plot(yr,rM,pch=16,xlab='Year',ylab='Relative F',ylim=c(0.3,1)))
with(IL,arrows(yr,y0=rU,y1=rL, length=0))
xx = rmed(IL$yr,IL$rM)
with(xx,lines(yr,x,col='salmon',lwd=2))
abline(h=rl,col='blue',lwd=2)
dev.off()