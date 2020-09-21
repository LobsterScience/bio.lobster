#plots


##Commercial
ff = "LFA34Update"
fp1 = file.path(project.datadirectory('bio.lobster'),"analysis",ff)
fpf = fpf1 = file.path(project.figuredirectory('bio.lobster'),ff)

Sp = read.csv(file=file.path(fpf,paste('LFA34-NEFSCSpringCommercialB.csv',sep="-")))
DF = read.csv(file=file.path(fpf,paste('LFA34-DFOCommercialB.csv',sep="-")))
Fa = read.csv(file=file.path(fpf,paste('LFA34-NEFSCFallCommercialB.csv',sep="-")))
IL = read.csv(file=file.path(fpf1,'ILTSCommB.csv'))


png(file=file.path(fpf,'commb.png'),units='in',width=10,height=8,pointsize=18, res=300,type='cairo')
par(mfrow=c(3,2), mar=c(4,4,2,1))

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

with(DF,plot(yr,w.Yst,pch=16,xlab='Year',ylab = 'DFO Commercial Biomass'))
DFr = rmed(DF$yr, DF$w.Yst)
lines(DFr$yr, DFr$x, lwd=2, col='salmon')
with(DF, arrows(yr, y0=w.ci.Yst.l, y1=w.ci.Yst.u, length=0))
abline(h=686, lwd=2, col='green',lty=2)
abline(h=107, lwd=2, col='blue',lty=3)

dev.off()

