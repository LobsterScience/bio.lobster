#Contextual 1
ff = "LFA35-38Assessment"
fp1 = file.path(project.datadirectory('bio.lobster'),"analysis",ff)
fpf = fpf1 = file.path(project.figuredirectory('bio.lobster'),ff)
Sp = read.csv(file=file.path(fpf, paste('LFA35-38','DFOtotalabund.csv')))
Tm = read.csv(file=file.path(fpf,'LFA35-38bottomtemp.csv'))
Gi = read.csv( file=file.path(fpf1,'GiniLandings35-38.csv'))
Pr = read.csv(file=file.path(fpf1,'LobPredators35-38.csv'))



png(file=file.path(fpf,'Contextual1.png'),units='in',width=10,height=8,pointsize=18, res=300,type='cairo')
par(mfrow=c(2,3), mar=c(4,4,2,1))

with(Sp,plot(yr,n.yst,pch=16,xlab='Year',ylab = 'DFO RV Total Abundance'))
with(Sp, arrows(yr, y0=n.ci.yst.l, y1=n.ci.yst.u, length=0))
with(Sp,lines(yr,runmed(n.yst,3),lwd=2, col='salmon'))

with(Sp,plot(yr,gini,pch=16,xlab='Year',ylab = 'DFO RV Gini Index'))
with(Sp,lines(yr,runmed(gini,3),lwd=2, col='salmon'))

with(Sp,plot(yr,dwao,pch=16,xlab='Year',ylab = 'DFO RV Area Occupied'))
with(Sp,lines(yr,runmed(dwao,3),lwd=2, col='salmon'))


with(Pr,plot(yr,n.yst,pch=16,xlab='Year',ylab = 'Predator Abundance'))
with(Pr, arrows(yr, y0=n.ci.yst.l, y1=n.ci.yst.u, length=0))
with(Pr,lines(yr,runmed(n.yst,3),lwd=2, col='salmon'))


with(Pr,plot(yr,w.yst,pch=16,xlab='Year',ylab = 'Predator Biomass'))
with(Pr, arrows(yr, y0=w.ci.yst.l, y1=w.ci.yst.u, length=0))
with(Pr,lines(yr,runmed(w.yst,3),lwd=2, col='salmon'))

with(Tm,plot(yr,n.yst,pch=16,xlab='Year',ylab = 'DFO RV Bottom Temperature',ylim=c(5,11)))
with(Tm, arrows(yr, y0=n.ci.yst.l, y1=n.ci.yst.u, length=0))
with(Tm,lines(yr,runmed(n.yst,3),lwd=2, col='salmon'))

graphics.off()


png(file=file.path(fpf,'Contextual1.png'),units='in',width=10,height=8,pointsize=18, res=300,type='cairo')
par(mfrow=c(1,3), mar=c(4,4,2,1))

with(subset(Gi, LFA==35),plot(YEAR,LANDINGS,pch=16,xlab='Year',ylab = 'LFA 35 Gini Index Fishery'))
with(subset(Gi, LFA==35),lines(yr,runmed(LANDINGS,3),lwd=2, col='salmon'))

with(subset(Gi, LFA==36),plot(YEAR,LANDINGS,pch=16,xlab='Year',ylab = 'LFA 36 Gini Index Fishery'))
with(subset(Gi, LFA==36),lines(yr,runmed(LANDINGS,3),lwd=2, col='salmon'))


with(subset(Gi, LFA==38),plot(YEAR,LANDINGS,pch=16,xlab='Year',ylab = 'LFA 38 Gini Index Fishery'))
with(subset(Gi, LFA==38),lines(yr,runmed(LANDINGS,3),lwd=2, col='salmon'))