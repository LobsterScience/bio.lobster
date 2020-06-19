#plot scallop surveys
fpf1 = file.path(project.figuredirectory('bio.lobster'),"LFA3438Framework2019")

load(file=file.path(project.datadirectory('bio.lobster'),'data','LFA3438Framework','ScallopSurveyIndicators.rdata'))
load(file=file.path(project.datadirectory('bio.lobster'),'outputs','ScallopSurveyIndicators.rdata'))

SFA29 = ScallopSurveyIndicatorsF34
SPA3 = ScallopSurveyIndicatorsS34[which(rownames(ScallopSurveyIndicatorsS34)>2003),]
yrs29=as.numeric(row.names(SFA29))
yrs3=as.numeric(row.names(SPA3))

png(file=file.path(fpf1,'SFA29Totabund.png'),units='in',width=10,height=8,pointsize=18, res=300,type='cairo')
with(SFA29,plot(yrs29,tot.ab.f34,pch=16,xlab='Year',ylab = 'Abundance'))
with(SFA29,lines(yrs29,runmed(tot.ab.f34,3),lwd=2, col='salmon'))
dev.off()


png(file=file.path(fpf1,'SPA3Totabund.png'),units='in',width=10,height=8,pointsize=18, res=300,type='cairo')
with(SPA3,plot(yrs3,tot.ab.s34,pch=16,xlab='Year',ylab = 'Abundance'))
with(SPA3,lines(yrs3,runmed(tot.ab.s34,3),lwd=2, col='salmon'))
dev.off()



png(file=file.path(fpf1,'SFA29recruitabund.png'),units='in',width=10,height=8,pointsize=18, res=300,type='cairo')
with(SFA29,plot(yrs29,R1.ab.f34,pch=16,xlab='Year',ylab = 'Recruit Abundance'))
with(SFA29,lines(yrs29,runmed(R1.ab.f34,3),lwd=2, col='salmon'))
dev.off()


png(file=file.path(fpf1,'SPA3recruitabund.png'),units='in',width=10,height=8,pointsize=18, res=300,type='cairo')
with(SPA3,plot(yrs3,R1.ab.s34,pch=16,xlab='Year',ylab = 'Abundance'))
with(SPA3,lines(yrs3,runmed(R1.ab.s34,3),lwd=2, col='salmon'))
dev.off()


#### lfa 35

s35 = ScallopSurveyIndicators35
png(file=file.path(fpf1,'S35recruitabund.png'),units='in',width=10,height=8,pointsize=18, res=300,type='cairo')
with(s35,plot(yrs29,R1.ab.35,pch=16,xlab='Year',ylab = 'Abundance'))
with(s35,lines(yrs29,runmed(R1.ab.35,3),lwd=2, col='salmon'))
dev.off()


#### lfa 36

s36 = ScallopSurveyIndicators36
png(file=file.path(fpf1,'S36recruitabund.png'),units='in',width=10,height=8,pointsize=18, res=300,type='cairo')
with(s36,plot(yrs29,R1.ab.36,pch=16,xlab='Year',ylab = 'Abundance'))
with(s36,lines(yrs29,runmed(R1.ab.36,3),lwd=2, col='salmon'))
dev.off()


#### lfa 38

s38 = ScallopSurveyIndicators38
yrs38=as.numeric(row.names(s38))

png(file=file.path(fpf1,'S38recruitabund.png'),units='in',width=10,height=8,pointsize=18, res=300,type='cairo')
with(s38,plot(yrs38,R1.ab.38,pch=16,xlab='Year',ylab = 'Abundance'))
with(s38,lines(yrs38,runmed(R1.ab.38,3),lwd=2, col='salmon'))
dev.off()


#### fsrs recruitment
 	load(file=file.path(project.datadirectory("bio.lobster"),"outputs","fsrsModelIndicators34.rdata"))

 	fsrs34=subset(recruit,Area==34)

png(file=file.path(fpf1,'FSRS34recruitabund.png'),units='in',width=10,height=8,pointsize=18, res=300,type='cairo')
with(fsrs34,plot(YEAR,mu,pch=16,xlab='Year',ylab = 'Abundance',ylim=c(0,6)))
with(fsrs34,lines(YEAR,runmed(mu,3),lwd=2, col='salmon'))
with(fsrs34,arrows(x0=YEAR,x1 = YEAR, y0 = ub, y1 = lb, lwd=1, angle=90, length= 0))
dev.off()

