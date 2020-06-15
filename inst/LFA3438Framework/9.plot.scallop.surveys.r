#plot scallop surveys
fpf1 = file.path(project.figuredirectory('bio.lobster'),"LFA3438Framework2019")

load(file=file.path(project.datadirectory('bio.lobster'),'data','LFA3438Framework','ScallopSurveyIndicators.rdata'))

SFA29 = ScallopSurveyIndicatorsF34
SPA3 = ScallopSurveyIndicatorsS34[which(rownames(ScallopSurveyIndicatorsS34)>2003),]
png(file=file.path(fpf1,'SFA29Totabund.png'),units='in',width=15,height=12,pointsize=18, res=300,type='cairo')
with(SFA29,plot(1999:2018,tot.ab.f34,pch=16,xlab='Year',ylab = 'Abundance'))
with(SFA29,lines(1999:2018,runmed(tot.ab.f34,3),lwd=2, col='salmon'))
dev.off()


png(file=file.path(fpf1,'SPA3Totabund.png'),units='in',width=15,height=12,pointsize=18, res=300,type='cairo')
with(SPA3,plot(2004:2018,tot.ab.s34,pch=16,xlab='Year',ylab = 'Abundance'))
with(SPA3,lines(2004:2018,runmed(tot.ab.s34,3),lwd=2, col='salmon'))
dev.off()



png(file=file.path(fpf1,'SFA29recruitabund.png'),units='in',width=15,height=12,pointsize=18, res=300,type='cairo')
with(SFA29,plot(1999:2018,R1.ab.f34,pch=16,xlab='Year',ylab = 'Abundance'))
with(SFA29,lines(1999:2018,runmed(R1.ab.f34,3),lwd=2, col='salmon'))
dev.off()


png(file=file.path(fpf1,'SPA3recruitabund.png'),units='in',width=15,height=12,pointsize=18, res=300,type='cairo')
with(SPA3,plot(2004:2018,R1.ab.s34,pch=16,xlab='Year',ylab = 'Abundance'))
with(SPA3,lines(2004:2018,runmed(R1.ab.s34,3),lwd=2, col='salmon'))
dev.off()


#### lfa 35

s35 = ScallopSurveyIndicators35
png(file=file.path(fpf1,'S35recruitabund.png'),units='in',width=15,height=12,pointsize=18, res=300,type='cairo')
with(s35,plot(1999:2018,R1.ab.35,pch=16,xlab='Year',ylab = 'Abundance'))
with(s35,lines(1999:2018,runmed(R1.ab.35,3),lwd=2, col='salmon'))
dev.off()


#### lfa 36

s36 = ScallopSurveyIndicators36
png(file=file.path(fpf1,'S36recruitabund.png'),units='in',width=15,height=12,pointsize=18, res=300,type='cairo')
with(s36,plot(1999:2018,R1.ab.36,pch=16,xlab='Year',ylab = 'Abundance'))
with(s36,lines(1999:2018,runmed(R1.ab.36,3),lwd=2, col='salmon'))
dev.off()


#### lfa 38

s38 = ScallopSurveyIndicators38
png(file=file.path(fpf1,'S38recruitabund.png'),units='in',width=15,height=12,pointsize=18, res=300,type='cairo')
with(s38,plot(c(1999:2003, 2005:2018),R1.ab.38,pch=16,xlab='Year',ylab = 'Abundance'))
with(s38,lines(c(1999:2003, 2005:2018),runmed(R1.ab.38,3),lwd=2, col='salmon'))
dev.off()
