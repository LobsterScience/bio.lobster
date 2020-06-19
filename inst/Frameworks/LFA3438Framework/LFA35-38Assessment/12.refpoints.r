

require(bio.lobster)
p = bio.lobster::load.environment()
require(bio.polygons)
require(PBSmapping)
require(bio.utilities)

##landings
###post framework ###id productivity based on survey trends

require(bcp)
ff = "LFA35-38Assessment"
fp1 = file.path(project.datadirectory('bio.lobster'),"analysis",ff)
fpf1 = file.path(project.figuredirectory('bio.lobster'),ff)

df =  read.csv(file.path(dadir,'LFA3538TotalB.csv'))
df2 = read.csv(file.path(dadir,'LFA3538CommercialB.csv'))
df = subset(df,yr<1999)
df2 = subset(df2,yr>1998)
df = as.data.frame(rbind(df,df2))
df = df[,c('yr','w.Yst','w.ci.Yst.l','w.ci.Yst.u')] #proportion of total weight that is commercial
df$w.Yst[which(df$yr<1999)] <- df$w.Yst[which(df$yr<1999)]*0.746
df$w.ci.Yst.l[which(df$yr<1999)] <- df$w.ci.Yst.l[which(df$yr<1999)]*0.746
df$w.ci.Yst.u[which(df$yr<1999)] <- df$w.ci.Yst.u[which(df$yr<1999)]*0.746

h = subset(df,w.Yst>0) #need to remove the zeros for bcp to function correctly

b = bcp(log(h$w.Yst),w0=0.2,p0=0.05)
plot(b,xaxlab=h$yr,xlab='Year')
savePlot(file.path(fpf1,'BCPCommB35-38.png'))
j = cbind(h$yr,b$posterior.prob)

hh = 2010
####2010 common change point across BoF LFAs

####LFA 35

cp = read.csv(file.path(fp1,'CPUEmodelindex.csv'))
c5 = subset(cp, LFA==35)


png(file=file.path(fpf1,'LFA35CPUERefs.png'),units='in',width=15,height=12,pointsize=18, res=300,type='cairo')
with(c5, plot(YEAR, mu, xlab='Year',ylab='Modelled CPUE', pch=16 ,type='p', ylim=c(0,5) ))
with(c5, arrows(YEAR, y0=ub,y1=lb, length=0))
K = median(subset(c5,YEAR>hh)$mu)
USR = K*.4
LRP = K*.2
abline(h=USR, col='green',lwd=2)
abline(h=LRP, col='blue',lwd=2)
xx = rmed(c5$YEAR, c5$mu)
text(2015,USR+USR*0.1,'USR')
text(2015,LRP-LRP*0.1,'LRP')
lines(xx$yr,xx$x, lwd=2,col='salmon')
dev.off()



####LFA 36

c6 = subset(cp, LFA==36)
png(file=file.path(fpf1,'LFA36CPUERefs.png'),units='in',width=15,height=12,pointsize=18, res=300,type='cairo')
with(c6, plot(YEAR, mu, xlab='Year',ylab='Modelled CPUE', pch=16 ,type='p', ylim=c(0,5) ))
with(c6, arrows(YEAR, y0=ub,y1=lb, length=0))
K = median(subset(c6,YEAR>hh)$mu)
USR = K*.4
LRP = K*.2
text(2015,USR+USR*0.1,'USR')
text(2015,LRP-LRP*0.1,'LRP')

abline(h=USR, col='green',lwd=2)
abline(h=LRP, col='blue',lwd=2)
xx = rmed(c6$YEAR, c6$mu)
lines(xx$yr,xx$x, lwd=2,col='salmon')
dev.off()

####LFA 38

c8 = subset(cp, LFA==38)
png(file=file.path(fpf1,'LFA38CPUERefs.png'),units='in',width=15,height=12,pointsize=18, res=300,type='cairo')
with(c8, plot(YEAR, mu, xlab='Year',ylab='Modelled CPUE', pch=16 ,type='p', ylim=c(0,6) ))
with(c8, arrows(YEAR, y0=ub,y1=lb, length=0))
K = median(subset(c8,YEAR>hh)$mu)
USR = K*.4
LRP = K*.2
text(2015,USR+USR*0.1,'USR')
text(2015,LRP-LRP*0.1,'LRP')
abline(h=USR, col='green',lwd=2)
abline(h=LRP, col='blue',lwd=2)
xx = rmed(c8$YEAR, c8$mu)
lines(xx$yr,xx$x, lwd=2,col='salmon')
dev.off()