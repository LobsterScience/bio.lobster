require(bio.lobster)
require(bio.utilities)

a = lobster.db('process.logs.unfiltered')

aa = aggregate(DATE_FISHED~LFA+SYEAR+VESSEL_NAME+LICENCE_ID,data=a,FUN=function(x) length(unique(x)))

#Potential Fishings Days
dat = lobster.db('season.dates')
dat$DF = dat$END_DATE - dat$START_DATE
dF = aggregate(DF~SYEAR+LFA,data=dat,FUN=sum)


gv = aggregate(DATE_FISHED~LFA+SYEAR,data=aa,FUN='median')

with(subset(gv,LFA==27),plot(SYEAR,DATE_FISHED,type='l'))

pdf('~/tmp/LFA27DaysFished.pdf')
par(mfrow=c(2,2),mar=c(3,4,1.5,0.5),oma=c(0.4,1,1,1))

x = subset(aa,LFA==27 & SYEAR>2003)
y = unique(x$SYEAR)
y = y[order(y)]
for(i in y){
    u = subset(x,SYEAR==i)
    u = table(u$DATE_FISHED)
    u = data.frame(Days=as.numeric(names(u)),Count=u)
    id = data.frame(Days=min(u$Days):max(u$Days))
    u = merge(id,u,all.x=T)
    u = u[,c(1,3)]
    u = na.zero(u)
    names(u) = c('Days','Freq')
    u$cs = cumsum(u$Freq)/sum(u$Freq,na.rm=T)
}
dev.off()

aggregate(DATE_FISHED~LFA,data=subset(gv,SYEAR>2012),FUN=median)