require(bio.lobster)
require(bio.utilities)

a = lobster.db('process.logs.unfiltered')


 b =  aggregate(DATE_FISHED~LICENCE_ID+SYEAR+LFA,data=subset(a,WEIGHT_KG>0 & SYEAR>2015),FUN=function(x) length(unique(x)))
 
 bb = aggregate(DATE_FISHED~LFA,data=b,FUN=mean)
 
 ####
 par(mfrow=c(2,2),mar=c(3,4,1.5,0.5),oma=c(0.4,1,1,1))
 
 g = aggregate(cbind(WEIGHT_KG, NUM_OF_TRAPS)~WOS+SYEAR+LFA,subset(a,LFA==34 &SYEAR == 2018),FUN=sum)
 g$CPUE = g$WEIGHT_KG / g$NUM_OF_TRAPS
 
 hh = aggregate(DATE_FISHED~WOS,data=subset(a,LFA==34 & SYEAR==2018),FUN=min)
 g = merge(g,hh)
 
 with(g,{
   plot(DATE_FISHED,CPUE,pch=16,xlab='Date Fished', ylab='CPUE kg/TH')
          r = rmed(yr=DATE_FISHED,x=CPUE)
          lines(r$yr,r$x,col='red',lwd=3)
          legend('topright','LFA34',bty='n')
          })
 
 
 
 g = aggregate(cbind(WEIGHT_KG, NUM_OF_TRAPS)~WOS+SYEAR+LFA,subset(a,LFA==38 &SYEAR == 2018),FUN=sum)
 g$CPUE = g$WEIGHT_KG / g$NUM_OF_TRAPS
 
 hh = aggregate(DATE_FISHED~WOS,data=subset(a,LFA==38 & SYEAR==2018),FUN=min)
 g = merge(g,hh)
 
 with(g,{
   plot(DATE_FISHED,CPUE,pch=16,xlab='Date Fished', ylab='CPUE kg/TH')
   r = rmed(yr=DATE_FISHED,x=CPUE)
   lines(r$yr,r$x,col='red',lwd=3)
   legend('topright','LFA38',bty='n')
 })
 
 
 
 g = aggregate(cbind(WEIGHT_KG, NUM_OF_TRAPS)~WOS+SYEAR+LFA,subset(a,LFA==27 &SYEAR == 2018),FUN=sum)
 g$CPUE = g$WEIGHT_KG / g$NUM_OF_TRAPS
 
 hh = aggregate(DATE_FISHED~WOS,data=subset(a,LFA==27 & SYEAR==2018),FUN=min)
 g = merge(g,hh)
 
 with(g,{
   plot(DATE_FISHED,CPUE,pch=16,xlab='Date Fished', ylab='CPUE kg/TH')
   r = rmed(yr=DATE_FISHED,x=CPUE)
   lines(r$yr,r$x,col='red',lwd=3)
   legend('topright','LFA27',bty='n')
 })
 
 
 g = aggregate(cbind(WEIGHT_KG, NUM_OF_TRAPS)~WOS+SYEAR+LFA,subset(a,LFA==29 &SYEAR == 2018),FUN=sum)
 g$CPUE = g$WEIGHT_KG / g$NUM_OF_TRAPS
 
 hh = aggregate(DATE_FISHED~WOS,data=subset(a,LFA==29 & SYEAR==2018),FUN=min)
 g = merge(g,hh)
 
 with(g,{
   plot(DATE_FISHED,CPUE,pch=16,xlab='Date Fished', ylab='CPUE kg/TH')
   r = rmed(yr=DATE_FISHED,x=CPUE)
   lines(r$yr,r$x,col='red',lwd=3)
   legend('topright','LFA29',bty='n')
 })
 