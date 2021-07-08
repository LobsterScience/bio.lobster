require(bio.lobster)
require(bio.utilities)
u = lobster.db('process.logs')
	
earlySeasonCPUE <- function(u1 = u,yrs=2007:2020,lfa=38,date='Jan 1'){
	out = list()
  for(i in 1:length(yrs)){
      y = yrs[i]
	    k = subset(u1, SYEAR==y & LFA==lfa)
	    uu = aggregate(WEIGHT_KG~DOS+DATE_FISHED, data=k, FUN=sum)
	uu$CSUM=cumsum(uu$WEIGHT_KG)/sum(uu$WEIGHT_KG)
	d = as.Date(paste(y,1,1,sep="-"))
	l = which.min(abs(uu$DATE_FISHED-d))
	
	with(uu, plot(DOS, CSUM, xlab='Day of Season',ylab='Cumulative Proportion of Landings', type='l', main=paste(y-1,y,sep="-")))
	legend('bottomright',paste(paste("Proportion of total landings between", paste(min(uu$DATE_FISHED),'and',d,"=",round(uu$CSUM[l],2),sep=" "),sep="\n")), bty='n',cex=.8)
	segments(x0=c(0,uu$DOS[l]),x1=c(uu$DOS[l],uu$DOS[l]),y0=c(uu$CSUM[l],0), y1=c(uu$CSUM[l],uu$CSUM[l]),col='red')
	out[[i]] = data.frame(Year=y,DateF=min(uu$DATE_FISHED),Prop=uu$CSUM[l],Days=l)
  }
	return(do.call(rbind,out))
  }
pdf('~/tmp/l38LandingsPreJan1.pdf')
par(mfrow=c(2,2),mar = c(2, 4, 2, 1))

g = earlySeasonCPUE()
g$Dayss = g$DateF-as.Date(paste(g$Year-1,11,1,sep="-"))
dev.off()

pdf('~/tmp/l38DayOfNovPreJan1.pdf')
with(g,plot(Dayss+1,Prop,xlab='Date of Start of Season',ylab='Prop of Total Landings'))
dev.off()
