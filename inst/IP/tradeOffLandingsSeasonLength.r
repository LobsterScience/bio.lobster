	u = lobster.db('process.logs')
	
seasonLengthTradeOff <- function(u = u,lfa,delta_ndays){
		u = subset(u, SYEAR>2016 & LFA==lfa)
	uu = aggregate(WEIGHT_KG~DOS, data=u, FUN=sum)
	uu$CSUM=cumsum(uu$WEIGHT_KG)/sum(uu$WEIGHT_KG)
	o = mean(diff(uu$CSUM[1:4]))
	print(o)
	ooo = o * delta_ndays
	oo = uu$DOS[which.min(abs(uu$CSUM-(1-ooo)))]
	days = max(uu$DOS)-oo
	with(uu, plot(DOS, CSUM, xlab='Day of Season',ylab='Cumulative Proportion of Landings', type='l', main=lfa))
	legend('bottomright',paste(paste(delta_ndays, "days at start of season equals",sep=" "), paste(days, "days at the end of the season", sep=" "),sep="\n"), bty='n',cex=.8)
	segments(x0=c(delta_ndays,oo,delta_ndays,oo),x1=c(delta_ndays,oo,-5,-5),y0=c(0,0,ooo,1-ooo), y1=c(ooo,1-ooo,ooo,1-ooo),col='red')
	}

###density dependence will likely be impt for pos neg neu growth
pdf('~/tmp/LandingsTradeOffs.pdf')
par(mfrow=c(2,2),mar = c(2, 4, 2, 1))

seasonLengthTradeOff(u,27,2)
seasonLengthTradeOff(u,29,2)
seasonLengthTradeOff(u,30,2)
seasonLengthTradeOff(u,'31A',2)


seasonLengthTradeOff(u,'31B',2)
seasonLengthTradeOff(u,32,2)
seasonLengthTradeOff(u,33,2)
seasonLengthTradeOff(u,34,2)

seasonLengthTradeOff(u,'35',2)
seasonLengthTradeOff(u,36,2)
seasonLengthTradeOff(u,38,2)
dev.off()