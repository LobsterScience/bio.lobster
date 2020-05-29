yrs = 2007:2016
logsInSeason=lobster.db("process.logs")

logdata = subset(logsInSeason,month(DATE_FISHED)==5)


gtcr = gridTrends(logdata,308:310,yrs=yrs,variable="CPUE")

 plot(yrs,rep(0.5,length(yrs)),type='n',ylim=c(0,1),ylab="CPUE")

 for(i in 1:3)lines(yrs,gtcr[[i]],lty=i,col=i)




gtla = gridTrends(logdata,308:310,yrs=yrs,variable="TOTAL_WEIGHT_KG",fun=sum)

 plot(yrs,rep(0.5,length(yrs)),type='n',ylim=c(0,500),ylab="Landings (t)")

 for(i in 1:3)lines(yrs,gtla[[i]]/1000,lty=i,col=i)


