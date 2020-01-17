#' @export
plotSurveyIndex<-function(trend.dat,yrs,graphic='pdf',index.variable="LobDen",moving.avg=T,moving.median=T,ref.points=T,index.stations=T,fn="SurveyIndex",yLabel=expression(paste("Mean N / ", km^2)),wd=8,ht=6,se=F,add.legend=F,legend.specs = list(leg.place='topleft',lty=c(1,2,1),col=c('orange','green','rgb(0,0,1,0.5)'))){

	## Plot Survey Index Figure 4

	if(index.stations){

			trend.dat = calcIndexStations(trend.dat,n=16, map=F, include.current.year=F)

		}


		if(missing(yrs))yrs=sort(unique(trend.dat$YEAR))
		trend.dat = subset(trend.dat,YEAR%in%yrs)
		trend.dat$Y <- unlist(trend.dat[index.variable])

	LPT   <- with(trend.dat,tapply(Y,YEAR,mean,na.rm=T))
	yrs   <- as.numeric(names(LPT))
	LPTsd <- with(trend.dat,tapply(Y,YEAR,sd,na.rm=T))
	LPTn  <- with(trend.dat,tapply(Y,YEAR,length))
	LPTse <- LPTsd/sqrt(LPTn)
	rmLPT <- mavg(LPT)

	
	if(graphic=='pdf')pdf(file.path( project.figuredirectory("bio.lobster"), paste0(fn,".pdf")),wd,ht)
	if(graphic=='png')png(file=file.path(project.figuredirectory("bio.lobster"), paste0(fn,".png")),units='in',width=wd,height=ht,pointsize=12, res=300,type='cairo')	
	if(graphic=='R')x11()
	plot(yrs,LPT,pch=16,ylim=c(0,max(LPT+LPTse)),xlab='',ylab=yLabel,las=1)
	axis(1,yrs,lab=F,tck=-0.01)
	if(se==T) arrows(yrs, LPT+LPTse, yrs, LPT-LPTse ,code=3,angle=90,length=0.05)
	if(se) arrows(yrs, LPT+LPTse, yrs, LPT-LPTse ,code=3,angle=90,length=0.05)
	if(moving.avg)lines(yrs[-(1:2)],rmLPT[!is.na(rmLPT)],lty=3,col='red',lwd=3)
	if(moving.median) lines(yrs,runmed(LPT,3),lty=2,col='green',lwd=3)

	if(ref.points){
		abline(h=median(LPT[1:14])*0.8,col='blue',lwd=2)
		#text(max(yrs)+.5,median(LPT[1:14]*0.8)*.85,"Upper Stock Reference",col=rgb(0,0,1,0.5),pos=2,cex=0.8)
	print(median(LPT[1:14]*0.8))
	}

	return(list(Mean = LPT, yrs = yrs, moving.median = runmed(LPT,3), moving.mean = rmLPT ))
	if(add.legend){with(legend.specs)

	}
	if(graphic!='R'){
		print(paste('Plot is saved in',file.path( project.figuredirectory('bio.lobster'), paste0(fn,".pdf")),sep=""))
		dev.off()
	}
}
