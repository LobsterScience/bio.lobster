#' @export
plotSurveyIndex<-function(trend.dat,moving.avg=T,moving.median=T,ref.points=T,index.stations=T,fn="SurveyIndex",wd=8,ht=6,se=F){

	## Plot Survey Index Figure 4
	
	if(index.stations){
		Stns<-read.csv(file.path(project.datadirectory('bio.lobster'),'data',"survey32Stations.csv"))
		trend.dat<-subset(trend.dat,SID%in%Stns$SID)
		}

	LPT   <- with(trend.dat,tapply(NUM_STANDARDIZED,YEAR,mean,na.rm=T))
	yrs   <- as.numeric(names(LPT))
	LPTsd <- with(trend.dat,tapply(NUM_STANDARDIZED,YEAR,sd,na.rm=T))
	LPTn  <- with(trend.dat,tapply(NUM_STANDARDIZED,YEAR,length))
	LPTse <- LPTsd/sqrt(LPTn)
	rmLPT <- mavg(LPT)

	
	pdf(file.path( project.figuredirectory("lobster"), paste0(fn,".pdf")),wd,ht)

	plot(yrs,LPT,pch=16,ylim=c(0,max(LPT+LPTse)),xlab='',ylab='Mean N / Standard Tow',las=1)
	axis(1,yrs,lab=F,tck=-0.01)
	if(se==T) arrows(yrs, LPT+LPTse, yrs, LPT-LPTse ,code=3,angle=90,length=0.05)
	if(se) arrows(yrs, LPT+LPTse, yrs, LPT-LPTse ,code=3,angle=90,length=0.05)
	if(moving.avg)lines(yrs[-(1:2)],rmLPT[!is.na(rmLPT)],lty=1,col='orange',lwd=2)
	if(moving.median) lines(yrs,runmed(LPT,3),lty=2,col='green',lwd=2)

	if(ref.points){
		abline(h=median(LPT[2:15]*0.8),col=rgb(0,0,1,0.5))
		text(max(yrs)+.5,median(LPT[2:15]*0.8)*.85,"Upper Stock Reference",col=rgb(0,0,1,0.5),pos=2,cex=0.8)
	}
	print(paste('Plot is saved in',file.path( project.figuredirectory('bio.lobster'), paste0(fn,".pdf")),sep=""))
	dev.off()
}
