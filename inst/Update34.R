####### Update 34: May 2015
loadfunctions('lobster')

	## Landings

	AnnualSlip<-read.csv(file.path(project.datadirectory('lobster'),"data","Commercial","AnnualSlipLand.csv"))
	Historic<-read.delim(file.path(project.datadirectory('lobster'),"data","Commercial","LFA34_Landings_1892-2004.txt"))
	SeasonalSlip<-read.csv(file.path(project.datadirectory('lobster'),"data","Commercial","SeasonalSlipLand.csv"))


	Annual34<-rbind(subset(Historic,YEAR<1947), subset(AnnualSlip,select=c("YEAR","LFA34")))
	SeasonalSlip$YEAR<-as.numeric(substr(SeasonalSlip$SEASON,6,9))
	Seasonal34<-subset(SeasonalSlip,YEAR<2015,c("SEASON","LFA34","YEAR"))

	require(ggplot2)

	# Plot Landings (1892-present) Figure 2
	pdf(file.path( project.datadirectory("lobster"), "R","LFA34Landings.pdf"),8,6)
	ggplot(Annual34,aes(YEAR,LFA34)) + geom_bar(data=Annual34,fill=rgb(0,0,1,0.5),stat='identity') +
		geom_line(data=Seasonal34,colour='red',size=1) +
		scale_y_continuous(breaks=seq(0, 25000, 2000)) + scale_x_continuous(breaks=seq(1890, 2015, 5)) +
		theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5,size=10, colour='black'),panel.background=element_rect(fill="white", colour='black'),panel.border=element_rect(,fill=NA,colour='black'),axis.text.y=element_text(size=10, colour='black'),
					axis.title.y= element_text(size=15)) + xlab('') + ylab('Landings (t)')
	dev.off()

	## Commercial catch rate index
	LFA34logData<-read.delim(file.path(project.datadirectory('lobster'),"data","Commercial","LFA34_CPUE_Data_2015.05.12.txt"))
	LFA34logData<-subset(LFA34logData,SYEAR!="OOS"&SYEAR<2015)
	LFA34logData$WEIGHT_KG<-LFA34logData$WEIGHT_LBS*0.4536

	catch<-with(LFA34logData,tapply(WEIGHT_KG,SYEAR,sum))
	effort<-with(LFA34logData,tapply(NUM_OF_TRAPS,SYEAR,sum))
	n<-with(LFA34logData,tapply(NUM_OF_TRAPS,SYEAR,length))

	cpueLFA34.dat<-data.frame(year=sort(unique(LFA34logData$SYEAR)),n=n,catch=catch,effort=effort,cpue=catch/effort)
	write.csv(cpueLFA34.dat,file.path(project.datadirectory('lobster'),'data',"LFA34CPUE.csv"),row.names=F)

	KgPTH<- cpueLFA34.dat$cpue
	yrs<-sort(as.numeric(cpueLFA34.dat$year))
	rmKgPTH<-mavg(KgPTH)

	# Plot Commercial CPUE Figure 3

	pdf(file.path( project.datadirectory("lobster"), "R","LFA34CommercialCPUE.pdf"),8,6)
	plot(yrs,KgPTH,pch=16,ylim=c(0,max(KgPTH)),xlab='',ylab='CPUE (Kg/TH)',las=1, main="LFA 34 - Commercial Log CPUE",xaxt='n')
	#axis(1)
	#axis(1,yrs,lab=F,tck=-0.01)
	axis(1,yrs,lab=paste(yrs-1,substr(yrs,3,4),sep='-'),las=2)
	#arrows(yrs, KgPTH+KgPTHse, yrs, KgPTH-KgPTHse ,code=3,angle=90,length=0.1)
	lines(yrs[-(1:2)],rmKgPTH[!is.na(rmKgPTH)],lty=2,col='orange',lwd=2)
	abline(h=median(KgPTH[1:11]*0.8),col=rgb(0,0,1,0.5))
	text(max(yrs)+.5,median(KgPTH[2:15]*0.8)*.85,"Upper Stock Reference",col=rgb(0,0,1,0.5),pos=2,cex=0.8)
	dev.off()
	
	pdf(file.path( project.datadirectory("lobster"), "R","LFA34CommercialCPUEandEffort.pdf"),5,8)
	par(mfrow=c(2,1))
	plot(yrs,KgPTH,pch=16,ylim=c(0,max(KgPTH)),xlab='',ylab='CPUE (Kg/TH)',las=1, main="LFA 34 - Commercial Log CPUE",xaxt='n')
	#axis(1)
	#axis(1,yrs,lab=F,tck=-0.01)
	axis(1,yrs,lab=paste(yrs-1,substr(yrs,3,4),sep='-'),las=2)
	#arrows(yrs, KgPTH+KgPTHse, yrs, KgPTH-KgPTHse ,code=3,angle=90,length=0.1)
	lines(yrs[-(1:2)],rmKgPTH[!is.na(rmKgPTH)],lty=2,col='orange',lwd=2)
	abline(h=median(KgPTH[1:11]*0.8),col=rgb(0,0,1,0.5))
	text(max(yrs)+.5,median(KgPTH[2:15]*0.8)*.85,"Upper Stock Reference",col=rgb(0,0,1,0.5),pos=2,cex=0.8)
	plot(yrs,cpueLFA34.dat$effort/10^6,pch=16,ylim=c(0,30),xlab='',ylab='Trap Hauls (millions)',las=1, main="LFA 34 - Commercial Log Effort",xaxt='n',type='b')
	axis(1,yrs,lab=paste(yrs-1,substr(yrs,3,4),sep='-'),las=2)
	dev.off()

	## ITQ-ILTS Survey
	
	surveyLobsters34<-LobsterSurveyProcess(lfa="34",yrs=1996:2015,mths=c("Jul","Jun"),bin.size=5)

	## Plot Survey Index Figure 4
	plotSurveyIndex(surveyLobsters34)

	# get data
	#surveyLobsters<-read.csv(file.path(project.datadirectory('lobster'),"data","surveyLobsters.csv")) 
	#surveyLobsters34<-subset(surveyLobsters,!is.na(NUM_STANDARDIZED)&LFA==34&HAULCCD_ID==1&YEAR>1995&MONTH%in%c("Jul","Jun"))

	## STATIONS assigned based on proximity
	#ITQspat34<-subset(surveyLobsters34,select=c("SET_ID","SET_LONG","SET_LAT","HAUL_LONG","HAUL_LAT","STATION"))
	#names(ITQspat34)[2:5]<-c("X1","Y1","X2","Y2")
	#ITQspat34$EID<-1:nrow(ITQspat34)
	#pdf(file.path( project.datadirectory("lobster"), "R","LFA34ITQSurveyStations.pdf"),8,11)
	#ITQspat34ns<-assignStation(ITQspat34,lines=T)
	#dev.off()
	##write.csv(ITQspat34ns$events,file.path(project.datadirectory('lobster'),'data',"surveyTows.csv"),row.names=F)
	##write.csv(ITQspat34ns$stations,file.path(project.datadirectory('lobster'),'data',"surveyStations.csv"),row.names=F)

	## add assigned stations to data
	#surveyLobsters34<-merge(surveyLobsters34,subset(ITQspat34ns$events,select=c("SET_ID","SID")),all=T)

	## create a reduced station list for stations that were sampled in at least 15 of 19 year (red) and also in the last two years (green)
	#unlist(lapply(with(surveyLobsters34,tapply(YEAR,SID,unique)),length))->stnreps
	#redStns<-which(stnreps>14)
	#greenStns<-redStns[redStns%in%subset(surveyLobsters34,YEAR>2012)$SID]
	#write.csv(subset(ITQspat34ns$stations,SID%in%greenStns),file.path(project.datadirectory('lobster'),'data',"survey32Stations.csv"),row.names=F)
	#write.csv(subset(ITQspat34ns$stations,SID%in%redStns),file.path(project.datadirectory('lobster'),'data',"surveyStations.csv"),row.names=F)
	#greenStns<-read.csv(file.path(project.datadirectory('lobster'),'data',"survey32Stations.csv"))$SID
	#
	## Plot Map of Stations Figure 5
	##ITQpolys<-read.csv(file.path(project.datadirectory('lobster'),'data',"ITQareas.csv"))
	#pdf(file.path( project.datadirectory("lobster"), "R","LFA34ITQ25SurveyStations.pdf"),8,11)
	#LobsterMap('34',mapRes="UR",title=paste("LFA 34 ITQ Survey Reduced Stations",i),isobath=seq(50,500,50),bathcol=rgb(0,0,1,0.2),bathy.source='bathy')
	##points(Y~X,subset(ITQspat34ns$stations,SID%in%redStns),pch=1,cex=1.2,col='red')#,col=rgb(0,0,0,0.5))
	##points(Y~X,subset(ITQspat34ns$stations,SID%in%redStns),pch=16,cex=0.7,col='red')#,col=rgb(0,0,0,0.5))
	#points(Y~X,subset(ITQspat34ns$stations,SID%in%greenStns),pch=1,cex=1.2,col='red')#,col=rgb(0,0,0,0.5))
	#points(Y~X,subset(ITQspat34ns$stations,SID%in%greenStns),pch=16,cex=0.7,col='red')#,col=rgb(0,0,0,0.5))
	##addPolys(ITQpolys,border='grey')
	#dev.off()


	## Plot Survey Index Figure 4

	#trend.dat<-surveyLobsters34


	#LPT<- with(trend.dat,tapply(NUM_STANDARDIZED,YEAR,mean,na.rm=T))
	#yrs<-as.numeric(names(LPT))
	#LPTsd<- with(trend.dat,tapply(NUM_STANDARDIZED,YEAR,sd,na.rm=T))
	#LPTn<- with(trend.dat,tapply(NUM_STANDARDIZED,YEAR,length))
	#LPTse<-LPTsd/sqrt(LPTn)
	#ma<-function(x,n=3){filter(x,rep(1/n,n),sides=2)}
	#rmLPT<-ma(LPT)

	##pdf(file.path( project.datadirectory("lobster"), "R","LFA34LobsterAbundanceTrend.pdf"),8,6)

	#plot(yrs,LPT,pch=16,ylim=c(0,max(LPT+LPTse)),xlab='',ylab='Mean N / Standard Tow',las=1, main="LFA 34 - ITQ Survey")
	#axis(1,yrs,lab=F,tck=-0.01)
	#arrows(yrs, LPT+LPTse, yrs, LPT-LPTse ,code=3,angle=90,length=0.1)
	#lines(yrs[-(1:2)],rmLPT[!is.na(rmLPT)],lty=2,col='orange',lwd=2)
	#abline(h=median(LPT[2:15]*0.8),col=rgb(0,0,1,0.5))
	#text(max(yrs)+.5,median(LPT[2:15]*0.8)*.85,"Upper Stock Reference",col=rgb(0,0,1,0.5),pos=2,cex=0.8)

	#redstn.trend.dat<-subset(surveyLobsters34,SID%in%greenStns)
	#
	#LPT<- with(redstn.trend.dat,tapply(NUM_STANDARDIZED,YEAR,mean,na.rm=T))
	#yrs<-as.numeric(names(LPT))
	#LPTsd<- with(redstn.trend.dat,tapply(NUM_STANDARDIZED,YEAR,sd,na.rm=T))
	#LPTn<- with(redstn.trend.dat,tapply(NUM_STANDARDIZED,YEAR,length))
	#LPTse<-LPTsd/sqrt(LPTn)
	#rmLPT<-ma(LPT)
	##write.csv(data.frame(SYEAR=yrs,N=LPTn,LPT=LPT,LPT.SE=LPTse),file.path(project.datadirectory('lobster'),'data',"LFA34SurveyIndex.csv"),row.names=F)

	#plot(yrs,LPT,pch=16,ylim=c(0,max(LPT+LPTse)),xlab='',ylab='Mean N / Standard Tow',las=1, main="LFA 34 - ITQ Survey - Reduced Stations")
	#axis(1,yrs,lab=F,tck=-0.01)
	#arrows(yrs, LPT+LPTse, yrs, LPT-LPTse ,code=3,angle=90,length=0.1)
	#lines(yrs[-(1:2)],rmLPT[!is.na(rmLPT)],lty=2,col='orange',lwd=2)
	#abline(h=median(LPT[2:15]*0.8),col=rgb(0,0,1,0.5))
	#text(max(yrs)+.5,median(LPT[2:15]*0.8)*.85,"Upper Stock Reference",col=rgb(0,0,1,0.5),pos=2,cex=0.8)

#	dev.off()

	####### EXTRAS

	# Length Composition
	
	# Lobster Survey
	CLFsurvey<-list()
	CLFsurvey$LobsterSurvey<-t(sapply(2005:2015,function(y){colMeans(subset(surveyLobsters34,YEAR==y,paste0("CL",seq(0,215,5))),na.rm=T)}))
	
	# Scallop Survey
	SCALSURV34.dat<-ScallopSurveyProcess(SPA=c("3","29"))
	CLFsurvey$ScallopSurvey<-t(sapply(2005:2015,function(y){colMeans(subset(SCALSURV34.dat,YEAR==y,paste0("CL",seq(5,220,5))),na.rm=T)}))
	#BubblePlotCLF(CLFsurvey,inch=0.2,bg=rgb(0,1,0,0.1),yrs=2005:2015,bins=seq(0,220,5),filen="SurveyLFA34",prop=lT)
	BarPlotCLF(CLFsurvey,yrs=2005:2015,bins=seq(0,220,5),col='grey',filen="SurveyLFA34",rel=F,ymax=c(18.5,.6))

	# at sea sampling
	lobster.db('atSea')
	atSea.LFA34.dat<-addSYEAR(subset(atSea,LFA==34))
	atSea.LFA34.dat$YEAR<-year(atSea.LFA34.dat$SDATE)
	#CLF(subset(atSea.LFA34.dat,!is.na(SYEAR),c("SYEAR","CARLENGTH")))
	LFA34lf<-CLF(subset(atSea.LFA34.dat,!is.na(YEAR),c("YEAR","CARLENGTH")),yrs=1982:2014)
	BubblePlotCLF(LFA34lf,inch=0.2,bg=rgb(0,1,0,0.1),prop=T,filen="SeaSamplingLFA34",yrs=1982:2014)
	LFA34lf<-CLF(subset(atSea.LFA34.dat,!is.na(YEAR),c("YEAR","CARLENGTH")),yrs=2005:2014)
	BarPlotCLF(LFA34lf,yrs=2005:2014,bins=seq(0,220,5),col='grey',filen="SeaSamplingLFA34",rel=T)

	# port sampling
	lobster.db('port')
	port34<-subset(port,LFA==34)



	######## Distribution plots ######################################
	#
	# lobster distribution by year in LFA 34 from survey
	#

	pdf(file.path( project.datadirectory("lobster"), "R","LFA34LobsterDensity.pdf"),8,11)

	for(i in 2005:2013){
		
		# interpolate abundance
		interp.data<-na.omit(subset(surveyLobsters34,YEAR==i,c('SET_ID','SET_LONG','SET_LAT','NUM_STANDARDIZED')))
		lob.contours<-interpolation(interp.data,ticks='define',place=3,nstrata=5,str.min=0,interp.method='gstat',blank=T,res=0.005,smooth=F,idp=3.5,blank.dist=0.2)

		# define contour lines
		print(lob.contours$str.def)
		lvls=c(1, 5, 10, 20, 50, 100, 200, 500)

		# generate contour lines
		cont.lst<-contour.gen(lob.contours$image.dat,lvls,subset(LFAs,LFA==34),col="YlGn",colorAdj=1)

		# plot Map
		LobsterMap('34',mapRes="UR",contours=cont.lst,title=paste("LFA 34 Lobster Density",i),isobath=seq(50,500,50),bathcol=rgb(0,0,1,0.2),bathy.source='bathy')
		points(SET_LAT~SET_LONG,surveyLobsters,subset=YEAR==i,pch=16,cex=0.5)#,col=rgb(0,0,0,0.5))
		ContLegend("bottomright",lvls=lvls,Cont.data=cont.lst$Cont.data,title="#/standard tow",inset=0.02,cex=0.8,bg='white')
	}
	#dev.off()

	######## seperate July and Sept for 2014

	surveyLobsters$MONTH<-month(surveyLobsters$BOARD_DATE,T)
	#pdf(file.path( project.datadirectory("lobster"), "R","LFA34LobsterDensity2014.pdf"),8,11)

	for(i in c("Jul","Sep")){
		
		# interpolate abundance
		interp.data<-na.omit(subset(surveyLobsters,LFA==34&HAULCCD_ID==1&YEAR==2014&MONTH==i,c('SET_ID','SET_LONG','SET_LAT','NUM_STANDARDIZED')))
		lob.contours<-interpolation(interp.data,ticks='define',place=3,nstrata=5,str.min=0,interp.method='gstat',blank=T,blank.type=1,res=0.01,smooth=F,idp=3.5,blank.dist=0.2)

		# define contour lines
		print(lob.contours$str.def)
		lvls=c(1, 5, 10, 20, 50, 100, 200, 500)

		# generate contour lines
		cont.lst<-contour.gen(lob.contours$image.dat,lvls,col="YlGn",colorAdj=1)

		# plot Map
		LobsterMap('34',mapRes="UR",contours=cont.lst,title=paste("LFA 34 Lobster Density",i,"2014"),isobath=seq(50,500,50),bathcol=rgb(0,0,1,0.2),bathy.source='bathy')
		points(SET_LAT~SET_LONG,interp.data,pch=16,cex=0.5)#,col=rgb(0,0,0,0.5))
		ContLegend("bottomright",lvls=lvls,Cont.data=cont.lst$Cont.data,title="#/standard tow",inset=0.02,cex=0.8,bg='white')
	}
		# interpolate abundance
		interp.data<-na.omit(subset(surveyLobsters34,YEAR==2015,c('SET_ID','SET_LONG','SET_LAT','NUM_STANDARDIZED')))
		lob.contours<-interpolation(interp.data,ticks='define',place=3,nstrata=5,str.min=0,interp.method='gstat',blank=T,res=0.005,smooth=F,idp=3.5,blank.dist=0.2)

		# define contour lines
		print(lob.contours$str.def)
		lvls=c(1, 5, 10, 20, 50, 100, 200, 500)

		# generate contour lines
		cont.lst<-contour.gen(lob.contours$image.dat,lvls,subset(LFAs,LFA==34),col="YlGn",colorAdj=1)

		# plot Map
		LobsterMap('34',mapRes="UR",contours=cont.lst,title=paste("LFA 34 Lobster Density",i),isobath=seq(50,500,50),bathcol=rgb(0,0,1,0.2),bathy.source='bathy')
		points(SET_LAT~SET_LONG,surveyLobsters,subset=YEAR==2015,pch=16,cex=0.5)#,col=rgb(0,0,0,0.5))
		ContLegend("bottomright",lvls=lvls,Cont.data=cont.lst$Cont.data,title="#/standard tow",inset=0.02,cex=0.8,bg='white')

	dev.off()

	## berried females
	pdf(file.path( project.datadirectory("lobster"), "R","LFA34BerriedFemaleDensity.pdf"),8,11)

	for(i in 2005:2013){
		
		# interpolate abundance
		interp.data<-na.omit(subset(surveyLobsters,LFA==34&YEAR==i,c('SET_ID','SET_LONG','SET_LAT','BERRIED_FEMALES')))
		lob.contours<-interpolation(interp.data,ticks='define',place=3,nstrata=5,str.min=0,interp.method='gstat',blank=T,res=0.005,smooth=F,idp=3.5,blank.dist=0.2)

		# define contour lines
		print(lob.contours$str.def)
		lvls=c(0.5, 1, 2, 3, 4, 5)

		# generate contour lines
		cont.lst<-contour.gen(lob.contours$image.dat,lvls,subset(LFAs,LFA==34),col="YlGn",colorAdj=1)

		# plot Map
		LobsterMap('34',mapRes="UR",contours=cont.lst,title=paste("LFA 34 Lobster Density",i),isobath=seq(50,500,50),bathcol=rgb(0,0,1,0.2),bathy.source='bathy')
		points(SET_LAT~SET_LONG,surveyLobsters,subset=YEAR==i,pch=16,cex=0.5)#,col=rgb(0,0,0,0.5))
		ContLegend("bottomright",lvls=lvls,Cont.data=cont.lst$Cont.data,title="#/standard tow",inset=0.02,cex=0.8,bg='white')
	}
	#dev.off()

	######## seperate July and Sept for 2014

	surveyLobsters$MONTH<-month(surveyLobsters$BOARD_DATE,T)
	#pdf(file.path( project.datadirectory("lobster"), "R","LFA34LobsterDensity2014.pdf"),8,11)

	for(i in c("Jul","Sep")){
		
		# interpolate abundance
		interp.data<-na.omit(subset(surveyLobsters,LFA==34&YEAR==2014&MONTH==i,c('SET_ID','SET_LONG','SET_LAT','BERRIED_FEMALES')))
		lob.contours<-interpolation(interp.data,ticks='define',place=3,nstrata=5,str.min=0,interp.method='gstat',blank=T,blank.type=1,res=0.01,smooth=F,idp=3.5,blank.dist=0.2)

		# define contour lines
		print(lob.contours$str.def)
		lvls=c(0.5, 1, 2, 3, 4, 5)

		# generate contour lines
		cont.lst<-contour.gen(lob.contours$image.dat,lvls,col="YlGn",colorAdj=1)

		# plot Map
		LobsterMap('34',mapRes="UR",contours=cont.lst,title=paste("LFA 34 Lobster Density",i,"2014"),isobath=seq(50,500,50),bathcol=rgb(0,0,1,0.2),bathy.source='bathy')
		points(SET_LAT~SET_LONG,interp.data,pch=16,cex=0.5)#,col=rgb(0,0,0,0.5))
		ContLegend("bottomright",lvls=lvls,Cont.data=cont.lst$Cont.data,title="#/standard tow",inset=0.02,cex=0.8,bg='white')
	}
	dev.off()


	
	# FSRS CPUE
	FSRScpue.dat<-read.csv(file.path( project.datadirectory("lobster"), "data","FSRScpue.csv"))
	FSRScpue.dat$subarea[FSRScpue.dat$LFA=='34']<-FSRScpue.dat$LFA[FSRScpue.dat$LFA=='34']
	FSRScpue34.dat<-reshape( subset(FSRScpue.dat,LFA=='34',c("SYEAR","pred.s.cpue","pred.l.cpue")),varying=list(c("pred.s.cpue","pred.l.cpue")),direction='long',timevar='Size',times=c("sublegal","legal"))
	xyplot(pred.s.cpue~SYEAR|Size, data=FSRScpue34.dat, ylab="CPUE (No. Lobsters / Trap Haul)",xlab= "Year", as.table=T,type='b',ylim=c(0,7))
	pdf(file.path( project.datadirectory("lobster"), "R","LFA34sublegalFSRS.pdf"),8,5)
	xyplot(pred.s.cpue~SYEAR|LFA, data=subset(FSRScpue.dat,subarea%in%c('33 West','34')), ylab="CPUE (No. Lobsters / Trap Haul)",xlab= "Year", as.table=T,type='b',ylim=c(0,7))
	dev.off()

##-- Update 35-38

	## Commercial catch rate index
	LFA3538logData<-read.delim(file.path(project.datadirectory('lobster'),"data","Commercial","LFA35-38CPUE_2015.05.19.txt"))
	LFA3538logData<-subset(LFA3538logData,SYEAR!="OOS"&SYEAR%in%2006:2014)
	LFA3538logData$WEIGHT_KG<-LFA3538logData$WEIGHT_LBS*0.4536

	catch<-with(LFA3538logData,tapply(WEIGHT_KG,SYEAR,sum))
	effort<-with(LFA3538logData,tapply(NUM_OF_TRAPS,SYEAR,sum))
	n<-with(LFA3538logData,tapply(NUM_OF_TRAPS,SYEAR,length))

	cpueLFA3538.dat<-data.frame(year=sort(unique(LFA3538logData$SYEAR)),n=n,catch=catch,effort=effort,cpue=catch/effort)
	write.csv(cpueLFA3538.dat,file.path(project.datadirectory('lobster'),'data',"LFA3538CPUE.csv"),row.names=F)

	KgPTH<- cpueLFA3538.dat$cpue
	yrs<-sort(as.numeric(cpueLFA3538.dat$year))
	rmKgPTH<-ma(KgPTH)

	#pdf(file.path( project.datadirectory("lobster"), "R","LFA3538CommercialCPUE.pdf"),8,6)
	plot(yrs,KgPTH,pch=16,ylim=c(0,max(KgPTH)),xlab='',ylab='CPUE (Kg/TH)',las=1, main="LFA 35 to 38 - Commercial Log CPUE",xaxt='n')
	axis(1,yrs,lab=paste(yrs-1,substr(yrs,3,4),sep='-'),las=2)
	lines(yrs[-(1:2)],rmKgPTH[!is.na(rmKgPTH)],lty=2,col='orange',lwd=2)
	abline(h=median(KgPTH[1:4]*0.5),col=rgb(0,0,1,0.5))
	text(max(yrs),median(KgPTH[1:4]*0.5)*.85,"Upper Stock Reference",col=rgb(0,0,1,0.5),pos=2,cex=0.8)
	dev.off()

