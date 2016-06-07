####### Update 35: May 2015
loadfunctions('lobster')

	## Landings

AnnualSlip<-lobster.db( DS="annual.landings.redo")
SeasonalSlip<-lobster.db( DS="seasonal.landings.redo")
#Historic<-lobster.db( DS="historical.landings.redo")

AnnualSlip<-lobster.db( DS="annual.landings")
SeasonalSlip<-lobster.db( DS="seasonal.landings")
#Historic<-lobster.db( DS="historical.landings")


	Annual35<-subset(AnnualSlip,select=c("YEAR","LFA35"))
	SeasonalSlip$YEAR<-as.numeric(substr(SeasonalSlip$SEASON,6,9))
	Seasonal35<-subset(SeasonalSlip,select=c("SEASON","LFA35","YEAR"))

	require(ggplot2)

	# Plot Landings (1892-present) Figure 2
	pdf(file.path( project.datadirectory("lobster"), "R","LFA35Landings.pdf"),8,6)
	ggplot(Annual35,aes(YEAR,LFA35)) + geom_bar(data=Annual35,fill=rgb(0,0,1,0.5),stat='identity') +
		geom_line(data=Seasonal35,colour='red',size=1) +
		scale_y_continuous(breaks=seq(0, 25000, 2000)) + scale_x_continuous(breaks=seq(1890, 2015, 5)) +
		theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5,size=10, colour='black'),panel.background=element_rect(fill="white", colour='black'),panel.border=element_rect(,fill=NA,colour='black'),axis.text.y=element_text(size=10, colour='black'),
					axis.title.y= element_text(size=15)) + xlab('') + ylab('Landings (t)')
	dev.off()

	## Commercial catch rate index
	LFA35logData<-read.delim(file.path(project.datadirectory('lobster'),"data","Commercial","LFA35_CPUE_Data_2015.05.12.txt")) # from Cheryl

				logDat <- lobster.db('process.logs')
				logDat <- subset(logDat,SYEAR %in% 2003:(p$current.assessment.year-1) & LFA %in% c(35,36,38))
		
				catch.new  <- with(logDat,tapply(WEIGHT_KG,SYEAR,sum))
				effort.new <- with(logDat,tapply(NUM_OF_TRAPS,SYEAR,sum))
				n.new      <- with(logDat,tapply(NUM_OF_TRAPS,SYEAR,length))

				cpueLFA3538.dat <- data.frame(year=2004:(p$current.assessment.year-1),n=c(n,n.new),catch=c(catch,catch.new),effort=c(effort,effort.new),cpue=c(catch/effort,catch.new/effort.new))






	LFA35logData<-subset(LFA35logData,SYEAR!="OOS"&SYEAR<2015)
	LFA35logData$WEIGHT_KG<-LFA35logData$WEIGHT_LBS*0.4536

	catch<-with(LFA35logData,tapply(WEIGHT_KG,SYEAR,sum))
	effort<-with(LFA35logData,tapply(NUM_OF_TRAPS,SYEAR,sum))
	n<-with(LFA35logData,tapply(NUM_OF_TRAPS,SYEAR,length))

	cpueLFA35.dat<-data.frame(year=sort(unique(LFA35logData$SYEAR)),n=n,catch=catch,effort=effort,cpue=catch/effort)
	write.csv(cpueLFA35.dat,file.path(project.datadirectory('lobster'),'data',"LFA35CPUE.csv"),row.names=F)

	KgPTH<- cpueLFA35.dat$cpue
	yrs<-sort(as.numeric(cpueLFA35.dat$year))
	rmKgPTH<-ma(KgPTH)

	# Plot Commercial CPUE Figure 3

	pdf(file.path( project.datadirectory("lobster"), "R","LFA35CommercialCPUE.pdf"),8,6)
	plot(yrs,KgPTH,pch=16,ylim=c(0,max(KgPTH)),xlab='',ylab='CPUE (Kg/TH)',las=1, main="LFA 35 - Commercial Log CPUE",xaxt='n')
	#axis(1)
	#axis(1,yrs,lab=F,tck=-0.01)
	axis(1,yrs,lab=paste(yrs-1,substr(yrs,3,4),sep='-'),las=2)
	#arrows(yrs, KgPTH+KgPTHse, yrs, KgPTH-KgPTHse ,code=3,angle=90,length=0.1)
	lines(yrs[-(1:2)],rmKgPTH[!is.na(rmKgPTH)],lty=2,col='orange',lwd=2)
	abline(h=median(KgPTH[1:11]*0.8),col=rgb(0,0,1,0.5))
	text(max(yrs)+.5,median(KgPTH[2:15]*0.8)*.85,"Upper Stock Reference",col=rgb(0,0,1,0.5),pos=2,cex=0.8)
	dev.off()


	# Scallop Survey
	SCALSURV35.dat<-ScallopSurveyProcess(SPA=c("1","29"))
	CLFsurvey$ScallopSurvey<-t(sapply(2005:2014,function(y){colMeans(subset(SCALSURV35.dat,YEAR==y,paste0("CL",seq(5,220,5))),na.rm=T)}))
	BubblePlotCLF(CLFsurvey,inch=0.2,bg=rgb(0,1,0,0.1),yrs=2005:2014,bins=seq(0,220,5),filen="SurveyLFA35",prop=lT)
	BarPlotCLF(CLFsurvey,yrs=2005:2014,bins=seq(0,220,5),col='grey',filen="SurveyLFA35",rel=F,ymax=c(25,1))

	# at sea sampling
	lobster.db('atSea')
	atSea.LFA35.dat<-addSYEAR(subset(atSea,LFA==35))
	atSea.LFA35.dat$YEAR<-year(atSea.LFA35.dat$SDATE)
	#CLF(subset(atSea.LFA35.dat,!is.na(SYEAR),c("SYEAR","CARLENGTH")))
	LFA35lf<-CLF(subset(atSea.LFA35.dat,!is.na(YEAR),c("YEAR","CARLENGTH")),yrs=1982:2014)
	BubblePlotCLF(LFA35lf,inch=0.2,bg=rgb(0,1,0,0.1),prop=T,filen="SeaSamplingLFA35",yrs=1982:2014)
	LFA35lf<-CLF(subset(atSea.LFA35.dat,!is.na(YEAR),c("YEAR","CARLENGTH")),yrs=2005:2014)
	BarPlotCLF(LFA35lf,yrs=2005:2014,bins=seq(0,220,5),col='grey',filen="SeaSamplingLFA35",rel=T)

	# port sampling
	lobster.db('port')
	port35<-subset(port,LFA==35)



	######## Distribution plots ######################################
	#
	# lobster distribution by year in LFA 35 from survey
	#

	pdf(file.path( project.datadirectory("lobster"), "R","LFA35LobsterDensity.pdf"),8,11)

	for(i in 2005:2013){
		
		# interpolate abundance
		interp.data<-na.omit(subset(surveyLobsters35,YEAR==i,c('SET_ID','SET_LONG','SET_LAT','NUM_STANDARDIZED')))
		lob.contours<-interpolation(interp.data,ticks='define',place=3,nstrata=5,str.min=0,interp.method='gstat',blank=T,res=0.005,smooth=F,idp=3.5,blank.dist=0.2)

		# define contour lines
		print(lob.contours$str.def)
		lvls=c(1, 5, 10, 20, 50, 100, 200, 500)

		# generate contour lines
		cont.lst<-contour.gen(lob.contours$image.dat,lvls,subset(LFAs,LFA==35),col="YlGn",colorAdj=1)

		# plot Map
		LobsterMap('35',mapRes="UR",contours=cont.lst,title=paste("LFA 35 Lobster Density",i),isobath=seq(50,500,50),bathcol=rgb(0,0,1,0.2),bathy.source='bathy')
		points(SET_LAT~SET_LONG,surveyLobsters,subset=YEAR==i,pch=16,cex=0.5)#,col=rgb(0,0,0,0.5))
		ContLegend("bottomright",lvls=lvls,Cont.data=cont.lst$Cont.data,title="#/standard tow",inset=0.02,cex=0.8,bg='white')
	}
	#dev.off()

	######## seperate July and Sept for 2014

	surveyLobsters$MONTH<-month(surveyLobsters$BOARD_DATE,T)
	#pdf(file.path( project.datadirectory("lobster"), "R","LFA35LobsterDensity2014.pdf"),8,11)

	for(i in c("Jul","Sep")){
		
		# interpolate abundance
		interp.data<-na.omit(subset(surveyLobsters,LFA==35&HAULCCD_ID==1&YEAR==2014&MONTH==i,c('SET_ID','SET_LONG','SET_LAT','NUM_STANDARDIZED')))
		lob.contours<-interpolation(interp.data,ticks='define',place=3,nstrata=5,str.min=0,interp.method='gstat',blank=T,blank.type=1,res=0.01,smooth=F,idp=3.5,blank.dist=0.2)

		# define contour lines
		print(lob.contours$str.def)
		lvls=c(1, 5, 10, 20, 50, 100, 200, 500)

		# generate contour lines
		cont.lst<-contour.gen(lob.contours$image.dat,lvls,col="YlGn",colorAdj=1)

		# plot Map
		LobsterMap('35',mapRes="UR",contours=cont.lst,title=paste("LFA 35 Lobster Density",i,"2014"),isobath=seq(50,500,50),bathcol=rgb(0,0,1,0.2),bathy.source='bathy')
		points(SET_LAT~SET_LONG,interp.data,pch=16,cex=0.5)#,col=rgb(0,0,0,0.5))
		ContLegend("bottomright",lvls=lvls,Cont.data=cont.lst$Cont.data,title="#/standard tow",inset=0.02,cex=0.8,bg='white')
	}
	dev.off()

	## berried females
	pdf(file.path( project.datadirectory("lobster"), "R","LFA35BerriedFemaleDensity.pdf"),8,11)

	for(i in 2005:2013){
		
		# interpolate abundance
		interp.data<-na.omit(subset(surveyLobsters,LFA==35&YEAR==i,c('SET_ID','SET_LONG','SET_LAT','BERRIED_FEMALES')))
		lob.contours<-interpolation(interp.data,ticks='define',place=3,nstrata=5,str.min=0,interp.method='gstat',blank=T,res=0.005,smooth=F,idp=3.5,blank.dist=0.2)

		# define contour lines
		print(lob.contours$str.def)
		lvls=c(0.5, 1, 2, 3, 4, 5)

		# generate contour lines
		cont.lst<-contour.gen(lob.contours$image.dat,lvls,subset(LFAs,LFA==35),col="YlGn",colorAdj=1)

		# plot Map
		LobsterMap('35',mapRes="UR",contours=cont.lst,title=paste("LFA 35 Lobster Density",i),isobath=seq(50,500,50),bathcol=rgb(0,0,1,0.2),bathy.source='bathy')
		points(SET_LAT~SET_LONG,surveyLobsters,subset=YEAR==i,pch=16,cex=0.5)#,col=rgb(0,0,0,0.5))
		ContLegend("bottomright",lvls=lvls,Cont.data=cont.lst$Cont.data,title="#/standard tow",inset=0.02,cex=0.8,bg='white')
	}
	#dev.off()

	######## seperate July and Sept for 2014

	surveyLobsters$MONTH<-month(surveyLobsters$BOARD_DATE,T)
	#pdf(file.path( project.datadirectory("lobster"), "R","LFA35LobsterDensity2014.pdf"),8,11)

	for(i in c("Jul","Sep")){
		
		# interpolate abundance
		interp.data<-na.omit(subset(surveyLobsters,LFA==35&YEAR==2014&MONTH==i,c('SET_ID','SET_LONG','SET_LAT','BERRIED_FEMALES')))
		lob.contours<-interpolation(interp.data,ticks='define',place=3,nstrata=5,str.min=0,interp.method='gstat',blank=T,blank.type=1,res=0.01,smooth=F,idp=3.5,blank.dist=0.2)

		# define contour lines
		print(lob.contours$str.def)
		lvls=c(0.5, 1, 2, 3, 4, 5)

		# generate contour lines
		cont.lst<-contour.gen(lob.contours$image.dat,lvls,col="YlGn",colorAdj=1)

		# plot Map
		LobsterMap('35',mapRes="UR",contours=cont.lst,title=paste("LFA 35 Lobster Density",i,"2014"),isobath=seq(50,500,50),bathcol=rgb(0,0,1,0.2),bathy.source='bathy')
		points(SET_LAT~SET_LONG,interp.data,pch=16,cex=0.5)#,col=rgb(0,0,0,0.5))
		ContLegend("bottomright",lvls=lvls,Cont.data=cont.lst$Cont.data,title="#/standard tow",inset=0.02,cex=0.8,bg='white')
	}
	dev.off()


	
	# FSRS CPUE
	FSRScpue.dat<-read.csv(file.path( project.datadirectory("lobster"), "data","FSRScpue.csv"))
	FSRScpue.dat$subarea[FSRScpue.dat$LFA=='35']<-FSRScpue.dat$LFA[FSRScpue.dat$LFA=='35']
	FSRScpue35.dat<-reshape( subset(FSRScpue.dat,LFA=='35',c("SYEAR","pred.s.cpue","pred.l.cpue")),varying=list(c("pred.s.cpue","pred.l.cpue")),direction='long',timevar='Size',times=c("sublegal","legal"))
	xyplot(pred.s.cpue~SYEAR|Size, data=FSRScpue35.dat, ylab="CPUE (No. Lobsters / Trap Haul)",xlab= "Year", as.table=T,type='b',ylim=c(0,7))
	pdf(file.path( project.datadirectory("lobster"), "R","LFA35sublegalFSRS.pdf"),8,5)
	xyplot(pred.s.cpue~SYEAR|LFA, data=subset(FSRScpue.dat,subarea%in%c('33 West','35')), ylab="CPUE (No. Lobsters / Trap Haul)",xlab= "Year", as.table=T,type='b',ylim=c(0,7))
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

