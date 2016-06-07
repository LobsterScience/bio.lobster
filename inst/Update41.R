RLibrary("lubridate","dplyr","ggplot2")

loadfunctions(c('lobster','groundfish','BIOsurvey','utility'))

##______________________##
##                      ##
##   at Sea sampling    ##
##______________________##
##                      ##

	lobster.db('atSea')

	atSea41<-subset(atSea,LFA==41&SPECIESCODE==2550)
	atSea41$YEAR<-year(atSea41$STARTDATE)
	atSea41$EID<-1:nrow(atSea41)
		
		# Look at the data!
		atSeaCLF<-CLF(subset(atSea41,!is.na(YEAR),c("YEAR","CARLENGTH")))
		BubblePlotCLF(atSeaCLF$CLF,inch=0.2,bg=rgb(0,1,0,0.1),prop=T,filen="SeaSamplingLFA41",yrs=atSeaCLF$yrs)
		BarPlotCLF(atSeaCLF$CLF,,yrs=atSeaCLF$yrs,col='grey',filen="SeaSamplingLFA41",rel=T,LS=83,rows=9,sample.size=rowSums(atSeaCLF$CLF$CLF))

	### Map Areas 
	LobsterMap('41')		
	LFA41areas<-read.csv(file.path( project.datadirectory("lobster"), "data","maps","LFA41Offareas.csv"))
	LFA41areasEXT<-read.csv(file.path( project.datadirectory("lobster"), "data","maps","LFA41Offareas_ext.csv"))
	addPolys(LFA41areas,border='red')
	addPolys(LFA41areasEXT,border='grey')



	### Add Areas 
	events<-na.omit(with(atSea41,data.frame(EID=EID,X=LONGITUDE,Y=LATITUDE)))
	key<-findPolys(events,LFA41areasEXT,maxRows=1e+06)
	atSea41<-merge(atSea41,merge(key[,-3],subset(LFA41areas,!duplicated(PID),c("PID","OFFAREA"))),all=T)

	### Add Season
	atSea41$season<-getSeason(atSea41$STARTDATE)
	atSea41$AreaSeason<-paste(atSea41$OFFAREA,atSea41$season,sep='.')

	### Select females
	atSea41F<-subset(atSea41,SEX>1)

	### calculate average size
	#sapply(unique(atSea41$AreaSeason),function(a){with(subset(atSea41F,AreaSeason==a),tapply(CARLENGTH,YEAR,mean,na.rm=T))}) # mean
	sapply(unique(atSea41$AreaSeason),function(a){with(subset(atSea41F,AreaSeason==a),tapply(CARLENGTH,YEAR,median,na.rm=T))}) # median
	medians<-with(subset(atSea41,YEAR%in%(1977:2012)),tapply(CARLENGTH,AreaSeason,median,na.rm=T))
	medians-(medians-95)/2


	medians<-with(atSea41,tapply(CARLENGTH,YEAR,median))
	plot(as.numeric(names(medians)),medians,type='b')
	with(atSea41,tapply(CAPTAIN,YEAR,unique))

	### Map Areas 
	LobsterMap('41',poly.lst=list(LFA41areas,data.frame(PID=1:5,border='red')))		




##______________________##
##                      ##
##       RV Survey      ##
##______________________##
##                      ##


	# Summer Survey index for 4X
	RVS4X.lst<-GroundfishSurveyProcess(Strata=c(477,478,480,481,482,483,484),Years=1980:2015) 
	RVS4Xlgf.lst<-GroundfishSurveyProcess(size.range=c(140,240),Sex=2:3,Strata=c(480:481),Years=2010:2015) 

	mavg(RVS4Xlgf.lst$index)
	
	# Survey index for 4X
	RVS5Z.lst<-GroundfishSurveyProcess(Strata=c('5Z1','5Z2','5Z3','5Z4'),Series=c('georges'),Years=1987:2015) 

	### Map Areas 
	LobsterMap('41')		
	LFA41areas<-read.csv(file.path( project.datadirectory("lobster"), "data","maps","LFA41Offareas.csv"))
	RVstrata<-read.csv(file.path( project.datadirectory("lobster"), "data","maps","summerstrata.csv"))
	RVstrata$POS<-1:nrow(RVstrata)
	RVstrataLabs<-read.csv(file.path( project.datadirectory("lobster"), "data","maps","summer_strata_labels.csv"))
	LobsterMap('41')		
	addPolys(LFA41areas)
	addPolys(RVstrata,border='red')
	addLabels(RVstrataLabs,col=rgb(1,0,0,0.3),cex=1)

	points(LATITUDE~LONGITUDE,atSea41,pch='.')
	
	RVS4X.lst<-GroundfishSurveyProcess(Strata=c(477,478,480,481,482,483,484),Years=1980:2015) 
	#RVS4X.lst<-GroundfishSurveyProcess(Strata=c(477,478,482,483,484),Years=1980:2015) 

	DougsNumbers<-read.csv("DougsNumbers.csv")

	RVindex<-data.frame(YEAR=1980:2015,RV4X=RVS4X.lst$index,RV4Xse=RVS4X.lst$se)
	RVindex$RV4X[RVindex$YEAR<1999]<-DougsNumbers$Stratified.Mean[DougsNumbers$Year<1999]
	RVindex$RV4Xse[RVindex$YEAR<1999]<-DougsNumbers$Standard.Error[DougsNumbers$Year<1999]
	RVindex$MVAvg3<-mavg(RVindex$RV4X)

	bgcol<-rep('darkblue',nrow(RVindex))
	#bgcol[which(RVindex$YEAR%in%(1995:1997))]<-'grey'
	bgcol[which(RVindex$YEAR<1999)]<-'red'

	pdf(file.path( project.datadirectory("lobster"), "R","LFA41updateFig3.pdf"),8,6)
	with(RVindex,plot(YEAR,RV4X,pch=21,col='lightblue',bg=bgcol,xlab='',ylab='Mean # / Tow',las=1,ylim=c(0,max(RV4X+RV4Xse,na.rm=T))))
	with(RVindex,arrows(YEAR, RV4X+RV4Xse, YEAR, RV4X-RV4Xse ,code=3,angle=90,length=0.03))
	with(RVindex,points(YEAR,RV4X,pch=21,col='lightblue',bg=bgcol))
	with(RVindex,lines(YEAR[-1],MVAvg3[-length(YEAR)],lty=3,col='blue',lwd=2))
	lines(1995:2015,rep(1.48,length(1995:2015)),lty=3,col='green',lwd=2)
	lines(1995:2015,rep(0.16,length(1995:2015)),lty=3,col='red',lwd=2)
	legend('topleft',c("3yr Moving Average","50% Median 1995-09","40% Median 1983-94"),col=c('blue','green','red'),lty=c(3,3,3),bty='n',inset=0.02,lwd=2)
	dev.off()




##______________________##
##                      ##
##       	LOGS 	    ##
##______________________##
##                      ##


	lobster.db('logs41')
	logs41$YEAR<-year(logs41$FV_FISHED_DATETIME)

	logs41$SYEAR<-year(logs41$FV_FISHED_DATETIME)
	logs41$LFA<-41
	logs41$WEIGHT_KG<-logs41$ADJCATCH*0.4536
	logs41$DATE_FISHED<-logs41$FV_FISHED_DATETIME


	lbs<-with(logs41,tapply(ADJCATCH,YEAR,sum,na.rm=T))
	names(slip41)<-c("Year","Landings.slip")
	slip41$Landings.slip<-slip41$Landings.slip*0.0004536
	landat<-merge(data.frame(Year=names(lbs),Landings.t=lbs*0.0004536),slip41,all=T)
	
	# Total landings
	pdf(file.path( project.datadirectory("lobster"), "R","LFA41updateFig2.pdf"),8,5)
	with(subset(landat,Year<2015),barplot(Landings.slip,names=Year,ylim=c(0,1000),ylab="Landings (t)",cex.names=0.8,cex.axis=0.8))
	dev.off()

	# Spatial Catch
	pdf(file.path( project.datadirectory("lobster"), "R","LFA41spatialcatch.pdf"))
	for(y in 2002:2015){
		logs41.dat<-na.omit(subset(logs41,YEAR==y,c('MON_DOC_ID','DDLON','DDLAT','WEIGHT_KG')))
		names(logs41.dat)<-c("EID","X","Y","Z")
		logs41.dat$EID<-1:nrow(logs41.dat)

		lvls=c(10,50,100,500,1000,5000,10000,50000)
		LFA41polys<-gridPlot(logs41.dat,lvls=lvls,border=NA,FUN=sum,grid.size=1/60)
		LobsterMap('41',poly.lst=LFA41polys[1:2],title=y)
		addPolys(LFA41areas)
		ContLegend('bottomright',bty='n',lvls=lvls,Cont.data=LFA41polys,title='kg')


	}
	dev.off()

	# CPUE
	CPUEplot(logs41,lfa=41,graphic='pdf',wd=10,ht=8,lab='LFA41')


	## Jonah crab


	lobster.db('logs41jonah')
	logs41jonah$YEAR<-year(logs41jonah$DATE_FISHED)

	logs41jonah$SYEAR<-year(logs41jonah$DATE_FISHED)
	logs41jonah$LFA<-41
	logs41jonah$WEIGHT_KG<-logs41jonah$ADJCATCH_LBS*0.4536


	lbs<-with(logs41jonah,tapply(ADJCATCH_LBS,YEAR,sum,na.rm=T))
	JClandat<-data.frame(Year=names(lbs),Landings.t=lbs*0.0004536)
	
	pdf(file.path( project.datadirectory("lobster"), "R","LFA41_JC.pdf"),8,5)
	barplot(JClandat$Landings.t,names=JClandat$Year,ylim=c(0,1000),ylab="Landings (t)",cex.names=0.8,cex.axis=0.8)
	dev.off()

	# Spatial Catch
	pdf(file.path( project.datadirectory("lobster"), "R","LFA41spatialcatch_JC.pdf"))
	for(y in 2002:2008){
		logs41jonah.dat<-na.omit(subset(logs41jonah,YEAR==y,c('DOC_ID','DDLON','DDLAT','WEIGHT_KG')))
		names(logs41jonah.dat)<-c("EID","X","Y","Z")
		logs41jonah.dat$EID<-1:nrow(logs41jonah.dat)

		lvls=c(10,50,100,500,1000,5000,10000,50000)
		LFA41polys<-gridPlot(logs41jonah.dat,lvls=lvls,border=NA,FUN=sum,grid.size=1/60)
		LobsterMap('41',poly.lst=LFA41polys[1:2],title=y)
		addPolys(LFA41areas)
		ContLegend('bottomright',bty='n',lvls=lvls,Cont.data=LFA41polys,title='kg')


	}
	dev.off()

	# CPUE
	CPUEplot(logs41jonah,lfa=41,graphic='pdf',wd=10,ht=8,lab='LFA41_JC')



##______________________##
##                      ##
##       BYCATCH	    ##
##______________________##
##                      ##



	# Bycatch from Observer Data
	bycatch41<-read.csv(file.path(project.datadirectory('lobster'),'data',"LFA41bycatch2015.csv"))
	bycatch41$LONGITUDE<-bycatch41$LONGITUDE*-1
	bycatch41$YEAR<-year(bycatch41$BOARD_DATE)

	# Remove records not observed
	bycatch41<-subset(bycatch41,SOURCE==0&OFFAREA!="UNKNOWN")

	# create list of all species recorded
	species<-aggregate(EST_DISCARD_WT ~ COMMON + SPECCD_ID, data = bycatch41, sum, na.rm=T)
	species<-species[order(species$EST_DISCARD_WT,decreasing=T),]

	# calculate total discarded and kept weights from observed sets 
	kept<-aggregate(EST_KEPT_WT ~ YEAR + QUARTER + OFFAREA, data = bycatch41, sum, na.rm=T)
	discard<-aggregate(EST_DISCARD_WT ~ YEAR + QUARTER  + OFFAREA + SPECCD_ID, data = bycatch41, sum, na.rm=T)



	# this is to add in zeros for species not observed in a particular set
	discard_allsp<-merge(discard[!duplicated(paste(discard$YEAR, discard$QUARTER, discard$OFFAREA)),1:3],species[,1:2])
	discard<-merge(discard,discard_allsp,all=T)
	observed<-merge(kept,discard,all=T)
	observed$EST_DISCARD_WT[is.na(observed$EST_DISCARD_WT)]<-0
	
	# calculate discard rate
	observed$discardRate<-observed$EST_DISCARD_WT/observed$EST_KEPT_WT
	
	

	## Total Catch from Logs
	lobster.db('logs41')
	logs41$YEAR<-year(logs41$FV_FISHED_DATETIME)
	logs41$QUARTER<-quarter(logs41$FV_FISHED_DATETIME)
	logs41$ADJCATCHKG<-logs41$ADJCATCH*0.4536

	# switch to areas in bycatch data
	logs41$AREA<-logs41$OFFAREA	
	logs41$OFFAREA[logs41$AREA == 'GBANK'] <- '1GBANK'
	logs41$OFFAREA[logs41$AREA == 'GBASIN'] <- '2GBASIN'
	logs41$OFFAREA[logs41$AREA == 'SEBROWNS'] <- '3SEBROWNS'
	logs41$OFFAREA[logs41$AREA == 'SWBROWNS'] <- '4WBROWNS'
	logs41$OFFAREA[logs41$AREA == 'CROWELL'] <- '4WBROWNS'

	# calculate total landings by YEAR, QUARTER & OFFAREA
	#total<-aggregate(ADJCATCHKG ~ YEAR + QUARTER + OFFAREA , data = logs41, sum, na.rm=T)
	total<-aggregate(ADJCATCHKG ~ YEAR + QUARTER + OFFAREA , data = subset(logs41,OFFAREA!="UNKNOWN"), sum, na.rm=T)
	total<-merge(total,species[,1:2])


	# merge with observed sets
	combined<-merge(observed,total,all=T)
	# seperate the sampled quarter + year + area combinations from the non-sampled ones
	notSampled<-subset(combined,is.na(discardRate))
	Sampled<-subset(combined,!is.na(discardRate))

	# calculate mean discard rate for each year and area and species
	AreaMeans<-aggregate(discardRate ~ YEAR + OFFAREA + SPECCD_ID, data = combined, mean, na.rm=T)

	# fill in annual means for quarters not sampled 
	tmp1<-merge(notSampled[,-which(names(notSampled)=='discardRate')], AreaMeans,all.x=T)

	# calculate mean discard rate for each year and species
	AnnualMeans<-aggregate(discardRate ~ YEAR + SPECCD_ID, data = combined, mean, na.rm=T)

	# fill in area means for years not sampled 
	tmp2<-merge(tmp1[is.na(tmp1$discardRate),-which(names(tmp1)=='discardRate')], AnnualMeans,all.x=T)
	
	# combine so that there is a discard rate for all not-sampled records 
	notSampled<-rbind(subset(tmp1,!is.na(discardRate)),tmp2)

	# combine sampled and non sampled 
	finaldata<-rbind(Sampled,notSampled)

	# calculate total discards
	finaldata$TotalDiscards<-finaldata$ADJCATCHKG*finaldata$discardRate

	# add in NAFO areas
	finaldata$NAFOAREA<-NA
	finaldata$NAFOAREA[finaldata$OFFAREA %in% c('1GBANK','2GBASIN')] <- "5Z"
	finaldata$NAFOAREA[finaldata$OFFAREA %in% c('3SEBROWNS','4WBROWNS')] <- "4X"

	yrs<-sort(unique(finaldata$YEAR))
	table4X<-sapply(yrs,function(y){with(subset(finaldata,NAFOAREA=="4X"&YEAR==y),tapply(TotalDiscards,COMMON,sum))})
	table4X<-data.frame(round(table4X[order(rowSums(table4X),decreasing=T),]))
	names(table4X)<-yrs
	table4X

	table5Z<-sapply(yrs,function(y){with(subset(finaldata,NAFOAREA=="5Z"&YEAR==y),tapply(TotalDiscards,COMMON,sum))})
	table5Z<-data.frame(round(table5Z[order(rowSums(table5Z),decreasing=T),]))
	names(table5Z)<-yrs
	table5Z

	table<-sapply(yrs,function(y){with(subset(finaldata,YEAR==y),tapply(TotalDiscards,COMMON,sum))})
	table<-data.frame(round(table[order(rowSums(table),decreasing=T),]))[-1,]
	names(table)<-yrs
	table




	# add in NAFO areas
	observed$NAFOAREA<-NA
	observed$NAFOAREA[observed$OFFAREA %in% c('1GBANK','2GBASIN')] <- "5Z"
	observed$NAFOAREA[observed$OFFAREA %in% c('3SEBROWNS','4WBROWNS')] <- "4X"

	yrs<-sort(unique(subset(observed,NAFOAREA=="4X")$YEAR))
	ObsTable4X<-sapply(yrs,function(y){with(subset(observed,NAFOAREA=="4X"&YEAR==y),tapply(EST_DISCARD_WT,COMMON,sum))})
	ObsTable4X<-data.frame(round(ObsTable4X[order(rowSums(ObsTable4X),decreasing=T),]))
	names(ObsTable4X)<-yrs
	ObsTable4X

	yrs<-sort(unique(subset(observed,NAFOAREA=="5Z")$YEAR))
	ObsTable5Z<-sapply(yrs,function(y){with(subset(observed,NAFOAREA=="5Z"&YEAR==y),tapply(EST_DISCARD_WT,COMMON,sum))})
	ObsTable5Z<-data.frame(round(ObsTable5Z[order(rowSums(ObsTable5Z),decreasing=T),]))
	names(ObsTable5Z)<-yrs
	ObsTable5Z


	yrs<-sort(unique(observed$YEAR))
	ObsTable<-sapply(yrs,function(y){with(subset(observed,YEAR==y),tapply(EST_DISCARD_WT,COMMON,sum))})
	ObsTable<-data.frame(round(ObsTable[order(rowSums(ObsTable),decreasing=T),]))[-1,]
	names(ObsTable)<-yrs
	ObsTable


	write.csv(table4X,file.path(project.datadirectory('lobster'),'assessments',"LFA41","TotalByCatch4X.csv"))
	write.csv(ObsTable4X,file.path(project.datadirectory('lobster'),'assessments',"LFA41","ObservedByCatch4X.csv"))
	write.csv(table5Z,file.path(project.datadirectory('lobster'),'assessments',"LFA41","TotalByCatch5Z.csv"))
	write.csv(ObsTable5Z,file.path(project.datadirectory('lobster'),'assessments',"LFA41","ObservedByCatch5Z.csv"))
	write.csv(table,file.path(project.datadirectory('lobster'),'assessments',"LFA41","TotalByCatch.csv"))
	write.csv(ObsTable,file.path(project.datadirectory('lobster'),'assessments',"LFA41","ObservedByCatch.csv"))




	combined$Time<-combined$YEAR+(combined$QUARTER-1)*0.25
par(mfrow=c(2,2))
for(i in 1:4){
	plot(discardRate~Time,subset(combined,SPECCD_ID==2511&OFFAREA==areas[i]))
}

	combined$logEST_KEPT_WT<-log(combined$EST_KEPT_WT)

	jonah<-subset(combined,SPECCD_ID==species$SPECCD_ID[2])


	fit<-gam(EST_DISCARD_WT ~ offset(logEST_KEPT_WT) + Time  + OFFAREA, data=jonah, family=poisson(link="log"))
 	exp(predict.glm(fit))->jonah$predicted






	fit<-list()
	for(i in 1:2){
	
	fit[[i]]<-glm(EST_DISCARD_WT ~ offset(logEST_KEPT_WT) + as.factor(YEAR) + as.factor(QUARTER)  + OFFAREA, data=subset(combined,SPECCD_ID==species$SPECCD_ID[i]), family=poisson(link="log"))

	}


	subset(combined,SPECCD_ID==species$SPECCD_ID[i])
 	predict.glm(fit[[2]])->combined$predicted




	predDat<-total
	predDat$logEST_KEPT_WT<-log(predDat$ADJCATCHKG)
