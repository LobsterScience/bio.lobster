	
p = bio.lobster::load.environment()

la()

#temp.dat = read.csv(file.path(project.datadirectory('bio.lobster'),'data','CTS_Temperature.csv'))
#collector.dat = read.csv(file.path(project.datadirectory('bio.lobster'),'data','CollectorData.csv'))
FSRScpue.dat = read.csv(file.path( project.datadirectory("bio.lobster"), "data","products","FSRScpue.csv"))

#------------------## LFA 34 ##-----------------#
#------------------## LBM ##-----------------#


	####### Growth Parameters 
	#######

		# [1=male, 2=female, 3=berried]
		# length-weight 
		a=c(0.000608,0.001413,0.00482)
		b=c(3.0583,2.8746,2.638)

		# VB
		Linf=c(281,207)
		k=c(0.065,0.089)
		t0=c(0.76,0.42)

	
	####### Abundance Indices
	#######


		# CPUE
		FSRScpue34.dat = subset(FSRScpue.dat,LFA==34)
		LOGScpue34.dat = read.csv(file.path(project.datadirectory('lobster'),'data',"products","LFA34CPUE.csv"))
		
		# Lobster Survey 
		SURVindex34.dat = read.csv(file.path(project.datadirectory('lobster'),'data',"products","LFA34SurveyIndex.csv"))

		# Scallop Surveyc
		SCALSURV.dat = ScallopSurveyProcess(SPA=c("3","29"),Years=1996:2014)
		LPT =  with(SCALSURV.dat,tapply(NLobsStd,YEAR,mean,na.rm=T))
		yrs = as.numeric(names(LPT))
		LPTsd =  with(SCALSURV.dat,tapply(NLobsStd,YEAR,sd,na.rm=T))
		LPTn =  with(SCALSURV.dat,tapply(NLobsStd,YEAR,length))
		LPTse = LPTsd/sqrt(LPTn)
		rmLPT = ma(LPT)
		write.csv(data.frame(SYEAR=1996:2014,N=LPTn,LPT=LPT,LPT.SE=LPTse),file.path(project.datadirectory('lobster'),'data',"products","LFA34ScalSurveyIndex.csv"),row.names=F)

		SCALSURVindex34.dat = read.csv(file.path(project.datadirectory('lobster'),'data',"products","LFA34ScalSurveyIndex.csv"))


		SURVindex34.dat$DEN = SURVindex34.dat$LPT/17
		SCALSURVindex34.dat$DEN = SCALSURVindex34.dat$LPT
		
		pdf('LFA34SurveyDensity.pdf')
		plot(DEN~SYEAR,SURVindex34.dat,type='b',ylim=c(0,max(DEN)),pch=16,ylab='Lobster Density #/1000m2')
		lines(DEN~SYEAR,SCALSURVindex34.dat,type='b',pch=16,col='red')
		legend('bottomright',c('Lobster Survey','Scallop Survey x4'),pch=16,col=c('black','red'))
		dev.off()
 
 		# Groundfish Survey
		RVS.lst = GoundfishSurveyProcess(Lengths=F) # Lengths currently not working, problem in groundfish.analysis

		# FSRS

	
	####### Landings
	#######

		AnnualLand.dat = read.csv(file.path(project.datadirectory('lobster'),"data","inputs","AnnualSlipLand.csv"))
		HistoricLand.dat = read.delim(file.path(project.datadirectory('lobster'),"data","inputs","LFA34_Landings_1892-2004.txt"))
		SeasonalLand.dat = read.csv(file.path(project.datadirectory('lobster'),"data","inputs","SeasonalSlipLand.csv"))
	
		Annual.dat = reshape(AnnualLand.dat,idvar="YEAR",varying=names(AnnualLand.dat)[-1],times=substr(names(AnnualLand.dat)[-1],4,6),direction='long',timevar="LFA",v.names="CATCH")
		Season.dat = reshape(SeasonalLand.dat,idvar="SEASON",varying=names(SeasonalLand.dat)[-1],times=substr(names(SeasonalLand.dat)[-1],4,6),direction='long',timevar="LFA",v.names="CATCH")
		Annual.dat$SYEAR = Annual.dat$YEAR
		Season.dat$SYEAR = as.numeric(substr(Season.dat$SEASON,6,9))
		Landings.dat = rbind(subset(Annual.dat,LFA<33&YEAR>1975,c("SYEAR","LFA","CATCH")),subset(Season.dat,select=c("SYEAR","LFA","CATCH")))

		write.csv(Landings.dat,file.path( project.datadirectory("lobster"), "data","products","TotalLandings.csv"),row.names=F)



		# Ref points
		annual.landings     = lobster.db('annual.landings') #annual.landings
		seasonal.landings   = lobster.db('seasonal.landings')
		#historical.landings = lobster.db('historical.landings')
		
		landings = cbind(annual.landings[annual.landings$YR%in%1985:2016,1:9],seasonal.landings[seasonal.landings$YR%in%1985:2016,2:6])
		USR = apply(subset(landings,YR%in%1988:2009,-1),2,median)*0.8
		LRP = apply(subset(landings,YR%in%1988:2009,-1),2,median)*0.4
		value = apply(subset(landings,YR%in%1988:2016,-1),2,runmed,3)[which(landings$YR==2016),]


	####### Length Compositions
	#######

		bins = seq(53,220,5)


		# Lobster Survey

		Yrs = 2005:2014
		surveyLobsters34 = LobsterSurveyProcess(lfa="34",yrs=Yrs,mths=c("Jul","Jun"),size.range=range(bins),bin.size=diff(bins)[1])
		LS32stns = read.csv(file.path(project.datadirectory('lobster'),'data',"survey32Stations.csv"))
		LobsterSurveyCLF = t(sapply(Yrs,function(y){colMeans(subset(surveyLobsters34,YEAR==y&SID%in%LS32stns$SID,paste0("CL",bins[-length(bins)])),na.rm=T)}))
		LobsterSurveyCLF = t(sapply(Yrs,function(y){colMeans(subset(surveyLobsters34,YEAR==y,paste0("CL",bins[-length(bins)])),na.rm=T)}))
		#BubblePlotCLF(LobsterSurveyCLF,inch=0.2,bg=rgb(0,1,0,0.1),yrs=Yrs,bins=bins,filen="LobSurvLFA34",prop=T)
		BarPlotCLF(list(LobsterSurveyCLF),yrs=Yrs,bins=bins,col='grey',filen="LobSurvLFA34npt",rel=F,ymax=4)
		LobsterSurveyCLF = t(sapply(Yrs,function(y){colMeans(subset(surveyLobsters34,YEAR==y&SID%in%LS32stns$SID,paste0("CL",bins[-length(bins)])),na.rm=T)}))
		BarPlotCLF(list(LobsterSurveyCLF),yrs=Yrs,bins=bins,col='grey',filen="LobSurvLFA34npt32stn",rel=F,ymax=4)
		

		# Scallop Survey

		Yrs = 2000:2014
		SCALSURV34.dat = ScallopSurveyProcess(SPA=c("3","29"),Years=Yrs,size.range=range(bins),bin.size=diff(bins)[1])
		ScallopSurveyCLF = t(sapply(Yrs,function(y){colMeans(subset(SCALSURV34.dat,YEAR==y,paste0("CL",bins[-length(bins)])),na.rm=T)}))
		BubblePlotCLF(ScallopSurveyCLF,inch=0.2,bg=rgb(0,1,0,0.1),yrs=Yrs,bins=bins,filen="ScalSurvLFA34",prop=T)
		BarPlotCLF(ScallopSurveyCLF,yrs=Yrs,bins=bins,col='grey',filen="ScalSurvLFA34",rel=T)

		### Scallop Survey seperate SPA 3 and SFA 29
		# SFA 29 : Aug-Oct 2000-
		# SPA 3 : Aug-Sep 1991-2003; May-Jul 2004-
		Yrs = 2000:2014
		ScalSurvey = list()
		SCALSURV3.dat = ScallopSurveyProcess(SPA="3",Years=Yrs,size.range=range(bins),bin.size=diff(bins)[1])
		SCALSURV29.dat = ScallopSurveyProcess(SPA="29",Years=Yrs,size.range=range(bins),bin.size=diff(bins)[1])
		ScalSurvey$ScallopSurvey3 = t(sapply(Yrs,function(y){colMeans(subset(SCALSURV3.dat,YEAR==y,paste0("CL",bins[-length(bins)])),na.rm=T)}))
		ScalSurvey$ScallopSudata.frame(PID=1,POS=1:nrow(x),X=x$SET_LONG,Y=x$SET_LAT)rvey29 = t(sapply(Yrs,function(y){colMeans(subset(SCALSURV29.dat,YEAR==y,paste0("CL",bins[-length(bins)])),na.rm=T)}))
		BubblePlotCLF(ScalSurvey,inch=0.2,bg=rgb(0,1,0,0.1),yrs=Yrs,bins=bins,filen="ScalSurveyLFA34",prop=T)
		BarPlotCLF(ScalSurvey,yrs=Yrs,bins=bins,col='grey',filen="ScalSurveyLFA34",rel=T)
		

		# At Sea Sampling

		lobster.db('atSea')
		Yrs = 1982:2014
		atSea.LFA34.dat = addSYEAR(subset(atSea,LFA==34))
		atSea.LFA34.dat$YEAR = year(atSea.LFA34.dat$SDATE)
		atSea.LFA34.dat$SYEAR[month(atSea.LFA34.dat$SDATE)<12&atSea.LFA34.dat$YEAR<2001] = atSea.LFA34.dat$YEAR[month(atSea.LFA34.dat$SDATE)<12&atSea.LFA34.dat$YEAR<2001]
		atSea.LFA34.dat$SYEAR[month(atSea.LFA34.dat$SDATE)==12&atSea.LFA34.dat$YEAR<2000] = atSea.LFA34.dat$YEAR[month(atSea.LFA34.dat$SDATE)==12&atSea.LFA34.dat$YEAR<2000]-1
		atSea.LFA34.dat$Q = quarter(atSea.LFA34.dat$SDATE)
		atSeaCLF = CLF(subset(atSea.LFA34.dat,!is.na(YEAR)&SEX%in%1:2,c("SYEAR","CARLENGTH","Q","SEX")),yrs=Yrs,bins=bins)
		BubblePlotCLF(atSeaCLF$CLF,inch=0.2,bg=rgb(0,1,0,0.1),prop=T,bins=bins,filen="SeaSamplingLFA34",yrs=Yrs)
		BarPlotCLF(atSeaCLF$CLF,,yrs=Yrs,bins=bins,col='grey',filen="SeaSamplingLFA34",rel=T,LS=83)

		
		# Port Sampling

		Yrs = 2007:2014
		PSLFA34 = PortSamplesProcess(lfa='34',min.size=80)

		portCLF = CLF(subset(PSLFA34$portlengths,SEX!="B",-1),yrs=Yrs,bins=bins)
		BubblePlotCLF(portCLF$CLF,inch=0.2,bg=rgb(0,1,0,0.1),bins=bins,prop=T,filen="PortSamplingLFA34",yrs=Yrs,LS=82.5)
		BarPlotCLF(portCLF,yrs=Yrs,bins=bins,col='grey',filen="PortSamplingLFA34",rel=T,LS=82.5)

		# combined Catch Composition
			
		Yrs = 1982:2014
	
		PortSamps = subset(PSLFA34$portlengths,SEX!="B",-1)
		PortSamps$SEX = as.numeric(PortSamps$SEX=="F")+1

		SeaSamps = subset(atSea.LFA34.dat,!is.na(YEAR)&SEX%in%1:2,c("SYEAR","CARLENGTH","Q","SEX"))
		names(SeaSamps)[2] = "LENGTH"

		CombinedSamps = rbind(SeaSamps,PortSamps)
		combinedCLF = CLF(subset(CombinedSamps,LENGTH>83),yrs=Yrs,bins=bins)
		BubblePlotCLF(combinedCLF$CLF,inch=0.2,bg=rgb(0,1,0,0.1),prop=T,bins=bins,filen="combinedLFA34",yrs=Yrs)


		# FSRS recruitment traps

		lobster.db("fsrs")

	
	

		BarPlotCLF(atSeaCLF$CLF,,yrs=Yrs,bins=bins,col='grey',filen="SeaSamplingLFA34",rel=T,LS=83)

#------------------## for modelling ##-----------------#
	
	# Create .dat file for ADMB
	inputLBM = list(syr=1976, eyr=2014, linf=Linf, vbk=k, wa=a, wb=b, catchSyr=1976, catch=SeasonalLand.dat$LFA34[-40], cpueSyr=1999, cpue=LOGScpue34.dat$cpue, surv1Syr=1996, surv1=SURVindex34.dat$LPT, surv2Syr=1996, surv2=SCALSURVindex34.dat$LPT, surv3Syr=1976, surv3=RVS.lst$index, nlbin=length(bins)-1, minbin=min(bins), stepbin=diff(bins)[1], FLCsyr= 1982, FisheryLengthComp= combinedCLF$CLF, SLCsyr= 2005, SurveyLengthComp=LobsterSurveyCLF)
	#inputSCAL = list(syr=1976, eyr=2014, nage=12, linf=Linf, vbk=k, wa=a, wb=b, catchSyr=1976, catch=SeasonalLand.dat$LFA34[-40], cpueSyr=1999, cpue=LOGScpue34.dat$cpue, surv1Syr=1996, surv1=SURVindex34.dat$LPT, surv2Syr=1996, surv2=SCALSURVindex34.dat$LPT, surv3Syr=1976, surv3=RVSURVindex34.dat$LPT, nlbin=length(bins)-1, minbin=min(bins), stepbin=diff(bins)[1]), FLCsyr= , FisheryLengthComp= , SLCsyr= , SurveyLengthComp= )

	# for ADMB
	write.dat(inputLBM,file.path(project.codedirectory('lobster'),'src','admb','LFA34Data.dat'),ncolumns=inputLBM$nlbin)

	# in R!
  	#parameters = c(r=1, K=24000, q=0.1 , B0=7000 )
 	#result = optim( parameters, fn=biomass.logistic.recursion, input.data= inputLBM) 







##### collectors


CollectorData = read.csv(file.path( project.datadirectory("lobster"), "data","CollectorData.csv"))
CollectorData$Study.Area[CollectorData$Study.Area=="Lobster Bay "] = "Lobster Bay"

CollectorSummary = function(CollectorData,sp="Homarus americanus",bins=0:60){
CollectorData$Study.Area[CollectorData$Study.Area=="Lobster Bay "] = "Lobster Bay"
CollectorData$Year = year(as.Date(CollectorData$Date.retrieved,format="%m/%d/%Y"))

areas = unique(CollectorData$Study.Area)
for(i in 1:length(areas)){
	Cdata = subset(CollectorData,Species=="Homarus americanus"&Study.Area==areas[i])
	sites = unique(Cdata$Site)
	years = min(Cdata$Year):max(Cdata$Year)

			
	CLF[[i]] = colMeans(t(sapply(sites,function(s){with(subset(Cdata,Site==s&Length>=min(bins)&Length<max(bins)),hist(Length,breaks=bins,plot=F)$count)})))

	t(sapply(Yrs,function(y){colMeans(subset(SCALSURV3.dat,YEAR==y,paste0("CL",bins[-length(bins)])),na.rm=T)}))

}

	CollectorCLF = CLF(subset(CollectorData,Species=="Homarus americanus"&Study.Area=="Lobster Bay",c("Year","Length")),bins=0:60)
		BarPlotCLF(CollectorCLF$CLF,yrs=CollectorCLF$yrs,bins=0:60,col='grey',filen="LobsterBayCollector",rel=F,LS=83,ymax=38)


############ data exploration...




		port34 = merge(subset(PSLFA34$portsamples,select=c("SAMPLE_ID","LFA","SYEAR","GRID_NUM")),PSLFA34$portlengths,all=T)

			portSampGrids = lobGridPlot(subset(port34,select=c("LFA","GRID_NUM","LENGTH")),FUN=mean)
			LobsterMap('34',poly.lst=portSampGrids)
			ContLegend("bottomleft",lvls=portSampGrids$lvls,Cont.data=portSampGrids,title="mean length",inset=0.02,cex=0.8,bg='white')

		N2P(LobsterSurveyCLF)
		N2P(ScallopSurveyCLF)
		N2P(atSeaCLF)
		N2P(portCLF)


		PSLFA34$portsamples
		logsInSeason = read.csv(file.path( project.datadirectory("lobster"), "data","logsInSeason.csv"))
		logs34 = subset(logsInSeason,LFA==34,c("DATE_FISHED","WEIGHT_KG","NUM_OF_TRAPS"))
		names(logs34) = c('year','catch','effort')
		test.dat = jackknife(logs34)
		test.dat$year = as.Date(test.dat$year)
		data.frame(year=seq(min(test.dat$year),max(test.dat$year),1))->not.fished
		jack.dat = merge(test.dat,not.fished,all=T)
 		catch = with(logs34,tapply(catch,year,sum,na.rm=T)
 		effort = with(logs34,tapply(effort,year,sum,na.rm=T))

		unique(logs34$COMMUNITY_CODE)


		## Ports
		PSLFA34$portsamples$X = convert.dd.dddd(PSLFA34$portsamples$PORT_LON)*-1
		PSLFA34$portsamples$Y = convert.dd.dddd(PSLFA34$portsamples$PORT_LAT)

		PSports = subset(PSLFA34$portsamples,!duplicated(PORT_CODknE))$PORT_CODE
		ASports = subset(atSea.LFA34.dat,!duplicated(PORT))$PORT
		SLports = subset(logs,!duplicated(COMMUNITY_CODE))$COMMUNITY_CODE

		ports = subset(read.csv(file.path( project.datadirectory("lobster"), "data","Ports.csv")),LFA==lfa)
		grids = subset(read.csv(file.path( project.datadirectory("lobster"), "data","maps","lfa27_38_centgrid.csv")),LFA==lfa)

		LobsterMap('34')



Port2Grid = function(logs,lfa='34'){


	ports = subset(read.csv(file.path( project.datadirectory("lobster"), "data","Ports.csv")),LFA==lfa)
	names(ports)[c(1,7,8)] = c("COMMUNITY_CODE","Y1","X1")
	grids = subset(read.csv(file.path( project.datadirectory("lobster"), "data","maps","lfa27_38_centgrid.csv")),LFA==lfa)
	grids$X2 = grids$CENTLON*-1
	grids$Y2 = grids$CENTLAT

	lpg = subset(logs,select=c("COMMUNITY_CODE","GRID_NUM"))
	lpg$PG = paste(lpg$COMMUNITY_CODE,lpg$GRID_NUM,sep='.')
	rpg = with(lpg,tapply(PG,PG,length))
	#lineSegs = subset(lpg,!duplicated(PG))
	#lineSegs = merge(merge(merge(lineSegs,subset(ports,X1<0&Y1>0,c("COMMUNITY_CODE","X1","Y1"))),subset(grids,select=c("GRID_NUM","X2","Y2"))),data.frame(PG=names(rpg),REPS=rpg))
	lineSegs = merge(merge(lpg,subset(ports,X1<0&Y1>0,c("COMMUNITY_CODE","X1","Y1"))),subset(grids,select=c("GRID_NUM","X2","Y2")))


	loadfunctions('lobster')
	LobsterMap(lfa)

	#with(lineSegs,segments(X1,Y1,X2,Y2,lwd=log(REPS)+1,col=rgb(0,0,0,0.1)))
	with(lineSegs,segments(X1,Y1,jitter(X2),jitter(Y2),col=rgb(0,0,0,0.1)))


}



	loadfunctions('lobster')
	PortSummary()
	P2Ggif(logs34)



		loadfunctions('lobster')
		atSea.LFA34.dat$SAMPLED = 1
		pdf("atSeaSamples.pdf")
		for(y in 1987:2014){
			atSeaSampGrids = lobGridPlot(subset(atSea.LFA34.dat,SYEAR==y,c("LFA","GRIDNO","SAMPLED")),FUN=sum)
			LobsterMap('34',poly.lst=atSeaSampGrids,title=y)
		}
		dev.off()
			atSeaSampGrids = lobGridPlot(subset(atSea.LFA34.dat,!is.na(CARLENGTH),c("LFA","GRIDNO","CARLENGTH")),FUN=mean)
			LobsterMap('34',poly.lst=atSeaSampGrids)
			ContLegend("bottomleft",lvls=atSeaSampGrids$lvls,Cont.data=atSeaSampGrids,title="mean length",inset=0.02,cex=0.8,bg='white')


		cpuegrids = lobGridPlot(subset(logsInSeason,LFA=='34'&SYEAR==2014,c("LFA","GRID_NUM","CPUE")),FUN=mean)
		LobsterMap('34',poly.lst=capuegrids)


#------------------## LFA 27 ##-----------------#

tagging.dat = read.csv(file.path(project.datadirectory('bio.lobster'),'data','CapeBretonTaggingData.csv'))

tagging.dat$TagLat = convert.dd.dddd(tagging.dat$TagLat)
tagging.dat$TagLon = convert.dd.dddd(tagging.dat$TagLon)*-1
tagging.dat$TagDate = as.Date(tagging.dat$TagDate,"%d-%b-%y")
tagging.dat$CapDate = as.Date(tagging.dat$CapDate,"%d-%b-%y")
tagging.dat$DAL = tagging.dat$CapDate-tagging.dat$TagDate
tagging.dat$SizeDiff = tagging.dat$CapSize-tagging.dat$TagSize
tagging.dat$MoltIncr = tagging.dat$SizeDiff/tagging.dat$TagSize

tagging.dat$Molted = ifelse(tagging.dat$MoltIncr<0.04,0,1)
hist(tagging.dat$MoltIncr[tagging.dat$SizeDiff>-10],breaks=50)


plot(Molted~DAL,tagging.dat)


### atSea

		lobster.db('atSea')

		atSea$YEAR = year(atSea$STARTDATE)

		atSeaSamples = sapply(sort(unique(atSea$LFA)),function(x){with(subset(atSea,LFA==x&!is.na(CARLENGTH)),tapply(CARLENGTH,YEAR,length))})

### FSRS

		FSRS.dat = read.csv(file.path( project.datadirectory("lobster"), "data","FSRSrectraps.csv"))
		FSRS.dat$total = FSRS.dat$SHORTS+FSRS.dat$LEGALS


		FSRSsamples = sapply(sort(unique(FSRS.dat$LFA)),function(x){with(subset(FSRS.dat,LFA==x),tapply(total,SYEAR,sum))})
		names(FSRSsamples) = sort(unique(FSRS.dat$LFA))


Moltquality.samples = read.csv(file.path(project.datadirectory('bio.lobster'),'data','inputs','MoltQuality','LMQSAMPLES.csv'))
Moltquality.trips = read.csv(file.path(project.datadirectory('bio.lobster'),'data','inputs','MoltQuality','LMQTRIPS.csv'))
Moltquality.traps = read.csv(file.path(project.datadirectory('bio.lobster'),'data','inputs','MoltQuality','LMQTRAPS.csv'))
Moltquality.summary = read.csv(file.path(project.datadirectory('bio.lobster'),'data','inputs','MoltQuality','LMQSUMMARY.csv'))
Moltquality.raw = read.csv(file.path(project.datadirectory('bio.lobster'),'data','inputs','MoltQuality','LMQraw.csv'))


p$area="GOM"
dx=1:660
lens=c(30,50,70,90,110,130)
p$moltPr = list(a=-5,b=0.013)

plot(range(dx),0:1,type='n')

for(i in 1:length(lens)){
	
	py=predMolt(p,cl=lens[i],doy=dx,sex=1)
	lines(dx,py,lty=i)
	py2 = pPrMolt(cw=lens[i],a=p$moltPr$a,b=p$moltPr$b,d=dx,) 
	lines(dx,py2,col='blue',lty=2)


}
p$moltPr = list(a=-6,b=0.004)
dx=1:2500


x11()
plot(range(dx),0:1,type='n')

for(i in 1:length(lens)){
	
	py=predMolt(p,cl=lens[i],doy=dx,sex=1,gdd=T)
	lines(dx,py,lty=i)
	py2 = pPrMolt(cw=lens[i],a=p$moltPr$a,b=p$moltPr$b,d=dx) 
	lines(dx,py2,col='purple',lty=2)


}


for(i in 1:length(lens)){
	
	py2 = pPrMolt(cw=lens[i],a=p$moltPr$a,b=p$moltPr$b,d=dx,x=1.2) 
	lines(dx,py2,col='red',lty=2)


}


##################### Temperature Data


Temp1  =  read.csv(file.path(project.datadirectory('bio.lobster'),'Temperature Data','TemperatureDetails1.csv'))
Temp2  =  read.csv(file.path(project.datadirectory('bio.lobster'),'Temperature Data','TemperatureDetails2.csv'))
Temp3  =  read.csv(file.path(project.datadirectory('bio.lobster'),'Temperature Data','TemperatureDetails3.csv'))
Temp4  =  read.csv(file.path(project.datadirectory('bio.lobster'),'Temperature Data','TemperatureDetails4.csv'))
Temp  =  rbind(Temp2,Temp3,Temp4)

names(Temp)  =  names(Temp1)

Temp  =  rbind(Temp1,Temp)
Temp$Date = as.Date(Temp$Date,format="%d-%m-%Y")


#ProjInfo  =  read.csv(file.path(project.datadirectory('bio.lobster'),'Temperature Data','ProjectInfo.csv'))

#ProjInfo = subset(ProjInfo,!is.na(ProjectID))

#lfaports = read.table(file.path(project.datadirectory('bio.lobster'),'data','LFAports.txt'),header=F)
#names(lfaports) = c("LFA","Location")

#ProjInfo = merge(ProjInfo,lfaports,all=T)

# write.csv(ProjInfo, file.path(project.datadirectory('bio.lobster'),'Temperature Data','ProjectInfo.csv'),row.names=F)

otherTemp = subset(Temp,ProjectID%in%ProjInfo$ProjectID[ProjInfo$Project%in%c("CTS","Vemco")])
fsrsTemp = subset(Temp,ProjectID%in%ProjInfo$ProjectID[!ProjInfo$Project%in%c("CTS","Vemco")])


		with(filteredTempData,plot(as.Date(Date),Temperature,pch=16,cex=0.3,col=rgb(0,0,0,0.1)))

 
ProjInfo = read.csv(file.path(project.datadirectory('bio.lobster'),'Temperature Data','ProjectInfo.csv'))

Temp = read.csv(file.path(project.datadirectory('bio.lobster'),'Temperature Data','temperatureData.csv'))

otherTemp = subset(Temp,ProjectID%in%ProjInfo$ProjectID[ProjInfo$Project%in%c("CTS","Vemco")])
fsrsTemp = subset(Temp,ProjectID%in%ProjInfo$ProjectID[!ProjInfo$Project%in%c("CTS","Vemco")])

Temp$Date  = as.Date(Temp$Date)




Temp33 = subset(Temp,ProjectID%in%ProjInfo$ProjectID[ProjInfo$LFA=='33'])
Temp32 = subset(Temp,ProjectID%in%ProjInfo$ProjectID[ProjInfo$LFA=='32'])
Temp31A = subset(Temp,ProjectID%in%ProjInfo$ProjectID[ProjInfo$LFA=='31A'])
Temp31B = subset(Temp,ProjectID%in%ProjInfo$ProjectID[ProjInfo$LFA=='31B'])
Temp30 = subset(Temp,ProjectID%in%ProjInfo$ProjectID[ProjInfo$LFA=='30'])
Temp29 = subset(Temp,ProjectID%in%ProjInfo$ProjectID[ProjInfo$LFA=='29'])
Temp28 = subset(Temp,ProjectID%in%ProjInfo$ProjectID[ProjInfo$LFA=='28'])
Temp27 = subset(Temp,ProjectID%in%ProjInfo$ProjectID[ProjInfo$LFA=='27'])



		with(Temp33,plot(Date,Temperature,pch=16,cex=0.3,col=rgb(0,0,0,0.1),main='LFA 33'))
		with(Temp27,plot(Date,Temperature,pch=16,cex=0.3,col=rgb(0,0,0,0.1),main='LFA 27'))



lfas= c('33','32','31A','31B','30','29','27')

newTempLFA = list()
sdates=lobster.db('season.dates')
write.csv(sdates, file.path(project.datadirectory('bio.lobster'),'Temperature Data','SeasonDates.csv'),row.names=F)

for(i in 1:length(lfas)){

	projs = na.omit(ProjInfo$ProjectID[ProjInfo$LFA==lfas[i]&ProjInfo$ProjectID%in%fsrsTemp$ProjectID])

	TempLFA = subset(fsrsTemp,ProjectID%in%projs)

	# select for records within season
	TempLFA$SYEAR = NA
	h  =  sdates[sdates$LFA==lfas[i],]	
	for(j in 1:nrow(h)) {
		TempLFA$SYEAR[as.Date(TempLFA$Date)>=as.Date(h[j,'START_DATE'])&as.Date(TempLFA$Date)<=as.Date(h[j,'END_DATE'])] = h[j,'SYEAR']
	}

	newTempLFA[[i]] = subset(TempLFA,!is.na(SYEAR))
}

filteredTempData = do.call("rbind",newTempLFA)
write.csv(filteredTempData, file.path(project.datadirectory('bio.lobster'),'Temperature Data','filteredTempData.csv'),row.names=F)
filteredTempData = read.csv(file.path(project.datadirectory('bio.lobster'),'Temperature Data','filteredTempData.csv'))

filteredTempData$Date  = as.Date(filteredTempData$Date)
Temp27 = subset(filteredTempData,ProjectID%in%ProjInfo$ProjectID[ProjInfo$LFA=='27'])


for(i in 1:length(lfas)){	

	projs = na.omit(ProjInfo$ProjectID[ProjInfo$LFA==lfas[i]&ProjInfo$ProjectID%in%fsrsTemp$ProjectID])

	TempLFA = subset(fsrsTemp,ProjectID%in%projs)
	#dir.create(file.path(project.datadirectory('bio.lobster'),'Temperature Data','plots',paste0('LFA',lfas[i])))

	filteredTempLFA = subset(filteredTempData,ProjectID%in%projs)




	for(j in 1:length(projs)){

		pdf(file.path(project.datadirectory('bio.lobster'),'Temperature Data','plots',paste0('LFA',lfas[i]),paste0(projs[j],'.pdf')))
		with(subset(TempLFA,ProjectID==projs[j]),plot(Date,Temperature,pch=16,cex=0.3,col=rgb(0,0,0,0.2),main=paste('LFA',lfas[i],projs[j])))
		with(subset(filteredTempLFA,ProjectID==projs[j]),points(Date,Temperature,pch=16,cex=0.3,col=rgb(1,0,0,0.2)))
		dev.off()

	}
}

### FSRS

lobster.db('fsrs')

fsrsT =  subset(fsrs,TEMP>-90)
fsrsT$Dloc = paste(fsrsT$HAUL_DATE,fsrsT$LATITUDE,fsrsT$LONGITUDE)

fsrsT = subset(fsrsT,!duplicated(Dloc))
fsrsT$HAUL_DATE = as.Date(fsrsT$HAUL_DATE)

pdf('FSRStempsMap.pdf')
LobsterMap('all')

with(fsrsT,points(LONG_DD,LAT_DD,pch=16,cex=0.3,col=rgb(1,0,0,0.1)))
dev.off()

lfas = c("27", "28",  "29",  "30",  "31A", "31B", "32",  "33",  "34",  "35")
nLFAs = sort(unique(fsrsT$LFA) )

pdf('FSRStemps.pdf',height=11,width=8)
par(mfrow=c(5,1),mar=c(3,3,3,3),las=1)
for(i in 1:length(lfas)){
	plot(TEMP~HAUL_DATE,subset(fsrsT,LFA==nLFAs[i]),pch='.',col=rgb(0,0,0,0.2),main=paste("LFA",lfas[i]),ylim=c(-1,18))
	#lines(rDailyTemps

}
dev.off()


with(subset(fsrsT,!duplicated(paste(LATITUDE,LONGITUDE))),tapply(TEMP,LFA,length))


# rough daily temps

m = 10 # maginitude of seasonal variation

b = 10 # avg annual temp

x = 1:6570 # days of the year ()

s =  70 # coldest day of the year

t = -cos(2*pi/365.25*(x-s))*m+b # daily temperature

d = as.Date("1999-01-01")+x

	#png(file.path(project.datadirectory('bio.lobster'),'Temperature Data','plots','LFA33temps.png'))
		plot(TEMP~HAUL_DATE,fsrsT,pch=16,cex=0.3,col=rgb(1,0,0,0.1),main='LFA 33')
		lines(d,t)
		#dev.off()

