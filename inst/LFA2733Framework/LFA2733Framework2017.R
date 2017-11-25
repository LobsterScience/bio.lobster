    
  
	p = bio.lobster::load.environment()

    p$lfas = c("27", "28", "29", "30", "31.1", "31.2", "32", "33") # specify lfas for data summary

    p$syr = 2005
    p$yrs = p$syr:p$current.assessment.year

   

    ## Carapace Length Frequency Plots
	
	# at Sea Sampling
	CarapaceLengthFrequencies(LFAs= p$lfas, DS='atSea', by="LFA", fn='LFA2733')
	CarapaceLengthFrequencies(LFAs= '27', DS='atSea', by=c("Q","SEX"), fn='27')
	CarapaceLengthFrequencies(LFAs= '33', DS='atSea', by=c("Q","SEX"), fn='33')
	
	CarapaceLengthFrequencies(LFAs= '33', DS='atSea', by='SEX', fn='33',Yrs = c(2009,2010,2012:2014),vers=2,graphic="R")
	
	
	# Port sampling
	CarapaceLengthFrequencies(LFAs= c("27", "28", "29", "30", "31", "32", "33"), DS='port', Yrs=2007:2016, by="LFA", fn='LFA2733')
	CarapaceLengthFrequencies(LFAs='34', DS='port', Yrs=2010:2015, by="SEX", bins=seq(0,200,1), fn='34')
	
	# FSRS recruitment traps
	CarapaceLengthFrequencies(LFAs= p$lfas, DS='fsrs', by="LFA", bins=seq(0,140,10))
	CarapaceLengthFrequencies(LFAs= p$lfas, DS='fsrs', by="LFA", bins=c(seq(0,70,10),75,seq(80,200,10)))



    ## CPUE
    p$lfas = c("27", "28", "29", "30", "31A", "31B", "32", "33") # specify lfas for data summary
    p$subareas = c("27N","27S", "28", "29", "30", "31A", "31B", "32", "33E", "33W") # specify lfas for data summary


    logsInSeason<-lobster.db('process.logs')

    cpueLFA.dat = CPUEplot(logsInSeason,lfa= p$lfas,yrs=2006:2016,graphic='R')
    cpueSubArea.dat = CPUEplot(logsInSeason,subarea= p$subareas,yrs=2006:2016,graphic='R')




	## Fishery Footprint

	catchgrids = lobGridPlot(subset(logsInSeason,LFA%in%p$lfas&SYEAR==2016,c("LFA","GRID_NUM","TOTAL_WEIGHT_KG")),FUN=sum,place=1)
	LobsterMap('27-33',poly.lst=catchgrids)



	## FSRS MOdels

	FSRSvesday<-FSRSModelData()
	FSRSModelResultsShort = list()
	FSRSModelResultsLegal = list()
	for(i in 1:length( p$subareas)){

		mdata = subset(FSRSvesday,subarea==p$subareas[i])
		FSRSModelResultsShort[[i]]=FSRSmodel(LFA27north, response="SHORTS")
		FSRSModelResultsLegal[[i]]=FSRSmodel(LFA27north, response="LEGALS")

	}






    ## Sea Sampling

    lobster.db( DS="atSea", p=p)		# at Sea sampling from materialized view
    lobster.db( DS="fsrs", p=p)		# FSRS recruitment traps


	ss33 = subset(atSea,LFA==33)
	ss33$SDATE=ss33$STARTDATE
	ss33=addSYEAR(ss33)

	fsrs33 = subset(fsrs,LFA==33)
	fsrs33$SDATE=fsrs33$HAUL_DATE
	fsrs33=addSYEAR(fsrs33)

  
  LobsterMap('33')
  with(subset(fsrs33,SYEAR==2009),points(LONG_DD,LAT_DD,pch=16,cex=0.5,col=rgb(1,0,0,0.1)))

Yrs=2009:2010
sex=1:3
	atSeaCLF<-CLF(subset(ss33,SYEAR%in%Yrs&SEX%in%sex,c("SYEAR","CARLENGTH","SEX")),yrs=Yrs,bins=seq(0,220,5),vers=2)



 with(subset(logs33,!duplicated(paste(VR_NUMBER,DATE_FISHED))),tapply(SYEAR,SYEAR,length))


	logs34 = subset(logs,LFA==34)
	logs34$SDATE=logs34$DATE_FISHED
	logs34=addSYEAR(logs34)


 with(subset(logs34,!duplicated(paste(VR_NUMBER,DATE_FISHED))),tapply(SYEAR,SYEAR,length))


    a=subset(atSea,LFA==33&year(STARTDATE)==2009)
    
    a$triptrap = paste(a$TRIPNO,a$TRAPNO,sep='.')

    traploc = subset(a,)

