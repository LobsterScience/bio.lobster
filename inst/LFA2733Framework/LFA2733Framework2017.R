    
  
	p = bio.lobster::load.environment()


    p$syr = 2005
    p$yrs = p$syr:p$current.assessment.year

    figdir = file.path(project.datadirectory("bio.lobster"),"figures","LFA2733Framework2018")

     p$lfas = c("27", "28", "29", "30", "31A", "31B", "32", "33") # specify lfas for data summary
  

    ## Carapace Length Frequency Plots
	
	# at Sea Sampling
	
	CarapaceLengthFrequencies(LFAs= '33', DS='atSea', by='SEX', fn='33',Yrs = c(2009:2014),vers=2,rootdir=figdir)
	CarapaceLengthFrequencies(LFAs= '32', DS='atSea', by='SEX', fn='32',Yrs = c(2009:2016),vers=2,rootdir=figdir)
	CarapaceLengthFrequencies(LFAs= '31A', DS='atSea', by='SEX', fn='31A',Yrs = c(2009:2016),vers=2,rootdir=figdir)
	CarapaceLengthFrequencies(LFAs= '31B', DS='atSea', by='SEX', fn='31B',Yrs = c(2009:2016),vers=2,rootdir=figdir)
	CarapaceLengthFrequencies(LFAs= '30', DS='atSea', by='SEX', fn='30',Yrs = c(2009:2016),vers=2,rootdir=figdir)
	CarapaceLengthFrequencies(LFAs= '29', DS='atSea', by='SEX', fn='29',Yrs = c(2009:2016),vers=2,rootdir=figdir)
	CarapaceLengthFrequencies(LFAs= '27', DS='atSea', by='SEX', fn='27',Yrs = c(2009:2016),vers=2,rootdir=figdir)
	
	
    p$lfas = c("27", "28", "29", "30", "31.1", "31.2", "32", "33") # specify lfas for data summary
	# FSRS recruitment traps
	CarapaceLengthFrequencies(LFAs= p$lfas, DS='fsrs', by="LFA", bins=seq(0,140,10),rootdir=figdir)
	CarapaceLengthFrequencies(LFAs= '27', fn= '27', DS='fsrs', by="SEX", bins=seq(0,140,10), vers=2,Yrs = c(2009:2016),rootdir=figdir)
	CarapaceLengthFrequencies(LFAs= '29', fn= '29', DS='fsrs', by="SEX", bins=seq(0,140,10), vers=2,Yrs = c(2009:2016),rootdir=figdir)
	CarapaceLengthFrequencies(LFAs= '30', fn= '30', DS='fsrs', by="SEX", bins=seq(0,140,10), vers=2,Yrs = c(2009:2016),rootdir=figdir)
	CarapaceLengthFrequencies(LFAs= 31.1,fn= '31A', DS='fsrs', by="SEX", bins=seq(0,140,10),vers=2,Yrs = c(2009:2016),rootdir=figdir)
	CarapaceLengthFrequencies(LFAs= 31.2,fn= '31B', DS='fsrs', by="SEX", bins=seq(0,140,10),vers=2,Yrs = c(2009:2016),rootdir=figdir)
	CarapaceLengthFrequencies(LFAs= '32', fn= '32', DS='fsrs', by="SEX", bins=seq(0,140,10), vers=2,Yrs = c(2009:2016),rootdir=figdir)
	CarapaceLengthFrequencies(LFAs= '33', fn= '33', DS='fsrs', by="SEX", bins=seq(0,140,10), vers=2,Yrs = c(2009:2016),rootdir=figdir)
	#CarapaceLengthFrequencies(LFAs= '27', DS='fsrs', by="LFA", bins=c(seq(0,70,10),75,seq(80,200,10)),graphic='R',rootdir=figdir)



    ## CPUE
    p$lfas = c("27", "28", "29", "30", "31A", "31B", "32", "33") # specify lfas for data summary
    p$subareas = c("27N","27S", "28", "29", "30", "31A", "31B", "32", "33E", "33W") # specify lfas for data summary


    logsInSeason<-lobster.db('process.logs')

    cpueLFA.dat = CPUEplot(logsInSeason,lfa= p$lfas,yrs=2006:2016,graphic='R',export=T)
    cpueLFA.dat = CPUEplot(logsInSeason,lfa= p$lfas,yrs=2006:2016,graphic='pdf',path=figdir)
    cpueSubArea.dat = CPUEplot(logsInSeason,subarea= p$subareas,yrs=2006:2016,graphic='R')

	#write.csv(cpueLFA.dat$annual.data,"CPUEannualData.csv",row.names=F)
	#write.csv(na.omit(cpueLFA.dat$daily.data),"CPUEdailyData.csv",row.names=F)


	## Fishery Footprint
	catchLevels = c(0,100000,200000,300000,400000,500000,600000,700000,800000)
	yrs = 2013:2016
	for(i in 1:length(yrs)){
		catchgrids = lobGridPlot(subset(logsInSeason,LFA%in%p$lfas&SYEAR==yrs[i],c("LFA","GRID_NUM","TOTAL_WEIGHT_KG")),FUN=sum,lvls=catchLevels)
		pdf(file.path(figdir,paste0("FisheryFootprint",yrs[i],".pdf")))
		LobsterMap('27-33',poly.lst=catchgrids)
	    SpatialHub::contLegend('bottomright',lvls=catchgrids$lvls/1000,Cont.data=catchgrids,title="Catch (tons)",inset=0.02,cex=0.8,bg='white')
	    dev.off()
	}


	## FSRS MOdels

	FSRSvesday<-FSRSModelData()
	FSRSModelResultsShort = list()
	FSRSModelResultsLegal = list()
	for(i in 1:length( p$subareas)){

		mdata = subset(FSRSvesday,subarea==p$subareas[i])
		FSRSModelResultsShort[[i]]=FSRSmodel(LFA27north, response="SHORTS")
		FSRSModelResultsLegal[[i]]=FSRSmodel(LFA27north, response="LEGALS")

	}



	## Commercial CPUE MOdels

	CPUE.data<-CPUEModelData()
	CPUEModelResults = list()
	for(i in 1:length( p$subareas)){

		mdata = subset(CPUE.data,subarea==p$subareas[i])
		CPUEModelResults[[i]]=CPUEmodel(LFA27north)

	}





