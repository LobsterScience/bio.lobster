    
  
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
	catchLevels = c(0,100000,200000,300000,400000,500000,600000,700000,800000)
	yrs = 2013:2016
	for(i in 1:length(yrs)){
		catchgrids = lobGridPlot(subset(logsInSeason,LFA%in%p$lfas&SYEAR==yrs[i],c("LFA","GRID_NUM","TOTAL_WEIGHT_KG")),FUN=sum,lvls=catchLevels)
		pdf(file.path(project.datadirectory("bio.lobster"),"figures",paste0("FisheryFootprint",yrs[i],".pdf")))
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
	CPUEModelResultsShort = list()
	CPUEModelResultsLegal = list()
	for(i in 1:length( p$subareas)){

		mdata = subset(CPUE.data,subarea==p$subareas[i])
		CPUEModelResultsShort[[i]]=CPUEmodel(LFA27north)

	}





