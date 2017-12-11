    
  
	p = bio.lobster::load.environment()
	la()


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


    logsInSeason<-lobster.db('process.logs.redo')
    logsInSeason<-lobster.db('process.logs')

    cpueLFA.dat = CPUEplot(logsInSeason,lfa= p$lfas,yrs=2006:2016,graphic='R',export=T)
    cpueLFA.dat = CPUEplot(logsInSeason,lfa= p$lfas,yrs=2006:2016,graphic='pdf',path=figdir)
    cpueSubArea.dat = CPUEplot(logsInSeason,subarea= p$subareas,yrs=2006:2016,graphic='R')



	## Commercial CPUE MOdels
	mf1 = formula(logWEIGHT ~ fYEAR + DOS + TEMP + DOS * TEMP)
	mf2 = formula(logWEIGHT ~ fYEAR + DOS + TEMP)
	mf3 = formula(logWEIGHT ~ fYEAR + DOS)
	mf4 = formula(logWEIGHT ~ fYEAR + TEMP)
	mf5 = formula(logWEIGHT ~ fYEAR + DOS + TEMP + (1 | fYEAR/fAREA)) # combined


	TempModelling = TempModel()
	#CPUE.data<-CPUEModelData(p,redo=T,TempModelling)
	CPUE.data<-CPUEModelData(p,redo=F)
	CPUEModelResults1 = list()
	CPUEModelResults2 = list()
	CPUEModelResults3 = list()
	CPUEModelResults4 = list()
	AICs1 = c()
	AICs2 = c()
	AICs3 = c()
	AICs4 = c()
	for(i in 1:length( p$subareas)){

		mdata = subset(CPUE.data,subarea==p$subareas[i])
		CPUEModelResults1[[i]] = CPUEmodel(mf1,mdata)
		CPUEModelResults2[[i]] = CPUEmodel(mf2,mdata)
		CPUEModelResults3[[i]] = CPUEmodel(mf3,mdata)
		CPUEModelResults4[[i]] = CPUEmodel(mf4,mdata)
		AICs1[i] = CPUEModelResults1[[i]]$model$aic
		AICs2[i] = CPUEModelResults2[[i]]$model$aic
		AICs3[i] = CPUEModelResults3[[i]]$model$aic
		AICs4[i] = CPUEModelResults4[[i]]$model$aic


	}
	names(CPUEModelResults1) = p$subareas
	names(CPUEModelResults2) = p$subareas
	names(CPUEModelResults3) = p$subareas
	names(CPUEModelResults4) = p$subareas


	for(i in 1:length(CPUEModelResults))

	CPUECombinedModelResults = CPUEmodel(mf5,CPUE.data,combined=T)

	cpue1c=CPUEModelPlot(CPUECombinedModelResults,TempModelling,combined=T,lfa = c("27N","27S", "28", "29", "30"),xlim=c(2010,2016.4),ylim=c(0,10.5),graphic='R',path=figdir,lab='1c')
	cpue2c=CPUEModelPlot(CPUECombinedModelResults,TempModelling,combined=T,lfa = c("31A", "31B", "32", "33E", "33W"),xlim=c(2010,2016.4),ylim=c(0,10.5),graphic='pdf',path=figdir,lab='2c')

	out=CPUEModelPlot(CPUEModelResults,TempModelling,lfa = c("33W","33E"),xlim=c(2014,2017.5),ylim=c(0,20),wd=15)
	out1=CPUEModelPlot(CPUEModelResults,TempModelling,lfa = c("27N","27S", "28", "29", "30"),xlim=c(2010,2016.4),ylim=c(0,10.5))
	out2=CPUEModelPlot(CPUEModelResults,TempModelling,lfa = c("31A", "31B", "32", "33E", "33W"),xlim=c(2010,2016.4),ylim=c(0,10.5))
	cpue1=CPUEModelPlot(CPUEModelResults,TempModelling,lfa = c("27N","27S", "28", "29", "30"),xlim=c(2010,2016.4),ylim=c(0,10.5),graphic='pdf',path=figdir,lab=1)
	cpue2=CPUEModelPlot(CPUEModelResults,TempModelling,lfa = c("31A", "31B", "32", "33E", "33W"),xlim=c(2010,2016.4),ylim=c(0,10.5),graphic='pdf',path=figdir,lab=2)



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
		FSRSModelResultsShort[[i]]=FSRSmodel(mdata, response="SHORTS")
		FSRSModelResultsLegal[[i]]=FSRSmodel(mdata, response="LEGALS")

	}


