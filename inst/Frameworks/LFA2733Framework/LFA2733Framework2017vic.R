    
  
	p = bio.lobster::load.environment()


    p$syr = 2005
    p$yrs = p$syr:p$current.assessment.year

    figdir = file.path(project.datadirectory("bio.lobster"),"figures","LFA2733Framework2018")

     p$lfas = c("27", "28", "29", "30", "31A", "31B", "32", "33") # specify lfas for data summary
  

    ## Carapace Length Frequency Plots
	
	# at Sea Sampling
	CarapaceLengthFrequencies(LFAs= '27', DS='atSea', by='SEX', fn='27',Yrs = c(2011:2016),vers=2,rootdir=figdir)
	CarapaceLengthFrequencies(LFAs= '29', DS='atSea', by='SEX', fn='29',Yrs = c(2013, 2015, 2016),vers=2,rootdir=figdir)
	CarapaceLengthFrequencies(LFAs= '30', DS='atSea', by='SEX', fn='30',Yrs = c(2012),vers=2,rootdir=figdir) 
	CarapaceLengthFrequencies(LFAs= '31A', DS='atSea', by='SEX', fn='31A',Yrs = c(2011:2016),vers=2,rootdir=figdir)
	CarapaceLengthFrequencies(LFAs= '31B', DS='atSea', by='SEX', fn='31B',Yrs = c(2011:2016),vers=2,rootdir=figdir)
	CarapaceLengthFrequencies(LFAs= '32', DS='atSea', by='SEX', fn='32',Yrs = c(2011:2016),vers=2,rootdir=figdir)
	CarapaceLengthFrequencies(LFAs= '33', DS='atSea', by='SEX', fn='33',Yrs = c(2012:2014),vers=2,rootdir=figdir)
	
	
	 p$lfas = c("27", "28", "29", "30", "31.1", "31.2", "32", "33") # specify lfas for data summary
	# FSRS recruitment traps
	CarapaceLengthFrequencies(LFAs= p$lfas, DS='fsrs', by="LFA", bins=seq(0,140,10),rootdir=figdir)
	
	CarapaceLengthFrequencies(LFAs= '27', fn= '27', DS='fsrs', by="SEX", bins=seq(0,140,10), vers=2,Yrs = c(2011:2016),rootdir=figdir,ss=NULL)
	CarapaceLengthFrequencies(LFAs= '29', fn= '29', DS='fsrs', by="SEX", bins=seq(0,140,10), vers=2,Yrs = c(2011:2016),rootdir=figdir,ss=NULL)
	CarapaceLengthFrequencies(LFAs= '30', fn= '30', DS='fsrs', by="SEX", bins=seq(0,140,10), vers=2,Yrs = c(2011:2016),rootdir=figdir,ss=NULL)
	CarapaceLengthFrequencies(LFAs= 31.1, fn= '31A', DS='fsrs', by="SEX", bins=seq(0,140,10),vers=2,Yrs = c(2011:2016),rootdir=figdir,ss=NULL)
	CarapaceLengthFrequencies(LFAs= 31.2, fn= '31B', DS='fsrs', by="SEX", bins=seq(0,140,10),vers=2,Yrs = c(2011:2016),rootdir=figdir,ss=NULL)
	CarapaceLengthFrequencies(LFAs= '32', fn= '32', DS='fsrs', by="SEX", bins=seq(0,140,10), vers=2,Yrs = c(2011:2016),rootdir=figdir,ss=NULL)
	CarapaceLengthFrequencies(LFAs= '33', fn= '33', DS='fsrs', by="SEX", bins=seq(0,140,10), vers=2,Yrs = c(2011:2016),rootdir=figdir,ss=NULL)

	CarapaceLengthFrequencies(LFAs= '27', fn= '27', DS='fsrs', by="SEX", bins=seq(0,140,10), vers=2,Yrs = c(2011:2016),rootdir=figdir)
	CarapaceLengthFrequencies(LFAs= '29', fn= '29', DS='fsrs', by="SEX", bins=seq(0,140,10), vers=2,Yrs = c(2011:2016),rootdir=figdir)
	CarapaceLengthFrequencies(LFAs= '30', fn= '30', DS='fsrs', by="SEX", bins=seq(0,140,10), vers=2,Yrs = c(2011:2016),rootdir=figdir)
	CarapaceLengthFrequencies(LFAs= 31.1, fn= '31A', DS='fsrs', by="SEX", bins=seq(0,140,10),vers=2,Yrs = c(2011:2016),rootdir=figdir)
	CarapaceLengthFrequencies(LFAs= 31.2, fn= '31B', DS='fsrs', by="SEX", bins=seq(0,140,10),vers=2,Yrs = c(2011:2016),rootdir=figdir)
	CarapaceLengthFrequencies(LFAs= '32', fn= '32', DS='fsrs', by="SEX", bins=seq(0,140,10), vers=2,Yrs = c(2011:2016),rootdir=figdir)
	CarapaceLengthFrequencies(LFAs= '33', fn= '33', DS='fsrs', by="SEX", bins=seq(0,140,10), vers=2,Yrs = c(2011:2016),rootdir=figdir)

	

    ## CPUE
    p$lfas = c("27", "28", "29", "30", "31A", "31B", "32", "33") # specify lfas for data summary
    p$subareas = c("27N","27S", "28", "29", "30", "31A", "31B", "32", "33E", "33W") # specify lfas for data summary


    logsInSeason<-lobster.db('process.logs')

    cpueLFA.dat = CPUEplot(logsInSeason,lfa= p$lfas,yrs=2006:2016,graphic='pdf',path=figdir)
    cpueSubArea.dat = CPUEplot(logsInSeason,subarea= p$subareas,yrs=2006:2016,graphic='R')




	## Fishery Footprint - Landings
	catchLevels = c(0,100000,200000,300000,400000,500000,600000,700000,800000)
	yrs = 2011:2016
	for(i in 1:length(yrs)){
		catchgrids = lobGridPlot(subset(logsInSeason,LFA%in%p$lfas&SYEAR==yrs[i],c("LFA","GRID_NUM","TOTAL_WEIGHT_KG")),FUN=sum,lvls=catchLevels)
		saveRDS(catchgrids,file=file.path(fd,paste('Figure3',yrs[i],'.rds')))
#		pdf(file.path(figdir,paste0("FisheryFootprint",yrs[i],".pdf")))
		LobsterMap('27-33',poly.lst=catchgrids)
	  	title(yrs[i],line=-3,cex.main=2,adj=0.3)
	    SpatialHub::contLegend('bottomright',lvls=catchgrids$lvls/1000,Cont.data=catchgrids,title="Catch (tons)",inset=0.02,cex=0.8,bg='white')
#	    dev.off()
	}

	## Fishery Footprint - CPUE
	
	cpueLevels = c(0,0.2,0.4,0.6,0.8,0.9,1,2,3)
	yrs = 2011:2016
	#logsInSeason$logCPUE = log(logsInSeason$CPUE+1)
	for(i in 1:length(yrs)){
	  cpuegrids = lobGridPlot(subset(logsInSeason,LFA%in%p$lfas&SYEAR == yrs[i],c("LFA","GRID_NUM","CPUE")),FUN=median,lvls=cpueLevels)	
#	  pdf(file.path(figdir,paste0("FishFootcpue", yrs[i],".pdf")))
 	saveRDS(cpuegrids,file=file.path(fd,paste('Figure5',yrs[i],'.rds')))
#	
	  LobsterMap('27-33',poly.lst=cpuegrids)
	  	title(yrs[i],line=-3,cex.main=2,adj=0.3)
	  SpatialHub::contLegend('bottomright',lvls=cpuegrids$lvls,Cont.data=cpuegrids,title="CPUE (kg/TH)",inset=0.02,cex=0.8,bg='white')
#	  dev.off()
	}
	
	## Fishery Footprint - Mean Pots Hauled 
	
	potLevels = c (0,1000,100000,200000,300000,400000,500000,600000)
	yrs = 2011:2016
	for(i in 1:length(yrs)){
	potgrids = lobGridPlot(subset(logsInSeason,LFA%in%p$lfas&SYEAR == yrs[i],c("LFA","GRID_NUM","NUM_OF_TRAPS")),FUN=sum,lvls=potLevels) 
 	saveRDS(potgrids,file=file.path(fd,paste('Figure4',yrs[i],'.rds')))

	#pdf(file.path(figdir,paste0("FishFootpot", yrs[i],".pdf")))
	LobsterMap('27-33',poly.lst=potgrids)
	  	title(yrs[i],line=-3,cex.main=2,adj=0.3)
	SpatialHub::contLegend('bottomright',lvls=potgrids$lvls/1000,Cont.data=potgrids,title="Pots Hauled (000s)",inset=0.02,cex=0.8,bg='white')
	#dev.off()
	}
	 
	
	## Fishery Footprint - Days Fished
	daysLevels = c(0,500,1000,1500,2000,2500,3000)
	daysFished<-aggregate(DATE_FISHED ~ SYEAR + LFA + GRID_NUM + LICENCE_ID, data=logsInSeason,FUN= function(x) length(unique(x)))	
	yrs = 2011:2016
	for (i in 1: length(yrs)){
	  daysgrids = lobGridPlot(subset(daysFished, LFA%in%p$lfas&SYEAR == yrs[i],c("LFA", "GRID_NUM", "DATE_FISHED")),FUN=sum, lvls= daysLevels)
	saveRDS(daysgrids,file=file.path(fd,paste('Figure6',yrs[i],'.rds')))

#	  pdf(file.path(figdir,paste0("FishFootDaysFished", yrs[i],".pdf")))
	  LobsterMap('27-33',poly.lst=daysgrids)
	  	title(yrs[i],line=-3,cex.main=2,adj=0.3)
	  SpatialHub::contLegend('bottomright',lvls=daysgrids$lvls,Cont.data=daysgrids,title="Total Days Fished",inset=0.02,cex=0.8,bg='white')
#	  dev.off()
	}
	
	
	## Fishery Footprint - Licences Fished
	
	licenceLevels = c(0,15,30,45,60,75,90,105,120)
	yrs=2011:2016
	daysFished$LICENCE<-1
	for(i in 1: length(yrs)){
	  licencegrids = lobGridPlot(subset(daysFished, LFA%in%p$lfas&SYEAR==yrs[i], c("LFA", "GRID_NUM", "LICENCE")), FUN=sum, lvls= licenceLevels)
		saveRDS(licencegrids,file=file.path(fd,paste('Figure7',yrs[i],'.rds')))

	 #pdf(file.path(figdir,paste0("FishFootLicenceFished", yrs[i],".pdf")))
	 LobsterMap('27-33', poly.lst=licencegrids)
	  	title(yrs[i],line=-3,cex.main=2,adj=0.3)
	 SpatialHub::contLegend('bottomright', lvls=licencegrids$lvls, Cont.data=licencegrids, title= "Number of Licence Fished", inset =0.02,cex=0.8,bg='white')
  dev.off()
  }
	
	
	###Models need to be fixed for following 

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





