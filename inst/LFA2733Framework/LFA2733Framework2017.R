    
  
	p = bio.lobster::load.environment()
	la()


    p$syr = 2005
    p$yrs = p$syr:p$current.assessment.year

    figdir = file.path(project.datadirectory("bio.lobster"),"figures","LFA2733Framework2018")

    p$lfas = c("27", "28", "29", "30", "31A", "31B", "32", "33") # specify lfas for data summary
  
    p$subareas = c("27N","27S", "28", "29", "30", "31A", "31B", "32", "33E", "33W") # specify lfas for data summary

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
	#mf2 = formula(logWEIGHT ~ fYEAR + DOS + TEMP)
	#mf3 = formula(logWEIGHT ~ fYEAR + DOS)
	#mf4 = formula(logWEIGHT ~ fYEAR + TEMP)
	#mf5 = formula(logWEIGHT ~ fYEAR + DOS + TEMP + (1 | fYEAR/fAREA)) # combined


	TempModelling = TempModel( annual.by.area=F)
	#CPUE.data<-CPUEModelData(p,redo=T,TempModelling)
	CPUE.data<-CPUEModelData(p,redo=T)
	
	#CPUE.data$WEIGHT_KG = CPUE.data$TOTAL_WEIGHT_KG
    cpueSubArea.dat = CPUEplot(CPUE.data,subarea= p$subareas,yrs=1981:2016,graphic='R')


	CPUEModelResults1 = list()
	#CPUEModelResults2 = list()
	#CPUEModelResults3 = list()
	#CPUEModelResults4 = list()
	#AICs1 = c()
	#AICs2 = c()
	#AICs3 = c()
	#AICs4 = c()
	for(i in 1:length( p$subareas)){
	#for(i in 1:length( p$lfas)){

		mdata = subset(CPUE.data,subarea==p$subareas[i])
		#mdata = subset(CPUE.data,LFA==p$lfas[i])
		#CPUEModelResults1[[i]] = CPUEmodel(mf1,mdata,lfa=p$lfas[i])
		CPUEModelResults1[[i]] = CPUEmodel(mf1,mdata)
		#CPUEModelResults2[[i]] = CPUEmodel(mf2,mdata)
		#CPUEModelResults3[[i]] = CPUEmodel(mf3,mdata)
		#CPUEModelResults4[[i]] = CPUEmodel(mf4,mdata)
		#AICs1[i] = CPUEModelResults1[[i]]$model$aic
		#AICs2[i] = CPUEModelResults2[[i]]$model$aic
		#AICs3[i] = CPUEModelResults3[[i]]$model$aic
		#AICs4[i] = CPUEModelResults4[[i]]$model$aic


	}
	names(CPUEModelResults1) = p$subareas
	#names(CPUEModelResults2) = p$subareas
	#names(CPUEModelResults3) = p$subareas
	#names(CPUEModelResults4) = p$subareas
	
	#AICs = data.frame(rbind(AICs1,AICs2,AICs3,AICs4))
	#names(AICs) = p$subareas
	#AICs
	#sweep(AICs,2,FUN='-',apply(AICs,2,min))



	#CPUECombinedModelResults = CPUEmodel(mf5,CPUE.data,combined=T)	

	#cpue1c=CPUEModelPlot(CPUECombinedModelResults,TempModelling,combined=T,lfa = c("27N","27S", "28", "29", "30"),xlim=c(2010,2016.4),ylim=c(0,10.5),graphic='R',path=figdir,lab='1c')
	#cpue2c=CPUEModelPlot(CPUECombinedModelResults,TempModelling,combined=T,lfa = c("31A", "31B", "32", "33E", "33W"),xlim=c(2010,2016.4),ylim=c(0,10.5),graphic='pdf',path=figdir,lab='2c')

	#out=CPUEModelPlot(CPUEModelResults,TempModelling,lfa = c("33W","33E"),xlim=c(2014,2017.5),ylim=c(0,20),wd=15)
	#out1=CPUEModelPlot(CPUEModelResults,TempModelling,lfa = c("27N","27S", "28", "29", "30"),xlim=c(2010,2016.4),ylim=c(0,10.5))
	#out2=CPUEModelPlot(CPUEModelResults,TempModelling,lfa = c("31A", "31B", "32", "33E", "33W"),xlim=c(2010,2016.4),ylim=c(0,10.5))
	cpue1=CPUEModelPlot(CPUEModelResults1,TempModelling,lfa = c("27N","27S", "28", "29", "30"),xlim=c(2010,2016.4),ylim=c(0,10.5),graphic='R',path=figdir,lab=1)
	cpue2=CPUEModelPlot(CPUEModelResults1,TempModelling,lfa = c("31A", "31B", "32", "33E", "33W"),xlim=c(2010,2016.4),ylim=c(0,10.5),graphic='R',path=figdir,lab=2)
	#cpue1=CPUEModelPlot(CPUEModelResults1,TempModelling,lfa = c("27", "28", "29", "30"),xlim=c(2010,2016.4),ylim=c(0,10.5),graphic='R',path=figdir,lab=1)
	#cpue2=CPUEModelPlot(CPUEModelResults1,TempModelling,lfa = c("31A", "31B", "32", "33"),xlim=c(2010,2016.4),ylim=c(0,10.5),graphic='R',path=figdir,lab=2)
	cpue=rbind(cpue1,cpue2)

	cpue.annual=list()
	for(i in 1:length(p$subareas)){

	 	mu=with(subset(cpue,LFA==p$subareas[i]),tapply(mu,YEAR,mean))
	 	mu.sd=with(subset(cpue,LFA==p$subareas[i]),tapply(mu,YEAR,sd))
	 	cpue.annual[[i]] = data.frame(Area=p$subareas[i],Year=as.numeric(names(mu)),CPUE=mu,CPUE.sd=mu.sd)
   	
	}
	cpueModel = subset(do.call("rbind",cpue.annual),Year<2017)
	x11()
	pdf(file.path( figdir,"CPUEmodelAnnualIndex.pdf"),8, 10)
	par(mfrow=c(length(p$subareas),1),mar=c(0,0,0,0),omi=c(0.5,1,0.5,0.5),las=1)

	for(i in 1:length(p$subareas)){

		plot(CPUE~Year,subset(cpueModel,Area==p$subareas[i]),type='b',pch=21,bg='red',ylim=c(0,max(cpueModel$CPUE+cpueModel$CPUE.sd,na.rm=T)),xlim=c(min(cpueModel$Year),max(cpueModel$Year)),xaxt='n')
		points(CPUE~YEAR,subset(cpueSubArea.dat$annual.dat,LFA==p$subareas[i]&YEAR<2017),pch=16,col='blue',cex=0.9)
		lines(CPUE+CPUE.sd~Year,subset(cpueModel,Area==p$subareas[i]),lty=2)
		lines(CPUE-CPUE.sd~Year,subset(cpueModel,Area==p$subareas[i]),lty=2)
		axis(1,lab=F)
		axis(4)
		if(i==length(p$subareas))axis(1)
		
		text(min(cpueModel$Year,na.rm=T),max(cpueModel$CPUE,na.rm=T)*.8,paste(p$subareas[i]),cex=2,pos=4)
	}
	mtext("CPUE (kg/TH)", 2, 3, outer = T, cex = 1,las=0)	
	dev.off()
	
	cpueData2=    CPUEplot(CPUE.data,lfa= p$lfas,yrs=1981:2016,graphic='R')$annual.data

	save(list=c("cpueModel","cpueData"),file=file.path(project.datadirectory("bio.lobster"),"outputs","cpueIndicators.rdata"))
	save(cpueData2,file=file.path(project.datadirectory("bio.lobster"),"outputs","cpueIndicators2.rdata"))
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
    p$subareas = c("27N","27S", "28", "29", "30", "31A", "31B", "32", "33E", "33W") # specify lfas for data summary

	#Base

	FSRSvesday<-FSRSModelData()
	FSRSvesdayComm<-FSRSModelData(trap.type="commercial")
	FSRSModelResultsRecruit = list()
	FSRSModelResultsShort = list()
	FSRSModelResultsLegal = list()
	shorts.lst = list()
	legals.lst = list()
	recruit.lst = list()

	for(i in 1:length( p$subareas)){

		mdata = subset(FSRSvesday,subarea==p$subareas[i])

		FSRSModelResultsShort[[i]]=FSRSmodel(mdata, response="SHORTS",interaction=F)
		pdata	= 	FSRSModelResultsShort[[i]]$pData
		pdata$Area = p$subareas[i]
		shorts.lst[[i]] = pdata

		FSRSModelResultsLegal[[i]]=FSRSmodel(mdata, response="LEGALS",interaction=F)
		pdata	= 	FSRSModelResultsLegal[[i]]$pData
		pdata$Area = p$subareas[i]
		legals.lst[[i]] = pdata

		FSRSModelResultsRecruit[[i]]=FSRSmodel(mdata, response="RECRUITS",interaction=F)
		pdata	= 	FSRSModelResultsRecruit[[i]]$pData
		pdata$Area = p$subareas[i]
		recruit.lst[[i]] = pdata


	}

	names(FSRSModelResultsShort) = p$subareas
	names(FSRSModelResultsLegal) = p$subareas
	names(FSRSModelResultsRecruit) = p$subareas
	
	shorts = do.call("rbind",shorts.lst)
	legals = do.call("rbind",legals.lst)
	recruit = do.call("rbind",recruit.lst)

	library(ggplot2)

	pdf(file.path( figdir,"FSRSmodelBase.pdf"),8, 10)

	sp <- ggplot()
	sp <- sp + geom_point(data = shorts, aes(y = mu, x = YEAR), shape = 16, size = 2)
	sp <- sp + xlab("Year") + ylab("Lobsters / Trap")
	sp <- sp + theme(text = element_text(size=15)) + theme_bw()
	sp <- sp + geom_line(data = shorts, aes(x = YEAR, y = mu), colour = "black")
	sp <- sp + geom_ribbon(data = shorts, aes(x = YEAR, ymax = ub, ymin = lb ), alpha = 0.5)
	sp <- sp + facet_wrap(  ~Area, ncol=2,scales = "fixed")
	sp

	lp <- ggplot()
	lp <- lp + geom_point(data = legals, aes(y = mu, x = YEAR), shape = 16, size = 2)
	lp <- lp + xlab("Year") + ylab("Lobsters / Trap")
	lp <- lp + theme(text = element_text(size=15)) + theme_bw()
	lp <- lp + geom_line(data = legals, aes(x = YEAR, y = mu), colour = "black")
	lp <- lp + geom_ribbon(data = legals, aes(x = YEAR, ymax = ub, ymin = lb ), alpha = 0.5)
	lp <- lp + facet_wrap(  ~Area, ncol=2,scales = "fixed")
	lp

	rp <- ggplot()
	rp <- rp + geom_point(data = recruit, aes(y = mu, x = YEAR), shape = 16, size = 2)
	rp <- rp + xlab("Year") + ylab("Lobsters / Trap")
	rp <- rp + theme(text = element_text(size=15)) + theme_bw()
	rp <- rp + geom_line(data = recruit, aes(x = YEAR, y = mu), colour = "black")
	rp <- rp + geom_ribbon(data = recruit, aes(x = YEAR, ymax = ub, ymin = lb ), alpha = 0.5)
	rp <- rp + facet_wrap(  ~Area, ncol=2,scales = "fixed")
	rp

	dev.off()



	#Bayes

	FSRSvesday<-FSRSModelData()
	FSRSModelResultsRecruit = list()
	FSRSModelResultsShort = list()
	FSRSModelResultsLegal = list()
	shorts.lst = list()
	legals.lst = list()
	recruit.lst = list()

	for(i in 1:length( p$subareas)){
	st = Sys.time()

		mdata = subset(FSRSvesday,subarea==p$subareas[i])

		FSRSModelResultsShort[[i]]=FSRSmodel(mdata, response="SHORTS",interaction=F,type="bayesian",iter=5000,redo=F,ptraps=1000)
		pdata	= 	FSRSModelResultsShort[[i]]$pData
		pdata$Area = p$subareas[i]
		shorts.lst[[i]] = pdata

		FSRSModelResultsLegal[[i]]=FSRSmodel(mdata, response="LEGALS",interaction=F,type="bayesian",iter=5000,redo=F,ptraps=1000)
		pdata	= 	FSRSModelResultsLegal[[i]]$pData
		pdata$Area = p$subareas[i]
		legals.lst[[i]] = pdata

		FSRSModelResultsRecruit[[i]]=FSRSmodel(mdata, response="RECRUITS",interaction=F,type="bayesian",iter=5000,redo=F,ptraps=1000)
		pdata	= 	FSRSModelResultsRecruit[[i]]$pData
		pdata$Area = p$subareas[i]
		recruit.lst[[i]] = pdata
		print( Sys.time() - st)


	}

	names(FSRSModelResultsShort) = p$subareas
	names(FSRSModelResultsLegal) = p$subareas
	names(FSRSModelResultsRecruit) = p$subareas
	
	shorts = do.call("rbind",shorts.lst)
	legals = do.call("rbind",legals.lst)
	recruit = do.call("rbind",recruit.lst)

	library(ggplot2)

	pdf(file.path( figdir,"FSRSmodelBayesShorts.pdf"),8, 10)
	sp <- ggplot()
	sp <- sp + geom_point(data = shorts, aes(y = median, x = YEAR), shape = 16, size = 2)
	sp <- sp + xlab("Year") + ylab("Lobsters / Trap")
	sp <- sp + theme(text = element_text(size=15)) + theme_bw()
	sp <- sp + geom_line(data = shorts, aes(x = YEAR, y = median), colour = "black")
	sp <- sp + geom_ribbon(data = shorts, aes(x = YEAR, ymax = ub, ymin = lb ), alpha = 0.5)
	sp <- sp + facet_wrap(  ~Area, ncol=2,scales = "fixed")
	sp
	dev.off()

	pdf(file.path( figdir,"FSRSmodelBayesLegals.pdf"),8, 10)
	lp <- ggplot()
	lp <- lp + geom_point(data = legals, aes(y = median, x = YEAR), shape = 16, size = 2)
	lp <- lp + xlab("Year") + ylab("Lobsters / Trap")
	lp <- lp + theme(text = element_text(size=15)) + theme_bw()
	lp <- lp + geom_line(data = legals, aes(x = YEAR, y = median), colour = "black")
	lp <- lp + geom_ribbon(data = legals, aes(x = YEAR, ymax = ub, ymin = lb ), alpha = 0.5)
	lp <- lp + facet_wrap(  ~Area, ncol=2,scales = "fixed")
	lp
	dev.off()

	pdf(file.path( figdir,"FSRSmodelBayesRecruits.pdf"),8, 10)
	rp <- ggplot()
	rp <- rp + geom_point(data = recruit, aes(y = median, x = YEAR), shape = 16, size = 2)
	rp <- rp + xlab("Year") + ylab("Lobsters / Trap")
	rp <- rp + theme(text = element_text(size=15)) + theme_bw()
	rp <- rp + geom_line(data = recruit, aes(x = YEAR, y = median), colour = "black")
	rp <- rp + geom_ribbon(data = recruit, aes(x = YEAR, ymax = ub, ymin = lb ), alpha = 0.5)
	rp <- rp + facet_wrap(  ~Area, ncol=2,scales = "fixed")
	rp

	dev.off()


	FSRSvesdayComm = FSRSModelData(trap.type="commercial")

	cssa = c("33W","33E")
	FSRSModelResultsRecruitComm = list()
	FSRSModelResultsShortComm = list()
	FSRSModelResultsLegalComm = list()
	shortsComm.lst = list()
	legalsComm.lst = list()
	recruitComm.lst = list()

	for(i in 1:2){
		st = Sys.time()

		mdata = subset(FSRSvesdayComm,subarea==cssa[i])

		FSRSModelResultsShortComm[[i]]=FSRSmodel(mdata, response="SHORTS",interaction=F,type="bayesian",iter=5000,redo=F,tag="Comm",ptraps=1000)
		pdata	= 	FSRSModelResultsShortComm[[i]]$pData
		pdata$Area = cssa[i]
		shortsComm.lst[[i]] = pdata

		FSRSModelResultsLegalComm[[i]]=FSRSmodel(mdata, response="LEGALS",interaction=F,type="bayesian",iter=5000,redo=F,tag="Comm",ptraps=1000)
		pdata	= 	FSRSModelResultsLegalComm[[i]]$pData
		pdata$Area = cssa[i]
		legalsComm.lst[[i]] = pdata

		FSRSModelResultsRecruitComm[[i]]=FSRSmodel(mdata, response="RECRUITS",interaction=F,type="bayesian",iter=5000,redo=F,tag="Comm",ptraps=1000)
		pdata	= 	FSRSModelResultsRecruitComm[[i]]$pData
		pdata$Area = cssa[i]
		recruitComm.lst[[i]] = pdata
		print( Sys.time() - st)
	}


	names(FSRSModelResultsShortComm) = cssa
	names(FSRSModelResultsLegalComm) = cssa
	names(FSRSModelResultsRecruitComm) = cssa
	
	shortsComm = do.call("rbind",shortsComm.lst)
	legalsComm = do.call("rbind",legalsComm.lst)
	recruitComm = do.call("rbind",recruitComm.lst)

	library(ggplot2)

	pdf(file.path( figdir,"FSRSmodelBayesCommShorts.pdf"),8, 2.5)

	sp <- ggplot()
	sp <- sp + geom_point(data = shortsComm, aes(y = median, x = YEAR), shape = 16, size = 2)
	sp <- sp + xlab("") + ylab("") + xlim(1999,2016)
	sp <- sp + theme(text = element_text(size=15)) + theme_bw()
	sp <- sp + geom_line(data = shortsComm, aes(x = YEAR, y = median), colour = "black")
	sp <- sp + geom_ribbon(data = shortsComm, aes(x = YEAR, ymax = ub, ymin = lb ), alpha = 0.5)
	sp <- sp + facet_wrap(  ~Area, ncol=2,scales = "fixed")
	sp
	dev.off()

	pdf(file.path( figdir,"FSRSmodelBayesCommLegals.pdf"),8, 2.5)
	lp <- ggplot()
	lp <- lp + geom_point(data = legalsComm, aes(y = median, x = YEAR), shape = 16, size = 2)
	lp <- lp + xlab("") + ylab("Lobsters / Trap") + xlim(1999,2016)
	lp <- lp + theme(text = element_text(size=15)) + theme_bw()
	lp <- lp + geom_line(data = legalsComm, aes(x = YEAR, y = median), colour = "black")
	lp <- lp + geom_ribbon(data = legalsComm, aes(x = YEAR, ymax = ub, ymin = lb ), alpha = 0.5)
	lp <- lp + facet_wrap(  ~Area, ncol=2,scales = "fixed")
	lp
	dev.off()

	pdf(file.path( figdir,"FSRSmodelBayesCommRecruits.pdf"),8, 2.5)
	rp <- ggplot()
	rp <- rp + geom_point(data = recruitComm, aes(y = median, x = YEAR), shape = 16, size = 2)
	rp <- rp + xlab("Year") + ylab("") + xlim(1999,2016)
	rp <- rp + theme(text = element_text(size=15)) + theme_bw()
	rp <- rp + geom_line(data = recruitComm, aes(x = YEAR, y = median), colour = "black")
	rp <- rp + geom_ribbon(data = recruitComm, aes(x = YEAR, ymax = ub, ymin = lb ), alpha = 0.5)
	rp <- rp + facet_wrap(  ~Area, ncol=2,scales = "fixed")
	rp

	dev.off()
	
	FSRSvesday<-FSRSModelData()
	FSRSModelResultsRecruitLFA = list()
	FSRSModelResultsShortLFA = list()
	FSRSModelResultsLegalLFA = list()
	shortsLFA.lst = list()
	legalsLFA.lst = list()
	recruitLFA.lst = list()

	for(i in 1:length( p$lfas)){
	st = Sys.time()

		mdata = subset(FSRSvesday,LFA==p$lfas[i])

		FSRSModelResultsShortLFA[[i]]=FSRSmodel(mdata, lfa=p$lfa[i],response="SHORTS",interaction=F,type="bayesian",iter=5000,redo=F,ptraps=1000)
		pdata	= 	FSRSModelResultsShortLFA[[i]]$pData
		pdata$Area = p$lfas[i]
		shortsLFA.lst[[i]] = pdata
		print( st <- Sys.time() - st)

		FSRSModelResultsLegalLFA[[i]]=FSRSmodel(mdata, lfa=p$lfa[i], response="LEGALS",interaction=F,type="bayesian",iter=5000,redo=F,ptraps=1000)
		pdata	= 	FSRSModelResultsLegalLFA[[i]]$pData
		pdata$Area = p$lfas[i]
		legalsLFA.lst[[i]] = pdata
		print( st <- Sys.time() - st)

		FSRSModelResultsRecruitLFA[[i]]=FSRSmodel(mdata, lfa=p$lfa[i], response="RECRUITS",interaction=F,type="bayesian",iter=5000,redo=F,ptraps=1000)
		pdata	= 	FSRSModelResultsRecruitLFA[[i]]$pData
		pdata$Area = p$lfas[i]
		recruitLFA.lst[[i]] = pdata
		print( st <- Sys.time() - st)


	}

	names(FSRSModelResultsShortLFA) = p$lfas
	names(FSRSModelResultsLegalLFA) = p$lfas
	names(FSRSModelResultsRecruitLFA) = p$lfas
	
	shortsLFA = do.call("rbind",shortsLFA.lst)
	legalsLFA = do.call("rbind",legalsLFA.lst)
	recruitLFA = do.call("rbind",recruitLFA.lst)

 	save(list=c("shortsLFA","legalsLFA","recruitLFA"),file=file.path(project.datadirectory("bio.lobster"),"outputs","fsrsModelIndicators.rdata"))

	pdf(file.path( figdir,"FSRSmodelBayesLFAShorts.pdf"),8, 2.5)

	sp <- ggplot()
	sp <- sp + geom_point(data = shortsLFA, aes(y = median, x = YEAR), shape = 16, size = 2)
	sp <- sp + xlab("") + ylab("") + xlim(1999,2016)
	sp <- sp + theme(text = element_text(size=15)) + theme_bw()
	sp <- sp + geom_line(data = shortsLFA, aes(x = YEAR, y = median), colour = "black")
	sp <- sp + geom_ribbon(data = shortsLFA, aes(x = YEAR, ymax = ub, ymin = lb ), alpha = 0.5)
	sp <- sp + facet_wrap(  ~Area, ncol=2,scales = "fixed")
	sp
	dev.off()

	pdf(file.path( figdir,"FSRSmodelBayesLFALegals.pdf"),8, 2.5)
	lp <- ggplot()
	lp <- lp + geom_point(data = legalsLFA, aes(y = median, x = YEAR), shape = 16, size = 2)
	lp <- lp + xlab("") + ylab("Lobsters / Trap") + xlim(1999,2016)
	lp <- lp + theme(text = element_text(size=15)) + theme_bw()
	lp <- lp + geom_line(data = legalsLFA, aes(x = YEAR, y = median), colour = "black")
	lp <- lp + geom_ribbon(data = legalsLFA, aes(x = YEAR, ymax = ub, ymin = lb ), alpha = 0.5)
	lp <- lp + facet_wrap(  ~Area, ncol=2,scales = "fixed")
	lp
	dev.off()

	pdf(file.path( figdir,"FSRSmodelBayesLFARecruits.pdf"),8, 2.5)
	rp <- ggplot()
	rp <- rp + geom_point(data = recruitLFA, aes(y = median, x = YEAR), shape = 16, size = 2)
	rp <- rp + xlab("Year") + ylab("") + xlim(1999,2016)
	rp <- rp + theme(text = element_text(size=15)) + theme_bw()
	rp <- rp + geom_line(data = recruitLFA, aes(x = YEAR, y = median), colour = "black")
	rp <- rp + geom_ribbon(data = recruitLFA, aes(x = YEAR, ymax = ub, ymin = lb ), alpha = 0.5)
	rp <- rp + facet_wrap(  ~Area, ncol=2,scales = "fixed")
	rp

	dev.off()


	TempModelling = TempModel(areas = 'subarea')
	TempModelPlot(TempModelling,xlim=c(1980,2017),depths=c(5,25,50),Area=c("27N","27S", "29", "30","31A","31B", "32", "33E", "33W"),graphic='png',type=1:2)

 tempModel=TempModelPlot(TempModelling,xlim=c(1980,2017),depths=c(5,25,50),Area=c("27N","27S", "29", "30","31A","31B", "32", "33E", "33W"),graphic='png',type=3)
 tempData=TempModelPlot(TempModelling,xlim=c(1980,2017),depths=c(5,25,50),Area=c("27N","27S", "29", "30","31A","31B", "32", "33E", "33W"),graphic='png',type=4)
 save(list=c("tempModel","tempData"),file=file.path(project.datadirectory("bio.lobster"),"outputs","tempIndicators.rdata"))

 tempData2=TempModelPlot(TempModelling,xlim=c(1980,2017),depths=c(5,25,50),Area=c("27", "33"),lfa=T,graphic='R',type=4)
 save(tempData2,file=file.path(project.datadirectory("bio.lobster"),"outputs","tempIndicators2.rdata"))



######################## sim Molt


# som

	pl = data.frame(LFA=c("LFA27-30","LFA29","LFA28,30","LFA31A","LFA32","LFA32x","LFA33","LFA33x","LFA34"),
		a=c(14.266, 14.173, 16.505, 14.53521, 10.4, 18.99223, 14.23,24.87275,22.37302),
		b=c(-0.1959, -0.1727, -0.2132,-0.20347, -0.112,-0.21128, -0.144,-0.25725,-0.23187))

cl=50:130
plot(cl,seq(0,1,l=length(cl)),type='n',xlab='CL',ylab='SoM')

for(i in 1:nrow(pl)){

	pMat = with(pl[i,],	1/(1+(exp(a+(b*cl)))))	

	lines(cl,pMat,lty=i,col=i)
}

legend('bottomright',pl$LFA,lty=1:nrow(pl),col=1:nrow(pl))



#	p$lfas = c("27N","27S", "28", "29", "30", "31A", "31B", "32", "33E", "33W") 
#
#								# carapace length bins (mm)
#	TempModelling = TempModel(areas = 'subarea')
#	p$TempModel = TempModelling$Model
#	moltModel = moltPrModel(p,redo.dd=F)
#	p$moltPrModel = moltModel # degree day growth
#
#
#	plist = getSimList(p,sex=1)
#
#	
#	DTs = list()
#	dt = c()
#
#for(l in 1:length(p$lfas)){
#
#	plist[[l]]$ddoy = cumsum(plist[[l]]$dailytemps)
#	for(i in 1:length(plist[[l]]$lens))	{
#		dt[i] = min(which(pPrMolt(plist[[l]],cl=plist[[l]]$lens[i])>0.5))
#	}
#	names(dt) = plist[[l]]$lens
#
#	DTs[[l]] = dt
#
#}
#
#names(DTs) = p$lfas
#
#save(DTs,file="deltaTs.rdata")
#
#	
# plot(p$lens,dt2,type='l',ylim=c(0,1000),xlab='CL (mm)',ylab='days')
# lines(p$lens,dt1,lty=2)
# lines(p$lens,dt3,lty=2)
#
#
#plot(yrs,rowSums(males$finalPop),type='l')
#lines(yrs,rowSums(females$finalPop+females$finalBerried),lty=2)
#
## VB
#Linf=c(281,207)
#k=c(0.065,0.089)
#t0=c(0.76,0.42)
#age=seq(1,23,0.1)

