    
  
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
	
	AICs = data.frame(rbind(AICs1,AICs2,AICs3,AICs4))
	names(AICs) = p$subareas
	AICs
	sweep(AICs,2,FUN='-',apply(AICs,2,min))


	for(i in 1:length(CPUEModelResults))

	CPUECombinedModelResults = CPUEmodel(mf5,CPUE.data,combined=T)	

	cpue1c=CPUEModelPlot(CPUECombinedModelResults,TempModelling,combined=T,lfa = c("27N","27S", "28", "29", "30"),xlim=c(2010,2016.4),ylim=c(0,10.5),graphic='R',path=figdir,lab='1c')
	cpue2c=CPUEModelPlot(CPUECombinedModelResults,TempModelling,combined=T,lfa = c("31A", "31B", "32", "33E", "33W"),xlim=c(2010,2016.4),ylim=c(0,10.5),graphic='pdf',path=figdir,lab='2c')

	out=CPUEModelPlot(CPUEModelResults,TempModelling,lfa = c("33W","33E"),xlim=c(2014,2017.5),ylim=c(0,20),wd=15)
	out1=CPUEModelPlot(CPUEModelResults,TempModelling,lfa = c("27N","27S", "28", "29", "30"),xlim=c(2010,2016.4),ylim=c(0,10.5))
	out2=CPUEModelPlot(CPUEModelResults,TempModelling,lfa = c("31A", "31B", "32", "33E", "33W"),xlim=c(2010,2016.4),ylim=c(0,10.5))
	cpue1=CPUEModelPlot(CPUEModelResults1,TempModelling,lfa = c("27N","27S", "28", "29", "30"),xlim=c(2010,2016.4),ylim=c(0,10.5),graphic='pdf',path=figdir,lab=1)
	cpue2=CPUEModelPlot(CPUEModelResults1,TempModelling,lfa = c("31A", "31B", "32", "33E", "33W"),xlim=c(2010,2016.4),ylim=c(0,10.5),graphic='pdf',path=figdir,lab=2)



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

	pdf(file.path( figdir,"FSRSmodelBayes.pdf"),8, 10)

	sp <- ggplot()
	sp <- sp + geom_point(data = shorts, aes(y = median, x = YEAR), shape = 16, size = 2)
	sp <- sp + xlab("Year") + ylab("Lobsters / Trap")
	sp <- sp + theme(text = element_text(size=15)) + theme_bw()
	sp <- sp + geom_line(data = shorts, aes(x = YEAR, y = median), colour = "black")
	sp <- sp + geom_ribbon(data = shorts, aes(x = YEAR, ymax = ub, ymin = lb ), alpha = 0.5)
	sp <- sp + facet_wrap(  ~Area, ncol=2,scales = "fixed")
	sp

	lp <- ggplot()
	lp <- lp + geom_point(data = legals, aes(y = median, x = YEAR), shape = 16, size = 2)
	lp <- lp + xlab("Year") + ylab("Lobsters / Trap")
	lp <- lp + theme(text = element_text(size=15)) + theme_bw()
	lp <- lp + geom_line(data = legals, aes(x = YEAR, y = median), colour = "black")
	lp <- lp + geom_ribbon(data = legals, aes(x = YEAR, ymax = ub, ymin = lb ), alpha = 0.5)
	lp <- lp + facet_wrap(  ~Area, ncol=2,scales = "fixed")
	lp

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

		FSRSModelResultsShortComm[[i]]=FSRSmodel(mdata, response="SHORTS",interaction=F,type="bayesian",iter=5000,tag="Comm",ptraps=1000)
		pdata	= 	FSRSModelResultsShortComm[[i]]$pData
		pdata$Area = cssa[i]
		shortsComm.lst[[i]] = pdata

		FSRSModelResultsLegalComm[[i]]=FSRSmodel(mdata, response="LEGALS",interaction=F,type="bayesian",iter=5000,tag="Comm",ptraps=1000)
		pdata	= 	FSRSModelResultsLegalComm[[i]]$pData
		pdata$Area = cssa[i]
		legalsComm.lst[[i]] = pdata

		FSRSModelResultsRecruitComm[[i]]=FSRSmodel(mdata, response="RECRUITS",interaction=F,type="bayesian",iter=5000,tag="Comm",ptraps=1000)
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

	pdf(file.path( figdir,"FSRSmodelBayesComm.pdf"),8, 2.5)

	sp <- ggplot()
	sp <- sp + geom_point(data = shortsComm, aes(y = median, x = YEAR), shape = 16, size = 2)
	sp <- sp + xlab("Year") + ylab("Lobsters / Trap") + xlim(1999,2016)
	sp <- sp + theme(text = element_text(size=15)) + theme_bw()
	sp <- sp + geom_line(data = shortsComm, aes(x = YEAR, y = median), colour = "black")
	sp <- sp + geom_ribbon(data = shortsComm, aes(x = YEAR, ymax = ub, ymin = lb ), alpha = 0.5)
	sp <- sp + facet_wrap(  ~Area, ncol=2,scales = "fixed")
	sp

	lp <- ggplot()
	lp <- lp + geom_point(data = legalsComm, aes(y = median, x = YEAR), shape = 16, size = 2)
	lp <- lp + xlab("Year") + ylab("Lobsters / Trap") + xlim(1999,2016)
	lp <- lp + theme(text = element_text(size=15)) + theme_bw()
	lp <- lp + geom_line(data = legalsComm, aes(x = YEAR, y = median), colour = "black")
	lp <- lp + geom_ribbon(data = legalsComm, aes(x = YEAR, ymax = ub, ymin = lb ), alpha = 0.5)
	lp <- lp + facet_wrap(  ~Area, ncol=2,scales = "fixed")
	lp

	rp <- ggplot()
	rp <- rp + geom_point(data = recruitComm, aes(y = median, x = YEAR), shape = 16, size = 2)
	rp <- rp + xlab("Year") + ylab("Lobsters / Trap") + xlim(1999,2016)
	rp <- rp + theme(text = element_text(size=15)) + theme_bw()
	rp <- rp + geom_line(data = recruitComm, aes(x = YEAR, y = median), colour = "black")
	rp <- rp + geom_ribbon(data = recruitComm, aes(x = YEAR, ymax = ub, ymin = lb ), alpha = 0.5)
	rp <- rp + facet_wrap(  ~Area, ncol=2,scales = "fixed")
	rp

	dev.off()






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



	p$lfas = c("27N","27S", "28", "29", "30", "31A", "31B", "32", "33E", "33W") 

								# carapace length bins (mm)
	TempModelling = TempModel(areas = 'subarea')
	p$TempModel = TempModelling$Model
	moltModel = moltPrModel(p,redo.dd=F)
	p$moltPrModel = moltModel # degree day growth


	plist = getSimList(p,sex=1)

	
	DTs = list()
	dt = c()

for(l in 1:length(p$lfas)){

	plist[[l]]$ddoy = cumsum(plist[[l]]$dailytemps)
	for(i in 1:length(plist[[l]]$lens))	{
		dt[i] = min(which(pPrMolt(plist[[l]],cl=plist[[l]]$lens[i])>0.5))
	}
	names(dt) = plist[[l]]$lens

	DTs[[l]] = dt

}

names(DTs) = p$lfas

save(DTs,file="deltaTs.rdata")

	
 plot(p$lens,dt2,type='l',ylim=c(0,1000),xlab='CL (mm)',ylab='days')
 lines(p$lens,dt1,lty=2)
 lines(p$lens,dt3,lty=2)


plot(yrs,rowSums(males$finalPop),type='l')
lines(yrs,rowSums(females$finalPop+females$finalBerried),lty=2)

# VB
Linf=c(281,207)
k=c(0.065,0.089)
t0=c(0.76,0.42)
age=seq(1,23,0.1)


BubblePlotCLF(list(x),bins=bins,yrs=yrs,log.trans=T,filen='',prop=F,LS=82.5,inch=0.2,bg=rgb(1,0,0,0.1),graphic="R")

		lines(age-3.2,lvb(age,Linf[2],k[2],t0[2]))

BubblePlotCLF(list(y),bins=bins,yrs=yrs,log.trans=T,filen='',prop=F,LS=82.5,inch=0.2,bg=rgb(0,0,1,0.1),graphic="R")
		
		lines(age-3.5,lvb(age,Linf[1],k[1],t0[1]))

BubblePlotCLF(list(z),bins=bins,yrs=yrs,log.trans=T,filen='',prop=F,LS=82.5,inch=0.2,bg=rgb(1,0,1,0.1),graphic="R")

BubblePlotCLF(list(x+z),bins=bins,yrs=yrs,log.trans=T,filen='',prop=F,LS=82.5,inch=0.2,bg=rgb(1,0,0,0.1),graphic="R")

	
#BubblePlotCLF(list(males$finalPop),bins=bins,yrs=yrs,log.trans=T,filen='1',prop=F,LS=82.5,inch=0.2,bg=rgb(0,0,1,0.1),graphic="pdf",ylim=c(40,150),xlim=c(0,10))
BubblePlotCLF(list(y),bins=bins,yrs=yrs,log.trans=T,filen='2',prop=F,LS=82.5,inch=0.2,bg=rgb(0,0,1,0.1),graphic="pdf",ylim=c(40,150),xlim=c(0,10))



################################


bpCLF = 

BarPlotCLF2(bpCLF,yrs=1:20,bins=p$lens,filen=,LS=p$LS )
	

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

		age=seq(4,23,0.1)
		lines(age-3,lvb(age,Linf[1],k[1],t0[1])



	### Molt probs

	x11()

	par(mfrow=c(2,1))

	p$moltPr = list(a=-9,b=0.02,x=0.5)

	moltProbPlot(p)

	p$moltPr = list(a=-15,b=0.002,x=0.5) # degree day growth

	moltProbPlot(p,gdd=T)


	x11()

	as=c(-25,-20,-15)
	bs=c(0.0025,0.003,0.0035)

	l=length(as)
	par(mfrow=c(l,l))

	for(i in 1:l){
		for(j in 1:l){
			p$moltPr = list(a=as[i],b=bs[j],x=0.7) # degree day growth

			moltProbPlot(p,gdd=T,main=paste('a =',as[i],', b =',bs[j]))

		}
	}


	x11()

	as=c(-15,-10,-5)
	bs=c(0.015,0.02,0.025)

	l=length(as)
	par(mfrow=c(l,l))

	for(i in 1:l){
		for(j in 1:l){
			p$moltPr = list(a=as[i],b=bs[j],x=0.5) # degree day growth

			moltProbPlot(p,gdd=F,main=paste('a =',as[i],', b =',bs[j]))

		}
	}


	p$moltPr = list(a=-9,b=0.0013,x=1.2) # degree day growth

	moltProbPlot(p,gdd=T)
