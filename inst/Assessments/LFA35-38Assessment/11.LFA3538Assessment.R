    
  
	p = bio.lobster::load.environment()
	la()
		
	assessment.year = 2019 ########### check the year ############### !!!!!!!!!!!


    p$syr = 1989
    p$yrs = p$syr:assessment.year
ff = "LFA35-38Assessment"

    figdir = file.path(project.figuredirectory('bio.lobster'),ff)

    p$lfas = c( "35", "36", "38") # specify lfas for data summary
    p$subareas = c( "35", "36", "38") # specify lfas for data summary


logsInSeason=lobster.db("process.logs")


	## Fishery Footprint - Landings
	catchLevels = c(0,100000,200000,300000,400000,500000,600000,700000,800000)
	yrs = 2011:2018
	for(i in 1:length(yrs)){
		catchgrids = lobGridPlot(subset(logsInSeason,LFA%in%p$lfas&SYEAR==2016,c("LFA","GRID_NUM","TOTAL_WEIGHT_KG")),FUN=sum,lvls=catchLevels)
		pdf(file.path(figdir,paste0("FisheryFootprint",yrs[i],".pdf")))
		LobsterMap('34-38',poly.lst=catchgrids)
	  	title(yrs[i],line=-3,cex.main=2,adj=0.3)
	    SpatialHub::contLegend('bottomright',lvls=catchgrids$lvls/1000,Cont.data=catchgrids,title="Catch (tons)",inset=0.02,cex=0.8,bg='white')
	    dev.off()
	}

	## Fishery Footprint - CPUE
	
	cpueLevels = c(0,0.2,0.4,0.6,0.8,0.9,1,2,3)
	yrs = 2011:2018
	#logsInSeason$logCPUE = log(logsInSeason$CPUE+1)
	for(i in 1:length(yrs)){
	  cpuegrids = lobGridPlot(subset(logsInSeason,LFA%in%p$lfas&SYEAR == yrs[i],c("LFA","GRID_NUM","CPUE")),FUN=median,lvls=cpueLevels)	
	  pdf(file.path(figdir,paste0("FishFootcpue", yrs[i],".pdf")))
	  LobsterMap('34-38',poly.lst=cpuegrids)
	  	title(yrs[i],line=-3,cex.main=2,adj=0.3)
	  SpatialHub::contLegend('bottomright',lvls=cpuegrids$lvls,Cont.data=cpuegrids,title="CPUE (kg/TH)",inset=0.02,cex=0.8,bg='white')
	  dev.off()
	}
	
	## Fishery Footprint - Mean Pots Hauled 
	
	potLevels = c (0,1000,100000,200000,300000,400000,500000,600000)
	yrs = 2011:2018
	for(i in 1:length(yrs)){
	potgrids = lobGridPlot(subset(logsInSeason,LFA%in%p$lfas&SYEAR == yrs[i],c("LFA","GRID_NUM","NUM_OF_TRAPS")),FUN=sum,lvls=potLevels) 
	pdf(file.path(figdir,paste0("FishFootpot", yrs[i],".pdf")))
	LobsterMap('34-38',poly.lst=potgrids)
	  	title(yrs[i],line=-3,cex.main=2,adj=0.3)
	SpatialHub::contLegend('bottomright',lvls=potgrids$lvls/1000,Cont.data=potgrids,title="Pots Hauled (000s)",inset=0.02,cex=0.8,bg='white')
	dev.off()
	}
	 
	
	## Fishery Footprint - Days Fished
	daysLevels = c(0,500,1000,1500,2000,2500,3000)
	daysFished<-aggregate(DATE_FISHED ~ SYEAR + LFA + GRID_NUM + LICENCE_ID, data=logsInSeason,FUN= function(x) length(unique(x)))	
	yrs = 2011:2018
	for (i in 1: length(yrs)){
	  daysgrids = lobGridPlot(subset(daysFished, LFA%in%p$lfas&SYEAR == yrs[i],c("LFA", "GRID_NUM", "DATE_FISHED")),FUN=sum, lvls= daysLevels)
	  pdf(file.path(figdir,paste0("FishFootDaysFished", yrs[i],".pdf")))
	  LobsterMap('34-38',poly.lst=daysgrids)
	  	title(yrs[i],line=-3,cex.main=2,adj=0.3)
	  SpatialHub::contLegend('bottomright',lvls=daysgrids$lvls,Cont.data=daysgrids,title="Total Days Fished",inset=0.02,cex=0.8,bg='white')
	  dev.off()
	}
	
	
	## Fishery Footprint - Licences Fished
	
	licenceLevels = c(0,15,30,45,60,75,90,105,120)
	yrs=2011:2018
	daysFished$LICENCE<-1
	for(i in 1: length(yrs)){
	  licencegrids = lobGridPlot(subset(daysFished, LFA%in%p$lfas&SYEAR==yrs[i], c("LFA", "GRID_NUM", "LICENCE")), FUN=sum, lvls= licenceLevels)
	 pdf(file.path(figdir,paste0("FishFootLicenceFished", yrs[i],".pdf")))
	 LobsterMap('34-38', poly.lst=licencegrids)
	  	title(yrs[i],line=-3,cex.main=2,adj=0.3)
	 SpatialHub::contLegend('bottomright', lvls=licencegrids$lvls, Cont.data=licencegrids, title= "Number of Licence Fished", inset =0.02,cex=0.8,bg='white')
	  dev.off()
	  }
	

    ## CPUE


    logsInSeason<-lobster.db('process.logs.redo')
    logsInSeason<-lobster.db('process.logs')

    write.csv(logsInSeason,file.path(project.datadirectory("bio.lobster"),'data',"Logs.csv"),row.names=F)

    cpueLFA.dat = CPUEplot(logsInSeason,lfa= p$lfas,yrs=2002:2019,graphic='R',export=F)
    cpueLFA.dat = CPUEplot(logsInSeason,lfa= p$lfas,yrs=2006:2019,graphic='pdf',path=figdir)
    cpueSubArea.dat = CPUEplot(logsInSeason,subarea= p$subareas,yrs=2006:2019,graphic='R')



	## Commercial CPUE MOdels
	mf1 = formula(logWEIGHT ~ fYEAR + DOS + TEMP + DOS * TEMP)
	mf2 = formula(logWEIGHT ~ fYEAR + DOS + TEMP)
	mf3 = formula(logWEIGHT ~ fYEAR + DOS)
	mf4 = formula(logWEIGHT ~ fYEAR + TEMP)
	#mf5 = formula(logWEIGHT ~ fYEAR + DOS + TEMP + (1 | fYEAR/fAREA)) # combined


	TempModelling = TempModel( annual.by.area=F)
	#CPUE.data<-CPUEModelData(p,redo=T,TempModelling)
	CPUE.data<- CPUEModelData(p,redo=F)
	CPUE.data=subset(CPUE.data,!(LFA==35&SYEAR<2006)) #exclude partial year of data in 35
	CPUE.data=subset(CPUE.data,!(LFA==36&SYEAR<2005)) #exclude partial year of data in 36

#Modelled Temperature at first day of season
		pL=0
		t=c()
		d=c()
		k=1
		for(i in 1:length(p$lfas)){
			for(j in 2:length(p$yrs)){
				Cdat=subset(CPUE.data,LFA==p$lfas[i]&SYEAR==p$yrs[j])
				Cdat=Cdat[order(Cdat$DATE_FISHED),]
				Cdat$pLanded = cumsum(Cdat$WEIGHT_KG)/sum(Cdat$WEIGHT_KG)
				if(nrow(Cdat)>1){
				x=abs(Cdat$pLanded-pL)
				d[k]=Cdat$DOS[which(x==min(x))]
				t[k]=Cdat$TEMP[which(x==min(x))]
				names(d)[k]=paste(p$lfas[i],p$yrs[j],sep='.')
		    	names(t)[k]=paste(p$lfas[i],p$yrs[j],sep='.')
				k=k+1
				}
			}
		}

##t and d Overwrite with summary
	t=with(subset(CPUE.data,DOS==1),tapply(TEMP,LFA,mean))
	d=1

aicc = function(aa = model.output) {
	k = 	attr(logLik(aa),'df')
	logLiks = logLik(aa)[1]
	n = nobs(aa)
	aaa = 2*nParams-2*logLiks
	aaa + (2*k^2+2*k) / (n-k-1)
	}


	pData=list()

	CPUEModelResults1 = list()
	CPUEModelResults2 = list()
	CPUEModelResults3 = list()
	CPUEModelResults4 = list()
	AICs1 = c()
	AICs2 = c()
	AICs3 = c()
	AICs4 = c()
	for(i in 1:length( p$lfas)){

		mdata = subset(CPUE.data,LFA==p$lfas[i]&SYEAR%in%p$yrs)
		CPUEModelResults1[[i]] = CPUEmodel(mf1,mdata,t=t[i],d=d)
		CPUEModelResults2[[i]] = CPUEmodel(mf2,mdata,t=t[i],d=d)
		CPUEModelResults3[[i]] = CPUEmodel(mf3,mdata,t=t[i],d=d)
		CPUEModelResults4[[i]] = CPUEmodel(mf4,mdata,t=t[i],d=d)
		AICs1[i] = aicc(CPUEModelResults1[[i]]$model)
		AICs2[i] = aicc(CPUEModelResults2[[i]]$model)
		AICs3[i] = aicc(CPUEModelResults3[[i]]$model)
		AICs4[i] = aicc(CPUEModelResults4[[i]]$model)


	}
	names(CPUEModelResults1) = p$lfas
	names(CPUEModelResults2) = p$lfas
	names(CPUEModelResults3) = p$lfas
	names(CPUEModelResults4) = p$lfas
	
	AICs = data.frame(rbind(AICs1,AICs2,AICs3,AICs4))
	names(AICs) = p$lfas
	AICs
	AICtable=sweep(AICs,2,FUN='-',apply(AICs,2,min))

	pData=list()
	for(i in 1:length( p$lfas)){
		pData[[i]]=CPUEModelResults1[[i]]$pData
		pData[[i]]$LFA=p$lfas[i]
	}

	CPUEindex=do.call("rbind",pData)


	write.csv(AICtable,file.path( figdir,"CPUEmodelAIC.csv"),row.names=F)
	write.csv(CPUEindex,file.path( figdir,"CPUEmodelindex.csv"),row.names=F)



	#CPUECombinedModelResults = CPUEmodel(mf5,CPUE.data,combined=T)	

	cpue1= CPUEModelPlot(CPUEModelResults1,TempModelling,lfa = p$lfas,xlim=c(1989,2018.4),ylim=c(0,10.5),graphic='R',path=figdir,lab=1,wd=11,ht=8)


	ty=sapply(1:length(p$lfas),function(x){with(subset(CPUE.data,DOS==1&LFA==p$lfas[x]),tapply(TEMP,SYEAR,mean))})
	fs=lobster.db('season.dates')
	d=merge(data.frame(LFA=rep(p$lfas,lapply(ty,length)),SYEAR=names(unlist(ty)),TEMP2=unlist(ty)),fs[,c('LFA','SYEAR','START_DATE')])
	names(d)[2]="YEAR"


	CPUEindex = merge(d,CPUEindex)

	m2=subset(cpue1,DOS==1,c("LFA","YEAR","mu"))
	names(m2)[3]="mu2"
	
	CPUEindex = merge(CPUEindex,m2)

	write.csv(subset(CPUEindex,LFA==34&YEAR%in%2005:2018,c("LFA","YEAR","START_DATE","TEMP2","TEMP","mu","mu2")),file.path( figdir,"figures","Brad","CPUEmodelindex34.csv"),row.names=F)
	write.csv(subset(CPUEindex,LFA==35&YEAR%in%2005:2018,c("LFA","YEAR","START_DATE","TEMP2","TEMP","mu","mu2")),file.path( figdir,"figures","Brad","CPUEmodelindex35.csv"),row.names=F)
	write.csv(subset(CPUEindex,LFA==36&YEAR%in%2005:2018,c("LFA","YEAR","START_DATE","TEMP2","TEMP","mu","mu2")),file.path( figdir,"figures","Brad","CPUEmodelindex36.csv"),row.names=F)
	write.csv(subset(CPUEindex,LFA==38&YEAR%in%2005:2018,c("LFA","YEAR","START_DATE","TEMP2","TEMP","mu","mu2")),file.path( figdir,"figures","Brad","CPUEmodelindex38.csv"),row.names=F)

    #pdf2png(file.path(figdir,"CPUEmodel1"))
    cpueLFA.dat = CPUEplot(CPUE.data,lfa= p$lfas,yrs=1989:2018,graphic='png',export=T,path=figdir)

	cpue.annual=list()
	for(i in 1:length(p$lfas)){
		MU=c()
		MU.sd=c()
		for(j in 1:length(p$yrs)){
			MU[j]=with(subset(cpue1,LFA==p$lfas[i]&YEAR==p$yrs[j]),weighted.mean(mu,WEIGHT_KG))
			MU.sd[j]=with(subset(cpue1,LFA==p$lfas[i]&YEAR==p$yrs[j]),sqrt(sum(WEIGHT_KG/sum(WEIGHT_KG) * (mu - MU[j])^2)))
		}

	 	#MU=with(subset(cpue1,LFA==p$lfas[i]),tapply(mu,YEAR,weighted.mean,WEIGHT_KG))
	 	#MU.sd=with(subset(cpue1,LFA==p$lfas[i]),tapply(mu,YEAR,sd))
		#xm <- weighted.mean(x, wt)y6723
	 	#v <- sum(wt * (x - xm)^2)
	 	cpue.annual[[i]] = data.frame(Area=p$lfas[i],Year=p$yrs,CPUE=MU,CPUE.ub=MU+MU.sd,CPUE.lb=MU-MU.sd)

	 	
	 	#cpue.annual[[i]] = with(CPUEModelResults1[[i]]$pData,data.frame(Area=p$lfas[i],Year=YEAR,CPUE=mu,CPUE.ub=ub,CPUE.lb=lb))

	 	

	}
	cpueModel = subset(do.call("rbind",cpue.annual),Year<2019)


	#x11()
	#pdf(file.path( figdir,"CPUEmodelAnnualIndex.pdf"),8, 10)
	par(mfrow=c(length(p$lfas),1),mar=c(0,0,0,0),omi=c(0.5,1,0.5,0.5),las=1)

	for(i in 1:length(p$lfas)){


		plot(mu~YEAR,CPUEModelResults1[[i]]$pData,type='b',pch=21,bg='red',ylim=c(0,7),xlim=c(1989,2018),xaxt='n')
		points(CPUE~YEAR,subset(cpueLFA.dat$annual.dat,LFA==p$lfas[i]&YEAR<2019),pch=16,col='blue',cex=0.9)
		#lines(ub~YEAR,CPUEModelResults1[[i]]$pData,lty=2)
		#lines(lb~YEAR,CPUEModelResults1[[i]]$pData,lty=2)
		axis(1,lab=F)
		axis(4)
		if(i==length(p$lfas))axis(1)
		
		text(1989,6,paste(p$lfas[i]),cex=2,pos=4)
	}
	mtext("CPUE (kg/TH)", 2, 3, outer = T, cex = 1,las=0)	
	dev.off()
	
	cpueData2=    CPUEplot(CPUE.data,lfa= p$lfas,yrs=1981:2018,graphic='R')$annual.data

	save(list=c("cpueModel","cpueData2"),file=file.path(project.datadirectory("bio.lobster"),"outputs","cpueIndicators3438.rdata"))
	save(cpueData2,file=file.path(project.datadirectory("bio.lobster"),"outputs","cpueIndicators3438_2.rdata"))
	#write.csv(cpueLFA.dat$annual.data,"CPUEannualData.csv",row.names=F)
	#write.csv(na.omit(cpueLFA.dat$daily.data),"CPUEdailyData.csv",row.names=F)

	load(file=file.path(project.datadirectory("bio.lobster"),"outputs","cpueIndicators3438.rdata"))

	# Landings and Effort ############

	 	land = lobster.db('seasonal.landings')
		land$YEAR = as.numeric(substr(land$SYEAR,6,9))

		for(i in 1:length(p$lfas)){

			d1 = data.frame(YEAR = land$YEAR, LANDINGS = land[,paste0("LFA",p$lfas[i])])
			d2 = subset(cpueData2,LFA==p$lfas[i])

			d2  = merge(data.frame(LFA=d2$LFA[1],YEAR=min(d2$YEAR):max(d2$YEAR)),d2,all.x=T)

			fishData = merge(d2,d1) 
			fishData$EFFORT2 = fishData$LANDINGS * 1000 / fishData$CPUE

			# plot
			x11(width=8,height=5)
			FisheryPlot(fishData[,c("YEAR","LANDINGS","EFFORT2")],lfa = p$lfas[i],fd=figdir)
		}



	## FSRS MOdels

	#Base

	FSRSvesday = FSRSModelData()

	FSRSvesday = subset(FSRSvesday,SYEAR<2019)
	FSRSModelResultsRecruit = list()
	FSRSModelResultsShort = list()
	FSRSModelResultsLegal = list()
	shorts.lst = list()
	legals.lst = list()
	recruit.lst = list()
	p$areas = c('34','35')
	for(i in 1:length( p$areas)){

		mdata = subset(FSRSvesday,LFA==p$areas[i])

		FSRSModelResultsShort[[i]]=FSRSmodel(mdata, response="SHORTS",interaction=F)
		pdata	= 	FSRSModelResultsShort[[i]]$pData
		pdata$Area = p$areas[i]
		shorts.lst[[i]] = pdata

		FSRSModelResultsLegal[[i]]=FSRSmodel(mdata, response="LEGALS",interaction=F)
		pdata	= 	FSRSModelResultsLegal[[i]]$pData
		pdata$Area = p$areas[i]
		legals.lst[[i]] = pdata

		FSRSModelResultsRecruit[[i]]=FSRSmodel(mdata, response="RECRUITS",interaction=F)
		pdata	= 	FSRSModelResultsRecruit[[i]]$pData
		pdata$Area = p$areas[i]
		recruit.lst[[i]] = pdata


	}

	names(FSRSModelResultsShort) = p$areas
	names(FSRSModelResultsLegal) = p$areas
	names(FSRSModelResultsRecruit) = p$areas
	
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
	sp <- sp + facet_wrap(  ~Area, ncol=1,scales = "fixed")
	sp

	lp <- ggplot()
	lp <- lp + geom_point(data = legals, aes(y = mu, x = YEAR), shape = 16, size = 2)
	lp <- lp + xlab("Year") + ylab("Lobsters / Trap")
	lp <- lp + theme(text = element_text(size=15)) + theme_bw()
	lp <- lp + geom_line(data = legals, aes(x = YEAR, y = mu), colour = "black")
	lp <- lp + geom_ribbon(data = legals, aes(x = YEAR, ymax = ub, ymin = lb ), alpha = 0.5)
	lp <- lp + facet_wrap(  ~Area, ncol=1,scales = "fixed")
	lp

	rp <- ggplot()
	rp <- rp + geom_point(data = recruit, aes(y = mu, x = YEAR), shape = 16, size = 2)
	rp <- rp + xlab("Year") + ylab("Lobsters / Trap")
	rp <- rp + theme(text = element_text(size=15)) + theme_bw()
	rp <- rp + geom_line(data = recruit, aes(x = YEAR, y = mu), colour = "black")
	rp <- rp + geom_ribbon(data = recruit, aes(x = YEAR, ymax = ub, ymin = lb ), alpha = 0.5)
	rp <- rp + facet_wrap(  ~Area, ncol=1,scales = "fixed")
	rp

	dev.off()



	#Bayes

	FSRSvesday<-FSRSModelData()
	FSRSvesday = subset(FSRSvesday,SYEAR<2019)
	#FSRSvesdayComm<-FSRSModelData(trap.type="commercial")
	FSRSModelResultsRecruit = list()
	FSRSModelResultsShort = list()
	FSRSModelResultsLegal = list()
	shorts.lst = list()
	legals.lst = list()
	recruit.lst = list()

	for(i in 1:length( p$areas)){
	st = Sys.time()

		mdata = subset(FSRSvesday,subarea==p$areas[i])

		FSRSModelResultsShort[[i]]=FSRSmodel(mdata, response="SHORTS",interaction=F,type="bayesian",iter=5000,redo=T,ptraps=1000)
		pdata	= 	FSRSModelResultsShort[[i]]$pData
		pdata$Area = p$areas[i]
		shorts.lst[[i]] = pdata
		
		FSRSModelResultsLegal[[i]]=FSRSmodel(mdata, response="LEGALS",interaction=F,type="bayesian",iter=5000,redo=T,ptraps=1000)
		pdata	= 	FSRSModelResultsLegal[[i]]$pData
		pdata$Area = p$areas[i]
		legals.lst[[i]] = pdata

		FSRSModelResultsRecruit[[i]]=FSRSmodel(mdata, response="RECRUITS",interaction=F,type="bayesian",iter=5000,redo=T,ptraps=1000)
		pdata	= 	FSRSModelResultsRecruit[[i]]$pData
		pdata$Area = p$areas[i]
		recruit.lst[[i]] = pdata
		print( Sys.time() - st)


	}

	names(FSRSModelResultsShort) = p$areas
	names(FSRSModelResultsLegal) = p$areas
	names(FSRSModelResultsRecruit) = p$areas
	
	shorts = do.call("rbind",shorts.lst)
	legals = do.call("rbind",legals.lst)
	recruit = do.call("rbind",recruit.lst)

	library(ggplot2)

	#pdf(file.path( figdir,"FSRSmodelBayesShorts.pdf"),8, 10)
	png(file.path(figdir,"FSRSmodelBayesShorts.png"),width=8,height=10,units='in',res=200)
	sp <- ggplot()
	sp <- sp + geom_point(data = shorts, aes(y = median, x = YEAR), shape = 16, size = 2)
	sp <- sp + xlab("Year") + ylab("Lobsters / Trap")
	sp <- sp + theme(text = element_text(size=15)) + theme_bw()
	sp <- sp + geom_line(data = shorts, aes(x = YEAR, y = median), colour = "black")
	sp <- sp + geom_ribbon(data = shorts, aes(x = YEAR, ymax = ub, ymin = lb ), alpha = 0.5)
	sp <- sp + facet_wrap(  ~Area, ncol=1,scales = "fixed")
	sp
	dev.off()

	#pdf(file.path( figdir,"FSRSmodelBayesLegals.pdf"),8, 10)
	png(file.path(figdir,"FSRSmodelBayesLegals.png"),width=8,height=10,units='in',res=200)
	lp <- ggplot()
	lp <- lp + geom_point(data = legals, aes(y = median, x = YEAR), shape = 16, size = 2)
	lp <- lp + xlab("Year") + ylab("Lobsters / Trap")
	lp <- lp + theme(text = element_text(size=15)) + theme_bw()
	lp <- lp + geom_line(data = legals, aes(x = YEAR, y = median), colour = "black")
	lp <- lp + geom_ribbon(data = legals, aes(x = YEAR, ymax = ub, ymin = lb ), alpha = 0.5)
	lp <- lp + facet_wrap(  ~Area, ncol=1,scales = "fixed")
	lp
	dev.off()

	#pdf(file.path( figdir,"FSRSmodelBayesRecruits.pdf"),8, 10)
	png(file.path(figdir,"FSRSmodelBayesRecruits.png"),width=8,height=10,units='in',res=200)
	rp <- ggplot()
	rp <- rp + geom_point(data = recruit, aes(y = median, x = YEAR), shape = 16, size = 2)
	rp <- rp + xlab("Year") + ylab("Lobsters / Trap")
	rp <- rp + theme(text = element_text(size=15)) + theme_bw()
	rp <- rp + geom_line(data = recruit, aes(x = YEAR, y = median), colour = "black")
	rp <- rp + geom_ribbon(data = recruit, aes(x = YEAR, ymax = ub, ymin = lb ), alpha = 0.5)
	rp <- rp + facet_wrap(  ~Area, ncol=1,scales = "fixed")
	rp

	dev.off()

 	save(list=c("shorts","legals","recruit"),file=file.path(project.datadirectory("bio.lobster"),"outputs","fsrsModelIndicators3435.rdata"))

	
	TempModelling = TempModel(areas = 'subarea')
	TempModelPlot(TempModelling,xlim=c(1980,2018),depths=c(5,25,50),Area=c("27N","27S", "29", "30","31A","31B", "32", "33E", "33W"),graphic='png',type=1:2)

 tempModel=TempModelPlot(TempModelling,xlim=c(1980,2018),depths=c(5,25,50),Area=c("27N","27S", "29", "30","31A","31B", "32", "33E", "33W"),graphic='png',type=3)
 tempData=TempModelPlot(TempModelling,xlim=c(1980,2018),depths=c(5,25,50),Area=c("27N","27S", "29", "30","31A","31B", "32", "33E", "33W"),graphic='png',type=4)
 save(list=c("tempModel","tempData"),file=file.path(project.datadirectory("bio.lobster"),"outputs","tempIndicators.rdata"))

 tempData2=TempModelPlot(TempModelling,xlim=c(1980,2018),depths=c(5,25,50),Area=c("27", "33"),lfa=T,graphic='R',type=4)
 save(tempData2,file=file.path(project.datadirectory("bio.lobster"),"outputs","tempIndicators2.rdata"))






	# map
		pdf(file.path( figdir,"ScallopSurveyBubblesBoF2018.pdf"),11,8)
	LobsterMap("BoF",mapRes="UR",title="BoF Scallop Survey",isobath=seq(50,500,50),bathcol=rgb(0,0,1,0.2),bathy.source='bathy')
	points(y~x,zeros,pch=4)
	surveyBubbles(data,scaler=0.08,pie=T)
	dev.off()



########### CCIR

		load(file.path(project.datadirectory('bio.lobster'),'data','inputs','ccir_groupings.rdata')) #object names Groupings


		lobster.db('ccir.redo') 
		ccir_data = subset(ccir_data,YEAR<2019)
		
		inp = read.csv(file.path(project.datadirectory('bio.lobster'),'data','inputs','ccir_inputs.csv'))
				
				# fill in table where data is missing for recent years
				inp.lst=list()
				lfas = unique(inp$LFA)
				for(i in 1:length(lfas)){
					inpt = subset(inp,LFA==lfas[i])
					maxyr=max(inpt$Year)
					inp.lst[[i]] = rbind(inpt, data.frame(LFA=lfas[i],Year=(maxyr+1):assessment.year,inpt[inpt$Year==maxyr,3:ncol(inpt)]))
				}
				inp = do.call("rbind",inp.lst)
							
				write.csv(inp,file.path(project.datadirectory('bio.lobster'),'data','inputs',paste0('ccir_inputs',assessment.year,'.csv')))

		inp = read.csv(file.path(project.datadirectory('bio.lobster'),'data','inputs',paste0('ccir_inputs',assessment.year,'.csv')))
		inp33 = subset(inp,LFA==33)
		inp34 = inp33
		inp34$LFA = 34
		inp35 = inp33
		inp35$LFA = 35
		inp=rbind(inp,inp34,inp35)

		load(file.path(project.datadirectory('bio.lobster'),'data','inputs','ccir_groupings.rdata')) #object names Groupings
		load(file.path(project.datadirectory('bio.lobster'),'data','inputs','ccir_seasons.rdata'))
		lobster.db('ccir')

		logs = lobster.db('process.logs')
		
		require(bio.ccir)
		require(rstan)

		load_all(paste(git.repo,'bio.ccir',sep="/")) # for debugging
		dat = ccir_compile_data(x = ccir_data,log.data = logs, area.defns = Groupings[1:6], size.defns = inp, season.defns = Seasons, sexs = 1.5) #sexs 1.5 means no sex defn


		lobster.db('ccir.redo') 
		ccir_data = subset(ccir_data,YEAR<2019)
		
#		inp = read.csv(file.path(project.datadirectory('bio.lobster'),'data','inputs','ccir_inputs.csv'))
#				
#				# fill in table where data is missing for recent years
#				inp.lst=list()
#				lfas = unique(inp$LFA)
#				for(i in 1:length(lfas)){
#					inpt = subset(inp,LFA==lfas[i])
#					maxyr=max(inpt$Year)
#					inp.lst[[i]] = rbind(inpt, data.frame(LFA=lfas[i],Year=(maxyr+1):assessment.year,inpt[inpt$Year==maxyr,3:ncol(inpt)]))
#				}
#				inp = do.call("rbind",inp.lst)
#							
#				write.csv(inp,file.path(project.datadirectory('bio.lobster'),'data','inputs',paste0('ccir_inputs',assessment.year,'.csv')))
#
#		inp = read.csv(file.path(project.datadirectory('bio.lobster'),'data','inputs',paste0('ccir_inputs',assessment.year,'.csv')))
#		inp33 = subset(inp,LFA==33)
#		inp34 = inp33
#		inp34$LFA = 34
#		inp35 = inp33
#		inp35$LFA = 35
#		inp=rbind(inp,inp34,inp35)
#		write.csv(inp,file.path(project.datadirectory('bio.lobster'),'data','inputs',paste0('ccir_inputs',assessment.year,'.csv')))

		inp = read.csv(file.path(project.datadirectory('bio.lobster'),'data','inputs',paste0('ccir_inputs',assessment.year,'.csv')))
		load(file.path(project.datadirectory('bio.lobster'),'data','inputs','ccir_groupings.rdata')) #object names Groupings
		load(file.path(project.datadirectory('bio.lobster'),'data','inputs','ccir_seasons.rdata'))
		lobster.db('ccir')

		gridsbylfa=with(ccir_data,tapply(Grid,LFA,unique))
		groups3435=list(list(lfa=34,G1=gridsbylfa$'34'),list(lfa=35,G1=gridsbylfa$'35'))



		logs = lobster.db('process.logs')
		
		require(bio.ccir)
		require(rstan)

		load_all(paste(git.repo,'bio.ccir',sep="/")) # for debugging
		dat = ccir_compile_data(x = ccir_data,log.data = logs, area.defns = groups3435[1], size.defns = inp, season.defns = Seasons, sexs = 1.5) #sexs 1.5 means no sex defn

		out.binomial = list()
		attr(out.binomial,'model') <- 'binomial'
		for(i in 1:length(dat)) {
			ds = dat[[i]]
			ds$method = 'binomial'
			x = ccir_stan_run(dat = ds,save=F)
			out.binomial[[i]] <- ccir_stan_summarize(x)
		}
		ouBin = ccir_collapse_summary(out.binomial)
		attr(ouBin,'model') <- 'binomial' 
		#ouBin$Yr = ouBin$Yr +1
		save(ouBin,file=file.path(project.datadirectory('bio.lobster'),'outputs','ccir','summary','compiledBinomialModels2732.rdata'))
		#load(file=file.path(project.datadirectory('bio.lobster'),'outputs','ccir','summary','compiledBinomialModels2732.rdata'))

		u = subset(ouBin, LFA == 27)
		g = unique(u$Grid)
		g = strsplit(g,"\\.")
		o = aggregate(WEIGHT_KG~SYEAR,data=subset(logs,GRID_NUM %in% g[[1]]),FUN=sum)
		names(o)[2] = g[[1]][1]
		o2 = aggregate(WEIGHT_KG~SYEAR,data=subset(logs,GRID_NUM %in% g[[2]]),FUN=sum)
		names(o2)[2] = g[[2]][1]
		o = merge(o,o2)
		names(o)[1] = 'Yr'
		oo <- ccir_timeseries_exploitation_plots(ouBin,combined.LFA=T,landings=o)

		u = subset(ouBin, LFA != 27)
		kl = unique(u$Grid) 
		outs=list()
		for(i in 1:length(kl)) {
			u = subset(ouBin, Grid == kl[i])
			outs[[i]] <- ccir_timeseries_exploitation_plots(u)
		}
		o = do.call(rbind,outs)
		ooo = subset(o,select=c(Yr,ERfl,ERfm,ERfu,ERf75,LFA))

		oo = rbind(oo,ooo)
		oo$LFA[oo$LFA == "LFA 27 Combined"] = 27
		oo$LFA[oo$LFA == "LFA 29"] = 29
		oo$LFA[oo$LFA == "LFA 30"] = 30
		oo$LFA[oo$LFA == "LFA 31A"] = "31A"
		oo$LFA[oo$LFA == "LFA 31B"] = "31B"
		oo$LFA[oo$LFA == "LFA 32"] = 32

		save(oo,file=file.path(project.datadirectory('bio.lobster'),'outputs','ccir','summary','compiledExploitationCCIR2732.rdata'))
		load(file=file.path(project.datadirectory('bio.lobster'),'outputs','ccir','summary','compiledExploitationCCIR2732.rdata'))
		RR75  = aggregate(ERf75~LFA,data=oo[oo$Yr<2017,],FUN=max)

		ouBin = subset(ouBin,Yr<2018&LFA==34,-2)
		save(ouBin,file=file.path(project.datadirectory('bio.lobster'),'outputs','ccir','summary','compiledBinomialModels34.rdata'))
		load(file=file.path(project.datadirectory('bio.lobster'),'outputs','ccir','summary','compiledBinomialModels34.rdata'))

		oo <- ccir_timeseries_exploitation_plots(ouBin,Main="34")
		

		save(oo,file=file.path(project.datadirectory('bio.lobster'),'outputs','ccir','summary','compiledExploitationCCIR34.rdata'))
		load(file=file.path(project.datadirectory('bio.lobster'),'outputs','ccir','summary','compiledExploitationCCIR33.rdata'))
		oo33=oo
		load(file=file.path(project.datadirectory('bio.lobster'),'outputs','ccir','summary','compiledExploitationCCIR34.rdata'))
		RR75  = aggregate(ERf75~LFA,data=oo[oo$Yr<2018,],FUN=max)$ERf75

	# plot

		x11(width=8,height=5)
		ExploitationRatePlots(data = oo[,c("Yr","ERfm","ERfl","ERfu")],lfa = 34,fd=figdir,runM=F)
		load(file=file.path(project.datadirectory('bio.lobster'),'outputs','ccir','summary','compiledBinomialModels33.rdata'))

		with(subset(ouBin,LFA=='33W'),lines(Yr,ERfm,lty=2,col='blue'))

		ExploitationRatePlots(data = oo[,c("Yr","ERfm","ERfl","ERfu")],lrp=RR75,lfa = 34,fd=figdir)


	}









######################## sim Molt
