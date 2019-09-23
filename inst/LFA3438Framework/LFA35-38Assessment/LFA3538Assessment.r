# LFA 34 Assessment Script
#	______
#	`>___;'______       /3
#	  ---~<.     ))))))) 3
#	 _____ `,-----%%%%% \3
#	 `>___;- |}}}	 
#           
 
	p = bio.lobster::load.environment()
	la()

	assessment.year = 2019 ########### check the year ############### !!!!!!!!!!!


    p$syr = 1989
    p$yrs = p$syr:assessment.year



	    # define place for figures to go
	    figdir = file.path(project.datadirectory("bio.lobster"),"figures","Assessment","LFA3538")

	    p$lfas = c("35","36","38") # specify lfa
    	p$subareas = c("35","36","38") # specify subareas for data summary
	    
	    # update data through ROracle
	    lobster.db('fsrs.redo')
	    lobster.db('logs.redo')
	    logs=lobster.db('process.logs.redo')
		#CPUE.data<-CPUEModelData(p,redo=T)
	 
# Map ################

		x11(width=5, height=5)
		LobsterMap(ylim=c(43.3,46),	xlim=c(-67.8,-63.2))

		savePlot(file.path(figdir,'LFA3538map.png'),type='png')


# CPUE ###############
		load_all('~/git/bio.utilities')
		logs=lobster.db("process.logs")
		TempModelling = TempModel( annual.by.area=F)
		CPUE.data<-CPUEModelData(p,redo=T,TempModelling)

		## Commercial CPUE MOdels
		mf1 = formula(logWEIGHT ~ fYEAR + DOS + TEMP + DOS * TEMP)

		CPUE.data<- CPUEModelData(p,redo=F)
		t=with(subset(CPUE.data,DOS==1),tapply(TEMP,LFA,mean))
	
		pData=list()

		CPUEModelResults = list()

		for(i in 1:length( p$lfas)){

			mdata = subset(CPUE.data,LFA==p$lfas[i]&SYEAR%in%p$yrs)

			CPUEModelResults[[i]] = CPUEmodel(mf1,mdata,t=t[i],d=1)
			pData[[i]]=CPUEModelResults[[i]]$pData
			pData[[i]]$LFA=p$lfas[i]
		}

		names(CPUEModelResults) = p$lfas

		CPUEindex=do.call("rbind",pData)


	# plot
<<<<<<< HEAD
	cpue1= CPUEModelPlot(CPUEModelResults,TempModelling,lfa = p$lfas,xlim=c(1989,2019.4),ylim=c(0,10.5),graphic='R',path=figdir,lab=1,wd=11,ht=8)

	x11(width=8,height=5)
	#35
	l35 = subset(CPUEindex,LFA==35,c("YEAR","mu"))
	k = median(l35$mu[which(l35$YEAR %in% 2011:2018)])
	usr = .4*k
	lrp = .2*k
	CatchRatePlot(data = l35, usr=usr, lrp=lrp, lfa = 35, fd=figdir)
	


	#36
	l36 = subset(CPUEindex,LFA==36,c("YEAR","mu"))
	k = median(l36$mu[which(l36$YEAR %in% 2011:2018)])
	usr = .4*k
	lrp = .2*k
	CatchRatePlot(data = l36, usr=usr, lrp=lrp, lfa = 36, fd=figdir)



	#38
	l38 = subset(CPUEindex,LFA==38,c("YEAR","mu"))
	k = median(l38$mu[which(l38$YEAR %in% 2011:2018)])
	usr = .4*k
	lrp = .2*k
	CatchRatePlot(data = l38, usr=usr, lrp=lrp, lfa = 38, fd=figdir)
=======
	for(i in 1:length( p$lfas)){
		
		x11(width=8,height=5)


		K = median(subset(CPUEindex,LFA== p$lfas[i]&YEAR>2010)$mu)
		USR = K*.4
		LRP = K*.2
		CatchRatePlot(data = subset(CPUEindex,LFA== p$lfas[i],c("YEAR","mu")),usr = USR,lrp=LRP, lfa = p$lfas[i], fd=figdir)
		print(paste("USR =",round(USR,2)))
		print(paste("LRP =",round(LRP,2)))
	}
>>>>>>> develop



# FSRS #############

		FSRSvesday<-FSRSModelData()

		mdata = subset(FSRSvesday,LFA==35)

		FSRSModelResultsLegal=FSRSmodel(mdata,lfa=35, response="LEGALS",interaction=F,type="bayesian",iter=5000,redo=T,ptraps=1000)
		FSRSModelShortsRecruit=FSRSmodel(mdata,lfa=35, response="SHORTS",interaction=F,type="bayesian",iter=5000,redo=T,ptraps=1000)
		FSRSModelResultsRecruit=FSRSmodel(mdata,lfa=35, response="RECRUITS",interaction=F,type="bayesian",iter=5000,redo=T,ptraps=1000)

		FSRSModelResultsLegal=FSRSmodel(mdata,lfa=35, response="LEGALS",interaction=F,type="bayesian",iter=5000,redo=F,ptraps=1000)
		legals = FSRSModelResultsLegal$pData
		legals$Area = 35
		
		FSRSModelResultsRecruit=FSRSmodel(mdata,lfa=35, response="RECRUITS",interaction=F,type="bayesian",iter=5000,redo=F,ptraps=1000)
		recruit =  FSRSModelResultsRecruit$pData
		recruit$Area = 35

		FSRSModelShortsRecruit=FSRSmodel(mdata,lfa=35, response="SHORTS",interaction=F,type="bayesian",iter=5000,redo=F,ptraps=1000)
		shorts =  FSRSModelShortsRecruit$pData
		shorts$Area = 35
 	
 	save(list=c("shorts","legals","recruit"),file=file.path(project.datadirectory("bio.lobster"),"outputs","fsrsModelIndicators35.rdata"))


	# plot
	x11(width=8,height=7)
	FSRSCatchRatePlot(recruits = recruit[,c("YEAR","median","lb","ub")],legals=legals[,c("YEAR","median","lb","ub")],lfa = 35,fd=figdir,title='')


# Landings and Effort ############


	land = lobster.db('seasonal.landings')
	land$YR = as.numeric(substr(land$SYEAR,6,9))

	for(i in 1:length(p$lfas)){

		d1 = data.frame(YEAR = land$YR, LANDINGS = land[,paste0("LFA",p$lfas[i])])
		d2 = subset(CPUEindex,LFA==p$lfas[i],c("LFA","YEAR","mu"))
		names(d2)[3]="CPUE"

		d2  = merge(data.frame(LFA=d2$LFA[1],YEAR=min(d2$YEAR):max(d2$YEAR)),d2,all.x=T)

		fishData = merge(d2,d1) 
		fishData$EFFORT2 = fishData$LANDINGS * 1000 / fishData$CPUE

		# plot
		x11(width=8,height=5)
		FisheryPlot(fishData[,c("YEAR","LANDINGS","EFFORT2")],lfa = p$lfas[i],fd=figdir,preliminary=nrow(fishData))
	}



# Contextual Indicators #############
