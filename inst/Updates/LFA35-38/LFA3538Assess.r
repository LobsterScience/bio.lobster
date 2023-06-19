# LFA 34 Assessment Script
#	______
#	`>___;'______       /3
#	  ---~<.     ))))))) 3
#	 _____ `,-----%%%%% \3
#	 `>___;- |}}}	 
#           
 
	p = bio.lobster::load.environment()
	la()

	assessment.year = 2020 ########### check the year ############### !!!!!!!!!!!

	  p$syr = 1989
    p$yrs = p$syr:assessment.year



	    # define place for figures to go
	    #figdir = file.path(project.datadirectory("bio.lobster"),"figures","Assessment","LFA3538")
figdir = file.path("LFA35_Update")
    
	    p$lfas = c("35") # specify lfa
    	p$subareas = c("35") # specify subareas for data summary
	    
	    # update data through RODBC
    	lobster.db('temperature.data.redo')
      lobster.db('fsrs.redo')
	    lobster.db('logs.redo')
	    logs=lobster.db('process.logs.redo')
		#CPUE.data<-CPUEModelData(p,redo=T)
	 
# Map ################

		x11(width=5, height=5)
		LobsterMap(ylim=c(43.3,46),	xlim=c(-67.8,-63.2))

		savePlot(file.path(figdir,'LFA3538map.png'),type='png')


# CPUE ###############
	require('bio.utilities')
		logs=lobster.db("process.logs")
		TempModelling = TempModel( annual.by.area=F, redo.data=F)
			CPUE.data<-CPUEModelData(p,redo=F,TempModelling)

			## Commercial CPUE MOdels
			mf1 = formula(logWEIGHT ~ fYEAR + DOS + TEMP + DOS * TEMP)

			CPUE.data<- CPUEModelData(p,redo=F)
			t=with(subset(CPUE.data,DOS==1),tapply(TEMP,LFA,mean))
		
			pData=list()

			CPUEModelResults = list()

			for(i in 1:length( p$lfas)){

				mdata = subset(CPUE.data,LFA==p$lfas[i]&SYEAR%in%p$yrs)
				if(nrow(mdata)>10){
				CPUEModelResults[[i]] = CPUEmodel(mf1,mdata,t=t[i],d=1)
				pData[[i]]=CPUEModelResults[[i]]$pData
				pData[[i]]$LFA=p$lfas[i]
			}
			}

			names(CPUEModelResults) = p$lfas

			CPUEindex=do.call("rbind",pData)


	# plot

	x11(width=8,height=5)
	#35
	l35 = subset(CPUEindex,LFA==35,c("YEAR","mu"))
	k = median(l35$mu[which(l35$YEAR %in% 2011:2018)])
	usr = .4*k
	lrp = .2*k
	

	plot(l35$YEAR,l35$mu,xlab='Year',ylab='CPUE (kg/TH)',type='p',pch=16,ylim=c(0,6))
	running.median = with(rmed(l35$YEAR,l35$mu),data.frame(YEAR=yr,running.median=x))
	l35=merge(l35,running.median,all=T)
	lines(l35$YEAR,l35$running.median,col='blue',lty=1,lwd=3)
	abline(h=usr,col='green',lwd=2,lty=2)
	abline(h=lrp,col='red',lwd=2,lty=3)
	


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
 	
 	save(list=c("shorts","legals","recruit"),file=figdir)


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
