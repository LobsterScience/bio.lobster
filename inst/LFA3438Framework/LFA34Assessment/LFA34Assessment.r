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
	    figdir = file.path(project.datadirectory("bio.lobster"),"figures","Assessment","LFA34")

	    p$lfas = "34" # specify lfa
    	p$subareas = c("34") # specify subareas for data summary
	    
	    # update data through ROracle
	    lobster.db('fsrs.redo')
	    lobster.db('logs.redo')
	    logs=lobster.db('process.logs.redo')
		#CPUE.data<-CPUEModelData(p,redo=T)
	 
# Map ################

		x11(width=5, height=5)
		LobsterMap('34')
		text(x=c(-65.2,-65.7,-67.4),y=c(43.4,44.9,43.1),labels=c(33,35,41),col=rgb(0,0,0,0.8),cex=1.5)

		savePlot(file.path(figdir,'LFA34map.png'),type='png')


# CPUE ###############
		
		logs=lobster.db("process.logs")
		TempModelling = TempModel( annual.by.area=F)
		CPUE.data<-CPUEModelData(p,redo=F,TempModelling)

		## Commercial CPUE MOdels
		mf1 = formula(logWEIGHT ~ fYEAR + DOS + TEMP + DOS * TEMP)

		CPUE.data<- CPUEModelData(p,redo=F)
		t=mean(subset(CPUE.data,DOS==1)$TEMP)

		mdata = subset(CPUE.data,SYEAR%in%p$yrs)
		CPUEModelResults = CPUEmodel(mf1,mdata,t=t,d=1)
		crd = CPUEModelResults$pData[,c("YEAR","mu")]
		cpue1= CPUEModelPlot(CPUEModelResults,TempModelling,
			mdata=mdata,combined=T, lfa = p$lfas,xlim=c(1989,2018.4),ylim=c(0,10.5),
			graphic='R',path=figdir,lab=1,wd=11,ht=8)

	# plot
	x11(width=8,height=5)
	CatchRatePlot(data = crd, lfa = 34, fd=figdir)
	savePlot(file.path(figdir,'CPUELFA342019.png'))


########### CCIR
		
		lobster.db('ccir.redo') 
		#ccir_data = subset(ccir_data,YEAR<2019)
		
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

		#load_all(paste(git.repo,'bio.ccir',sep="/")) # for debugging
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

		ouBin = subset(ouBin,Yr<2018&LFA==34,-2)
		save(ouBin,file=file.path(project.datadirectory('bio.lobster'),'outputs','ccir','summary','compiledBinomialModels34.rdata'))
		load(file=file.path(project.datadirectory('bio.lobster'),'outputs','ccir','summary','compiledBinomialModels34.rdata'))

		oo <- ccir_timeseries_exploitation_plots(ouBin,Main="34")
		

		save(oo,file=file.path(project.datadirectory('bio.lobster'),'outputs','ccir','summary','compiledExploitationCCIR34.rdata'))
		load(file=file.path(project.datadirectory('bio.lobster'),'outputs','ccir','summary','compiledExploitationCCIR34.rdata'))
		RR75  = aggregate(ERf75~LFA,data=oo[oo$Yr<2018,],FUN=max)$ERf75

	# plot

		x11(width=8,height=5)
		ExploitationRatePlots(data = oo[,c("Yr","ERfm","ERfl","ERfu")],lfa = 34,fd=figdir,runM=F)
		load(file=file.path(project.datadirectory('bio.lobster'),'outputs','ccir','summary','compiledBinomialModels33.rdata'))

		with(subset(ouBin,LFA=='33W'),lines(Yr,ERfm,lty=2,col='blue'))

		ExploitationRatePlots(data = oo[,c("Yr","ERfm","ERfl","ERfu")],lrp=RR75,lfa = 34,fd=figdir)



# FSRS #############

		FSRSvesday<-FSRSModelData()

		mdata = subset(FSRSvesday,LFA==34)

		FSRSModelResultsLegal=FSRSmodel(mdata,lfa=34, response="LEGALS",interaction=F,type="bayesian",iter=5000,redo=T,ptraps=1000)
		FSRSModelShortsRecruit=FSRSmodel(mdata,lfa=34, response="SHORTS",interaction=F,type="bayesian",iter=5000,redo=T,ptraps=1000)
		FSRSModelResultsRecruit=FSRSmodel(mdata,lfa=34, response="RECRUITS",interaction=F,type="bayesian",iter=5000,redo=T,ptraps=1000)

		FSRSModelResultsLegal=FSRSmodel(mdata,lfa=34, response="LEGALS",interaction=F,type="bayesian",iter=5000,redo=F,ptraps=1000)
		legals = FSRSModelResultsLegal$pData
		legals$Area = 34
		
		FSRSModelResultsRecruit=FSRSmodel(mdata,lfa=34, response="RECRUITS",interaction=F,type="bayesian",iter=5000,redo=F,ptraps=1000)
		recruit =  FSRSModelResultsRecruit$pData
		recruit$Area = 34

		FSRSModelShortsRecruit=FSRSmodel(mdata,lfa=34, response="SHORTS",interaction=F,type="bayesian",iter=5000,redo=F,ptraps=1000)
		shorts =  FSRSModelShortsRecruit$pData
		shorts$Area = 34
 	
 	save(list=c("shorts","legals","recruit"),file=file.path(project.datadirectory("bio.lobster"),"outputs","fsrsModelIndicators34.rdata"))


	# plot
	x11(width=8,height=7)
	FSRSCatchRatePlot(recruits = recruit[,c("YEAR","median","lb","ub")],legals=legals[,c("YEAR","median","lb","ub")],lfa = 34,fd=figdir,title='')


# Landings and Effort ############

	 	land = lobster.db('seasonal.landings')


		land$YEAR = as.numeric(substr(land$SYEAR,6,9))
		land$LANDINGS = land$LFA34
		fishData = merge(cpueData,land[,c("YEAR","LANDINGS")]) 
		fishData$EFFORT2 = fishData$LANDINGS * 1000 / fishData$CPUE



	# plot
	x11(width=8,height=5)
	FisheryPlot(fishData[,c("YEAR","LANDINGS","EFFORT2")],lfa = 34,fd=figdir)



# Contextual Indicators #############
