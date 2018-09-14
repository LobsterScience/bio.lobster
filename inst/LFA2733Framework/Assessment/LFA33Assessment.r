 
	p = bio.lobster::load.environment()
	la()



	    figdir = file.path(project.datadirectory("bio.lobster"),"figures","Assessment","LFA33")

	    p$lfas = "33" # specify lfas for data summary

	    # update data through ROracle
	    lobster.db('fsrs.redo')
	    logs=lobster.db('process.logs.redo')
		CPUE.data<-CPUEModelData(p,redo=T)
	 

		#CPUE
		
		logs=lobster.db("process.logs")
		CPUE.data<-CPUEModelData(p,redo=F)
		cpueData=    CPUEplot(CPUE.data,lfa= p$lfas,yrs=1981:2018,graphic='R')$annual.data
		crd = subset(cpueData,LFA==33,c("YEAR","CPUE"))	
		mu = median(crd$CPUE[crd$YEAR<2017])
		usr = mu * 0.8
		lrp = mu * 0.4

	# plot
	x11(width=8,height=5)
	CatchRatePlot(data = crd ,usr = usr,lrp=lrp,lfa = 33,fd=figdir)

		# CCIR
		lobster.db('ccir.redo') 
		inp = read.csv(file.path(project.datadirectory('bio.lobster'),'data','inputs','ccir_inputs.csv'))
		load(file.path(project.datadirectory('bio.lobster'),'data','inputs','ccir_groupings.rdata')) #object names Groupings
		load(file.path(project.datadirectory('bio.lobster'),'data','inputs','ccir_seasons.rdata'))
		lobster.db('ccir')

		logs = lobster.db('process.logs')
		logs$LFA = ifelse(logs$LFA == '31A','31a',logs$LFA)
		logs$LFA = ifelse(logs$LFA == '31B','31b',logs$LFA)

		require(bio.ccir)
		require(rstan)
		#load_all(paste(git.repo,'bio.ccir',sep="/")) # for debugging
		ccir_data = subset(ccir_data,YEAR<2019)
		dat = ccir_compile_data(x = ccir_data,log.data = logs, area.defns = Groupings[7], size.defns = inp, season.defns = Seasons, sexs = 1.5) #sexs 1.5 means no sex defn
		
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
		save(ouBin,file=file.path(project.datadirectory('bio.lobster'),'outputs','ccir','summary','compiledBinomialModels33.rdata'))
		#load(file=file.path(project.datadirectory('bio.lobster'),'outputs','ccir','summary','compiledBinomialModels33.rdata'))

		g = unique(ouBin$Grid)
		g = strsplit(g,"\\.")
		o = aggregate(WEIGHT_KG~SYEAR,data=subset(logs,GRID_NUM %in% g[[1]]),FUN=sum)
		names(o)[2] = g[[1]][1]
		o2 = aggregate(WEIGHT_KG~SYEAR,data=subset(logs,GRID_NUM %in% g[[2]]),FUN=sum)
		names(o2)[2] = g[[2]][1]
		o = merge(o,o2)
		names(o)[1] = 'Yr'
		oo <- ccir_timeseries_exploitation_plots(ouBin,combined.LFA=T,landings=o)

		RR75 = max(oo$ERf75[oo$Yr<2017])


		save(oo,file=file.path(project.datadirectory('bio.lobster'),'outputs','ccir','summary','compiledExploitationCCIR33.rdata'))
		load(file=file.path(project.datadirectory('bio.lobster'),'outputs','ccir','summary','compiledExploitationCCIR33.rdata'))

	# plot
	x11(width=8,height=5)
	ExploitationRatePlots(data = oo[,c("Yr","ERfm","ERfl","ERfu")],lrp=RR75,lfa = 33,fd=figdir)


		# FSRS
		FSRSvesday<-FSRSModelData()

		mdata = subset(FSRSvesday,LFA==33&SYEAR<2019)

		FSRSModelResultsShort=FSRSmodel(mdata,lfa=33, response="SHORTS",interaction=F,type="bayesian",iter=5000,redo=T,ptraps=1000)

		FSRSModelResultsLegal=FSRSmodel(mdata,lfa=33, response="LEGALS",interaction=F,type="bayesian",iter=5000,redo=T,ptraps=1000)

		FSRSModelResultsRecruit=FSRSmodel(mdata,lfa=33, response="RECRUITS",interaction=F,type="bayesian",iter=5000,redo=T,ptraps=1000)


		FSRSModelResultsShort=FSRSmodel(mdata,lfa=33, response="SHORTS",interaction=F,type="bayesian",iter=5000,redo=F,ptraps=1000)
		shorts = FSRSModelResultsShort$pData

		FSRSModelResultsLegal=FSRSmodel(mdata,lfa=33, response="LEGALS",interaction=F,type="bayesian",iter=5000,redo=F,ptraps=1000)
		legals = FSRSModelResultsLegal$pData

		FSRSModelResultsRecruit=FSRSmodel(mdata,lfa=33, response="RECRUITS",interaction=F,type="bayesian",iter=5000,redo=F,ptraps=1000)
		recruit =  FSRSModelResultsRecruit$pData

	# plot
	x11(width=8,height=5)
	FSRSCatchRatePlot(data = recruit[,c("YEAR","median")],lfa = 33,fd=figdir)


		# Landings and Effort
	 	land = lobster.db('seasonal.landings')

	 	

