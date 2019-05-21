# LFA 33 Assessment Script
#	______
#	`>___;'______       /3
#	  ---~<.     ))))))) 3
#	 _____ `,-----%%%%% \3
#	 `>___;- |}}}	 
#           
 
	p = bio.lobster::load.environment()
	la()



	    # define place for figures to go
	    figdir = file.path(project.datadirectory("bio.lobster"),"figures","Assessment","LFA33")

	    p$lfas = "33" # specify lfa
    	p$subareas = c("33E", "33W") # specify subareas for data summary

	    # update data through ROracle
	    lobster.db('fsrs.redo')
	    lobster.db('logs.redo')
	    logs=lobster.db('process.logs.redo')
		#CPUE.data<-CPUEModelData(p,redo=T)
	 
# Map ################

		x11(width=5, height=5)
		LobsterMap('33')
		text(x=c(-65.5,-65.5,-64.5),y=c(43.1,42.7,42.7),labels=c(34,40,41),col=rgb(0,0,0,0.8),cex=1.5)

		savePlot(file.path(figdir,'LFA33map.png'),type='png')


# CPUE ###############
		
		logs=lobster.db("process.logs")
		CPUE.data<-CPUEModelData(p,redo=T)
		cpueData=    CPUEplot(CPUE.data,lfa= p$lfas,yrs=1981:2018,graphic='R')$annual.data
		crd = subset(cpueData,LFA==33,c("YEAR","CPUE"))	
		mu = median(crd$CPUE[crd$YEAR<2017])
		usr = mu * 0.8
		lrp = mu * 0.4


		# Other LFAs
		mu = with(subset(cpueData,YEAR<2017&YEAR>1989),tapply(CPUE,LFA,median))	



	# plot
	x11(width=8,height=5)
	CatchRatePlot(data = crd ,usr = usr,lrp=lrp,lfa = 33,fd=figdir)



# CCIR ###############

		lobster.db('ccir.redo') 
		inp = read.csv(file.path(project.datadirectory('bio.lobster'),'data','inputs','ccir_inputs.csv'))
		load(file.path(project.datadirectory('bio.lobster'),'data','inputs','ccir_groupings.rdata')) #object names Groupings
		load(file.path(project.datadirectory('bio.lobster'),'data','inputs','ccir_seasons.rdata'))
		lobster.db('ccir')

		logs = lobster.db('process.logs')
		
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
		out.binomial[[1]]$LFA = "33W"
		out.binomial[[2]]$LFA = "33E"
		ouBin = ccir_collapse_summary(out.binomial)
		attr(ouBin,'model') <- 'binomial' 
		#ouBin$Yr = ouBin$Yr +1
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

		save(oo,file=file.path(project.datadirectory('bio.lobster'),'outputs','ccir','summary','compiledExploitationCCIR33.rdata'))
		load(file=file.path(project.datadirectory('bio.lobster'),'outputs','ccir','summary','compiledExploitationCCIR33.rdata'))
		RR75 = max(oo$ERf75[oo$Yr<2017])#0.8146457

	# plot
	x11(width=8,height=5)
	ExploitationRatePlots(data = oo[,c("Yr","ERfm","ERfl","ERfu")],lrp=RR75,lfa = 33,fd=figdir)


# FSRS #############

		FSRSvesday<-FSRSModelData()

		mdata = subset(FSRSvesday,LFA==33&SYEAR<2019)

		FSRSModelResultsLegal=FSRSmodel(mdata,lfa=33, response="LEGALS",interaction=F,type="bayesian",iter=5000,redo=T,ptraps=1000)
		FSRSModelShortsRecruit=FSRSmodel(mdata,lfa=33, response="SHORTS",interaction=F,type="bayesian",iter=5000,redo=T,ptraps=1000)
		FSRSModelResultsRecruit=FSRSmodel(mdata,lfa=33, response="RECRUITS",interaction=F,type="bayesian",iter=5000,redo=T,ptraps=1000)

		FSRSModelResultsLegal=FSRSmodel(mdata,lfa=33, response="LEGALS",interaction=F,type="bayesian",iter=5000,redo=F,ptraps=1000)
		legals = FSRSModelResultsLegal$pData
		legals$Area = 33
		
		FSRSModelResultsRecruit=FSRSmodel(mdata,lfa=33, response="RECRUITS",interaction=F,type="bayesian",iter=5000,redo=F,ptraps=1000)
		recruit =  FSRSModelResultsRecruit$pData
		recruit$Area = 33

		FSRSModelShortsRecruit=FSRSmodel(mdata,lfa=33, response="SHORTS",interaction=F,type="bayesian",iter=5000,redo=F,ptraps=1000)
		shorts =  FSRSModelShortsRecruit$pData
		shorts$Area = 33
 	
 	save(list=c("shorts","legals","recruit"),file=file.path(project.datadirectory("bio.lobster"),"outputs","fsrsModelIndicators33.rdata"))


	# plot
	x11(width=8,height=7)
	FSRSCatchRatePlot(recruits = recruit[,c("YEAR","median","lb","ub")],legals=legals[,c("YEAR","median","lb","ub")],lfa = 33,fd=figdir,title='')


# Landings and Effort ############

	 	land = lobster.db('seasonal.landings')


		land$YEAR = as.numeric(substr(land$SYEAR,6,9))
		land$LANDINGS = land$LFA33
		fishData = merge(cpueData,land[,c("YEAR","LANDINGS")]) 
		fishData$EFFORT2 = fishData$LANDINGS * 1000 / fishData$CPUE



	# plot
	x11(width=8,height=5)
	FisheryPlot(fishData[,c("YEAR","LANDINGS","EFFORT2")],lfa = 33,fd=figdir)



# Contextual Indicators #############


#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#mood


# run this "bio/bio.lobster/inst/LFA2733Framework/Assessment/1.IndicatorEstimation.CohortAnalysis.r"
# and this "bio/bio.lobster/inst/LFA2733Framework/Assessment/ContextualIndicators.r"



# Phase plot for conclusions and advice

x11(width=8,height=7)

x = read.csv(file.path(figdir,"CatchRateRefs33.csv"))
y = read.csv(file.path(figdir,"ExploitationRefs33.csv"))
		RR75 = 0.8146457
		usr = 0.2840067
		lrp = 0.1420034


hcrPlot(B=x$running.median[x$YEAR>2005],mF=y$running.median,USR=usr,LRP=lrp,RR=RR75,yrs=2006:2018,ylims=c(0,1),xlims=NULL,labels=c('USR','LRP','RR'),RRdec=F, ylab = 'Exploitation', xlab = 'CPUE',yr.ends=T) 
#hcrPlot(B=x$CPUE[x$YEAR>2005],mF=y$ERfm,USR=usr,LRP=lrp,RR=RR75,yrs=2006:2018,ylims=c(0,1),xlims=NULL,labels=c('USR','LRP','RR'),RRdec=F, ylab = 'Exploitation', xlab = 'CPUE',yr.ends=T) 
savePlot(file.path(figdir,'PhasePlot.png'),type='png')


# Fishery footprint


logs=lobster.db("process.logs")

catchgrids.lst=list()

	## Fishery Footprint - Landings
	catchLevels = c(0,100000,200000,300000,400000,500000,600000,700000)
	yrs = 2011:2018
	for(i in 1:length(yrs)){
		catchgrids.lst[[i]] = lobGridPlot(subset(logs,LFA%in%p$lfas&SYEAR==yrs[i],c("LFA","GRID_NUM","TOTAL_WEIGHT_KG")),FUN=sum,lvls=catchLevels)
		pdf(file.path(figdir,paste0("FisheryFootprint",yrs[i],".pdf")),5,5)
		LobsterMap('33',poly.lst=catchgrids.lst[[i]])
		text(x=c(-65.5,-65.5,-64.5),y=c(43.1,42.7,42.7),labels=c(34,40,41),col=rgb(0,0,0,0.8),cex=1.5)
	  	title(yrs[i],line=-3,cex.main=2,adj=0.3)
	    SpatialHub::contLegend('bottomright',lvls=catchgrids.lst[[i]]$lvls/1000,Cont.data=catchgrids.lst[[i]],title="Catch (tons)",inset=0.02,cex=0.8,bg='white')
	    dev.off()
	    pdf2png(file.path(figdir,paste0("FisheryFootprint",yrs[i])))
	}

