# LFA 27-32 Assessment Script
#	______
#	`>___;'______       /3
#	  ---~<.     ))))))) 3
#	 _____ `,-----%%%%% \3
#	 `>___;- |}}}	 
#            
 
	p = bio.lobster::load.environment()
	la()

		assessment.year = p$current.assessment.year ########### check the year ############### !!!!!!!!!!!
		p$current.assessment.year = p$current.assessment.year - 1 ########### check the year ############### !!!!!!!!!!!


	    # define place for figures to go
	    figdir = file.path(project.datadirectory("bio.lobster"),"figures","Assessment","LFA2732")

    	p$lfas = c("27", "28", "29", "30", "31A", "31B", "32") # specify lfas for data summary
    	p$subareas = c("27N","27S", "28", "29", "30", "31A", "31B", "32") # specify lfas for data summary

	    # update data through ROracle
	    #lobster.db('fsrs.redo')
	    #lobster.db('logs.redo')
	    TempModelData(save=T) # update temperature data

	    logs=lobster.db('process.logs.redo')
		CPUE.data<-CPUEModelData(p,redo=T)
	 

	 pdf(file.path(figdir,"MapLFA2732.pdf"),4.5,5)
	 LobsterMap('27-32')
	 dev.off()

	 for(i in 1:length(p$lfas)){

		 pdf(file.path(figdir,paste0("MapLFA",p$lfas[i],".pdf")),4.5,5)
		 LobsterMap(p$lfas[i])
		 dev.off()
	     pdf2png(file.path(figdir,paste0("MapLFA",p$lfas[i],".pdf")))


	 }


	 

# CPUE ###############
		
		logs=lobster.db("process.logs")
		CPUE.data<-CPUEModelData(p,redo=F)
		cpueData=    CPUEplot(CPUE.data,lfa= p$lfas,yrs=1982:2018,graphic='R')$annual.data


		# Other LFAs
		mu = with(subset(cpueData,YEAR<2017&YEAR>1989),tapply(CPUE,LFA,median))	

		for(i in 1:length(p$lfas)){

			crd = subset(cpueData,LFA==p$lfas[i],c("YEAR","CPUE"))	
			usr = mu[i] * 0.8
			lrp = mu[i] * 0.4

			# plot
			x11(width=8,height=5)

			CatchRatePlot(data = crd ,usr = usr,lrp=lrp,lfa = p$lfas[i],fd=figdir)


		}




# CCIR ###############

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


		load(file.path(project.datadirectory('bio.lobster'),'data','inputs','ccir_groupings.rdata')) #object names Groupings
		load(file.path(project.datadirectory('bio.lobster'),'data','inputs','ccir_seasons.rdata'))
		lobster.db('ccir')

		logs = lobster.db('process.logs')
		
		require(bio.ccir)
		require(rstan)

		load_all(paste(git.repo,'bio.ccir',sep="/")) # for debugging
		dat = ccir_compile_data(x = ccir_data,log.data = logs, area.defns = Groupings[1:6], size.defns = inp, season.defns = Seasons, sexs = 1.5) #sexs 1.5 means no sex defn
		
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

	# plot
	for(i in c("27", "29", "30", "31A", "31B", "32")){

		o = subset(oo,LFA==i)

		RR7 = subset(RR75,LFA==i)$ERf75

		x11(width=8,height=5)
		ExploitationRatePlots(data = o[,c("Yr","ERfm","ERfl","ERfu")],lrp=RR7,lfa = i,fd=figdir)


	}


# FSRS #############

	FSRSvesday<-FSRSModelData()

	for(i in c("27", "29", "30", "31A", "31B", "32")){

		mdata = subset(FSRSvesday,LFA==i&SYEAR<2019)

		FSRSModelResultsLegal=FSRSmodel(mdata,lfa=i, response="LEGALS",interaction=F,type="bayesian",iter=5000,redo=T,ptraps=1000)
		FSRSModelShortsRecruit=FSRSmodel(mdata,lfa=i, response="SHORTS",interaction=F,type="bayesian",iter=5000,redo=T,ptraps=1000)
		FSRSModelResultsRecruit=FSRSmodel(mdata,lfa=i, response="RECRUITS",interaction=F,type="bayesian",iter=5000,redo=T,ptraps=1000)

		FSRSModelResultsLegal=FSRSmodel(mdata,lfa=i, response="LEGALS",interaction=F,type="bayesian",iter=5000,redo=F,ptraps=1000)
		legals = FSRSModelResultsLegal$pData
		legals$Area = i
		
		FSRSModelResultsRecruit=FSRSmodel(mdata,lfa=i, response="RECRUITS",interaction=F,type="bayesian",iter=5000,redo=F,ptraps=1000)
		recruit =  FSRSModelResultsRecruit$pData
		recruit$Area = i

		FSRSModelShortsRecruit=FSRSmodel(mdata,lfa=i, response="SHORTS",interaction=F,type="bayesian",iter=5000,redo=F,ptraps=1000)
		shorts =  FSRSModelShortsRecruit$pData
		shorts$Area = i
 	
	 	save(list=c("shorts","legals","recruit"),file=file.path(project.datadirectory("bio.lobster"),"outputs",paste0("fsrsModelIndicators",i,".rdata")))


		# plot
		x11(width=8,height=7)
		FSRSCatchRatePlot(recruits = recruit[,c("YEAR","median","lb","ub")],legals=legals[,c("YEAR","median","lb","ub")],lfa = i,fd=figdir,title='')

	}


# Landings and Effort ############

	land = lobster.db('annual.landings')

	for(i in 1:length(p$lfas)){

		d1 = data.frame(YEAR = land$YR, LANDINGS = land[,paste0("LFA",p$lfas[i])])
		d2 = subset(cpueData,LFA==p$lfas[i])

		d2  = merge(data.frame(LFA=d2$LFA[1],YEAR=min(d2$YEAR):max(d2$YEAR)),d2,all.x=T)

		fishData = merge(d2,d1) 
		fishData$EFFORT2 = fishData$LANDINGS * 1000 / fishData$CPUE

		# plot
		x11(width=8,height=5)
		FisheryPlot(fishData[,c("YEAR","LANDINGS","EFFORT2")],lfa = p$lfas[i],fd=figdir)
	}



# Contextual Indicators #############


#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#mood


# run this "bio/bio.lobster/inst/LFA2733Framework/Assessment/ContextualIndicatorsSetup.r"
# and this "bio/bio.lobster/inst/LFA2733Framework/Assessment/ContextualIndicators.r"



# Phase plot for conclusions and advice
	
	load(file=file.path(project.datadirectory('bio.lobster'),'outputs','ccir','summary','compiledExploitationCCIR2732.rdata'))
	RR75  = aggregate(ERf75~LFA,data=oo[oo$Yr<2017,],FUN=max)

	lfas2 = c("27", "29", "30", "31A", "31B", "32")
	for(i in 1:length(lfas2)){

		x11(width=8,height=7)

		x = read.csv(file.path(figdir,paste0("CatchRateRefs",lfas2[i],".csv")))
		y = read.csv(file.path(figdir,paste0("ExploitationRefs",lfas2[i],".csv")))

		RR = subset(RR75,LFA==lfas2[i])$ERf75

		hcrPlot(B=x$running.median[x$YEAR>=min(y$Yr)],mF=y$running.median,USR=usr,LRP=lrp,RR=RR,yrs=min(y$Yr):assessment.year,ylims=c(0,1),xlims=NULL,labels=c('USR','LRP','RR'),RRdec=F, ylab = 'Exploitation', xlab = 'CPUE',yr.ends=T,main=paste("LFA",lfas2[i])) 
		savePlot(file.path(figdir,paste0('PhasePlot',lfas2[i],'.png')),type='png')
	}


# Fishery footprint


logs=lobster.db("process.logs")

catchgrids.lst=list()

	## Fishery Footprint - Landings
	catchLevels = c(0,100000,200000,300000,400000,500000,600000,700000)
	yrs = 2011:2018
	for(i in 1:length(yrs)){
		catchgrids.lst[[i]] = lobGridPlot(subset(logs,LFA%in%p$lfas&SYEAR==yrs[i],c("LFA","GRID_NUM","TOTAL_WEIGHT_KG")),FUN=sum,lvls=catchLevels)
		pdf(file.path(figdir,paste0("FisheryFootprint",yrs[i],".pdf")),4.5,5)
		LobsterMap('27-32',poly.lst=catchgrids.lst[[i]])
	  	title(yrs[i],line=-3,cex.main=2,adj=0.9)
	    SpatialHub::contLegend('topleft',lvls=catchgrids.lst[[i]]$lvls/1000,Cont.data=catchgrids.lst[[i]],title="Catch (tons)",inset=0.02,cex=0.8,bg='white')
	    dev.off()
	    pdf2png(file.path(figdir,paste0("FisheryFootprint",yrs[i])))
	}

