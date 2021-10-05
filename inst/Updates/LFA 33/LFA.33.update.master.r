# LFA 33 Update Script

	p = bio.lobster::load.environment()
	la()



	    # define place for figures to go
	    figdir = file.path(project.datadirectory("bio.lobster","assessments","Updates","LFA33",p$current.assessment.year))
	    dir.create( figdir, recursive = TRUE, showWarnings = FALSE )
	    p$lfas = c("33") # specify lfas for data summary
	    p$subareas = c("33W", "33E") # specify lfas for data summary

	    # update data through ROracle
	    NewDataPull =F
	    #NewDataPull =T

	    if(NewDataPull){
	      lobster.db('fsrs.redo')
	      lobster.db('logs.redo')
	      lobster.db('annual.landings.redo')
	      lobster.db('seasonal.landings.redo')
	      #lobster.db('vlog.redo')
	      logs=lobster.db('process.logs.redo')
	    }


# Map ################

		#x11(width=5, height=5)
	#	LobsterMap('33')
	#	savePlot(file.path(figdir,'LFA33map.png'),type='png')

		#or

		png(filename=file.path(figdir, "MapLFA33.png"),width=5, height=5, units = "in", res = 800)
		LobsterMap('33')
		dev.off()

# CPUE ###############

	# 	logs=lobster.db("process.logs")
	# 	CPUE.data<-CPUEModelData(p,redo=F)
	# 	cpueData = CPUEplot(CPUE.data,lfa= p$lfas,yrs=1982:2020,graphic='R', plot=F)$annual.data
	# 	crd = subset(cpueData,LFA==33,c("YEAR","CPUE"))
	#
	# #	mu = with(subset(cpueData,YEAR<2017&YEAR>1989),tapply(CPUE,LFA,median))
	# 	mu=median(crd$CPUE[crd$YEAR %in% c(1990:2016)])
	# 	usr = mu * 0.8
	# 	lrp = mu * 0.4
	#
	# 	crd  = merge(data.frame(YEAR=min(crd$YEAR):max(crd$YEAR)),crd,all.x=T)
	#
	# 	crd = subset(cpueData,LFA==33,c("YEAR","CPUE"))
	# 	mu=median(crd$CPUE[crd$YEAR %in% c(1990:2016)])
	# 	usr = mu * 0.8
	# 	lrp = mu * 0.4
	#
	# 	crd  = merge(data.frame(YEAR=min(crd$YEAR):max(crd$YEAR)),crd,all.x=T)
	#
		logs=lobster.db("process.logs")

		#Choose one to redo or not Add TempSkip=T to not model CPUE with Temps
		#CPUE.data<-CPUEModelData(p,redo=T,TempSkip=T)
		#CPUE.data<-CPUEModelData(p,redo=F)
		
		cpueData=    CPUEplot(CPUE.data,lfa= p$lfas,yrs=1981:max(CPUE.data$SYEAR),graphic='R')$annual.data
		crd = subset(cpueData,LFA==33,c("YEAR","CPUE"))
		mu = median(crd$CPUE[crd$YEAR %in% c(1990:2016)])
		usr = mu * 0.8
		lrp = mu * 0.4


		write.csv(crd,file.path(figdir,file='CatchRateRefs33.csv'))

		png(filename=file.path(figdir, "CPUE_only.png"),width=8, height=5.5, units = "in", res = 800)
    		par(mar=c(2.0,5.5,2.0,3.0))
    		xlim=c(1990,max(crd$YEAR))
    		plot(crd[,1],crd[,2],xlab=' ',ylab='CPUE (kg/TH)',type='b',pch=16,xlim=xlim, ylim=c(0, 1.05*max(crd$CPUE)))
    		points(max(crd$YEAR), crd$CPUE[which(crd$YEAR==max(crd$YEAR))], pch=17, col='red', cex=1.2)
		dev.off()

    png(filename=file.path(figdir, "CPUE_LFA33.png"),width=8, height=5.5, units = "in", res = 800)
        par(mar=c(2.0,5.5,2.0,3.0))
        xlim=c(1990,max(crd$YEAR))
        plot(crd[,1],crd[,2],xlab=' ',ylab='CPUE (kg/TH)',type='p',pch=16,xlim=xlim, ylim=c(0, 1.05*max(crd$CPUE)))
        points(max(crd$YEAR), crd$CPUE[which(crd$YEAR==max(crd$YEAR))], pch=17, col='red', cex=1.2)
        running.median = with(rmed(crd[,1],crd[,2]),data.frame(YEAR=yr,running.median=x))
        crd=merge(crd,running.median,all=T)
        lines(crd[,1],crd$running.median,col='blue',lty=1,lwd=2)
        abline(h=usr,col='green',lwd=2,lty=2)
        abline(h=lrp,col='red',lwd=2,lty=3)
    dev.off()
    # plot
    #x11(width=8,height=5)
    CatchRatePlot(data = crd ,usr = usr,lrp=lrp,lfa = 33,fd=figdir, save=F)



# CCIR ###############
#The following two tables need to be updated to reflect current year:
#C:\bio.data\bio.lobster\data\inputs\ccir.inputs.csv
#C:\bio.data\bio.lobster\data\inputs\MinLegalSize
    
		lobster.db('ccir.redo')
		inp = read.csv(file.path(project.datadirectory('bio.lobster'),'data','inputs','ccir_inputs.csv'))
		load(file.path(project.datadirectory('bio.lobster'),'data','inputs','ccir_groupings.rdata')) #object names Groupings
		load(file.path(project.datadirectory('bio.lobster'),'data','inputs','ccir_seasons.rdata'))

    
		lobster.db('ccir')

		logs = lobster.db('process.logs')


		require(bio.ccir)
		require(rstan)
		#load_all(paste(git.repo,'bio.ccir',sep="/")) # for debugging
		ccir_data = subset(ccir_data,YEAR<=2021)
		dat = ccir_compile_data(x = ccir_data,log.data = logs, area.defns = Groupings[7], size.defns = inp, season.defns = Seasons, sexs = 1.5) #sexs 1.5 means no sex defn

		out.binomial = list()
		attr(out.binomial,'model') <- 'binomial'
		for(i in 34:length(dat)) { #if run breaks, update 1:length(dat) to reflect run# ie.e 16:length(dat)
			ds = dat[[i]]
			ds$method = 'binomial'
			x = ccir_stan_run(dat = ds,save=T)
			out.binomial[[i]] <- ccir_stan_summarize(x)
		}

		#load statement below combines ccir summaries if broken runs
		#ensure folder has only model run summaries
		da = file.path(project.datadirectory('bio.lobster'),'outputs','ccir','summary') #modify as required

		d = list.files(da,full.names=T)
		out.binomial = list()

		for( i in 1:length(d)){
		  load(d[i])
		  out.binomial[[i]] <- out
		}


		out.binomial[[1]]$LFA = "33W"
		out.binomial[[2]]$LFA = "33E"
		ouBin = ccir_collapse_summary(out.binomial)
		attr(ouBin,'model') <- 'binomial'
		#ouBin$Yr = ouBin$Yr +1
		save(ouBin,file=file.path(project.datadirectory('bio.lobster'),'outputs','ccir','summary','compiledBinomialModels33.rdata'))
		load(file=file.path(project.datadirectory('bio.lobster'),'outputs','ccir','summary','compiledBinomialModels33.rdata'))

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
		RR75 = max(oo$ERf75[oo$Yr<2021])#0.8343305
#########Linux to here

#oo=read.csv(file.path(figdir, "LFA33ccirout.csv"))
	# plot

png(filename=file.path(figdir, "CCIR_LFA33.png"),width=8, height=5, units = "in", res = 800)
ExploitationRatePlots(data = oo[,c("Yr","ERfm","ERfl","ERfu")],lrp=RR75,lfa = 33,fd=figdir, save=F)
dev.off()

#data = oo[,c("Yr","ERfm","ERfl","ERfu")]
#write.csv(data,file.path(figdir,paste('CCIR_LFA33.csv',sep='')))

# FSRS #############

		FSRSvesday<-FSRSModelData()

		mdata = subset(FSRSvesday,LFA==33&SYEAR<2022)

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
	#x11(width=8,height=7)
 	png(filename=file.path(figdir, "FSRS.legals.recruits.png"),width=8, height=5, units = "in", res = 800)
	FSRSCatchRatePlot(recruits = recruit[,c("YEAR","median","lb","ub")],legals=legals[,c("YEAR","median","lb","ub")],lfa = 33,fd=figdir,title='')
dev.off()

# Landings and Effort ############

	 	land = lobster.db('seasonal.landings')


		land$YEAR = as.numeric(substr(land$SYEAR,6,9))
		land$LANDINGS = land$LFA33
		fishData = merge(cpueData,land[,c("YEAR","LANDINGS")])
		fishData$EFFORT2 = fishData$LANDINGS * 1000 / fishData$CPUE

		# plot Landings

		png(filename=file.path(figdir, "Landings_LFA33.png"),width=8, height=5, units = "in", res = 800)
    		par(mar=c(5.1, 4.1, 4.1, 5.1),las=1)
    		plot(fishData$YEAR,fishData$LANDINGS,xlab='Year',ylab='Landings (t)',type='h',main="LFA 33",ylim=c(0,max(fishData$LANDINGS)*1.2),pch=15,col='grey',lwd=10,lend=3)
    		lines(max(fishData$YEAR),fishData$LANDINGS[length(fishData$LANDINGS)],type='h',pch=21,col=rgb(1,0.6,0),lwd=10,lend=3)
    dev.off()

	# plot Landings / Effort Together

		png(filename=file.path(figdir, "Landings_Effort_LFA33.png"),width=8, height=5, units = "in", res = 800)
    		#FisheryPlot <- function(data,lfa=NULL,fd=file.path(project.figuredirectory('bio.lobster','ReferencePoints')),title = paste('LFA',lfa),fn=paste0('FisheryPlot',lfa),preliminary=NULL,units='t',...) {
    	  par(mar=c(5.1, 4.1, 4.1, 5.1),las=1)
    	  plot(fishData$YEAR,fishData$LANDINGS,xlab='Year',ylab='Landings (t)',type='h',main="LFA 33",ylim=c(0,max(fishData$LANDINGS)*1.2),pch=15,col='grey',lwd=10,lend=3)
    	  lines(max(fishData$YEAR),fishData$LANDINGS[length(fishData$LANDINGS)],type='h',pch=21,col=rgb(1,0.6,0),lwd=10,lend=3)

    	  par(new=T)
    	  plot(fishData$YEAR,fishData$EFFORT2/1000,ylab='',xlab='', type='b', pch=16, axes=F,ylim=c(0,max(fishData$EFFORT2/1000,na.rm=T)))
    	  points(max(fishData$YEAR),fishData$EFFORT2[length(fishData$EFFORT2)]/1000, type='b', pch=21,bg=rgb(1,0.6,0))
    	  axis(4)
    	  mtext("Effort ('000s Trap Hauls)", 4, 3.5, outer = F,las=0)

    	  write.csv(fishData,file.path(figdir,paste('FisheryPlot33.csv',sep='')))
	  dev.off()

	  #-----------------------------------------------------------------------

	  # to compare weekly fishing effort year to year (include in AC presentation if desired)
	  logs33=logs[logs$LFA=="33",]
	  logs33$unique_days=paste(logs33$VR_NUMBER, logs33$DATE_FISHED, sep=':')


	  #To Double Check Number of Fishing Days in each week of season
	  days=aggregate(DATE_FISHED~WOS+SYEAR,data=logs33,FUN=function(x) length(unique(x)))
	  days.y0=days[days$SYEAR==max(days$SYEAR),]
	  days.y1=days[days$SYEAR==(max(days$SYEAR)-1),]
	  plot(x=days$WOS,y=days$DATE_FISHED, type='n', main= "Days Fished by Week", xlab="Week of Season", ylab="Days Fished", xlim=c(2,27))
	  lines(days.y0$WOS, days.y0$DATE_FISHED, col="red", lwd=2)
	  text(paste(days.y0$SYEAR[1]), x=25, y=1, col="red", cex=1.6)
	  lines(days.y1$WOS, days.y1$DATE_FISHED, col="blue")
	  text(paste(days.y1$SYEAR[1]), x=25, y=2, col="blue", cex=1.6)

	  # to plot weekly fishing effort year to year (include in AC presentation if desired)
	  png(filename=file.path(figdir, "Weekly_Comparison.png"),width=8, height=5.5, units = "in", res = 1200)
    	  days=aggregate(unique_days~WOS+SYEAR, data=logs33, length)
    	  days.y0=days[days$SYEAR==max(days$SYEAR),]
    	  days.y1=days[days$SYEAR==(max(days$SYEAR)-1),]
    	  plot(x=days$WOS,y=days$unique_days, type='n', main= "Days Fished by Week", xlab="Week of Season", ylab="Days Fished", xlim=c(2,27), xaxt='n')
    	  axis(side=1, at=seq(1,25,by=4.4), lab=c('Dec','Jan', 'Feb', "Mar", 'Apr', 'May'))
    	  lines(days.y0$WOS, days.y0$unique_days, col="red")
    	  text(paste(days.y0$SYEAR[1]), x=26, y=300, col="red", cex=1.5)
    	  lines(days.y1$WOS, days.y1$unique_days, col="blue")
    	  text(paste(days.y1$SYEAR[1]), x=26, y=800, col="blue", cex=1.5)
	  dev.off()

	  
	  # Phase plot for conclusions and advice
	  
	  #x11(width=8,height=7)
	  
	  x = read.csv(file.path(figdir,"CatchRateRefs33.csv"))
	  y = read.csv(file.path(figdir,"ExploitationRefs33.csv"))
	  
	  x=x[x$YEAR %in% unique(y$Yr)]
	  
	  RR75 = 0.8343305
	  usr = 0.2840067
	  lrp = 0.1420034
	  
	  png(filename=file.path(figdir, "PhasePlot_LFA33.png"),width=8, height=8, units = "in", res = 800)
	  hcrPlot(B=x$running.median[x$YEAR>2005],mF=y$running.median,USR=usr,LRP=lrp,RR=RR75,yrs=2006:max(x$YEAR),ylims=c(0,1),xlims=NULL,labels=c('USR','LRP','RR'),RRdec=F, ylab = 'Exploitation', xlab = 'CPUE',yr.ends=T)
	  dev.off()
	  
	  #hcrPlot(B=x$CPUE[x$YEAR>2005],mF=y$ERfm,USR=usr,LRP=lrp,RR=RR75,yrs=2006:2018,ylims=c(0,1),xlims=NULL,labels=c('USR','LRP','RR'),RRdec=F, ylab = 'Exploitation', xlab = 'CPUE',yr.ends=T)
	  #savePlot(file.path(figdir,'PhasePlot33.png'),type='png')


# Contextual Indicators #############


#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#mood


# run this "bio/bio.lobster/inst/LFA2733Framework/Assessment/1.IndicatorEstimation.CohortAnalysis.r"
# and this "bio/bio.lobster/inst/LFA2733Framework/Assessment/ContextualIndicators.r"

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

