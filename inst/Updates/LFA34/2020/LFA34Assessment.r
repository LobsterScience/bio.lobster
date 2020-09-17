# LFA 34 Assessment Script
#	______
#	`>___;'______       /3
#	  ---~<.     ))))))) 3
#	 _____ `,-----%%%%% \3
#	 `>___;- |}}}	 
#           
 require(bio.lobster)
 require(devtools)
 require(lubridate)
 require(bio.utilities)
 require(SpatialHub)
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
		CPUE.data<-CPUEModelData(p,redo=T,TempModelling)

		## Commercial CPUE MOdels
		mf1 = formula(logWEIGHT ~ fYEAR + DOS + TEMP + DOS * TEMP)

	#	CPUE.data<- CPUEModelData(p,redo=F)
		t=mean(subset(CPUE.data,DOS==1)$TEMP)

		mdata = subset(CPUE.data,SYEAR%in%p$yrs & LFA==34)

		CPUEModelResults = CPUEmodel(mf1,mdata,t=t,d=1,lfa=34)
		crd = CPUEModelResults$pData[,c("YEAR","mu")]
		cpue1= CPUEModelPlot(CPUEModelResults,TempModelling,
			mdata=mdata,combined=T, lfa = p$lfas,xlim=c(1989,2019.4),ylim=c(0,10.5),
			graphic='R',path=figdir,lab=1,wd=11,ht=8)

	# plot
	x11(width=8,height=5)
	CatchRatePlot(data = crd, lfa = 34, fd=figdir)
	cpueData=read.csv(file.path(figdir,"CatchRateRefs34.csv"))

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
		load(file=file.path(project.datadirectory('bio.lobster'),'outputs','ccir','summary','compiledBinomialModels34.rdata'))
		ExploitationRatePlots(data = ouBin[,c("Yr","ERfm","ERfl","ERfu")],lfa = 34,fd=figdir,runM=T)

		with(subset(ouBin,LFA=='33W'),lines(Yr,ERfm,lty=2,col='blue'))

		ExploitationRatePlots(data = oo[,c("Yr","ERfm","ERfl","ERfu")],lrp=RR75,lfa = 34,fd=figdir)



# FSRS #############

		FSRSvesday<-FSRSModelData()

		mdata = subset(FSRSvesday,LFA==34)

	#	FSRSModelResultsLegal=FSRSmodel(mdata,lfa=34, response="LEGALS",interaction=F,type="bayesian",iter=5000,redo=T,ptraps=1000)
	#	FSRSModelShortsRecruit=FSRSmodel(mdata,lfa=34, response="SHORTS",interaction=F,type="bayesian",iter=5000,redo=T,ptraps=1000)
		FSRSModelResultsRecruit=FSRSmodel(mdata,lfa=34, response="RECRUITS",interaction=F,type="bayesian",iter=5000,redo=T,ptraps=1000)

	#	FSRSModelResultsLegal=FSRSmodel(mdata,lfa=34, response="LEGALS",interaction=F,type="bayesian",iter=5000,redo=F,ptraps=1000)
	#	legals = FSRSModelResultsLegal$pData
	#	legals$Area = 34
		
		FSRSModelResultsRecruit=FSRSmodel(mdata,lfa=34, response="RECRUITS",interaction=F,type="bayesian",iter=5000,redo=F,ptraps=1000)
		recruit =  FSRSModelResultsRecruit$pData
		recruit$Area = 34
fpf1 = file.path(project.figuredirectory('bio.lobster'),"LFA34Assessment")
		write.csv(recruit, file=file.path(fpf1,'FSRSRecruit.csv'))


 	 png(file=file.path(fpf1,'FSRSLFA34Recruitment.png'),units='in',width=15,height=12,pointsize=18, res=300,type='cairo')
 	 with(recruit,plot(YEAR,median,pch=16,xlab='Year',ylab='Recruit abundance',ylim=c(0,5)))
	 with(recruit,arrows(YEAR,y0=lb,y1=ub, length=0))
	 xx = rmed(recruit$YEAR,recruit$median)
	 xx = as.data.frame(do.call(cbind,xx))
	 with(subset(xx),lines(yr,x,col='salmon',lwd=3))
	 dev.off()


	#	FSRSModelShortsRecruit=FSRSmodel(mdata,lfa=34, response="SHORTS",interaction=F,type="bayesian",iter=5000,redo=F,ptraps=1000)
	#	shorts =  FSRSModelShortsRecruit$pData
	#	shorts$Area = 34
 	
 	save(list=c("shorts","legals","recruit"),file=file.path(project.datadirectory("bio.lobster"),"outputs","fsrsModelIndicators34.rdata"))


	# plot
	x11(width=8,height=7)
	FSRSCatchRatePlot(recruits = recruit[,c("YEAR","median","lb","ub")],legals=legals[,c("YEAR","median","lb","ub")],lfa = 34,fd=figdir,title='')


# Landings and Effort ############

	 	land = lobster.db('seasonal.landings')


		land$YEAR = as.numeric(substr(land$SYEAR,6,9))
		land$LANDINGS = land$LFA34
		fishData = merge(cpueData,land[,c("YEAR","LANDINGS")]) 
		fishData$EFFORT2 = fishData$LANDINGS * 1000 / fishData$mu



	# plot
	x11(width=8,height=5)
	FisheryPlot(fishData[,c("YEAR","LANDINGS","EFFORT2")],lfa = 34,fd=figdir, preliminary=nrow(fishData), units='kt')



# Contextual Indicators #############


##Recruits
ff = "LFA34Assessment"
fp1 = file.path(project.datadirectory('bio.lobster'),"analysis",ff)
fpf = fpf1 = file.path(project.figuredirectory('bio.lobster'),ff)

Sp = read.csv(file=file.path(fpf,paste('LFA34-NEFSCSpringrecruits.csv',sep="-")))
DF = read.csv(file=file.path(fpf,paste('LFA34-DFOrecruits.csv',sep="-")))
Fa = read.csv(file=file.path(fpf,paste('LFA34-NEFSCfallrecruits.csv',sep="-")))
FS = read.csv(file=file.path(fpf1,'FSRSRecruit.csv'))

fpf1 = file.path(project.figuredirectory('bio.lobster'),"LFA3438Framework2019")

load(file=file.path(project.datadirectory('bio.lobster'),'data','LFA3438Framework','ScallopSurveyIndicators.rdata'))

SFA29 = ScallopSurveyIndicatorsF34
yrs29=as.numeric(row.names(SFA29))



IL = read.csv(file=file.path(fpf1,'ILTSRecruitN.csv'))
png(file=file.path(fpf,'recruitabund.png'),units='in',width=10,height=8,pointsize=18, res=300,type='cairo')
par(mfrow=c(3,2), mar=c(4,4,2,1))

with(Sp,plot(yr,n.yst,pch=16,xlab='Year',ylab = 'NEFSC Spring Recruit Abundance',ylim=c(0,15)))
with(Sp, arrows(yr, y0=n.ci.yst.l, y1=n.ci.yst.u, length=0))
with(Sp,lines(yr,runmed(n.yst,3),lwd=2, col='salmon'))


with(Fa,plot(yr,n.yst,pch=16,xlab='Year',ylab = 'NEFSC Fall Recruit Abundance',ylim=c(0,35)))
with(Fa, arrows(yr, y0=n.ci.yst.l, y1=n.ci.yst.u, length=0))
with(Fa,lines(yr,runmed(n.yst,3),lwd=2, col='salmon'))


with(DF,plot(yr,n.yst,pch=16,xlab='Year',ylab = 'DFO RV Recruit Abundance',ylim=c(0,15)))
with(DF, arrows(yr, y0=n.ci.yst.l, y1=n.ci.yst.u, length=0))
with(DF,lines(yr,runmed(n.yst,3),lwd=2, col='salmon'))


with(SFA29,plot(yrs29,R1.ab.f34,pch=16,xlab='Year',ylab = 'Scallop Survey Recruit Abundance'))
with(SFA29,lines(yrs29,runmed(R1.ab.f34,3),lwd=2, col='salmon'))

with(IL,plot(Year,B,pch=16,xlab='Year',ylab = 'ILTS Recruit Abundance'))
with(IL, arrows(Year, y0=lB, y1=uB, length=0))
with(IL,lines(Year,runmed(B,3),lwd=2, col='salmon'))


with(FS,plot(YEAR,mu,pch=16,xlab='Year',ylab = 'FSRS Recruit Abundance',ylim=c(1,5.75)))
with(FS, arrows(YEAR, y0=lb, y1=ub, length=0))
with(FS,lines(YEAR,runmed(mu,3),lwd=2, col='salmon'))
dev.off()

#Contextual 1
ff = "LFA34Assessment"
fp1 = file.path(project.datadirectory('bio.lobster'),"analysis",ff)
fpf = fpf1 = file.path(project.figuredirectory('bio.lobster'),ff)

Sp = read.csv(file=file.path(fpf,paste('LFA34-NEFSCSpringtotalabund.csv',sep="-")))
DF = read.csv(file=file.path(fpf,paste('LFA34-DFOtotalabund.csv',sep="-")))
Fa = read.csv(file=file.path(fpf,paste('LFA34-NEFSCfalltotalabund.csv',sep="-")))


png(file=file.path(fpf,'Contextual1.png'),units='in',width=10,height=8,pointsize=18, res=300,type='cairo')
par(mfrow=c(2,3), mar=c(4,4,2,1))

with(Sp,plot(yr,n.yst,pch=16,xlab='Year',ylab = 'NEFSC Spring Total Abundance'))
with(Sp, arrows(yr, y0=n.ci.yst.l, y1=n.ci.yst.u, length=0))
with(Sp,lines(yr,runmed(n.yst,3),lwd=2, col='salmon'))


with(Fa,plot(yr,n.yst,pch=16,xlab='Year',ylab = 'NEFSC Fall Total Abundance'))
with(Fa, arrows(yr, y0=n.ci.yst.l, y1=n.ci.yst.u, length=0))
with(Fa,lines(yr,runmed(n.yst,3),lwd=2, col='salmon'))


with(DF,plot(yr,n.yst,pch=16,xlab='Year',ylab = 'DFO RV Total Abundance'))
with(DF, arrows(yr, y0=n.ci.yst.l, y1=n.ci.yst.u, length=0))
with(DF,lines(yr,runmed(n.yst,3),lwd=2, col='salmon'))

Sp = read.csv(file=file.path(fpf,paste('LFA34-NEFSCSpringtemperature.csv',sep="-")))
DF = read.csv(file=file.path(fpf,paste('LFA34-DFOtemperature.csv',sep="-")))
Fa = read.csv(file=file.path(fpf,paste('LFA34-NEFSCfalltemperature.csv',sep="-")))


with(Sp,plot(yr,n.yst,pch=16,xlab='Year',ylab = 'NEFSC Spring Temperature'))
with(Sp, arrows(yr, y0=n.ci.yst.l, y1=n.ci.yst.u, length=0))
with(Sp,lines(yr,runmed(n.yst,3),lwd=2, col='salmon'))


with(Fa,plot(yr,n.yst,pch=16,xlab='Year',ylab = 'NEFSC Fall Temperature'))
with(Fa, arrows(yr, y0=n.ci.yst.l, y1=n.ci.yst.u, length=0))
with(Fa,lines(yr,runmed(n.yst,3),lwd=2, col='salmon'))


with(DF,plot(yr,n.yst,pch=16,xlab='Year',ylab = 'DFO RV Temperature'))
with(DF, arrows(yr, y0=n.ci.yst.l, y1=n.ci.yst.u, length=0))
with(DF,lines(yr,runmed(n.yst,3),lwd=2, col='salmon'))
dev.off()


#Contextual 2
ff = "LFA34Assessment"
fp1 = file.path(project.datadirectory('bio.lobster'),"analysis",ff)
fpf = fpf1 = file.path(project.figuredirectory('bio.lobster'),ff)

Sp = read.csv(file=file.path(fpf,paste('LFA34-NEFSCSpringtotalabund.csv',sep="-")))
DF = read.csv(file=file.path(fpf,paste('LFA34-DFOtotalabund.csv',sep="-")))
Fa = read.csv(file=file.path(fpf,paste('LFA34-NEFSCfalltotalabund.csv',sep="-")))


png(file=file.path(fpf,'Contextual2.png'),units='in',width=10,height=8,pointsize=18, res=300,type='cairo')
par(mfrow=c(2,3), mar=c(4,4,2,1))

with(Sp,plot(yr,dwao,pch=16,xlab='Year',ylab = 'NEFSC Spring Area Occupied'))
#with(Sp, arrows(yr, y0=n.ci.yst.l, y1=n.ci.yst.u, length=0))
with(Sp,lines(yr,runmed(dwao,3),lwd=2, col='salmon'))


with(Fa,plot(yr,dwao,pch=16,xlab='Year',ylab = 'NEFSC Fall Area Occupied'))
#with(Fa, arrows(yr, y0=n.ci.yst.l, y1=n.ci.yst.u, length=0))
with(Fa,lines(yr,runmed(dwao,3),lwd=2, col='salmon'))


with(DF,plot(yr,dwao,pch=16,xlab='Year',ylab = 'DFO RV Area Occupied'))
#with(DF, arrows(yr, y0=n.ci.yst.l, y1=n.ci.yst.u, length=0))
with(DF,lines(yr,runmed(dwao,3),lwd=2, col='salmon'))

with(Sp,plot(yr,gini,pch=16,xlab='Year',ylab = 'NEFSC Spring Gini Index'))
#with(Sp, arrows(yr, y0=n.ci.yst.l, y1=n.ci.yst.u, length=0))
with(Sp,lines(yr,runmed(gini,3),lwd=2, col='salmon'))


with(Fa,plot(yr,gini,pch=16,xlab='Year',ylab = 'NEFSC Fall Gini Index'))
#with(Fa, arrows(yr, y0=n.ci.yst.l, y1=n.ci.yst.u, length=0))
with(Fa,lines(yr,runmed(gini,3),lwd=2, col='salmon'))


with(DF,plot(yr,gini,pch=16,xlab='Year',ylab = 'DFO RV Gini Index'))
#with(DF, arrows(yr, y0=n.ci.yst.l, y1=n.ci.yst.u, length=0))
with(DF,lines(yr,runmed(gini,3),lwd=2, col='salmon'))

dev.off()

#Contextual 3

png(file=file.path(fpf,'Contextual3.png'),units='in',width=10,height=4,pointsize=18, res=300,type='cairo')
par(mfrow=c(1,3), mar=c(4,4,2,1))


gL = read.csv(file=file.path(project.figuredirectory('bio.lobster'),"LFA34Assessment",'GiniLandings34.csv'))
DF = read.csv(file=file.path(fpf1,'predatorIndex34.csv'))

with(gL, plot(YEAR, LANDINGS, pch=16, xlab='Year',ylab='Gini Landings'))
with(gL,lines(YEAR,runmed(LANDINGS,3),lwd=2, col='salmon'))

with(DF,plot(yr,n.yst,pch=16,xlab='Year',ylab = 'DFO RV Predator Abundance',ylim=c(0,300)))
with(DF, arrows(yr, y0=n.ci.yst.l, y1=n.ci.yst.u, length=0))
with(DF,lines(yr,runmed(n.yst,3),lwd=2, col='salmon'))


with(DF,plot(yr,w.yst,pch=16,xlab='Year',ylab = 'DFO RV Predator Biomass',ylim=c(0,210)))
with(DF, arrows(yr, y0=w.ci.yst.l, y1=w.ci.yst.u, length=0))
with(DF,lines(yr,runmed(w.yst,3),lwd=2, col='salmon'))

dev.off()