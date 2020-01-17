  
	p = bio.lobster::load.environment()
	la()

	load_all('~/bio/bio.ccir')
	require(bio.ccir)

	require(car)
	require(rstan)

	redo.data = FALSE

	if(redo.data) {
		lobster.db('fsrs.redo') #this requires ODBC connection
		lobster.db('ccir.redo') #this does not
	}

	#Groupings for seasons, and regions -- Hard coded---Need to be manually updated
	inp = read.csv(file.path(project.datadirectory('bio.lobster'),'data','inputs','ccir_inputs.csv'))
	 load(file.path(project.datadirectory('bio.lobster'),'data','inputs','ccir_groupings.rdata')) #object names Groupings
	 load(file.path(project.datadirectory('bio.lobster'),'data','inputs','ccir_seasons.rdata'))
	lobster.db('ccir')

	logs = lobster.db('process.logs')
	logs$LFA = ifelse(logs$LFA == '31A','31a',logs$LFA)
	logs$LFA = ifelse(logs$LFA == '31B','31b',logs$LFA)

	reps = 5
	runs = c(0.5,0.6,0.7,0.8,0.9)
	out.binomial = list()
	attr(out.binomial,'model') <- 'binomial'
	lobster.db('ccir')

	for(k in 1:reps){

		for(r in 1:length(runs)){
			

			#thin ccir data
			lobster.db('ccir')
			ccir_data_thined = thin_ccir_data(ccir_data,p=runs[r])

			# LFA 27
			dat = ccir_compile_data(x = ccir_data_thined,log.data = logs, area.defns = Groupings[4], size.defns = inp, season.defns = Seasons, sexs = 1.5) #sexs 1.5 means no sex defn


			for(i in 1:length(dat)) {

				print(i)
				ds = dat[[i]]
				ds$method = 'binomial'
				x = ccir_stan_run(dat = ds,save=F)
				out.binomial[[i]] <- ccir_stan_summarize(x,save=F)
				out.binomial[[i]]$P = runs[r]
				out.binomial[[i]]$Rep = k


			}

		ouBin = ccir_collapse_summary(out.binomial)
		attr(ouBin,'model') <- 'binomial' 

		save(ouBin,file=file.path(project.datadirectory('bio.lobster'),'outputs','ccir','summary','test',paste0('compiledBinomialModelsLFA',Groupings[[4]]$lfa,r,k,'.rdata')))

		}
	}

	reps=5
	testSummary = data.frame()
	for(k in 1:reps){

		for(r in 1:length(runs)){

			load(file=file.path(project.datadirectory('bio.lobster'),'outputs','ccir','summary','test',paste0('compiledBinomialModelsLFA',Groupings[[4]]$lfa,r,k,'.rdata')))
			ouBin = subset(ouBin,Rep == k & P == runs[r])
			outs = list()
			kl = unique(ouBin$Grid) 
			for(i in 1:length(kl)) {
				u = subset(ouBin, Grid == kl[i])
				outs[[i]] <- ccir_timeseries_exploitation_plots(u)
			}
			o = do.call(rbind,outs)
			ooo = subset(o,select=c(Yr,ERfl,ERfm,ERfu,LFA,P,Rep))


			outs = list()
			if(F){
			SLfa = c(27)
			lan = lobster.db('process.logs')
			for(i in 1:length(SLfa)) {
				u = subset(ouBin, LFA == SLfa[i])
				g = unique(u$Grid)
				g = strsplit(g,"\\.")
				o = aggregate(WEIGHT_KG~SYEAR,data=subset(lan,GRID_NUM %in% g[[1]]),FUN=sum)
				names(o)[2] = g[[1]][1]
				o2 = aggregate(WEIGHT_KG~SYEAR,data=subset(lan,GRID_NUM %in% g[[2]]),FUN=sum)
				names(o2)[2] = g[[2]][1]
				o = merge(o,o2)
				names(o)[1] = 'Yr'
				outs[[i]] <- ccir_timeseries_exploitation_plots(u,combined.LFA=T,landings=o)
				}
				oo = do.call(rbind,outs)
				oo$P = runs[r]
				oo$Rep = k

				oi = rbind(ooo,oo)
			}
			else oi = ooo

			testSummary = rbind(testSummary, oi)
		}
	}
	save(testSummary,file=file.path(project.datadirectory('bio.lobster'),'outputs','ccir','summary','test','compiledLFA30ExploitationCCIR.rdata'))


	load(file=file.path(project.datadirectory('bio.lobster'),'outputs','ccir','summary','compiledExploitationCCIR.rdata'))

	lfa = "27"
	load(file=file.path(project.datadirectory('bio.lobster'),'outputs','ccir','summary','test',paste0('compiledLFA',lfa,'ExploitationCCIR.rdata')))

	pdf(paste0("CCIRtest",lfa,".pdf"))
	par(mfrow=c(3,3),mar=c(0,0,0,0),omi=c(1,1,1,1))
	yrs = 2009:2017
	lfa = paste("LFA",lfa,"Combined")

	for(y in 1:length(yrs)){

		plot(c(1.05,0.45),c(-0.05,1.05),type='n',xaxt='n',yaxt='n')
		if(y%in%c(1,4,7))axis(2)
		if(y%in%c(7,8,9))axis(1)
		points(rep(0,3),with(subset(oi,Yr==yrs[y]&LFA==lfa),c("ERfl","ERfm","ERfu")),pch=16,col='red')
		abline(h=subset(oi,Yr==yrs[y]&LFA==lfa)$ERfm,col='red')
		abline(h=subset(oi,Yr==yrs[y]&LFA==lfa)$ERfu,col='red',lty=2)
		abline(h=subset(oi,Yr==yrs[y]&LFA==lfa)$ERfl,col='red',lty=2)
		for(i in 1:reps){
			lines(ERfm~(P),subset(testSummary,Yr==yrs[y]&LFA==lfa&Rep==i),col=rgb(0,0,1,0.5))
			lines(ERfu~(P),subset(testSummary,Yr==yrs[y]&LFA==lfa&Rep==i),col=rgb(0,0,1,0.5),lty=2)
			lines(ERfl~(P),subset(testSummary,Yr==yrs[y]&LFA==lfa&Rep==i),col=rgb(0,0,1,0.5),lty=2)
		}


	}

mtext("Participation",1,3,outer=T)
mtext('Exploitation Estimate',2,3,outer=T)
mtext(lfa,3,3,outer=T)
dev.off()






	



