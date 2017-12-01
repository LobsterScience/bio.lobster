

require(bio.lobster)
require(bio.utilities)
require(PBSmapping)
la()
 
 overall.at.sea.indicators =T
 if(overall.at.sea.indicators) { 
		lobster.db('atSea.clean')
		
		#data frame of available at sea data
			ad =  as.data.frame(unique(cbind(atSea.clean$LFA,atSea.clean$SYEAR)))
			ad = ad[order(ad[,1],ad[,2]),]
			names(ad) = c('LFA','YEAR')
			ad = subset(ad,YEAR<2017 & LFA<34)

			atsea = atSea.clean
			logs = lobster.db('process.logs.unfiltered')
			logsa = lobster.db('process.logs')
	
			#run the summary statistics using different weighting strategies
			for(i in 1:nrow(ad)) {
				print(ad[i,])
					da = atSeaLogbookLinker(atSea = atsea, logsa = logs, year=ad[i,'YEAR'],lfa=ad[i,'LFA'],females.only=F)
					logsa1 = try(aggregate(WEIGHT_KG~GRID_NUM+SD_LOG_ID+SYEAR+WOS+BUMPUP+LFA,data=subset(logsa,LFA==ad[i,'LFA'] & SYEAR %in% ad[i,'YEAR']), FUN=sum),silent=T)
					logsa2 = try(aggregate(NUM_OF_TRAPS~GRID_NUM+SD_LOG_ID+SYEAR+WOS+BUMPUP+LFA,data=subset(logsa,LFA==ad[i,'LFA'] & SYEAR %in% ad[i,'YEAR']), FUN=sum),silent=T)	
					logsa3 = try(merge(logsa1,logsa2,all=T),silent=T)
					if ( "try-error" %in% c(class(logsa3))) logsa3=NULL		
					out[[i]] =  unlist(weightedCLF(x=da,logs=logsa3))
			}
			out = rbindFill(out)
			out = toNums(out,2:ncol(out))
			save(out,file = file.path(project.datadirectory('bio.lobster'),'outputs','atSeaIndicatorsLFA27-33.rdata'))


			load(file.path(project.datadirectory('bio.lobster'),'outputs','atSeaIndicatorsLFA27-33.rdata'))
			mls = read.csv(file=file.path(project.datadirectory('bio.lobster'),'data','inputs','MLS.Changes.all.LFA.csv'))

			atSeaIndicatorPlots(x = out,lfa=27,mls=mls)
			atSeaIndicatorPlots(x = out,lfa=28,mls=mls)
			atSeaIndicatorPlots(x = out,lfa=29,mls=mls)
			atSeaIndicatorPlots(x = out,lfa=30,mls=mls)
			atSeaIndicatorPlots(x = out,lfa='31A',mls=mls)
			atSeaIndicatorPlots(x = out,lfa='31B',mls=mls)
			atSeaIndicatorPlots(x = out,lfa=32,mls=mls)
			atSeaIndicatorPlots(x = out,lfa=33,mls=mls)
}


###for females only for reproductive potential
reproductive.potential = T
if(reproductive.potential){		
			lobster.db('atSea.clean')
			g = lobster.db('annual.landings')
			p = lobster.db('seasonal.landings')
			mls = read.csv(file=file.path(project.datadirectory('bio.lobster'),'data','inputs','MLS.Changes.all.LFA.csv'))
			ad =  as.data.frame(unique(cbind(atSea.clean$LFA,atSea.clean$SYEAR)))
			ad = ad[order(ad[,1],ad[,2]),]
			names(ad) = c('LFA','YEAR')
			names(mls)[1] <- 'YEAR'
			ad = merge(ad,mls)
			ad = subset(ad,YEAR<2017 & LFA<34)
			atsea = atSea.clean
			vec = 30:250
			logs = lobster.db('process.logs.unfiltered')
	
		out = list()

		for(i in 86:nrow(ad)) {
			print(ad[i,])
				po = ad[i,'LFA']
				yo = ad[i,'YEAR']
				mm = ad[i,'MLS_MM']
				da = atSeaLogbookLinker(atSea = atsea, logsa = logs, year=yo,lfa=po,females.only=T)
				logsa1 = try(aggregate(WEIGHT_KG~GRID_NUM+SD_LOG_ID+SYEAR+WOS+BUMPUP+LFA,data=subset(logsa,LFA==po & SYEAR %in% yo), FUN=sum),silent=T)
				logsa2 = try(aggregate(NUM_OF_TRAPS~GRID_NUM+SD_LOG_ID+SYEAR+WOS+BUMPUP+LFA,data=subset(logsa,LFA==po & SYEAR %in% yo), FUN=sum),silent=T)	
				logsa3 = try(merge(logsa1,logsa2,all=T),silent=T)
				if ( "try-error" %in% c(class(logsa3))) logsa3=NULL		
				op = weightedCLF(x=da,logs=logsa3,returnLF=T,one.output = T)
				

				if(po == 27) 	{ll = "LFA27-30"; 	lle = 'LFA27-30'; lp = g[,c('YR',names(g)[grep(po,names(g))])]}
				if(po == 28) 	{ll = 'LFA28,30'; 	lle = 'LFA27-30'; lp = g[,c('YR',names(g)[grep(po,names(g))])]}
				if(po == 29) 	{ll = 'LFA29'; 		lle = 'LFA27-30'; lp = g[,c('YR',names(g)[grep(po,names(g))])]}
				if(po == 30) 	{ll = 'LFA28,30'; 	lle = 'LFA27-30'; lp = g[,c('YR',names(g)[grep(po,names(g))])]}
				if(po == '31A') {ll = 'LFA28,30'; 	lle = 'LFA27-30'; lp = g[,c('YR',names(g)[grep(po,names(g))])]}
				if(po == '31B') {ll = 'LFA28,30'; 	lle = 'LFA27-30'; lp = g[,c('YR',names(g)[grep(po,names(g))])]}
				if(po == 32) 	{ll = 'LFA32'; 		lle = 'LFA27-30'; lp = g[,c('YR',names(g)[grep(po,names(g))])]}
				if(po == 33) 	{ll = 'LFA33'; 		lle = 'LFA33'; lp = p[,c('SYEAR',names(p)[grep(po,names(p))])]; lp = rename.df(lp,'SYEAR','YR')}
				if(exists('out',op)){
				print(i)
				v0 = hist(op$out$vec, breaks=vec,plot=F)
				
				v0$wts = lobLW(v0$mids)
				v0$bwts = v0$counts * v0$wts
				
				iw = sum(v0$bwts[v0$mids<mm])
				wi = sum(v0$bwts[v0$mids>=mm])
				le = subset(lp,YR == yo)[,2] * op$out$prop.female #only female landings
				if(po ==33) le = subset(lp,substr(YR,6,9) == yo)[,2] * op$out$prop.female #only female landings
				li = le * iw/wi

				v0$acWt = c(v0$bwts[v0$mids<mm] / iw * li, v0$bwts[v0$mids>=mm] / wi * le) #total weight of lobsters in each size bin based on proportion of total weight
				v0$N = v0$acWt / v0$wts # tons / g = #'s in '000000
				
				eggProd = pMat(lfa=ll,v0$mids) * Fecundity(lle,v0$mids) * v0$N
				out[[i]] = c(LFA = po, YEAR=yo, MLS=mm, EggProd = sum(eggProd))
				rm(v0)
				}
			}
		out = as.data.frame(do.call(rbind,out))
		out = toNums(out,2:ncol(out))
		save(out,file = file.path(project.datadirectory('bio.lobster'),'outputs','atSeaIndicatorsFemalesLFA27-33.rdata'))
#		load(file = file.path(project.datadirectory('bio.lobster'),'outputs','atSeaIndicatorsFemalesLFA27-33.rdata'))

}



###for females only for reproductive potential
landings.numbers = T
if(landings.numbers){		
			lobster.db('atSea.clean')
			g = lobster.db('annual.landings')
			p = lobster.db('seasonal.landings')
			mls = read.csv(file=file.path(project.datadirectory('bio.lobster'),'data','inputs','MLS.Changes.all.LFA.csv'))
			ad =  as.data.frame(unique(cbind(atSea.clean$LFA,atSea.clean$SYEAR)))
			ad = ad[order(ad[,1],ad[,2]),]
			names(ad) = c('LFA','YEAR')
			names(mls)[1] <- 'YEAR'
			ad = merge(ad,mls)
			ad = subset(ad,YEAR<2017 & LFA<34)
			atsea = atSea.clean
			logs = lobster.db('process.logs.unfiltered')
	
		out = list()

		for(i in 1:nrow(ad)) {
			print(ad[i,])
				po = ad[i,'LFA']
				yo = ad[i,'YEAR']
				mm = ad[i,'MLS_MM']
				da = atSeaLogbookLinker(atSea = atsea, logsa = logs, year=yo,lfa=po,females.only=T)
				logsa1 = try(aggregate(WEIGHT_KG~GRID_NUM+SD_LOG_ID+SYEAR+WOS+BUMPUP+LFA,data=subset(logsa,LFA==po & SYEAR %in% yo), FUN=sum),silent=T)
				logsa2 = try(aggregate(NUM_OF_TRAPS~GRID_NUM+SD_LOG_ID+SYEAR+WOS+BUMPUP+LFA,data=subset(logsa,LFA==po & SYEAR %in% yo), FUN=sum),silent=T)	
				logsa3 = try(merge(logsa1,logsa2,all=T),silent=T)
				if ( "try-error" %in% c(class(logsa3))) logsa3=NULL		
				op = weightedCLF(x=da,logs=logsa3,returnLF=T,one.output = T)
				

				if(po == 27) 	{ll = "LFA27-30"; 	lle = 'LFA27-30'; lp = g[,c('YR',names(g)[grep(po,names(g))])]}
				if(po == 28) 	{ll = 'LFA28,30'; 	lle = 'LFA27-30'; lp = g[,c('YR',names(g)[grep(po,names(g))])]}
				if(po == 29) 	{ll = 'LFA29'; 		lle = 'LFA27-30'; lp = g[,c('YR',names(g)[grep(po,names(g))])]}
				if(po == 30) 	{ll = 'LFA28,30'; 	lle = 'LFA27-30'; lp = g[,c('YR',names(g)[grep(po,names(g))])]}
				if(po == '31A') {ll = 'LFA28,30'; 	lle = 'LFA27-30'; lp = g[,c('YR',names(g)[grep(po,names(g))])]}
				if(po == '31B') {ll = 'LFA28,30'; 	lle = 'LFA27-30'; lp = g[,c('YR',names(g)[grep(po,names(g))])]}
				if(po == 32) 	{ll = 'LFA32'; 		lle = 'LFA27-30'; lp = g[,c('YR',names(g)[grep(po,names(g))])]}
				if(po == 33) 	{ll = 'LFA33'; 		lle = 'LFA33'; lp = p[,c('SYEAR',names(p)[grep(po,names(p))])]; lp = rename.df(lp,'SYEAR','YR')}
				if(exists('out',op)){
				print(i)

				vec = mm:250
				oo = op$out$vec[op$out$vec>mm & op$out$vec<250]
				v0 = hist(oo, breaks=vec,plot=F)
				
				v0$wts = lobLW(v0$mids)
				v0$bwts = v0$counts * v0$wts
				
				
				le = subset(lp,YR == yo)[,2] 
				if(po ==33) le = subset(lp,substr(YR,6,9) == yo)[,2] 
				v0$acWt = v0$bwts / sum(v0$bwts) * le
				v0$N = v0$acWt / v0$wts # tons / g = #'s in '000000
				out[[i]] = c(LFA = po, YEAR=yo, MLS=mm, N = sum(v0$N),Land = le)
				rm(v0)
				}
			}
			out = as.data.frame(do.call(rbind,out))
			out = toNums(out,2:ncol(out))
		
		
		save(out,file = file.path(project.datadirectory('bio.lobster'),'outputs','atSeaIndicatorsNumbersLandedLFA27-33.rdata'))
#		load(file = file.path(project.datadirectory('bio.lobster'),'outputs','atSeaIndicatorsFemalesLFA27-33.rdata'))

}




####changes in landed numbers using the length freq data -- sticking with no weighting to keep analyses the same for longest time period
			lobster.db('atSea.clean')
			ad =  as.data.frame(unique(cbind(atSea.clean$LFA,atSea.clean$SYEAR)))
			ad = ad[order(ad[,1],ad[,2]),]
			names(ad) = c('LFA','YEAR')

			ad = subset(ad,YEAR<2017 & LFA==27)
			names(mls)[1] <- 'YEAR'
			g = lobster.db('annual.landings')
			g = merge(ad,g[,c('YR','LFA27')],by.x='YEAR',by.y='YR')
			g = merge(g,mls)
						atsea = atSea.clean
				v = lobLW(30.5:249.5)
			#run the summary statistics using different weighting strategies
			out = c()
			for(i in 1:nrow(g)) {
				print(g[i,])
					da = atSeaLogbookLinker(atSea = atsea, logsa = logs, year=g[i,'YEAR'],lfa=g[i,'LFA'])
					gf =  weightedCLF(x=da,weighting.scheme='none',returnLF=T)
					l = g[i,'LFA27'] 
					m = g[i,'MLS_MM']
					gf = gf[gf>=m & gf<=250]
					ii = min(gf):max(gf)
					d = hist(gf, breaks=ii,plot=F)
					v = lobLW(d$mids)/1000
					out = c(out,sum(d$density*l/v))
				}

				plot(g$YEAR,out,type='l')
				par(new=T)
				plot(g$YEAR,g$LFA27,type='l',col='red',yaxt='n')



## Non Successful growth modelling

			load_all('~/git/bio.growth')

			x = out[[7]]$none$vec
			xi = identifyModes(x,span=5)
			vi = identifyVariancesLambdas(x,xi,span=5)
			li	<- vi[[1]]
			vi	<- vi[[2]]

			#oo <- annualMixBoot(x=x,init.mode=xi,ni=5000,mode.thresh=6, init.lam=li,init.var=vi)
			a = gammamixEM(x,k=3)
			histAllMixtures(oo[[c(1,1)]],freq=F)


