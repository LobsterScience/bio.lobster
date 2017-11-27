

require(bio.lobster)
require(bio.utilities)
require(PBSmapping)
la()
   
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
		da = atSeaLogbookLinker(atSea = atsea, logsa = logs, year=ad[i,'YEAR'],lfa=ad[i,'LFA'])
		logsa1 = try(aggregate(WEIGHT_KG~GRID_NUM+SD_LOG_ID+SYEAR+WOS+BUMPUP+LFA,data=subset(logsa,LFA==ad[i,'LFA'] & SYEAR %in% ad[i,'YEAR']), FUN=sum),silent=T)
		logsa2 = try(aggregate(NUM_OF_TRAPS~GRID_NUM+SD_LOG_ID+SYEAR+WOS+BUMPUP+LFA,data=subset(logsa,LFA==ad[i,'LFA'] & SYEAR %in% ad[i,'YEAR']), FUN=sum),silent=T)	
		logsa3 = try(merge(logsa1,logsa2,all=T),silent=T)
		if ( "try-error" %in% c(class(logsa3))) logsa3=NULL		
		out[[i]] =  unlist(weightedCLF(x=da,logs=logsa3))
}
out = rbindFill(out)
out = toNums(out,2:ncol(out))
save(out,file = file.path(project.datadirectory('bio.lobster'),'outputs','atSeaIndicatorsLFA27-33.rdata'))

mls = read.csv(file=file.path(project.datadirectory('bio.lobster'),'data','inputs','MLS.Changes.all.LFA.csv'))

atSeaIndicatorPlots(x = out,lfa=27,mls=mls)
atSeaIndicatorPlots(x = out,lfa=28,mls=mls)
atSeaIndicatorPlots(x = out,lfa=29,mls=mls)
atSeaIndicatorPlots(x = out,lfa=30,mls=mls)
atSeaIndicatorPlots(x = out,lfa='31A',mls=mls)
atSeaIndicatorPlots(x = out,lfa='31B',mls=mls)
atSeaIndicatorPlots(x = out,lfa=32,mls=mls)
atSeaIndicatorPlots(x = out,lfa=33,mls=mls)



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
		gf =  weightedCLF(x=da,weighting.scheme='none',returnHistStats=T)
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



###
load_all('~/git/bio.growth')

x = out[[7]]$none$vec
xi = identifyModes(x,span=5)
vi = identifyVariancesLambdas(x,xi,span=5)
li	<- vi[[1]]
vi	<- vi[[2]]

#oo <- annualMixBoot(x=x,init.mode=xi,ni=5000,mode.thresh=6, init.lam=li,init.var=vi)
a = gammamixEM(x,k=3)
histAllMixtures(oo[[c(1,1)]],freq=F)


