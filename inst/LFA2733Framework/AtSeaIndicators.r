

require(bio.lobster)
require(bio.utilities)
require(PBSmapping)
la()
 
 overall.at.sea.indicators =T
 if(overall.at.sea.indicators) { 
		atSea.clean = lobster.db('atSea.clean')
		atSea.clean$PORT = ifelse(atSea.clean$PORT==-99,0,atSea.clean$PORT)
		cG = lobster.db('community.to.grid.historic')		
		cH = lobster.db('community.to.grid.contemporary')		
		dats = lobster.db('landings.by.port')
		#data frame of available at sea data
			ad =  as.data.frame(unique(cbind(atSea.clean$LFA,atSea.clean$SYEAR)))
			ad = ad[order(ad[,1],ad[,2]),]
			names(ad) = c('LFA','YEAR')
			ad = subset(ad,YEAR<2017 & LFA<34)

			atsea = atSea.clean
			
			#run the summary statistics using different weighting strategies
			out = list()
			for(i in  1:nrow(ad)) {
				print(ad[i,])
					 da = atSeaWeightings(atSea = atsea, comGridHist =subset(cG,LFA==ad[i,'LFA']),comGridCont = subset(cH,LFA==ad[i,'LFA'] & SYEAR==ad[i,'YEAR']), year=ad[i,'YEAR'],lfa=ad[i,'LFA'],females.only=F)
					out[[i]] =  unlist(weightedCLF(x=da))
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
			atSea.clean = lobster.db('atSea.clean')
			atSea.clean$PORT = ifelse(atSea.clean$PORT==-99,0,atSea.clean$PORT)
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
			cG = lobster.db('community.to.grid.historic')		
			cH = lobster.db('community.to.grid.contemporary')		
	
			vec = 30:250
	out = list()

		for(i in 1:nrow(ad)) {
			print(ad[i,])
				po = ad[i,'LFA']
				yo = ad[i,'YEAR']
				mm = ad[i,'MLS_MM']
				da = atSeaWeightings(atSea = atsea, comGridHist =subset(cG,LFA==ad[i,'LFA']),comGridCont = subset(cH,LFA==ad[i,'LFA'] & SYEAR==ad[i,'YEAR']), year=ad[i,'YEAR'],lfa=ad[i,'LFA'],females.only=T)
				op = weightedCLF(x=da,returnLF=T)
				

				if(po == 27) 	{ll = "LFA27-30"; 	lle = 'LFA27-30'; lp = g[,c('YR',names(g)[grep(po,names(g))])]}
				if(po == 28) 	{ll = 'LFA28,30'; 	lle = 'LFA27-30'; lp = g[,c('YR',names(g)[grep(po,names(g))])]}
				if(po == 29) 	{ll = 'LFA29'; 		lle = 'LFA27-30'; lp = g[,c('YR',names(g)[grep(po,names(g))])]}
				if(po == 30) 	{ll = 'LFA28,30'; 	lle = 'LFA27-30'; lp = g[,c('YR',names(g)[grep(po,names(g))])]}
				if(po == '31A') {ll = 'LFA28,30'; 	lle = 'LFA27-30'; lp = g[,c('YR',names(g)[grep(po,names(g))])]}
				if(po == '31B') {ll = 'LFA28,30'; 	lle = 'LFA27-30'; lp = g[,c('YR',names(g)[grep(po,names(g))])]}
				if(po == 32) 	{ll = 'LFA32'; 		lle = 'LFA27-30'; lp = g[,c('YR',names(g)[grep(po,names(g))])]}
				if(po == 33) 	{ll = 'LFA33'; 		lle = 'LFA33'; lp = p[,c('SYEAR',names(p)[grep(po,names(p))])]; lp = rename.df(lp,'SYEAR','YR')}
				if(!is.null(op)){
				v0 = hist(op$vec, breaks=vec,plot=F)
				
				v0$wts = lobLW(v0$mids)
				v0$bwts = v0$counts * v0$wts
				
				iw = sum(v0$bwts[v0$mids<mm])
				wi = sum(v0$bwts[v0$mids>=mm])
				le = subset(lp,YR == yo)[,2] * op$prop.female #only female landings
				if(po ==33) le = subset(lp,substr(YR,6,9) == yo)[,2] * op$prop.female #only female landings
				li = le * iw/wi

				v0$acWt = c(v0$bwts[v0$mids<mm] / iw * li, v0$bwts[v0$mids>=mm] / wi * le) #total weight of lobsters in each size bin based on proportion of total weight
				v0$N = v0$acWt / v0$wts # tons / g = #'s in '000000
				
				eggProd = pMat(lfa=ll,v0$mids) * Fecundity(lle,v0$mids) * v0$N
				out[[i]] = c(LFA = po, Year=yo, MLS=mm, EggProd = sum(eggProd))
				rm(v0)
				}
			}
		}
		out = as.data.frame(do.call(rbind,out))
		out = toNums(out,2:ncol(out))
		save(out,file = file.path(project.datadirectory('bio.lobster'),'outputs','atSeaIndicatorsFemalesLFA27-33.rdata'))
#		load(file = file.path(project.datadirectory('bio.lobster'),'outputs','atSeaIndicatorsFemalesLFA27-33.rdata'))
names(mls)[1] <- 'Year'
			
			atSeaIndicatorPlots(x = out,lfa=27,mls=mls,indicators='ReproductivePotential')
			atSeaIndicatorPlots(x = out,lfa=28,mls=mls,indicators='ReproductivePotential')
			atSeaIndicatorPlots(x = out,lfa=29,mls=mls,indicators='ReproductivePotential')
			atSeaIndicatorPlots(x = out,lfa=30,mls=mls,indicators='ReproductivePotential')
			atSeaIndicatorPlots(x = out,lfa='31A',mls=mls,indicators='ReproductivePotential')
			atSeaIndicatorPlots(x = out,lfa='31B',mls=mls,indicators='ReproductivePotential')
			atSeaIndicatorPlots(x = out,lfa=32,mls=mls,indicators='ReproductivePotential')
			atSeaIndicatorPlots(x = out,lfa=33,mls=mls,indicators='ReproductivePotential')
}


