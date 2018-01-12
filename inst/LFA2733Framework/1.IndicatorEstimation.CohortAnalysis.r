


require(bio.lobster)
require(bio.utilities)
require(PBSmapping)
la()
 


###for cohort analysis and numbers landed
landings.numbers = T
if(landings.numbers){		
			atSea.clean = lobster.db('atSea.clean')
			atSea.clean$PORT = ifelse(atSea.clean$PORT==-99,0,atSea.clean$PORT)
			g = lobster.db('annual.landings')
			p = lobster.db('seasonal.landings')
			mls = read.csv(file=file.path(project.datadirectory('bio.lobster'),'data','inputs','MLS.Changes.all.LFA.csv'))
			load(file.path(project.datadirectory('bio.lobster'),'outputs','deltaTsSimBH.rdata')) #DTs
			ad =  as.data.frame(unique(cbind(atSea.clean$LFA,atSea.clean$SYEAR)))
			ad = ad[order(ad[,1],ad[,2]),]
			names(ad) = c('LFA','YEAR')
			names(mls)[1] <- 'YEAR'
			ad = merge(ad,mls)
			ad = subset(ad,YEAR<2017 & LFA<34)
			atsea = atSea.clean
			cG = lobster.db('community.to.grid.historic')		
			cH = lobster.db('community.to.grid.contemporary')		
	
		out = list()
		outN = list()
		outS = list()

		for(i in 1:nrow(ad)) {
			print(ad[i,])
				po = ad[i,'LFA']
				yo = ad[i,'YEAR']
				mm = ad[i,'MLS_MM']
				da = atSeaWeightings(atSea = atsea, comGridHist =subset(cG,LFA==ad[i,'LFA']),comGridCont = subset(cH,LFA==ad[i,'LFA'] & SYEAR==ad[i,'YEAR']), year=ad[i,'YEAR'],lfa=ad[i,'LFA'],females.only=F,at.sea.samples=T,mls=mm)
				op = weightedCLF(x=da,returnLF=T,at.sea.samples=T)
				os = op
				os$vec<-NULL
			outS[[i]] <- unlist(os)
			#Tc is fractional year of catch
				if(po == 27) 	{ll = "LFA27-30"; 	lle = 'all areas'; lp = g[,c('YR',names(g)[grep(po,names(g))])]; dt = DTs[[grep('27N',names(DTs))]]; Tc = 0.67}
				if(po == 28) 	{ll = 'LFA28,30'; 	lle = 'all areas'; lp = g[,c('YR',names(g)[grep(po,names(g))])]; dt = DTs[[grep('28',names(DTs))]]; Tc = 0.67}
				if(po == 29) 	{ll = 'LFA29'; 		lle = 'all areas'; lp = g[,c('YR',names(g)[grep(po,names(g))])]; dt = DTs[[grep('29',names(DTs))]]; Tc = 0.67}
				if(po == 30) 	{ll = 'LFA28,30'; 	lle = 'all areas'; lp = g[,c('YR',names(g)[grep(po,names(g))])]; dt = DTs[[grep('30',names(DTs))]]; Tc = 0.67}
				if(po == '31A') {ll = 'LFA29';  	lle = 'all areas'; lp = g[,c('YR',names(g)[grep(po,names(g))])]; dt = DTs[[grep('31A',names(DTs))]]; Tc = 0.67}
				if(po == '31B') {ll = 'LFA32';	 	lle = 'all areas'; lp = g[,c('YR',names(g)[grep(po,names(g))])]; dt = DTs[[grep('31B',names(DTs))]]; Tc = 0.67}
				if(po == 32) 	{ll = 'LFA32'; 		lle = 'all areas'; lp = g[,c('YR',names(g)[grep(po,names(g))])]; dt = DTs[[grep('32',names(DTs))]]; Tc = 0.67}
				if(po == 33) 	{ll = 'LFA33'; 		lle = 'all areas'; lp = p[,c('SYEAR',names(p)[grep(po,names(p))])]; lp = rename.df(lp,'SYEAR','YR'); dt = DTs[[grep('33W',names(DTs))]]; Tc = 0.3} 

				if(!is.null(op)){
				
				vec = mm:250
				oo = op$vec[op$vec>mm & op$vec<250]
				v0 = hist(oo, breaks=vec,plot=F)
				
				v0$wts = lobLW(v0$mids)
				v0$bwts = v0$counts * v0$wts
				
				
				le = subset(lp,YR == yo)[,2] 
				if(po ==33) le = subset(lp,substr(YR,6,9) == yo)[,2] 
				v0$acWt = v0$bwts / sum(v0$bwts) * le
				v0$N = v0$acWt / v0$wts # tons / g = #'s in '000000
				outN[[i]]  = data.frame(N = v0$N, Len = v0$mids,LFA = po, Year = yo,MLS=mm)
 				
 				#newly recruited fraction
 				outS[[i]] = c(outS[[i]], new.rec = sum(v0$N[v0$mids %in% seq(mm,mm+11,by=0.5)]) / sum(v0$N[v0$mids %in% seq(mm,mm+100,by=0.5)]))
 				outS[[i]] = c(outS[[i]], recWt = sum(v0$N[v0$mids %in% seq(mm,mm+11,by=0.5)]*v0$wts[v0$mids %in% seq(mm,mm+11,by=0.5)]) / sum(v0$N[v0$mids %in% seq(mm,mm+11,by=0.5)]))
 				#Production
				iw = v0$mids>mm
				pMature = sum(pMat(lfa=ll,cl=v0$mids[iw]) * v0$N[iw]) /sum(v0$N[iw])
				eggProd = sum(pMat(lfa=ll,cl=v0$mids) * Fecundity(lle,v0$mids) * v0$N * as.numeric(outS[[i]]['prop.female']))
				outS[[i]] = c(outS[[i]], PropMating = pMature,EggProduction = eggProd)			
			

				brks = seq(mm,max(as.numeric(names(dt))),by=5)
				dt = dt[which.min(abs(brks[1]-as.numeric(names(dt)))):length(dt)]
				dt =data.frame(dt=dt,brks = as.numeric(names(dt)))
				dt$dt = dt$dt / 365 #* (5 / (as.numeric(names(dt))*0.15))
				dt = dt[1:length(brks),]
				dt$brks = brks
				v0$LCA = brks[findInterval(v0$mids,vec=brks)]
				LCAN = aggregate(v0$N~v0$LCA,FUN=sum)
				
				LCA = merge(LCAN,dt,by.x='v0$LCA',by.y = 'brks')
				k = which(LCA[,2]==0)[1]
				
				LCA = LCA[1:(k-1),]
				###need to get dts

				ca = cohortAnalysis(lens = LCA[,1], N = LCA[,2], dt = LCA[,3]) #annual

				LCA$LFA = po
				LCA$Year = yo
				LCA$MLS = mm
				out[[i]] = c(LFA = po, YEAR=yo, MLS=mm, N = sum(v0$N),Land = le,expl =ca$expl, F = ca$wF,M = ca$M,tF = ca$termF)
				}
			}
			out = as.data.frame(do.call(rbind,out))
			out = toNums(out,2:ncol(out))
			save(out,file = file.path(project.datadirectory('bio.lobster'),'outputs','atSeaIndicatorsNumbersLandedLFA27-33.rdata'))
			load(file.path(project.datadirectory('bio.lobster'),'outputs','atSeaIndicatorsNumbersLandedLFA27-33.rdata'))

			outN = as.data.frame(do.call(rbind,outN))
			save(outN,file = file.path(project.datadirectory('bio.lobster'),'outputs','atSeaIndicatorsNatSizeLFA27-33.rdata'))
			load(file.path(project.datadirectory('bio.lobster'),'outputs','atSeaIndicatorsNatSizeLFA27-33.rdata'))

			outS = as.data.frame(do.call(rbind,outS))
			outS = toNums(outS,2:ncol(outS))
		   save(outS,file = file.path(project.datadirectory('bio.lobster'),'outputs','SummaryatSeaIndicatorsDataLFA27-33.rdata'))
		   load(file = file.path(project.datadirectory('bio.lobster'),'outputs','SummaryatSeaIndicatorsDataLFA27-33.rdata'))
		
	#####three year windowed expls using the data loaded above
runLCA = list()
m=0
			lfa = c(27,29,30,'31A','31B',32,33)
			for(i in 1:length(lfa)){
					o = subset(outN,LFA==lfa[i])
				
					#specific years where data was available for ~3 in a row

					if(lfa[i]==27) yrs = list(1990:1992, 1991:1993, 1992:1994, 1993:1995, 1994:1997,1995:1999,1997:2000,1999:2001, 2000:2002,2001:2003, 2002:2004,2003:2005,2004:2007,2005:2009,2007:2010,2009:2011,2010:2012,2011:2013,2012:2014,2013:2015)
					if(lfa[i]==29) yrs = list(1990:1993, 2008:2015)
					if(lfa[i]==30) yrs = list(1999:2001,2000:2002,2001:2003,2002:2004,2003:2005,2004:2007,2005:2008,2007:2009)
					if(lfa[i]=='31A') yrs = list(2001:2003, 2007:2009,2008:2010,2009:2011,2010:2012,2011:2013,2012:2014,2013:2015)
					if(lfa[i]=='31B') yrs = list(2002:2004,2008:2010,2009:2011,2010:2012,2011:2013,2012:2014,2013:2015)
					if(lfa[i]=='32') yrs = list(2001:2003,2002:2004,2009:2011,2010:2012,2011:2013,2012:2014,2013:2015)
					if(lfa[i]=='33') yrs = list(1985:1987,2001:2003,2002:2004,2009:2012,2010:2014)
					
				if(lfa[i] == 27) 	{dt = DTs[[grep('27N',names(DTs))]]; Tc = 0.67}
				if(lfa[i] == 29) 	{dt = DTs[[grep('29',names(DTs))]]; Tc = 0.67}
				if(lfa[i] == 30) 	{dt = DTs[[grep('30',names(DTs))]]; Tc = 0.67}
				if(lfa[i] == '31A') {dt = DTs[[grep('31A',names(DTs))]]; Tc = 0.67}
				if(lfa[i] == '31B') {dt = DTs[[grep('31B',names(DTs))]]; Tc = 0.67}
				if(lfa[i] == 32) 	{dt = DTs[[grep('32',names(DTs))]]; Tc = 0.67}
				if(lfa[i] == 33) 	{dt = DTs[[grep('33W',names(DTs))]]; Tc = 0.3} 

				for(j in 1:length(yrs)){
						m =m+1
						print(m)
						p = subset(o,Year %in% yrs[[j]])
						mm = max(unique(p$MLS))
						p = subset(p,Len>=mm)
				brks = seq(mm,max(as.numeric(names(dt))),by=5)
				dt1 = dt[which.min(abs(brks[1]-as.numeric(names(dt)))):length(dt)]
				dt1 =data.frame(dt=dt1,brks = as.numeric(names(dt1)))
				dt1$dt = dt1$dt / 365 #* (5 / (as.numeric(names(dt))*0.15))
				dt1 = dt1[1:length(brks),]
				dt1$brks = brks
				p$LCA = brks[findInterval(p$Len,vec=brks)]
				p = merge(p,dt1,by.x='LCA',by.y = 'brks')
				p$I = 1
				LCAN = aggregate(cbind(N,I,dt)~LCA,data = p,FUN=sum)
				w = which.max(LCAN$LCA[LCAN$I==15])
				LCAN = LCAN[1:w,]
						LCAN$dt = LCAN$dt/LCAN$I
						k = which(LCAN$N==0)[1]
						if(!is.na(k))LCAN = LCAN[1:(k-1),]
						ca = cohortAnalysis(lens = LCAN$LCA, N = LCAN$N, dt = LCAN$dt)
					runLCA[[m]] = c(LFA = lfa[i],Year.min = min(yrs[[j]]),Year.max = max(yrs[[j]]),F = ca$wF, expl = ca$expl)
					}
			}
		
			runLCA = as.data.frame(do.call(rbind,runLCA))
			runLCA = toNums(runLCA,c('Year.min','Year.max','F','expl'))
		save(runLCA,file = file.path(project.datadirectory('bio.lobster'),'outputs','atSeaIndicatorsExploitationAggregatedLFA27-33.rdata'))
		load(file.path(project.datadirectory('bio.lobster'),'outputs','atSeaIndicatorsExploitationAggregatedLFA27-33.rdata')	)	
}




#sensitivity analysis
oo = c()
tf = seq(0.1,2,by=0.1)
for(i in tf){
oo = c(oo,cohortAnalysis(lens = LCAN$LCA, N = LCAN$N, dt = LCAN$dt,termF=i)$wF)
}

plot(tf,oo,xlab='Terminal F',ylab='Estimated Exploitation',type='b',ylim=c(0.8,0.9))
savePlot(file='/backup/bio_data/bio.lobster/figures/CAsensitivityToTermF.png',type='png')

tf = seq(0.1,0.2,by=0.01)
oo = c()
for(i in tf){
oo = c(oo,cohortAnalysis(lens = LCAN$LCA, N = LCAN$N, dt = LCAN$dt,M=i)$expl)
}
plot(tf,oo,xlab='Natural Mortality',ylab='Estimated Exploitation',type='b',ylim=c(0.5,0.65))
savePlot(file='/backup/bio_data/bio.lobster/figures/CAsensitivityToM.png',type='png')





#Cohort Analysis Plots

load(file.path(project.datadirectory('bio.lobster'),'outputs','atSeaIndicatorsNumbersLandedLFA27-33.rdata'))
load(file.path(project.datadirectory('bio.lobster'),'outputs','atSeaIndicatorsExploitationAggregatedLFA27-33.rdata'))

CAplots(ann = out, yr3 = runLCA)


#sample sizes
load(file.path(project.datadirectory('bio.lobster'),'outputs','atSeaIndicatorsLFA27-33.rdata'))
ouS = out[,c('LFA','Year','TotalLobsters')]

load(file.path(project.datadirectory('bio.lobster'),'outputs','atSeaIndicatorsNumbersLandedLFA27-33.rdata'))
out$Year = out$YEAR


####FSRS commercial samples

fsrs.commercial.samples = T
if(fsrs.commercial.samples){		
			lobster.db('fsrs.commercial.samples')
			fs = subset(fsrs.comm,SYEAR>2002)
			p = lobster.db('seasonal.landings')
			load(file.path(project.datadirectory('bio.lobster'),'outputs','deltaTsSimBH.rdata')) #DTs
 			dt  = DTs[grep('33',names(DTs))]
			ad = 2004:2016
			cH = lobster.db('community.to.grid.contemporary')		
			cH = subset(cH, LFA==33)
			cG = lobster.db('community.to.grid.historic')		
			cG = subset(cG, LFA==33)
	
		outN = list()
		out = list()
		outS = list()
		for(i in 1:length(ad)) {
			print(ad[i])
				
				da = atSeaWeightings(atSea = fs, comGridCont = subset(cH, SYEAR==ad[i]),comGridHist = cG, year=ad[i],females.only=F,fsrs.commercial.samples=T,mls=10)
					op = weightedCLF(x=da,returnLF=T,fsrs.commercial.samples=T)
					os = op
				os$vec<-NULL
			outS[[i]] <- unlist(os)
		
			#Tc is fractional year of catch
				ll = 'LFA33'
				lle = 'LFA33'
				yo = ad[i]
				mm = 10.5
			
				lp = p[,c('SYEAR',names(p)[grep(ll,names(p))])]
				lp = rename.df(lp,'SYEAR','YR')
				dt = DTs[[grep('33W',names(DTs))]]
				dt = dt[which(names(dt)==85):which(names(dt)==130)]
				dt = c(dt[1],mean(dt[2:3]),mean(dt[4:5]),mean(dt[6:7]),mean(dt[8:9]))
				Tc = 0.3

				if(!is.null(op)){
				
				vec = c(10.5,11,12,13,14)
				oo = op$vec[op$vec>=mm & op$vec<15]
				v0 = table(oo)
				
				wts = lobLW(c(85,95,105,115,125))
				bwts = v0 * wts
				
				
				le = subset(lp,substr(YR,6,9) == yo)[,2] 
				acWt = bwts / sum(bwts) * le
				N = acWt / wts # tons / g = #'s in '000000
				outN[[i]]  = data.frame(N = N, Len = names(v0),Year = yo,MLS=mm)



 				outS[[i]] = c(outS[[i]], new.rec = as.numeric(v0[1] / sum(v0)))
 
				ca = cohortAnalysis(lens = as.numeric(names(N)), N = as.numeric(N), dt = c(dt[1],dt[2:length(dt)]*2)/365) #annual

				out[[i]] = c(YEAR=yo, MLS=mm, N = sum(N),Land = le,expl =ca$expl, F = ca$wF,M = ca$M,tF = ca$termF)
				}
			}

			out = as.data.frame(do.call(rbind,out))
			out = toNums(out,2:ncol(out))
			out$LFA=33
			save(out,file = file.path(project.datadirectory('bio.lobster'),'outputs','fsrsNumbersLanded33.rdata'))
			load(file.path(project.datadirectory('bio.lobster'),'outputs','fsrsNumbersLanded33.rdata'))

			outN = as.data.frame(do.call(rbind,outN))
			save(outN,file = file.path(project.datadirectory('bio.lobster'),'outputs','fsrsNatSizeLFA33.rdata'))
			load(file.path(project.datadirectory('bio.lobster'),'outputs','fsrsNatSizeLFA33.rdata'))

			outS = as.data.frame(do.call(rbind,outS))
			outS = toNums(outS,2:ncol(outS))
			save(outS,file = file.path(project.datadirectory('bio.lobster'),'outputs','SummaryfsrsCommercialSamplesLanded33.rdata'))
#3-year running
			o = outN
			runLCA = list()
			#specific years where data was available for ~3 in a row

				 yrs = list(2004:2006,2005:2007,2006:2008,2007:2009,2008:2010,2009:2011,2010:2012,2011:2013,2012:2014,2013:2015,2014:2016)
				dt = DTs[[grep('33W',names(DTs))]]
				dt = dt[which(names(dt)==85):which(names(dt)==130)]
				dt = c(dt[1],mean(dt[2:3])*2,mean(dt[4:5])*2,mean(dt[6:7])*2,mean(dt[8:9])*2) / 365
			
				for(j in 1:length(yrs)){
						p = subset(o,Year %in% yrs[[j]])
						
				LCAN=aggregate(N.Freq~Len,data=p,FUN=sum)
				ca = cohortAnalysis(lens = as.numeric(LCAN$Len), N = LCAN$N.Freq, dt = as.numeric(dt))
					runLCA[[j]] = c(Year.min = min(yrs[[j]]),Year.max = max(yrs[[j]]),F = ca$wF, expl = ca$expl)
					}
		
			runLCA = as.data.frame(do.call(rbind,runLCA))
			runLCA = toNums(runLCA,c('Year.min','Year.max','F','expl'))
			runLCA$LFA=33
		save(runLCA,file = file.path(project.datadirectory('bio.lobster'),'outputs','fsrsExploitationAggregated33.rdata'))
	load(file = file.path(project.datadirectory('bio.lobster'),'outputs','fsrsExploitationAggregated33.rdata'))

##fsrs ca plots
					load(file.path(project.datadirectory('bio.lobster'),'outputs','fsrsNatSizeLFA33.rdata'))
					load(file = file.path(project.datadirectory('bio.lobster'),'outputs','fsrsExploitationAggregated33.rdata'))
CAplots(ann=out,yr3=runLCA,fsrs=T)
}



###port samples
redo.port.samples=T
if(redo.port.samples){
			lobster.db('port.sampling')
			port = subset(port,!is.na(PORT_CODE))
			port[,c('N_MALES','N_FEM','NBF')] = na.zero(port[,c('N_MALES','N_FEM','NBF')])
			g = lobster.db('annual.landings')
			p = lobster.db('seasonal.landings')
			mls = read.csv(file=file.path(project.datadirectory('bio.lobster'),'data','inputs','MLS.Changes.all.LFA.csv'))
			load(file.path(project.datadirectory('bio.lobster'),'outputs','deltaTsSimBH.rdata')) #DTs
			ad =  as.data.frame(unique(cbind(port$LFA,port$SYEAR)))
			ad = ad[order(ad[,1],ad[,2]),]
			names(ad) = c('LFA','YEAR')
			names(mls)[1] <- 'YEAR'
			ad = merge(ad,mls)
			ad = subset(ad,YEAR<2017 & LFA<34)
			cG = lobster.db('community.to.grid.historic')		
			cH = lobster.db('community.to.grid.contemporary')		
	
		out = list()
		outN = list()
		outS = list()

		for(i in 1:nrow(ad)) {
			print(ad[i,])
				po = ad[i,'LFA']
				yo = ad[i,'YEAR']
				mm = ad[i,'MLS_MM']
				da = atSeaWeightings(atSea = port, comGridHist =subset(cG,LFA==ad[i,'LFA']),comGridCont = subset(cH,LFA==ad[i,'LFA'] & SYEAR==ad[i,'YEAR']), year=ad[i,'YEAR'],lfa=ad[i,'LFA'],females.only=F,fsrs.commercial.samples=F,port.samples=T)
				op = NULL
				
				if(!is.null(da)) {
					op = weightedCLF(x=da,returnLF=T,port.samples=T)
					os = op
					os$vec<-NULL
					outS[[i]] <- unlist(os)
	
			#Tc is fractional year of catch
				if(po == 27) 	{ll = "LFA27-30"; 	lle = 'all areas'; lp = g[,c('YR',names(g)[grep(po,names(g))])]; dt = DTs[[grep('27N',names(DTs))]]; Tc = 0.67}
				if(po == 28) 	{ll = 'LFA28,30'; 	lle = 'all areas'; lp = g[,c('YR',names(g)[grep(po,names(g))])]; dt = DTs[[grep('28',names(DTs))]]; Tc = 0.67}
				if(po == 29) 	{ll = 'LFA29'; 		lle = 'all areas'; lp = g[,c('YR',names(g)[grep(po,names(g))])]; dt = DTs[[grep('29',names(DTs))]]; Tc = 0.67}
				if(po == 30) 	{ll = 'LFA28,30'; 	lle = 'all areas'; lp = g[,c('YR',names(g)[grep(po,names(g))])]; dt = DTs[[grep('30',names(DTs))]]; Tc = 0.67}
				if(po == '31A') {ll = 'LFA29';  	lle = 'all areas'; lp = g[,c('YR',names(g)[grep(po,names(g))])]; dt = DTs[[grep('31A',names(DTs))]]; Tc = 0.67}
				if(po == '31B') {ll = 'LFA32';	 	lle = 'all areas'; lp = g[,c('YR',names(g)[grep(po,names(g))])]; dt = DTs[[grep('31B',names(DTs))]]; Tc = 0.67}
				if(po == 32) 	{ll = 'LFA32'; 		lle = 'all areas'; lp = g[,c('YR',names(g)[grep(po,names(g))])]; dt = DTs[[grep('32',names(DTs))]]; Tc = 0.67}
				if(po == 33) 	{ll = 'LFA33'; 		lle = 'all areas'; lp = p[,c('SYEAR',names(p)[grep(po,names(p))])]; lp = rename.df(lp,'SYEAR','YR'); dt = DTs[[grep('33W',names(DTs))]]; Tc = 0.3} 

				if(!is.null(op)){
				
				vec = mm:250
				oo = op$vec[op$vec>mm & op$vec<250]
				v0 = hist(oo, breaks=vec,plot=F)
				
				v0$wts = lobLW(v0$mids)
				v0$bwts = v0$counts * v0$wts
				
				
				le = subset(lp,YR == yo)[,2] 
				if(po ==33) le = subset(lp,substr(YR,6,9) == yo)[,2] 
				v0$acWt = v0$bwts / sum(v0$bwts) * le
				v0$N = v0$acWt / v0$wts # tons / g = #'s in '000000
				outN[[i]]  = data.frame(N = v0$N, Len = v0$mids,LFA = po, Year = yo,MLS=mm)
 
		 		outS[[i]] = c(outS[[i]], new.rec = sum(v0$N[v0$mids %in% seq(mm,mm+11,by=0.5)]) / sum(v0$N[v0$mids %in% seq(mm,mm+110,by=0.5)]))
				outS[[i]] = c(outS[[i]], recWt = sum(v0$N[v0$mids %in% seq(mm,mm+11,by=0.5)]*v0$wts[v0$mids %in% seq(mm,mm+11,by=0.5)]) / sum(v0$N[v0$mids %in% seq(mm,mm+11,by=0.5)]))
 				outS[[i]] = c(outS[[i]], landWt = sum(v0$N*v0$wts) / sum(v0$N))
 			
				iw = v0$mids>mm
				pMature = sum(pMat(lfa=ll,cl=v0$mids[iw]) * v0$N[iw]) /sum(v0$N[iw])
				eggProd = sum(pMat(lfa=ll,cl=v0$mids) * Fecundity(lle,v0$mids) * v0$N * as.numeric(outS[[i]]['prop.female']))
				outS[[i]] = c(outS[[i]], PropMating = pMature,EggProduction = eggProd)			
		

				brks = seq(mm,max(as.numeric(names(dt))),by=5)
				dt = dt[which.min(abs(brks[1]-as.numeric(names(dt)))):length(dt)]
				dt =data.frame(dt=dt,brks = as.numeric(names(dt)))
				dt$dt = dt$dt / 365 #* (5 / (as.numeric(names(dt))*0.15))
				dt = dt[1:length(brks),]
				dt$brks = brks
				v0$LCA = brks[findInterval(v0$mids,vec=brks)]
				LCAN = aggregate(v0$N~v0$LCA,FUN=sum)
				
				LCA = merge(LCAN,dt,by.x='v0$LCA',by.y = 'brks')
				k = which(LCA[,2]==0)[1]
				
				LCA = LCA[1:(k-1),]
				###need to get dts

				ca = cohortAnalysis(lens = LCA[,1], N = LCA[,2], dt = LCA[,3]) #annual

				LCA$LFA = po
				LCA$Year = yo
				LCA$MLS = mm
				out[[i]] = c(LFA = po, YEAR=yo, MLS=mm, N = sum(v0$N),Land = le,expl =ca$expl, F = ca$wF,M = ca$M,tF = ca$termF)
				}
			}
		}
			out = as.data.frame(do.call(rbind,out))
			out = toNums(out,2:ncol(out))
			save(out,file = file.path(project.datadirectory('bio.lobster'),'outputs','portNumbersLanded27-33.rdata'))
			load(file.path(project.datadirectory('bio.lobster'),'outputs','portNumbersLanded27-33.rdata'))

			outN = as.data.frame(do.call(rbind,outN))
			save(outN,file = file.path(project.datadirectory('bio.lobster'),'outputs','portNatSizeLFA27-33.rdata'))
			load(file.path(project.datadirectory('bio.lobster'),'outputs','portNatSizeLFA27-33.rdata'))

			outS = as.data.frame(do.call(rbind,outS))
			outS = toNums(outS,2:ncol(outS))
			save(outS,file = file.path(project.datadirectory('bio.lobster'),'outputs','portSummaryLFA27-33.rdata'))
			load(file.path(project.datadirectory('bio.lobster'),'outputs','portSummaryLFA27-33.rdata'))
		
	
	#####three year windowed expls using the data loaded above
runLCA = list()
m=0
			lfa = c(27,29,'31A','31B',32,33)
			for(i in 1:length(lfa)){
					o = subset(outN,LFA==lfa[i])
				
					#specific years where data was available for ~3 in a row

					if(lfa[i]==27) yrs = list(1989:1991,1990:1994, 1994:1996, 1995:1997, 1996:1998, 1997:1999,1998:2001,1999:2002,2001:2003)
					if(lfa[i]==29) yrs = list(1989:1993, 1990:1994, 1993:1995, 1994:1997, 1995:1998, 1997:1999, 1998:2000, 1999:2001, 2000:2002, 2001:2003)
					if(lfa[i]=='31A') yrs = list(1985:1987,1986:1988,1987:1989, 1988:1990, 1989:1991, 1990:1992, 1991:1993, 1992:1994, 1998:2000,1999:2001,2000:2002,2001:2003,2002:2004, 2003:2005, 2004:2006)
					if(lfa[i]=='31B') yrs = list(1986:1988,1987:1989, 1988:1990, 1989:1992, 1990:1993, 1992:1994, 1993:1996, 1994:1998,1996:1999,1998:2000,1999:2001,2000:2002,2001:2003, 2002:2004, 2003:2005)
					if(lfa[i]=='32') yrs = list(1985:1987,1986:1988,1987:1989, 1988:1991, 1989:1992, 1991:1993, 1992:1994, 1993:1996, 1994:1998,1996:1999,1998:2000,1999:2001,2000:2002,2001:2003)
					if(lfa[i]=='33') yrs = list(1992:1994, 1993:1995, 1994:1996, 1995:1997, 1996:1998, 1997:1999, 2007:2010, 2009:2011, 2010:2013)
					
				if(lfa[i] == 27) 	{dt = DTs[[grep('27N',names(DTs))]]; Tc = 0.67}
				if(lfa[i] == 29) 	{dt = DTs[[grep('29',names(DTs))]]; Tc = 0.67}
				if(lfa[i] == '31A') {dt = DTs[[grep('31A',names(DTs))]]; Tc = 0.67}
				if(lfa[i] == '31B') {dt = DTs[[grep('31B',names(DTs))]]; Tc = 0.67}
				if(lfa[i] == 32) 	{dt = DTs[[grep('32',names(DTs))]]; Tc = 0.67}
				if(lfa[i] == 33) 	{dt = DTs[[grep('33W',names(DTs))]]; Tc = 0.3} 

				for(j in 1:length(yrs)){
						m =m+1
						print(m)
						p = subset(o,Year %in% yrs[[j]])
						mm = max(unique(p$MLS))
						p = subset(p,Len>=mm)
				brks = seq(mm,max(as.numeric(names(dt))),by=5)
				dt1 = dt[which.min(abs(brks[1]-as.numeric(names(dt)))):length(dt)]
				dt1 =data.frame(dt=dt1,brks = as.numeric(names(dt1)))
				dt1$dt = dt1$dt / 365 #* (5 / (as.numeric(names(dt))*0.15))
				dt1 = dt1[1:length(brks),]
				dt1$brks = brks
				p$LCA = brks[findInterval(p$Len,vec=brks)]
				p = merge(p,dt1,by.x='LCA',by.y = 'brks')
				p$I = 1
				LCAN = aggregate(cbind(N,I,dt)~LCA,data = p,FUN=sum)
				lop = min(LCAN$I,na.rm=T)
				w = which.max(LCAN$LCA[LCAN$I==lop])
				LCAN = LCAN[1:w,]
						LCAN$dt = LCAN$dt/LCAN$I
						k = which(LCAN$N==0)[1]
						if(!is.na(k))LCAN = LCAN[1:(k-1),]
						ca = cohortAnalysis(lens = LCAN$LCA, N = LCAN$N, dt = LCAN$dt)
					runLCA[[m]] = c(LFA = lfa[i],Year.min = min(yrs[[j]]),Year.max = max(yrs[[j]]),F = ca$wF, expl = ca$expl)
					}
			}
		
			runLCA = as.data.frame(do.call(rbind,runLCA))
			runLCA = toNums(runLCA,c('Year.min','Year.max','F','expl'))
		save(runLCA,file = file.path(project.datadirectory('bio.lobster'),'outputs','PORTatSeaIndicatorsExploitationAggregatedLFA27-33.rdata'))
		load(file = file.path(project.datadirectory('bio.lobster'),'outputs','PORTatSeaIndicatorsExploitationAggregatedLFA27-33.rdata'))
CAplots(ann=out,yr3=runLCA,port=T)


}




#############

fsrs.recruit.samples = T
if(fsrs.recruit.samples){		
			lobster.db('fsrs')
			fsrs$LFA = ifelse(fsrs$LFA=='31.1','31A',fsrs$LFA)
			fsrs$LFA = ifelse(fsrs$LFA=='31.2','31B',fsrs$LFA)
			#these shouldbe in lobster.db
			fsrs = addSYEAR(fsrs,date.field='HAUL_DATE')
            season.dates = lobster.db('season.dates')

			      fsrs$WOS = NA
                        lfa = unique(fsrs$LFA) 
                            for(i in 1:length(lfa)) {
                                  h  = season.dates[season.dates$LFA==lfa[i],]  
                               for(j in unique(fsrs$SYEAR[fsrs$LFA==lfa[i]])){
                                   fsrs$WOS[fsrs$LFA==lfa[i] & fsrs$SYEAR==j] = floor(as.numeric(fsrs$SDATE[fsrs$LFA==lfa[i] & fsrs$SYEAR==j]-min(h$START_DATE[h$SYEAR==j]))/7)+1
                                }
                          }
                  
			fsrs = subset(fsrs,SYEAR>2003)
			p = lobster.db('seasonal.landings')
			g = lobster.db('annual.landings')
			p = lobster.db('seasonal.landings')
			mls = mls2fsrs()
			mls = rename.df(mls,'Year','YEAR')
			load(file.path(project.datadirectory('bio.lobster'),'outputs','deltaTsSimBH.rdata')) #DTs
			ad =  as.data.frame(unique(cbind(fsrs$LFA,fsrs$SYEAR)))
			ad = ad[order(ad[,1],ad[,2]),]
			names(ad) = c('LFA','YEAR')
			ad = merge(ad,mls)
			ad = subset(ad,YEAR<2017 & LFA<34)
			cG = lobster.db('community.to.grid.historic')		
			cH = lobster.db('community.to.grid.contemporary')		
	
		outS = list()
		for(i in 1:nrow(ad)) {
			print(ad[i,])
	
				da = atSeaWeightings(atSea = fsrs, comGridHist =subset(cG,LFA==ad[i,'LFA']),comGridCont = subset(cH,LFA==ad[i,'LFA'] & SYEAR==ad[i,'YEAR']),mls=ad[i,'fsrs'], year=ad[i,'YEAR'],lfa=ad[i,'LFA'],females.only=F,fsrs.recruit.samples=T)
				op = weightedCLF(x=da,returnLF=T,fsrs.recruit.samples=T)
							os = op
				os$vec<-NULL
			outS[[i]] <- unlist(os)
		####No cohort analysis
}
			outS = as.data.frame(do.call(rbind,outS))
			outS = toNums(outS,2:ncol(outS))
			save(outS,file = file.path(project.datadirectory('bio.lobster'),'outputs','SummaryfsrsrecruitmentSamplesLanded27-33.rdata'))
#3-year running
}





#Overall plots
		load(file = file.path(project.datadirectory('bio.lobster'),'outputs','PORTatSeaIndicatorsExploitationAggregatedLFA27-33.rdata'))
po = runLCA
		load(file = file.path(project.datadirectory('bio.lobster'),'outputs','fsrsExploitationAggregated33.rdata'))
fs = runLCA
		load(file.path(project.datadirectory('bio.lobster'),'outputs','atSeaIndicatorsExploitationAggregatedLFA27-33.rdata'))
aS = runLCA


CAplotsMultDataSets(atSea = aS, fsrs = fs, port = po)


