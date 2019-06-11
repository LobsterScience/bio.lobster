
 
p = bio.lobster::load.environment()
la()

assessment.year = p$current.assessment.year - 1 ########### check the year ############### !!!!!!!!!!!
lobster.db('atSea.redo') # on windows
lobster.db('atSea.clean.redo')
lobster.db('community.to.grid.contemporary.redo')


###for cohort analysis and numbers landed

			atSea.clean = lobster.db('atSea.clean')
			atSea.clean$PORT = ifelse(atSea.clean$PORT==-99,0,atSea.clean$PORT)
			g = lobster.db('annual.landings')
			p = lobster.db('seasonal.landings')
			mls = read.csv(file=file.path(project.datadirectory('bio.lobster'),'data','inputs','MLS.Changes.all.LFA.csv'))
				
				# fill in table where data is missing for recent years
				mls.lst=list()
				lfas = unique(mls$LFA)
				for(i in 1:length(lfas)){
					mlst = subset(mls,LFA==lfas[i])
					maxyr=max(mlst$Year)
					mls.lst[[i]] = rbind(mlst, data.frame(LFA=lfas[i],Year=(maxyr+1):assessment.year,mlst[mlst$Year==maxyr,3:ncol(mlst)]))
				}
				mls = do.call("rbind",mls.lst)
							
				write.csv(mls,file.path(project.datadirectory('bio.lobster'),'data','inputs',paste0('MLS.Changes.all.LFA',assessment.year,'.csv')),row.names=F)
			mls = read.csv(file=file.path(project.datadirectory('bio.lobster'),'data','inputs',paste0('MLS.Changes.all.LFA',assessment.year,'.csv')))
				write.csv(mls,file.path(project.datadirectory('bio.lobster'),'data','inputs',paste0('MLS.Changes.all.LFA',assessment.year,'.csv')),row.names=F)

			load(file.path(project.datadirectory('bio.lobster'),'outputs','deltaTsSimBH.rdata')) #DTs
			ad =  as.data.frame(unique(cbind(atSea.clean$LFA,atSea.clean$SYEAR)))
			ad = ad[order(ad[,1],ad[,2]),]
			names(ad) = c('LFA','YEAR')
			names(mls)[1] <- 'YEAR'
			ad = merge(ad,mls)
			ad = subset(ad,YEAR<assessment.year+1 & LFA<34) 
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
			outS[[i]] <- os
		# brad ran to here to get data for a plot
		#outS[[i]] <- op$vec
		#} 
		#save(outS,file=file.path(project.datadirectory("bio.lobster"),"outputs","atSeaCLF.rdata"))

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

			outS = toNums(outS,which(!names(outS)%in%c('LFA','quants')))
			outS$LFA = as.character(outS$LFA)
			outS$quants = as.character(outS$quants)

		   save(outS,file = file.path(project.datadirectory('bio.lobster'),'outputs','SummaryatSeaIndicatorsDataLFA27-33.rdata'))
		   load(file = file.path(project.datadirectory('bio.lobster'),'outputs','SummaryatSeaIndicatorsDataLFA27-33.rdata'))



#### by weeks

			atSea.clean = lobster.db('atSea.clean')
			atSea.clean$PORT = ifelse(atSea.clean$PORT==-99,0,atSea.clean$PORT)
			g = lobster.db('annual.landings')
			p = lobster.db('seasonal.landings')
			mls = read.csv(file=file.path(project.datadirectory('bio.lobster'),'data','inputs',paste0('MLS.Changes.all.LFA',assessment.year,'.csv')))


			load(file.path(project.datadirectory('bio.lobster'),'outputs','deltaTsSimBH.rdata')) #DTs
			ad =  as.data.frame(unique(cbind(atSea.clean$LFA,atSea.clean$SYEAR)))
			ad = ad[order(ad[,1],ad[,2]),]
			names(ad) = c('LFA','YEAR')
			names(mls)[1] <- 'YEAR'
			ad = merge(ad,mls)
			ad = subset(ad,YEAR<assessment.year+1 & LFA<34) 
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
				io= list(WOS=c(3,4,5,6))
				if(po ==  33) io= list(WOS=c(4:25))
			
				op = weightedCLF(x=da,returnLF=T,at.sea.samples=T,grouping=io)
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
			save(out,file = file.path(project.datadirectory('bio.lobster'),'outputs','SubsetWksatSeaIndicatorsNumbersLandedLFA27-33.rdata'))
			load(file.path(project.datadirectory('bio.lobster'),'outputs','SubsetWksatSeaIndicatorsNumbersLandedLFA27-33.rdata'))

			outN = as.data.frame(do.call(rbind,outN))
			save(outN,file = file.path(project.datadirectory('bio.lobster'),'outputs','SubsetWksatSeaIndicatorsNatSizeLFA27-33.rdata'))
			load(file.path(project.datadirectory('bio.lobster'),'outputs','SubsetWksatSeaIndicatorsNatSizeLFA27-33.rdata'))

			outS = as.data.frame(do.call(rbind,outS))
			outS = toNums(outS,2:ncol(outS))
		   save(outS,file = file.path(project.datadirectory('bio.lobster'),'outputs','SubsetWksSummaryatSeaIndicatorsDataLFA27-33.rdata'))
		   load(file = file.path(project.datadirectory('bio.lobster'),'outputs','SubsetWksSummaryatSeaIndicatorsDataLFA27-33.rdata'))
		
			load(file.path(project.datadirectory('bio.lobster'),'outputs','atSeaIndicatorsNumbersLandedLFA27-33.rdata'))
	aS = out

			load(file.path(project.datadirectory('bio.lobster'),'outputs','fsrsNumbersLanded33.rdata'))
	fS = out

		load(file.path(project.datadirectory('bio.lobster'),'outputs','portNumbersLanded27-33.rdata'))
	pS = out


###new recruit biomass


		   load(file = file.path(project.datadirectory('bio.lobster'),'outputs','SummaryatSeaIndicatorsDataLFA27-33.rdata')) #at Sea
		   aSS = outS 
		   aSS = rename.df(aSS,'Year','YEAR')
		   aS = merge(aS[,c('YEAR','LFA','N','Land','expl')],aSS[,c('YEAR','LFA','new.rec','recWt')])
		   aS$ID = 'aS'

		   	load(file.path(project.datadirectory('bio.lobster'),'outputs','portSummaryLFA27-33.rdata'))
			pSS = outS
			pSS = rename.df(pSS,'Year','YEAR')
		    pS = merge(pS[,c('YEAR','LFA','N','Land','expl')],pSS[,c('YEAR','LFA','new.rec','recWt')])
			pS$ID = 'pS'


			aA = rbind(aS,pS)
			aA$nNewRec = aA$N * aA$new.rec
			a2 = aggregate(cbind(nNewRec,N,recWt)~YEAR + LFA,data=aA,FUN = mean)

			  load(file=file.path(project.datadirectory('bio.lobster'),'outputs','ccir','summary','compiledExploitationCCIR33.rdata'))

			oi=oo
			oi$LFA <- "33"

			  load(file=file.path(project.datadirectory('bio.lobster'),'outputs','ccir','summary','compiledExploitationCCIR2732.rdata'))

			oi = rbind(oo,oi)
			oi = rename.df(oi,'Yr','YEAR')
			aa = merge(a2,oi,by=c('YEAR','LFA'))

			aa$RecB = aa$nNewRec / aa$ERfm * aa$recWt
			aa$RecBu = aa$nNewRec / aa$ERfl * aa$recWt
			aa$RecBl = aa$nNewRec / aa$ERfu * aa$recWt

			ii = which(aa$ERfl<0)

			aa = aa[-ii,]

			save(aa,file=file.path(project.datadirectory('bio.lobster'),'outputs','EstimatedRecruitBiomassLFA27-33.rdata')) 

			

############## FSRS recruitment


			lobster.db('fsrs')
			fsrs$LFA = ifelse(fsrs$LFA=='31.1','31A',fsrs$LFA)
			fsrs$LFA = ifelse(fsrs$LFA=='31.2','31B',fsrs$LFA)
			#these shouldbe in lobster.db
			fsrs = addSYEAR(fsrs,date.field='HAUL_DATE')
            season.dates = backFillSeasonDates(lobster.db('season.dates'),eyr=assessment.year)

			      fsrs$WOS = NA
                        lfa = unique(fsrs$LFA) 
                            for(i in 1:length(lfa)) {
                                  h  = season.dates[season.dates$LFA==lfa[i],]  
                               for(j in unique(fsrs$SYEAR[fsrs$LFA==lfa[i]])){
                                   fsrs$WOS[fsrs$LFA==lfa[i] & fsrs$SYEAR==j] = floor(as.numeric(fsrs$HAUL_DATE[fsrs$LFA==lfa[i] & fsrs$SYEAR==j]-min(h$START_DATE[h$SYEAR==j]))/7)+1
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
			ad = subset(ad,YEAR<2019 & LFA<34)
			cG = lobster.db('community.to.grid.historic')		
			cH = lobster.db('community.to.grid.contemporary')		
	
		outS = list()
		for(i in 1:nrow(ad)) {
			print(ad[i,])
	
				da = atSeaWeightings(atSea = fsrs, comGridHist =subset(cG,LFA==ad[i,'LFA']),comGridCont = subset(cH,LFA==ad[i,'LFA'] & SYEAR==ad[i,'YEAR']),mls=ad[i,'fsrs'], year=ad[i,'YEAR'],lfa=ad[i,'LFA'],females.only=F,fsrs.recruit.samples=T)
					op = weightedCLF(x=da,returnLF=T,fsrs.recruit.samples=T,lab=paste("LFA",ad[i,"LFA"],ad[i,"YEAR"]))
							os = op
				os$vec<-NULL
			outS[[i]] <- unlist(os)
		####No cohort analysis
}
			outS = as.data.frame(do.call(rbind,outS))
			outS = toNums(outS,2:ncol(outS))
			save(outS,file = file.path(project.datadirectory('bio.lobster'),'outputs','SummaryfsrsrecruitmentSamplesLanded27-33.rdata'))
