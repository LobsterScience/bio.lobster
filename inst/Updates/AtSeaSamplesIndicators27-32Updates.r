


require(bio.lobster)
require(bio.utilities)
require(PBSmapping)
la()
 

			lobster.db('atSea.redo')
			lobster.db('atSea.clean.redo')
			
			atSea.clean = lobster.db('atSea.clean')
			atSea.clean = subset(atSea.clean, SPECIESCODE==2550)
			
			atSea.clean$PORT = ifelse(atSea.clean$PORT==-99,0,atSea.clean$PORT)
			g = lobster.db('annual.landings')
			p = lobster.db('seasonal.landings')
			mls = read.csv(file=file.path(project.datadirectory('bio.lobster'),'data','inputs','MLS.Changes.all.LFA.csv'))
			mls$X = NULL
			load(file.path(project.datadirectory('bio.lobster'),'outputs','deltaTsSimBH.rdata')) #DTs
			ad =  as.data.frame(unique(cbind(atSea.clean$LFA,atSea.clean$SYEAR)))
			ad = ad[order(ad[,1],ad[,2]),]
			names(ad) = c('LFA','YEAR')
			names(mls)[1] <- 'YEAR'
			ad = merge(ad,mls)
			ad = subset(ad,YEAR<2020 & LFA<33)
			atsea = atSea.clean
			cG = lobster.db('community.to.grid.historic')		
			cH = lobster.db('community.to.grid.contemporary.redo')		
	
		out = list() #
		outN = list()
		outS = list()

		for(i in 1:nrow(ad)) {
			print(ad[i,])
				po = ad[i,'LFA']
				yo = ad[i,'YEAR']
				mm = ad[i,'MLS_MM']
				#weighting at sea samples by the proportion of landings
				da = atSeaWeightings(atSea = atsea, comGridHist =subset(cG,LFA==ad[i,'LFA']),comGridCont = subset(cH,LFA==ad[i,'LFA'] & SYEAR==ad[i,'YEAR']), year=ad[i,'YEAR'],lfa=ad[i,'LFA'],females.only=F,at.sea.samples=T,mls=mm)
				op = weightedCLF(x=da,returnLF=T,at.sea.samples=T)
				os = op
				os$vec<-NULL
			outS[[i]] <- os
		
			#Tc is fractional year of catch used for cohort analysis
				if(po == 27) 	{ll = "LFA27-30"; 	lle = 'all areas'; lp = g[,c('YR',names(g)[grep(po,names(g))])]; dt = DTs[[grep('27N',names(DTs))]]; Tc = 0.67}
				if(po == 28) 	{ll = 'LFA28,30'; 	lle = 'all areas'; lp = g[,c('YR',names(g)[grep(po,names(g))])]; dt = DTs[[grep('28',names(DTs))]]; Tc = 0.67}
				if(po == 29) 	{ll = 'LFA29'; 		lle = 'all areas'; lp = g[,c('YR',names(g)[grep(po,names(g))])]; dt = DTs[[grep('29',names(DTs))]]; Tc = 0.67}
				if(po == 30) 	{ll = 'LFA28,30'; 	lle = 'all areas'; lp = g[,c('YR',names(g)[grep(po,names(g))])]; dt = DTs[[grep('30',names(DTs))]]; Tc = 0.67}
				if(po == '31A') {ll = 'LFA29';  	lle = 'all areas'; lp = g[,c('YR',names(g)[grep(po,names(g))])]; dt = DTs[[grep('31A',names(DTs))]]; Tc = 0.67}
				if(po == '31B') {ll = 'LFA32';	 	lle = 'all areas'; lp = g[,c('YR',names(g)[grep(po,names(g))])]; dt = DTs[[grep('31B',names(DTs))]]; Tc = 0.67}
				if(po == 32) 	{ll = 'LFA32'; 		lle = 'all areas'; lp = g[,c('YR',names(g)[grep(po,names(g))])]; dt = DTs[[grep('32',names(DTs))]]; Tc = 0.67}

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
				ca = cohortAnalysis(lens = LCA[,1], N = LCA[,2], dt = LCA[,3]) #annual

				LCA$LFA = po
				LCA$Year = yo
				LCA$MLS = mm
				out[[i]] = c(LFA = po, YEAR=yo, MLS=mm, N = sum(v0$N),Land = le,expl =ca$expl, F = ca$wF,M = ca$M,tF = ca$termF)
				}
			}
			out = as.data.frame(do.call(rbind,out))
			out = toNums(out,2:ncol(out))
			save(out,file = file.path(project.datadirectory('bio.lobster'),'outputs','atSeaIndicatorsNumbersLandedLFA27-32.rdata'))

			outN = as.data.frame(do.call(rbind,outN))
			save(outN,file = file.path(project.datadirectory('bio.lobster'),'outputs','atSeaIndicatorsNatSizeLFA27-32.rdata'))
			
			outS = as.data.frame(do.call(rbind,outS))
			outS = toNums(outS,2:ncol(outS))
		   save(outS,file = file.path(project.datadirectory('bio.lobster'),'outputs','SummaryatSeaIndicatorsDataLFA27-32.rdata'))
#loader	   
		load(file = file.path(project.datadirectory('bio.lobster'),'outputs','atSeaIndicatorsNumbersLandedLFA27-32.rdata'))
		load(file = file.path(project.datadirectory('bio.lobster'),'outputs','atSeaIndicatorsNatSizeLFA27-32.rdata'))
		load(file = file.path(project.datadirectory('bio.lobster'),'outputs','SummaryatSeaIndicatorsDataLFA27-32.rdata'))
	
L = unique(out$LFA)
pdf('~/tmp/NLandings.pdf')
for(i in 1:length(L)){
	 par(mar=c(4,5,3,5))
		xpp = subset(out,LFA==L[i])
		plot(xpp$YEAR,xpp$N,type='p', pch=16, ,xlab='Year',ylab = 'Landing #')
		rmean = runmed(xpp$N,k=3,endrule='median')
        rmean.yr = xpp$YEAR[1:length(xpp$YEAR)]
		  lines(rmean.yr,rmean,lty=1,lwd=3,col='firebrick2')
		par(mar=c(4,5,3,5),new=T)
		plot(xpp$YEAR,xpp$Land,type='p',lty=3,xaxt='n', yaxt='n',ylab='',xlab='',pch=17)
		rmean = runmed(xpp$Land,k=3,endrule='median')
        rmean.yr = xpp$YEAR[1:length(xpp$YEAR)]
		lines(rmean.yr,rmean,lty=1,lwd=3,col='blue')
		axis(side=4,srt=90)
		mtext(side=4,'Landing Wt',line=3,col='blue')
		title(paste("LFA",L[i]))	
	}
dev.off()



L = unique(outS$LFA)
pdf('~/tmp/propBerried.pdf')
for(i in 1:length(L)){
	 par(mar=c(4,5,3,5))
		xpp = subset(outS,LFA==L[[i]][1])
		plot(xpp$Year,xpp$prop.berried,type='p', pch=16, ,xlab='Year',ylab = 'Proportion Berried')
		rmean = runmed(xpp$prop.berried,k=3,endrule='median')
        rmean.yr = xpp$Year[1:length(xpp$Year)]
		  lines(rmean.yr,rmean,lty=1,lwd=3,col='firebrick2')
		  title(paste("LFA",L[[i]][1]))	

			}
dev.off()


L = unique(outS$LFA)
pdf('~/tmp/EggProd.pdf')
for(i in 1:length(L)){
	 par(mar=c(4,5,3,5))
		xpp = subset(outS,LFA==L[[i]][1])
		plot(xpp$Year,xpp$EggProduction,type='p', pch=16, ,xlab='Year',ylab = 'Reproductive Potential')
		rmean = runmed(xpp$EggProduction,k=3,endrule='median')
        rmean.yr = xpp$Year[1:length(xpp$Year)]
		  lines(rmean.yr,rmean,lty=1,lwd=3,col='firebrick2')
		  title(paste("LFA",L[[i]][1]))	

			}
dev.off()


L = unique(outS$LFA)
pdf('~/tmp/Recruitment.pdf')
for(i in 1:length(L)){
	 par(mar=c(4,5,3,5))
		xpp = subset(outS,LFA==L[[i]][1])
		plot(xpp$Year,xpp$new.rec,type='p', pch=16, ,xlab='Year',ylab = 'Proportion New Recruits')
		rmean = runmed(xpp$new.rec,k=3,endrule='median')
        rmean.yr = xpp$Year[1:length(xpp$Year)]
		  lines(rmean.yr,rmean,lty=1,lwd=3,col='firebrick2')
		  title(paste("LFA",L[[i]][1]))	

			}
dev.off()