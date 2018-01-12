#landings and abundance


require(bio.lobster)
require(bio.utilities)
require(PBSmapping)
la()


#abundance comes from the cohort analysis part from previous and can load from there
			load(file.path(project.datadirectory('bio.lobster'),'outputs','atSeaIndicatorsNumbersLandedLFA27-33.rdata'))
	aS = out

			load(file.path(project.datadirectory('bio.lobster'),'outputs','fsrsNumbersLanded33.rdata'))
	fS = out

		load(file.path(project.datadirectory('bio.lobster'),'outputs','portNumbersLanded27-33.rdata'))
	pS = out


	p = lobster.db('seasonal.landings')
	g = lobster.db('annual.landings')
	g = g[-nrow(g),]
	p = p[-nrow(p),]

	g = subset(g,YR>=1980)
		
	ad = c(27,28,29,30,'31A','31B',32,33)
	
	for(i in 1:length(ad)) {
			print(ad[i])
				po = ad[i]
				if(po == 27) 	{ lp = g[,c('YR',names(g)[grep(po,names(g))])]}
				if(po == 28) 	{ lp = g[,c('YR',names(g)[grep(po,names(g))])]}
				if(po == 29) 	{ lp = g[,c('YR',names(g)[grep(po,names(g))])]}
				if(po == 30) 	{ lp = g[,c('YR',names(g)[grep(po,names(g))])]}
				if(po == '31A') { lp = g[,c('YR',names(g)[grep(po,names(g))])]}
				if(po == '31B') { lp = g[,c('YR',names(g)[grep(po,names(g))])]}
				if(po == 32) 	{ lp = g[,c('YR',names(g)[grep(po,names(g))])]}
				if(po == 33) 	{ lp = p[,c('SYEAR',names(p)[grep(po,names(p))])]; lp = rename.df(lp,'SYEAR','YR'); lp$YR = as.numeric(substr(lp$YR,6,9)); lp = subset(lp,YR>=1980)}
				op = list()
				op[[1]] = subset(aS,LFA == ad[i],select=c(YEAR,N))
				op[[2]] = subset(pS,LFA == ad[i],select=c(YEAR,N))
				if(ad[i] == 33) op[[3]] = subset(fS,LFA == ad[i],select=c(YEAR,N))

				LandAbundPlot(land = lp,abund = op,lfa = ad[i])
			}



###new recruit biomass


		   load(file = file.path(project.datadirectory('bio.lobster'),'outputs','SummaryatSeaIndicatorsDataLFA27-33.rdata')) #at Sea
		   aSS = outS 
		   aSS = rename.df(aSS,'Year','YEAR')
		   aS = merge(aS[,c('YEAR','LFA','N','Land','expl')],aSS[,c('YEAR','LFA','new.rec','recWt','landWt')])
		   aS$ID = 'aS'

		   	load(file.path(project.datadirectory('bio.lobster'),'outputs','portSummaryLFA27-33.rdata'))
			pSS = outS
			pSS = rename.df(pSS,'Year','YEAR')
		    pS = merge(pS[,c('YEAR','LFA','N','Land','expl')],pSS[,c('YEAR','LFA','new.rec','recWt','landWt')])
			pS$ID = 'pS'


aA = rbind(aS,pS)
aA$nNewRec = aA$N * aA$new.rec
a2 = aggregate(cbind(nNewRec,N,recWt)~YEAR + LFA,data=aA,FUN = mean)

  load(file=file.path(project.datadirectory('bio.lobster'),'outputs','ccir','summary','compiledExploitationCCIR.rdata'))

oi$LFA[which(oi$LFA=='LFA 27 Combined')] <- "27"
oi$LFA[which(oi$LFA=='LFA 33 Combined')] <- "33"
oi$LFA[which(oi$LFA=='LFA 30')] <- "30"
oi$LFA[which(oi$LFA=='LFA 29')] <- "29"
oi$LFA[which(oi$LFA=='LFA 31A')] <- "31A"
oi$LFA[which(oi$LFA=='LFA 31B')] <- "31B"
oi$LFA[which(oi$LFA=='LFA 32')] <- "32"

oi = rename.df(oi,'Yr','YEAR')
aa = merge(a2,oi,by=c('YEAR','LFA'))

aa$RecB = aa$nNewRec / aa$ERfm * aa$recWt
aa$RecBu = aa$nNewRec / aa$ERfl * aa$recWt
aa$RecBl = aa$nNewRec / aa$ERfu * aa$recWt

ii = which(aa$ERfl<0)

aa = aa[-ii,]

save(aa,file=file.path(project.datadirectory('bio.lobster'),'outputs','EstimatedRecruitBiomassLFA27-33.rdata')) 
hj = unique(aa$LFA)

for(i in hj){
	li = subset(aa,LFA==i)

	ylims = range(c(li$RecBl,li$RecBu))
	if(i %in% c('31A','31B')) ylims[2] = 1500
	if(i %in% c('30')) ylims[2] = 1000
	
	plot(li$YEAR,li$RecB,col='blue',type='b',lty=1,xlab='Year',ylab='New Recruit Biomass',main=paste('LFA',i),ylim=ylims)
	arrows(li$YEAR,y1=li$RecB,y0 = li$RecBu, length=0,col='blue')
	arrows(li$YEAR,y1=li$RecB,y0 = li$RecBl, length=0,col='blue')
savePlot(file.path(project.figuredirectory('bio.lobster'),'AtSeaIndictors',paste('NewRecruitBiomass',i,'.png',sep="")),type='png')
}


