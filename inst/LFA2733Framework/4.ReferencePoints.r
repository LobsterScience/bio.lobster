



  load(file=file.path(project.datadirectory('bio.lobster'),'outputs','ccir','summary','compiledExploitationCCIR.rdata'))

  

require(bio.lobster)
require(bio.utilities)
require(PBSmapping)
la()

	p = lobster.db('seasonal.landings')
	g = lobster.db('annual.landings')
	g = g[-nrow(g),]
	p = p[-nrow(p),]

	g = subset(g,YR>=1985)
	ad = c(27,'28-29',30,'31',32,33)
	usr = c(1629,120,79,250,242,1838)
	lrp = c(1629,120,79,250,242,1838)/2
	for(i in 1:length(ad)) {
			print(ad[i])
				po = ad[i]
				if(po == 27) 	{ lp = g[,c('YR',names(g)[grep(po,names(g))])]}
				if(po == '28-29') 	{ lp = g[,c('YR',names(g)[grep('28',names(g))],names(g)[grep('29',names(g))])]; lp = na.zero(lp); lp$L = lp$LFA28+lp$LFA29; lp$LFA28 = lp$LFA29 = NULL}
				if(po == 29) 	{ lp = g[,c('YR',names(g)[grep(po,names(g))])]}
				if(po == 30) 	{ lp = g[,c('YR',names(g)[grep(po,names(g))])]}
				if(po == '31') { lp = g[,c('YR',names(g)[grep(po,names(g))])]}
				if(po == 32) 	{ lp = g[,c('YR',names(g)[grep(po,names(g))])]}
				if(po == 33) 	{ lp = p[,c('SYEAR',names(p)[grep(po,names(p))])]; lp = rename.df(lp,'SYEAR','YR'); lp$YR = as.numeric(substr(lp$YR,6,9)); lp = subset(lp,YR>=1985)}
				op = list()
				
				LandPlot(lp,usr = usr[i],lrp=lrp[i],lfa = ad[i])
			}



# commercial catch rates
la()
load(file=file.path(project.datadirectory('bio.lobster'),'outputs','rawcpueIndicators27-33B.rdata')) 

hq = rename.df(cpueData2,'YEAR','YR')
hq$DATE = hq$CATCH = hq$EFFORT <- NULL

hq = subset(hq,YR>=1985)
ad = unique(hq$LFA)

mu = aggregate(CPUE~LFA,data=subset(hq,YR %in% 1990:2016),FUN=median)

	usr = mu$CPUE * 0.8
	lrp = mu$CPUE * 0.4
	
	for(i in 1:length(ad)) {
		lp = subset(hq,LFA==ad[i],select=c(YR,CPUE))
		CatchRatePlot(lp,usr = usr[i],lrp=lrp[i],lfa = ad[i])
	}


#Fsrs resutls
load(file=file.path(project.datadirectory('bio.lobster'),'outputs','fsrsModelIndicatorsJan2018.rdata')) 
lF = rename.df(legalsLFA,c('YEAR','Area','mu'),c('YR','LFA','FSRS.Legal.CPUE'))
lF = lF[,c('YR','LFA','FSRS.Legal.CPUE')]

rF = rename.df(recruitLFA,c('YEAR','Area','mu'),c('YR','LFA','FSRS.Recruit.CPUE'))
rF = rF[,c('YR','LFA','FSRS.Recruit.CPUE')]

sF = rename.df(shortsLFA,c('YEAR','Area','mu'),c('YR','LFA','FSRS.Shorts.CPUE'))
sF = sF[,c('YR','LFA','FSRS.Shorts.CPUE')]

allS = Reduce(function(...) merge(...,all=T),list(lF,rF,sF))
allS = subset(allS,LFA %ni% 28)

ad = unique(allS$LFA)

iop = aggregate(cbind(FSRS.Recruit.CPUE,FSRS.Legal.CPUE)~LFA,data=subset(allS,YR<=2016),FUN=median)
iop$usrL = iop$FSRS.Legal.CPUE * 0.8
iop$lrpL = iop$FSRS.Legal.CPUE * 0.4

iop$usrR = iop$FSRS.Recruit.CPUE * 0.8
iop$lrpR = iop$FSRS.Recruit.CPUE * 0.4

for(i in 1:length(ad)){

		ip = subset(iop,LFA==ad[i])
		lp = subset(allS,LFA==ad[i],select=c(YR,FSRS.Legal.CPUE))
		FSRSCatchRatePlot(lp,usr = ip$usrL,lrp=ip$lrpL,lfa = ad[i],recruit=F)
		lp = subset(allS,LFA==ad[i],select=c(YR,FSRS.Recruit.CPUE))
		FSRSCatchRatePlot(lp,usr = ip$usrR,lrp=ip$lrpR,lfa = ad[i],recruit=T)
	
	}

#CCIR for RR
# CCIR
load(file=file.path(project.datadirectory('bio.lobster'),'outputs','ccir','summary','compiledExploitationCCIR.rdata'))

RRs = aggregate(ERfm~LFA,data=oi,FUN=max)
RR75  = aggregate(ERf75~LFA,data=oi,FUN=max)
ad = c(27,29,30,'31A','31B',32,33)

	for(i in 1:length(ad)) {
			print(ad[i])
				poi = po = ad[i]
				if(po == 27) 	{  poi = "LFA 27 Combined"}
				if(po == 29) 	{  poi = 'LFA 29'}
				if(po == 30) 	{  poi = 'LFA 30'}
				if(po == '31A') {  poi = 'LFA 31A'}
				if(po == '31B') {  poi = 'LFA 31B'}
				if(po == 32) 	{  poi = 'LFA 32'}
				if(po == 33) 	{ poi = "LFA 33 Combined"}
				op = list()

				oo = subset(oi,LFA==poi,select=c(Yr,ERfm))
				
      
				RR = subset(RRs,LFA==poi)$ERfm
				RR7 = subset(RR75,LFA==poi)$ERf75

			ExploitationRatePlots(oo,usr = RR,lrp=RR7,lfa = ad[i])
			

			}


# bayesian chage point analysis for defining reference points. NOT USED

require(bcp)

	p = lobster.db('seasonal.landings')
	g = lobster.db('annual.landings')
	p$YR = as.numeric(substr(p$SYEAR,6,9))
	
	g = subset(g,YR>=1980 & YR<2017)
	p = subset(p,YR>=1980 & YR<2017)

      b = bcp(g$LFA27,w0 = 0.2, p0 = 0.01)
      plot(b,xaxlab=g$YR,xlab='Year')
  

  	  b = bcp(g$LFA28,w0 = 0.2, p0 = 0.01)
      plot(b,xaxlab=g$YR,xlab='Year')
  
  	  b = bcp(g$LFA29,w0 = 0.2, p0 = 0.01)
      plot(b,xaxlab=g$YR,xlab='Year')
  

  	  b = bcp(g$LFA30,w0 = 0.2, p0 = 0.01)
      plot(b,xaxlab=g$YR,xlab='Year')
  
