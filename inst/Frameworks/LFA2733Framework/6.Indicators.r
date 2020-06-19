#6.Indicators


require(bio.lobster)
require(bio.utilities)
require(PBSmapping)
la()

#mid seseason indicators - median and max size; new recruits

#LOADS of DATA handling....sorry :(
	p = lobster.db('seasonal.landings')
	g = lobster.db('annual.landings')
	g = g[-nrow(g),]
	p = p[-nrow(p),]
	p = rename.df(p,'SYEAR','YR')
	p$YR = as.numeric(substr(p$YR,6,9))
	p = subset(p,YR>=1980)
	g = subset(g,YR>=1980)
	g  = reshape(g,idvar='YR',varying=list(2:ncol(g)),direction='long',times=names(g)[2:ncol(g)])
	g$LFA = substr(g$time,4,8)		
	ad = c(27,28,29,30,'31A','31B',32)
	g = subset(g,LFA %in% ad, select=c(YR,LFA27,LFA))
	g = rename.df(g,'LFA27','Landings')

	p = p[,c('YR','LFA33')]
	p$LFA = '33'
	p = rename.df(p,'LFA33','Landings')
	
	pp = rbind(g,p)

	load(file.path(project.datadirectory('bio.lobster'),'outputs','atSeaIndicatorsNumbersLandedLFA27-33.rdata'))
		aS = out
		aS = rename.df(aS,'YEAR','YR')
		df = c('LFA','YR','N','expl')
		aS = aS[,df]
		names(aS)[3:ncol(aS)] = paste('AtSea',names(aS)[3:ncol(aS)],sep="." )
	
   load(file = file.path(project.datadirectory('bio.lobster'),'outputs','SummaryatSeaIndicatorsDataLFA27-33.rdata')) #at Sea
		   aSS = outS 
		   aSS = rename.df(aSS,'Year','YR')
	aSS = rename.df(aSS,c('YEAR','quants.50%','quants.97.5%'),c('YR','median','max'))		 
	df = c( "LFA",    "YR", "prop.female" , "PropMating" ,"EggProduction" )
	aSS = aSS[,df]
	 load(file = file.path(project.datadirectory('bio.lobster'),'outputs','SubsetWksSummaryatSeaIndicatorsDataLFA27-33.rdata')) #at Sea
			aSSS = outS	
			aSSS = rename.df(aSSS,'Year','YR')
			aSSS = rename.df(aSSS,c('YEAR','quants.50%','quants.97.5%'),c('YR','median','max'))		 
			df = c( "LFA",    "YR", "median", "max" ,"new.rec" ,"prop.berried")
			aSSS = aSSS[,df]
			aSS = merge(aSS,aSSS,all=T)
	names(aSS)[3:ncol(aSS)] = paste('AtSea',names(aSS)[3:ncol(aSS)],sep="." )
	aS = merge(aSS,aS,by=c('YR','LFA'),all=T)
	
#FSRS Commercial
	load(file.path(project.datadirectory('bio.lobster'),'outputs','fsrsNumbersLanded33.rdata'))
	fS = out
    	fS = rename.df(fS,'YEAR','YR')
		df = c('LFA','YR','N','expl')
		fS = fS[,df]
		names(fS)[3:ncol(fS)] = paste('FSRSComm',names(fS)[3:ncol(fS)],sep="." )
	

    load(file = file.path(project.datadirectory('bio.lobster'),'outputs','SummaryfsrsCommercialSamplesLanded33.rdata'))
    fSS = outS
    fSS = rename.df(fSS,'Year','YR')
	fSS = rename.df(fSS,c('YEAR','quants.50%','quants.97.5%'),c('YR','median','max'))		 
	df = c( "LFA",    "YR", "median", "max" , "prop.female" , "prop.berried" , "new.rec" )
	fSS = fSS[,df]
	names(fSS)[3:ncol(fSS)] = paste('FSRSComm',names(fSS)[3:ncol(fSS)],sep="." )

	fS = merge(fSS,fS,by=c('YR','LFA'),all=T)

#Port Samples
		load(file.path(project.datadirectory('bio.lobster'),'outputs','portNumbersLanded27-33.rdata'))
	
	pS = out
	pS = rename.df(pS,'YEAR','YR')
		df = c('LFA','YR','N','expl')
		pS = pS[,df]
		names(pS)[3:ncol(pS)] = paste('Port',names(pS)[3:ncol(pS)],sep="." )

	   	load(file.path(project.datadirectory('bio.lobster'),'outputs','portSummaryLFA27-33.rdata'))
			pSS = outS
			pSS = rename.df(pSS,'Year','YR')
	    	pSS = rename.df(pSS,c('YEAR','quants.50%','quants.97.5%'),c('YR','median','max'))		 
	df = c( "LFA",    "YR", "prop.female" , 'PropMating','EggProduction')
	pSS = pSS[,df]

	   load(file.path(project.datadirectory('bio.lobster'),'outputs','subsetweeksportSummaryLFA27-33.rdata'))
	 		pSSS = outS	
			pSSS = rename.df(pSSS,'Year','YR')
			pSSS = rename.df(pSSS,c('YEAR','quants.50%','quants.97.5%'),c('YR','median','max'))		 
			df = c( "LFA",    "YR", "median", "max" ,"new.rec" )
			pSSS = pSSS[,df]

			pSS = merge(pSS,pSSS,all=T)


	names(pSS)[3:ncol(pSS)] = paste('Port',names(pSS)[3:ncol(pSS)],sep="." )

	pS = merge(pSS,pS,by=c('YR','LFA'),all=T)

#Fsrs recruitment
		load(file = file.path(project.datadirectory('bio.lobster'),'outputs','SummaryfsrsrecruitmentSamplesLanded27-33.rdata'))

			fR = outS
    fR = rename.df(fR,'Year','YR')
	fR = rename.df(fR,c('YEAR','quants.50%','quants.97.5%'),c('YR','median','max'))		 
	df = c( "LFA",    "YR", "median", "max" , "prop.female" , "prop.berried" )
	fR = fR[,df]
	names(fR)[3:ncol(fR)] = paste('FSRSRec',names(fR)[3:ncol(fR)],sep="." )

#Merge data sets

	mls = read.csv(file=file.path(project.datadirectory('bio.lobster'),'data','inputs','MLS.Changes.all.LFA.csv'))
	mls = rename.df(mls,'Year','YR')
	mls = mls[,c('YR','LFA','MLS_MM')]
	mls = subset(mls, LFA %in%  c(27,28,29,30,'31A','31B',32,33))

	load(file=file.path(project.datadirectory('bio.lobster'),'outputs','EstimatedRecruitBiomassLFA27-33.rdata')) 
	
	aa = aa[,c('YEAR','LFA','RecB')]
	aa = rename.df(aa,c("YEAR",'RecB'),c('YR','RecruitmentBiomass'))

load(file=file.path(project.datadirectory('bio.lobster'),'outputs','ccir','summary','compiledExploitationCCIR.rdata'))

oi$LFA[which(oi$LFA=='LFA 27 Combined')] <- "27"
oi$LFA[which(oi$LFA=='LFA 33 Combined')] <- "33"
oi$LFA[which(oi$LFA=='LFA 30')] <- "30"
oi$LFA[which(oi$LFA=='LFA 29')] <- "29"
oi$LFA[which(oi$LFA=='LFA 31A')] <- "31A"
oi$LFA[which(oi$LFA=='LFA 31B')] <- "31B"
oi$LFA[which(oi$LFA=='LFA 32')] <- "32"

oi = rename.df(oi,'Yr','YR')

oi = oi[grep('LFA',oi$LFA,invert=T),]
oi = subset(oi,ERfl>0,select=c(YR,LFA,ERfm))
oi = rename.df(oi,'ERfm','CCIR.Exploitation')	
	allS = Reduce(function(...) merge(...,all=T),list(aS,fS,pS,fR,pp,aa,oi))



#FSRS temperature 

load(file.path(project.datadirectory('bio.lobster'),'Temperature Data','tempIndicators.rdata'))
tempData = rename.df(tempData,c('area','year','t'),c('LFA','YR','Temperature'))
tempData$t.sd = NULL
allS = merge(allS,tempData,all=T)


#CPUE raw data
		load(file=file.path(project.datadirectory('bio.lobster'),'outputs','rawcpueIndicators27-33B.rdata')) 
	
hq = rename.df(cpueData2,'YEAR','YR')
hq$DATE = hq$CATCH = hq$EFFORT <- NULL

allS = merge(allS,hq,all=T)

#FSRS models
	load(file=file.path(project.datadirectory('bio.lobster'),'outputs','fsrsModelIndicatorsJan2018.rdata')) 
lF = rename.df(legalsLFA,c('YEAR','Area','mu'),c('YR','LFA','FSRS.Legal.CPUE'))
lF = lF[,c('YR','LFA','FSRS.Legal.CPUE')]

rF = rename.df(recruitLFA,c('YEAR','Area','mu'),c('YR','LFA','FSRS.Recruit.CPUE'))
rF = rF[,c('YR','LFA','FSRS.Recruit.CPUE')]

sF = rename.df(shortsLFA,c('YEAR','Area','mu'),c('YR','LFA','FSRS.Shorts.CPUE'))
sF = sF[,c('YR','LFA','FSRS.Shorts.CPUE')]

allS = Reduce(function(...) merge(...,all=T),list(allS,lF,rF,sF))
#

allS = subset(allS,YR<2016)


	save(allS,	file=file.path(project.datadirectory('bio.lobster'),'outputs','CompiledIndicatorsLFA27-33.rdata')) 
	
load(file=file.path(project.datadirectory('bio.lobster'),'outputs','CompiledIndicatorsLFA27-33.rdata')) 


	 old = c('YR','LFA','AtSea.prop.female','AtSea.PropMating','AtSea.EggProduction','AtSea.median','AtSea.max','AtSea.new.rec','AtSea.prop.berried','AtSea.N','AtSea.expl','FSRSComm.median','FSRSComm.max','FSRSComm.prop.female','FSRSComm.prop.berried','FSRSComm.new.rec','FSRSComm.N','FSRSComm.expl','Port.prop.female','Port.PropMating','Port.EggProduction','Port.median','Port.max','Port.new.rec','Port.N','Port.expl','FSRSRec.median','FSRSRec.max','FSRSRec.prop.female','FSRSRec.prop.berried','Landings','RecruitmentBiomass','CCIR.Exploitation','Temperature')
	 new = c('YR','LFA','At.Sea.SexRatio','At.Sea.Prop.Mature','At.Sea.Reprod.Pot','At.Sea.Median.CL','At.Sea.Max.CL','At.Sea.Prop.NewRec','At.Sea.Prop.Berried','At.Sea.Landed.Abund.','At.Sea.CA.Exploit','FSRS.Comm.Median.CL','FSRS.Comm.Max.CL','FSRS.Comm.SexRatio','FSRS.Comm.Prop.Berried','FSRS.Comm.Prop.NewRec','FSRS.Comm.Landed.Abund','FSRS.Comm.CA.Exploit','Port.SexRatio','Port.Prop.Mature','Port.Reprod.Pot','Port.Median.CL','Port.Max.CL','Port.Prop.NewRec','Port.Landed.Abund','Port.CA.Exploit','FSRS.Rec.Median.CL','FSRS.Rec.Max.CL','FSRS.Rec.SexRatio','FSRS.Rec.Prop.Berried','Landings.Wt','BiomassRecruits','ExploitationCCIR','Temperature')

allS = rename.df(allS,old,new)

	#id the variables for log trans
	re = c()
	re=c(re,grep('Reprod',names(allS)))
	#re=c(re,grep('Abund',names(allS)))
	re=c(re,grep('Land',names(allS)))
	re=c(re,grep('Bio',names(allS)))
	
	allS[,re] <- log(allS[,re]+1)

#loop through LFAs
lfa = unique(allS$LFA)

for(i in lfa) {
	dd = subset(allS,LFA==i)
	t0 = 1980
	t1 = 2015
	rownames(dd) = t0:t1
	dd$LFA = dd$YR = NULL
	iu = apply(dd,2,function(x) sum(is.na(x)*1)/length(x)<1-5/35)
	dd = dd[,names(iu)[iu]] # removing all indicators with less than 5 values
	iu=c()
	iu = c(iu,which(names(dd)=='FSRS.Rec.Max.CL'))
	iu = c(iu,which(names(dd)=='FSRS.Rec.Median.CL'))
	iu = c(iu,grep('Sex',names(dd)))
	if(i==33) {
	iu = c(iu,which(names(dd)=='FSRS.Comm.MaxCL'))
	iu = c(iu,which(names(dd)=='FSRS.Comm.MedianCL'))
	}
	dd = dd[,-iu]
	fname = file.path(project.figuredirectory('bio.lobster'),'indicators')
	dir.create(fname)
	Y = pcaAnalyseData(dd, t0, t1,fname=fname,OFN = paste('OrdinationLFA',i,sep=""),addscores=T)
}

  
