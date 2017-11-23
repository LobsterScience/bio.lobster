#CA

require(bio.lobster)
require(bio.utilities)
require(PBSmapping)
    lobster.db( DS="atSea", p=p)		# at Sea sampling from materialized view
    lobster.db( DS="fsrs", p=p)		# FSRS recruitment traps


	atSea = subset(atSea, LFA<40)
	atSea = addSYEAR(atSea)

#assign years	
	ih = which(is.na(atSea$SYEAR))
	atSea$SYEAR[ih] <- year(atSea$SDATE[ih])
	atSea$SYEAR[ih] <- ifelse(atSea$LFA[ih] %in% 35:38 & month(atSea$SDATE[ih]) %in% c(10,11,12), year(atSea$SDATE[ih])+1,atSea$SYEAR[ih])
	atSea = subset(atSea, SYEAR>2002)

#assign grids
	LFAgrid<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","GridPolys.csv"))
	atSea = makePBS(atSea,polygon=F)
	a = which(is.na(atSea$Y) | is.na(atSea$X))
	a1 = findPolys(atSea[-a,],LFAgrid,maxRows = 3e6,includeBdry=1)
	atSea = merge(atSea,a1,by='EID')
	atSea$mon = month(atSea$SDATE) 
	atSea$I = 1
	atSea$CARLENGTH = round(atSea$CARLENGTH)
	i = which(atSea$CARLENGTH>250)
	atSea = atSea[-i,]

#merging atSea and logbooks
	logsa = lobster.db('process.logs.unfiltered')
	logsa = aggregate(cbind(NUM_OF_TRAPS,WEIGHT_KG)~GRID_NUM+SD_LOG_ID+SYEAR+WOS+BUMPUP+LFA,data=logsa, FUN=sum)
	links = lobster.db('atSea.logbook.link')

	###aside
	aS = aggregate(CARLENGTH~TRIPNO+LFA+GRIDNO+SYEAR,data=atSea,FUN=length)
	aS = rename.df(aS,'GRIDNO','GRID_NUM')
	a = merge(aS,links,by='TRIPNO')

	CC=list()
	rn = c("SD_LOG_ID_DO","SD_LOG_ID_DB","SD_LOG_ID_DA")
	for(i in rn) {
		a = rename.df(a,i,'SD_LOG_ID')
		a = 


	}






CLF = merge(CLF, links, by='TRIPNO',all.x=T)      
CLF = rename.df(CLF,'SD_LOG_ID_DO','SD_LOG_ID')
CLFa = merge(CLF,logs,by=c('SD_LOG_ID','GRID_NUM','SYEAR','LFA'),all.x=T)
i = which(is.na(CLFa$WEIGHT_KG))

CC = CLFa[-i,]
CC$SD_LOG_ID_DB <- CC$SD_LOG_ID_DA <- NULL
CLFa = CLFa[i,]
CLFa$SD_LOG_ID <- NULL

CLFa = rename.df(CLFa,'SD_LOG_ID_DB','SD_LOG_ID')
CLFa$WOS = CLFa$NUM_OF_TRAPS = CLFa$WEIGHT_KG <-NULL
CLFa = merge(CLFa,logs,by=c('SD_LOG_ID','GRID_NUM','LFA','SYEAR'),all.x=T)
i = which(is.na(CLFa$WEIGHT_KG))

CCC = CLFa[-i,]
CCC$SD_LOG_ID_DA <- NULL
CC = rbind(CC,CCC)

CLFa = CLFa[i,]
CLFa$SD_LOG_ID <- NULL
CLFa = rename.df(CLFa,'SD_LOG_ID_DA','SD_LOG_ID')
CLFa$WOS = CLFa$NUM_OF_TRAPS = CLFa$WEIGHT_KG <-NULL
CLFa = merge(CLFa,logs,by=c('SD_LOG_ID','GRID_NUM','LFA','SYEAR'),all.x=T)
i = which(is.na(CLFa$WEIGHT_KG))

CCC = CLFa[-i,]




###Start with one area we know we have data

require(bio.lobster)
require(bio.utilities)
require(PBSmapping)
   

i = 1981:2016
ll = '32'
out = list() 
	lobster.db('atSea.clean')
	atsea = atSea.clean
	logs = lobster.db('process.logs.unfiltered')

for(a in 1:length(i)) {


		da = atSeaLogbookLinker(atSea = atsea, logsa = logs, year=i[a],lfa=ll)
		logsa = lobster.db('process.logs')
		logsa1 = try(aggregate(WEIGHT_KG~GRID_NUM+SD_LOG_ID+SYEAR+WOS+BUMPUP+LFA,data=subset(logsa,LFA==ll & SYEAR %in% i[a]), FUN=sum),silent=T)
		logsa2 = try(aggregate(NUM_OF_TRAPS~GRID_NUM+SD_LOG_ID+SYEAR+WOS+BUMPUP+LFA,data=subset(logsa,LFA==ll & SYEAR %in% i[a]), FUN=sum),silent=T)	
		logsa3 = try(merge(logsa1,logsa2,all=T))
		if ( "try-error" %in% c(class(logsa3))) logsa3=NULL
		      
out[[a]] = weightedCLF(x=da,logs=logsa3,grouping=list(WOS = 1:4))

}


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


