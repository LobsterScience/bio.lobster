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
   

i = 2010:2016
out = list() 
for(a in 1:length(i)) {
da = atSeaLogbookLinker(year=i[a],lfa='27')
logsa = lobster.db('process.logs')
logsa1 = aggregate(WEIGHT_KG~GRID_NUM+SD_LOG_ID+SYEAR+WOS+BUMPUP+LFA,data=subset(logsa,LFA=='27' & SYEAR %in% i[a]), FUN=sum)
logsa2 = aggregate(NUM_OF_TRAPS~GRID_NUM+SD_LOG_ID+SYEAR+WOS+BUMPUP+LFA,data=subset(logsa,LFA=='27' & SYEAR %in% i[a]), FUN=sum)	
logsa3 = merge(logsa1,logsa2,all=T)

out[[i]] = weightedCLF(x=da,logs=logsa3)
}





