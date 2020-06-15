
  
	p = bio.lobster::load.environment()	
	la()
    figdir = file.path(project.datadirectory("bio.lobster"),"figures","Bycatch")
    dir.create(figdir)

	LFAs<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","LFAPolys.csv"))
	LF = subset(LFAs,PID>32)
	LFAgrid<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","GridPolys.csv"))
	LG = subset(LFAgrid,PID>32 & PID<300)
	
	LobsterMap('all')



    lobster.db('atSea')
    lobster.db('atSea.CatchLevel')

    logsInSeason<-lobster.db('process.logs')
    
	atS = subset(atSea,LFA %in% c(33,34,35,36,38,41))
	atS = makePBS(atS,polygon=F)
	atSC = makePBS(atSeaCatchLevel,polygon=F)

	atSCr = subset(atSC,!is.na(X))
	atSCr = findPolys(atSCr,LF)
	atSCr$LFAe = atSCr$PID

	atSC = merge(atSC,atSCr[,c('EID','LFAe')],by='EID',all.x=T)
    i = which(is.na(atSC$LFA)&!is.na(atSC$LFAe))
    atSC$LFA[i] <- atSC$LFAe[i]
    atSC$LFAe <- NULL
    atSC = subset(atSC,LFA %in% c('33','34','35','36','38','40','41'))
    atSC$ID = 1
    atSC$UID = paste(atSC$TRIPNO,atSC$STRINGNO,sep="_")
    o = as.data.frame(unique(cbind(atSC$UID,atSC$COMMENTS)))

    save(atS,file='~/tmp/atSeaData.rdata')
    save(atSC,file='~/tmp/atSeaDataAggregated.rdata')

    atSCe = read.csv(file.path(project.datadirectory('bio.lobster'),'data','AtSeaDataAggregatedWithEmptyTrapsCoates.csv'))
    atSS = merge(atSC, atSCe[,c('UID','Sampled','Total','Nempty')],by='UID',all.x=T)
    save(atSS,file=file.path(project.datadirectory('bio.lobster'),'data','AtSeaDataAggregatedWithEmptyTrapsCoates.rdata'))
    h = with(atSS,((as.numeric(Nempty))/ (as.numeric(Total))))


#time series of effort -- using either cpue or logbook recorded effort