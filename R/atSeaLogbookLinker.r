#' @export

atSeaLogbookLinker <- function(atSea,logsa,year=2016,lfa='31B',females.only=T) {
    links = lobster.db('atSea.logbook.link') 
	Fish.Date = lobster.db('season.dates')
	atSea = subset(atSea, SYEAR ==year & LFA == lfa)
	
    atSea$WOS<-NA
    h <- subset(Fish.Date,LFA==lfa & SYEAR == year)
    atSea$WOS = 1
	if(year>1999)    atSea$WOS <- floor(as.numeric(as.POSIXct(atSea$SDATE)-min(h$START_DATE))/7)+1
 	i = which(atSea$WOS<0)
 	if(length(i)>0) atSea = subset(atSea,WOS>0)
	tr = unique(atSea$TRIPNO)
	links = subset(links, TRIPNO %in% tr)	
		

	atSea$I <- ifelse(atSea$SPECIESCODE==2550,1,0)
	atSea = rename.df(atSea,'GRIDNO','GRID_NUM')
	
				a 		= aggregate(CARLENGTH~TRIPNO+GRID_NUM+SYEAR,na.action=NULL,data=atSea,FUN=length)
				trt 	= aggregate(TRAPNO~TRIPNO+LFA+GRID_NUM+SYEAR, data=atSea ,na.action=NULL, FUN= function(x)length (unique(x)))
				trt 	= rename.df(trt,'TRAPNO','TrapsSampled')
				
				nLTr 	= aggregate(TRAPNO~TRIPNO+LFA+GRID_NUM+SYEAR, data=subset(atSea,SPECIESCODE==2550),na.action=NULL, FUN= function(x)length (unique(x)))	
				nLTr 	= rename.df(nLTr,'TRAPNO','TrapsSampledwLobster')
				
				nL 		= aggregate(I~TRIPNO+LFA+GRID_NUM+SYEAR, data=subset(atSea,SPECIESCODE==2550) ,na.action=NULL, FUN= sum)	
				nL 	= rename.df(nL ,'I','NLobster')
				
				nLf 	= aggregate(I~TRIPNO+LFA+GRID_NUM+SYEAR, data=subset(atSea,SPECIESCODE==2550 & SEX==2) ,na.action=NULL, FUN= sum)	
				nLf 	= rename.df(nLf ,'I','NFemaleLobster')
				
				nLm 	= aggregate(I~TRIPNO+LFA+GRID_NUM+SYEAR, data=subset(atSea,SPECIESCODE==2550 & SEX==1) ,na.action=NULL, FUN= sum)	
				nLm 	= rename.df(nLm ,'I','NMaleLobster')
			
				nLb		= try(aggregate(I~TRIPNO+LFA+GRID_NUM+SYEAR, data=subset(atSea,SPECIESCODE==2550 & SEX==3) ,na.action=NULL, FUN= sum),silent=T)	
				if(!is.null(nrow(nLb))) {nLb 	= rename.df(nLb ,'I','NBerriedLobster')} else {nLb = trt[,c('TRIPNO','GRID_NUM','LFA','SYEAR')]; nLb$NBerriedLobster = 0} 
				
				nLc     = try(aggregate(I~TRIPNO+LFA+GRID_NUM+SYEAR, data=subset(atSea,SPECIESCODE==2550 & CULL>0) ,na.action=NULL, FUN= length),silent=T)	
				if(!is.null(nrow(nLc))) {nLc	= rename.df(nLc ,'I','NCullLobster')} else {nLc = trt[,c('TRIPNO','GRID_NUM','LFA','SYEAR')]; nLc$NCullLobster = 0}
				
				nLv     = try(aggregate(I~TRIPNO+LFA+GRID_NUM+SYEAR, data=subset(atSea,SPECIESCODE==2550 & VNOTCH>0) ,na.action=NULL, FUN= length),silent=T)	
				if(!is.null(nrow(nLv))) {nLv 	= rename.df(nLv ,'I','NVnotchedLobster')} else {nLv = trt[,c('TRIPNO','GRID_NUM','LFA','SYEAR')]; nLv$NVnotchedLobster = 0}
			
			trSum = Reduce(function(...) merge(...,all=T),list(trt,nLTr,nL,nLm,nLf,nLb,nLc,nLv))
			trSum = na.zero(trSum)

	aS = subset(atSea,SPECIESCODE==2550)
	if(females.only) aS = subset(aS,SEX %in% c(2,3))

	aS$ids = paste(aS$TRIPNO, aS$LFA, aS$GRID_NUM,aS$WOS, aS$SYEAR,sep="-")
	i = which(aS$CARLENGTH>=250)
	if(length(i) >0) aS = aS[-i,]
	IDs = unique(aS$ids)
	aS = subset(aS,CARLENGTH %in% 30:250)
	breaks = seq(30,250,1)
	CLF = list()
			   for(i in 1:length(IDs)){
            			CLF[[i]]<-with(subset(aS,ids==IDs[i]), {
            			g = c(IDs[i],hist(CARLENGTH,breaks = breaks,plot=F)$counts)
            			return(g)
            			})
        		}
	
			CLF = as.data.frame(do.call(rbind,CLF))
			names(CLF) <- c('ids',breaks[-1])
			CLF = toNums(CLF,2:ncol(CLF))
			CLF = cbind(do.call(rbind,strsplit(CLF$ids,"-")),CLF)
			names(CLF)[1:5] = c('TRIPNO','LFA','GRID_NUM','WOS','SYEAR')
			aCC = CLF = merge(CLF,trSum)
			


	if(year>2001){
		logsa3 = aggregate(cbind(WEIGHT_KG,NUM_OF_TRAPS)~GRID_NUM+SD_LOG_ID+SYEAR+BUMPUP+LFA,data=subset(logsa,LFA==lfa & SYEAR %in% year),na.action=NULL, FUN=sum)
		aa  =merge(logsa3,links,by.x='SD_LOG_ID',by.y = 'SD_LOG_ID_DO')
		aCC = merge(CLF,aa,by=c('TRIPNO','GRID_NUM','LFA','SYEAR'),all.x=T)
	
	i = which(is.na(aCC$WEIGHT_KG))
	aC = aCC[i,]
	aCC = aCC[-i,]
	aCk = unique(aCC$TRIPNO)
	#split the landings if two grids are sampled but only one log book evident
	
	if(any(aC$TRIPNO %in% aCk)){
		ik = which(aC$TRIPNO %in% aCk)
			for(j  in 1:length(ik)) {
												#if(j==14) browser()
				uu = which(aCC$TRIPNO==aC$TRIPNO[ik[j]])
				ww = which(aC$TRIPNO==aC$TRIPNO[ik[j]])
				if(length(uu)>1) uu = uu[1]
				for(d in 1:length(ww)){
				w = aC[ww[d],]
				u = aCC[uu,]
				x = w$TrapsSampled / (w$TrapsSampled+u$TrapsSampled) #EFFORT proration
				w$SD_LOG_ID = u$SD_LOG_ID
				w$WOS 		= u$WOS
				w$BUMPUP 	= u$BUMPUP
				w$WEIGHT_KG = u$WEIGHT_KG * x 
				u$WEIGHT_KG = u$WEIGHT_KG - w$WEIGHT_KG
				w$NUM_OF_TRAPS = round(u$NUM_OF_TRAPS*x)
				u$NUM_OF_TRAPS = u$NUM_OF_TRAPS - w$NUM_OF_TRAPS
				w$SD_LOG_ID_DB = u$SD_LOG_ID_DB
				w$SD_LOG_ID_DA = u$SD_LOG_ID_DA
				aC[ww[d],] <- w
				aCC[uu,] <- u
				}
			}
		}
		
	aCC = rbind(aCC,aC)	

	if(any(aCC$TRIPNO %in% aa$TRIPNO)){
	uo1 = which(is.na(aCC$WEIGHT_KG))
	if(length(uo1)>1){
	uo = aCC[uo1,]
	aCC = aCC[-uo1,]
	tr = unique(uo$TRIPNO)	
	if(any(aa$TRIPNO %in% tr)) {
		io = which(uo$TRIPNO %in% aa$TRIPNO)
		iou = uo[io,]
		iou = within(iou,{SD_LOG_ID = BUMPUP= WEIGHT_KG= NUM_OF_TRAPS= SD_LOG_ID_DB= SD_LOG_ID_DA =NULL}) 
		if(all(is.na(aa$GRID_NUM))) {
			aa$GRID_NUM = NULL
			} else {
			iou$GRID_NUM = NULL	
			}
		aW = merge(iou,aa,by=c('TRIPNO','LFA','SYEAR'),all.x=T)
		uo = uo[-io,]
		aCC = rbind(aCC,aW)
		aCC = rbind(aCC,uo)
		} else {
	aCC = rbind(aCC,uo)
		}

	
#try logbooks from day before
	aa  =merge(logsa3,links,by.x='SD_LOG_ID',by.y = 'SD_LOG_ID_DB')
	tr = unique(uo$TRIPNO)	

	if(any(aa$TRIPNO %in% tr)) {
		io = which(uo$TRIPNO %in% aa$TRIPNO)
		iou = uo[io,]
		iou = within(iou,{SD_LOG_ID = BUMPUP= WEIGHT_KG= NUM_OF_TRAPS= SD_LOG_ID_DB= SD_LOG_ID_DA=NULL}) 
		aW = merge(iou,aa,by=c('TRIPNO','LFA','GRID_NUM','SYEAR'),all.x=T)
		i = which(is.na(aW$WEIGHT_KG))
		aC = aW[i,]
		aWW = aW[-i,]
	aCk = unique(aWW$TRIPNO)
	#split the landings if two grids are sampled but only one log book evident
		if(any(aC$TRIPNO %in% aCk)){
		ik = which(aC$TRIPNO %in% aCk)
			for(j  in 1:length(ik)) {
			
				uu = which(aWW$TRIPNO==aC$TRIPNO[ik[j]])
				ww = which(aC$TRIPNO==aC$TRIPNO[ik[j]])
				if(length(uu)>1) uu = uu[1]
				for(d in 1:length(ww)){
							w = aC[ww[d],]
							u = aWW[uu,]
							x = w$TrapsSampled / (w$TrapsSampled+u$TrapsSampled) #EFFORT proration
							w$SD_LOG_ID = u$SD_LOG_ID
							w$WOS 		= u$WOS
							w$BUMPUP 	= u$BUMPUP
							w$WEIGHT_KG = u$WEIGHT_KG * x 
							u$WEIGHT_KG = u$WEIGHT_KG - w$WEIGHT_KG
							w$NUM_OF_TRAPS = round(u$NUM_OF_TRAPS*x)
							u$NUM_OF_TRAPS = u$NUM_OF_TRAPS - w$NUM_OF_TRAPS
							w$SD_LOG_ID_DB = u$SD_LOG_ID_DB
							w$SD_LOG_ID_DA = u$SD_LOG_ID_DA
							aC[ww[d],] <- w
							aWW[uu,] <- u
							}
			}
		}
		aWW = rename.df(aWW,'SD_LOG_ID_DO','SD_LOG_ID_DB')
		aW = rename.df(aW,'SD_LOG_ID_DO','SD_LOG_ID_DB')
	aCC = rbind(aCC,aWW)
	aCC	= rbind(aCC,aW)
	uo1 = which(is.na(aCC$WEIGHT_KG))
	uo = aCC[uo1,]
	aCC = aCC[-uo1,]
	tr = unique(uo$TRIPNO)	
	if(any(aa$TRIPNO %in% tr)) {
		io = which(uo$TRIPNO %in% aa$TRIPNO)
		iou = uo[io,]
		iou = within(iou,{SD_LOG_ID = BUMPUP= WEIGHT_KG= NUM_OF_TRAPS= SD_LOG_ID_DB= SD_LOG_ID_DA = GRID_NUM=NULL}) 
		aW = merge(iou,aa,by=c('TRIPNO','LFA','SYEAR'),all.x=T)
		aW = rename.df(aW,'SD_LOG_ID_DO','SD_LOG_ID_DB')
		uo = uo[-io,]
		}
	aCC = rbind(aCC,aW)
			}
		}	
	}
}
	return(aCC)
}