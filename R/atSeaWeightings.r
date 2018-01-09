#' @export

atSeaWeightings <- function(atSea,comGridHist,comGridCont,year=2016,lfa='31B',mls,females.only=T, at.sea.samples=F, fsrs.commercial.samples = F,port.samples=F,fsrs.recruit.samples=F) {
  
   if(at.sea.samples){
	 atSea = subset(atSea, SYEAR ==year & LFA == lfa)
	if(any( atSea$WOS<0) | any(is.na(atSea$WOS))) { u = which(is.na(atSea$WOS) | atSea$WOS<0); atSea$WOS[u] <- NA}
	atSea = rename.df(atSea,'GRIDNO','GRID_NUM')
	aS = atSea = subset(atSea,SPECIESCODE==2550)
	atSea$I <- ifelse(atSea$SPECIESCODE==2550,1,0)
	
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
		
				nLsf 	= aggregate(I~TRIPNO+LFA+GRID_NUM+SYEAR, data=subset(atSea, SEX==2 & CARLENGTH<mls) ,na.action=NULL, FUN= sum)	
				nLsf 	= rename.df(nLsf ,'I','NFemaleShortLobster')

				nLlf 	= aggregate(I~TRIPNO+LFA+GRID_NUM+SYEAR, data=subset(atSea, SEX==2 & CARLENGTH>=mls) ,na.action=NULL, FUN= sum)	
				nLlf 	= rename.df(nLlf ,'I','NFemalelegalLobster')
				
				nLm 	= aggregate(I~TRIPNO+GRID_NUM+LFA+SYEAR, data=subset(atSea, SEX==1) ,na.action=NULL, FUN= sum)	
				nLm 	= rename.df(nLm ,'I','NMaleLobster')			

				nLsm 	= aggregate(I~TRIPNO+GRID_NUM+LFA+SYEAR, data=subset(atSea, SEX==1 & CARLENGTH<mls) ,na.action=NULL, FUN= sum)	
				nLsm 	= rename.df(nLsm ,'I','NmaleShortLobster')

				nLlm 	= aggregate(I~TRIPNO+GRID_NUM+LFA+SYEAR, data=subset(atSea, SEX==1 & CARLENGTH>=mls) ,na.action=NULL, FUN= sum)	
				nLlm 	= rename.df(nLlm ,'I','NmalelegalLobster')
				
							
			trSum = Reduce(function(...) merge(...,all=T),list(trt,nL,nLm,nLf,nLsf,nLlf,nLsm,nLlm,nLb,nLv,nLc))
			trSum = na.zero(trSum)
	

	if(females.only) aS = subset(aS,SEX %in% c(2,3))

	aS$ids = paste(aS$TRIPNO, aS$LFA, aS$GRID_NUM,aS$WOS, aS$SYEAR,aS$PORT,sep="-")
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
			names(CLF)[1:6] = c('TRIPNO','LFA','GRID_NUM','WOS','SYEAR','PORT')
			aCC = CLF = merge(CLF,trSum)

#using landings by port or by stat dist
if(year<=2004){
		cg = aggregate(PropLand~LFA+WOS+GRID_NUM,data=comGridHist,FUN=sum)
		CC = merge(CLF,cg,all.x=T)
		if(any(is.na(CC$PropLand))) {
			v = which(is.na(CC$PropLand))
				for(l in v){
						if(!is.na(CC$WOS[l]) & is.na(CC$GRID_NUM[l])) CC$PropLand[l] = median(cg$PropLand[cg$WOS==CC$WOS[l]],na.rm=T)
						if(is.na(CC$WOS[l]) & !is.na(CC$GRID_NUM[l])) CC$PropLand[l] = median(cg$PropLand[cg$GRID_NUM==CC$GRID_NUM[l]],na.rm=T)
						if((is.na(CC$WOS[l]) &  is.na(CC$GRID_NUM[l])) |( !is.na(CC$WOS[l]) &  !is.na(CC$GRID_NUM[l]))) CC$PropLand[l] = median(cg$PropLand,na.rm=T)
						if(CC$GRID_NUM[l] %ni% cg$GRID_NUM) CC$PropLand[l] = median(cg$PropLand[cg$WOS==CC$WOS[l]],na.rm=T)
						if(CC$WOS[l] %ni% cg$WOS) CC$PropLand[l] = median(cg$PropLand[cg$GRID_NUM==CC$GRID_NUM[l]],na.rm=T)
						if(CC$WOS[l] %ni% cg$WOS & CC$GRID_NUM[l] %ni% cg$GRID_NUM) CC$PropLand[l] = quantile(cg$PropLand,0.25,na.rm=T)
					}
			}

}

	if(year>2004){
		if(nrow(comGridCont)==0) cg = aggregate(PropLand~LFA+WOS+GRID_NUM,data=comGridHist,FUN=sum)
		if(nrow(comGridCont)>0) cg = aggregate(PropLand~LFA+WOS+GRID_NUM+SYEAR,data=comGridCont,FUN=sum)
		 
		CC = merge(CLF,cg,all.x=T)
		ii = is.na(CC$PropLand)
		if(sum(!ii*1)/length(ii)<0.6) cg = aggregate(PropLand~WOS+GRID_NUM,data=comGridHist,FUN=sum)
		CC = merge(CLF,cg,all.x=T)


	if(any(is.na(CC$PropLand))) {
			v = which(is.na(CC$PropLand))
				for(l in v){
						if(!is.na(CC$WOS[l]) & is.na(CC$GRID_NUM[l])) CC$PropLand[l] = median(cg$PropLand[cg$WOS==CC$WOS[l]],na.rm=T)
						if(is.na(CC$WOS[l]) & !is.na(CC$GRID_NUM[l])) CC$PropLand[l] = median(cg$PropLand[cg$GRID_NUM==CC$GRID_NUM[l]],na.rm=T)
						if((is.na(CC$WOS[l]) &  is.na(CC$GRID_NUM[l])) |( !is.na(CC$WOS[l]) &  !is.na(CC$GRID_NUM[l]))) CC$PropLand[l] = median(cg$PropLand,na.rm=T)
						if(CC$GRID_NUM[l] %ni% cg$GRID_NUM) CC$PropLand[l] = median(cg$PropLand[cg$WOS==CC$WOS[l]],na.rm=T)
						if(CC$WOS[l] %ni% cg$WOS) CC$PropLand[l] = median(cg$PropLand[cg$GRID_NUM==CC$GRID_NUM[l]],na.rm=T)
						if(CC$WOS[l] %ni% cg$WOS & CC$GRID_NUM[l] %ni% cg$GRID_NUM) CC$PropLand[l] = quantile(cg$PropLand,0.25,na.rm=T)
				}
			}

	}
}
if(fsrs.commercial.samples) {
			lfa=33
			   	atSea = subset(atSea, SYEAR ==year)
				if(any( atSea$WOS<0) | any(is.na(atSea$WOS))) { u = which(is.na(atSea$WOS) | atSea$WOS<0); atSea$WOS[u] <- NA}
				h = which(atSea$Size==10 & atSea$Short==0)
				atSea$Size[h] = 10.5
				a 		= aggregate(Size~GRID_NUM+SYEAR,na.action=NULL,data=atSea,FUN=length)
				trt 	= aggregate(paste(Trap.Number,TR.ID,sep="")~GRID_NUM+WOS+SYEAR, data=atSea ,na.action=NULL, FUN= function(x)length (unique(x)))
				trt 	= rename.df(trt,'paste(Trap.Number, TR.ID, sep = "")','TrapsSampled')
				
				atSea$I <- 1
				nL 		= aggregate(I~GRID_NUM+SYEAR+WOS, data=subset(atSea) ,na.action=NULL, FUN= sum)	
				nL 	= rename.df(nL ,'I','NLobster')
				
				nLf 	= aggregate(I~GRID_NUM+SYEAR+WOS, data=subset(atSea, Sex==2) ,na.action=NULL, FUN= sum)	
				nLf 	= rename.df(nLf ,'I','NFemaleLobster')
				
			
				nLsf 	= aggregate(I~GRID_NUM+WOS+SYEAR, data=subset(atSea, Sex==2 & Size<=mls) ,na.action=NULL, FUN= sum)	
				nLsf 	= rename.df(nLsf ,'I','NFemaleShortLobster')

				nLlf 	= aggregate(I~GRID_NUM+WOS+SYEAR, data=subset(atSea, Sex==2 & Size>mls) ,na.action=NULL, FUN= sum)	
				nLlf 	= rename.df(nLlf ,'I','NFemalelegalLobster')
				
				nLm 	= aggregate(I~GRID_NUM+WOS+SYEAR, data=subset(atSea, Sex==1) ,na.action=NULL, FUN= sum)	
				nLm 	= rename.df(nLm ,'I','NMaleLobster')			

				nLsm 	= aggregate(I~GRID_NUM+WOS+SYEAR, data=subset(atSea, Sex==1 & Size<=mls) ,na.action=NULL, FUN= sum)	
				nLsm 	= rename.df(nLsm ,'I','NmaleShortLobster')

				nLlm 	= aggregate(I~GRID_NUM+WOS+SYEAR, data=subset(atSea, Sex==1 & Size>mls) ,na.action=NULL, FUN= sum)	
				nLlm 	= rename.df(nLlm ,'I','NmalelegalLobster')
				
				nLb		= try(aggregate(Berried~GRID_NUM+WOS+SYEAR, data=subset(atSea) ,na.action=NULL, FUN= sum),silent=T)	
				if(!is.null(nrow(nLb))) {nLb 	= rename.df(nLb ,'Berried','NBerriedLobster')} else {nLb = trt[,c('GRID_NUM','WOS','SYEAR')]; nLb$NBerriedLobster = 0} 
				
				nLv     = try(aggregate(V_NOTCHED~GRID_NUM+WOS+SYEAR, data=atSea ,na.action=NULL, FUN= sum),silent=T)	
				if(!is.null(nrow(nLv))) {nLv 	= rename.df(nLv ,'V_NOTCHED','NVnotchedLobster')} else {nLv = trt[,c('GRID_NUM','WOS','SYEAR')]; nLv$NVnotchedLobster = 0}
			
			trSum = Reduce(function(...) merge(...,all=T),list(trt,nL,nLm,nLf,nLsf,nLlf,nLsm,nLlm,nLb,nLv))
			
trSum = na.zero(trSum)


	aS = atSea
	if(females.only) aS = subset(aS,SEX %in% c(2))

	aS$ids = paste(aS$GRID_NUM,aS$WOS, aS$SYEAR,sep="-")
	IDs = unique(aS$ids)
	breaks = c(4,5,6,7,8,9,10,10.5,11,12,13,14,15)
	aS = subset(aS,Size %in% breaks)
	
	CLF = list()
			   for(i in 1:length(IDs)){
            			CLF[[i]]<-with(subset(aS,ids==IDs[i]), {
            				lpo = rep(Size,times=I)	
            				g = c(IDs[i],hist(lpo,breaks = breaks,plot=F)$counts)
            			return(g)
            			})
        		}
	
			CLF = as.data.frame(do.call(rbind,CLF))
			names(CLF) <- c('ids',breaks[-1])
			CLF = toNums(CLF,2:ncol(CLF))
			CLF = cbind(do.call(rbind,strsplit(CLF$ids,"-")),CLF)
			names(CLF)[1:3] = c('GRID_NUM','WOS','SYEAR')
			aCC = CLF = merge(CLF,trSum)

		if(nrow(comGridCont)==0) cg = aggregate(PropLand~WOS+GRID_NUM,data=comGridHist,FUN=sum)
		if(nrow(comGridCont)>0)  cg = aggregate(PropLand~WOS+GRID_NUM+SYEAR,data=comGridCont,FUN=sum)
	
	 
		CC = merge(CLF,cg,all.x=T)
			ii = is.na(CC$PropLand)
		if(sum(!ii*1)/length(ii)<0.6) cg = aggregate(PropLand~WOS+GRID_NUM,data=comGridHist,FUN=sum)
		
		CC = merge(CLF,cg,all.x=T)

	if(any(is.na(CC$PropLand))) {
			v = which(is.na(CC$PropLand))
				for(l in v){
						if(!is.na(CC$WOS[l]) & is.na(CC$GRID_NUM[l])) CC$PropLand[l] = median(cg$PropLand[cg$WOS==CC$WOS[l]],na.rm=T)
						if(is.na(CC$WOS[l]) & !is.na(CC$GRID_NUM[l])) CC$PropLand[l] = median(cg$PropLand[cg$GRID_NUM==CC$GRID_NUM[l]],na.rm=T)
						if((is.na(CC$WOS[l]) &  is.na(CC$GRID_NUM[l])) |( !is.na(CC$WOS[l]) &  !is.na(CC$GRID_NUM[l]))) CC$PropLand[l] = median(cg$PropLand,na.rm=T)
						if(CC$GRID_NUM[l] %ni% cg$GRID_NUM) CC$PropLand[l] = median(cg$PropLand[cg$WOS==CC$WOS[l]],na.rm=T)
						if(CC$WOS[l] %ni% cg$WOS) CC$PropLand[l] = median(cg$PropLand[cg$GRID_NUM==CC$GRID_NUM[l]],na.rm=T)
						if(CC$WOS[l] %ni% cg$WOS & CC$GRID_NUM[l] %ni% cg$GRID_NUM) CC$PropLand[l] = quantile(cg$PropLand,0.25,na.rm=T)
				}
			}
		}


 if(port.samples){
	 atSea = subset(atSea, SYEAR ==year & LFA == lfa)
	if(any( atSea$WOS<0) | any(is.na(atSea$WOS))) { u = which(is.na(atSea$WOS) | atSea$WOS<0); atSea$WOS[u] <- NA}
				atSea$NL = atSea$N_FEM+atSea$N_MALES
				trt 	= aggregate(SAMPLE_SEQ~PORT_CODE+WOS+LFA+SYEAR, data=atSea ,na.action=NULL, FUN= function(x)length (unique(x)))
				trt 	= rename.df(trt,'SAMPLE_SEQ','SAMPLES_BY_PORT')

				nL 		= aggregate(NL~PORT_CODE+WOS+LFA+SYEAR, data=atSea ,na.action=NULL, FUN= sum)	
				nL 	= rename.df(nL ,'NL','NLobster')
				
				nLf 	= aggregate(N_FEM~LFA+PORT_CODE+WOS+SYEAR, data=atSea ,na.action=NULL, FUN= sum)	
				nLf 	= rename.df(nLf ,'N_FEM','NFemaleLobster')
				
				nLm 	= aggregate(N_MALES~LFA+PORT_CODE+WOS+SYEAR, data=atSea,na.action=NULL, FUN= sum)	
				nLm 	= rename.df(nLm ,'N_MALES','NMaleLobster')
			
			trSum = Reduce(function(...) merge(...,all=T),list(trt,nL,nLm,nLf))
			trSum = na.zero(trSum)
	

	
	if(females.only) atSea$NL = atSea$N_FEM
	aS = atSea

	aS$ids = paste(aS$LFA,aS$WOS, aS$SYEAR,aS$PORT_CODE,sep="-")
	aS = aggregate(NL~L_SIZE+ids,data=aS,FUN=sum)
	
	i = which(aS$L_SIZE>=250)
	if(length(i) >0) aS = aS[-i,]
	IDs = unique(aS$ids)
	aS = subset(aS,L_SIZE %in% 70:250)
	breaks = seq(70,250,1)
	br = data.frame(breaks = breaks)
		CLF = list()
		   for(i in 1:length(IDs)){
	    	       	i2 = subset(aS,ids == IDs[i])
	        	    bb = merge(br,i2[,c('L_SIZE','NL')],by.x = 'breaks',by.y = 'L_SIZE',all.x=T)
	            	bb = na.zero(bb)
	            	g = c(IDs[i],bb[,2])
	           		CLF[[i]]<- g
	          		}
		
			CLF = as.data.frame(do.call(rbind,CLF))
				names(CLF) <- c('ids',breaks)
				CLF = toNums(CLF,2:ncol(CLF))
				CLF = cbind(do.call(rbind,strsplit(CLF$ids,"-")),CLF)
				names(CLF)[1:4] = c('LFA','WOS','SYEAR','PORT_CODE')
				aCC = CLF = merge(CLF,trSum)
				CLF = rename.df(CLF,'PORT_CODE','PORT')
		
	
			
#using landings by port or by stat dist
if(year<=2004){
		cg = aggregate(PropLand~LFA+WOS+PORT,data=comGridHist,FUN=sum)
		CC = merge(CLF,cg,all.x=T)
		if(any(is.na(CC$PropLand))) {
			v = which(is.na(CC$PropLand))
				for(l in v){
						if(!is.na(CC$WOS[l]) & is.na(CC$PORT[l])) CC$PropLand[l] = median(cg$PropLand[cg$WOS==CC$WOS[l]],na.rm=T)
						if(is.na(CC$WOS[l]) & !is.na(CC$PORT[l])) CC$PropLand[l] = median(cg$PropLand[cg$PORT==CC$PORT[l]],na.rm=T)
						if((is.na(CC$WOS[l]) &  is.na(CC$PORT[l])) |( !is.na(CC$WOS[l]) &  !is.na(CC$PORT[l]))) CC$PropLand[l] = median(cg$PropLand,na.rm=T)
						if(CC$PORT[l] %ni% cg$PORT) CC$PropLand[l] = median(cg$PropLand[cg$WOS==CC$WOS[l]],na.rm=T)
						if(CC$WOS[l] %ni% cg$WOS) CC$PropLand[l] = median(cg$PropLand[cg$PORT==CC$PORT[l]],na.rm=T)
						if(CC$WOS[l] %ni% cg$WOS & CC$PORT[l] %ni% cg$PORT) CC$PropLand[l] = quantile(cg$PropLand,0.25,na.rm=T)
					}
			}

}

	if(year>2004){
		if(nrow(comGridCont)==0) cg = aggregate(PropLand~LFA+WOS+PORT,data=comGridHist,FUN=sum)
		if(nrow(comGridCont)>0) cg = aggregate(PropLand~LFA+WOS+PORT+SYEAR,data=comGridCont,FUN=sum)
		 
		CC = merge(CLF,cg,all.x=T)
			ii = is.na(CC$PropLand)
		if(sum(!ii*1)/length(ii)<0.6) cg = aggregate(PropLand~WOS+PORT,data=comGridHist,FUN=sum)
		
		CC = merge(CLF,cg,all.x=T)

	if(any(is.na(CC$PropLand))) {
			v = which(is.na(CC$PropLand))
				for(l in v){
						if(!is.na(CC$WOS[l]) & is.na(CC$PORT[l])) CC$PropLand[l] = median(cg$PropLand[cg$WOS==CC$WOS[l]],na.rm=T)
						if(is.na(CC$WOS[l]) & !is.na(CC$PORT[l])) CC$PropLand[l] = median(cg$PropLand[cg$PORT==CC$PORT[l]],na.rm=T)
						if((is.na(CC$WOS[l]) &  is.na(CC$PORT[l])) |( !is.na(CC$WOS[l]) &  !is.na(CC$PORT[l]))) CC$PropLand[l] = median(cg$PropLand,na.rm=T)
						if(CC$PORT[l] %ni% cg$PORT) CC$PropLand[l] = median(cg$PropLand[cg$WOS==CC$WOS[l]],na.rm=T)
						if(CC$WOS[l] %ni% cg$WOS) CC$PropLand[l] = median(cg$PropLand[cg$PORT==CC$PORT[l]],na.rm=T)
						if(CC$WOS[l] %ni% cg$WOS & CC$PORT[l] %ni% cg$PORT) CC$PropLand[l] = quantile(cg$PropLand,0.25,na.rm=T)
				}
			}

	}
}
if(fsrs.recruit.samples) {
				h = which(atSea$SIZE_CD==mls & atSea$SHORT==0)
				atSea$SIZE_CD[h] = mls+0.5
				breaks = c(na.omit(as.numeric(unique(atSea$SIZE_CD))))	
				breaks = breaks[order(breaks)]
				 atSea = subset(atSea, SYEAR ==year & LFA == lfa)
				if(any( atSea$WOS<0) | any(is.na(atSea$WOS))) { u = which(is.na(atSea$WOS) | atSea$WOS<0); atSea$WOS[u] <- NA}
				atSea = rename.df(atSea,'LFA_GRID','GRID_NUM')
	
				
				trt 	= aggregate(paste(TRAP_NO,RECORD_NUMBER,sep="")~GRID_NUM+WOS+SYEAR, data=atSea ,na.action=NULL, FUN= function(x)length (unique(x)))
				trt 	= rename.df(trt,'paste(TRAP_NO, RECORD_NUMBER, sep = "")','TrapsSampled')
				
				atSea$I <- 1
				nL 		= aggregate(I~GRID_NUM+WOS+SYEAR, data=subset(atSea) ,na.action=NULL, FUN= sum)	
				nL 	= rename.df(nL ,'I','NLobster')
				
				nLf 	= aggregate(I~GRID_NUM+WOS+SYEAR, data=subset(atSea, SEX==2) ,na.action=NULL, FUN= sum)	
				nLf 	= rename.df(nLf ,'I','NFemaleLobster')
				
				nLsf 	= aggregate(I~GRID_NUM+WOS+SYEAR, data=subset(atSea, SEX==2 & SIZE_CD<=mls) ,na.action=NULL, FUN= sum)	
				nLsf 	= rename.df(nLsf ,'I','NFemaleShortLobster')

				nLlf 	= aggregate(I~GRID_NUM+WOS+SYEAR, data=subset(atSea, SEX==2 & SIZE_CD>mls) ,na.action=NULL, FUN= sum)	
				nLlf 	= rename.df(nLlf ,'I','NFemalelegalLobster')
				
				nLm 	= aggregate(I~GRID_NUM+WOS+SYEAR, data=subset(atSea, SEX==1) ,na.action=NULL, FUN= sum)	
				nLm 	= rename.df(nLm ,'I','NMaleLobster')			

				nLsm 	= aggregate(I~GRID_NUM+WOS+SYEAR, data=subset(atSea, SEX==1 & SIZE_CD<=mls) ,na.action=NULL, FUN= sum)	
				nLsm 	= rename.df(nLsm ,'I','NmaleShortLobster')

				nLlm 	= aggregate(I~GRID_NUM+WOS+SYEAR, data=subset(atSea, SEX==1 & SIZE_CD>mls) ,na.action=NULL, FUN= sum)	
				nLlm 	= rename.df(nLlm ,'I','NmalelegalLobster')
				
				nLb		= try(aggregate(I~GRID_NUM+WOS+SYEAR, data=subset(atSea, SEX==3) ,na.action=NULL, FUN= sum),silent=T)	
				if(!is.null(nrow(nLb))) {nLb 	= rename.df(nLb ,'I','NBerriedLobster')} else {nLb = trt[,c('GRID_NUM','WOS','SYEAR')]; nLb$NBerriedLobster = 0} 
				
				nLv     = try(aggregate(V_NOTCHED~GRID_NUM+WOS+SYEAR, data=atSea ,na.action=NULL, FUN= sum),silent=T)	
				if(!is.null(nrow(nLv))) {nLv 	= rename.df(nLv ,'V_NOTCHED','NVnotchedLobster')} else {nLv = trt[,c('GRID_NUM','WOS','SYEAR')]; nLv$NVnotchedLobster = 0}
			
			trSum = Reduce(function(...) merge(...,all=T),list(trt,nL,nLm,nLf,nLsf,nLlf,nLsm,nLlm,nLb,nLv))
			trSum = na.zero(trSum)
	

	aS = aggregate(I~GRID_NUM+WOS+SYEAR+SIZE_CD,data=atSea,FUN=sum)
	if(females.only) aS = subset(aS,SEX %in% c(2))

	aS$ids = paste(aS$GRID_NUM,aS$WOS, aS$SYEAR,sep="-")
	IDs = unique(aS$ids)
	aS = subset(aS,SIZE_CD %in% breaks)
	CLF = list()
			   for(i in 1:length(IDs)){
            			CLF[[i]]<-with(subset(aS,ids==IDs[i]), {
            			lpo = rep(SIZE_CD,times=I)	
            			g = c(IDs[i],hist(lpo,breaks = breaks,plot=F)$counts)
            			return(g)
            			})
        		}
	
			CLF = as.data.frame(do.call(rbind,CLF))
			names(CLF) <- c('ids',breaks[-1])
			CLF = toNums(CLF,2:ncol(CLF))
			CLF = cbind(do.call(rbind,strsplit(CLF$ids,"-")),CLF)
			names(CLF)[1:3] = c('GRID_NUM','WOS','SYEAR')
			aCC = CLF = merge(CLF,trSum)

		if(nrow(comGridCont)==0) cg = aggregate(PropLand~WOS+GRID_NUM,data=comGridHist,FUN=sum)
		if(nrow(comGridCont)>0)  cg = aggregate(PropLand~WOS+GRID_NUM+SYEAR,data=comGridCont,FUN=sum)
	
	 
		CC = merge(CLF,cg,all.x=T)
		ii = is.na(CC$PropLand)
		if(sum(!ii*1)/length(ii)<0.6) cg = aggregate(PropLand~WOS+GRID_NUM,data=comGridHist,FUN=sum)
		
		CC = merge(CLF,cg,all.x=T)
			
	if(any(is.na(CC$PropLand))) {
			v = which(is.na(CC$PropLand))
				for(l in v){
						if(!is.na(CC$WOS[l]) & is.na(CC$GRID_NUM[l])) CC$PropLand[l] = median(cg$PropLand[cg$WOS==CC$WOS[l]],na.rm=T)
						if(is.na(CC$WOS[l]) & !is.na(CC$GRID_NUM[l])) CC$PropLand[l] = median(cg$PropLand[cg$GRID_NUM==CC$GRID_NUM[l]],na.rm=T)
						if((is.na(CC$WOS[l]) &  is.na(CC$GRID_NUM[l])) |( !is.na(CC$WOS[l]) &  !is.na(CC$GRID_NUM[l]))) CC$PropLand[l] = median(cg$PropLand,na.rm=T)
						if(CC$GRID_NUM[l] %ni% cg$GRID_NUM) CC$PropLand[l] = median(cg$PropLand[cg$WOS==CC$WOS[l]],na.rm=T)
						if(CC$WOS[l] %ni% cg$WOS) CC$PropLand[l] = median(cg$PropLand[cg$GRID_NUM==CC$GRID_NUM[l]],na.rm=T)
						if(CC$WOS[l] %ni% cg$WOS & CC$GRID_NUM[l] %ni% cg$GRID_NUM) CC$PropLand[l] = quantile(cg$PropLand,0.25,na.rm=T)
				}
			}
		}

	if(nrow(CC)>0) {
		CC = subset(CC,NLobster>=15)
	CC$LFA = lfa
		return(CC)
	}
	}
