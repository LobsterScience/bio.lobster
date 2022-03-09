#
require(bio.lobster)
require(bio.utilities)


wd = ('C:/Users/CookA/Desktop/dellshared/Bycatch in the Lobster Fishery')
setwd(wd)

a =  readRDS(file=file.path(wd,'data/logbookReadyFILTERED.rds'))
b = lobster.db('seasonal.landings')
b = subset(b,SYEAR %in% c('2018-2019','2019-2020','2020-2021'), select=c('SYEAR','LFA33','LFA34','LFA35'))
b$SYEAR = 2019:2021
b = reshape(b,idvar='SYEAR', varying=list(2:4),direction='long')
b$LFA=rep(c(33,34,35),each=3)
b$time <- NULL
names(b)[2]='SlipLand'
bup = aggregate(cbind(WEIGHT_KG,NUM_OF_TRAPS)~SYEAR+LFA,data=a,FUN=sum)
bup$CPUE = bup$WEIGHT_KG/bup$NUM_OF_TRAPS
bAll = merge(b,bup)

#block bootstrap CPUE
sL= split(a,f=list(a$LFA, a$SYEAR))
cpue.lst<-list()
cpue.ann = list()
#if using blocked approach ##
blocking=F
if(blocking){
    for(i in 1:length(sL)){
      tmp<-sL[[i]]
      tmp = tmp[,c('DATE_FISHED','WEIGHT_KG','NUM_OF_TRAPS')]
      names(tmp)<-c('time','catch','effort')
      tmp$date<-as.Date(tmp$time)
      first.day<-min(tmp$date)
      tmp$time<-julian(tmp$date,origin=first.day-1)
      g<-blockedBootstrap(tmp,block.center = 14,block.range = 7)
      cpue.lst[[i]] <- c(lfa=unique(sL[[i]]$LFA),yr = unique(sL[[i]]$SYEAR),g)
    }
    
    cc =as.data.frame(do.call(rbind,cpue.lst))
}

biasCorrectedRatio=T
if(biasCorrectedRatio){
  for(i in 1:length(sL)){
    tmp<-sL[[i]]
    tmp = tmp[,c('DATE_FISHED','WEIGHT_KG','NUM_OF_TRAPS')]
    names(tmp)<-c('time','catch','effort')
    tmp$date<-as.Date(tmp$time)
    first.day<-min(tmp$date)
    tmp$time<-julian(tmp$date,origin=first.day-1)
    g<-biasCorrCPUE(tmp)
    cpue.lst[[i]] <- c(lfa=unique(sL[[i]]$LFA),yr = unique(sL[[i]]$SYEAR),g)
  }
  
  cc =as.data.frame(do.call(rbind,cpue.lst))
}


cAll = merge(bAll,cc,by.x=c('LFA','SYEAR'),by.y=c('lfa','yr'))

cAll$NTRAPs = cAll$SlipLand*1000/as.numeric(cAll$unBCPUE)
cAll$NTRAPSU = cAll$SlipLand*1000/as.numeric(cAll$l95)
cAll$NTRAPSL = cAll$SlipLand*1000/as.numeric(cAll$u95)

##########################################
#partion effort in cAll based on elements in sL

partEffort = list()

for(i in 1:length(sL)){
    tmp = sL[[i]]
    tTH = aggregate(NUM_OF_TRAPS~LFA,data=tmp,FUN=sum)
    tC = subset(cAll, LFA==unique(tmp$LFA) & SYEAR == unique(tmp$SYEAR)) 
    pTH = aggregate(NUM_OF_TRAPS~GRID_NUM+GridGroup+DATE_FISHED+Period,data=tmp,FUN=sum)
    pTH$BTTH = pTH$NUM_OF_TRAPS / tTH$NUM_OF_TRAPS * tC$NTRAPs
    pTH$BlTH = pTH$NUM_OF_TRAPS / tTH$NUM_OF_TRAPS * tC$NTRAPSL
    pTH$BuTH = pTH$NUM_OF_TRAPS / tTH$NUM_OF_TRAPS * tC$NTRAPSU
    
    sTH =   aggregate(NUM_OF_TRAPS~GridGroup+Period,data=tmp,FUN=sum)
    sTH$BTTH = sTH$NUM_OF_TRAPS / tTH$NUM_OF_TRAPS * tC$NTRAPs
    sTH$BlTH = sTH$NUM_OF_TRAPS / tTH$NUM_OF_TRAPS * tC$NTRAPSL
    sTH$BuTH = sTH$NUM_OF_TRAPS / tTH$NUM_OF_TRAPS * tC$NTRAPSU
    
}


