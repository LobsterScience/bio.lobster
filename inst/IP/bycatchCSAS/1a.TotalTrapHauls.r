#
require(bio.lobster)
require(bio.utilities)
load_all('C:/Users/Cooka/Documents/git/bio.utilities')

wd = ('C:\\Users\\Cooka\\OneDrive - DFO-MPO\\BycatchLobster')

setwd(wd)

a =  bycatch.db('logbook.merge')
a = subset(a,SYEAR>2004)
b = lobster.db('seasonal.landings')
b = b[30:47,]
b$LFA38B <- NULL
b$SYEAR = 2005:2022
b = subset(b,SYEAR<2022)
b = reshape(b,idvar='SYEAR', varying=list(2:6),direction='long')
b$LFA=rep(c(33,34,35,36,38),each=17)
b$time <- NULL
names(b)[1:2]=c('YR','SlipLand')


d = lobster.db('annual.landings')
d = subset(d,YR>2004, select=c(YR,LFA27,LFA28,LFA29,LFA30,LFA31A,LFA31B,LFA32))
d = reshape(d,idvar='YR', varying=list(2:8),direction='long')
d$LFA=rep(c(27,28,29,30,'31A','31B',32),each=17)
d$time <- NULL
names(d)[1:2]=c('YR','SlipLand')
bd = rbind(d,b)

bup = aggregate(cbind(WEIGHT_KG,NUM_OF_TRAPS)~SYEAR+LFA,data=a,FUN=sum)
bup$CPUE = bup$WEIGHT_KG/bup$NUM_OF_TRAPS
bAll = merge(bd,bup,by.x=c('YR','LFA'),by.y=c('SYEAR','LFA'))

sL= split(a,f=list(a$LFA, a$SYEAR))
sL = rm.from.list(sL)
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


cAll = merge(bAll,cc,by.x=c('LFA','YR'),by.y=c('lfa','yr'))

cAll$NTRAPs = cAll$SlipLand*1000/as.numeric(cAll$unBCPUE)
cAll$NTRAPSU = cAll$SlipLand*1000/as.numeric(cAll$l95)
cAll$NTRAPSL = cAll$SlipLand*1000/as.numeric(cAll$u95)

##########################################
#partion effort in cAll based on elements in sL

partEffort = list()

for(i in 1:length(sL)){
    tmp = sL[[i]]
    tTH = aggregate(NUM_OF_TRAPS~LFA,data=tmp,FUN=sum)
    tC = subset(cAll, LFA==unique(tmp$LFA) & YR == unique(tmp$SYEAR)) 
    pTH = aggregate(NUM_OF_TRAPS~GridGroup+Period+LFA+SYEAR,data=tmp,FUN=sum)
    pTH$BTTH = pTH$NUM_OF_TRAPS / tTH$NUM_OF_TRAPS * tC$NTRAPs
    pTH$BlTH = pTH$NUM_OF_TRAPS / tTH$NUM_OF_TRAPS * tC$NTRAPSL
    pTH$BuTH = pTH$NUM_OF_TRAPS / tTH$NUM_OF_TRAPS * tC$NTRAPSU
   
    partEffort[[i]] = pTH
}

partEffort = do.call(rbind, partEffort)

saveRDS(partEffort,'results/BumpedUpEffortByGridGroup.rds')
