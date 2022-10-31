#
require(bio.lobster)
require(bio.utilities)
require(devtools)
load_all('C:/Users/Cooka/Documents/git/bio.utilities')
la()
wd = ('C:\\Users\\Cooka\\OneDrive - DFO-MPO\\KMKNO Data')

setwd(wd)

a =  lobster.db('process.logs')
a = subset(a,SYEAR %in% 2018:2022)
b = lobster.db('seasonal.landings')
b$SYEAR = 1976:2023
b$LFA38B <- NULL
b = subset(b,SYEAR>2004 & SYEAR<=2022)
b = reshape(b[,c(1:4)],idvar='SYEAR', varying=list(2:4),direction='long')
b$LFA=rep(c(33,34,35),each=18)
b$time <- NULL
names(b)[1:2]=c('YR','SlipLand')
bd = b

bup = aggregate(cbind(WEIGHT_KG,NUM_OF_TRAPS)~SYEAR+LFA,data=subset(a,LFA %in% 33:35),FUN=sum)
bup$CPUE = bup$WEIGHT_KG/bup$NUM_OF_TRAPS
bAll = merge(bd,bup,by.x=c('YR','LFA'),by.y=c('SYEAR','LFA'))

a=subset(a, LFA %in% 33:35)
sL= split(a,f=list(a$LFA, a$SYEAR))
sL = rm.from.list(sL)
cpue.lst<-list()
cpue.ann = list()

biasCorrectedRatio=T
if(biasCorrectedRatio){
  for(i in 1:length(sL)){
    tmp<-sL[[i]]
    tmp = tmp[,c('DATE_FISHED','WEIGHT_KG','NUM_OF_TRAPS')]
    names(tmp)<-c('time','catch','effort')
    tmp$date<-as.Date(tmp$time)
    first.day<-min(tmp$date)
    tmp$time<-julian(tmp$date,origin=first.day-1)
    tmp$time = floor(tmp$time/7)+1
    g<-biasCorrCPUE(tmp,by.time = F)
    cpue.lst[[i]] <- c(g,lfa=unique(sL[[i]]$LFA),yr = unique(sL[[i]]$SYEAR))
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
    tmp$date<-as.Date(tmp$DATE_FISHED)
    first.day<-min(tmp$date)
    tmp$time<-julian(tmp$date,origin=first.day-1)
    tmp$WOS = floor(tmp$time/7)+1
    
    tTH = aggregate(NUM_OF_TRAPS~LFA+SYEAR,data=tmp,FUN=sum)
    tC = subset(cAll, LFA==unique(tmp$LFA) & YR == unique(tmp$SYEAR)) 
    pTH = aggregate(NUM_OF_TRAPS~WOS+LFA+SYEAR,data=tmp,FUN=sum)
    pTH$BTTH = pTH$NUM_OF_TRAPS / tTH$NUM_OF_TRAPS * tC$NTRAPs
    pTH$BlTH = pTH$NUM_OF_TRAPS / tTH$NUM_OF_TRAPS * tC$NTRAPSL
    pTH$BuTH = pTH$NUM_OF_TRAPS / tTH$NUM_OF_TRAPS * tC$NTRAPSU
   
    partEffort[[i]] = pTH
}

partEffort = do.call(rbind, partEffort)
partEffort = subset(partEffort,select=c(LFA,SYEAR,WOS,BTTH))



  for(i in 1:length(sL)){
    tmp<-sL[[i]]
    tmp = tmp[,c('DATE_FISHED','WEIGHT_KG','NUM_OF_TRAPS','VR_NUMBER')]
    names(tmp)<-c('time','catch','effort','BOAT')
    tmp$date<-as.Date(tmp$time)
    first.day<-min(tmp$date)
    tmp$time<-julian(tmp$date,origin=first.day-1)
    tmp$time = floor(tmp$time/7)+1
    g<-biasCorrCPUE(tmp,by.time = T)
    g = subset(g,select=c(t,unBCPUE))
    g1 = aggregate(cbind(catch,effort)~time+BOAT,data=tmp,FUN=sum)
    g1$CPUE = g1$catch / g1$effort
    g2 = (aggregate(CPUE~time,data=g1,FUN= function(x) quantile(x,c(0.025))))
    g3 = (aggregate(CPUE~time,data=g1,FUN= function(x) quantile(x,c(0.975))))
    names(g3)=c('t','Upper')
    names(g2)=c('t','Lower')
    g23 = merge(g2,g3)
    g = merge(g,g23)
    cpue.lst[[i]] <- data.frame(g,lfa=unique(sL[[i]]$LFA),yr = unique(sL[[i]]$SYEAR))
  }
  
  cc =as.data.frame(do.call(rbind,cpue.lst))

out = merge(partEffort,cc,by.x=c('LFA','SYEAR','WOS'),by.y=c('lfa','yr','t'))



write.csv(out,'CPUEEffortByWeekOfSeason.csv')
