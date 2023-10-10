#
require(devtools)
require(bio.lobster)
require(bio.utilities)
require(dplyr)
load_all('C:/Users/Cooka/Documents/git/bio.utilities')
la()
wd = ('C:\\Users\\Cooka\\OneDrive - DFO-MPO\\SWNBOFTechReport')

setwd(wd)

a =  lobster.db('process.logs')
a = subset(a,SYEAR>2004)
b = lobster.db('seasonal.landings')
b$LFA38B <- NULL
b$SYEAR = substr(b$SYEAR,6,9)
b = subset(b,!is.na(SYEAR))
b = lobster.db('percent_reporting')
b = b %>% gather(key='LFA',value='Landings',-SYEAR)
b$LFA = substr(b$LFA, 4,8)
names(b)=c('YR','LFA','SlipLand')
bd = b


bup = aggregate(cbind(WEIGHT_KG,NUM_OF_TRAPS)~SYEAR+LFA,data=a,FUN=sum)
bup$CPUE = bup$WEIGHT_KG/bup$NUM_OF_TRAPS
bAll = merge(bd,bup,by.x=c('YR','LFA'),by.y=c('SYEAR','LFA'))

a = subset(a,LFA %in% c(33:38))
sL= split(a,f=list(a$LFA, a$SYEAR))
sL = rm.from.list(sL)
cpue.lst<-list()
cpue.ann = list()

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

cAll = merge(bAll,cc,by.x=c('LFA','YR'),by.y=c('lfa','yr'))

cAll$NTRAPs = cAll$SlipLand*1000/as.numeric(cAll$unBCPUE)
cAll$NTRAPSU = cAll$SlipLand*1000/as.numeric(cAll$l95)
cAll$NTRAPSL = cAll$SlipLand*1000/as.numeric(cAll$u95)

saveRDS(cAll,file='TrapHaulsLandingCPUEAllLFAs.rds')

###########################################
#part the effort to grids

partEffort = list()

for(i in 1:length(sL)){
  tmp = sL[[i]]
  tTH = aggregate(NUM_OF_TRAPS~LFA,data=tmp,FUN=sum)
  tC = subset(cAll, LFA==unique(tmp$LFA) & YR == unique(tmp$SYEAR)) 
  pTH = aggregate(NUM_OF_TRAPS~GRID_NUM+WOS+LFA+SYEAR,data=tmp,FUN=sum)
  pTH$BTTH = pTH$NUM_OF_TRAPS / tTH$NUM_OF_TRAPS * tC$NTRAPs
  pTH$BlTH = pTH$NUM_OF_TRAPS / tTH$NUM_OF_TRAPS * tC$NTRAPSL
  pTH$BuTH = pTH$NUM_OF_TRAPS / tTH$NUM_OF_TRAPS * tC$NTRAPSU
  
  partEffort[[i]] = pTH
}

partEffort = do.call(rbind, partEffort)
saveRDS(partEffort,'DiscretizedData/TrapHaulsWithinGridandWeek.rds')


#############################################
# PartitionLandings to Grids

partLandings = list()

for(i in 1:length(sL)){
  tmp = sL[[i]]
  tTH = aggregate(WEIGHT_KG~LFA,data=tmp,FUN=sum)
  tC = subset(cAll, LFA==unique(tmp$LFA) & YR == unique(tmp$SYEAR)) 
  pTH = aggregate(WEIGHT_KG~GRID_NUM+WOS+LFA+SYEAR,data=tmp,FUN=sum)
  pTH$BL = pTH$WEIGHT_KG / (tTH$WEIGHT_KG )* (tC$SlipLand*1000)
  partLandings[[i]] = pTH
}

partLandings = do.call(rbind, partLandings)

saveRDS(partLandings,'DiscretizedData/LandingsWithinGridandWeek.rds')

###################################################
##Licenses By Grid and Week

g = lobster.db('process.logs')
gg = aggregate(SD_LOG_ID~LFA+WOS+GRID_NUM+SYEAR,data = g,FUN=function(x) length(unique(x)))
saveRDS(gg,'DiscretizedData/SDLOGSWithinGridandWeek.rds')

#############merge


Tot = merge(merge(partEffort,partLandings),gg)

Tot = subset(Tot,select=c(SYEAR,LFA,GRID_NUM,WOS,BTTH,BL,SD_LOG_ID))
names(Tot)= c('FishingYear','LFA','Grid','WeekofSeason','TrapHauls','Landings','Trips')


sTot = subset(Tot,Trips>4)
saveRDS(sTot,'DiscretizedData/PrivacyScreened_TrapHauls_Landings_Trips_GridandWeek.rds')



Tot$W2 = ceiling(Tot$WeekofSeason/2)*2
Tot2 =aggregate(cbind(TrapHauls,Landings,Trips)~FishingYear+LFA+Grid+W2,data=Tot,FUN=sum)
sTot = subset(Tot2,Trips>4)
saveRDS(sTot,'DiscretizedData/PrivacyScreened_TrapHauls_Landings_Trips_GridandTwoWeekIntervals.rds')


##############################################################################################
#by Port#

partEffortCC = list()

for(i in 1:length(sL)){
  tmp = sL[[i]]
  tTH = aggregate(NUM_OF_TRAPS~LFA,data=tmp,FUN=sum)
  tC = subset(cAll, LFA==unique(tmp$LFA) & YR == unique(tmp$SYEAR)) 
  pTH = aggregate(NUM_OF_TRAPS~COMMUNITY_CODE+WOS+LFA+SYEAR,data=tmp,FUN=sum)
  pTH$BTTH = pTH$NUM_OF_TRAPS / tTH$NUM_OF_TRAPS * tC$NTRAPs
  pTH$BlTH = pTH$NUM_OF_TRAPS / tTH$NUM_OF_TRAPS * tC$NTRAPSL
  pTH$BuTH = pTH$NUM_OF_TRAPS / tTH$NUM_OF_TRAPS * tC$NTRAPSU
  
  partEffortCC[[i]] = pTH
}

partEffortCC = do.call(rbind, partEffortCC)
saveRDS(partEffortCC,'DiscretizedData/TrapHaulsWithinCommunityCodeandWeek.rds')


#############################################
# PartitionLandings to port

partLandingsCC = list()

for(i in 1:length(sL)){
  tmp = sL[[i]]
  tTH = aggregate(WEIGHT_KG~LFA,data=tmp,FUN=sum)
  tC = subset(cAll, LFA==unique(tmp$LFA) & YR == unique(tmp$SYEAR)) 
  pTH = aggregate(WEIGHT_KG~COMMUNITY_CODE+WOS+LFA+SYEAR,data=tmp,FUN=sum)
  pTH$BL = pTH$WEIGHT_KG / (tTH$WEIGHT_KG )* (tC$SlipLand*1000)
  partLandingsCC[[i]] = pTH
}

partLandingsCC = do.call(rbind, partLandingsCC)

saveRDS(partLandingsCC,'DiscretizedData/LandingsWithinCommunityandWeek.rds')

###################################################
##Licenses By port and Week

g = lobster.db('process.logs')
g = subset(g,SYEAR>2004 )

gg = aggregate(SD_LOG_ID~LFA+WOS+COMMUNITY_CODE+SYEAR,data = g,FUN=function(x) length(unique(x)))

saveRDS(gg,'DiscretizedData/SDLOGSWithinCommunityandWeek.rds')

#############merge


TotCC = merge(merge(partEffortCC,partLandingsCC),gg)

TotCC = subset(TotCC,select=c(SYEAR,LFA,COMMUNITY_CODE,WOS,BTTH,BL,SD_LOG_ID))
names(TotCC)= c('FishingYear','LFA','Community','WeekofSeason','TrapHauls','Landings','Trips')


sTotCC = subset(TotCC,Trips>4)
saveRDS(sTot,'DiscretizedData/PrivacyScreened_TrapHauls_Landings_Trips_ComunityandWeek.rds')



TotCC$W2 = ceiling(TotCC$WeekofSeason/2)*2
TotCC2 =aggregate(cbind(TrapHauls,Landings,Trips)~FishingYear+LFA+Community+W2,data=TotCC,FUN=sum)
sTot = subset(Tot2,Trips>4)
saveRDS(sTot,'DiscretizedData/PrivacyScreened_TrapHauls_Landings_Trips_CommunityandTwoWeek.rds')

