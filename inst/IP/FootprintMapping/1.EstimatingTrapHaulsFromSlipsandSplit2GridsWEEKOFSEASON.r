#
require(devtools)
require(bio.lobster)
require(bio.utilities)
require(sf)
load_all('C:/Users/Cooka/Documents/git/bio.utilities')


la()
wd = ('C:\\Users\\Cooka\\OneDrive - DFO-MPO\\CanUsCollabEffortMapping')
setwd(wd)



layerDir=file.path(project.datadirectory("bio.lobster"), "data","maps")
r<-readRDS(file.path( layerDir,"GridPolysSF.rds"))
r = st_as_sf(r)

a =  lobster.db('process.logs')
a = subset(a,SYEAR>2004 & SYEAR<2025)
b = lobster.db('seasonal.landings')
b = subset(b,!is.na(SYEAR))
b$SYEAR = 1976:2024
b$LFA38B <- NULL
b = subset(b,SYEAR>2004 & SYEAR<2025)
b = reshape(b,idvar='SYEAR', varying=list(2:6),direction='long')
b$LFA=rep(c(33,34,35,36,38),each=20)
b$time <- NULL
names(b)[1:2]=c('YR','SlipLand')


d = lobster.db('annual.landings')
d = subset(d,YR>2004 & YR<2025, select=c(YR,LFA27,LFA28,LFA29,LFA30,LFA31A,LFA31B,LFA32))
d = reshape(d,idvar='YR', varying=list(2:8),direction='long')
d$LFA=rep(c(27,28,29,30,'31A','31B',32),each=20)
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

#pe = merge(partEffort,r,by.x=c('GRID_NUM','LFA'),by.y=c('GRID_NO','LFA'))

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
g = subset(g,SYEAR>2004 & SYEAR<2025)

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

##annual

ann = aggregate(cbind(TrapHauls,Landings,Trips)~LFA+FishingYear,data=Tot,FUN=sum)
ann = subset(ann,LFA %ni% 28)
saveRDS(ann,file='TrapHauls_Landings_Trips_annual.rds')
g = lobster.db('licence_categories')
g = subset(g,LFA %ni% c('38B',41))
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
# PartitionLandings to Grids

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
##Licenses By Grid and Week

g = lobster.db('process.logs')
g = subset(g,SYEAR>2004 & SYEAR<2023)

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

########################
#Price Data


pr = read.csv(file.path(project.datadirectory('bio.lobster'),'data','PriceData','LOBSTER_1995-2021Price.csv'))
pr$Price = pr$Total.Catch.Value/pr$Total.Round.Weight.Kgs
pr = pr[which(is.finite(pr$Price)),]
pr = pr[which(!is.na(pr$Price)),]
pr = subset(pr,Price >3 & Price < 30)
pra = aggregate(Price~Date+Area.Fished+Community.Code,data=pr,FUN=mean)
pra = subset(pra,Date>'2004-10-01')

pra$yr = year(pra$Date)
pra$mn =month(pra$Date)
pra = pra[order(pra$Date),]
pra$syr = ifelse(pra$Area.Fished %in% c('33','34','35','36','38') & pra$mn %in% c(10,11,12), pra$yr+1,pra$yr )


rm.from.list<-function(list1) {
  a<-dim_list(list1)
  if(any(a[,1]==0)) {
    list1<-list1[-which(a[,1]==0)]
  }
  return(list1)
}

lo = lobster.db('process.logs.unfiltered')
lo = aggregate(cbind(DATE_FISHED,DOS)~SYEAR+LFA,data=subset(lo,SYEAR>2004),FUN=min)
lo$D = as.Date(lo$DATE_FISHED,origin=as.Date('1970-01-01'))
lo = subset(lo, DOS==1 & LFA %ni% '28')
ul = unique(lo$LFA)

pra = subset(pra,Area.Fished %in% ul)

pras = split(pra,f=list(pra$Area.Fished,pra$syr))
pras = rm.from.list(pras)
pras1 = list()
m=0
for(i in 1:length(pras)){
  u = pras[[i]]
  u$Date = as.Date(u$Date)
  jj = unique(u$Area.Fished)
  yy = unique(u$syr)
  if(jj %in% ul){
    lll = subset(lo,LFA==jj & SYEAR==yy)
    if(nrow(lll)==1){
      m=m+1
      u = subset(u,Date>=lll$D)
      uu = aggregate(Price~Date+Area.Fished,data=u,FUN=mean)  
      uu$DOS = as.numeric(uu$Date-min(uu$Date)+1 )
      pras1[[m]] = uu
    }   
  }
}

oo = as.data.frame(do.call(rbind,pras1))
oo$SY = year(oo$Date)
