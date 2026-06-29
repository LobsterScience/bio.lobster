require(bio.lobster)
require(SpatialHub)
require(lubridate)
require(bio.utilities)
require(ggplot2)
require(dplyr)
require(sf)
require(PBSmapping)
require(grid)
require(patchwork)
require(tidyr)

p = bio.lobster::load.environment()

#amc updated May 13 2026
#la()

#adjust as required
assessment.year = "2025" 

figdir = file.path(project.datadirectory("bio.lobster","requests","mpa.group",assessment.year))
dir.create( figdir, recursive = TRUE, showWarnings = FALSE )
setwd(figdir)

layerDir=file.path(code_root,"bio.lobster.data", "mapping_data")

# update data through ROracle
NewDataPull =F
if(NewDataPull){
  lobster.db('fsrs.redo')
  lobster.db('logs.redo')
  lobster.db('annual.landings.redo')
  #lobster.db('vlog.redo') #These are static now, no need to update
  logs=lobster.db('season.dates.redo') #updates season dates as required
  logs=lobster.db('process.logs.redo')
  per.rec= lobster.db("percent_reporting")
}



# Fishery footprint-
#------------------------------------------------------------
r = readRDS(file.path(project.datadirectory('bio.lobster'),'data','maps','LFA27-38GridsPrunedtoDepth-sf.rds'))

#r<-readRDS(file.path( layerDir,"GridPolys_DepthPruned_37Split.rds"))
r = st_as_sf(r)

a =  lobster.db('process.logs')
a = subset(a,SYEAR>=2010 & SYEAR<=assessment.year) #subsetting data to year specified above
b = lobster.db('seasonal.landings')

b = subset(b <- b[b$SYEAR != "2025-2026", ])

b$SYEAR = 1976:assessment.year
b$LFA38B <- NULL
b = subset(b,SYEAR>2004 & SYEAR<=assessment.year)
b = reshape(b,idvar='SYEAR', varying=list(2:6),direction='long')
num.yr=length(2005:assessment.year)
b$LFA=rep(c(33,34,35,36,38),each=num.yr)
b$time <- NULL
names(b)[1:2]=c('YR','SlipLand')


d = lobster.db('annual.landings')
d = subset(d,YR>2004 & YR<=assessment.year, select=c(YR,LFA27,LFA28,LFA29,LFA30,LFA31A,LFA31B,LFA32))
d = reshape(d,idvar='YR', varying=list(2:8),direction='long')
d$LFA=rep(c(27,28,29,30,'31A','31B',32),each=num.yr)
d$time <- NULL
names(d)[1:2]=c('YR','SlipLand')
bd = rbind(d,b)

bup = aggregate(cbind(WEIGHT_KG,NUM_OF_TRAPS)~SYEAR+LFA,data=a,FUN=sum)
bup$CPUE = bup$WEIGHT_KG/bup$NUM_OF_TRAPS
bAll = merge(bd,bup,by.x=c('YR','LFA'),by.y=c('SYEAR','LFA'))

sL= split(a,f=list(a$LFA, a$SYEAR))
sL = rm.from.list2(sL)
cpue.lst<-list()
cpue.ann = list()

for(i in 1:length(sL)){
  tmp<-sL[[i]]
  tmp = tmp[,c('DATE_FISHED','WEIGHT_KG','NUM_OF_TRAPS')]
  names(tmp)<-c('time','catch','effort')
  tmp$date<-as.Date(tmp$time)
  first.day<-min(tmp$date)
  tmp$time<-julian(tmp$date,origin=first.day-1)
  g<-biasCorrCPUE(tmp,by.time = F)
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
  pTH = aggregate(NUM_OF_TRAPS~GRID_NUM+LFA+SYEAR,data=tmp,FUN=sum)
  pTH$BTTH = pTH$NUM_OF_TRAPS / tTH$NUM_OF_TRAPS * tC$NTRAPs
  pTH$BlTH = pTH$NUM_OF_TRAPS / tTH$NUM_OF_TRAPS * tC$NTRAPSL
  pTH$BuTH = pTH$NUM_OF_TRAPS / tTH$NUM_OF_TRAPS * tC$NTRAPSU
  
  partEffort[[i]] = pTH
}

partEffort = do.call(rbind, partEffort)

#pe = merge(partEffort,r,by.x=c('GRID_NUM','LFA'),by.y=c('GRID_NO','LFA'))

saveRDS(partEffort,'TrapHaulsWithinGrid.rds')


#############################################
# PartitionLandings to Grids

partLandings = list()

for(i in 1:length(sL)){
  tmp = sL[[i]]
  tTH = aggregate(WEIGHT_KG~LFA,data=tmp,FUN=sum)
  tC = subset(cAll, LFA==unique(tmp$LFA) & YR == unique(tmp$SYEAR)) 
  pTH = aggregate(WEIGHT_KG~GRID_NUM+LFA+SYEAR,data=tmp,FUN=sum)
  pTH$BL = pTH$WEIGHT_KG / (tTH$WEIGHT_KG )* (tC$SlipLand*1000)
  partLandings[[i]] = pTH
}

partLandings = do.call(rbind, partLandings)

saveRDS(partLandings,'LandingsWithinGrid.rds')

###################################################
##Licenses By Grid and Week

g = lobster.db('process.logs')
g = subset(g,SYEAR>2004 & SYEAR<=assessment.year)

gg = aggregate(SD_LOG_ID~LFA+GRID_NUM+SYEAR,data = g,FUN=function(x) length(unique(x)))

saveRDS(gg,'SDLOGSWithinGrid.rds')

#############merge
#Licenses By Grid and Week

g = lobster.db('process.logs')
g = subset(g,SYEAR>2004 & SYEAR<=assessment.year)

gKL = aggregate(LICENCE_ID~LFA+GRID_NUM+SYEAR,data = g,FUN=function(x) length(unique(x)))

saveRDS(gKL,'LicencesWithinCommunity.rds')

#############merge


Tot = merge(merge(merge(partEffort,partLandings),gg),gKL)

Tot = subset(Tot,select=c(SYEAR,LFA,GRID_NUM,BTTH,BL,SD_LOG_ID,LICENCE_ID))
names(Tot)= c('FishingYear','LFA','Grid','TrapHauls','Landings','Trips','NLics')

#-----------
# Following section can privacy screen
#Do not use for sharing within DFO

#Tot$PrivacyScreen = ifelse(Tot$NLics>4,1,0)
#Tot <- Tot[Tot$PrivacyScreen != 0, ] #removes grids with <5 licenses reporting
#saveRDS(Tot,'PrivacyScreened_TrapHauls_Landings_Trips_Gridand.rds')

Tot$LFA = ifelse(Tot$LFA=='31B',312,Tot$LFA)
Tot$LFA = ifelse(Tot$LFA=='31A',311,Tot$LFA)


#making plots of Tot

#GrMap = readRDS(file.path( layerDir,"GridPolys_DepthPruned_37Split.rds"))
GrMap = readRDS(file.path(project.datadirectory('bio.lobster'),'data','maps','LFA27-38GridsPrunedtoDepth-sf.rds'))
coa = st_as_sf(readRDS(file.path( project.datadirectory("bio.lobster"), "data","maps","CoastlineSF_NY_NL.rds")))
GrMap1 = GrMap

GrMap1$area = st_area(GrMap1)/1000000
GrMap1$V2 = paste(GrMap1$LFA, GrMap1$GRID_NO,sep="-")
attr(GrMap1$area, "units") <- "km^2"
st_geometry(GrMap1)<- NULL
gg = aggregate(area~LFA+GRID_NO,data=GrMap1,FUN=function(x) abs(sum(x)))

GrMap2 =merge(GrMap,gg)

gTot = merge(GrMap2,Tot,by.x=c('LFA','GRID_NO'),by.y=c('LFA','Grid'),all.x=T)


gTot=Tot #may be needed, over writes above merge


gTot$CPUE= as.numeric(gTot$Landings)/as.numeric(gTot$TrapHauls)
gTot = subset(gTot, LFA !=37)

g27p = gTot
g27p <- g27p %>%
    mutate(LFA = case_when(
        LFA == "311" ~ "31A",
        LFA == "312" ~ "31B",
        TRUE ~ LFA  # Keep other values unchanged
    ))

#remove unneeded variables for sharing data


g27p=subset(g27p, FishingYear<=2023)

st_geometry(g27p) <- NULL
g27p$V2 <- NULL
names(g27p)=c("LFA", "REPORTING_GRID", "AREA", "FISHING_YEAR", "TRAP_HAULS", "LANDINGS_KG", "TRIPS", "LICENCES_REPORTED", "CPUE_KG_TH" )

g27p=subset(g27p, FishingYear<= assessment.year) #Shouldn't have any data above assessment.year but just in case
g27p <- g27p[, c("LFA", "Grid", "FishingYear",
                 "TrapHauls", "Landings", "Trips",
                 "NLics", "CPUE")]
names(g27p)=c("LFA", "REPORTING_GRID", "FISHING_YEAR", "TRAP_HAULS", "LANDINGS_KG", "TRIPS", "LICENCES_REPORTED","CPUE_KG_TH" )


saveRDS(g27p,'lobster.pruned.grid.data.rds')

test=readRDS('lobster.pruned.grid.data.rds')

#------------------------------------------------
#OpenData request would like the exact license_ID's by grid by year

a =  lobster.db('process.logs')

lic_summary <- a %>%
  filter(SYEAR >= 2010) %>%
  group_by(SYEAR, LFA, GRID_NUM) %>%
  summarise(
    LICENCES_REPORTED = n_distinct(LICENCE_ID),
    UNIQUE_LICENCE_IDS = paste(sort(unique(LICENCE_ID)), collapse = ","),
    .groups = "drop"
  ) %>%
  arrange(SYEAR, LFA, GRID_NUM)

Licence_Summary <- as.data.frame(lic_summary)

names(Licence_Summary) <- c(
  "FISHING_YEAR",
  "LFA",
  "REPORTING_GRID",
  "LICENCES_REPORTED",
  "UNIQUE_LICENCE_IDS"
)

saveRDS(Licence_Summary,'Licence_Summary.rds')

  openxlsx::write.xlsx(
    Licence_Summary,
    "Licence_Summary.xlsx",
    colNames = TRUE
  )

