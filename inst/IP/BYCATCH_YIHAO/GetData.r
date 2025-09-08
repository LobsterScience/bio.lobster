###
require(bio.lobster)
require(sf)
require(devtools)
#at sea

lobster.db('atSea')
a = subset(atSea,LFA %in% c(33,34,35,36,38,40,41) & DESCRIPTION %ni% c('EA-MINAS','Maturity Sampling','Data not collected by trap','Petitcodiac Sampling','Tagging','Out of Season',"Black Point Ocean Disposal exp",'Industry Sample')
           & !is.na(SPECIESCODE) & STARTDATE>as.Date('2008-11-15') ,           select=c(TRIPNO,STARTDATE,LFA,LICENCE_ID,TRAPNO,STRINGNO,DEPTH, SOAKDAYS, LONGITUDE,LATITUDE, SPECIESCODE, SPECIES, SEX, SHELL, CARLENGTH, CONDITION,CALWT)) 
a$YR=year(a$STARTDATE)
#remove trips that bycatch was irregularly reported   
a = subset(a, TRIPNO %ni% c('100026598','100027348','100027353','100027817','100027518','100027440','100027457','100027620','100027686','100027688','100027906','100028083','100028384','100027912','100028311','100028646','100028755','100028318','100028756','100028322','100028758','100029647','100029958','100053554','100053610','100053809','100054369','100054449','100053815','100053817','100053818','100053821','100053831','100054367','100053850','100053851','100054365','100054363','100054467','100026458','100026459','100026802','100026836','100026837','100026918','100026838','100027164','100026839','100026840','100027064','100027077','100027377','100027104','100027097','100027098','100027138','100027437','100027417','100027537','100029278','100029899','100029917','100030161','100030205','100030558','100030788','100051425','100052590','100038701','100039237','100043257','100045023','100048250','100054704'))



saveRDS(a,file='LongerTS_bycatch_lobster_traps.rds')

#for nathan
ab = subset(a,SPECIESCODE==10)
ab = bio.utilities::rename.df(ab,'CARLENGTH','Length')
gr = readRDS(file.path(git.repo,'bio.lobster.data','mapping_data','GridPolysSF.rds'))
gr = st_make_valid(gr)
abs = st_as_sf(subset(ab,!is.na(LONGITUDE)),coords=c('LONGITUDE','LATITUDE'),crs=4326)


Fish.Date = lobster.db('season.dates')
Fish.Date = backFillSeasonDates(Fish.Date,eyr=year(Sys.time()))
lfa  =  sort(unique(Fish.Date$LFA))
Fish.Date$START_DATE = as.Date(Fish.Date$START_DATE)#,"%d/%m/%Y")
Fish.Date$END_DATE = as.Date(Fish.Date$END_DATE)#,"%d/%m/%Y")

                          abs$DATE_FISHED = as.Date(abs$STARTDATE,"%Y-%m-%d", tz="UTC" )
                          #logs$SYEAR = year(logs$DATE_FISHED)

                        for(i in 1:length(lfa)) {
                                h  =  Fish.Date[Fish.Date$LFA==lfa[i],]
                            for(j in 1:nrow(h)) {
                                abs$SYEAR[abs$LFA==lfa[i]&abs$DATE_FISHED>=h[j,'START_DATE']&abs$DATE_FISHED<=h[j,'END_DATE']] = h[j,'SYEAR']
                                }
                              }
                 i = which(abs$TRIPNO=="019919-291122")
 abs$SYEAR[i] = 2023
abs$SYEAR[which(abs$LFA==41)] <- abs$YR[which(abs$LFA==41)]
abs = st_join(abs,gr)
abs$LFA = abs$LFA.x
abs = subset(abs,SYEAR>2015)
                       abs$WOS = NA
                            for(i in 1:length(lfa)) {
                                  h  =  Fish.Date[Fish.Date$LFA==lfa[i],]
                               for(j in unique(abs$SYEAR[abs$LFA==lfa[i]])){
                                   print(c(lfa[i],j))
                                  abs$WOS[abs$LFA==lfa[i]&abs$SYEAR==j] = floor(as.numeric(abs$DATE_FISHED[abs$LFA==lfa[i]&abs$SYEAR==j]-min(h$START_DATE[h$SYEAR==j]))/7)+1
                                }
                              }

abss = subset(abs,select=c(TRIPNO, DATE_FISHED,WOS,SYEAR,LFA,SPECIESCODE,Length, CONDITION ))

da = fisheryFootprintData(period='weekly')

saveRDS(list(abss,gr,da),file='Cod_bycatch_lobster_traps_grids_effort.rds')

#grids


layerDir=file.path(project.datadirectory("bio.lobster"), "data","maps")
r<-readRDS(file.path( layerDir,"GridPolysSF.rds"))
r = st_as_sf(r)

a =  lobster.db('process.logs')
a = subset(a,SYEAR>2004 & SYEAR<2024)
b = lobster.db('seasonal.landings')
b = subset(b,!is.na(SYEAR))
b$SYEAR = 1976:2024
b$LFA38B <- NULL
b = subset(b,SYEAR>2004 & SYEAR<2024)
b = reshape(b,idvar='SYEAR', varying=list(2:6),direction='long')
b$LFA=rep(c(33,34,35,36,38),each=19)
b$time <- NULL
names(b)[1:2]=c('YR','SlipLand')


d = lobster.db('annual.landings')
d = subset(d,YR>2004 & YR<2024, select=c(YR,LFA27,LFA28,LFA29,LFA30,LFA31A,LFA31B,LFA32))
d = reshape(d,idvar='YR', varying=list(2:8),direction='long')
d$LFA=rep(c(27,28,29,30,'31A','31B',32),each=19)
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

saveRDS(list(Effort=partEffort,Landings=partLandings,atSea=a),'TrapHauls_LandingsWithinGridandWeek_atSeaData.rds')

saveRDS(a,'atSeaDataYihao_wts.rds')
