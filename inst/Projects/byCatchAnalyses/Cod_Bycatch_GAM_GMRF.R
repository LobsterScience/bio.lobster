#Bank of packages needed to complete the following analysis:#

library(mgcv)
library(dplyr)
library(ggplot2)
library(tidyverse)
require(devtools)
require(SpatialHub)
library(sf)
library(lubridate)
library(MASS)
library(sp)
library(rgdal)
require(PBSmapping)
library(rgdal)
library(proj4)
library(spdep)
library(viridis)
require(bio.lobster)

load('~/tmp/atSeaData.rdata')
load('~/tmp/AtSeaDataAggregatedWithEmptyTrapsCoates.rdata')


load('/SpinDr/backup/bio_data/bio.lobster/data/predspace.rdata')

    #Pull the data needed from atSS#
    Cod2 <- subset(atSS,select=c(UID,STARTDATE, LFA, DEPTH, GRIDNO, SPECIESCODE, SPECIES, EST_NUM_CAUGHT, EST_KEPT_WT, EST_DISCARD_WT, NUM_HOOK_HAUL, Sampled, Total, Nempty, Y, X))

    #Remove NA's from weight and # caught data# 
    Cod2$EST_KEPT_WT[is.na(Cod2$EST_KEPT_WT)] <- 0
    Cod2$EST_DISCARD_WT[is.na(Cod2$EST_DISCARD_WT)] <- 0
    Cod2$EST_NUM_CAUGHT[is.na(Cod2$EST_NUM_CAUGHT)] <- 0
    Cod2$NUM_HOOK_HAUL[is.na(Cod2$NUM_HOOK_HAUL)] <- 0

    #Get total weight and number caught into respective columns#
    Cod2$CODWEIGHT <- ifelse(Cod2$SPECIESCODE == 10,Cod2$EST_DISCARD_WT+Cod2$EST_KEPT_WT,0)
    Cod2$CODNUMBER <- ifelse(Cod2$SPECIESCODE == 10,Cod2$EST_NUM_CAUGHT,0)


     All = Cod2[!duplicated(Cod2[,c('UID')]),]
     Co = subset(Cod2, SPECIESCODE==10)
     CoAll = rbind( Co,All)
     CodFiltered = CoAll[!duplicated(CoAll$UID, fromLast=T),]

    Codtot = CodFiltered
    #Leave base dataset alone and manipulate variables 
    Codtot$Month = month(Codtot$STARTDATE)
    Codtot$Year = year(Codtot$STARTDATE)
    Codtot$Year <- as.factor(Codtot$Year)
    Codtot$EID = 1:nrow(Codtot)
    attr(Codtot,'projection') <- 'LL' 
    Codtot = subset(Codtot,!is.na(X))
    #Making a mesh for the data.based on grids
LFAgrid<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","GridPolys.csv"))
    LFA41<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","LFA41Offareas.csv"))
    grL = subset(LFAgrid, PID>32 &PID < 100)    
    grL$area=grL$PID
    LFA41$area = LFA41$OFFAREA
    LFA41$OFFAREA <- NULL
    LFA41$SID = 1

Gr41 = makeGrid(y=seq(41,43.8,by=.1667),x=seq(-68,-63.6,by=.1667),addSID=FALSE)
LFA41u = joinPolys(LFA41,operation='UNION')
LFA41u = subset(LFA41u,SID==1)
LFA41u$PID=LFA41u$PID+100
G41 = joinPolys(LFA41u,Gr41,'INT')
G41$PID = G41$PID+1000
G41$SID=1
G41$area = '41'
grs = rbind(grL,G41, all=T)
    if(any(grs$area==T)) grs = grs[-which(grs$area==T),]
attr(grs,'projection') <- 'LL'

#grs is the grids
grs$PID = (grs$PID+grs$SID/1000)*1000
grs$SID=NULL

F = findPolys(Codtot,grs)
CF = merge(Codtot,F,by='EID')
CF = subset(CF, Bdry==0)
attr(CF,'projection') <- "LL"

#making the list of connections to grids


    CF = subset(CF, NUM_HOOK_HAUL>1)
    CF$lHook = log(CF$NUM_HOOK_HAUL)
    save(CF,file='~/tmp/CF.rdata')
    CF$LOCIDS = as.character(CF$PID)
    ctrl <- gam.control(nthreads = 6) # use 6 parallel threads, reduce if fewer physical CPU cores
    
#make the neighbour joins
require(sp)
gr = subset(grs, PID %in% unique(CF$LOCIDS))
g = split(gr,f=gr$PID)
nm = c()
gp = list()
for(i in 1:length(g)){
    gp[[i]] = Polygons(list(Polygon(g[[i]][,c('X','Y')])),unique(g[[i]]$PID))
  }
gpp = SpatialPolygons(gp,proj4string=CRS("+proj=longlat +datum=WGS84"))
gpnb = poly2nb(gpp,row.names=names(gpp))
names(gpnb)=names(gpp)

#need to figure out this problem of if needing a single value per grid. Hope not
CW = aggregate(CODWEIGHT~LOCIDS, data=CF, FUN = mean)
CD = aggregate(DEPTH~LOCIDS, data=CF, FUN = mean)

    #want the offset to be same as link
    CTF = formula(CODWEIGHT~ (Year) + s(LOCIDS, bs = 'mrf', k=50,xt = list(nb = gpnb)))
    CTM = gam(CTF,data=CF, family = Tweedie(p=1.25,link=log), method = "REML")

    #Transform Decimal degree coordinates into UTM projection#
    latlon <- totdat3 %>% dplyr::select(X,Y)
    coordinates(latlon) <- c("X", "Y")
    proj4string(latlon) <- CRS("+proj=longlat +datum=WGS84")
    res <- spTransform(latlon, CRS("+proj=utm +zone=20 ellps=WGS84"))
    planar <-as.data.frame(res)
    planar <- planar %>% rename("plon" = "X") %>% rename("plat" = "Y")

    #Further analysis needs km instead of meters#
    planar <- planar %>% mutate(plon = plon/1000) %>% mutate(plat = plat/1000)
    totdat3 <- merge(totdat3,planar,by="UID")

    #Build GAM formula and model# 
    #CTF = formula(CODWEIGHT~ as.factor(Year) + s(SET_DEPTH, k = 20) + s(plon, plat, bs='ts',k=100))

    #AMC
    totdat3 = subset(totdat3, NUM_HOOK_HAUL>1)
    totdat3$lHook = log(totdat3$NUM_HOOK_HAUL)
    save(totdat3,file='~/tmp/totdat3.rdata')
   
require(PBSmapping)
#logbook trap hauls by grid by year
  require(bio.lobster)
  la()
  logs = lobster.db('process.logs')
  logs = subset(logs, LFA>32)
  logs$Year = year(logs$DATE_FISHED)

  lobster.db('logs41')
  log4 = logs41
  log4$Date = strptime(log4$FV_FISHED_DATETIME,format = '%Y-%m-%d %H:%M:%S')
   
  log4$year = year(log4$Date)  
  l41s = aggregate(NUM_OF_TRAPS~year+OFFAREA, data=log4, FUN=sum)
  lrest3 = aggregate(NUM_OF_TRAPS~GRID_NUM+Year,data=logs,FUN=sum)
  
    LFAgrid<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","GridPolys.csv"))
    LFA41<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","LFA41Offareas.csv"))
    grL = subset(LFAgrid, PID>32)    
    grL$area=grL$PID
    LFA41$area = LFA41$OFFAREA
    LFA41$OFFAREA <- NULL
    LFA41$SID = 1
    grs = rbind(grL,LFA41, all=T)
    if(any(grs$area==T)) grs = grs[-which(grs$area==T),]

latlon <- grs %>% dplyr::select(X,Y)
coordinates(latlon) <- c("X", "Y")
proj4string(latlon) <- CRS("+proj=longlat +datum=WGS84")
res <- spTransform(latlon, CRS("+proj=utm +zone=20 ellps=WGS84"))
planar <-as.data.frame(res)
planar <- planar %>% rename("plon" = "X") %>% rename("plat" = "Y")

#Further analysis needs km instead of meters#
planar <- planar %>% mutate(plon = plon/1000) %>% mutate(plat = plat/1000)
grs$X <- planar$plon
grs$Y <- planar$plat

Ps$X = Ps$plon
Ps$Y = Ps$plat
ht = findPolys(Ps, grs,maxRows=nrow(Ps))
Ps = merge(Ps,ht)


Pss = list()
Years = unique(totdat3$Year)

require(bio.utilities)
for(i in 1:length(Years)){
  Pst = Ps
  Pst$Year = Years[i]
  Pst$NUM_OF_TRAPS = NA
  Pst$GRID_NUM = Pst$SID
  lw = subset(lrest3, Year==Years[i])
  Pst1 = subset(Pst,PID>10)
  Pst1$GRID_NUM = Pst1$SID
  Pst1 = fillNaDf2(Pst1,lw,'GRID_NUM','NUM_OF_TRAPS' )
  Pst2 = subset(Pst,PID<10)
  l41s1 = subset(l41s, year==Years[i])
  l41s1$PID = ifelse(l41s1$OFFAREA=='GBANK',2,ifelse(l41s1$OFFAREA=='GBASIN',3,ifelse(l41s1$OFFAREA=='SEBROWNS',4,ifelse(l41s1$OFFAREA=='SWBROWNS',5,1))))

  Pst2 = fillNaDf2(Pst2,l41s1,'PID','NUM_OF_TRAPS' )

Pst = rbind(Pst1, Pst2)
Pst = subset(Pst, !is.na(NUM_OF_TRAPS))
Pst$lHook = log(Pst$NUM_OF_TRAPS)
   Pss[[i]] = Pst
} 

Ps = do.call(rbind,Pss)


Ps$lHook = log(10)
a_lp_matrix = predict(object = CTM3, Ps,
                      type = "lpmatrix")

a_coef_mean = coef(CTM3)
a_vcov = vcov(CTM3)
a_par_coef_posterior = rmvn(n = n_sims, 
                            mu = a_coef_mean,
                            V = a_vcov)
ilink = family(CTM3)$linkinv

preds = ilink(a_lp_matrix %*% t(a_par_coef_posterior))
apreds = as.data.frame(preds)

Press = cbind(Ps,apreds)
save(Press,file='~/tmp/outss.rdata')
load('~/tmp/outss.rdata')
#apreds$YEAR = Ps$Year

apply(Press[,14:1013],1,median)

asa = as.data.frame(aggregate(.~YEAR,data=apreds,FUN=sum))
ag = apply(asa[,:1001],1,quantile,0.5)/1000

png(file=file.path(fpf1,'ILTSrecruitabund.png'),units='in',width=15,height=12,pointsize=18, res=300,type='cairo')
plot(asa$YEAR,apply(asa[,2:1001],1,quantile,0.5)/1000,xlab='Year',ylab='Recruit abundance',pch=16,ylim=c(0,55000))
arrows(asa$YEAR, y0 = apply(asa[,2:1001],1,quantile,0.025)/1000, y1 =apply(asa[,2:1001],1,quantile,0.975)/1000,length=0 )
lines(asa$YEAR,runmed(ag,k=3),col='salmon',lwd=2)
dev.off()