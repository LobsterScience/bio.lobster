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
require(bio.utilities)

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
    Codtot = subset(Codtot, Year>2007)
  
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
grs$OLDSID=grs$SID
grs$SID=NULL
grsU = as.data.frame(unique(grs[,c('PID','OLDSID')]))
F = findPolys(Codtot,grs)
CF = merge(Codtot,F,by='EID')
CF = subset(CF, Bdry==0)
attr(CF,'projection') <- "LL"

#making the list of connections to grids


    CF = subset(CF, NUM_HOOK_HAUL>1)
    CF$lHook = log(CF$NUM_HOOK_HAUL)
    save(CF,file='~/tmp/CF.rdata')
    load(file='~/tmp/CF.rdata')
    CF$LOCIDS = (CF$PID)
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
    CTF = formula(CODWEIGHT~ (Year) + s(LOCIDS, bs = 'mrf',xt = list(nb = gpnb))+offset(lHook))
    CTM = gam(CTF,data=CF, family = Tweedie(p=1.25,link=log), method = "REML")
    


#getting the logs to match the grids
l = lobster.db('process.logs.unfiltered')
lI = aggregate(NUM_OF_TRAPS~GRID_NUM+SYEAR,data=l,FUN=sum)
lIS = subset(lI,SYEAR>2005)
names(lIS)[1] = 'OLDSID'
lISm = merge(lIS,subset(grsU,PID<1000000))


 lobster.db('logs41')
 require(bio.utilities)

e = makePBS(logs41, polygon=FALSE)
ee = findPolys(na.omit(e[,c('X','Y','EID')]),grs)
l41 = merge(e,ee,by='EID')
l41 = merge(l41,grsU,by='PID')
l41$SYEAR = year(l41$FV_FISHED_DATETIME)
l41agg = aggregate(NUM_OF_TRAPS~SYEAR+PID+OLDSID,data=l41,FUN=sum)
l41aggS = subset(l41agg,SYEAR>2005)


clogs = rbind(lISm,l41aggS)
clogs1 = aggregate(NUM_OF_TRAPS~SYEAR+PID,data=clogs,FUN=sum)

clogs1$LOCIDS = clogs1$PID
clogs1$Year = clogs1$SYEAR
clogs1$lHook = log(clogs1$NUM_OF_TRAPS)
uLO = unique(clogs1$LOCIDS)
uLG = names(gpp)

#plot of overlaps
plotPolys(grs) #all grids
addPolys(subset(grs, LOCIDS%in% uLO),col='blue') #fishing grids
plot(gpp,add=T,col='red') #grids where we have bycatch info

AG = aggregate(NUM_OF_TRAPS~PID,data=clogs1,FUN=sum)
AG2 = aggregate(NUM_OF_TRAPS~PID,data=subset(clogs1,PID %in% unique(gr$PID)),FUN=sum)

sum(AG2[,2])/sum(AG[,2]) ##about 88% of trap hauls represnted


pDat = subset(clogs1, LOCIDS %in% names(gpp) & Year > 2007 & Year<2019, select=c(Year, LOCIDS, lHook))

  ilink <- family(CTM)$linkinv   # this is the inverse of the link function
  plo = as.data.frame(predict(CTM,pDat, type='link',se.fit=TRUE))
    plo$upper = ilink(plo$fit - (1.96 * plo$se.fit))  
    plo$lower = ilink(plo$fit - (1.96 * plo$se.fit))
    plo$fitted = ilink(plo$fit)
    Fis = as.data.frame(cbind(pDat,plo))
    Fis$PID = Fis$LOCIDS

#preds by year
outs = aggregate(fitted~Year,data=Fis,FUN=sum)

#combined preds

outsS = aggregate(fitted~LOCIDS,data=Fis,FUN=mean)
outsSV = aggregate(fitted~LOCIDS,data=Fis,FUN=var)

sum(outsS[,2])


require(BrewerPal) 
gpp1 = subset(grs,PID %in% names(gpp))
plotPolys(gpp1)
    xx=outsS
    xx$PID = xx$LOCIDS
    xq = quantile(xx$fitted,c(0.25,0.4,0.55,0.7,0.85,0.95)) 
        xq = round(xq/5)*5
        xll = xq
        lbrks<-length(xq)
        m=0
          xx$Z = xx$fitted
          xx<-as.PolyData(xx)
          
          xx<-makeProps(xx,xq,"col",cols)
          xx[which(xx$Z<xq[1]),'col'] <- cols[1]        
          xx[which(xx$Z>xq[length(xq)]),'col'] <- cols[length(cols)]        
      LobsterMap(title=y,ylim=c(41,46.5),xlim=c(-67.8,-63),labels='nn',addSummerStrata=FALSE,poly.lst = list(gpp1,xx))
  
      contLegend('bottomright',bty='n',cex=0.8,lvls = xll,Cont.data = data.frame(col=cols),title='Cod Weight(kg)')
      dev.off()
