# Lobster Fishery Footprint
require(bio.survey)
require(bio.lobster)
p = bio.lobster::load.environment()
p$libs = NULL

require(bio.lobster)
require(bio.utilities)
require(sf)
la()

lobster.db('logs41') #make sure to do a database recapture through logs41.redo before moving on

logs41 = rename.df(logs41,c('FV_FISHED_DATETIME'),c('DATE_FISHED'))
rL = readRDS(file.path("C:/Users/Cooka/Documents/bio_data/bio.lobster/data/maps/LFAPolysSF.rds"))


logs41$yr = year(logs41$DATE_FISHED) #2002 to present
ziff41$yr = year(ziff41$DATE_FISHED) #1995 to 2001
ziff41$DDLON = ziff41$DDLON * -1
off41$yr  = year(off41$DATE_FISHED) #1981 to 1994

logs41$OFFAREA = NULL	

a41 = logs41
a41$fishingYear = sapply(a41$DATE_FISHED,offFishingYear)
a41 = makePBS(a41,polygon=F)
a41$ADJCATCH = a41$ADJCATCH / 2.2 #convert to kgs then to t

a41 = subset(a41,!is.na(DDLON))
a41$DDLON = a41$DDLON*-1
aS = st_as_sf(a41,coords=c('DDLON','DDLAT'),crs=4326)

aSS=st_join(aS,rL,join=st_intersects)
aSS = subset(aSS, !is.na(LFA))

aSS$month = month.name[month(aSS$DATE_FISHED)]
aSS$CP = aSS$ADJCATCH/aSS$NUM_OF_TRAPS
aSS = subset(aSS,CP<40)
aSS$CPUE = ifelse(aSS$CPUE>10,10,aSS$CPUE)

offshoreFeetPrints <- function(x,yr1){
  width_inch <- 8.5
  height_inch <- 11
  
  x1 = subset(x,yr==yr1)
    ok1 = ggplot(rL)+
      #scale_fill_distiller(trans='identity',palette='Spectral') +
      geom_sf()+
      geom_sf(data=x1,pch=21, aes(size = CPUE),fill = alpha("red", 0.2),col = "grey20")+
      facet_wrap(~month)+        
        coord_sf(xlim=c(-68,-63.5),	ylim=c(41.1,44))
    nm = paste('Year',yr1,'- catch (kg) per unit effort (trap haul) by string, split by month.png',sep="")
    ggsave(nm, plot = ok1, width = width_inch, height = height_inch, units = "in") 
}
wd = ('C:\\Users\\Cooka\\OneDrive - DFO-MPO\\CanUsCollabEffortMapping\\Offshore\\')
setwd(wd)

offshoreFeetPrints(x=x,yr1=2015)
offshoreFeetPrints(x=x,yr1=2016)
offshoreFeetPrints(x=x,yr1=2017)
offshoreFeetPrints(x=x,yr1=2018)
offshoreFeetPrints(x=x,yr1=2019)
offshoreFeetPrints(x=x,yr1=2020)
offshoreFeetPrints(x=x,yr1=2021)
offshoreFeetPrints(x=x,yr1=2022)
offshoreFeetPrints(x=x,yr1=2023)
