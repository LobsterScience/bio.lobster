require(bio.lobster)
require(bio.utilities)
require(ggplot2)
require(sf)
require(devtools)
la()

lobster.db('atSea')

g = subset(atSea, SPECIESCODE==2550 & !is.na(CARLENGTH))
g$YR = lubridate::year(g$STARTDATE)
g$SET_LON = g$LONGITUDE
g$SET_LAT = g$LATITUDE
g$FISH_LENGTH = g$CARLENGTH
g$SEX = g$SEX

g = subset(g,select=c(SET_LON,SET_LAT,YR,FISH_LENGTH,SEX))
g$DS = 'Traps'
  
  
r = groundfish.db('gs_trawl_conversions')
ri = r$gsinf
rd = r$gsdet
r = merge(subset(ri,select=c(mission, setno, slat, slong,sdate)),subset(rd, spec==2550,select=c(mission, setno,fsex,flen,clen)))
r$SET_LAT = convert.dd.dddd(r$slat)
r$SET_LON = convert.dd.dddd(r$slong)*-1
r$YR = lubridate::year(r$sdate)
r = subset(r,clen>0, select=c(SET_LON,SET_LAT,YR,flen,fsex))
r$DS = 'RV'

lobster.db('survey')
sm = subset(surveyMeasurements,SPECCD_ID==2550, select= c(TRIP_ID, SET_NO,SET_LON,SET_LAT, SET_DATE, FISH_LENGTH, SEX))
sm$YR = lubridate::year(sm$SET_DATE)

sm = subset(sm,select=c(SET_LON,SET_LAT,YR,FISH_LENGTH,SEX))
sm$DS = 'ILTS'

names(g) = names(sm) = names(r) 
ou = dplyr::bind_rows(list(g,sm,r))
ou = subset(ou,!is.na(SET_LON) & !is.na(SET_LAT))
ous = st_as_sf(ou,coords=c('SET_LON','SET_LAT'),crs=4326)

rL = readRDS(file.path( git.repo,'bio.lobster.data', "mapping_data","LFAPolysSF.rds"))
rL = st_as_sf(rL)
st_crs(rL) <- 4326

orl = st_join(ous,rL)

of = aggregate(flen~LFA+YR+DS,data=subset(orl,fsex==3 & flen<130 & flen>50) , FUN=function(x) quantile(x,0.01))

i = which(of$LFA %in% c(40,41) & of$DS == 'ILTS' )
of = of[-i,]
#i = which(of$DS == 'RV' & of$YR<1998)
#of = of[-i,]
i = which(of$DS == 'RV' & of$LFA %in% c(27,29,30,311,312,32,33))
of = of[-i,]

i = which(of$DS == 'Traps' & of$LFA %in% c(36,38) & of$YR>2013)
of = of[-i,]

i = which(of$DS == 'ILTS' & of$LFA %in% c(35,36,38) & of$YR<2019)
of = of[-i,]

i = which(of$DS == 'Traps' & of$LFA %in% c(40) )
of = of[-i,]

of = subset(of,LFA !=28 & LFA !=37)
of$fYR = as.factor(of$YR)
 i = which(of$LFA==311)
 of$LFA[i] = '31A'
 i = which(of$LFA==312)
 of$LFA[i] = '31B'
 
 ggplot(of,aes(x=YR,y=flen,colour=DS,group=DS))+
  geom_point(size=0.8)+facet_wrap(~LFA,scales='free') +
  geom_smooth(method='lm', se=F) + 
  labs(x='Year',y='Minimum Observed Length (mm) Berried Females')+
  scale_x_continuous(breaks = scales::pretty_breaks(n=3))+theme_test()

 
 ggplot(subset(of,LFA==34),aes(x=YR,y=flen,colour=DS,group=DS))+
   geom_point(size=1.5)+facet_wrap(~LFA,scales='free') +
   geom_smooth(method='lm', se=F,size=1.5) + labs(x='Year',y='Minimum Observed Length (mm) Berried Females')+
   scale_x_continuous(breaks = scales::pretty_breaks(n=3))+theme_test()
 
