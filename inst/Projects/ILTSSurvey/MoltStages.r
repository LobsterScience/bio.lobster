#Shrinking Lobsters

require(bio.lobster)
require(devtools)
require(bio.utilities)
require(sf)
require(ggplot2)
lobster.db("survey")

surveyMeasurements$YEAR = year(surveyMeasurements$SET_DATE)
y = subset(surveyMeasurements,SPECCD_ID==2550 & month(SET_DATE) %in% c(6,7))
yf = subset(surveyMeasurements,SPECCD_ID==2550 & month(SET_DATE) %in% c(9,10))

sc = subset(surveyCatch)

#fall
fa = prop.table(table(yf$SHELL,yf$YEAR,yf$SEX),2)
barplot(fa)

fad = as.data.frame(fa)
ggplot(fad,aes(x=Var2,y=Freq,fill=Var1))+geom_col()
fad$M = ifelse(fad$Var1 %in% 1:4 & fad$Var3 %in% c(1,2),'Moulted, need to shrink',ifelse(fad$Var1==7,'Imminent Moult- length is fine',ifelse(fad$Var3==3,'berried, length is fine','Intermoult- ?')))
fada = aggregate(Freq~M+Var2,data=fad,FUN=sum)
ggplot(fada,aes(x=Var2,y=Freq,fill=M))+geom_col()

#summer
fa = prop.table(table(y$SHELL,y$YEAR),2)
fad = as.data.frame(fa)
#fad$M = ifelse(fad$Var1 %in% 1:4 & fad$Var3 %in% c(1,2),'Moulted, all good',ifelse(fad$Var1==7,'Imminent Moult- length is fine',ifelse(fad$Var3==3,'berried, length is fine','Intermoult- ?')))
fada = aggregate(Freq~Var1+Var2,data=fad,FUN=sum)
ggplot(subset(fada,Var2 %in% 2018:2023),aes(x=Var2,y=Freq,fill=Var1))+geom_col()



x = groundfish.db('special.lobster.sampling')
xx = groundfish.db('gs_trawl_conversions')
  set = xx$gsinf
  cas = xx$gscat
  de = xx$gsdet
  de = subset(de,spec==2550)
  
me = merge(subset(de,select=c(flen,mission,setno,fsex,specimen_id)),subset(x,select=c(flen,mission,setno,fsex,specimen_id,molt_stage)),all=T)

mei = merge(subset(set,select=c(mission,setno,slat,slong,strat,sdate)),me)

mei$X = convert.dd.dddd(mei$slong)
mei$Y = convert.dd.dddd(mei$slat)

mei$mon = lubridate::month(mei$sdate)
mei$year = lubridate::year(mei$sdate)

mei = subset(mei,mon %in% c(6,7,8) & year>2015)
plot(prop.table(table(mei$year,mei$molt_stage),1))



rL = readRDS(file.path( project.datadirectory("bio.lobster"), "data","maps","LFAPolysSF.rds"))
rL = st_as_sf(rL)
st_crs(rL) <- 4326
