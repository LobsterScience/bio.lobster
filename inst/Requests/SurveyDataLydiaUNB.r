#rv survey data for lydia

require(bio.lobster)
require(sf)
require(ggplot2)
require(devtools)
la()

set = groundfish.db(DS='gsinf.odbc')
cas = groundfish.db(DS='gscat.odbc')
stra = groundfish.db(DS='gsstratum')
de = groundfish.db(DS='gsdet.odbc')
v = groundfish.db('polygons')

de = bio.utilities::toNums(de,2:10)
cas = bio.utilities::toNums(cas,2:8)
set$X = convert.dd.dddd(set$slong) *-1
set$Y = convert.dd.dddd(set$slat)

ii = which(months(set$sdate) %in% c('June','July','August') & set$type %in% c(1,5) & set$gear %in% c(3,9))
set = set[ii,]

set$uid = paste(set$mission,set$setno, sep="")
de$uid = paste(de$mission,de$setno, sep="")
cas$uid = paste(cas$mission,cas$setno, sep="")

de = subset(de,uid %in% set$uid)
cas = subset(cas,uid %in% set$uid)

de = subset(de,spec==2550)
cas = subset(cas,spec==2550)

set$wing = ifelse(set$gear==3,10.7,12.8) #m for WIIA and Yankee
set$distKm = set$dist*1.8052 
stra$areaKM = stra$area * 3.4299

set$sweptArea_km2 = set$wing/1000 * set$distKm
set$depth_m = (set$dmin+set$dmax) / 2 * 1.8288 #from fm to m

set = subset(set,select=c(mission, setno, sdate, strat, X,Y, bottom_temperature, depth_m,sweptArea_km2))
cas = subset(cas,select=c(mission, setno, sampwgt, totwgt,totno,size_class))
de = subset(de,select=c(mission, setno, flen, fsex,fwt,clen,size_class))

stra = subset(stra, strat %in% unique(set$strat))

stra_defs = data.frame(cols=names(stra),defs=c('strata label','minimum depth (fm)','maximum depth (fm)','area in Nm2','depth criteria','NAFO','area in km2'))
attr(stra,'column_defs') <- stra_defs

de_defs = data.frame(cols=names(de),defs=c('Mission','set','lobster length mm','sex 1=male, 2=female, 3=berried','weight in g','number at length','size class (needed to match the catch table)'))
attr(de,'column_defs') <- de_defs

cas_defs = data.frame(cols=names(cas),defs=c('Mission','set','sample weight (if not all lobster were measured); kg','total weight of catch kg','total number in catch','if lobsters were split into size classes for measurements'))
attr(cas,'column_defs') <- cas_defs

set_defs = data.frame(cols=names(set),defs=c('Mission','set','date of tow','strata number','Longitude (wgs84)','Latitude (wgs84)', 'temperature in C','Average depth of tow m','estimate of swept area in km2; distance towed x wing spread'))
attr(set,'column_defs') <- set_defs

saveRDS(list(Set_info=set, Catch_info=cas, Detailed_sampling=de, Strata_details=stra, Strata_polygons=v),file='C:/Users/Cooka//Desktop/dellshared/Student Advisory Committees/White,L/groundfish_surveyData.rds')

a =list(Set_info=set, Catch_info=cas, Detailed_sampling=de, Strata_details=stra, Strata_polygons=v)