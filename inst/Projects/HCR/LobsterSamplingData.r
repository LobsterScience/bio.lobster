#build the data

lobster.db('atSea')
a = subset(atSea,SPECIESCODE==2550)

lobster.db('survey')
b = subset(surveyMeasurements,SPECCD_ID==2550,select=c(TRIP,LFA, FISHSET_ID,SET_DATE,SET_LAT,SET_LONG,SET_DEPTH,FISH_ID,FISH_LENGTH,SEX,EGG,CLUTCH,SHELL,DAMAGE))

v = groundfish.db('special.lobster.sampling')
g = groundfish.db('gsinf.odbc')

vg = merge(v,subset(g,select=c(mission,setno,slat,slong,depth, bottom_temperature)),by=c('mission','setno'))

saveRDS(a,'atSeaTrapSampling.rds')
saveRDS(b,'lobstertrawlsurveysampling.rds')
saveRDS(vg,'lobsterRVtrawlsurveysampling.rds')
