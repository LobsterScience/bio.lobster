#all 2022 data for Natalie

require(bio.lobster)
require(bio.utilities)
require(devtools)
require(lubridate)
require(dplyr)
la()

#rvsurvey
x = groundfish.db('gsinf.odbc')
x = subset(x,year(sdate)==2022 & month(sdate) %in% c(6,7,8) & strat>470, select=c(mission,setno, sdate,slat, slong,strat))
x$X = bio.lobster::convert.dd.dddd(x$slong)*-1
x$Y = bio.lobster::convert.dd.dddd(x$slat)
x$Unique_ID = paste(x$mission,x$setno,sep="_")
x = subset(x,select=c(Unique_ID,sdate,X,Y))
x$Source = 'RV_Survey'
names(x)[2] = 'Date'

#ilts
y = ILTS_ITQ_All_Data(species=2550,redo_base_data = F)
y = subset(y,YEAR==2022)
y = distinct(y,TRIP_ID,SET_NO,SET_DATE,SET_LAT,SET_LONG)
y$Unique_ID = paste(y$TRIP_ID,y$SET_NO,sep="_")
y = subset(y,select=c(Unique_ID,SET_DATE,SET_LONG,SET_LAT))
names(y)[2:4] = c('Date','X','Y')
y$Source = 'ILTS'

#at sea
db.setup(un=oracle.personal.user,pw=oracle.personal.password)
as = connect.command(con,'select * from cooka.bycatch_swlss_2022')

as$X = bio.lobster::convert.dd.dddd(as$LONGDDMM)
as$Y = bio.lobster::convert.dd.dddd(as$LATDDMM)
as = distinct(as,TRIP,BOARD_DATE,SET_NO,STRATUM_ID,X,Y)
as$Unique_ID = paste(as$TRIP,as$STRATUM_ID,as$SET_NO,sep="_")
as = subset(as,select=c(BOARD_DATE,X,Y,Unique_ID))
names(as)[1] = 'Date'
as$Source = 'AtSea_Sampling'

#fsrs
lobster.db('fsrs')
f = subset(fsrs,LFA %in% c(34,35) & SYEAR==2022)
f = distinct(f,HAUL_DATE,VESSEL_CD,LAT_DD,LONG_DD)
names(f)=c('Date','Unique_ID','Y',"X")
f$Source='Recruitment_Trap'
f$Unique_ID = as.character(f$Unique_ID)


comb = bind_rows(list(x,y,as,f))