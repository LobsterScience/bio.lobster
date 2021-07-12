require(bio.lobster)
require(bio.utilities)
require(RODBC)
require(lubridate)
#SWLSS
x20 = read.csv(	'C:/Users/cooka/Desktop/BYCATCH/Bycatch Validation 20-21/SummaryOfTrips/CompiledData.SWLSS.2021-07-09.csv')
x19 = read.csv(	'C:/Users/cooka/Desktop/BYCATCH/Bycatch Validation 19-20/SummaryOfTrips/CompiledData.SWLSS.2021-07-09.csv')
x18 = read.csv(	'C:/Users/cooka/Desktop/BYCATCH/Bycatch Validation Oct 2019/SummaryOfTrips/CompiledData.SWLSS.2020-02-21.csv')

xAll = rbind(x18,x19,x20)
xAll$X = convert.dd.dddd(xAll$LONGDDMM)*-1
xAll$Y = convert.dd.dddd(xAll$LATDDMM)
xAll$EID = 1:nrow(xAll)
saveRDS(xAll,'C:/Users/cooka/Desktop/sharedfolder/Bycatch in the Lobster Fishery/data/CompiledSWLSS.rds')

#LOGS
x = lobster.db('process.logs.unfiltered')
x = subset(x,LFA %in% c(33,34,35) & SYEAR %in% 2019:2021)
saveRDS(x,'C:/Users/cooka/Desktop/sharedfolder/Bycatch in the Lobster Fishery/data/Compiledlogs.rds')




#OBSERVERS
con = odbcConnect(oracle.lobster.server , uid=oracle.lobster.user, pwd=oracle.lobster.password, believeNRows=F) # believeNRows=F required for oracle db's

tr = sqlQuery(con,'select * from lobster.istraps')
se = sqlQuery(con,'select * from lobster.issets_mv')
de = sqlQuery(con,'select * from lobster.isdetails_mv')


LobsterMap('33-35')
se$X = se$HAUL_LONG*-1
se$Y = se$HAUL_LAT
se$EID = 1:nrow(se)

addPoints(se,col='blue',pch=16)
addPoints(na.omit(xAll[,c('X','Y','EID')]),col='red',pch=16)

          