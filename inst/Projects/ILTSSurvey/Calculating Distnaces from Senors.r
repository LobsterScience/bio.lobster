#looping through sets to calculate swept areas

require(bio.lobster)
require(bio.utilities)
require(devtools)
require(geosphere)
la()

lobster.db('survey.redo')

ic = ILTSClick

junk = list()
for(i in 1:nrow(ic)){
    if(i %in% round(seq(5,nrow(ic),length.out=100))) print(i)
    v = ic[i,]
    rv = unique(subset(surveyCatch,TRIP_ID == v$TRIP_ID & SET_NO==v$SET_NO)$GEAR)
    bds = c(5,ifelse(rv=='NEST',c(20),c(30)))
    v$gear = rv
    
    v$st= strptime(sapply(strsplit(as.character(v$STARTDATETIME),split= " "), function(x) x[2]),"%H:%M:%S")
    v$et= strptime(sapply(strsplit(as.character(v$ENDDATETIME),split= " "), function(x) x[2]),"%H:%M:%S")
    if(yday(v$STARTDATETIME) != yday(v$ENDDATETIME))v$et$mday= v$et$mday +1 #if jumps over night
    
      
    v$yr = year(v$STARTDATETIME)
    
    v$nonweighted_spread = v$sensor = v$sweptArea = v$distance = NA
  
    #chagne to esorar and marport or netmind (esonar is same as netmind so use same call)
    se = subset(ILTSSensor,TRIP_ID == v$TRIP_ID & SET_NO==v$SET_NO)
    if(nrow(se)>0){
    v$sensor = unique(se$SOURCE)
    
    if(v$sensor=='MARPORT' & v$yr>2016) se = subset(se, VALIDITY=='RAW')
    if(v$sensor=='MARPORT' & v$yr==2016) se1 = subset(se, VALIDITY=='1000') #raw
      
    
  if(all(c(nrow(se)>5, length(unique(se$GPSTIME))>5) )){
    se$Time = strptime(se$GPSTIME,"%H%M%S")
    
    if(v$QUALITY>0) se = subset(se,Time>=v$st & Time<=v$et) #click touch time   
    if(v$QUALITY==0) se = subset(se,FLAG==1)   #winch time
    
    if(all(c(nrow(se)>5) )){
      #
    
    se$Y = c(sapply(se$LATITUDE,fixSensorLatLon)  )
    se$X = c(sapply(se$LONGITUDE,fixSensorLatLon)*-1  )
    
    se = se[order(se$GPSTIME),]
    #distance
    v$distance <- sum(sapply(2:nrow(se), function(i) {
      distGeo(se[i - 1, c("X", "Y")], se[i, c("X", "Y")])
    }))/1000
    
    
    # for future years, assume names are same as 2021, change this if needed:
    ##change dates to esonar and marport not year
    if(v$sensor=='MARPORT'){
         b = subset(se,TRANSDUCERNAME %in% c('PRP','WINGSPREAD') & SENSORNAME=='DISTANCE' & SENSORVALUE>=bds[1] & SENSORVALUE<=bds[2])
        if(nrow(b)>5){
          b$distSeg = c(0,sapply(2:nrow(b), function(i) {
            distGeo(b[i - 1, c("X", "Y")], b[i, c("X", "Y")])
              }))
         b$RealDist = b$distSeg/sum(b$distSeg)*v$distance
         v$sweptArea = sum(b$RealDist*b$SENSORVALUE/1000)
              }
         }
    if(v$sensor %in% c("ESONAR","NETMIND")){
      b = subset(se,TRANSDUCERNAME=="DoorSpread" & SENSORNAME %in% c("STBDDoorMaster","DoorMaster") & SENSORVALUE>=bds[1] & SENSORVALUE<=bds[2])
      if(nrow(b)>5){
          b$distSeg = c(0,sapply(2:nrow(b), function(i) {
          distGeo(b[i - 1, c("X", "Y")], b[i, c("X", "Y")])
          }))
          b$RealDist = b$distSeg/sum(b$distSeg)*v$distance
          v$sweptArea = sum(b$RealDist*b$SENSORVALUE/1000)
          }
        }
   
      }
    }
    }
    junk[[i]] = v
    rm(list=c('v','se','bds','rv'))
}

j = do.call(rbind,junk)
j$spread = j$sweptArea/j$distance*1000

j$gearid = ifelse(j$gear=='NEST',16,21)
j$sensorid = ifelse(j$sensor=='MARPORT',1,2)

with(subset(j,distance<2),plot(distance,spread,pch=gearid,col=sensorid))

