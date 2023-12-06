#comparing sensors and olex

require(bio.lobster)

lobster.db('survey')

ic = ILTSClick
ol = ILTSOlextracks
se = ILTSSensor
se = subset(se, !is.na(se$LATITUDE) & !is.na(se$LONGITUDE))

se$Y = c(sapply(se$LATITUDE,fixSensorLatLon)  )
se$X = c(sapply(se$LONGITUDE,fixSensorLatLon)*-1  )
se = subset(se,!is.na(Y) & !is.na(X))
se$Time = strptime(se$GPSTIME,"%H%M%S")

ic = subset(ic,QUALITY==1)
ic$st = strptime(ic$STARTTIME,"%H%M%S")
ic$et = strptime(ic$ENDTIME,"%H%M%S")

uic = ic[!duplicated(ic[,c('TRIP_ID','SET_NO')]),c('TRIP_ID','SET_NO')]

ol$Time = strptime(ol$STDTIME,"%H:%M:%S")+(3*60*60)
out=data.frame(uic,DistSens = NA, DistOlex=NA, DistOlexWinch=NA, DistSensWinch = NA)
for(i in 828:nrow(uic)){
  ses = subset(se,TRIP_ID==uic$TRIP_ID[i] & SET_NO==uic$SET_NO[i])        
  ols = subset(ol,TRIP_ID==uic$TRIP_ID[i] & SET_NO==uic$SET_NO[i])        
  ics = subset(ic,TRIP_ID==uic$TRIP_ID[i] & SET_NO==uic$SET_NO[i])
  sess = subset(ses, Time>=ics$st & Time<=ics$et )
  olss = subset(ols, Time>=ics$st & Time<=ics$et )
  
  sess = sess[order(sess$Time),]
  olss = olss[order(olss$Time),]
  if(nrow(sess)>10){
  out$DistSens[i] <- sum(sapply(2:nrow(sess), function(i) {
    distGeo(sess[i - 1, c("X", "Y")], sess[i, c("X", "Y")])
  }))/1000
  }
  if(nrow(olss)>10){
  out$DistOlex[i] <- sum(sapply(2:nrow(olss), function(i) {
    distGeo(olss[i - 1, c("X", "Y")], olss[i, c("X", "Y")])
  }))/1000
  }
  
  sess = subset(ses, Time>=ics$st & Time<=ics$et )
  olss = subset(ols, Time>=ics$st & Time<=ics$et )
 
  sur = subset(surveyCatch,TRIP_ID == uic$TRIP_ID[i] & SET_NO==uic$SET_NO[i])
  st = strptime(unique(sur$SET_TIME),"%H%M") + (3*60*60)
  et = strptime(unique(sur$HAUL_TIME),"%H%M")+ (3*60*60)
  sess = subset(ses, Time>=st & Time<=et )
  olss = subset(ols, Time>=st & Time<=et )
  
  
  if(nrow(sess)>10){
    out$DistSensWinch[i] <- sum(sapply(2:nrow(sess), function(i) {
      distGeo(sess[i - 1, c("X", "Y")], sess[i, c("X", "Y")])
    }))/1000
  }
  if(nrow(olss)>10){
    out$DistOlexWinch[i] <- sum(sapply(2:nrow(olss), function(i) {
      distGeo(olss[i - 1, c("X", "Y")], olss[i, c("X", "Y")])
    }))/1000
  }
  
  
  }