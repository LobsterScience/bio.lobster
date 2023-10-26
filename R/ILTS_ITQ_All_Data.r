#' @export

ILTS_ITQ_sets <-function(x,species=2550){
  lobster.db("survey")
  
  require(bio.lobster)
  require(bio.utilities)
  require(devtools)
  require(geosphere)
  
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
    
    v$median_spread= v$sensor = v$sweptArea = v$distance = NA
    
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
          options(digits=10)
          
          se$Y = c(sapply(se$LATITUDE,fixSensorLatLon)  )
          se$X = c(sapply(se$LONGITUDE,fixSensorLatLon)*-1  )
          
          se = se[order(se$GPSTIME),]
          #distance
          v$distance <- sum(sapply(2:nrow(se), function(i) {
            distGeo(se[i - 1, c("X", "Y")], se[i, c("X", "Y")])
          }))/1000
          
          if(v$distance>4 | v$distance<.5) browser()
          
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
              v$median_spread = median(b$SENSORVALUE,na.rm=T)
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
              v$median_spread = median(b$SENSORVALUE,na.rm=T)
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
  
  surveyCatch = merge(surveyCatch,j, by.x=c('TRIP_ID','SET_NO','YEAR'),by.y=c('TRIP_ID','SET_NO','YEAR'),all.x=T)
  surveyCatch$WEIGHT_KG = ifelse(is.na(surveyCatch$EST_DISCARD_WT),0,surveyCatch$EST_DISCARD_WT) + ifelse(is.na(surveyCatch$EST_KEPT_WT),0,surveyCatch$EST_KEPT_WT)
 
  ii = which(is.na(surveyCatch$distance))
  surveyCatch$distance[ii] = sapply(ii, function(i) {
    distGeo(surveyCatch[i, c("SET_LONG", "SET_LAT")], surveyCatch[i, c("HAUL_LONG", "HAUL_LAT")])
  })/1000
  
  ii = which(surveyCatch$distance>=4)
  surveyCatch$sweptArea[ii] = surveyCatch$distance[ii] = NA
  
  #windsorize long trips
  ij = aggregate(distance~YEAR+GEAR,data=subset(surveyCatch,!is.na(distance)),FUN=function(x) quantile(x,0.99))
  ik = which(is.na(surveyCatch$distance))
  for(i in 1:length(ik)){
      y = unique(surveyCatch$YEAR[ik[i]])
      g = unique(surveyCatch$GEAR[ik[i]])
      surveyCatch$distance[ik[i]] = ij[which(ij$YEAR==y & ij$GEAR==g),'distance']
      }
  
  ii = which(surveyCatch$distance<0.2)
  surveyCatch$sweptArea[ii] = surveyCatch$distance[ii] = NA
  
  
  #windsorize short trips
  ij = aggregate(distance~YEAR+GEAR,data=subset(surveyCatch,!is.na(distance)),FUN=function(x) quantile(x,0.01))
  ik = which(is.na(surveyCatch$distance))
  for(i in 1:length(ik)){
    y = unique(surveyCatch$YEAR[ik[i]])
    g = unique(surveyCatch$GEAR[ik[i]])
    surveyCatch$distance[ik[i]] = ij[which(ij$YEAR==y & ij$GEAR==g),'distance']
  }
  
  
  ##spread fillins #not enough data for years
  ij = aggregate(spread~GEAR,data=subset(surveyCatch,!is.na(spread)),FUN=function(x) quantile(x,0.5))
  ik = which(is.na(surveyCatch$spread))
  surveyCatch$sweptArea[ii] = NA
  
  for(i in 1:length(ik)){
    g = unique(surveyCatch$GEAR[ik[i]])
    surveyCatch$spread[ik[i]] = ij[which(ij$GEAR==g),'spread']
  }
  
  ik = which(is.na(surveyCatch$sweptArea))
  surveyCatch$sweptArea[ik] =surveyCatch$distance[ik] * surveyCatch$spread[ik]/1000 
  
  sC = surveyCatch
	sC = subset(sC, HAULCCD_ID %in% c(1))
	sC = subset(sC,select=c(TRIP_ID, SET_NO, YEAR, VESSEL_NAME, LFA, GEAR, FISHSET_ID, STATION, SPECCD_ID, NUM_CAUGHT, SET_LAT, SET_LONG, SET_DEPTH, SET_TIME, SET_DATE, SET_ID, STARTDATETIME, ENDDATETIME, DEPTHM, QUALITY, gear, distance, sweptArea, sensor, spread, WEIGHT_KG))
	
  temp = aggregate(TEMPC~TRIP_ID+SET_NO,data=ILTSTemp, FUN=median)
  sC = merge(sC,temp,all.x=T)
  
  #fill in number caught with avg weight_kg and abundance
  ii = which(is.na(sC$NUM_CAUGHT) & !is.na(sC$SPECCD_ID))
  spp = aggregate(cbind(WEIGHT_KG,NUM_CAUGHT)~GEAR+SPECCD_ID,data=sC[-ii,],FUN=function(x) sum(x,na.rm=T))
  spp$meanwt = spp$WEIGHT_KG / spp$NUM_CAUGHT
  
  for(i in 1:length(ii)){
    g = unique(sC$GEAR[ii[i]])
    sp = unique(sC$SPECCD_ID[ii[i]])
    if(sp %in% spp$SPECCD_ID){
    sC$NUM_CAUGHT[ii[i]] = spp[which(spp$GEAR==g & spp$SPECCD_ID==sp),'WEIGHT_KG'] / spp[which(spp$GEAR==g & spp$SPECCD_ID==sp),'meanwt']
      }
  }
  
  sM = subset(surveyMeasurements,select=c(TRIP_ID,SET_NO,FISH_ID,SPECCD_ID,FISH_LENGTH,SEX))
  sM = aggregate(FISH_ID~TRIP_ID+SET_NO+FISH_ID+SPECCD_ID+FISH_LENGTH+SEX,data=sM,FUN=function(x) length(unique(x)))
  names(sM)[ncol(sM)] = 'NUM_AT_LENGTH'
  
  sF = fishMeasurements
  names(sF)[4] = 'SEX'
  
    sM$ID = paste(sM$TRIP_ID,sM$SET_NO,sM$SPECCD_ID,sM$FISH_LENGTH,sep="-")
    sF$ID = paste(sF$TRIP_ID,sF$SET_NO,sF$SPECCD_ID,sM$FISH_LENGTH,sep="-")
    
    sF = subset(sF,ID %ni% unique(sM$ID)) #remove the duplicates from sF
    
    sFM = rbind(sF,sM)
    sFM$ID <- NULL
  
    NLM = aggregate(NUM_AT_LENGTH~TRIP_ID+SET_NO+SPECCD_ID,data=sFM,FUN=sum)
    names(NLM)[4] = 'NUM_MEASURED'
    
    sC = merge(sC,NLM,all.x=T)
    
    if(!any(is.na(sC$NUM_CAUGHT) & !is.na(sC$NUM_MEASURED))) {print('Survey measurements but not in catch table'); browser()}
    
    xx = merge(sC,sFM,all.x=T) #full dataset
    xx$ID = paste(xx$TRIP_ID,xx$SET_NO,sep="-")
    xy = subset(xx,SPECCD_ID==species)
    xS = subset(xx,unique(ID) %ni% unique(xy$ID))
    
    require(dplyr)
    xS = xS %>% dplyr::distinct(TRIP_ID,SET_NO,.keep_all = T)
    xS$SPECCD_ID = 2550
    xS$NUM_CAUGHT = xS$NUM_AT_LENGTH = xS$NUM_MEASURED = xS$WEIGHT_KG = 0
    xS$SEX = xS$FISH_LENGTH=NA
  
    xy$ID = xS$ID = NULL
    xFinal = rbind(xS,xy)
    return(xFinal)
 	}