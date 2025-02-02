#' ILTS_ITQ_All_Data
#'
#' This function pulls out all the ILTS ITQ data for all sets and species either aggregated by length and/or sex or raw data.
#' @param species Species codes from ISDB (2550 = lobster; 10 = Atlantic cod; etc)
#' @param redo_base_data if TRUE redoes all the swept area calculations and data filtering / cleanup
#' @param size specifies size class; commercial = c(82,200); recruit=c(70,82)
#' @param sex specifies sex for data; only applicable to lobster and crabs. 0 = unid; 1= male; 2= female; 3=egg carrying
#' @param aggregate if TRUE aggregates the data by trip and set based on above arguments. If no size or sex is specified its total abundance.
#' @return Data objects that contain the data for use in further analyses.
#' @examples ILTS_ITQ_All_Data(species=2550, size=c(1,200),sex=c(3),aggregate=T)
#' @export
ILTS_ITQ_All_Data <-function(species=2550,redo_base_data=F,redo_set_data=F,size = NULL, sex=NULL,aggregate=T,return_tow_tracks=F,applyGearConversion=T,biomass=T,extend_ts=T,return_base_data=F){
  require(dplyr)
  require(bio.lobster)
  require(bio.utilities)
  require(devtools)
  require(geosphere)

  outfile = file.path(project.datadirectory('bio.lobster'),'data','survey','ILTS_ITQ_all.data.rds')
  sensorfile = file.path(project.datadirectory('bio.lobster'),'data','survey','ILTS_ITQ_sensorData.rds')
  
  if(redo_base_data){
  
        lobster.db("survey")
  set_info_file = file.path(project.datadirectory('bio.lobster'),'data','survey','ILTS_ITQ_SETS.data.rds')
  if(redo_set_data){
      ic = ILTSClick
    ic$ID = paste(ic$TRIP_ID,ic$SET_NO,sep="_")
  #updating towed distance and spread stats  
  
    junk = list()
      for(i in 1:nrow(ic)){
            if(i %in% round(seq(5,nrow(ic),length.out=100))) print(i)
              v = ic[i,]
              sur = subset(surveyCatch,TRIP_ID == v$TRIP_ID & SET_NO==v$SET_NO)
              rv = unique(sur$GEAR)
              bds = c(5,ifelse(rv=='NEST',c(20),c(30)))
              v$gear = rv
              
              #use winch first and convert to GMT for matching with sensors
              v$st = strptime(unique(sur$SET_TIME),"%H%M")+(3*60*60)
              v$et = strptime(unique(sur$HAUL_TIME),"%H%M")+(3*60*60)
              
            #if touch down and lift off are good from click touch replace winch
            if(v$QUALITY_TOUCHDOWN==1) v$st= strptime(v$STARTTIME,"%H%M%S")
            if(v$QUALITY_LIFTOFF==1) v$et= strptime(v$ENDTIME,"%H%M%S")
            if(!is.na(v$STARTDATE) & !is.na(v$ENDDATE)) {if(yday(v$STARTDATE) != yday(v$ENDDATE))v$et$mday= v$et$mday +1} #if jumps over night
            
            v$yr = year(v$STARTDATE)
            
            v$nonweighted_spread = v$sensor = v$sweptArea = v$distance = NA
            
            #sensor data
            se = subset(ILTSSensor,TRIP_ID == v$TRIP_ID & SET_NO==v$SET_NO)
            se$Time = strptime(se$GPSTIME,"%H%M%S")
            se = se[order(se$GPSTIME),]
            
            se = subset(se,Time>=v$st & Time <= v$et)
            
            #if we dont have click touch data for touchdown, lift off or spread
            if(v$QUALITY_WINGSPREAD+v$QUALITY_TOUCHDOWN+v$QUALITY_LIFTOFF==0){
              se$Time = strptime(se$GPSTIME,"%H%M%S")
              se = subset(se,!is.na(se$LATITUDE))
                if(nrow(se)>10){
              se$Y = c(sapply(se$LATITUDE,fixSensorLatLon)  )
              se$X = c(sapply(se$LONGITUDE,fixSensorLatLon)*-1  )
              
              se = se[order(se$GPSTIME),]
              #distance
              v$distance <- sum(sapply(2:nrow(se), function(i) {
                distGeo(se[i - 1, c("X", "Y")], se[i, c("X", "Y")])
              }))/1000
              next
                }
              }
            #if we have decent sensor data
            if(nrow(se)>0){
              v$sensor = unique(se$SOURCE)
              
              if(all(c(nrow(se)>5, length(unique(se$GPSTIME))>5) )){
                se = subset(se,!is.na(se$LATITUDE))
                se$Y = c(sapply(se$LATITUDE,fixSensorLatLon)  )
                se$X = c(sapply(se$LONGITUDE,fixSensorLatLon)*-1  )
              
                #total distance from all GPS feed  
                #distance
                v$distance <- sum(sapply(2:nrow(se), function(i) {
                  distGeo(se[i - 1, c("X", "Y")], se[i, c("X", "Y")])
                }))/1000
                
                if(v$sensor=='MARPORT' & v$yr>2016) se = subset(se, VALIDITY=='RAW')
                if(v$sensor=='MARPORT' & v$yr==2016) se = subset(se, VALIDITY=='1000') #raw
                if(v$sensor=='MARPORT'){
                  #this section reduces the data to just segments with wing spread then caculates the distance that wingspread represents over the entire tow
                  b = subset(se,TRANSDUCERNAME %in% c('PRP','WINGSPREAD') & SENSORNAME=='DISTANCE' & SENSORVALUE>=bds[1] & SENSORVALUE<=bds[2])
                  b = b[order(b$GPSTIME),]
                  if(nrow(b)>5){
                    b$distSeg = c(0,sapply(2:nrow(b), function(i) {
                      distGeo(b[i - 1, c("X", "Y")], b[i, c("X", "Y")])
                    }))
                    b$RealDist = b$distSeg/sum(b$distSeg)*v$distance #bumped up to real towed distance
                    v$sweptArea = sum(b$RealDist*b$SENSORVALUE/1000)
                  }
                }
                
                if(v$sensor %in% c("ESONAR","NETMIND")){
                  #this section reduces the data to just segments with wing spread then caculates the distance that wingspread represents over the entire tow
                  #even though called DoorSpread is really wing spread
                  b = subset(se,TRANSDUCERNAME=="DoorSpread" & SENSORNAME %in% c("STBDDoorMaster","DoorMaster") & SENSORVALUE>=bds[1] & SENSORVALUE<=bds[2])
                  b = b[order(b$GPSTIME),]
                  
                  if(nrow(b)>5){
                    b$distSeg = c(0,sapply(2:nrow(b), function(i) {
                      distGeo(b[i - 1, c("X", "Y")], b[i, c("X", "Y")])
                    }))
                    b$RealDist = b$distSeg/sum(b$distSeg)*v$distance
                    v$sweptArea = sum(b$RealDist*b$SENSORVALUE/1000)
                  }
                }
                
              }
        
            junk[[i]] = v
            rm(list=c('v','se','bds','rv'))
            }
          }
  
        j = do.call(rbind,junk)
        j$spread = j$sweptArea/j$distance*1000 #weighted mean spread
        
        j$gearid = ifelse(j$gear=='NEST',16,21)
        j$sensorid = ifelse(j$sensor=='MARPORT',1,2)
        j$spread = ifelse(j$QUALITY_WINGSPREAD==1,j$spread,NA)
        j$sweptArea = ifelse(j$QUALITY_WINGSPREAD==1,j$sweptArea,NA)
        j$distance = ifelse(j$distance< .4,NA,j$distance)
  
  #Calc dist from Olex tracks
            jOL = subset(j,is.na(distance))
            olex_dists=data.frame(jOL[,c('TRIP_ID','SET_NO')],DistanceOlex=NA, Source=NA)
            for(i in 1:nrow(jOL)){
                sur = subset(ILTSOlex,TRIP_ID == jOL$TRIP_ID[i] & SET_NO==jOL$SET_NO[i])
                st = strptime(unique(sur$S_STDTIME),"%H:%M:%S")
                et = strptime(unique(sur$E_STDTIME),"%H:%M:%S")
                src='winch'
                ji = jOL[i,]
                if(nrow(ji)>0){
                  if(nrow(ji)>1) ji = ji[1,]
                #if touch down and lift off are good from click touch replace winch
                  if(ji$QUALITY_TOUCHDOWN==1) st= ji$st-(3*60*60)
                  if(ji$QUALITY_LIFTOFF==1) et= ji$et-(3*60*60)
                  src='click'
                }
               # if(st==et){
                #  st = strptime(unique(sur$SET_TIME),"%H%M") #+ (3*60*60)
                #  et = strptime(unique(sur$HAUL_TIME),"%H%M")#+ (3*60*60)
                #  src='winch'
                #  }
              ol = subset(ILTSOlextracks,SET_NO==jOL$SET_NO[i] & TRIP_ID==jOL$TRIP_ID[i])   
              ol$Time = strptime(ol$STDTIME,"%H:%M:%S")
              olS = subset(ol,Time>=st & Time<=et)
              if(nrow(olS)>5){
              olS = olS[order(olS$Time),]
              olex_dists$DistanceOlex[i] = sum(sapply(2:nrow(olS), function(i) {
                distGeo(olS[i - 1, c("X", "Y")], olS[i, c("X", "Y")])
              }))/1000
              olex_dists$Source[i]=src
              }
            }
  
        j1= merge(j,olex_dists,all.x=T)
        j1$distance = ifelse(is.na(j1$distance) & !is.na(j1$DistanceOlex),j1$DistanceOlex,j1$distance)
        
        surveyCatch = merge(surveyCatch,j1, by.x=c('TRIP_ID','SET_NO','YEAR'),by.y=c('TRIP_ID','SET_NO','YEAR'),all.x=T)
        surveyCatch$WEIGHT_KG = ifelse(is.na(surveyCatch$EST_DISCARD_WT),0,surveyCatch$EST_DISCARD_WT) + ifelse(is.na(surveyCatch$EST_KEPT_WT),0,surveyCatch$EST_KEPT_WT)
 
      surveyCatch$distanceWinch = sapply(1:nrow(surveyCatch), function(i) {
        distGeo(surveyCatch[i, c("SET_LONG", "SET_LAT")], surveyCatch[i, c("HAUL_LONG", "HAUL_LAT")])
      })/1000
      surveyCatch$distBias=surveyCatch$distance-surveyCatch$distanceWinch

        #  plot(surveyCatch$DEPTHM, surveyCatch$distBias,ylim=c(-1.5,.5))
        #require(mgcv)
        #b = gam(distBias~s(DEPTHM,k=3),data=subset(surveyCatch, distBias> -1.5))
        #plot(b,rug=T)

  
        ii = which(is.na(surveyCatch$distance))
        surveyCatch$distance[ii] = sapply(ii, function(i) {
          distGeo(surveyCatch[i, c("SET_LONG", "SET_LAT")], surveyCatch[i, c("HAUL_LONG", "HAUL_LAT")])
        })/1000
        
        ii = which(surveyCatch$distance>=4)
        surveyCatch$sweptArea[ii] = surveyCatch$distance[ii] = NA
        
      #windsorize long sets
          ij = aggregate(distance~YEAR+GEAR,data=subset(surveyCatch,!is.na(distance)),FUN=function(x) quantile(x,0.99))
          ik = which(is.na(surveyCatch$distance))
          for(i in 1:length(ik)){
              y = unique(surveyCatch$YEAR[ik[i]])
              g = unique(surveyCatch$GEAR[ik[i]])
              surveyCatch$distance[ik[i]] = ij[which(ij$YEAR==y & ij$GEAR==g),'distance']
              }
          
          ii = which(surveyCatch$distance<0.2)
          surveyCatch$sweptArea[ii] = surveyCatch$distance[ii] = NA
          
  
      #windsorize short hauls
          ij = aggregate(distance~YEAR+GEAR,data=subset(surveyCatch,!is.na(distance)),FUN=function(x) quantile(x,0.01))
          ik = which(is.na(surveyCatch$distance))
          for(i in 1:length(ik)){
            y = unique(surveyCatch$YEAR[ik[i]])
            g = unique(surveyCatch$GEAR[ik[i]])
            surveyCatch$distance[ik[i]] = ij[which(ij$YEAR==y & ij$GEAR==g),'distance']
          }
  
  
          ##spread fillins #not enough data for years
          ij = aggregate(spread~GEAR,data=subset(surveyCatch,!is.na(spread)),FUN=function(x) quantile(x,.5))
          ik = which(is.na(surveyCatch$spread))
          
          for(i in 1:length(ik)){
            g = unique(surveyCatch$GEAR[ik[i]])
            surveyCatch$spread[ik[i]] = ij[which(ij$GEAR==g),'spread']
          }
  
        ik = which(is.na(surveyCatch$sweptArea))
        surveyCatch$sweptArea[ik] =surveyCatch$distance[ik] * surveyCatch$spread[ik]/1000 
        saveRDS(surveyCatch, file= set_info_file)
  }
  surveyCatch = readRDS(file= set_info_file)
        sC = surveyCatch
      	sC = subset(sC, HAULCCD_ID %in% c(1))
      	
      	
      	sC = subset(sC,select=c(TRIP_ID, SET_NO, YEAR, VESSEL_NAME, LFA, GEAR, FISHSET_ID, STATION, SPECCD_ID, NUM_CAUGHT, SET_LAT, SET_LONG, SET_DEPTH, SET_TIME, SET_DATE, SET_ID, STARTTIME, ENDTIME, DEPTHM, gear, distance, sweptArea, sensor, spread, WEIGHT_KG))
        sC$ID = paste(sC$TRIP_ID,sC$SET_NO,sep="_")
      	ILTSTemp$ID = paste(ILTSTemp$TRIP_ID,ILTSTemp$SET_NO,sep="_")
        	sCa = as.data.frame(unique(subset(sC,!is.na(STARTTIME) | !is.na(ENDTIME), select=c(ID,STARTTIME,ENDTIME))))
        	ot = list()
        	for(i in 1:nrow(sCa)){
        	  tt = subset(ILTSTemp,ID==sCa$ID[i])
        	  ww = sCa[i,]
        	  if(nrow(tt)<5) next
        	  tt$st = strptime((tt$UTCTIME),"%H%M%S")
        	  ww$st = strptime(sCa$STARTTIME[i],"%H%M%S")
        	  ww$et = strptime(sCa$ENDTIME[i],"%H%M%S")
        	  
        	  tt1 = subset(tt,st>=ww$st & st<=ww$et)
        	  ww$temp = median(tt1$TEMPC,na.rm=T)
        	  ot[[i]] = ww
        	}
        	temp = as.data.frame(do.call(rbind,ot))
        	sC = merge(sC,temp[,c('ID','temp')],all.x=T)
          sC$ID = NULL
          
          
          #fill in number caught with avg weight_kg and weight
          ii = which(is.na(sC$NUM_CAUGHT) & !is.na(sC$SPECCD_ID))
          spp = aggregate(cbind(WEIGHT_KG,NUM_CAUGHT)~GEAR+SPECCD_ID,data=sC[-ii,],FUN=function(x) sum(x,na.rm=T))
          spp$meanwt = spp$WEIGHT_KG / spp$NUM_CAUGHT
          
          for(i in 1:length(ii)){
            g = unique(sC$GEAR[ii[i]])
            sp = unique(sC$SPECCD_ID[ii[i]])
            wt = unique(sC$WEIGHT_KG[ii[i]])
            if(sp %in% spp$SPECCD_ID){
            sC$NUM_CAUGHT[ii[i]] = round(wt / spp[which(spp$GEAR==g & spp$SPECCD_ID==sp),'meanwt'])
              }
          }
        
          sM = subset(surveyMeasurements,select=c(TRIP_ID,SET_NO,FISH_ID,SPECCD_ID,FISH_LENGTH,SEX))
          sM = aggregate(FISH_ID~TRIP_ID+SET_NO+FISH_ID+SPECCD_ID+FISH_LENGTH+SEX,data=sM,FUN=function(x) length(unique(x)))
          names(sM)[ncol(sM)] = 'NUM_AT_LENGTH'
          
        sF = fishMeasurements
        names(sF)[5] = 'SEX'
        
          sM$ID = paste(sM$TRIP_ID,sM$SET_NO,sM$SPECCD_ID,sM$FISH_LENGTH,sep="-")
          sF$ID = paste(sF$TRIP_ID,sF$SET_NO,sF$SPECCD_ID,sM$FISH_LENGTH,sep="-")
          
          sF = subset(sF,ID %ni% unique(sM$ID)) #remove the duplicates from sF
          if(any(names(sF)=='TRIP')) sF$TRIP = NULL
          sFM = rbind(sF,sM)
          sFM$ID <- NULL
        
        NLM = aggregate(NUM_AT_LENGTH~TRIP_ID+SET_NO+SPECCD_ID,data=sFM,FUN=sum)
        names(NLM)[4] = 'NUM_MEASURED'
        
        sC = merge(sC,NLM,all.x=T)
        sC$sa = sC$distance*sC$spread/1000
        
        ii = which(abs(sC$sweptArea - sC$sa)>0.001)
        
        sC$sweptArea[ii] = sC$sa[ii]
        sC$sa = NULL
        
    #if(!any(is.na(sC$NUM_CAUGHT) & !is.na(sC$NUM_MEASURED))) {print('Survey measurements but not in catch table'); browser()}
    
            xx = merge(sC,sFM,all.x=T) #full dataset
            xx$FISH_LENGTH = round(xx$FISH_LENGTH)
            xx$SEX[which(is.na(xx$SEX))]<- 0
           xFinal = xx
           xFinal$NUM_AT_LENGTH[which(is.na(xFinal$NUM_AT_LENGTH))] <- 0
           xFinal$NUM_MEASURED[which(is.na(xFinal$NUM_MEASURED))] <- 0
            xFinal$FISH_LENGTH[which(is.na(xFinal$FISH_LENGTH))] <- 0
            xFinal$PRORATED_NUM_AT_LENGTH = xFinal$NUM_AT_LENGTH * xFinal$NUM_CAUGHT/xFinal$NUM_MEASURED 
            xFinal$PRORATED_NUM_AT_LENGTH[which(is.na(xFinal$PRORATED_NUM_AT_LENGTH))] <- 0
            xFinal$SA_CORRECTED_PRORATED_N = xFinal$PRORATED_NUM_AT_LENGTH / xFinal$sweptArea #n/km2
        
            xFinal$SA_CORRECTED_TOTAL_N = xFinal$NUM_CAUGHT / xFinal$sweptArea #n/km2
            xFinal$SA_CORRECTED_TOTAL_WT = xFinal$WEIGHT_KG / xFinal$sweptArea #kg/km2
            x = xFinal
            x$SET_DEPTH = ifelse(is.na(x$SET_DEPTH),x$DEPTHM,x$SET_DEPTH)
            
            if(any(is.na(x$SET_DEPTH))){
              p = bio.lobster::load.environment()
              p = spatial_parameters(type='canada.east')
              
              attr(a,'projection') = "LL"
              x = lonlat2planar(x,input_names=c('SET_LONG','SET_LAT'),proj.type =  "lambert.conic.canada.east")
              
              #allocating depth to location
              ba = lobster.db('bathymetry')
              locsmap = match( 
                array_map( "xy->1", x[,c("plon","plat")], gridparams=p$gridparams ), 
                array_map( "xy->1", ba[,c("plon","plat")], gridparams=p$gridparams ) )
              x$Depth = ba$z[locsmap]
              x$SET_DEPTH =ifelse(is.na(x$SET_DEPTH),x$Depth,x$SET_DEPTH)
              x$plon = x$plat = x$Depth = NULL
            }
            
            xFinal=x
            
            saveRDS(xFinal,file=outfile)

              }
  ##using saved file  
    x = readRDS(outfile)
    if(return_base_data) return(x)
    if(return_tow_tracks) return(readRDS(sensorfile))
    x$ID = paste(x$TRIP_ID,x$SET_NO,sep="-")
   if(is.null(size) & is.null(sex)) xy = subset(x,SPECCD_ID==species)
   if(!is.null(size) & is.null(sex)) xy = subset(x,SPECCD_ID==species & FISH_LENGTH>=size[1] & FISH_LENGTH<=size[2])
   if(!is.null(size) & !is.null(sex)) xy = subset(x,SPECCD_ID==species & FISH_LENGTH>=size[1] & FISH_LENGTH<=size[2] & SEX %in% sex)
   
    #vessel conversions
    
              iv = subset(xy, GEAR=='280 BALLOON')
              ivi = subset(xy, GEAR!='280 BALLOON')
    if(species  %ni% c(10, 2550)) stop('vessel corrections not implemented for anything but lobster and cod')          
    if(species  ==2550)   {  print('Length based corrections')
                              sou = readRDS(file=file.path(bio.directory,'bio.lobster.data','survey_corrections','summarybootRhoNestBall_FINAL.rds'))
                              nlC = 2.282
                              #non length correct conv (predict(fit)[1]/(1-predict(fit)[1])) = 2.282
                              
            }
    if(species  ==10)   { 
                print('Length aggregated corrections')
                nlC =  0.5877103
                sou = data.frame(Length=unique(iv$FISH_LENGTH), Median=nlC)
                
              }
              
    
        ivs = merge(iv,sou[,c('Length','Median')], by.x=c('FISH_LENGTH'),by.y=c('Length'),all.x=T)
      ivs$Median = ifelse(ivs$FISH_LENGTH==0,nlC,ivs$Median)
      fl = distinct(subset(ivs,is.na(Median)),FISH_LENGTH)[,1]
      i = max(subset(ivs,!is.na(Median))$FISH_LENGTH)
      fl = max(ivs$Median[which(ivs$FISH_LENGTH==i)])
      ivs$Median[which(ivs$FISH_LENGTH>i & is.na(ivs$Median))] <- fl
      ivs$SA_CORRECTED_PRORATED_N = ivs$SA_CORRECTED_PRORATED_N * ivs$Median
      if(species==2550) ivs$wt = lobLW(ivs$FISH_LENGTH, sex= ivs$SEX)/1000
      if(species==10)  ivs$wt = (0.007005*ivs$FISH_LENGTH^	3.071947)/1000
      ivs$SA_CORRECTED_WTs =ivs$SA_CORRECTED_PRORATED_N* ivs$wt 
      
      ivsa = aggregate(cbind(SA_CORRECTED_PRORATED_N,SA_CORRECTED_WTs)~ID,data=ivs,FUN=sum)
      names(ivsa)[2:3]=c('N','WT')
      ivs = merge(ivs,ivsa)
      ivs$SA_CORRECTED_TOTAL_N = ivs$N
      ivs$SA_CORRECTED_TOTAL_WT = ivs$WT
      ivs = subset(ivs, select=c(-Median,-N,-WT, -wt,-SA_CORRECTED_WTs))
      xy = bind_rows(ivi,ivs)
      
      ##need to find the proportion of total catch within size range this calculates totals without filtering to sex and size-- required for extending TS
        xy2 = subset(x,SPECCD_ID==species & ID %in% unique(xy$ID) ) #same sets
      
        iv = subset(xy2, GEAR=='280 BALLOON')
        ivi = subset(xy2, GEAR!='280 BALLOON')
        
        sou = readRDS(file=file.path(bio.directory,'bio.lobster.data',"survey_corrections", 'summarybootRhoNestBall_FINAL.rds'))
        if(species  %ni% c(10, 2550)) stop('vessel corrections not implemented for anything but lobster and cod')          
        if(species  ==2550)   {  print('Length based corrections')
          sou = readRDS(file=file.path(bio.directory,'bio.lobster.data','survey_corrections','summarybootRhoNestBall_FINAL.rds'))
          nlC = 2.282
          #non length correct conv (predict(fit)[1]/(1-predict(fit)[1])) = 2.282
          
        }
        if(species  ==10)   { 
          print('Length aggregated corrections')
          nlC =  0.5877103
          sou = data.frame(Length=unique(iv$FISH_LENGTH), Median=nlC)
          
        }
        
        ivs = merge(iv,sou[,c('Length','Median')], by.x=c('FISH_LENGTH'),by.y=c('Length'),all.x=T)
        #non length correct conv (predict(fit)[1]/(1-predict(fit)[1])) = 2.282
        ivs$Median = ifelse(ivs$FISH_LENGTH==0,nlC,ivs$Median)
        fl = distinct(subset(ivs,is.na(Median)),FISH_LENGTH)[,1]
        i = max(subset(ivs,!is.na(Median))$FISH_LENGTH)
        fl = max(ivs$Median[which(ivs$FISH_LENGTH==i)])
        ivs$Median[which(ivs$FISH_LENGTH>i & is.na(ivs$Median))] <- fl
        ivs$SA_CORRECTED_PRORATED_N = ivs$SA_CORRECTED_PRORATED_N * ivs$Median
        
        if(species==2550) ivs$wt = lobLW(ivs$FISH_LENGTH, sex= ivs$SEX)/1000
        if(species==10)  ivs$wt = (0.007005*ivs$FISH_LENGTH^	3.071947)/1000
        ivs$SA_CORRECTED_WTs =ivs$SA_CORRECTED_PRORATED_N* ivs$wt 
        
        ivsa = aggregate(cbind(SA_CORRECTED_PRORATED_N,SA_CORRECTED_WTs)~ID,data=ivs,FUN=sum)
        names(ivsa)[2:3]=c('N','WT')
        ivs = merge(ivs,ivsa)
        ivs$SA_CORRECTED_TOTAL_N = ivs$N
        ivs$SA_CORRECTED_TOTAL_WT = ivs$WT
        ivs = subset(ivs, select=c(-Median,-N,-WT, -wt,-SA_CORRECTED_WTs))
        xy2 = bind_rows(ivi,ivs)
    
    
   if(aggregate){
     if(biomass){
       print('biomass')
       xy$wt = lobLW(xy$FISH_LENGTH, sex= xy$SEX)/1000
       if(species==2550) xy$wt = lobLW(xy$FISH_LENGTH, sex= xy$SEX)/1000
       if(species==10)  xy$wt = (0.007005*xy$FISH_LENGTH^	3.071947)/1000
       
       xy$SA_CORRECTED_PRORATED_N =xy$SA_CORRECTED_PRORATED_N* xy$wt 
       xy$wt <- NULL
       xy2$wt = lobLW(xy2$FISH_LENGTH, sex= xy2$SEX)/1000
       xy2$SA_CORRECTED_PRORATED_N =xy2$SA_CORRECTED_PRORATED_N* xy2$wt 
       xy2$wt <- NULL
     }
     
     #pruned to size and/or sex
      xy = aggregate(cbind(NUM_AT_LENGTH,PRORATED_NUM_AT_LENGTH,SA_CORRECTED_PRORATED_N)~ID,data=xy,FUN=sum) 
      xS = subset(x,(ID) %in% unique(xy$ID))
      xS = xS %>% dplyr::distinct(TRIP_ID,SET_NO,.keep_all = T)
      xS$SPECCD_ID = species
      xS$SA_CORRECTED_PRORATED_N = xS$PRORATED_NUM_AT_LENGTH= xS$SEX = xS$FISH_LENGTH = xS$NUM_CAUGHT = xS$NUM_AT_LENGTH = xS$NUM_MEASURED = xS$WEIGHT_KG = NULL
      xy = merge(xS,xy)
    
    #all sizes and sexes
      xy2 = aggregate(cbind(NUM_AT_LENGTH,PRORATED_NUM_AT_LENGTH,SA_CORRECTED_PRORATED_N)~ID,data=xy2,FUN=sum) 
      xS2 = subset(x,(ID) %in% unique(xy2$ID))
      xS2 = xS2 %>% dplyr::distinct(TRIP_ID,SET_NO,.keep_all = T)
      xS2$SPECCD_ID = species
      xS2$SA_CORRECTED_PRORATED_N = xS2$PRORATED_NUM_AT_LENGTH= xS2$SEX = xS2$FISH_LENGTH = xS2$NUM_CAUGHT = xS2$NUM_AT_LENGTH = xS2$NUM_MEASURED = xS2$WEIGHT_KG = NULL
      xy2 = merge(xS2,xy2)
      xy2 = bio.utilities::rename.df(xy2,'SA_CORRECTED_PRORATED_N','tot')
      
      xy = merge(xy,xy2[,c('ID','tot')])
      xy$prop = xy$SA_CORRECTED_PRORATED_N/xy$tot
      
      #develop the model to predict proportion of animals in a size class for filling in years with just abundance or biomass
      if(extend_ts){
        require(sf)
        u=0
        if(all(size==c(70,82))) {ou = readRDS(file.path( bio.directory,'bio.lobster.data', 'survey_corrections','modelled_recruit_Proportions_34-38.rds'));u=1}
        if(all(size==c(82,300))){ou = readRDS(file.path( bio.directory,'bio.lobster.data','survey_corrections' ,'modelled_commercial_Proportions_34-38.rds'));u=1}
        if(all(size==c(70,300))){ou = readRDS(file.path( bio.directory,'bio.lobster.data','survey_corrections' ,'modelled_commercial_and_recruit_Proportions_34-38.rds'));u=1}
        if(all(size==c(82,300))&biomass){ou = readRDS(file.path( bio.directory,'bio.lobster.data','survey_corrections' ,'modelled_commercial_Proportions_wt_34-38.rds'));u=1; biomass=F}
        
        if(u==0) {
          print('models need to be developed for this size range')
          break
        }
            ou = st_as_sf(ou)
      
          xP = subset(x,SPECCD_ID==2550 & ID %ni% unique(xy$ID) & FISH_LENGTH==0) # this is just for sets with no length data
          xPs = st_as_sf(xP,coords = c('SET_LONG','SET_LAT'),crs=4326)
          ss = st_nearest_feature(xPs,ou)
          ds = st_distance(xPs,ou[ss,],by_element=T)
          st_geometry(ou) = NULL
          xPs$prop = ou$Modelled_Proportion[ss]
          xPs$SET_LONG = st_coordinates(xPs)[,1]
          xPs$SET_LAT = st_coordinates(xPs)[,2]
          st_geometry(xPs) <- NULL
          xP = as_tibble(xPs)
           if(biomass){print('extending time series only works on numbers'); stop()}
          if(!biomass) xP$SA_CORRECTED_PRORATED_N = xP$SA_CORRECTED_TOTAL_N* xP$prop
          xP=subset(xP,select=c(-prop,-PRORATED_NUM_AT_LENGTH,-NUM_CAUGHT,-NUM_AT_LENGTH,-SA_CORRECTED_TOTAL_WT,-WEIGHT_KG,-NUM_MEASURED,-SA_CORRECTED_TOTAL_N,-FISH_LENGTH,-SEX))
          xy=subset(xy,select=c(-prop,-tot, -PRORATED_NUM_AT_LENGTH,-NUM_AT_LENGTH,-SA_CORRECTED_TOTAL_WT,-SA_CORRECTED_TOTAL_N))
          if(any(unique(xP$ID) %in% unique(xy$ID))) browser() #there is an issues with sets
          xy = rbind(xP,xy)
          }
         } 
   
    
    #bring in the zeros
    xS = subset(x,ID %ni% unique(xy$ID))
    xS = xS %>% dplyr::distinct(TRIP_ID,SET_NO,.keep_all = T)
    xS$SPECCD_ID = species
    xS$SA_CORRECTED_TOTAL_N = xS$SA_CORRECTED_TOTAL_WT = xS$SA_CORRECTED_PRORATED_N = xS$PRORATED_NUM_AT_LENGTH= xS$SEX = xS$FISH_LENGTH = xS$NUM_CAUGHT = xS$NUM_AT_LENGTH = xS$NUM_MEASURED = xS$WEIGHT_KG = 0
   
    if(aggregate){ xS$SEX = xS$FISH_LENGTH  = xS$NUM_CAUGHT = xS$WEIGHT_KG =  xS$NUM_MEASURED = xS$NUM_AT_LENGTH = xS$PRORATED_NUM_AT_LENGTH = xS$SA_CORRECTED_TOTAL_N = xS$SA_CORRECTED_TOTAL_WT= NULL}
   # if(!aggregate){stop('not complete')}
     if(!aggregate){ ###this needs to be checked based on all the work above May22024
      xS = x %>% dplyr::distinct(TRIP_ID,SET_NO,.keep_all = T)
      xS$SPECCD_ID = species
      xS$SA_CORRECTED_TOTAL_N = xS$SA_CORRECTED_TOTAL_WT = xS$SA_CORRECTED_PRORATED_N = xS$PRORATED_NUM_AT_LENGTH= xS$SEX = xS$FISH_LENGTH = xS$NUM_CAUGHT = xS$NUM_AT_LENGTH = xS$NUM_MEASURED = xS$WEIGHT_KG = NULL
      ss = aggregate(TRIP_ID~FISH_LENGTH,data=subset(xy,SEX<3 | is.na(SEX)),FUN=length)
      v = unique(xy$SEX[which(xy$SEX<3)])
      s1 = seq(min(ss$FISH_LENGTH),max(ss$FISH_LENGTH),1)
      s1 = expand.grid(v,s1)
      if(any(unique(xy$SEX)==3)){
        #shorter vector for berried
        ss = aggregate(TRIP_ID~FISH_LENGTH,data=subset(xy,SEX==3),FUN=length)
        s2 = seq(min(ss$FISH_LENGTH),max(ss$FISH_LENGTH),1)
        v=3
        s2 = expand.grid(v,s2)
        s1 = as.data.frame(rbind(s1,s2))
      }
      names(s1)=c('SEX','FISH_LENGTH')
      oo = list()
      for(i in 1:nrow(s1)){
          o = xS
          o$SEX = s1[i,'SEX']
          o$FISH_LENGTH = s1[i,'FISH_LENGTH']
          oo[[i]] = o
      }
      xS = bind_rows(oo)
      xS$FID = paste(xS$FISHSET_ID,xS$FISH_LENGTH,xS$SEX)
      xy$FID = paste(xy$FISHSET_ID,xy$FISH_LENGTH,xy$SEX)
      xS = subset(xS,FID %ni% unique(xy$FID))
      xS$SA_CORRECTED_PRORATED_N = 0
      xy$FID = xS$FID = NULL
      }

    xy$ID = xS$ID = NULL

      if( aggregate) xy = subset(xy,select=c(TRIP_ID, SET_NO, SPECCD_ID, YEAR, VESSEL_NAME, LFA, GEAR, FISHSET_ID, STATION, SET_LAT, SET_LONG, SET_DEPTH, SET_TIME, SET_DATE, SET_ID, STARTTIME, ENDTIME, DEPTHM, gear, distance, sweptArea, sensor, spread, temp, SA_CORRECTED_PRORATED_N))
    if(! aggregate) xy = subset(xy,select=c(TRIP_ID, SET_NO, SPECCD_ID, YEAR, VESSEL_NAME, LFA, GEAR, FISHSET_ID, STATION, SET_LAT, SET_LONG, SET_DEPTH, SET_TIME, SET_DATE, SET_ID, STARTTIME, ENDTIME, DEPTHM, gear, distance, sweptArea, sensor, spread, temp, SA_CORRECTED_PRORATED_N,FISH_LENGTH,SEX))
    
    xFinal = bind_rows(xS,xy)
 
    
    return(xFinal)
    }