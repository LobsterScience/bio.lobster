#' @export

compileAbundPresAbs <- function(redo=F,size=T){
      if(redo){
    #offshore logs
            lobster.db('logs41') #make sure to do a database recapture through logs41.redo before moving on
            
            logs41 = rename.df(logs41,c('FV_FISHED_DATETIME'),c('DATE_FISHED'))
            
            logs41$yr = year(logs41$DATE_FISHED) #2002 to present
            ziff41$yr = year(ziff41$DATE_FISHED) #1995 to 2001
            ziff41$DDLON = ziff41$DDLON * -1
            
            off41$yr  = year(off41$DATE_FISHED) #1981 to 1994
            off41$WEIGHT_KG = off41$ADJ_LOB_LBS/2.205
            ziff41$WEIGHT_KG = ziff41$EST_WEIGHT_LOG_LBS/2.205
            logs41$WEIGHT_KG = logs41$ADJCATCH/2.205
            logs41$DEPTH = ziff41$DEPTH = NA
            ziff41$LICENCE_ID = ziff41$ADJCATCH = off41$ADJ_LOB_LBS = ziff41$EST_WEIGHT_LOG_LBS= off41$OFFAREA = off41$LOB_EST_LBS = ziff41$CAPTAIN = logs41$OFFAREA = NULL 
            logs41$LOG_EFRT_STD_INFO_ID = logs41$CAPTAIN = logs41$LICENCE_ID = logs41$EST_WEIGHT_LOG_LBS = logs41$ADJCATCH = NULL
            
            a41 = rbind(off41,ziff41,logs41)
            a41$SOURCE = 'FISHING_LOGS_OFFSHORE'
            a41$MON_DOC_ID = a41$VR_NUMBER = a41$VESSEL_NAME = NULL
            a41 = rename.df(a41,'NUM_OF_TRAPS','OFFSET')
            a41$OFFSET_METRIC = 'Number of traps'
            a41$timestamp = as.POSIXct(a41$DATE_FISHED,tz='America/Halifax',origin=lubridate::origin)
            a41$timestamp = with_tz(a41$timestamp,"UTC")
            a41$dyear = lubridate::decimal_date(a41$timestamp)- lubridate::year(a41$timestamp)
            
            
            off41_LOGS = a41
            off41_LOGS = rename.df(off41_LOGS,c('DDLAT','DDLON','DATE_FISHED','yr','dyear'),c('LATITUDE','LONGITUDE','DATE','YEAR','DYEAR'))
            off41_LOGS = subset(off41_LOGS,select=c(DATE,LONGITUDE,LATITUDE,OFFSET,YEAR,SOURCE,OFFSET_METRIC,DYEAR,WEIGHT_KG))
            off41_LOGS$LONGITUDE = ifelse(off41_LOGS$LONGITUDE>0,off41_LOGS$LONGITUDE*-1,off41_LOGS$LONGITUDE)
            off41_LOGS$Gear = 'Commercial'
            off41_LOGS = subset(off41_LOGS, !is.na(LONGITUDE) & !is.na(WEIGHT_KG))
            x = makePBS(off41_LOGS,polygon=F)
            LFAs<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","LFAPolys.csv"))
            l41=subset(LFAs,PID==41)
            attr(l41,'projection') <- 'LL'
            ee = findPolys(x,l41,maxRows = nrow(x),includeBdry = T)
            off41_LOGS = subset(x,EID %in% ee$EID)
            off41_LOGS$id = paste(off41_LOGS$X, off41_LOGS$Y,sep='-')
            off41_LOGS = off41_LOGS[-which(off41_LOGS$id %in% c('-61.25-41.25','-55.9768-42.1087','-62.4353-42.1467','-60.2333-41.25')),]
            off41_LOGS = off41_LOGS[-which(off41_LOGS$DATE=='1996-10-10 23:00:00'),]
            

  #At Sea Samples
            
            lobster.db('atSea')
            a = atSea
            a$UID = paste(a$TRIPNO, a$STRINGNO,a$STARTDATE, a$LATITUDE,a$LONGITUDE, a$TRAPNO,sep="-")
            a$SPECIESCODE = ifelse(is.na(a$SPECIESCODE),0,a$SPECIESCODE)
            a$Berried = ifelse(a$SPECIESCODE == 2550 & a$SEX %in% 3,1,0)
            a$Lobster = ifelse(a$SPECIESCODE == 2550,1,0)
            a$Legal = ifelse(a$SPECIESCODE == 2550 & a$CARLENGTH > 82 & a$SEX %in% 1:2,1,0)
            a$Empty = ifelse(a$SPECIESCODE == 0,1,0)
            a$NonLobster = ifelse(a$SPECIESCODE %ni% c(0,2550), 1,0)
            
            a1 = subset(a,SPECIESCODE %in% c(2550))
            sc1=seq(13,253,by=5)
            a1$SZ = sc1[cut(a1$CARLENGTH,sc1,labels=F)]
            a1$SP_SZ = a1$SZ
            a1$P=1
            aa = aggregate(P~UID+SP_SZ,data=a1,FUN=sum)
            bb = reshape(aa[,c('UID','SP_SZ','P')],idvar='UID',timevar='SP_SZ', direction='wide')
            bb = na.zero(bb)
            
            cc = aggregate(cbind(NonLobster,Empty,Legal, Lobster,Berried,CALWT)~UID+LATITUDE+LONGITUDE+STARTDATE,data=a,FUN=sum)
            cc = subset(cc,Empty<2)
            dd = merge(cc,bb,by='UID',all.x=T)     
            dd$ID = 1:nrow(dd)
            dd$UID = NULL
            dd$OFFSET=1
            dd$OFFSET_METRIC = 'Number of traps'
            dd$timestamp = as.POSIXct(dd$STARTDATE,tz='America/Halifax',origin=lubridate::origin)
            dd$timestamp = with_tz(dd$timestamp,"UTC")
            dd$dyear = lubridate::decimal_date(dd$timestamp)- lubridate::year(dd$timestamp)
            dd$yr = year(dd$timestamp)
            dd$WEIGHT_KG = dd$CALWT/1000
            dd = rename.df(dd,c('STARTDATE','yr','dyear'),c('DATE','YEAR','DYEAR'))
            
            i = which(dd$LONGITUDE>-50 & dd$LATITUDE>50)
            dd$x=dd$LONGITUDE
            dd$y=dd$LATITUDE
            dd$LATITUDE[i] = dd$x[i]*-1
            dd$LONGITUDE[i] = dd$y[i]*-1
            
            dd$SOURCE = 'AT_SEA_SAMPLES'
            dd$timestamp = dd$CALWT = dd[,'P.2550-NA']=NULL
            dd$LONGITUDE = ifelse(dd$LONGITUDE>0, dd$LONGITUDE*-1,dd$LONGITUDE)
            dd1 = makePBS(dd,polygon=F)
            dd1 = subset(dd1,Y>40 & Y<50 & X> -68 & X< -55)
            coast<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","gshhs",paste0("shoreline",'HR',".csv")))
            d2 = PBSmapping::findPolys(dd1,coast,includeBdry = F, maxRows = nrow(dd1))
            dd1 = dd1[-which(dd1$EID %in% d2$EID),]
            dd1 = dd1[-which(dd1$X> -62 & dd1$X< -61 & dd1$Y>45.6),]
            dd1 = dd1[-which(dd1$X> -65.5 & dd1$Y< 42),]
            dd1 = dd1[-which(dd1$X> -61.5 & dd1$Y< 45),]
            dd1 = dd1[-which(dd1$X< -62& dd1$Y> 45.9),]
            dd1 = dd1[-which(dd1$X> -59.5),]
            dd1 = dd1[-which(dd1$Y> 47.5),]
            dd1 = dd1[-which(dd1$Y< 42.2 & dd1$X> -62.5),]
            dd1 = dd1[-which(dd1$Y< 45.3 & dd1$X> -60.8),]
            dd = dd1
            dd$LONGITUDE = dd$X
            dd$LATITUDE = dd$Y
            dd$X.1 = dd$Y.1 = NULL
            ddPrune = subset(dd,select=c(DATE,LONGITUDE,LATITUDE,OFFSET,SOURCE,OFFSET_METRIC,DYEAR,WEIGHT_KG,YEAR, Lobster,Berried,Legal))
            ddSize = dd
            ddPrune$Gear = 'Commercial'
            ddSize$Gear = 'Commercial'
            
  #FSRS         
            lobster.db('fsrs')
            fsrs$Empty = ifelse(fsrs$LOBSTER_NO==0,1,0)
            fsrs$Berried = ifelse(fsrs$SEX==3,1,0)
            fsrs$LONGITUDE=fsrs$LONG_DD
            fsrs$LATITUDE=fsrs$LAT_DD
            fsrs$YEAR = fsrs$HAUL_YEAR
            j = ceiling(seq(2.5,135,by=5))
            i = j[seq(1,27,by=2)]
            i=c(i[1:8],78,i[9:14])
            jj = which(fsrs$SYEAR>=2020)
            jk = which(!is.na(fsrs$SIZE_CD) & fsrs$SYEAR>=2020)
            jkk = which(fsrs$SIZE_CD> 0 & fsrs$SYEAR>=2020)
            jj = c(jkk,jk)
            
            ikk = which(!is.na(fsrs$SIZE_CD) & fsrs$SIZE_CD %in% 1:15 & fsrs$SYEAR<2020)
            ii = ikk
            
            ikl = which(!is.na(fsrs$SIZE_CD) & fsrs$LFA==36 & fsrs$SYEAR==2019)
            
            
            fsrs$CL = NA    
            fsrs$CL[jj] <-  j[fsrs$SIZE_CD[jj]]
            fsrs$CL[ii] <-  i[fsrs$SIZE_CD[ii]]
            fsrs$CL[ikl] <-  j[fsrs$SIZE_CD[ikl]]
            
            fsrs$CALC_WT = lobLW(CL=fsrs$CL,sex=fsrs$SEX,fsrs=F)
            
            fsrs$UID = paste(fsrs$VESSEL_CD,fsrs$HAUL_DATE, fsrs$RECORD_NUMBER,fsrs$TRAP_NO, fsrs$LATITUDE, fsrs$LONGITUDE,sep="-")
            Nl = aggregate(cbind(LOBSTER_NO,Empty)~UID+HAUL_DATE+LATITUDE+LONGITUDE,data=fsrs,FUN=max)
            WL = aggregate(cbind(CALC_WT,Berried,SHORT)~UID+HAUL_DATE+LATITUDE+LONGITUDE,data=fsrs,FUN=function(x) sum(x,na.rm=T))
            
            nwl = merge(Nl,WL,all.x=T)
            
            fsrs$P=1
            aa = aggregate(P~UID+CL,data=subset(fsrs, !is.na(CL) | CL>0),FUN=sum)
            bb = reshape(aa[,c('UID','CL','P')],idvar='UID',timevar='CL', direction='wide')
            bb = na.zero(bb)
            
            nwlp = merge(nwl,bb,all.x=T)
            nwlp = na.zero(nwlp)
            
            nwlp$OFFSET=1
            nwlp$OFFSET_METRIC = 'Number of traps'
            nwlp$SOURCE = 'FSRS_RECRUITMENT_TRAPS'
            nwlp$YEAR = year(nwlp$HAUL_DATE)
            nwlp$DATE = nwlp$HAUL_DATE
            nwlp$WEIGHT_KG = nwlp$CALC_WT/1000
            nwlp$timestamp = as.POSIXct(nwlp$DATE,tz='America/Halifax',origin=lubridate::origin)
            nwlp$timestamp = with_tz(nwlp$timestamp,"UTC")
            nwlp$DYEAR = lubridate::decimal_date(nwlp$timestamp)- lubridate::year(nwlp$timestamp)
            nwlp$Lobster = nwlp$LOBSTER_NO
            nwlp$Legal = nwlp$Lobster - nwlp$SHORT
            nwlp$UID <- nwlp$HAUL_DATE <- nwlp$LOBSTER_NO <- nwlp$CALC_WT <- nwlp$SHORT <- nwlp$timestamp <- NULL
            fsrsPrune = subset(nwlp,select=c(DATE, YEAR, LONGITUDE, LATITUDE,WEIGHT_KG, OFFSET, SOURCE, OFFSET_METRIC,DYEAR,Lobster, Berried,Legal  ))
            fsrsSize  = nwlp
            fsrsPrune$Gear = 'Recruitment'
            fsrsSize$Gear = 'Recruitment'
  #FSRS commercial samples
            lobster.db('fsrs.commercial.samples')
            fsrs.comm$LONGITUDE=fsrs.comm$X
            fsrs.comm$LATITUDE=fsrs.comm$Y
            fsrs.comm$YEAR = year(fsrs.comm$SDATE)
            j = ceiling(seq(2.5,135,by=5))
            i = j[seq(1,27,by=2)]
            i=c(i[1:8],78,i[9:14])
            fsrs.comm$CL <- ifelse(is.na(fsrs.comm$Size) | fsrs.comm$Size== 0, NA, i[fsrs.comm$Size])
            fsrs.comm$CALC_WT = lobLW(CL=fsrs.comm$CL,sex=fsrs.comm$Sex,fsrs=F)
            fsrs.comm$EMPTY = ifelse(fsrs.comm$Lobster.Number==0,1,0)
            fsrs.comm$UID = paste(fsrs.comm$Vessel.Code,fsrs.comm$SDATE,fsrs.comm$Record.Number, fsrs.comm$Trap.Number, fsrs.comm$LATITUDE, fsrs.comm$LONGITUDE,sep="-")
            Nl = aggregate(cbind(Lobster.Number,EMPTY)~UID+SDATE+LATITUDE+LONGITUDE,data=fsrs.comm,FUN=max)
            WL = aggregate(cbind(CALC_WT,Berried,Short)~UID+SDATE+LATITUDE+LONGITUDE,data=fsrs.comm,FUN=function(x) sum(x,na.rm=T))
            
            nwl = merge(Nl,WL,all.x=T)
            
            fsrs.comm$P=1
            aa = aggregate(P~UID+CL,data=subset(fsrs.comm, !is.na(CL) | CL>0),FUN=sum)
            bb = reshape(aa[,c('UID','CL','P')],idvar='UID',timevar='CL', direction='wide')
            bb = na.zero(bb)
            
            nwlp = merge(nwl,bb,all.x=T)
            nwlp = na.zero(nwlp)
            
            nwlp$OFFSET=1
            nwlp$OFFSET_METRIC = 'Number of traps'
            nwlp$SOURCE = 'FSRS_COMMERCIAL_TRAPS'
            nwlp$YEAR = year(nwlp$SDATE)
            nwlp$DATE = nwlp$SDATE
            nwlp$WEIGHT_KG = nwlp$CALC_WT/1000
            nwlp$timestamp = as.POSIXct(nwlp$DATE,tz='America/Halifax',origin=lubridate::origin)
            nwlp$timestamp = with_tz(nwlp$timestamp,"UTC")
            nwlp$DYEAR = lubridate::decimal_date(nwlp$timestamp)- lubridate::year(nwlp$timestamp)
            nwlp$Lobster = nwlp$Lobster.Number
            nwlp$Legal = nwlp$Lobster - nwlp$Short
            nwlp$UID <- nwlp$SDATE <- nwlp$Lobster.Number <- nwlp$CALC_WT <- nwlp$Short <- nwlp$timestamp <- NULL
            nwlp = subset(nwlp, LONGITUDE< -50 & LATITUDE>43 & LATITUDE<50)
            nwlp = makePBS(nwlp,polygon=F,coords = c('LONGITUDE','LATITUDE'))
            coast<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","gshhs",paste0("shoreline",'HR',".csv")))
            d2 = PBSmapping::findPolys(nwlp,coast,includeBdry = F, maxRows = nrow(nwlp))
            nwlp = rename.df(nwlp, c('X','Y'),c('LONGITUDE','LATITUDE'))
            nwlp = subset(nwlp, EID %ni% d2$EID)
            nwlp = subset(nwlp, LONGITUDE> -66 & LONGITUDE< -62)
            fsrsCommPrune = subset(nwlp,select=c(DATE, YEAR, LONGITUDE, LATITUDE,WEIGHT_KG, OFFSET, SOURCE, OFFSET_METRIC,DYEAR,Lobster, Berried,Legal  ))
            fsrsCommSize  = nwlp
            fsrsCommSize = rename.df(fsrsCommSize,'EMPTY','Empty')
            fsrsCommSize$Gear = 'Commercial'
            fsrsCommPrune$Gear = 'Commercial'
            ##merging all trap data
            ddSize$NonLobster = NULL
            trapSize = plyr::rbind.fill(fsrsCommSize, fsrsSize,ddSize)
            trapSize = na.zero(trapSize)
            
            trapsNoSize = plyr::rbind.fill(fsrsPrune,fsrsCommPrune,ddPrune,off41_LOGS)
            i = which(is.na(trapsNoSize$LONGITUDE)& !is.na(trapsNoSize$X))
            trapsNoSize$LONGITUDE[i] = trapsNoSize$X[i]
            i = which(is.na(trapsNoSize$LATITUDE)& !is.na(trapsNoSize$Y))
            trapsNoSize$LATITUDE[i] = trapsNoSize$Y[i]
            
            trapsNoSize$X = trapsNoSize$Y = trapsNoSize$EID = trapsNoSize$id = NULL
            trapsNoSize  = subset(trapsNoSize,Legal>=0)
            trapSize  = subset(trapSize,Legal>=0)
            
              saveRDS(trapSize,file.path(project.datadirectory('bio.lobster'),'data','CombinedCatchData','trapCatchesSize.rds')) 
              saveRDS(trapsNoSize,file.path(project.datadirectory('bio.lobster'),'data','CombinedCatchData','trapCatchesNoSize.rds')) 
            
    #ILTS and ITQ
            
            ilts = ILTS_ITQ_sets()  
            ilts$TRIP_ID = ilts$FISHSET_ID = NULL
            ilts$P.208 = ilts$P.223 = ilts$P.218 = ilts$P.213
            
    #RV Survey
            rv = RV_sets()
            rv$mission = rv$setno = NULL
            
    #NWFSC Surveys
            
            ne = NEFSC_sets()
            ne$MISSION = ne$SETNO = NULL
    #Snow crab
            sn = snowcrab_sets()
            sn$TRIP_ID = sn$SET_NO = NULL
            
            ww = plyr::rbind.fill(ilts,rv,ne,sn)
            ww = rename.df(ww,c('EMPTY'),c('Empty'))
            wwNoSize = subset(ww, select=c(DATE, YEAR, LONGITUDE, LATITUDE,WEIGHT_KG, OFFSET, SOURCE, OFFSET_METRIC,DYEAR,Lobster, Berried,Legal ))
            combinedNoSize = plyr::rbind.fill(trapsNoSize,wwNoSize)
            combinedNoSize$X = combinedNoSize$Y = combinedNoSize$EID = combinedNoSize$id = NULL
            
            combinedSize = plyr::rbind.fill(trapSize,ww)
          
            saveRDS(combinedSize,file.path(project.datadirectory('bio.lobster'),'data','CombinedCatchData','combinedCatchesSize.rds')) 
            saveRDS(combinedNoSize,file.path(project.datadirectory('bio.lobster'),'data','CombinedCatchData','combinedCatchesNoSize.rds')) 
} else {
 if(size) return(readRDS(file.path(project.datadirectory('bio.lobster'),'data','CombinedCatchData','combinedCatchesSize.rds')) )
  if(!size) return(readRDS(file.path(project.datadirectory('bio.lobster'),'data','CombinedCatchData','combinedCatchesNoSize.rds'))) 
      }            
}


#t = ggLobsterMap(ylim=c(40,48),xlim=c(-69,-55),bathy=F,addGrids = F,return.object = T)
#xx = st_as_sf(combinedNoSize, crs=4326, coords=c('LONGITUDE','LATITUDE'))

#xx$gYR = round(xx$YEAR/5)*5

#t +geom_sf(data=subset(xx,gYR==1970),size=.1) + geom_sf(data=subset(xx,gYR==1970 & WEIGHT_KG>0),size=.1,col='red')
