#' @export

compileAbundPresAbs_vessel_corr <- function(redo=F,size=T){
      if(redo){
    #offshore logs
            lobster.db('logs41') #make sure to do a database recapture through logs41.redo before moving on
            
            logs41 = rename.df(logs41,c('FV_FISHED_DATETIME'),c('DATE_FISHED'))
            
            logs41$yr = year(logs41$DATE_FISHED) #2002 to present
            ziff41$yr = year(ziff41$DATE_FISHED) #1995 to 2001
            ziff41$DDLON = ziff41$DDLON * -1
            
            off41$yr  = year(off41$DATE_FISHED) #1981 to 1994
            off41$WEIGHT_KG = off41$ADJ_LOB_LBS*0.453592
            ziff41$WEIGHT_KG = ziff41$EST_WEIGHT_LOG_LBS*0.453592
            logs41$WEIGHT_KG = logs41$ADJCATCH*0.453592
            logs41$DEPTH = ziff41$DEPTH = NA
            ziff41$LICENCE_ID = ziff41$ADJCATCH = off41$ADJ_LOB_LBS = ziff41$EST_WEIGHT_LOG_LBS= off41$OFFAREA = off41$LOB_EST_LBS = ziff41$CAPTAIN = logs41$OFFAREA = NULL 
           logs41$STRINGID = logs41$SOAK_DAYS = logs41$TRIP_ID = logs41$LAT = logs41$LON =  logs41$LOG_EFRT_STD_INFO_ID = logs41$CAPTAIN = logs41$LICENCE_ID = logs41$EST_WEIGHT_LOG_LBS = logs41$ADJCATCH = NULL
            
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
            
            offs = st_as_sf(off41_LOGS,coords = c('LONGITUDE','LATITUDE'),crs=4326)

            LFAs<-readRDS(file.path(git.repo,'bio.lobster.data','mapping_data','LFAPolysSF.rds'))
            l41=subset(LFAs,LFA==41)
            offs = st_join(offs,l41)
            off41_LOGS = subset(offs,!is.na(LFA))
            off41_LOGS$LONGITUDE = st_coordinates(off41_LOGS)[,1]
            off41_LOGS$LATITUDE = st_coordinates(off41_LOGS)[,2]
            
            off41_LOGS$id = paste(off41_LOGS$LONGITUDE, off41_LOGS$LATITUDE,sep='-')
            off41_LOGS = off41_LOGS[-which(off41_LOGS$id %in% c('-61.25-41.25','-55.9768-42.1087','-62.4353-42.1467','-60.2333-41.25')),]
            st_geometry(off41_LOGS) <- NULL
            off41_LOGS$Legal_wt = off41_LOGS$WEIGHT_KG
            off41_LOGS$DYEAR = off41_LOGS$V2 = off41_LOGS$LFA = off41_LOGS$WEIGHT_KG = NULL
            off41_LOGS$Empty = 0
  
  #Greyzone logs
            g = lobster.db('greyzone_logs')
            g$DATE = as.Date(g$DATE_SAILED, format = "%Y-%b-%d")
            g$OFFSET = g$NUM_OF_TRAPS
            g$YEAR = lubridate::year(g$DATE)
            g$SOURCE = 'GREY_ZONE_LOGS'
            g$OFFSET_METRIC = 'Number of Traps'
            g$Gear = 'Commercial'
            g$LATITUDE = round((((g$ENT_LATITUDE /100/100-trunc(g$ENT_LATITUDE/100/100))*100)/60)+trunc(g$ENT_LATITUDE/100/100),4)
            g$LONGITUDE = round((((g$ENT_LONGITUDE/100/100-trunc(g$ENT_LONGITUDE/100/100))*100)/60)+trunc(g$ENT_LONGITUDE/100/100),4)*-1
            g$id = paste(g$DATE,g$LONGITUDE,g$LATITUDE,sep="_")
            g$Legal_wt = g$EST_WEIGHT_LOG_LBS*0.453592
            g$Empty = 0
            
            g = subset(g,select=c(DATE,OFFSET, YEAR, SOURCE, OFFSET_METRIC, Gear, LONGITUDE, LATITUDE, id, Legal_wt, Empty))
            
  #At Sea Samples
            
            lobster.db('atSea')
            a = atSea
            a$UID = paste(a$TRIPNO, a$STRINGNO,a$STARTDATE, a$LATITUDE,a$LONGITUDE, a$TRAPNO,sep="-")
            a$SPECIESCODE = ifelse(is.na(a$SPECIESCODE),0,a$SPECIESCODE)
            a$Berried = ifelse(a$SPECIESCODE == 2550 & a$SEX %in% 3,1,0)
            a$Lobster = ifelse(a$SPECIESCODE == 2550,1,0)
            a$Legal = ifelse(a$SPECIESCODE == 2550 & a$CARLENGTH > 82 & a$SEX %in% 1:2,1,0)
            a$Recruit = ifelse(a$SPECIESCODE == 2550 & a$CARLENGTH %in% 70:82,1,0)
            a$Juv = ifelse(a$SPECIESCODE == 2550 & a$CARLENGTH<=60,1,0)
            a$Empty = ifelse(a$SPECIESCODE == 0,1,0)
            a$NonLobster = ifelse(a$SPECIESCODE %ni% c(0,2550), 1,0)
            a$Legal_wt = a$CALWT * a$Legal
            ###using maturity ogive from 33, 34 and 41
            
            #a$SSB = 
            a1 = subset(a,SPECIESCODE %in% c(0,2550))
            sc1=seq(13,253,by=5)
            a1$SZ = sc1[cut(a1$CARLENGTH,sc1,labels=F)]
            a1$SP_SZ = a1$SZ
            a1$P=1
            aa = aggregate(P~UID+SP_SZ,data=a1,FUN=sum)
            bb = reshape(aa[,c('UID','SP_SZ','P')],idvar='UID',timevar='SP_SZ', direction='wide')
            bb = na.zero(bb)
            
            cc = aggregate(cbind(NonLobster,Empty,Legal, Lobster,Berried,Recruit,Juv, Legal_wt)~UID+LATITUDE+LONGITUDE+STARTDATE,data=a,FUN=sum)
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
            ddPrune = subset(dd,select=c(DATE,LONGITUDE,LATITUDE,OFFSET,SOURCE,OFFSET_METRIC,DYEAR,Legal_wt,YEAR, Lobster,Berried,Legal,Recruit,Juv,Empty))
            ddSize = dd
            ddPrune$Gear = 'Commercial'
            ddSize$Gear = 'Commercial'
            ddSize$DYEAR = ddPrune$DYEAR = ddSize$NonLobster = ddSize$X = ddSize$Y =NULL
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
            fsrs$Recruit = ifelse(fsrs$CL %in% 70:82,1,0)
            fsrs$Legal = ifelse(fsrs$CL > 82 & fsrs$SHORT==0,1,0)
            fsrs$Legal_wt = fsrs$Legal * fsrs$CALC_WT/1000
            fsrs$Juv =  ifelse(fsrs$CL <=60, 1,0)
            #ssb
            
            fsrs$UID = paste(fsrs$VESSEL_CD,fsrs$HAUL_DATE, fsrs$RECORD_NUMBER,fsrs$TRAP_NO, fsrs$LATITUDE, fsrs$LONGITUDE,sep="-")
            Nl = aggregate(cbind(LOBSTER_NO,Empty)~UID+HAUL_DATE+LATITUDE+LONGITUDE+TEMP,data=fsrs,FUN=max)
            WL = aggregate(cbind(Legal, Legal_wt,Berried,SHORT,Recruit,Juv)~UID+HAUL_DATE+LATITUDE+LONGITUDE+TEMP,data=fsrs,FUN=function(x) sum(x,na.rm=T))
            
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
            nwlp$timestamp = as.POSIXct(nwlp$DATE,tz='America/Halifax',origin=lubridate::origin)
            nwlp$timestamp = with_tz(nwlp$timestamp,"UTC")
            nwlp$DYEAR = lubridate::decimal_date(nwlp$timestamp)- lubridate::year(nwlp$timestamp)
            nwlp$Lobster = nwlp$LOBSTER_NO
            nwlp$id = nwlp$UID
            nwlp$TEMP = nwlp$DYEAR = nwlp$UID <- nwlp$HAUL_DATE <- nwlp$LOBSTER_NO <- nwlp$CALC_WT <- nwlp$SHORT <- nwlp$timestamp <- NULL
            fsrsPrune = subset(nwlp,select=c(id,DATE, YEAR, LONGITUDE, LATITUDE,OFFSET,SOURCE, OFFSET_METRIC,Lobster, Berried,Legal,Legal_wt,Recruit,Empty,Juv  ))
            fsrsSize  = nwlp
            fsrsPrune$Gear = 'RecruitmentTrap'
            fsrsSize$Gear = 'RecruitmentTrap'
            
            
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
            fsrs.comm$Legal = ifelse(fsrs.comm$CL > 82 & fsrs.comm$Short==0,1,0)
            fsrs.comm$Legal_wt = fsrs.comm$Legal * fsrs.comm$CALC_WT/1000
            fsrs.comm$Juv = ifelse(fsrs.comm$CL <= 60 ,1,0)
            
            
            fsrs.comm$UID = paste(fsrs.comm$Vessel.Code,fsrs.comm$SDATE,fsrs.comm$Record.Number, fsrs.comm$Trap.Number, fsrs.comm$LATITUDE, fsrs.comm$LONGITUDE,sep="-")
            fsrs.comm$Recruit = ifelse(fsrs.comm$CL %in% 70:82,1,0)
            Nl = aggregate(cbind(Lobster.Number,EMPTY)~UID+SDATE+LATITUDE+LONGITUDE+Temp,data=fsrs.comm,FUN=max)
            WL = aggregate(cbind(Legal_wt,Legal,Berried,Recruit,Juv)~UID+SDATE+LATITUDE+LONGITUDE+Temp,data=fsrs.comm,FUN=function(x) sum(x,na.rm=T))
            
            nwl = merge(Nl,WL,all.x=T)
            
            fsrs.comm$P=1
            aa = aggregate(P~UID+CL,data=subset(fsrs.comm, !is.na(CL) | CL>0),FUN=sum)
            bb = reshape(aa[,c('UID','CL','P')],idvar='UID',timevar='CL', direction='wide')
            bb = na.zero(bb)
            aaa = aggregate(P~UID,data=aa,FUN=sum)
            nwlp = merge(nwl,bb,all.x=T)
            nwlp = merge(nwlp,aaa,all.x=T)
            
            nwlp = na.zero(nwlp)
            t = which(nwlp$P-nwlp$Lobster.Number !=0)
            nwlp$Lobster.Number[t] = nwlp$P[t]
            nwlp$OFFSET=1
            nwlp$OFFSET_METRIC = 'Number of traps'
            nwlp$SOURCE = 'FSRS_COMMERCIAL_TRAPS'
            nwlp$YEAR = year(nwlp$SDATE)
            nwlp$DATE = nwlp$SDATE
            nwlp$timestamp = as.POSIXct(nwlp$DATE,tz='America/Halifax',origin=lubridate::origin)
            nwlp$timestamp = with_tz(nwlp$timestamp,"UTC")
            nwlp$Lobster = nwlp$Lobster.Number
            nwlp$id = nwlp$UID
            nwlp$Temp = nwlp$UID <- nwlp$SDATE <- nwlp$Lobster.Number <- nwlp$CALC_WT <- nwlp$Short <- nwlp$timestamp <- nwlp$P <- NULL
            nwlp = subset(nwlp, LONGITUDE< -50 & LATITUDE>43 & LATITUDE<50)
            nwlp = makePBS(nwlp,polygon=F,coords = c('LONGITUDE','LATITUDE'))
            coast<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","gshhs",paste0("shoreline",'HR',".csv")))
            d2 = PBSmapping::findPolys(nwlp,coast,includeBdry = F, maxRows = nrow(nwlp))
            nwlp = rename.df(nwlp, c('X','Y','EMPTY'),c('LONGITUDE','LATITUDE','Empty'))
            nwlp = subset(nwlp, EID %ni% d2$EID)
            nwlp = subset(nwlp, LONGITUDE> -66 & LONGITUDE< -62)
            fsrsCommPrune = subset(nwlp,select=c(DATE, YEAR, LONGITUDE, LATITUDE, OFFSET, SOURCE, OFFSET_METRIC,Lobster, Berried,Legal,Recruit,Legal_wt,Empty,Juv  ))
            fsrsCommSize  = nwlp
            
            fsrsCommSize$Gear = 'Commercial'
            fsrsCommSize$P = NULL
            
            fsrsCommPrune$Gear = 'Commercial'
            ##merging all trap data
            ddSize$NonLobster = NULL
            
            ddSize$Date = as.Date(ddSize$DATE)
            fsrsSize$Date = as.Date(fsrsSize$DATE)
            fsrsCommSize$Date = as.Date(fsrsCommSize$DATE)
            
            trapSize = plyr::rbind.fill(fsrsCommSize, fsrsSize,ddSize)
            trapSize$DATE = trapSize$Date
            trapSize$Date = NULL
            trapSize = na.zero(trapSize)
            
            trapsNoSize = plyr::rbind.fill(fsrsPrune,fsrsCommPrune,ddPrune,off41_LOGS,g)
            i = which(is.na(trapsNoSize$LONGITUDE)& !is.na(trapsNoSize$X))
            trapsNoSize$LONGITUDE[i] = trapsNoSize$X[i]
            i = which(is.na(trapsNoSize$LATITUDE)& !is.na(trapsNoSize$Y))
            trapsNoSize$LATITUDE[i] = trapsNoSize$Y[i]
            
            trapsNoSize$X = trapsNoSize$Y = trapsNoSize$EID = trapsNoSize$id = NULL
            
            saveRDS(trapsNoSize,file.path(project.datadirectory('bio.lobster'),'data','CombinedCatchData','trapCatchesNoSize.rds')) 
            saveRDS(trapSize,file.path(project.datadirectory('bio.lobster'),'data','CombinedCatchData','trapCatchesSize.rds')) 
    
          #  trapsNoSize = readRDS(file.path(project.datadirectory('bio.lobster'),'data','CombinedCatchData','trapCatchesNoSize.rds')) 
          #  trapSize = readRDS(file.path(project.datadirectory('bio.lobster'),'data','CombinedCatchData','trapCatchesSize.rds')) 
            
            #RV Survey  ###covertt back to raw #s
            rv = RV_sets()
            rv$TEMP = rv$bottom_temperature
            rv = subset(rv,YEAR>1998)
            rv$id = paste(rv$mission,rv$setno,sep='_')
            rv$OFFSET = rv$OFFSET*1e6
            rv$OFFSET_METRIC = "TowedDist x wing spread m2"
            
            rv = rv %>%
                  select(id,Lobster, Legal, Legal_wt,Berried, Recruit, Juv,YEAR,DATE,EMPTY,starts_with("P."), LONGITUDE, LATITUDE, SOURCE, OFFSET, OFFSET_METRIC,Gear )
            
            #NEFSC Surveys
            ne = NEFSC_sets()
            ne$id = paste(ne$MISSION,ne$SETNO,sep="_")
            ne$OFFSET = ne$OFFSET*1e6
            ne$OFFSET_METRIC="TowedDist x wing spread m2"
            ne = ne %>%
                  select(id, Lobster, Legal, Legal_wt, Berried, Recruit, YEAR, DATE, EMPTY,Juv, starts_with("P."), LONGITUDE, LATITUDE,SOURCE, OFFSET, OFFSET_METRIC, Gear)
            
            #ILTS and ITQ
            
            ilts = ILTS_ITQ_All_Data(biomass = F,aggregate=F,redo_base_data = F)  
            ilts$N = ilts$SA_CORRECTED_PRORATED_N * ilts$sweptArea ###covertt back to raw #s
            sc1=seq(13,253,by=5)
            ilts$SZ = sc1[cut(ilts$FISH_LENGTH,sc1,labels=F)]
            ilts$Berried= ilts$Recruit = ilts$Legal=  ilts$Juv =0
            ilts$Berried = ifelse(ilts$SEX==3,ilts$N,ilts$Berried)
            ilts$Recruit = ifelse(ilts$FISH_LENGTH %in% 70:81,ilts$N,ilts$Recruit)
            ilts$Recruit = ifelse(ilts$FISH_LENGTH ==82,ilts$N/2,ilts$Recruit)
            ilts$Juv = ifelse(ilts$FISH_LENGTH <=60,ilts$N,ilts$Juv)
            
            ilts$Legal = ifelse(ilts$FISH_LENGTH >82,ilts$N,ilts$Legal)
            ilts$Legal = ifelse(ilts$FISH_LENGTH ==82,ilts$N/2,ilts$Legal)
            ilts$Legal_wt = lobLW(CL=ilts$FISH_LENGTH,sex=ilts$SEX) * ilts$N/1000
            ilts$ID = paste(ilts$TRIP_ID,ilts$SET_NO,sep="_")
            dA = aggregate(N~SZ+ID,data=ilts,FUN=sum)
            
            dS = aggregate(cbind(Berried,Legal,N,Legal_wt,Recruit,Juv)~TRIP_ID+SET_NO+ID,data=ilts,FUN=sum)
            dS$Lobster = dS$N
            dS$N = NULL
            dA$P = dA$N
            aa = aggregate(P~ID+SZ,data=dA,FUN=sum)
            bb = reshape(aa[,c('ID','SZ','P')],idvar='ID',timevar='SZ', direction='wide')
            bb = na.zero(bb)
            
            ca = merge(dS,bb)
            set = ilts %>%
                  distinct(TRIP_ID,SET_NO,SET_DATE,SET_LONG,SET_LAT,sweptArea,YEAR,temp,Length_comps)
            ilt = merge(set,ca,all.x=T)
            ilt$EMPTY = ifelse(ilt$Lobster>0,0,1)
            ilt$id = paste(ilt$TRIP_ID,ilt$SET_NO,sep="_")
            ilt$OFFSET = ilt$sweptArea *1e6
            ilt$OFFSET_METRIC = "TowedDist x wing spread m2"
            ilt$LONGITUDE = ilt$SET_LONG
            ilt$LATITUDE = ilt$SET_LAT
            ilt$DATE = ilt$SET_DATE
            ilt$SOURCE = 'ILTS'
            ilt$Gear = 'NEST'
            browser()
            
            p_cols <- grep("^P\\.", names(ilt), value = TRUE)
            extra_cols <- c("Berried", "Legal", "Legal_wt", "Recruit", "Juv")
            
            # Combine all target columns
            target_cols <- c(p_cols, extra_cols)
            ilt[ilt$Length_comps == 0, target_cols] <- NA   
            
            ilt = ilt %>%
            #  filter(YEAR>1998) %>%
              select(id, Lobster, Legal, Legal_wt, Berried, Recruit, YEAR, DATE,Juv, EMPTY, starts_with("P."), LONGITUDE, LATITUDE,SOURCE, OFFSET, OFFSET_METRIC, Gear)
            
    #Snow crab
            sn = snowcrab_sets()
            sn$id = paste(sn$TRIP_ID,sn$SET_NO,sep="_")
            sn$OFFSET = sn$OFFSET*1e6
            sn$OFFSET_METRIC =  "TowedDist x wing spread m2"
            sn$Gear = 'Nephrops'
            sn$Recruit = sn$Juv = sn$Berried =0
            sn = sn %>%
                select(id,Lobster, OFFSET, OFFSET_METRIC,SOURCE,LATITUDE, LONGITUDE,DATE,EMPTY,Gear,Recruit, Juv, Berried)
      
    #scallop surveys
            sc = scallop_sets()
            sc$EMPTY = ifelse(sc$Lobster==0,1,0)
            sc$SOURCE = 'Scallop Survey'
            sc$Gear = 'Dredge'
            sc$OFFSET_METRIC="TowedDist x wing spread m2"
            sc$DATE = sc$TOW_DATE
            sc$LONGITUDE = sc$X
            sc$LATITUDE = sc$Y
            sc$id = paste('Scall',sc$TOW_SEQ,sep="_")
            
            sc = sc %>%
              select(id, Lobster, Legal, Legal_wt, Berried, Recruit, YEAR, DATE,Juv, EMPTY, starts_with("P."), LONGITUDE, LATITUDE,SOURCE, OFFSET, OFFSET_METRIC, Gear)
    
    #MNR
            mn = MNR_sets()       
           
                     
    ##all good to here June 3 2025        
            ww = plyr::rbind.fill(ilt,rv,ne,sn,sc,mn)
            
            
            ws = st_as_sf(ww,coords=c('LONGITUDE','LATITUDE'),crs=4326)
            ww$Empty = ww$EMPTY
            ww$EMPTY = NULL
            wwNoSize = subset(ww, select=c(DATE, YEAR, LONGITUDE, LATITUDE, OFFSET, SOURCE, OFFSET_METRIC,Lobster, Berried,Legal,Recruit,Juv,Legal_wt,Empty,Gear))
            
            combinedNoSize = plyr::rbind.fill(trapsNoSize,wwNoSize)
            
            ww$DATE = as.Date(ww$DATE)
            
            combinedSize = plyr::rbind.fill(trapSize,ww)
            combinedNoSize = subset(combinedNoSize,OFFSET>0)
          combinedSize = subset(combinedSize,OFFSET>0)
          combinedSize$YEAR = year(combinedSize$DATE)
          combinedNoSize$YEAR = year(combinedNoSize$DATE)
          i = which(combinedNoSize$SOURCE=='Snow crab survey')
          combinedNoSize$Berried[i] = 0
          combinedSize = subset(combinedSize, SOURCE !='Snow crab survey')
          saveRDS(combinedSize,file.path(project.datadirectory('bio.lobster'),'data','CombinedCatchData','combinedCatchesSize.rds')) 
            saveRDS(combinedNoSize,file.path(project.datadirectory('bio.lobster'),'data','CombinedCatchData','combinedCatchesNoSize.rds')) 
} else {
 if(size) return(readRDS(file.path(project.datadirectory('bio.lobster'),'data','CombinedCatchData','combinedCatchesSize.rds')) )
  if(!size) return(readRDS(file.path(project.datadirectory('bio.lobster'),'data','CombinedCatchData','combinedCatchesNoSize.rds'))) 
      }            
}
