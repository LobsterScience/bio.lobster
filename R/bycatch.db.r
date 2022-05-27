#' @export


bycatch.db <- function(DS='redo.data',p=p,wd='C:\\Users\\Cooka\\OneDrive - DFO-MPO\\BycatchLobster'){
  options(stringsAsFactors=F)
  require(lubridate)
  dir.create(file.path(wd,'data'),showWarnings = F)
  dir.create(file.path(wd,'results'),showWarnings = F)
  
  if(grepl('odbc',DS)) db.setup() 
  
  if(DS=='odbc.redo'){
    xAll = connect.command(con,'select * from cooka.lobster_bycatch_assoc')
    xAll$X = convert.dd.dddd(xAll$LONGDDMM)*-1
    xAll$Y = convert.dd.dddd(xAll$LATDDMM)
    saveRDS(xAll,file.path(wd,'data/CompiledAtSeaMarch2022.rds'))
    
    xAll = connect.command(con,'select * from cooka.bycatch_cbfha_2022')
    xAll$X = convert.dd.dddd(xAll$LONGDDMM)*-1
    xAll$Y = convert.dd.dddd(xAll$LATDDMM)
    saveRDS(xAll,file.path(wd,'data/CBFHA_2021_atsea.rds'))
    
    xAll = connect.command(con,'select * from cooka.bycatch_esfpa_2022')
    xAll$X = convert.dd.dddd(xAll$LONGDDMM)*-1
    xAll$Y = convert.dd.dddd(xAll$LATDDMM)
    saveRDS(xAll,file.path(wd,'data/ESFPA_2019_atsea.rds'))
    
    
    tr = connect.command(con,'select * from lobster.istraps')
    se = connect.command(con,'select * from lobster.issets_mv')
    de = connect.command(con,'select * from lobster.isdetails_mv')
    saveRDS(list(tr,se,de),file=file.path(wd,'/data/ObserverInfo.rds'))
  }
  
  if(DS %in% c('logbook.merge.redo','logbook.merge')){
    
    if(grepl('redo',DS)){
          x = lobster.db('process.logs')
          b = subset(x,LFA %in% c(33,34,35) & SYEAR %in% 2019:2021)
          bb = subset(x, LFA %in% c(27) & SYEAR %in% 2019)
          bbb = subset(x, LFA %in% c('31A','31B') & SYEAR %in% 2018:2019)
          b = as.data.frame(rbind(rbind(b,bb),bbb))
          gt = read.csv(file.path(wd,'data/Grids2Targets.csv'))
          
          #logbook handling
          b$mn = month(b$DATE_FISHED)
          b$GridGroup = b$target = b$Period = NA
          for(i in 1:nrow(b)){
            pit = gt$GridGrouping[which(b$GRID_NUM[i]==gt$GRID_NUM & gt$LFA==b$LFA[i])]
            if(length(pit)>0){
              b$GridGroup[i] = pit
              m = b$mn[i]
              k = subset(gt,GRID_NUM==b$GRID_NUM[i]& gt$LFA==b$LFA[i])
              ll = ifelse(m >=k$Period1.Start & m<=k$Period1.End,'Period1',ifelse(m >=k$Period2.Start & m<=k$Period2.End,'Period2',ifelse(m >=k$Period3.Start & m<=k$Period3.End,'Period3','Period4')))
              lll = as.numeric(strsplit(ll,'Period')[[1]][2])
              b$target[i] <- k[,ll]
              b$Period[i] = lll
            }
          }
          saveRDS(b,file=file.path(wd,'data/logbookReadyFILTERED.rds'))
        return('complete')
        }
       return(readRDS(file.path(wd,'data/logbookReadyFILTERED.rds')))
    }
  
if(DS=='targets'){
    return( read.csv(file.path(wd,'data/Grids2Targets.csv'))
    )
  }

if(DS %in% c('SWLSS','SWLSS.redo')){
  
  if(grepl('redo',DS)){
    a = readRDS(file.path(wd,'data','CompiledAtSeaMarch2022.rds'))
    a = subset(a, OWNER_GROUP=='SWLSS')
    a$COMAREA_ID = toupper(a$COMAREA_ID)
    a$COMAREA_ID[which(a$TRIP== '107869-081219')] <- "L33"
    ms = read.csv('data/SWLSSTripmatch.csv')
    ms = subset(ms,select=c(TRIP,SD_LOG_ID_1,QUALITY))
    
    a$Legal = ifelse(a$SPECCD_ID == 2550 & a$FISH_LENGTH > 82 & a$SEXCD_ID %in% 1:2,1,0)
    a$Berried = ifelse(a$SPECCD_ID == 2550 & a$SEXCD_ID %in% 3,1,0)
    a$Lobster = ifelse(a$SPECCD_ID == 2550,1,0)
    a$Cod = ifelse(a$SPECCD_ID == 10,1,0)
    a$Cusk = ifelse(a$SPECCD_ID == 15,1,0)
    a$Jonah = ifelse(a$SPECCD_ID == 2511,1,0)
    a$SPECCD_ID  = ifelse(is.na(a$SPECCD_ID),9999,a$SPECCD_ID)
    a$Empty = ifelse(a$SPECCD_ID == 9999,1,0)
    a$LegalWt = a$Legal * a$CALWT_G/1000
    a$LegalWt[which(is.na(a$LegalWt))] <- 0
    a$CodWt = a$Cod * a$CALWT_G/1000
    a$CuskWt = a$Cusk * a$CALWT_G/1000
    a$JonahWt = a$Jonah * a$CALWT_G/1000
    a$JonahRtWt = ifelse(a$Jonah==1 & a$SEXCD_ID==1 & a$FISH_LENGTH>=130, a$JonahWt, 0)
    a$CodWt[which(is.na(a$CodWt))] <- 0
    a$CuskWt[which(is.na(a$CuskWt))] <- 0 
    a$JonahWt[which(is.na(a$JonahWt))] <- 0
    a$CALWT_G[which(is.na(a$CALWT_G))] <- 0
    a$LobsterWt = a$Lobster * a$CALWT_G/1000
    a$LobsterWt[which(is.na(a$LobsterWt))] <- 0
    
    a$Sz = round(a$FISH_LENGTH/5)*5
    a$SP_SZ = paste(a$SPECCD_ID, a$Sz, sep="-")
    
    a$UID = paste(a$TRIP, a$FISHSET_ID, a$TRAP_ID,sep="-")
    a$P = 1
    aa = aggregate(P~UID+SP_SZ,data=a,FUN=sum)
    
    bb = reshape(aa[,c('UID','SP_SZ','P')],idvar='UID',timevar='SP_SZ', direction='wide')
    bb = na.zero(bb)
    
    
    season.dates = backFillSeasonDates(lobster.db('season.dates'),eyr=year(Sys.time()))
    a = subset(a, !is.na(BOARD_DATE))
    a$LFA = as.numeric(unlist(lapply(strsplit(a$COMAREA_ID,'L'),"[[",2)))
    a = addSYEAR(a,date.field = 'BOARD_DATE')
    # season.dates = lobster.db('season.dates')
    a$WOS = NA
    m=0
    lfa = unique(a$LFA) 
    lfa = na.omit(lfa)
    for(i in 1:length(lfa)) {
      h  = season.dates[season.dates$LFA==lfa[i],]  
      k = na.omit(unique(a$SYEAR[a$LFA==lfa[i]]))
      #h = na.omit(h)
      k = intersect(k,h$SYEAR)
      for(j in k){
        m=m+1
        ll = which(a$LFA==lfa[i] & a$SYEAR==j)
        a$WOS[ll] = floor(as.numeric(a$BOARD_DATE[ll]-min(h$START_DATE[h$SYEAR==j]))/7)+1
      }
    }
    if(any(!is.finite(a$WOS))) {kl = which(!is.finite(a$WOS)); a$WOS[kl] = NA}
    a = subset(a,WOS>0)
    
    
    
    #per trap
    ac = aggregate(cbind(Lobster, Cod, Cusk, Jonah, Legal, Berried,Empty,LegalWt,CALWT_G,CodWt,CuskWt,JonahWt,JonahRtWt,LobsterWt)~UID+TRIP+X+Y+TRAP_ID+FISHSET_ID+COMAREA_ID+STRATUM_ID+NUM_HOOK_HAUL+BOARD_DATE+WOS+LFA+SYEAR, data=a,FUN=sum,na.rm=F)
    
    CDa = merge(ac,bb,by='UID')
    CDa$'P.9999-NA' = NULL
    CDa$P=1
    CDa$mn = month(CDa$BOARD_DATE)
    gt = bycatch.db('targets',wd=wd) 
    CDa$GridGroup = CDa$target = CDa$Period = NA
    for(i in 1:nrow(CDa)){
      pit = gt$GridGrouping[which(CDa$STRATUM_ID[i]==gt$GRID_NUM & gt$LFA==strsplit(CDa$COMAREA_ID[i],'L')[[1]][2])]
      if(length(pit)>0){
        CDa$GridGroup[i] = (pit)
        m = CDa$mn[i]
        k = subset(gt,GRID_NUM==CDa$STRATUM_ID[i] & LFA==strsplit(CDa$COMAREA_ID[i],'L')[[1]][2])
        ll = ifelse(m >=k$Period1.Start & m<=k$Period1.End,'Period1',ifelse(m >=k$Period2.Start & m<=k$Period2.End,'Period2',ifelse(m >=k$Period3.Start & m<=k$Period3.End,'Period3','Period4')))
        lll = as.numeric(strsplit(ll,'Period')[[1]][2])
        CDa$target[i] <- as.numeric(k[,ll])
        CDa$Period[i] = lll
        #	rm(k,m,ll,lll)
      }
    }
    CDa$COMAREA_ID = NULL
    CDa = bio.utilities::rename.df(CDa, c('TRAP_ID', 'FISHSET_ID','STRATUM_ID','NUM_HOOK_HAUL','BOARD_DATE','CALWT_G'),c('TRAPNO','SETNO','GRIDNUM','NUM_TRAPS','DATE_FISHED','CALWT'))
    CDa$DID = 'ASSOC'
    CDa$ll = paste(CDa$X,CDa$Y,sep='ll')
    CDa$Cluster = NA
    cc = unique(CDa$TRIP)
    for(i in 1:length(cc)){
      k = which(CDa$TRIP == cc[i])
      CDa$Cluster[k] = create_seq_from_column(CDa$ll[k])
    }
    CDa$ll <- NULL
    saveRDS(CDa,file=file.path(wd,'data','SWLSSreshape.rds'))
    return(CDa)
    }
  return(readRDS(file=file.path(wd,'data','SWLSSreshape.rds')))
}

if(DS %in% c('ISDB.redo','ISDB')){
  if(grepl('redo',DS)){
          g = lobster.db('atSea.redo')
          g = lobster.db('atSea.clean.redo')
          g = lobster.db('atSea.clean')
          gg = subset(g,DESCRIPTION=='ISDB' & SYEAR %in% (2019:2021)  & LFA %in% 33:35)
          gg$UID = paste(gg$TRIPNO,gg$TRAPNO,sep='_')
          
        #  LFAgrid<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","GridPolys.csv"))
          
          
          gt = t = bycatch.db('targets',wd=wd) 
          gg$GridGroup = gg$target = gg$Period = NA
          gg$mn = month(gg$SDATE)
          for(i in 1:nrow(gg)){
            pit = gt$GridGrouping[which(gg$GRIDNO[i]==gt$GRID_NUM & gt$LFA==gg$LFA[i])]
            if(length(pit)>0){
              gg$GridGroup[i] = (pit)
              m = gg$mn[i]
              k = subset(gt,GRID_NUM==gg$GRIDNO[i] & LFA==gg$LFA[i])
              ll = ifelse(m >=k$Period1.Start & m<=k$Period1.End,'Period1',ifelse(m >=k$Period2.Start & m<=k$Period2.End,'Period2',ifelse(m >=k$Period3.Start & m<=k$Period3.End,'Period3','Period4')))
              lll = as.numeric(strsplit(ll,'Period')[[1]][2])
              gg$target[i] <- as.numeric(k[,ll])
              gg$Period[i] = lll
              rm(k,m,ll,lll)
            }
          }	
          
          
          saveRDS(gg,file=file.path(wd,'data','ISDBclean.rds'))
          return(gg)
      }

        return(readRDS(file=file.path(wd,'data','ISDBclean.rds')))
  
}
  
  if(DS %in% c('ISDB.reshape.redo','ISDB.reshape')){
    if(grepl('redo',DS)){
    
      a = bycatch.db(DS='ISDB')
      a$UID = paste(a$TRIPNO,a$TRAPNO,sep='_')
      
      a$Legal = ifelse(a$SPECIESCODE == 2550 & a$CARLENGTH > 82 & a$SEX %in% 1:2,1,0)
      a$Berried = ifelse(a$SPECIESCODE == 2550 & a$SEX %in% 3,1,0)
      a$Lobster = ifelse(a$SPECIESCODE == 2550,1,0)
      a$Cod = ifelse(a$SPECIESCODE == 10,1,0)
      a$Cusk = ifelse(a$SPECIESCODE == 15,1,0)
      a$Jonah = ifelse(a$SPECIESCODE == 2511,1,0)
      a$SPECIESCODE  = ifelse(is.na(a$SPECIESCODE),9999,a$SPECIESCODE)
      a$Empty = ifelse(a$SPECIESCODE == 9999,1,0)
      a$LegalWt = a$Legal * a$CALWT/1000
      a$LegalWt[which(is.na(a$LegalWt))] <- 0
      a$CodWt = a$Cod * a$CALWT/1000
      a$CuskWt = a$Cusk * a$CALWT/1000
      a$JonahWt = a$Jonah * a$CALWT/1000
      a$JonahRtWt = ifelse(a$Jonah==1 & a$SEX==1 & a$CARLENGTH>=130, a$JonahWt, 0)
      a$CodWt[which(is.na(a$CodWt))] <- 0
      a$CuskWt[which(is.na(a$CuskWt))] <- 0 
      a$JonahWt[which(is.na(a$JonahWt))] <- 0
      a$CALWT[which(is.na(a$CALWT))] <- 0
      a$LobsterWt = a$Lobster * a$CALWT/1000
      a$LobsterWt[which(is.na(a$LobsterWt))] <- 0
      
      a$Sz = round(a$CARLENGTH/5)*5
      a$SP_SZ = paste(a$SPECIESCODE, a$Sz, sep="-")
      a$P = 1
      aa = aggregate(P~UID+SP_SZ,data=a,FUN=sum)
      bb = reshape(aa[,c('UID','SP_SZ','P')],idvar='UID',timevar='SP_SZ', direction='wide')
      bb = na.zero(bb)
      
      #per trap
      ac = aggregate(cbind(Lobster, Cod, Cusk, Jonah, Legal, Berried,Empty,LegalWt,CALWT,CodWt,CuskWt,JonahWt,JonahRtWt,LobsterWt)~UID
                     +TRIPNO+X+Y+TRAPNO+NUM_HOOK_HAUL+LFA+GRIDNO+SDATE+SYEAR+WOS+Period+target+GridGroup, data=a,FUN=sum,na.rm=F)
      
      BF = merge(ac,bb,by='UID')
      BF$'P.9999-NA' = NULL
      BF$P=1
      names(BF)[c(2,6,8,9)] = c('TRIP','NUM_TRAPS','GRIDNUM','DATE_FISHED')
      BF$DID = 'OBS'
      BF$ll = paste(BF$X,BF$Y,sep='ll')
      BF$Cluster = NA
      cc = unique(BF$TRIP)
      for(i in 1:length(cc)){
        k = which(BF$TRIP == cc[i])
        BF$Cluster[k] = create_seq_from_column(BF$ll[k])
      }
      BF$ll <- NULL
      
       saveRDS(BF,file=file.path(wd,'data','ISDBreshape.rds'))
      return(BF)
    }
    
    return(readRDS(file=file.path(wd,'data','ISDBreshape.rds')))
    
  }

 if(DS %in% c('ISDB.all.species.reshape.redo','ISDB.all.species.reshape')){
    if(grepl('redo',DS)){
      
      a = bycatch.db(DS='ISDB')
      a$UID = paste(a$TRIPNO,a$TRAPNO,sep='_')
      
      a$Legal = ifelse(a$SPECIESCODE == 2550 & a$CARLENGTH > 82 & a$SEX %in% 1:2,1,0)
      a$Berried = ifelse(a$SPECIESCODE == 2550 & a$SEX %in% 3,1,0)
      a$Lobster = ifelse(a$SPECIESCODE == 2550,1,0)
      a$Cod = ifelse(a$SPECIESCODE == 10,1,0)
      a$Cusk = ifelse(a$SPECIESCODE == 15,1,0)
      a$Jonah = ifelse(a$SPECIESCODE == 2511,1,0)
      a$SPECIESCODE  = ifelse(is.na(a$SPECIESCODE),9999,a$SPECIESCODE)
      a$Empty = ifelse(a$SPECIESCODE == 9999,1,0)
      a$LegalWt = a$Legal * a$CALWT/1000
      a$LegalWt[which(is.na(a$LegalWt))] <- 0
      a$CodWt = a$Cod * a$CALWT/1000
      a$CuskWt = a$Cusk * a$CALWT/1000
      a$JonahWt = a$Jonah * a$CALWT/1000
      a$CodWt[which(is.na(a$CodWt))] <- 0
      a$CuskWt[which(is.na(a$CuskWt))] <- 0 
      a$JonahWt[which(is.na(a$JonahWt))] <- 0
      a$CALWT[which(is.na(a$CALWT))] <- 0
      a$LobsterWt = a$Lobster * a$CALWT/1000
      a$LobsterWt[which(is.na(a$LobsterWt))] <- 0
      a$SPECIESCODE = bio.utilities::recode(a$SPECIESCODE, "12=193;13=193;200=211; 201=211; 202=211; 203=211; 204=211; 300=310; 301=310; 312=310; 636=122; 429=640; 648=640; 2521=2520")
      a$P = 1
      aa = aggregate(P~SPECIESCODE+UID,data=a,FUN=sum)
      bb = reshape(aa[,c('UID','SPECIESCODE','P')],idvar='UID',timevar='SPECIESCODE', direction='wide')
      bb = na.zero(bb)
      
      #per trap
      ac = aggregate(cbind(Lobster, Cod, Cusk, Jonah, Legal, Berried,Empty,LegalWt,CALWT,CodWt,CuskWt,JonahWt,LobsterWt)~UID
                     +TRIPNO+X+Y+TRAPNO+NUM_HOOK_HAUL+LFA+GRIDNO+SDATE+SYEAR+WOS+Period+target+GridGroup, data=a,FUN=sum,na.rm=F)
      
      BF = merge(ac,bb,by='UID')
      BF$'P.9999' = NULL
      BF$P=1
      names(BF)[c(2,6,8,9)] = c('TRIP','NUM_TRAPS','GRIDNUM','DATE_FISHED')
      BF$DID = 'OBS'
      saveRDS(BF,file=file.path(wd,'data','ISDB.all.species.reshape.rds'))
      return(BF)
    }
    
    return(readRDS(file=file.path(wd,'data','ISDB.all.species.reshape.rds')))
    
  }
  
  if(DS %in% c('SWLSS.all.species','SWLSS.all.species.redo')){
    
    if(grepl('redo',DS)){
      a = readRDS(file.path(wd,'data','CompiledAtSeaMarch2022.rds'))
      a = subset(a, OWNER_GROUP=='SWLSS')
      a$COMAREA_ID = toupper(a$COMAREA_ID)
      a$COMAREA_ID[which(a$TRIP== '107869-081219')] <- "L33"
      ms = read.csv('data/SWLSSTripmatch.csv')
      ms = subset(ms,select=c(TRIP,SD_LOG_ID_1,QUALITY))
      
      a$Legal = ifelse(a$SPECCD_ID == 2550 & a$FISH_LENGTH > 82 & a$SEXCD_ID %in% 1:2,1,0)
      a$Berried = ifelse(a$SPECCD_ID == 2550 & a$SEXCD_ID %in% 3,1,0)
      a$Lobster = ifelse(a$SPECCD_ID == 2550,1,0)
      a$Cod = ifelse(a$SPECCD_ID == 10,1,0)
      a$Cusk = ifelse(a$SPECCD_ID == 15,1,0)
      a$Jonah = ifelse(a$SPECCD_ID == 2511,1,0)
      a$SPECCD_ID  = ifelse(is.na(a$SPECCD_ID),9999,a$SPECCD_ID)
      a$Empty = ifelse(a$SPECCD_ID == 9999,1,0)
      a$LegalWt = a$Legal * a$CALWT_G/1000
      a$LegalWt[which(is.na(a$LegalWt))] <- 0
      a$CodWt = a$Cod * a$CALWT_G/1000
      a$CuskWt = a$Cusk * a$CALWT_G/1000
      a$JonahWt = a$Jonah * a$CALWT_G/1000
      a$CodWt[which(is.na(a$CodWt))] <- 0
      a$CuskWt[which(is.na(a$CuskWt))] <- 0 
      a$JonahWt[which(is.na(a$JonahWt))] <- 0
      a$CALWT_G[which(is.na(a$CALWT_G))] <- 0
      a$LobsterWt = a$Lobster * a$CALWT_G/1000
      a$LobsterWt[which(is.na(a$LobsterWt))] <- 0
      a$SPECCD_ID = bio.utilities::recode(a$SPECCD_ID, "2=9991; 9=9991; 12=193; 13=193; 20=23; 54=9991; 92=9991; 112=193; 201=211;
                                          204=211; 251=10; 300=310; 301=310; 302=310; 429=640;
                                          440=640; 600=640; 620=640; 2551=2550;")
      
      
      a$UID = paste(a$TRIP, a$FISHSET_ID, a$TRAP_ID,sep="-")
      a$P = 1
      aa = aggregate(P~SPECCD_ID+UID,data=a,FUN=sum)      
      bb = reshape(aa[,c('UID','SPECCD_ID','P')],idvar='UID',timevar='SPECCD_ID', direction='wide')
      bb = na.zero(bb)
      
      
      season.dates = backFillSeasonDates(lobster.db('season.dates'),eyr=year(Sys.time()))
      a = subset(a, !is.na(BOARD_DATE))
      a$LFA = as.numeric(unlist(lapply(strsplit(a$COMAREA_ID,'L'),"[[",2)))
      a = addSYEAR(a,date.field = 'BOARD_DATE')
      # season.dates = lobster.db('season.dates')
      a$WOS = NA
      m=0
      lfa = unique(a$LFA) 
      lfa = na.omit(lfa)
      for(i in 1:length(lfa)) {
        h  = season.dates[season.dates$LFA==lfa[i],]  
        k = na.omit(unique(a$SYEAR[a$LFA==lfa[i]]))
        #h = na.omit(h)
        k = intersect(k,h$SYEAR)
        for(j in k){
          m=m+1
          ll = which(a$LFA==lfa[i] & a$SYEAR==j)
          a$WOS[ll] = floor(as.numeric(a$BOARD_DATE[ll]-min(h$START_DATE[h$SYEAR==j]))/7)+1
        }
      }
      if(any(!is.finite(a$WOS))) {kl = which(!is.finite(a$WOS)); a$WOS[kl] = NA}
      a = subset(a,WOS>0)
      
      
      
      #per trap
      ac = aggregate(cbind(Lobster, Cod, Cusk, Jonah, Legal, Berried,Empty,LegalWt,CALWT_G,CodWt,CuskWt,JonahWt,LobsterWt)~UID
                     +TRIP+X+Y+TRAP_ID+FISHSET_ID+COMAREA_ID+STRATUM_ID+NUM_HOOK_HAUL+BOARD_DATE+WOS+LFA+SYEAR, data=a,FUN=sum,na.rm=F)
      
      CDa = merge(ac,bb,by='UID')
      CDa$'P.9999' = NULL
      CDa$P=1
      CDa$mn = month(CDa$BOARD_DATE)
      gt = bycatch.db('targets',wd=wd) 
      CDa$GridGroup = CDa$target = CDa$Period = NA
      for(i in 1:nrow(CDa)){
        pit = gt$GridGrouping[which(CDa$STRATUM_ID[i]==gt$GRID_NUM & gt$LFA==strsplit(CDa$COMAREA_ID[i],'L')[[1]][2])]
        if(length(pit)>0){
          CDa$GridGroup[i] = (pit)
          m = CDa$mn[i]
          k = subset(gt,GRID_NUM==CDa$STRATUM_ID[i] & LFA==strsplit(CDa$COMAREA_ID[i],'L')[[1]][2])
          ll = ifelse(m >=k$Period1.Start & m<=k$Period1.End,'Period1',ifelse(m >=k$Period2.Start & m<=k$Period2.End,'Period2',ifelse(m >=k$Period3.Start & m<=k$Period3.End,'Period3','Period4')))
          lll = as.numeric(strsplit(ll,'Period')[[1]][2])
          CDa$target[i] <- as.numeric(k[,ll])
          CDa$Period[i] = lll
          #	rm(k,m,ll,lll)
        }
      }
      CDa$COMAREA_ID = NULL
      names(CDa)[c(5:9,21)] =c('TRAPNO','SETNO','GRIDNUM','NUM_TRAPS','DATE_FISHED','CALWT')
      CDa$DID = 'ASSOC'
      saveRDS(CDa,file=file.path(wd,'data','SWLSS.all.species.reshape.rds'))
      return(CDa)
    }
    return(readRDS(file=file.path(wd,'data','SWLSS.all.species.reshape.rds')))
  }

if(DS %in% c('CBFHA.redo','CBFHA') ){
    if(grepl('redo',DS)){
    a =   readRDS(file.path(wd,'data/CompiledAtSeaMarch2022.rds'))
    a = subset(a,OWNER_GROUP=='CBFHA')
    aa =  readRDS(file.path(wd,'data/CBFHA_2021_atsea.rds'))
    aa$PORT_NAME = aa$VENT_CODE = aa$CULLS = NULL
    
    a = as.data.frame(rbind(a,aa))
    
    a$COMAREA_ID = toupper(a$COMAREA_ID)
  
    
    a$Legal = ifelse(a$SPECCD_ID == 2550 & a$FISH_LENGTH > 82 & a$SEXCD_ID %in% 1:2,1,0)
    a$Berried = ifelse(a$SPECCD_ID == 2550 & a$SEXCD_ID %in% 3,1,0)
    a$Lobster = ifelse(a$SPECCD_ID == 2550,1,0)
    a$Cod = ifelse(a$SPECCD_ID == 10,1,0)
    a$Cusk = ifelse(a$SPECCD_ID == 15,1,0)
    a$Jonah = ifelse(a$SPECCD_ID == 2511,1,0)
    a$Cunner = ifelse(a$SPECCD_ID == 122,1,0)
    a$SPECCD_ID  = ifelse(is.na(a$SPECCD_ID),9999,a$SPECCD_ID)
    a$Empty = ifelse(a$SPECCD_ID == 9999,1,0)
    a$LegalWt = a$Legal * a$CALWT_G/1000
    a$LegalWt[which(is.na(a$LegalWt))] <- 0
    a$CodWt = a$Cod * a$CALWT_G/1000
    a$CuskWt = a$Cusk * a$CALWT_G/1000
    a$JonahWt = a$Jonah * a$CALWT_G/1000
    a$CunnerWt = a$Cunner * a$CALWT_G/1000
    
    a$CodWt[which(is.na(a$CodWt))] <- 0
    a$CuskWt[which(is.na(a$CuskWt))] <- 0 
    a$JonahWt[which(is.na(a$JonahWt))] <- 0
    a$CALWT_G[which(is.na(a$CALWT_G))] <- 0
    a$CunnerWt[which(is.na(a$CunnerWt))] <- 0
    
    a$LobsterWt = a$Lobster * a$CALWT_G/1000
    a$LobsterWt[which(is.na(a$LobsterWt))] <- 0
    a$SPECCD_ID = bio.utilities::recode(a$SPECCD_ID, "300=310; 301=310; 302=310; 118=10;")
    
    
    a$Sz = round(a$FISH_LENGTH/5)*5
    a$SP_SZ = paste(a$SPECCD_ID, a$Sz, sep="-")
    
    a$UID = paste(a$TRIP, a$FISHSET_ID, a$TRAP_ID,sep="-")
    a$P = 1
    aa = aggregate(P~UID+SP_SZ,data=a,FUN=sum)
    bb = reshape(aa[,c('UID','SP_SZ','P')],idvar='UID',timevar='SP_SZ', direction='wide')
    bb = na.zero(bb)
    
    
    season.dates = backFillSeasonDates(lobster.db('season.dates'),eyr=year(Sys.time()))
    a = subset(a, !is.na(BOARD_DATE))
    a$LFA = as.numeric(unlist(lapply(strsplit(a$COMAREA_ID,'L'),"[[",2)))
    a = addSYEAR(a,date.field = 'BOARD_DATE')
    a$WOS = NA
    m=0
    lfa = unique(a$LFA) 
    lfa = na.omit(lfa)
    for(i in 1:length(lfa)) {
      h  = season.dates[season.dates$LFA==lfa[i],]  
      k = na.omit(unique(a$SYEAR[a$LFA==lfa[i]]))
      #h = na.omit(h)
      k = intersect(k,h$SYEAR)
      for(j in k){
        m=m+1
        ll = which(a$LFA==lfa[i] & a$SYEAR==j)
        a$WOS[ll] = floor(as.numeric(a$BOARD_DATE[ll]-min(h$START_DATE[h$SYEAR==j]))/7)+1
      }
    }
    if(any(!is.finite(a$WOS))) {kl = which(!is.finite(a$WOS)); a$WOS[kl] = NA}
    a = subset(a,WOS>0)
    
    
    
    #per trap
    ac = aggregate(cbind(Lobster, Cod, Cusk, Cunner, Jonah, Legal, Berried,Empty,CunnerWt, LegalWt,CALWT_G,CodWt,CuskWt,JonahWt,LobsterWt)~UID
                   +TRIP+X+Y+TRAP_ID+FISHSET_ID+COMAREA_ID+STRATUM_ID+NUM_HOOK_HAUL+BOARD_DATE+WOS+LFA+SYEAR, data=a,FUN=sum,na.rm=F)
    
    CDa = merge(ac,bb,by='UID')
    CDa$'P.9999-NA' = NULL
    CDa$P=1
    CDa$mn = month(CDa$BOARD_DATE)
    gt = bycatch.db('targets',wd=wd) 
    CDa$GridGroup = CDa$target = CDa$Period = NA
    for(i in 1:nrow(CDa)){
      pit = gt$GridGrouping[which(CDa$STRATUM_ID[i]==gt$GRID_NUM & gt$LFA==strsplit(CDa$COMAREA_ID[i],'L')[[1]][2])]
      if(length(pit)>0){
        CDa$GridGroup[i] = (pit)
        m = CDa$mn[i]
        k = subset(gt,GRID_NUM==CDa$STRATUM_ID[i] & LFA==strsplit(CDa$COMAREA_ID[i],'L')[[1]][2])
        ll = ifelse(m >=k$Period1.Start & m<=k$Period1.End,'Period1',ifelse(m >=k$Period2.Start & m<=k$Period2.End,'Period2',ifelse(m >=k$Period3.Start & m<=k$Period3.End,'Period3','Period4')))
        lll = as.numeric(strsplit(ll,'Period')[[1]][2])
        CDa$target[i] <- as.numeric(k[,ll])
        CDa$Period[i] = lll
        #	rm(k,m,ll,lll)
      }
    }
    CDa$COMAREA_ID = NULL
    names(CDa)[c(5:9,24)] =c('TRAPNO','SETNO','GRIDNUM','NUM_TRAPS','DATE_FISHED','CALWT')
    CDa$DID = 'ASSOC'
    CDa$ll = paste(CDa$X,CDa$Y,sep='ll')
    CDa$Cluster = NA
    cc = unique(CDa$TRIP)
    for(i in 1:length(cc)){
      k = which(CDa$TRIP == cc[i])
      CDa$Cluster[k] = create_seq_from_column(CDa$ll[k])
    }
    CDa$ll <- NULL
    
    CDa$SYEAR = year(CDa$DATE_FISHED)
    saveRDS(CDa,file=file.path(wd,'data','CBFHA.reshape.rds'))
    return(CDa)
    }
  return(readRDS(file=file.path(wd,'data','CBFHA.reshape.rds')))
}
  
  if(DS %in% c('CBFHA.all.species.redo','CBFHA.all.species') ){
    if(grepl('redo',DS)){
      a =   readRDS(file.path(wd,'data/CompiledAtSeaMarch2022.rds'))
      a = subset(a,OWNER_GROUP=='CBFHA')
      aa =  readRDS(file.path(wd,'data/CBFHA_2021_atsea.rds'))
      aa$PORT_NAME = aa$VENT_CODE = aa$CULLS = NULL
      
      a = as.data.frame(rbind(a,aa))
      
      a$COMAREA_ID = toupper(a$COMAREA_ID)
      
      
      a$Legal = ifelse(a$SPECCD_ID == 2550 & a$FISH_LENGTH > 82 & a$SEXCD_ID %in% 1:2,1,0)
      a$Berried = ifelse(a$SPECCD_ID == 2550 & a$SEXCD_ID %in% 3,1,0)
      a$Lobster = ifelse(a$SPECCD_ID == 2550,1,0)
      a$Cod = ifelse(a$SPECCD_ID == 10,1,0)
      a$Cusk = ifelse(a$SPECCD_ID == 15,1,0)
      a$Jonah = ifelse(a$SPECCD_ID == 2511,1,0)
      a$SPECCD_ID  = ifelse(is.na(a$SPECCD_ID),9999,a$SPECCD_ID)
      a$Empty = ifelse(a$SPECCD_ID == 9999,1,0)
      a$LegalWt = a$Legal * a$CALWT_G/1000
      a$LegalWt[which(is.na(a$LegalWt))] <- 0
      a$CodWt = a$Cod * a$CALWT_G/1000
      a$CuskWt = a$Cusk * a$CALWT_G/1000
      a$JonahWt = a$Jonah * a$CALWT_G/1000
      a$CodWt[which(is.na(a$CodWt))] <- 0
      a$CuskWt[which(is.na(a$CuskWt))] <- 0 
      a$JonahWt[which(is.na(a$JonahWt))] <- 0
      a$CALWT_G[which(is.na(a$CALWT_G))] <- 0
      a$LobsterWt = a$Lobster * a$CALWT_G/1000
      a$LobsterWt[which(is.na(a$LobsterWt))] <- 0
      a$SPECCD_ID = bio.utilities::recode(a$SPECCD_ID, "300=310; 301=310; 302=310; 118=10;")
      a$UID = paste(a$TRIP, a$FISHSET_ID, a$TRAP_ID,sep="-")
      a$P = 1
      aa = aggregate(P~UID+SPECCD_ID,data=a,FUN=sum)
      bb = reshape(aa[,c('UID','SPECCD_ID','P')],idvar='UID',timevar='SPECCD_ID', direction='wide')
      bb = na.zero(bb)
      
      
      season.dates = backFillSeasonDates(lobster.db('season.dates'),eyr=year(Sys.time()))
      a = subset(a, !is.na(BOARD_DATE))
      a$LFA = as.numeric(unlist(lapply(strsplit(a$COMAREA_ID,'L'),"[[",2)))
      a = addSYEAR(a,date.field = 'BOARD_DATE')
      a$WOS = NA
      m=0
      lfa = unique(a$LFA) 
      lfa = na.omit(lfa)
      for(i in 1:length(lfa)) {
        h  = season.dates[season.dates$LFA==lfa[i],]  
        k = na.omit(unique(a$SYEAR[a$LFA==lfa[i]]))
        #h = na.omit(h)
        k = intersect(k,h$SYEAR)
        for(j in k){
          m=m+1
          ll = which(a$LFA==lfa[i] & a$SYEAR==j)
          a$WOS[ll] = floor(as.numeric(a$BOARD_DATE[ll]-min(h$START_DATE[h$SYEAR==j]))/7)+1
        }
      }
      if(any(!is.finite(a$WOS))) {kl = which(!is.finite(a$WOS)); a$WOS[kl] = NA}
      a = subset(a,WOS>0)
      
      
      
      #per trap
      ac = aggregate(cbind(Lobster, Cod, Cusk, Jonah, Legal, Berried,Empty,LegalWt,CALWT_G,CodWt,CuskWt,JonahWt,LobsterWt)~UID
                     +TRIP+X+Y+TRAP_ID+FISHSET_ID+COMAREA_ID+STRATUM_ID+NUM_HOOK_HAUL+BOARD_DATE+WOS+LFA+SYEAR, data=a,FUN=sum,na.rm=F)
      
      CDa = merge(ac,bb,by='UID')
      CDa$'P.9999-NA' = NULL
      CDa$P=1
      CDa$mn = month(CDa$BOARD_DATE)
      gt = bycatch.db('targets',wd=wd) 
      CDa$GridGroup = CDa$target = CDa$Period = NA
      for(i in 1:nrow(CDa)){
        pit = gt$GridGrouping[which(CDa$STRATUM_ID[i]==gt$GRID_NUM & gt$LFA==strsplit(CDa$COMAREA_ID[i],'L')[[1]][2])]
        if(length(pit)>0){
          CDa$GridGroup[i] = (pit)
          m = CDa$mn[i]
          k = subset(gt,GRID_NUM==CDa$STRATUM_ID[i] & LFA==strsplit(CDa$COMAREA_ID[i],'L')[[1]][2])
          ll = ifelse(m >=k$Period1.Start & m<=k$Period1.End,'Period1',ifelse(m >=k$Period2.Start & m<=k$Period2.End,'Period2',ifelse(m >=k$Period3.Start & m<=k$Period3.End,'Period3','Period4')))
          lll = as.numeric(strsplit(ll,'Period')[[1]][2])
          CDa$target[i] <- as.numeric(k[,ll])
          CDa$Period[i] = lll
          #	rm(k,m,ll,lll)
        }
      }
      CDa$COMAREA_ID = NULL
      names(CDa)[c(5:9,21)] =c('TRAPNO','SETNO','GRIDNUM','NUM_TRAPS','DATE_FISHED','CALWT')
      CDa$DID = 'ASSOC'
      CDa$ll = paste(CDa$X,CDa$Y,sep='ll')
      CDa$Cluster = NA
      cc = unique(CDa$TRIP)
      for(i in 1:length(cc)){
        k = which(CDa$TRIP == cc[i])
        CDa$Cluster[k] = create_seq_from_column(CDa$ll[k])
      }
      CDa$ll <- NULL
      
      CDa$SYEAR = year(CDa$DATE_FISHED)
      saveRDS(CDa,file=file.path(wd,'data','CBFHA.all.species.reshape.rds'))
      return(CDa)
    }
    return(readRDS(file=file.path(wd,'data','CBFHA.all.species.reshape.rds')))
  }
  
  
if(DS %in% c('GCIFA',"GCIFA.redo")){
  if(grepl('redo',DS)){
    a =   readRDS(file.path(wd,'data/CompiledAtSeaMarch2022.rds'))
    a = subset(a,OWNER_GROUP=='GCIFA')
    
    a$COMAREA_ID = toupper(a$COMAREA_ID)
    
    a$Legal = ifelse(a$SPECCD_ID == 2550 & a$FISH_LENGTH > 82 & a$SEXCD_ID %in% 1:2,1,0)
    a$Legal = ifelse(a$SPECCD_ID == 2550 & a$COMAREA_ID=='L31A' & a$FISH_LENGTH %in% 115:125 & a$SEXCD_ID %in% 2,0,a$Legal)
    
    a$Berried = ifelse(a$SPECCD_ID == 2550 & a$SEXCD_ID %in% 3,1,0)
    a$Lobster = ifelse(a$SPECCD_ID == 2550,1,0)
    a$Cod = ifelse(a$SPECCD_ID == 10,1,0)
    a$Cusk = ifelse(a$SPECCD_ID == 15,1,0)
    a$Jonah = ifelse(a$SPECCD_ID == 2511,1,0)
    a$Cunner = ifelse(a$SPECCD_ID == 122,1,0)
    
    a$SPECCD_ID  = ifelse(is.na(a$SPECCD_ID),9999,a$SPECCD_ID)
    a$Empty = ifelse(a$SPECCD_ID == 9999,1,0)
    a$LegalWt = a$Legal * a$CALWT_G/1000
    a$LegalWt[which(is.na(a$LegalWt))] <- 0
    a$CodWt = a$Cod * a$CALWT_G/1000
    a$CuskWt = a$Cusk * a$CALWT_G/1000
    a$CunnerWt = a$Cunner * a$CALWT_G/1000
    
    a$JonahWt = a$Jonah * a$CALWT_G/1000
    a$CodWt[which(is.na(a$CodWt))] <- 0
    a$CuskWt[which(is.na(a$CuskWt))] <- 0 
    a$JonahWt[which(is.na(a$JonahWt))] <- 0
    a$CunnerWt[which(is.na(a$CunnerWt))] <- 0
    
    a$CALWT_G[which(is.na(a$CALWT_G))] <- 0
    a$LobsterWt = a$Lobster * a$CALWT_G/1000
    a$LobsterWt[which(is.na(a$LobsterWt))] <- 0
    a$SPECCD_ID = bio.utilities::recode(a$SPECCD_ID, "21=23; 65=9991; 120=122; 250=9991; 300=310; 455=640; 4250=4210; 301=310; 302=310; 118=10;")
    
    
    a$Sz = round(a$FISH_LENGTH/5)*5
    a$SP_SZ = paste(a$SPECCD_ID, a$Sz, sep="-")
    
    a$UID = paste(a$TRIP, a$FISHSET_ID, a$TRAP_ID,sep="-")
    a$P = 1
    aa = aggregate(P~UID+SP_SZ,data=a,FUN=sum)
    bb = reshape(aa[,c('UID','SP_SZ','P')],idvar='UID',timevar='SP_SZ', direction='wide')
    bb = na.zero(bb)
    
    
    season.dates = backFillSeasonDates(lobster.db('season.dates'),eyr=year(Sys.time()))
    a = subset(a, !is.na(BOARD_DATE))
    a$LFA = (unlist(lapply(strsplit(a$COMAREA_ID,'L'),"[[",2)))
    a = addSYEAR(a,date.field = 'BOARD_DATE')
    a$WOS = NA
    m=0
    lfa  = c('31A','31B')
    a = subset(a,LFA %in% lfa)
    for(i in 1:length(lfa)) {
      h  = season.dates[season.dates$LFA==lfa[i],]  
      k = na.omit(unique(a$SYEAR[a$LFA==lfa[i]]))
      #h = na.omit(h)
      k = intersect(k,h$SYEAR)
      for(j in k){
        ll = which(a$LFA==lfa[i] & a$SYEAR==j)
        a$WOS[ll] = floor(as.numeric(a$BOARD_DATE[ll]-min(h$START_DATE[h$SYEAR==j]))/7)+1
      }
    }
    if(any(!is.finite(a$WOS))) {kl = which(!is.finite(a$WOS)); a$WOS[kl] = NA}
    a = subset(a,WOS>0)
    
    #per trap
    ac = aggregate(cbind(Lobster, Cod, Cunner, Cusk, Jonah, Legal, Berried,Empty,LegalWt,CunnerWt, CALWT_G,CodWt,CuskWt,JonahWt,LobsterWt)~UID
                   +TRIP+X+Y+TRAP_ID+FISHSET_ID+COMAREA_ID+STRATUM_ID+BOARD_DATE+WOS+LFA+SYEAR, data=a,FUN=sum,na.rm=F)
    
    CDa = merge(ac,bb,by='UID')
    CDa$'P.9999-NA' = NULL
    CDa$P=1
    CDa$mn = month(CDa$BOARD_DATE)
    gt = bycatch.db('targets',wd=wd) 
     CDa$target = CDa$Period = NA
    CDa$GridGroup = 1
    for(i in 1:nrow(CDa)){
        m = CDa$mn[i]
        k = subset(gt, LFA==strsplit(CDa$COMAREA_ID[i],'L')[[1]][2])
        k = k[1,]
        ll = ifelse(m >=k$Period1.Start & m<=k$Period1.End,'Period1',ifelse(m >=k$Period2.Start & m<=k$Period2.End,'Period2',ifelse(m >=k$Period3.Start & m<=k$Period3.End,'Period3','Period4')))
        lll = as.numeric(strsplit(ll,'Period')[[1]][2])
        CDa$target[i] <- as.numeric(k[,ll])
        CDa$Period[i] = lll
        #	rm(k,m,ll,lll)
    }
    CDa$COMAREA_ID = NULL
      names(CDa)[c(5:8,22)] =c('TRAPNO','SETNO','GRIDNUM','DATE_FISHED','CALWT')
    CDa$DID = 'ASSOC'
    CDa$ll = paste(CDa$X,CDa$Y,sep='ll')
    CDa$Cluster = NA
    cc = unique(CDa$TRIP)
    for(i in 1:length(cc)){
      k = which(CDa$TRIP == cc[i])
      CDa$Cluster[k] = create_seq_from_column(CDa$ll[k])
    }
    CDa$ll <- NULL
    
    
    saveRDS(CDa,file=file.path(wd,'data','GCIFA.reshape.rds'))
    return(CDa)
  }
  return(readRDS(file=file.path(wd,'data','GCIFA.reshape.rds')))
  
  
      }
  
  if(DS %in% c('GCIFA.all.species',"GCIFA.all.species.redo")){
    if(grepl('redo',DS)){
      a =   readRDS(file.path(wd,'data/CompiledAtSeaMarch2022.rds'))
      a = subset(a,OWNER_GROUP=='GCIFA')
      
      a$COMAREA_ID = toupper(a$COMAREA_ID)
      
      a$Legal = ifelse(a$SPECCD_ID == 2550 & a$FISH_LENGTH > 82 & a$SEXCD_ID %in% 1:2,1,0)
      a$Legal = ifelse(a$SPECCD_ID == 2550 & a$COMAREA_ID=='L31A' & a$FISH_LENGTH %in% 115:125 & a$SEXCD_ID %in% 2,0,a$Legal)
      
      a$Berried = ifelse(a$SPECCD_ID == 2550 & a$SEXCD_ID %in% 3,1,0)
      a$Lobster = ifelse(a$SPECCD_ID == 2550,1,0)
      a$Cod = ifelse(a$SPECCD_ID == 10,1,0)
      a$Cusk = ifelse(a$SPECCD_ID == 15,1,0)
      a$Jonah = ifelse(a$SPECCD_ID == 2511,1,0)
      a$SPECCD_ID  = ifelse(is.na(a$SPECCD_ID),9999,a$SPECCD_ID)
      a$Empty = ifelse(a$SPECCD_ID == 9999,1,0)
      a$LegalWt = a$Legal * a$CALWT_G/1000
      a$LegalWt[which(is.na(a$LegalWt))] <- 0
      a$CodWt = a$Cod * a$CALWT_G/1000
      a$CuskWt = a$Cusk * a$CALWT_G/1000
      a$JonahWt = a$Jonah * a$CALWT_G/1000
      a$CodWt[which(is.na(a$CodWt))] <- 0
      a$CuskWt[which(is.na(a$CuskWt))] <- 0 
      a$JonahWt[which(is.na(a$JonahWt))] <- 0
      a$CALWT_G[which(is.na(a$CALWT_G))] <- 0
      a$LobsterWt = a$Lobster * a$CALWT_G/1000
      a$LobsterWt[which(is.na(a$LobsterWt))] <- 0
      a$SPECCD_ID = bio.utilities::recode(a$SPECCD_ID, "21=23; 65=9991; 120=122; 250=9991; 300=310; 455=640; 4250=4210; 301=310; 302=310; 118=10;")
      
      a$UID = paste(a$TRIP, a$FISHSET_ID, a$TRAP_ID,sep="-")
      a$P = 1
      aa = aggregate(P~UID+SPECCD_ID,data=a,FUN=sum)
      bb = reshape(aa[,c('UID','SPECCD_ID','P')],idvar='UID',timevar='SPECCD_ID', direction='wide')
      bb = na.zero(bb)
      
      
      season.dates = backFillSeasonDates(lobster.db('season.dates'),eyr=year(Sys.time()))
      a = subset(a, !is.na(BOARD_DATE))
      a$LFA = (unlist(lapply(strsplit(a$COMAREA_ID,'L'),"[[",2)))
      a = addSYEAR(a,date.field = 'BOARD_DATE')
      a$WOS = NA
      m=0
      lfa  = c('31A','31B')
      a = subset(a,LFA %in% lfa)
      for(i in 1:length(lfa)) {
        h  = season.dates[season.dates$LFA==lfa[i],]  
        k = na.omit(unique(a$SYEAR[a$LFA==lfa[i]]))
        #h = na.omit(h)
        k = intersect(k,h$SYEAR)
        for(j in k){
          ll = which(a$LFA==lfa[i] & a$SYEAR==j)
          a$WOS[ll] = floor(as.numeric(a$BOARD_DATE[ll]-min(h$START_DATE[h$SYEAR==j]))/7)+1
        }
      }
      if(any(!is.finite(a$WOS))) {kl = which(!is.finite(a$WOS)); a$WOS[kl] = NA}
      a = subset(a,WOS>0)
      
      #per trap
      ac = aggregate(cbind(Lobster, Cod, Cusk, Jonah, Legal, Berried,Empty,LegalWt,CALWT_G,CodWt,CuskWt,JonahWt,LobsterWt)~UID
                     +TRIP+X+Y+TRAP_ID+FISHSET_ID+COMAREA_ID+STRATUM_ID+BOARD_DATE+WOS+LFA+SYEAR, data=a,FUN=sum,na.rm=F)
      
      CDa = merge(ac,bb,by='UID')
      CDa$'P.9999-NA' = NULL
      CDa$P=1
      CDa$mn = month(CDa$BOARD_DATE)
      gt = bycatch.db('targets',wd=wd) 
      CDa$target = CDa$Period = NA
      CDa$GridGroup = 1
      for(i in 1:nrow(CDa)){
        m = CDa$mn[i]
        k = subset(gt, LFA==strsplit(CDa$COMAREA_ID[i],'L')[[1]][2])
        k = k[1,]
        ll = ifelse(m >=k$Period1.Start & m<=k$Period1.End,'Period1',ifelse(m >=k$Period2.Start & m<=k$Period2.End,'Period2',ifelse(m >=k$Period3.Start & m<=k$Period3.End,'Period3','Period4')))
        lll = as.numeric(strsplit(ll,'Period')[[1]][2])
        CDa$target[i] <- as.numeric(k[,ll])
        CDa$Period[i] = lll
        #	rm(k,m,ll,lll)
      }
      CDa$COMAREA_ID = NULL
      names(CDa)[c(5:8,20)] =c('TRAPNO','SETNO','GRIDNUM','DATE_FISHED','CALWT')
      CDa$DID = 'ASSOC'
      CDa$ll = paste(CDa$X,CDa$Y,sep='ll')
      CDa$Cluster = NA
      cc = unique(CDa$TRIP)
      for(i in 1:length(cc)){
        k = which(CDa$TRIP == cc[i])
        CDa$Cluster[k] = create_seq_from_column(CDa$ll[k])
      }
      CDa$ll <- NULL
      
      
      saveRDS(CDa,file=file.path(wd,'data','GCIFA.all.species.reshape.rds'))
      return(CDa)
    }
    return(readRDS(file=file.path(wd,'data','GCIFA.all.species.reshape.rds')))
    
    
  }
  
  
  
  
  
    
}