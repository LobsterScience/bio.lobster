#' @export
fisheryFootprintData <- function(yrs=2005:2024, layerDir=file.path(git.repo,'bio.lobster.data','mapping_data'),period='annual'){
  
  
  r<-readRDS(file.path( layerDir,"GridPolysSF.rds"))
  r = st_as_sf(r)
  
  a =  lobster.db('process.logs')
  a = subset(a,SYEAR %in% yrs)
  
  b = lobster.db('seasonal.landings')
  b = subset(b,!is.na(SYEAR))
  b$SYEAR = as.numeric(substr(b$SYEAR,6,9))
  b$LFA38B <- NULL
  b = subset(b,SYEAR %in% yrs)
  b = reshape(b,idvar='SYEAR', varying=list(2:6),direction='long')
  b$LFA=rep(c(33,34,35,36,38),each=length(yrs))
  b$time <- NULL
  names(b)[1:2]=c('YR','SlipLand')
  
  
  d = lobster.db('annual.landings')
  d = subset(d,YR %in% yrs, select=c(YR,LFA27,LFA28,LFA29,LFA30,LFA31A,LFA31B,LFA32))
  d = reshape(d,idvar='YR', varying=list(2:8),direction='long')
  d$LFA=rep(c(27,28,29,30,'31A','31B',32),each=length(yrs))
  d$time <- NULL
  names(d)[1:2]=c('YR','SlipLand')
  bd = rbind(d,b)
  
  bup = aggregate(cbind(WEIGHT_KG,NUM_OF_TRAPS)~SYEAR+LFA,data=a,FUN=sum)
  bAll = merge(bd,bup,by.x=c('YR','LFA'),by.y=c('SYEAR','LFA'))
  
  sL= split(a,f=list(a$LFA, a$SYEAR))
  sL = bio.utilities::rm.from.list(sL)
  cpue.lst<-list()
  cpue.ann = list()
  
  for(i in 1:length(sL)){
    tmp<-sL[[i]]
    tmp = tmp[,c('DATE_FISHED','WEIGHT_KG','NUM_OF_TRAPS')]
    names(tmp)<-c('time','catch','effort')
    tmp$date<-as.Date(tmp$time)
    first.day<-min(tmp$date)
    tmp$time<-julian(tmp$date,origin=first.day-1)
    g<-biasCorrCPUE(tmp)
    cpue.lst[[i]] <- c(lfa=unique(sL[[i]]$LFA),yr = unique(sL[[i]]$SYEAR),g)
  }
  
  cc =as.data.frame(do.call(rbind,cpue.lst))
  
  #annual/seasonal trap hauls
  cAll = merge(bAll,cc,by.x=c('LFA','YR'),by.y=c('lfa','yr'))
  cAll$NTRAPs = cAll$SlipLand*1000/as.numeric(cAll$unBCPUE)
  cAll$NTRAPSU = cAll$SlipLand*1000/as.numeric(cAll$l95)
  cAll$NTRAPSL = cAll$SlipLand*1000/as.numeric(cAll$u95)
  
  
  if(period=='annual'){
  #part the effort to grids
        partEffort = list()
        for(i in 1:length(sL)){
              tmp = sL[[i]]
              tTH = aggregate(NUM_OF_TRAPS~LFA,data=tmp,FUN=sum)
              tC = subset(cAll, LFA==unique(tmp$LFA) & YR == unique(tmp$SYEAR)) 
              pTH = aggregate(NUM_OF_TRAPS~GRID_NUM+LFA+SYEAR,data=tmp,FUN=sum)
              pTH$BTTH = round(pTH$NUM_OF_TRAPS / tTH$NUM_OF_TRAPS * tC$NTRAPs)
              pTH$BlTH = round(pTH$NUM_OF_TRAPS / tTH$NUM_OF_TRAPS * tC$NTRAPSL)
              pTH$BuTH = round(pTH$NUM_OF_TRAPS / tTH$NUM_OF_TRAPS * tC$NTRAPSU)
              
              partEffort[[i]] = pTH
          }
        partEffort = do.call(rbind, partEffort)
  #part the landings to grids
      partLandings = list()
            for(i in 1:length(sL)){
                tmp = sL[[i]]
                tTH = aggregate(WEIGHT_KG~LFA,data=tmp,FUN=sum)
                tC = subset(cAll, LFA==unique(tmp$LFA) & YR == unique(tmp$SYEAR)) 
                pTH = aggregate(WEIGHT_KG~GRID_NUM+LFA+SYEAR,data=tmp,FUN=sum)
                pTH$BL = round(pTH$WEIGHT_KG / (tTH$WEIGHT_KG )* (tC$SlipLand*1000))
                partLandings[[i]] = pTH
              }
        partLandings = do.call(rbind, partLandings)
  
  #trips
      gg = aggregate(SD_LOG_ID~LFA+GRID_NUM+SYEAR,data = a,FUN=function(x) length(unique(x)))
  
  #licences
      gl = aggregate(LICENCE_ID~LFA+GRID_NUM+SYEAR,data = a,FUN=function(x) length(unique(x)))
      
    Tot = merge(merge(merge(partEffort,partLandings),gg),gl)
  
    Tot = subset(Tot,select=c(SYEAR,LFA,GRID_NUM,BTTH,BL,SD_LOG_ID,LICENCE_ID))
    names(Tot)= c('FishingYear','LFA','Grid','TrapHauls','Landings','Trips','Licences')
        }
  
  
if(period=='weekly'){
  
    #part the effort to grids
    partEffort = list()
    for(i in 1:length(sL)){
      tmp = sL[[i]]
      tTH = aggregate(NUM_OF_TRAPS~LFA,data=tmp,FUN=sum)
      tC = subset(cAll, LFA==unique(tmp$LFA) & YR == unique(tmp$SYEAR)) 
      pTH = aggregate(NUM_OF_TRAPS~GRID_NUM+WOS+LFA+SYEAR,data=tmp,FUN=sum)
      pTH$BTTH = round(pTH$NUM_OF_TRAPS / tTH$NUM_OF_TRAPS * tC$NTRAPs)
      pTH$BlTH = round(pTH$NUM_OF_TRAPS / tTH$NUM_OF_TRAPS * tC$NTRAPSL)
      pTH$BuTH = round(pTH$NUM_OF_TRAPS / tTH$NUM_OF_TRAPS * tC$NTRAPSU)
      
      partEffort[[i]] = pTH
    }
    partEffort = do.call(rbind, partEffort)
    #part the landings to grids
    partLandings = list()
    for(i in 1:length(sL)){
      tmp = sL[[i]]
      tTH = aggregate(WEIGHT_KG~LFA,data=tmp,FUN=sum)
      tC = subset(cAll, LFA==unique(tmp$LFA) & YR == unique(tmp$SYEAR)) 
      pTH = aggregate(WEIGHT_KG~GRID_NUM+WOS+LFA+SYEAR,data=tmp,FUN=sum)
      pTH$BL = round(pTH$WEIGHT_KG / (tTH$WEIGHT_KG )* (tC$SlipLand*1000))
      partLandings[[i]] = pTH
    }
    partLandings = do.call(rbind, partLandings)
    
    #trips
    gg = aggregate(SD_LOG_ID~LFA+WOS+GRID_NUM+SYEAR,data = a,FUN=function(x) length(unique(x)))
    
    #licences
    gl = aggregate(LICENCE_ID~LFA+WOS+GRID_NUM+SYEAR,data = a,FUN=function(x) length(unique(x)))
    
    Tot = merge(merge(merge(partEffort,partLandings),gg),gl)
    
    Tot = subset(Tot,select=c(SYEAR,LFA,GRID_NUM,WOS,BTTH,BL,SD_LOG_ID,LICENCE_ID))
    names(Tot)= c('FishingYear','LFA','Grid',"WeekOfSeason",'TrapHauls','Landings','Trips','Licences')
}
  
  return(Tot)
  
}