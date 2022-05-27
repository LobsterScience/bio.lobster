require(bio.lobster)
require(bio.utilities)
require(RODBC)
require(lubridate)
require(devtools)
options(stringAsFactors=F)
la()

wd = ('C:\\Users\\Cooka\\OneDrive - DFO-MPO\\BycatchLobster')

setwd(wd)


####Comparison of Lobster Trap catches to Reported on Logs

        b=bycatch.db('logbook.merge')
     
      
      #ISDB 
          #ISDB 
        ao = bycatch.db('ISDB.reshape')
        aCo = aggregate(cbind(Lobster,Cod,Cusk,Jonah,Legal,Berried, Empty,LegalWt)~TRIP+LFA+Cluster,data=ao,FUN=mean)
        aCs = aggregate(cbind(Lobster,Cod,Cusk,Jonah,Legal,Berried, Empty,LegalWt)~TRIP+LFA+Cluster,data=ao,FUN=var,na.rm=T)
        aCs = rename.df(aCs,c('Lobster','Cod','Cusk','Jonah','Legal','Berried', 'Empty','LegalWt'),c('Lobstervar','Codvar','Cuskvar','Jonahvar','Legalvar','Berriedvar', 'Emptyvar','LegalWtvar'))
        aCC = aggregate(UID~TRIP+LFA+Cluster,data=ao,FUN=function(x) length(unique(x)))
        aCo = merge(aCo, aCC)
        aCo = merge(aCo, aCs)
        aCo$mean =aCo$LegalWt
        aCo$var =aCo$LegalWtvar
        aCo$n = aCo$UID
        
        #clustered mean
        o = list()
        ui = unique(aCo$TRIP)
        for(i in 1:length(ui)){
              j = which(aCo$TRIP == ui[i])
              o[[i]] =c(TRIP=unique(aCo$TRIP[j]),LFA=unique(aCo$LFA[j]),clusterStatistics(aCo[j,]))      
            }
        aCo = as.data.frame(do.call(rbind,o))
        aCo = toNums(aCo,1:6)
        
      ms = read.csv('data/ISDB_Trip_id to sd_log_idFinal.csv')
      ms = subset(ms,!is.na(SD_LOG_ID_1),select=c(trip_id,SD_LOG_ID_1))
      
      bLo = merge(b,ms,by.x='SD_LOG_ID', by.y='SD_LOG_ID_1')
      bLoL = aggregate(cbind(WEIGHT_KG,NUM_OF_TRAPS)~SD_LOG_ID+trip_id+LFA+SYEAR,data=bLo,FUN=sum)
      SBUMP0 = merge(aCo,bLoL,by.x='TRIP',by.y='trip_id')

      SBUMP0$TotLegalNTraps = SBUMP0$mean * SBUMP0$NUM_OF_TRAPS

      #SWLSS
  
        aoS = bycatch.db('SWLSS')
        aCoS = aggregate(cbind(Lobster,Cod,Cusk,Jonah,Legal,Berried, Empty,LegalWt)~TRIP+LFA+Cluster,data=aoS,FUN=mean)
        aCsS = aggregate(cbind(Lobster,Cod,Cusk,Jonah,Legal,Berried, Empty,LegalWt)~TRIP+LFA+Cluster,data=aoS,FUN=var,na.rm=T)
        aCsS = rename.df(aCsS,c('Lobster','Cod','Cusk','Jonah','Legal','Berried', 'Empty','LegalWt'),c('Lobstervar','Codvar','Cuskvar','Jonahvar','Legalvar','Berriedvar', 'Emptyvar','LegalWtvar'))
        aCCS = aggregate(UID~TRIP+LFA+Cluster,data=aoS,FUN=function(x) length(unique(x)))
        aCoS = merge(aCoS, aCCS)
        aCoS = merge(aCoS, aCsS)
        aCoS$mean =aCoS$LegalWt
        aCoS$var =aCoS$LegalWtvar
        aCoS$n = aCoS$UID
       o = list()
        ui = unique(aCoS$TRIP)
        for(i in 1:length(ui)){
              j = which(aCoS$TRIP == ui[i])
              o[[i]] =c(TRIP=unique(aCoS$TRIP[j]),LFA=unique(aCoS$LFA[j]),clusterStatistics(aCoS[j,]))      
            }
        aCoS = as.data.frame(do.call(rbind,o))
        aCoS = toNums(aCoS,2:6)
        aacc = as.data.frame(rbind(aCo,aCoS))
        write.csv(aacc,file='results/TRIPCPUE33-35.csv')
        
      
       ms = read.csv('data/SWLSSTripmatch.csv')
      ms = subset(ms,!is.na(SD_LOG_ID_1),select=c(TRIP,SD_LOG_ID_1))
      
      bLoS = merge(b,ms,by.x='SD_LOG_ID', by.y='SD_LOG_ID_1')
      bLoLS = aggregate(cbind(WEIGHT_KG,NUM_OF_TRAPS)~SD_LOG_ID+TRIP+LFA+SYEAR,data=bLoS,FUN=sum)
      SBUMPS = merge(aCoS,bLoLS,by.x='TRIP',by.y='TRIP')
      
      SBUMPS$TotLegalNTraps = SBUMPS$mean * SBUMPS$NUM_OF_TRAPS
      
      
      
SBUMP0 = subset(SBUMP0,mean>0)
SBUMPS = subset(SBUMPS,mean>0)
      
#FOR ALL DATA COMBINED          
SB = rbind(SBUMPS,SBUMP0)          

require(MASS)
          require(SimDesign)
                par(mfrow=c(1,2))
          with(SBUMPS,plot(WEIGHT_KG,TotLegalNTraps,ylab='Extrapolated landings from Sea Samples',main='SWLSS'))
          abline(a=0,b=1)
          LMSW = with(SBUMPS,lm((TotLegalNTraps)~(WEIGHT_KG)-1)) #treating the SWLSS data as 'truth'
          RLMSW = with(SBUMPS,rlm((TotLegalNTraps)~(WEIGHT_KG)-1)) #treating the SWLSS data as 'truth'
          with(SBUMP0,plot(WEIGHT_KG,TotLegalNTraps,ylab='Extrapolated landings from Sea Samples',main='Observer'))
          abline(a=0,b=1)
          LMO = (with(SBUMP0,lm((TotLegalNTraps)~(WEIGHT_KG)-1)))
          RLMO = (with(SBUMP0,rlm((TotLegalNTraps)~(WEIGHT_KG)-1)))
          
          with(SB,plot(WEIGHT_KG,TotLegalNTraps,ylab='Extrapolated landings from Sea Samples',main='All'))
          abline(a=0,b=1)
          LMA = (with(SB,lm((TotLegalNTraps)~(WEIGHT_KG)-1)))
          RLMA = (with(SB,rlm((TotLegalNTraps)~(WEIGHT_KG)-1)))
          
          
          #RMSE
          summary(LMO)$sigma
          summary(RLMO)$sigma
          
          summary(LMSW)$sigma
          summary(RLMSW)$sigma
          
          summary(LMA)$sigma
          summary(RLMA)$sigma
          
          biasSWLSS = SimDesign::bias(SBUMPS$TotLegalNTraps,SBUMPS$WEIGHT_KG,type='relative')
          biasOBs = SimDesign::bias( SBUMP0$TotLegalNTraps ,SBUMP0$WEIGHT_KG ,type='relative') # sum(SBUMP0$TotLegalNTraps - SBUMP0$WEIGHT_KG)/nrow(SBUMP0) under
          biasC = SimDesign::bias( SB$TotLegalNTraps ,SB$WEIGHT_KG ,type='relative') 
          
          
       

#For Each LFA and YEAR
SBS = split(SB, f=list(SB$LFA.x,SB$SYEAR))
SBS = rm.from.list(SBS)
ooo = list()
for(i in 1:length(SBS)){
    O = SBS[[i]]        
  L = with(O,lm((TotLegalNTraps)~(WEIGHT_KG)-1)) #treating the SWLSS data as 'truth'
  RL = with(O,rlm((TotLegalNTraps)~(WEIGHT_KG)-1)) #treating the SWLSS data as 'truth'
                  #RMSE
          summary(L)$sigma
          summary(RL)$sigma
          
          
          biasS = SimDesign::bias(O$TotLegalNTraps,O$WEIGHT_KG,type='relative')
    ooo[[i]] = c(unique(O$LFA.x),unique(O$SYEAR),nrow(O),biasS,coef(L),summary(L)$sigma,coef(RL),summary(RL)$sigma)   
          
}
  ooo = as.data.frame(do.call(rbind,ooo))

###GCIFA and CBFHA
  gg = bycatch.db('GCIFA')

         aCg = aggregate(cbind(Lobster,Cod,Cusk,Jonah,Legal,Berried, Empty,LegalWt)~TRIP+LFA+Cluster,data=gg,FUN=mean)
        aCgg = aggregate(cbind(Lobster,Cod,Cusk,Jonah,Legal,Berried, Empty,LegalWt)~TRIP+LFA+Cluster,data=gg,FUN=var,na.rm=T)
        aCg = rename.df(aCg,c('Lobster','Cod','Cusk','Jonah','Legal','Berried', 'Empty','LegalWt'),c('Lobstervar','Codvar','Cuskvar','Jonahvar','Legalvar','Berriedvar', 'Emptyvar','LegalWtvar'))
        aCCg = aggregate(UID~TRIP+LFA+Cluster,data=gg,FUN=function(x) length(unique(x)))
        aCog = merge(aCg, aCCg)
        aCog = merge(aCog, aCCg)
        aCog$mean =aCog$LegalWt
        aCog$var =aCog$LegalWtvar
        aCog$n = aCog$UID
       o = list()
        ui = unique(aCog$TRIP)
        for(i in 1:length(ui)){
              j = which(aCog$TRIP == ui[i])
              o[[i]] =c(TRIP=unique(aCog$TRIP[j]),LFA=unique(aCog$LFA[j]),clusterStatistics(aCog[j,]))      
            }
        aCog = as.data.frame(do.call(rbind,o))
       ggo = toNums(aCog,3:6)
    gga=ggo
  ms = read.csv('data/GCIFACBFHATripMatch.csv')
  #off record matching...skip
  #ms$SD_LOG_ID_DO[which(is.na(ms$SD_LOG_ID_DO))] = ms$SD_LOG_ID_DB[which(is.na(ms$SD_LOG_ID_DO))]
  #ms$SD_LOG_ID_DO[which(is.na(ms$SD_LOG_ID_DO))] = ms$SD_LOG_ID_DA[which(is.na(ms$SD_LOG_ID_DO))]
  
  ms = subset(ms,select=c(TRIP,SD_LOG_ID_DO))
  
  bLo = merge(b,ms,by.x='SD_LOG_ID', by.y='SD_LOG_ID_DO')
  bLoL = aggregate(cbind(WEIGHT_KG,NUM_OF_TRAPS)~SD_LOG_ID+TRIP+LFA+SYEAR,data=bLo,FUN=sum)
  bu = merge(ggo,bLoL)
  
  bu$TotLegalNTraps = bu$mean * bu$NUM_OF_TRAPS
  
  GC = split(bu,f=list(bu$LFA,bu$SYEAR))
  og = list()
  for(i in 1:length(GC)){
    O = GC[[i]]        
    L = with(O,lm((TotLegalNTraps)~(WEIGHT_KG)-1)) #treating the SWLSS data as 'truth'
    RL = with(O,rlm((TotLegalNTraps)~(WEIGHT_KG)-1)) #treating the SWLSS data as 'truth'
    biasS = SimDesign::bias(O$TotLegalNTraps,O$WEIGHT_KG,type='relative')
    og[[i]] = c(unique(O$LFA),unique(O$SYEAR),nrow(O),biasS,coef(L),summary(L)$sigma,coef(RL),summary(RL)$sigma)   
    
  }
  og = as.data.frame(do.call(rbind,og))
  
  

  #################
  ### CBFHA
  
  gg = bycatch.db('CBFHA')
        aCg = aggregate(cbind(Lobster,Cod,Cusk,Jonah,Legal,Berried, Empty,LegalWt)~TRIP+LFA+Cluster,data=gg,FUN=mean)
        aCgg = aggregate(cbind(Lobster,Cod,Cusk,Jonah,Legal,Berried, Empty,LegalWt)~TRIP+LFA+Cluster,data=gg,FUN=var,na.rm=T)
        aCg = rename.df(aCg,c('Lobster','Cod','Cusk','Jonah','Legal','Berried', 'Empty','LegalWt'),c('Lobstervar','Codvar','Cuskvar','Jonahvar','Legalvar','Berriedvar', 'Emptyvar','LegalWtvar'))
        aCCg = aggregate(UID~TRIP+LFA+Cluster,data=gg,FUN=function(x) length(unique(x)))
        aCog = merge(aCg, aCCg)
        aCog = merge(aCog, aCCg)
        aCog$mean =aCog$LegalWt
        aCog$var =aCog$LegalWtvar
        aCog$n = aCog$UID
       o = list()
        ui = unique(aCog$TRIP)
        for(i in 1:length(ui)){
              j = which(aCog$TRIP == ui[i])
              o[[i]] =c(TRIP=unique(aCog$TRIP[j]),LFA=unique(aCog$LFA[j]),clusterStatistics(aCog[j,]))      
            }
        aCog = as.data.frame(do.call(rbind,o))
       ggo = toNums(aCog,3:6)

gggg = as.data.frame(rbind(gga,ggo))
write.csv(gggg,file='results/CBFHAGCIFACPUE.csv')

  ms = read.csv('data/GCIFACBFHATripMatch.csv')
  #ms$SD_LOG_ID_DO[which(is.na(ms$SD_LOG_ID_DO))] = ms$SD_LOG_ID_DB[which(is.na(ms$SD_LOG_ID_DO))]
  #ms$SD_LOG_ID_DO[which(is.na(ms$SD_LOG_ID_DO))] = ms$SD_LOG_ID_DA[which(is.na(ms$SD_LOG_ID_DO))]
  
  ms = subset(ms,select=c(TRIP,SD_LOG_ID_DO))
  
  bLo = merge(b,ms,by.x='SD_LOG_ID', by.y='SD_LOG_ID_DO')
  bLoL = aggregate(cbind(WEIGHT_KG,NUM_OF_TRAPS)~SD_LOG_ID+TRIP+LFA+SYEAR,data=bLo,FUN=sum)
  bu = merge(ggo,bLoL)
  
  bu$TotLegalNTraps = bu$mean * bu$NUM_OF_TRAPS
  
  GC = split(bu,f=list(bu$LFA,bu$SYEAR))
  og = list()
  for(i in 1:length(GC)){
    O = GC[[i]]        
    L = with(O,lm((TotLegalNTraps)~(WEIGHT_KG)-1)) #treating the SWLSS data as 'truth'
    RL = with(O,rlm((TotLegalNTraps)~(WEIGHT_KG)-1)) #treating the SWLSS data as 'truth'
    biasS = SimDesign::bias(O$TotLegalNTraps,O$WEIGHT_KG,type='relative')
    og[[i]] = c(unique(O$LFA),unique(O$SYEAR),nrow(O),biasS,coef(L),summary(L)$sigma,coef(RL),summary(RL)$sigma)   
    
  }
  og = as.data.frame(do.call(rbind,og))
  
  
  
  
  
  
  