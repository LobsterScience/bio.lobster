require(bio.lobster)
require(bio.utilities)
require(RODBC)
require(lubridate)
require(devtools)
options(stringAsFactors=F)
la()

wd = ('C:\\Users\\Cooka\\OneDrive - DFO-MPO\\BycatchLobster')

setwd(wd)

bycatch.db('SWLSS.redo')
bycatch.db('GCIFA.redo')
bycatch.db('CBFHA.redo')


####Trips to Targets 
      b = bycatch.db('logbook.merge',wd=wd) 
      gt = t = bycatch.db('targets',wd=wd) 
      t = subset(t,LFA%in%c(27,'31A','31B',33:35))
      tt = aggregate(cbind(Period1,Period2,Period3)~LFA+GridGrouping,data=t,FUN=min)
      
      bA = aggregate(SD_LOG_ID~Period+GridGroup+LFA+SYEAR,data=b,FUN=function(x) length(unique(x)))
      tR = as.data.frame(reshape(tt,varying=3:5,direction = 'long',sep=""))
      names(tR) = c('LFA','GridGroup','Period','Target','Nt')
      tR$Nt = NULL

      bA = merge(bA,tR,all.x=T)

      sss = readRDS('results/BumpedUpEffortByGridGroup.rds')
      sss= subset(sss,select=c(Period,GridGroup,LFA,SYEAR,BTTH))
      
      bA = merge(bA,sss)
      names(bA)[c(5,7)] = c('LogsReported','BumpTH')
write.csv(bA,file=file.path(wd,'results','LogbooksAgg and Targets.csv'))

#logbooks

    bA = read.csv(file=file.path(wd,'results','LogbooksAgg and Targets.csv'))
    bA$X = NULL
#SWLSS sea sampling
        CDa = bycatch.db(DS='SWLSS')
        STraps = aggregate(P~SYEAR+GridGroup+LFA+Period+target,data=CDa,FUN=sum)
        ATrips = aggregate(TRIP~SYEAR+LFA+Period+target+GridGroup,data=CDa,FUN=function(x) length(unique(x)))
        STTraps =merge(STraps,ATrips,all=T)
        names(STTraps)[6:7] = c('NTrapsSampledSWLSS','NTripsSampledSWLSS')
          write.csv(STTraps,file=file.path('results','SWLSSTrips2Targets.csv'))
       
        STTraps = merge(STTraps,bA,all=T)
        write.csv(STTraps,file=file.path('results','SWLSSTrips2TargetsTrapsandTrips.csv'))
        STTraps = read.csv(file=file.path('results','SWLSSTrips2TargetsTrapsandTrips.csv'))
        STTraps$x = NULL
#adding IN ISBD
        gg = bycatch.db('ISDB')
        ## Trips that dont match with grids or something
        gr = subset(gg,is.na(target))
        gr = as.data.frame(unique(cbind(gr$TRIPNO,gr$LFA,gr$GRIDNO)))
        se = connect.command(con,'select * from lobster.issets_mv')
        
        names(gr) = c('TRIP_ID','LFA','Grid')
        ss = as.data.frame(unique(cbind(se$TRIP_ID,se$TRIP)))
        names(ss) = c('TRIP_ID','TRIP')
        merge(gr,ss)

###Moving on to summary of observer data update later after we get more complete info

        UTraps = aggregate(UID~SYEAR+LFA+GridGroup+Period+target,data=gg,FUN=function(x) length(unique(x)))
        UTrips = aggregate(TRIPNO~SYEAR+LFA+GridGroup+Period+target,data=gg,FUN=function(x) length(unique(x)))
        ggg = merge(UTrips,UTraps,all=T)
        names(ggg)[c(1,5:7)] = c('SYEAR','Target','ObserverTrips','ObserverTraps')
        
        STTraps = merge(STTraps,ggg,all=T)
        STTraps$X=STTraps$target = NULL
        
        write.csv(STTraps,file=file.path('results','SWLSSandObsTrips2TargetsTrapsandTrips.csv'))
        STTraps = read.csv(file=file.path('results','SWLSSandObsTrips2TargetsTrapsandTrips.csv'))

#adding in CBFHA

        gg = bycatch.db('CBFHA')
        STraps = aggregate(P~SYEAR+GridGroup+LFA+Period+target,data=gg,FUN=sum)
        ATrips = aggregate(TRIP~SYEAR+LFA+Period+target+GridGroup,data=gg,FUN=function(x) length(unique(x)))
        CBTraps =merge(STraps,ATrips,all=T)
        names(CBTraps)[6:7] = c('NTrapsSampledCBFHA','NTripsSampledCBFHA')
        CBTraps$target=NULL
        write.csv(CBTraps,file=file.path('results','CBFHATrips2Targets.csv'))
        
        STTraps = merge(STTraps,CBTraps,all=T)
        write.csv(STTraps,file=file.path('results','CBFHA_OBS_SWLSSTrips2TargetsTrapsandTrips.csv'))
        STTraps = read.csv(file=file.path('results','CBFHA_OBS_SWLSSTrips2TargetsTrapsandTrips.csv'))
        STTraps$X.1 = STTraps$X = NULL

#adding in GCIFA

        gg = bycatch.db('GCIFA')
        STraps = aggregate(P~SYEAR+GridGroup+LFA+Period+target,data=gg,FUN=sum)
        ATrips = aggregate(TRIP~SYEAR+LFA+Period+target+GridGroup,data=gg,FUN=function(x) length(unique(x)))
        CBTraps =merge(STraps,ATrips,all=T)
        CBTraps$target = NULL
        names(CBTraps)[5:6] = c('NTrapsSampledGCIFA','NTripsSampledGCIFA')
        write.csv(CBTraps,file=file.path('results','GCIFATrips2Targets.csv'))
        
        STTraps = merge(STTraps,CBTraps,all=T)
            write.csv(STTraps,file=file.path('results','GCIFA_CBFHA_OBS_SWLSSTrips2TargetsTrapsandTrips.csv'))


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
  
  
  
  
  
  
  
  
  ##########################################################
b = bycatch.db('logbook.merge',wd=wd) 
bW = aggregate(NUM_OF_TRAPS~WOS+SYEAR+LFA,data=b,FUN=sum)


aO = bycatch.db('ISDB.reshape')
aS = bycatch.db(DS='SWLSS')

nO = names(aO)
nS = names(aS)

nO = grep('P.',nO,invert=T)
nS = grep('P.',nS,invert=T)

aO = aO[,c(nO,12)]
aS = aS[,c(nS,426)]

aS$SETNO = aS$mn = NULL
aA = rbind(aO,aS)

aA$GPY = paste(aA$SYEAR,aA$LFA,aA$GridGroup,aA$Period,sep="-")
io = unique(aA$GPY)
aA$GP = paste(aA$LFA,aA$GridGroup,sep="-")

write.csv(aA,file=file.path('results','CompliedDataForModelling.csv'))



#Comparing Temporal sampling with Fishing (by LFA)
b = bycatch.db('logbook.merge',wd=wd) 
bW = aggregate(NUM_OF_TRAPS~WOS+SYEAR+LFA,data=b,FUN=sum)


aO = bycatch.db('ISDB.reshape')
aS = bycatch.db(DS='SWLSS')

              ii = c(33,34,35)
              jj = 2019:2021
              par(mfrow=c(3,3))
              for(i in ii){
                for(j in jj){
                  
                      bss = subset(bW,LFA==i & SYEAR==j)
                      bss$CE = cumsum(bss$NUM_OF_TRAPS)/sum(bss$NUM_OF_TRAPS)
                      plot(bss$WOS,bss$CE,type='l',lwd=2,xlab='WOS',ylab='Cumulative Effort',main=paste(i,j,sep="-"))
                      
                      aOs = subset(aO,LFA==i & SYEAR==j)
                      if(nrow(aOs)>0){
                        print(c(i,j))
                      aOs = aggregate(UID~WOS,data=aOs,FUN=function(x) length(unique(x)))
                      aOs$CE = cumsum(aOs$UID)/sum(aOs$UID)
                      lines(aOs$WOS,aOs$CE,type='l',lwd=2,col='red',lty=2)
                      }
                      aSs = subset(aS,LFA==i & SYEAR==j)
                      if(nrow(aSs)>0){
                        print(c(i,j))
                      aSs = aggregate(UID~WOS,data=aSs,FUN=function(x) length(unique(x)))
                      aSs$CE = cumsum(aSs$UID)/sum(aSs$UID)
                      lines(aSs$WOS,aSs$CE,type='l',lwd=2,col='green',lty=3)
                      }
                }
              }

            #by year, by lfa by grid group
            
            
            ii = c(33,34,35)
            jj = 2019:2021
            pdf('Figures/SamplingbyArea.pdf')
            for(i in ii){
              for(j in jj){
                
                bss = subset(b,LFA==i & SYEAR==j)
                gg = c(na.omit(unique(bss$GridGroup)))
                for(g in gg){
                bsst = subset(bss,GridGroup==g)
                bsst = aggregate(NUM_OF_TRAPS~WOS, data=bsst,FUN=sum)
                bsst$CE = cumsum(bsst$NUM_OF_TRAPS)/sum(bsst$NUM_OF_TRAPS)
                plot(bsst$WOS,bsst$CE,type='l',lwd=2,xlab='WOS',ylab='Cumulative Effort',main=paste(i,j,g,sep="-"))
                
                aOs = subset(aO,LFA==i & SYEAR==j & GridGroup==g)
                if(nrow(aOs)>0){
                  print(c(i,j))
                  aOs = aggregate(UID~WOS,data=aOs,FUN=function(x) length(unique(x)))
                  aOs$CE = cumsum(aOs$UID)/sum(aOs$UID)
                  lines(aOs$WOS,aOs$CE,type='l',lwd=2,col='red',lty=2)
                }
                aSs = subset(aS,LFA==i & SYEAR==j& GridGroup==g)
                if(nrow(aSs)>0){
                  print(c(i,j))
                  aSs = aggregate(UID~WOS,data=aSs,FUN=function(x) length(unique(x)))
                  aSs$CE = cumsum(aSs$UID)/sum(aSs$UID)
                  lines(aSs$WOS,aSs$CE,type='l',lwd=2,col='green',lty=3)
                  }
                }
              }
            }
            graphics.off()
            #by y
