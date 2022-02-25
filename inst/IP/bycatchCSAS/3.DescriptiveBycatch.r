require(bio.lobster)
require(bio.utilities)
require(RODBC)
require(lubridate)
require(devtools)
options(stringAsFactors=F)
la()


wd = ('C:/Users/CookA/Desktop/dellshared/Bycatch in the Lobster Fishery')
setwd(wd)



#logbooks

    bA = read.csv(file=file.path(wd,'results','LogbooksAgg and Targets.csv'))

#SWLSS sea sampling
    CDa = bycatch.db(DS='SWLSS')
        
        
#Empties by month
CDa$mn = month(CDa$BOARD_DATE)
CDa$SYEAR = year(CDa$BOARD_DATE)
CDa$SYEAR = ifelse(CDa$mn>=10,CDa$SYEAR+1,CDa$SYEAR)
Emp = aggregate(cbind(Empty,P)~COMAREA_ID+mn,data=CDa,FUN=sum)
Emp$PropEmt = Emp$Empty / Emp$P

#Traps Sampled By Area Need to merge in grid2targets

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


#Total number of traps per Year, Period and GridGrouping
        STraps = aggregate(P~SYEAR+GridGroup+COMAREA_ID+Period+target,data=CDa,FUN=sum)
        STraps$LFA = as.numeric(unlist(lapply(strsplit(STraps$COMAREA_ID,'L'),"[[",2)))
        
        ATrips = aggregate(TRIP~SYEAR+COMAREA_ID+Period+target+GridGroup,data=CDa,FUN=function(x) length(unique(x)))
        
        ATrips$LFA = as.numeric(unlist(lapply(strsplit(ATrips$COMAREA_ID,'L'),"[[",2)))
        
        TTraps = aggregate(NUM_OF_TRAPS~SYEAR+LFA+GridGroup+Period+target,data=b,FUN=sum)
        
        STTraps = merge(TTraps,STraps, all.x=T)
        
        STTraps =merge(STTraps,ATrips,all=T)
        
        STTraps$Prp = STTraps$P / STTraps$NUM_OF_TRAPS *100
  write.csv(STTraps,file=file.path('results','SWLSSTrips2Targets.csv'))
  

       STTraps = merge(STTraps,bA,all=T)
        names(STTraps) = c('Year','LFA','GridGroup','Period','Target','CI','NUM_OF_TRAPS_FISHED','NUM_OF_TRAPS_SAMPLED_SWLSS','NUM_OF_TRIPS_SAMPLED_SWLSS','P','NUM_SDLOGS','tt')
        STTraps$CI = STTraps$tt =NULL
        
        
        write.csv(STTraps,file=file.path('results','SWLSSTrips2TargetsTrapsandTrips.csv'))
  
        STTraps  = read.csv(file=file.path('results','SWLSSTrips2TargetsTrapsandTrips.csv'))
        
  
  boxplot(Prp~SYEAR+LFA,data=STTraps,las=2,xlab='',ylab='Total Traps Hauled (%)')


hist(CDa$Lobster,breaks=0:50,main="",xlab='Number of Lobster')
savePlot('Figures/DistributionOfLobsterCatchesSampledTraps.png')



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
names(ggg)[c(1,5:7)] = c('Year','Target','ObserverTrips','ObserverTraps')

STTraps = merge(STTraps,ggg,all=T)

write.csv(STTraps,file=file.path('results','SWLSSandObsTrips2TargetsTrapsandTrips.csv'))



####pertrap info using

ao = bycatch.db('ISDB.reshape.redo')


aCo = aggregate(cbind(Lobster,Cod,Cusk,Jonah,Legal,Berried, Empty,LegalWt)~TRIPNO+LFA+NUM_HOOK_HAUL,data=ao,FUN=median)



aCo$TotLegal = aCo$LegalWt * aCo$NUM_HOOK_HAUL

aCoC = aggregate(cbind(TotLegal,NUM_HOOK_HAUL)~TRIPNO,data=aCo,FUN=sum)


ms = read.csv('data/ISDB_Trip_id to sd_log_idFinal.csv')
ms = subset(ms,!is.na(SD_LOG_ID_1),select=c(trip_id,SD_LOG_ID_1))

bLo = merge(b,ms,by.x='SD_LOG_ID', by.y='SD_LOG_ID_1')
bLoL = aggregate(cbind(WEIGHT_KG,NUM_OF_TRAPS)~SD_LOG_ID+trip_id,data=bLo,FUN=sum)
SBUMP0 = merge(aCoC,bLoL,by.x='TRIPNO',by.y='trip_id')



###Comparison of precited v reported landings between SWLSS and Observer
with(SBUMP0,plot(WEIGHT_KG,TotLegal))
SBUMP0$TotLegalNTraps = SBUMP0$TotLegal/SBUMP0$NUM_HOOK_HAUL * SBUMP0$NUM_OF_TRAPS
require(MASS)
par(mfrow=c(1,2))
with(SBUMP,plot(WEIGHT_KG,TotLegal,ylab='Extrapolated landings from Sea Samples',main='SWLSS'))
abline(a=0,b=1)
LMSW = with(SBUMP,lm(log(TotLegal+.01)~log(WEIGHT_KG+.01)-1)) #treating the SWLSS data as 'truth'
RLMSW = with(SBUMP,rlm(log(TotLegal+.01)~log(WEIGHT_KG+.01)-1)) #treating the SWLSS data as 'truth'
with(SBUMP0,plot(WEIGHT_KG,TotLegalNTraps,ylab='Extrapolated landings from Sea Samples',main='Observer'))
abline(a=0,b=1)
LMO = (with(SBUMP0,lm(log(TotLegalNTraps+.01)~log(WEIGHT_KG+.01)-1)))
RLMO = (with(SBUMP0,rlm(log(TotLegalNTraps+.01)~log(WEIGHT_KG+.01)-1)))

#RMSE
summary(LMO)$sigma
summary(RLMO)$sigma

summary(LMSW)$sigma
summary(RLMSW)$sigma

biasSWLSS = sum(SBUMP$WEIGHT_KG - SBUMP$TotLegal)/nrow(SBUMP) 
biasOBs = sum(SBUMP0$WEIGHT_KG - SBUMP0$TotLegalNTraps )/nrow(SBUMP0) 



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
