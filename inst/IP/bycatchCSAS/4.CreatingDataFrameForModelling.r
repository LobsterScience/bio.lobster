require(bio.lobster)
require(bio.utilities)
require(RODBC)
require(lubridate)
require(devtools)
options(stringAsFactors=F)
la()

wd = ('C:\\Users\\Cooka\\OneDrive - DFO-MPO\\BycatchLobster')

setwd(wd)


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


#jitter Trap points -- unnecessary but keeping

                aT$II = paste(aT$X,aT$Y,sep="-")
                j = split(aT,f=list(aT$II))
            for(i in 1:length(j)){
                  if(nrow(j[[i]])==1) next
                      for(k in 2:nrow(j[[i]])){
                          j[[i]][k,'X'] = j[[i]][k,'X']+runif(1,-0.0001,0.0001)
                          j[[i]][k,'Y'] = j[[i]][k,'Y']+runif(1,-0.0001,0.0001)
                          } 
                        }
                      aTj = as_tibble(do.call(rbind,j))
                      aTl = st_as_sf(aTj,coords = c("X", "Y"),crs= 4326)
                  saveRDS(aTj,file=file.path('results','CompliedDataForModellingjit.rds'))
                  
            
