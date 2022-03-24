require(bio.lobster)
require(bio.utilities)
require(RODBC)
require(lubridate)
require(devtools)
options(stringAsFactors=F)
la()


wd = ('C:\\Users\\Cooka\\OneDrive - DFO-MPO\\BycatchLobster')
setwd(wd)

#bycatch.db(DS='odbc.redo',wd=wd)

#LOADING AND MERGING LOGBOOKS with TARGETS
    #bycatch.db(DS='logbook.merge.redo',wd=wd)

        b = bycatch.db('logbook.merge',wd=wd) 
        gt = t = bycatch.db('targets',wd=wd) 
        t = subset(t,LFA%in%33:35)
        tt = aggregate(cbind(Period1,Period2,Period3)~LFA+GridGrouping,data=t,FUN=min)
        
        bA = aggregate(SD_LOG_ID~Period+GridGroup+LFA+SYEAR,data=b,FUN=function(x) length(unique(x)))
        tR = as.data.frame(reshape(tt,varying=3:5,direction = 'long',sep=""))
        names(tR) = c('LFA','GridGroup','Period','Target','Nt')
        tR$Nt = NULL
        
        bA = merge(bA,tR,all.x=T)
        
        write.csv(bA,file=file.path(wd,'results','LogbooksAgg and Targets.csv'))

     

        
        
        
        
        
                
        ##trips by LFA and year
        
g = lobster.db('process.logs.unfiltered')

g$ID = paste(g$LICENCE_ID,g$DATE_FISHED,sep="-")
gg = aggregate(ID~LFA+SYEAR+LICENCE_ID,data=g,FUN=function(x) length(unique(x)))
ggg = aggregate(ID~LFA+SYEAR,data=gg,FUN=mean)    
write.csv(ggg,'data/MeanUniqueTripsPerLicenceLFAandSY.csv')
