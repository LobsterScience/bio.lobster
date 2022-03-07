require(bio.lobster)
require(bio.utilities)
require(RODBC)
require(lubridate)
require(devtools)
options(stringAsFactors=F)
la()


wd = ('C:/Users/CookA/Desktop/dellshared/Bycatch in the Lobster Fishery')
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
        
#polygons of grid groupings
        
        gG<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","LFA_33_TO_ 38_GRID_GROUPS_NAD83.csv"))
        addLabels(gG,placement='CENTROID')
        
        #inshore midshore offshore
        
        ggs = list(i=c(1,'2A','2B'),m=c(3,'4A','4B'),o=c('5,6'))
        

        
        