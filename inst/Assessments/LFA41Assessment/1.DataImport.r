# data building

require(bio.survey)
require(bio.lobster)
p = bio.lobster::load.environment()
p$libs = NULL
fp = file.path(project.datadirectory('bio.lobster'),"analysis")
la()
p$yrs = 1947:p$current.assessment.year
<<<<<<< HEAD
load_all('~/GitHub/bio.survey/')

=======
#load_all('~/git/bio.survey/')
>>>>>>> 8166747bcdc1efa15ee4ccbc0b303a07181b8751


windows=F
        # run in windows environment
 if(windows){       
        lobster.db( DS = "logs41.redo",  p=p)   # Offshore logs monitoring documents
        lobster.db( DS = "atSea.redo",   p=p)        # at Sea sampling from materialized view
        lobster.db( DS = "cris.redo",    p=p)     # CRIS database
        lobster.db( DS = 'annual.landings.redo', p=p) #static annual landings tabke needs to be updated by CDenton
        lobster.db( DS = 'seasonal.landings.redo', p=p) #static seasonal landings table needs to be updated by CDenton
        #lobster.db( DS = "lfa41.vms.redo")
        #lobster.db( DS="logs41jonah.redo")
        nefsc.db( DS = 'odbc.dump.redo',fn.root = file.path(project.datadirectory('bio.lobster'),'data'),p=p)
        lobster.db(DS = 'lfa41.observer.samples.redo') 
      }


	# load .RData objects

        lobster.db( DS="logs41", p=p)	# Offshore logs monitoring documents
        lobster.db( DS="atSea", p=p)		# at Sea sampling from materialized view
        lobster.db( DS="cris", p=p)		# CRIS database
        lobster.db( DS = 'annual.landings', p=p) #static annual landings tabke needs to be updated by CDenton
        lobster.db( DS = 'seasonal.landings', p=p) #static seasonal landings table needs to be updated by CDenton

     #American Trawl Survey Results

     inf = nefsc.db( DS = 'usinf.clean.redo',fn.root = NULL,p=p)
      ca = nefsc.db( DS = 'uscat.clean.redo',fn.root = NULL,p=p)
      de = nefsc.db( DS = 'usdet.clean.redo',fn.root = NULL,p=p)
         nefsc.db(DS = 'usstrata.area.redo')        


#### Landings
        
<<<<<<< HEAD
       # LFA41 Landings Update
       require(magrittr)
        
        logs41$YEAR<-year(logs41$FV_FISHED_DATETIME)
        ziff41$YEAR<-year(ziff41$DATE_FISHED)
        off41$YEAR<-year(off41$DATE_FISHED)
        
        names(logs41)[names(logs41)=="FV_FISHED_DATETIME"]<-"DATE_FISHED"
        names(slip41)[names(slip41)=="TO_CHAR(LANDING_DATE_TIME,'YYYY')"]<-"SYEAR"
        names(slip41)[names(slip41)=="SUM(SLIP_WEIGHT_LBS)"]<-"ADJCATCH"
        
        logs41$LANDINGS_T<-logs41$ADJCATCH*0.0004536
        ziff41$LANDINGS_T<-ziff41$ADJCATCH*0.0004536
        off41$LANDINGS_T<-off41$ADJ_LOB_LBS*0.0004536
        slip41$LANDINGS_T<-slip41$ADJCATCH*0.0004536
        l.41<-subset(logs41,select=c(DATE_FISHED, YEAR, LANDINGS_T,NUM_OF_TRAPS))
        z.41<-subset(ziff41,select=c(DATE_FISHED, YEAR, LANDINGS_T,NUM_OF_TRAPS))
        o.41<-subset(off41, select=c(DATE_FISHED, YEAR, LANDINGS_T,NUM_OF_TRAPS))
        
        tot.land<-rbind(l.41,z.41,o.41)
        tot.land$SYEAR<-sapply(tot.land$DATE_FISHED,offFishingYear)
        
        ann.41<-tot.land%>%group_by(YEAR)%>%summarise(LAND_T=sum(LANDINGS_T,na.rm=TRUE), TH=sum(NUM_OF_TRAPS, na.rm=TRUE),
                                                      CPUE=sum((LANDINGS_T/TH)*1000,na.rm=TRUE))%>%ungroup()%>%data.frame()
=======
#      # LFA41 Landings Update
#      require(maggittr)
#       
#       logs41$YEAR<-year(logs41$FV_FISHED_DATETIME)
#       ziff41$YEAR<-year(ziff41$DATE_FISHED)
#       off41$YEAR<-year(off41$DATE_FISHED)
#       
#       names(logs41)[names(logs41)=="FV_FISHED_DATETIME"]<-"DATE_FISHED"
#       names(slip41)[names(slip41)=="TO_CHAR(LANDING_DATE_TIME,'YYYY')"]<-"SYEAR"
#       names(slip41)[names(slip41)=="SUM(SLIP_WEIGHT_LBS)"]<-"ADJCATCH"
#       
#       logs41$LANDINGS_T<-logs41$ADJCATCH*0.0004536
#       ziff41$LANDINGS_T<-ziff41$ADJCATCH*0.0004536
#       off41$LANDINGS_T<-off41$ADJ_LOB_LBS*0.0004536
#       slip41$LANDINGS_T<-slip41$ADJCATCH*0.0004536
#       l.41<-subset(logs41,select=c(DATE_FISHED, YEAR, LANDINGS_T,NUM_OF_TRAPS))
#       z.41<-subset(ziff41,select=c(DATE_FISHED, YEAR, LANDINGS_T,NUM_OF_TRAPS))
#       o.41<-subset(off41, select=c(DATE_FISHED, YEAR, LANDINGS_T,NUM_OF_TRAPS))
#       
#       tot.land<-rbind(l.41,z.41,o.41)
#       tot.land$SYEAR<-sapply(tot.land$DATE_FISHED,offFishingYear)
#       
#       ann.41<-tot.land%>%group_by(YEAR)%>%summarise(LAND_T=sum(LANDINGS_T,na.rm=TRUE), TH=sum(NUM_OF_TRAPS, na.rm=TRUE),
#                                                     CPUE=sum((LANDINGS_T/TH)*1000,na.rm=TRUE))%>%ungroup()%>%data.frame()
>>>>>>> 8166747bcdc1efa15ee4ccbc0b303a07181b8751

