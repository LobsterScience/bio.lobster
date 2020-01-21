# data building

require(bio.survey)
require(bio.lobster)
p = bio.lobster::load.environment()
p$libs = NULL
fp = file.path(project.datadirectory('bio.lobster'),"analysis")
la()
p$yrs = 1947:p$current.assessment.year
load_all('~/git/bio.survey/')



        # run in windows environment

        lobster.db( DS = 'annual.landings.redo', p=p) #static annual landings tabke needs to be updated by CDenton
        lobster.db( DS = 'seasonal.landings.redo', p=p) #static seasonal landings table needs to be updated by CDenton
       

	# load .RData objects

        lobster.db( DS = 'annual.landings', p=p) #static annual landings take needs to be updated by CDenton
        lobster.db( DS = 'seasonal.landings', p=p) #static seasonal landings table needs to be updated by CDenton
        
        
annual.landings=lobster.db( DS = 'annual.landings', p=p)


#### Landings
        
       # LFA41 Landings Update
       require(maggittr)
        
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


        
        
        ##Landings barplot
        
        figfp = file.path(project.figuredirectory('bio.lobster'),'Assessment','LFA41')  
      
        ll <- annual.landings
        par(mar=c(3,5,3,3))
        cols<-c("steelblue4","steelblue4", "steelblue4", "steelblue4", "steelblue4", "steelblue4", "steelblue4", "steelblue4", "steelblue4",
"steelblue4", "steelblue4", "steelblue4", "steelblue4", "steelblue4", "steelblue4", "steelblue4", "firebrick3")
        bardens<-c(500,500,500,500,500,500,500,500,500,500,500,500,500,500,500,500,30)
        
        barplot(ll$LFA41[ll$YR > 2001], ylim =c(0,1000), col = cols, density = bardens, names.arg = 2002:2018, cex.names= 0.8, space =c(0,0), ylab = "Landings (t)" )
        box()
        abline(h=720, col ='red', lwd = 2)
        savePlot(file.path(figfp,'2018Landings.png'),type='png')
       
        