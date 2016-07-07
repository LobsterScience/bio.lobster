
#### Import lobster data from various databases
require(bio.lobster)


p = bio.lobster::load.environment()


# 
   

##### lumped function lobster.db
#be sure to update current.assessment.year.r 

p$yrs = 1947:p$current.assessment.year

        # run in windows environment
        if(redo.all) lobster.db(DS="complete.redo")
        
        lobster.db( DS ="logs.redo",    p=p)        # Inshore logs summary documents
        lobster.db( DS = "logs41.redo",  p=p)   # Offshore logs monitoring documents
        lobster.db( DS = "atSea.redo",   p=p)        # at Sea sampling from materialized view
        lobster.db( DS = "cris.redo",    p=p)     # CRIS database
        lobster.db( DS = "port.redo",    p=p)     # Port Sampling
        lobster.db( DS = "vlog.redo",    p=p)     # Voluntary logs
        lobster.db( DS = "fsrs.redo",    p=p)     # FSRS recruitment traps
        lobster.db( DS = "scallop.redo", p=p)  # scallop survey bycatch
        lobster.db( DS = "survey.redo",  p=p)   # ITLS Lobster Survey
        lobster.db( DS = 'annual.landings.redo', p=p) #static annual landings tabke needs to be updated by CDenton
        lobster.db( DS = 'seasonal.landings.redo', p=p) #static seasonal landings table needs to be updated by CDenton
        lobster.db( DS = "lfa41.vms.redo")
        nefsc.db( DS = 'odbc.dump.redo',fn.root = file.path(project.datadirectory('lobster'),'data'),p=p)

    #process log book data

        lobster.db(DS='process.logs.redo', p=p)

	# load .RData objects

        lobster.db( DS="logs",  p=p)		# Inshore logs summary documents
        lobster.db( DS="logs41", p=p)	# Offshore logs monitoring documents
        lobster.db( DS="atSea", p=p)		# at Sea sampling from materialized view
        lobster.db( DS="cris", p=p)		# CRIS database
        lobster.db( DS="port", p=p)		# Port Sampling
        lobster.db( DS="vlog", p=p)		# Voluntary logs
        lobster.db( DS="fsrs", p=p)		# FSRS recruitment traps
        lobster.db( DS="scallop", p=p)	# scallop survey bycatch
        lobster.db( DS="survey", p=p)	# ITLS Lobster Survey
        lobster.db( DS = 'annual.landings', p=p) #static annual landings tabke needs to be updated by CDenton
        lobster.db( DS = 'seasonal.landings', p=p) #static seasonal landings table needs to be updated by CDenton

     #American Trawl Survey Results

        nefsc.db( DS = 'usinf.clean.redo',fn.root = NULL,p=p)


#### Data Processing

#### Landings
        
       # Recent Landings Update
        Landings<-read.csv(file.path( project.datadirectory("lobster"), "data","inputs","AnnualandSeasonalLandingsLFA27-38.LFS2015.csv"))

        lfas=c(27,28,29,30,31.1,31.2,32,33,34)
        Landings$YEAR<-as.numeric(substr(Landings$YEAR,1,4))
        Landat<-merge(subset(Landings,TYPE=="Annual"&YEAR>1998,c("YEAR","LFA27", "LFA28", "LFA29", "LFA30", "LFA31A", "LFA31B", "LFA32")),
        subset(Landings,TYPE=="Seasonal"&YEAR>1998,c("YEAR","LFA33", "LFA34")))
        TotalLandings<-with(Landat,data.frame(LFA=sort(rep(lfas,nrow(Landat))),SYEAR=rep(YEAR,length(lfas)),C=c(LFA27, LFA28, LFA29, LFA30, LFA31A, LFA31B, LFA32, LFA33, LFA34)))

  
        write.csv(TotalLandings,file.path( project.datadirectory("lobster"), "data","products","TotalLandings.csv"))







#### FSRS recruitment traps only

recruitment.trap.db('raw.redo')
