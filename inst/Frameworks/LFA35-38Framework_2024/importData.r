# data building

require(bio.survey)
require(bio.lobster)
require(bio.groundfish)
require(bio.utilities)
p=list()
p$libs = NULL
fp = file.path(project.datadirectory('bio.lobster'),"analysis")
p$yrs = 1947:2024




        # run in windows environment
        #Data dumps
      
#Lobster Fishery Data
        lobster.db("season.dates.redo")
        lobster.db( DS = "logs.redo",  p=p)   # Offshore logs monitoring documents
        logsInSeason = lobster.db("process.logs.redo")
        lobster.db( DS = 'annual.landings.redo', p=p) #static annual landings tabke needs to be updated by CDenton
        lobster.db( DS = 'seasonal.landings.redo', p=p) #static seasonal landings table needs to be updated by CDenton
        lobster.db( DS = 'annual.landings', p=p) #static annual landings tabke needs to be updated by CDenton
        lobster.db( DS = 'seasonal.landings', p=p) #static seasonal landings table needs to be updated by CDenton
        
        
#RV Survey Data
        groundfish.db( DS="gscat.odbc.redo", datayrs=1970:2024)
        groundfish.db( DS="gsdet.odbc.redo", datayrs=1970:2024)
        groundfish.db( DS="gsinf.odbc.redo", datayrs=1970:2024)
        
      groundfish.db('gs_trawl_conversions_redo')
  

	    