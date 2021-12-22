#Lobster Population Model SUNY-UMM

require(bio.lobster)
require(bio.utilities)
require(devtools)
require(lubridate)
load_all('~/git/bio.survey/')

la()

wd = file.path(project.datadirectory('bio.lobster'),'PopModelInputs')
dir.create(wd,showWarnings = F)

p = bio.lobster::load.environment()
  
p=list()
  p$yrs = 1947:2021

redo.base=F
if(redo.base){
      
      lobster.db("season.dates.redo")
      
      lobster.db( DS = "logs.redo",  p=p)   # Offshore logs monitoring documents
      logsInSeason = lobster.db("process.logs.redo")
      lobster.db( DS = 'annual.landings.redo', p=p) #static annual landings tabke needs to be updated by CDenton
      lobster.db( DS = 'seasonal.landings.redo', p=p) #static seasonal landings table needs to be updated by CDenton
      nefsc.db( DS = 'odbc.dump.redo',fn.root = file.path(project.datadirectory('bio.lobster'),'data'),p=p)
      
      
      datayrs=1970:2021
      groundfish.db( DS="gscat.odbc.redo", datayrs=datayrs )
      groundfish.db( DS="gsdet.odbc.redo", datayrs=datayrs )
      groundfish.db( DS="gsinf.odbc.redo", datayrs=datayrs )
      
      
      lobster.db( DS = 'annual.landings', p=p) #static annual landings tabke needs to be updated by CDenton
      lobster.db( DS = 'seasonal.landings', p=p) #static seasonal landings table needs to be updated by CDenton
      inf = nefsc.db( DS = 'usinf.clean.redo',fn.root = NULL,p=p)
      ca = nefsc.db( DS = 'uscat.clean.redo',fn.root = NULL,p=p)
      de = nefsc.db( DS = 'usdet.clean.redo',fn.root = NULL,p=p)
      nefsc.db(DS = 'usstrata.area.redo')        
}




