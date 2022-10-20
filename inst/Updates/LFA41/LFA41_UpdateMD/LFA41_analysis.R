require(bio.base)
require(bio.lobster)
p = bio.lobster::load.environment()
require(bio.polygons)
p$libs = NULL
require(PBSmapping)
require(bio.lobster)
require(bio.utilities)
require(dplyr)

require(bio.survey)

p = bio.lobster::load.environment()
p$libs = NULL
fp = file.path(project.datadirectory('bio.lobster'),"analysis")
la()
p$yrs = 1947:p$current.assessment.year

load_all('~/GitHub/bio.survey/')




###DATA IMPORT###

windows=T
# run in windows environment
if(windows){       
  lobster.db( DS = "logs41.redo",  p=p)   # Offshore logs monitoring documents
  lobster.db( DS = "atSea.redo",   p=p)        # at Sea sampling from materialized view
  lobster.db( DS = "cris.redo",    p=p)     # CRIS database
  lobster.db( DS = 'annual.landings.redo', p=p) #static annual landings tabke needs to be updated by CDenton
  lobster.db( DS = 'seasonal.landings.redo', p=p) #static seasonal landings table needs to be updated by CDenton

  
  inf = nefsc.db( DS = 'usinf.redo.odbc',fn.root = NULL,p=p)
  ca = nefsc.db( DS = 'uscat.redo.odbc',fn.root = NULL,p=p)
  de = nefsc.db( DS = 'usdet.redo.odbc',fn.root = NULL,p=p)
  
  inf = nefsc.db( DS = 'usinf.clean.redo',fn.root = NULL,p=p)
  de = nefsc.db( DS = 'usdet.clean.redo',fn.root = NULL,p=p)
  ca = nefsc.db( DS = 'uscat.clean.redo',fn.root = NULL,p=p)
  
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



source("C:/Users/Howsevj/Documents/LFA41_UpdateMD/2.stratifiedAnalysis.r") 
source("C:/Users/Howsevj/Documents/LFA41_UpdateMD/2d.stratifiedAnalysisCommercial.r") 
source("C:/Users/Howsevj/Documents/LFA41_UpdateMD/2e.femalestratifiedSizeFrequencies.r") 
source("C:/Users/Howsevj/Documents/LFA41_UpdateMD/3.reproductivePotential.r") 


