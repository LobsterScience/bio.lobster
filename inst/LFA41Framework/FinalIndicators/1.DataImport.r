# data building

require(bio.survey)
require(bio.lobster)
p = bio.lobster::load.environment()
p$libs = NULL
fp = file.path(project.datadirectory('bio.lobster'),"analysis")
la()
load_all('~/git/bio.survey/')

			       nefsc.db(DS = 'uscat.clean.redo')        
                  nefsc.db(DS = 'usinf.clean.redo')        
                  nefsc.db(DS = 'usdet.clean.redo')        
                  nefsc.db(DS = 'usstrata.area.redo')        
