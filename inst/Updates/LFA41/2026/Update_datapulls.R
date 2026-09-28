
require(RODBC)
require(devtools)
require(roxygen2)
require(geosphere)
require(lubridate)
require(bio.utilities)
require(bio.lobster)
require(rstanarm)
require(rlang)
require(glue)
require(PBSmapping)
require(bio.survey)
require(dplyr)
require(ggplot2)
require(sf)
require(tidyr)


p=list()
p$assessment.year = 2026 ##Check Year
p$syr = 1989

p$lfa=41
p$yrs = 1947:p$assessment.year

p$lfas = c("41") # specify lfa


logs=lobster.db('process.logs.redo')
logs=lobster.db("process.logs")

land=lobster.db('annual.landings.redo')
land = lobster.db('annual.landings')

lobster.db("observer41.redo")
lobster.db("observer41")


groundfish.db('odbc.redo',datayrs = 1970:2026)
groundfish.db('gscat.odbc.redo')
groundfish.db('gsinf.odbc.redo')
groundfish.db('gsdet.odbc.redo')
groundfish.db('gs_trawl_conversions_redo')


#check what's in there
a = groundfish.db('gsinf.odbc.redo')
summary(a)

## FOR NEFSC Survey 

nefsc.db( DS = 'usinf.redo.odbc',fn.root = NULL,p=p)
nefsc.db( DS = 'uscat.redo.odbc',fn.root = NULL,p=p)
nefsc.db( DS = 'usdet.redo.odbc',fn.root = NULL,p=p)

nefsc.db( DS = 'usinf.clean.redo',fn.root = NULL,p=p)
nefsc.db( DS = 'usdet.clean.redo',fn.root = NULL,p=p)
nefsc.db( DS = 'uscat.clean.redo',fn.root = NULL,p=p)

inf = nefsc.db( DS = 'usinf',fn.root = NULL,p=p)
ca = nefsc.db( DS = 'uscat',fn.root = NULL,p=p)
de = nefsc.db( DS = 'usdet',fn.root = NULL,p=p)

inf = nefsc.db( DS = 'usinf.clean',fn.root = NULL,p=p)
de = nefsc.db( DS = 'usdet.clean',fn.root = NULL,p=p)
ca = nefsc.db( DS = 'uscat.clean',fn.root = NULL,p=p)


nefsc.db(DS = 'usstrata.area.redo.odbc')
nefsc.db(DS = 'usstrata.area')





