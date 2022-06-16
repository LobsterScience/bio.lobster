#Run me first

require(sdmTMB)
require(bio.lobster)
require(bio.utilities)
require(lubridate)
require(devtools)
require(dplyr)
require(ggplot2)
require(INLA)
options(stringAsFactors=F)
require(PBSmapping)
require(SpatialHub)
require(sf)
la()

p = bio.lobster::load.environment()
p = spatial_parameters(type='canada.east')
wd = ('C:/Users/CookA/Desktop/dellshared/Bycatch in the Lobster Fishery')
setwd(wd)

#note ... some model runs will result in DLL conflicts which need to be resolved prior to next model run or prediction
#you can fix this by running <stripDLL('sdmTMB')> # this will remove all DLLs using TMB except sdmTMB