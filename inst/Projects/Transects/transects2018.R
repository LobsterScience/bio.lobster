require(bio.lobster)
p = bio.lobster::load.environment()
require(bio.polygons)
p$libs = NULL
require(PBSmapping)
require(SpatialHub)
require(bio.lobster)
require(bio.utilities)
require(lubridate)
require(ggplot2)
require(tidyverse)
require(dplyr)
require(gbm)
la()


dive18<-read.csv(file.path( project.datadirectory('bio.lobster'),"data", "divesurvey","transectdata2018.csv"))
figfp = file.path(project.figuredirectory('bio.lobster'),'data','divesurvey')


#Map Transects

LobsterMap(ylim=c(43.5,44),xlim=c(-66.5,-65.3),boundaries='LFAs',title="Dive Transects", mapRes="UR")
with(dive18,segments(x0=-start_long,y0=start_lat, x1=-end_long, y1=end_lat))

#Use  convert.dd.dddd()  to change lat long 


dive18$denstiy<-NA


#Count the lobster per transect - number of rows within unique transect_no
#Divide the 




