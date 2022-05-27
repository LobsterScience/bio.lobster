#describing grid groupings depth distribution
require(bio.lobster)
require(bio.utilities)
require(RODBC)
require(lubridate)
require(devtools)
require(PBSmapping)
options(stringAsFactors=F)
la()


wd = ('C:\\Users\\Cooka\\OneDrive - DFO-MPO\\BycatchLobster')
setwd(wd)
p = bio.lobster::load.environment()
#polygons of grid groupings
LobsterMap('33-35')
gG<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","LFA_33_TO_ 38_GRID_GROUPS_NAD83.csv"))
addPolys(gG)
gGP = lonlat2planar(gG,"utm20", input_names=c("X", "Y"))

gGP = rename.df(gGP,c('X','Y','plon','plat'),c('xx','yy','X','Y'))

load(file.path(project.datadirectory('bio.lobster'),'data','predspace.rdata')) #sq km
Ps = data.frame(EID=1:nrow(predSpace),predSpace[,c('plon','plat','z')])
Ps = rename.df(Ps,c('plon','plat','z'),c('X','Y','SET_DEPTH'))
key=findPolys(Ps,gGP)
Ps = merge(Ps,key)

aggregate(SET_DEPTH~PID+SID,data=Ps,FUN=mean)

#inshore midshore offshore

