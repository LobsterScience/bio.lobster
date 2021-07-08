
require(bio.lobster)
require(bio.utilities)
require(PBSmapping)

####################################################################
######### BAY OF FUNDY EXPANSION ###################################
require(bio.lobster)
require(bio.utilities)
require(PBSmapping)
require(devtools)
la()

GFS = groundfish.db('gsinf.odbc')
GFS$X = convert.dd.dddd(GFS$slong*-1)
 GFS$Y = convert.dd.dddd(GFS$slat)
 GFS$X1 = convert.dd.dddd(GFS$elong*-1)
 GFS$Y1 = convert.dd.dddd(GFS$elat)
 
 GFS$EID = 1:nrow(GFS)

 LFAs<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","LFAPolys.csv"))
L = subset(LFAs, PID %in% c(38))

G = findPolys(na.omit(GFS[,c('X','Y','EID')]), L)
GF = subset(GFS, EID %in% G$EID)


LobsterMap('38')
with(GF,points(X,Y,bg='green',pch=21))
with(GF,identify(X,Y,EID))
g=makeGrid( x=seq(-67.4,-66.2,0.01),y=seq(43.8,45,0.01))
with(g,identify(X,Y))
ww=g[c(16951,18807,27003, 3859 ,11607 ,13487 ,23579, 27551, 28555, 32979, 36855, 37283, 37555, 40011, 42543, 43127, 45983, 46851, 47955, 48219),]
ww$YEAR='NEW2020'
ww$STATION=NA
ww$COMMENT = 'New'

LobsterMap('38')
with(ww,points(X,Y,bg='red',pch=21))
write.csv(ww, file.path(project.datadirectory('bio.lobster'),"data","survey","LFA38ProposedStations.csv"))




