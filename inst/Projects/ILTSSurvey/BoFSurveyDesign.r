
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
GFS$EID = 1:nrow(GFS)

 LFAs<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","LFAPolys.csv"))
L = subset(LFAs, PID %in% c(35,36))

G = findPolys(na.omit(GFS[,c('X','Y','EID')]), L)
GF = subset(GFS, EID %in% G$EID)


#2019 stations
#FundyStns = na.omit(read.csv(file.path(project.datadirectory('bio.lobster'),"data","survey","FundyStations.csv")))
FundyStns = (read.csv(file.path(project.datadirectory('bio.lobster'),"data","survey","ILTS2019Leg2stations.csv")))
LobsterMap('35-36')
with(FundyStns,points(X,Y,col='red',pch=16))

with(GF,points(X,Y,bg='green',pch=21))

with(FundyStns,points(X,Y,bg='red',pch=21))

LobsterMap('35-36')
with(GF,points(X,Y,bg='green',pch=21))
with(FundyStns,points(X,Y,bg='red',pch=21))

g=makeGrid( x=seq(-67.2,-63.2,0.01),y=seq(44.5,46,0.01))
#with(g,identify(X,Y))

ww = g[c(26123,  58007,  59899,  64623,  66079,  69059,  70615,  72699,  81923,  85043,  85551,  86923,  94759,  98423, 101255, 106131,
           106203, 112907, 114283, 125523, 125851, 127603, 135259, 136771, 144827, 159283),c('X','Y')]
ww$YEAR='NEW2020'
ww$STATION=NA
ww$COMMENT = 'GF and Mix'
FundyStns = rbind(FundyStns,ww)

LobsterMap('35-36')
with(FundyStns,points(X,Y,bg='red',pch=21))
with(GF,points(X,Y,bg='green',pch=21))
with(FundyStns,points(X,Y,bg='red',pch=21))

with(g,identify(X,Y))


w=g[c(  46959,  56579,  67591,  72443,  78975,  80583 , 85171, 101655 ,103067, 111335, 116047 ,128755),c('X','Y')]
w$YEAR='NEW2020'
w$STATION=NA
w$COMMENT = 'GF and Mix'


FundyStns = rbind(FundyStns,w)
write.csv(FundyStns, file.path(project.datadirectory('bio.lobster'),"data","survey","FundyStations2020.csv"))




