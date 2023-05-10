
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
L = subset(LFAs, PID %in% c(38))

G = findPolys(na.omit(GFS[,c('X','Y','EID')]), L)
GF = subset(GFS, EID %in% G$EID)


FundyStns = (read.csv(file.path(project.datadirectory('bio.lobster'),"data","maps","Stations around LFA 38.csv")))
gz = (read.csv(file.path(project.datadirectory('bio.lobster'),"data","maps","Grey Zone From ConditionsNAD83.csv")))

lobster.db('survey')
ilts = surveyCatch

LobsterMap('38')
addPolys(gz,col='yellow')
with(GF,points(X,Y,bg='green',pch=21))
with(FundyStns,points(X,Y,bg='red',pch=21))

with(subset(ilts,YEAR>2020),points(HAUL_LONG,HAUL_LAT,bg='blue',pch=21))

g=makeGrid( x=seq(-67.4,-66.4,0.01),y=seq(43.8,45,0.01))
#with(g,identify(X,Y))

ww = g[c(19811,32167,35135,37003,41155),c('X','Y')]
ww$YEAR='NEW2023'
ww$STATION=NA
ww$COMMENT = 'GF and Mix'

ww = g[c(19811,32167,35135,37003,41155),c('X','Y')]
ww$YEAR='NEW2023'
ww$STATION=NA
ww$EID = 1:nrow(ww)
ww$COMMENT = 'New'

points(ww$X,ww$Y,pch=21,col='orange')

with(g,identify(X,Y))


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




