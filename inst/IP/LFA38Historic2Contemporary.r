require(bio.lobster)
require(bio.utilities)
require(devtools)
require(PBSmapping)
la()

LobsterMap('38',labels='grid')


setwd('~/Documents/Adam/Lobster/LFA38/Jan2020')
a  = dir()
a = a[grep('.csv',a)]


b = lobster.db('process.logs.unfiltered')
b = subset(b,LFA==38)

LFA38Grids = c(38:42,48:53,61:66,74:78,86:89,97:99,107,108)
LFA37Grids = 38:42
bb = subset(b,GRID_NUM %in% LFA38Grids)

g = lobster.db('community_code')


gg = merge(bb,g,all.x=T)


bbs = aggregate(cbind(WEIGHT_KG,NUM_OF_TRAPS)~SYEAR,data=bb,FUN=sum)

unique(gg$COMMUNITY_NAME)

aggregate(LICENCE_ID~SYEAR+COMMUNITY_NAME,data=gg,FUN=function(x) length(unique(x)))

#Campbell and Duggan looked at south V north 

south = c("INGALL'S HEAD","SEAL COVE","WHITE HEAD")
north = c('NORTH HEAD')

#Numbers of Traps
F1 = read.csv('CampbellandDugganFigureData.csv')
F1[,1] <- NULL
h = lobster.db('seasonal.landings')
h$Year=1976:2020
h = h[,c('Year','LFA38')]
names(h)[2] = 'TLand'

#Landings TS
plot(F1$Year,F1$TLand,type='h',xlim=c(1875,2020),ylim=c(0,5800),lwd=2,col='grey60',ylab='Landings T')
lines(h$Year+.2,h$TLand,type='h',lwd=2,col='black')
legend('topleft',lty=c(1,1),lwd=c(2,2),col=c('grey60','black'),c('Campbell and Duggan 1980','Recent Data'),bty='n',cex=0.8)
 savePlot('LandingTimeSeries.png')

#Licences TS
hL = data.frame(Year=2018:2020, Licences=c(136,136,136))
plot(F1$Year,F1$Licenses,type='h',xlim=c(1940,2020),ylim=c(0,280),lwd=2,col='grey60',ylab='Licences')
lines(hL$Year+.2,hL$Licences,type='h',lwd=2,col='black')
legend('topright',lty=c(1,1),lwd=c(2,2),col=c('grey60','black'),c('Campbell and Duggan 1980','Recent Data'),bty='n',cex=0.8)
savePlot('LicencesTimeseries.png')

#Traps
tr = 135*375+1*375*.3
hT = data.frame(Year=2018:2020, Traps=rep(tr,3))
plot(F1$Year,F1$Traps/1000,type='h',xlim=c(1880,2020),ylim=c(0,52),lwd=2,col='grey60',ylab='Traps')
lines(hT$Year+.2,hT$Traps/1000,type='h',lwd=2,col='black')
legend('topright',lty=c(1,1),lwd=c(2,2),col=c('grey60','black'),c('Campbell and Duggan 1980','Recent Data'),bty='n',cex=0.8)
savePlot('TrapTimeseries.png')


#CPUE over Seasons
cu =read.csv("CumlativeCatchandCPUE1949-1953SouthernGM.csv")
cu$Day = ifelse(cu$Week=="I",1,15)
cu$Y2 = ifelse(cu$Month %in% c('Nov','Dec'), cu$Season-1, cu$Season)
cu$Date = as.Date(with(cu, paste(Y2, Month, Day, sep="-")), '%Y-%B-%d')
cu$ID = paste(cu$Month,cu$Week,sep='-')


xCU = aggregate(CPUE~ID,data=cu,FUN=mean)
xCU = xCU[c(8,1,2,3,6,7,4,5),]
with(subset(cu,Season==1949),plot(Date, CPUE,ylab='CPUE kg/TH',ylim=c(0,1.5)))
m=0
n=0
for(i in 1950:1953){
	n=n+1
	m=m+365
with(subset(cu,Season==i),points(Date-m, CPUE,ylab='CPUE kg/TH',pch=n,col=n))
}
points(subset(cu,Season==1949,select=Date)$Date, xCU$CPUE,pch=16,col='black',cex=2)
savePlot('CPUE49-53.png')

cu80 = read.csv("CatchandEffortSouthernGM7980.csv")
cu79 = read.csv("CatchandEffortSouthernGM7879.csv") 

cu80$Date = as.Date(cu80$period)
cu79$Date = as.Date(cu79$period)

with(cu79,plot(Date-10950,CPUE.kgTH.,type='l',col='black',lwd=2,ylim=c(0,1.9)))
with(cu80,lines(Date-11315,CPUE.kgTH.,type='l',col='black',lwd=2,lty=2))

savePlot('CPUE79-80.png')

gS = subset(gg,COMMUNITY_NAME %in% south)
gSS = aggregate(cbind(WEIGHT_KG,NUM_OF_TRAPS)~WOS+SYEAR,data=gg,FUN=sum)
gSD = aggregate(DATE_FISHED~WOS+SYEAR,data=gg,FUN=mean)

gR = merge(gSS,gSD)

gR$CPUE=gR$WEIGHT_KG/gR$NUM_OF_TRAPS

ii = unique(gSS$SYEAR)
m=0
coll = rep(c('black','red','blue'),each=5)
coll = c(coll,'blue')
with(subset(gR,SYEAR==2005),plot(DATE_FISHED,SYEAR,type='n',ylim=c(0,6),xlab='Date',ylab='CPUE kg/TH'))
for(i in 1:length(ii)){
	
	bb = subset(gR,SYEAR==ii[i])
	bb = bb[order(bb$WOS),]
	with(bb, lines(DATE_FISHED-m,CPUE,type='l',col=coll[i]))
	m=m+365
}

savePlot('CPUE05-20.png')

gR$yrcode = ifelse(gR$SYEAR <2010,2007,ifelse(gR$SYEAR %in% c (2010,2011,2012,2013,2014),2012,2017))

gRA = aggregate(cbind(WEIGHT_KG,NUM_OF_TRAPS)~WOS+yrcode,data=gR,FUN=sum)
gRA$CPUE = gRA$WEIGHT_KG/ gRA$NUM_OF_TRAPS


with(subset(gRA,yrcode==2007),plot(WOS,CPUE,type='l',,ylim=c(0,6),xlab='Week of Season',ylab='CPUE kg/TH',col='black'))
with(subset(gRA,yrcode==2012),lines(WOS,CPUE,type='l',,ylim=c(0,6),xlab='Week of Season',ylab='CPUE kg/TH',col='blue'))
with(subset(gRA,yrcode==2017),lines(WOS,CPUE,type='l',,ylim=c(0,6),xlab='Week of Season',ylab='CPUE kg/TH',col='red'))

savePlot('CPUEmeanbyblock.png')


#Combined CPUE









