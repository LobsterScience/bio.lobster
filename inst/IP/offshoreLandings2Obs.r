require(bio.lobster)
require(lubridate)
require(bio.utilities)
     
         lobster.db( DS="observer41")
         lobster.db( DS="logs41")



observer41$Mon = month(observer41$BOARD)
observer41$Yr = year(observer41$BOARD)

logs41$Mon = month(logs41$FV_FISHED)
logs41$Yr = year(logs41$FV_FISHED)


l41 = subset(logs41,Yr>2010 & Yr< 2018)
o41 = subset(observer41,Yr>2010 & Yr<2018)

l41 = makePBS(l41,polygon=F)

o41 = makePBS(o41,polygon=F)
o41$X = o41$X*-1

LobsterMap(41)
addPoints(na.omit(o41[,c('X','Y','EID')]))



outs = list()
outs = list()
yrs = unique(l41$Yr)

for(i in yrs){
	
		g = subset(l41,Yr==i)
		g = g[order(g$FV_FISHED_DATETIME),]
		g$LKg = cumsum(g$ADJCATCH/2.2)
		g$LKgGBAS = cumsum(g$ADJCATCH/2.2*ifelse(g$OFFAREA=='GBASIN',1,0))
		g$LKgGBAN = cumsum(g$ADJCATCH/2.2*ifelse(g$OFFAREA=='GBANK',1,0))

		g$LKgSEB = cumsum(g$ADJCATCH/2.2*ifelse(g$OFFAREA=='SEBROWNS',1,0))
		g$LKgSWB = cumsum(g$ADJCATCH/2.2*ifelse(g$OFFAREA=='SWBROWNS',1,0))

		g$LKgGBAS = g$LKg

}
 GBASIN   UNKNOWN  SWBROWNS SEBROWNS GBANK