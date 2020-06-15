#landings by grid

require(bio.lobster)
require(bio.survey)
require(bio.lobster)
p = bio.lobster::load.environment()
p$libs = NULL
require(bio.utilities)
la()

a = lobster.db('process.logs') 


#grid areas by LFA
		LFAs<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","LFAPolys.csv"))
		LFAgrid<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","GridPolys.csv"))
		attr(LFAgrid,'projection') <- "LL"
		ar = calcArea(LFAgrid)
		names(ar)[1:2] <- c('LFA','GRID_NUM')
		ar = subset(ar,LFA==34)
		cC = calcCentroid(subset(LFAgrid,PID==34))
		aC = merge(ar,cC,by.x='GRID_NUM',by.y='SID')
		
#Weekly
aA = aggregate(cbind(WEIGHT_KG,NUM_OF_TRAPS)~GRID_NUM+WOS+LFA+SYEAR,data=subset(a,LFA==34),FUN=sum)
aA = merge(aA, aC, by=c('LFA','GRID_NUM'),all.x=T)

aA$CPUE = aA$WEIGHT_KG / aA$NUM_OF_TRAPS
aA$wTCPUE = aA$CPUE/aA$area


aA = aA[order(aA$SYEAR, aA$WOS),]
aA = subset(aA,SYEAR>2004)
aA$id = paste(aA$SYEAR,aA$WOS,sep='-')
out = list()
ui = unique(aA$id)
m=0
for(j in ui){
		m = m+1
		k = subset(aA,id==j)
		x = sum(k$X*k$wTCPUE / sum(k$wTCPUE)		)
		y = sum(k$Y*k$wTCPUE / sum(k$wTCPUE)		)
		out[[m]] =c(j,x,y)

}

g = as.data.frame(do.call(rbind,out))

g = cbind(g, do.call(rbind,(strsplit(g[,1],"-"))))
names(g) = c('ID','X','Y','SYEAR','WOS')
g = toNums(g,c('X','Y','SYEAR','WOS'))
g$PID=g$SYEAR
g$POS = g$WOS
pdf('~/tmp/cg.pdf')
for(i in 2005:2019){
LobsterMap(34)
addLines(subset(g,SYEAR==i),col='red')
}
dev.off()


#changes in the start of the season
g$PID=1
g$POS = g$SYEAR
LobsterMap(34)
addLines(subset(g,WOS==1),col='red')



#annual
aA = aggregate(cbind(WEIGHT_KG,NUM_OF_TRAPS)~GRID_NUM+WOS+LFA+SYEAR,data=subset(a,LFA==34),FUN=sum)
aA = merge(aA, aC, by=c('LFA','GRID_NUM'),all.x=T)

aA$CPUE = aA$WEIGHT_KG / aA$NUM_OF_TRAPS
aA$wTCPUE = aA$CPUE/aA$area


aA = aA[order(aA$SYEAR, aA$WOS),]
aA = subset(aA,SYEAR>2004)
aA$id = aA$SYEAR
out = list()
ui = unique(aA$id)
m=0
for(j in ui){
		m = m+1
		k = subset(aA,id==j)
		x = sum(k$X*k$wTCPUE / sum(k$wTCPUE)		)
		y = sum(k$Y*k$wTCPUE / sum(k$wTCPUE)		)
		out[[m]] =c(j,x,y)

}

g = as.data.frame(do.call(rbind,out))

names(g) = c('SYEAR','X','Y')
g = toNums(g,c('X','Y','SYEAR'))
g$PID=1
g$POS = g$SYEAR
LobsterMap(34)
addLines(g,col='red')

#st marys bay 69,81, 92
a = lobster.db('process.logs') 
aA = aggregate(cbind(WEIGHT_KG,NUM_OF_TRAPS)~LFA+SYEAR,data=subset(a,GRID_NUM %in% c(92) & SYEAR >2004 & WOS %in% 1:3),FUN=sum)
with(aA,plot(SYEAR,WEIGHT_KG/NUM_OF_TRAPS,type='l'))
