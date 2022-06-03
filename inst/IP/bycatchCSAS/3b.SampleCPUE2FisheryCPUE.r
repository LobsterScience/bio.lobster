
require(bio.lobster)
require(bio.utilities)
require(RODBC)
require(lubridate)
require(devtools)
options(stringAsFactors=F)
la()

wd = ('C:\\Users\\Cooka\\OneDrive - DFO-MPO\\BycatchLobster')

setwd(wd)


x = read.csv(file='results/TRIPCPUE33-35.csv')

b = bycatch.db('logbook.merge')
b = subset(b,LFA %in% 33:35)


x$CP = round(x$mean/5,3)*5
b$CP = round(b$CPUE/5,3)*5
b = subset(b,CPUE<15)
#LFA 33
b1 = aggregate(SD_LOG_ID~CP,data=subset(b,LFA==33),FUN=function(x) length(unique(x)))
b1$p = cumsum(b1$SD_LOG_ID)/sum(b1$SD_LOG_ID)

x1 = aggregate(TRIP~CP,data=subset(x,LFA==33),FUN=function(x) length(unique(x)))
x1$p = cumsum(x1$TRIP)/sum(x1$TRIP)

g3 = ggplot(b1,aes(x=CP,y=p)) + geom_line() + geom_line(data=x1, aes(x=CP,y=p,col='red'),size=1.5) + 
  xlab('CPUE') + ylab('Cumulative Distribution') +theme(legend.position="none")+ ggtitle('LFA 33')

#LFA 34
b1 = aggregate(SD_LOG_ID~CP,data=subset(b,LFA==34),FUN=function(x) length(unique(x)))
b1$p = cumsum(b1$SD_LOG_ID)/sum(b1$SD_LOG_ID)

x1 = aggregate(TRIP~CP,data=subset(x,LFA==34),FUN=function(x) length(unique(x)))
x1$p = cumsum(x1$TRIP)/sum(x1$TRIP)

g4 = ggplot(b1,aes(x=CP,y=p)) + geom_line() + geom_line(data=x1, aes(x=CP,y=p,col='red'),size=1.5) + 
  xlab('CPUE') + ylab('Cumulative Distribution') +theme(legend.position="none") + ggtitle('LFA 34')

#LFA 35
b1 = aggregate(SD_LOG_ID~CP,data=subset(b,LFA==35),FUN=function(x) length(unique(x)))
b1$p = cumsum(b1$SD_LOG_ID)/sum(b1$SD_LOG_ID)

x1 = aggregate(TRIP~CP,data=subset(x,LFA==35),FUN=function(x) length(unique(x)))
x1$p = cumsum(x1$TRIP)/sum(x1$TRIP)

g5 = ggplot(b1,aes(x=CP,y=p)) + geom_line() + geom_line(data=x1, aes(x=CP,y=p,col='red'),size=1.5) + 
  xlab('CPUE') + ylab('Cumulative Distribution') +theme(legend.position="none")+ ggtitle('LFA 35')


#eastern shore and CB

g =  read.csv(file='results/CBFHAGCIFACPUE.csv')

b = bycatch.db('logbook.merge')
b = subset(b,LFA %in% c(27,'31A','31B'))

g$CP = round(g$mean/5,3)*5
b$CP = round(b$CPUE/5,3)*5
b = subset(b,CPUE<15)

b1 = aggregate(SD_LOG_ID~CP,data=subset(b,LFA==27),FUN=function(x) length(unique(x)))
b1$p = cumsum(b1$SD_LOG_ID)/sum(b1$SD_LOG_ID)

x1 = aggregate(TRIP~CP,data=subset(g,LFA==27),FUN=function(x) length(unique(x)))
x1$p = cumsum(x1$TRIP)/sum(x1$TRIP)

g27 = ggplot(b1,aes(x=CP,y=p)) + geom_line() + geom_line(data=x1, aes(x=CP,y=p,col='red'),size=1.5) + 
  xlab('CPUE') + ylab('Cumulative Distribution') +theme(legend.position="none")+ ggtitle('LFA 27')


b1 = aggregate(SD_LOG_ID~CP,data=subset(b,LFA=='31A'),FUN=function(x) length(unique(x)))
b1$p = cumsum(b1$SD_LOG_ID)/sum(b1$SD_LOG_ID)

x1 = aggregate(TRIP~CP,data=subset(g,LFA=='31A'),FUN=function(x) length(unique(x)))
x1$p = cumsum(x1$TRIP)/sum(x1$TRIP)

g31A = ggplot(b1,aes(x=CP,y=p)) + geom_line() + geom_line(data=x1, aes(x=CP,y=p,col='red'),size=1.5) + 
  xlab('CPUE') + ylab('Cumulative Distribution') +theme(legend.position="none")+ ggtitle('LFA 31A')


b1 = aggregate(SD_LOG_ID~CP,data=subset(b,LFA=='31B'),FUN=function(x) length(unique(x)))
b1$p = cumsum(b1$SD_LOG_ID)/sum(b1$SD_LOG_ID)

x1 = aggregate(TRIP~CP,data=subset(g,LFA=='31B'),FUN=function(x) length(unique(x)))
x1$p = cumsum(x1$TRIP)/sum(x1$TRIP)

g31B = ggplot(b1,aes(x=CP,y=p)) + geom_line() + geom_line(data=x1, aes(x=CP,y=p,col='red'),size=1.5) + 
  xlab('CPUE') + ylab('Cumulative Distribution') +theme(legend.position="none")+ ggtitle('LFA 31B')


require(gridExtra)

grid.arrange(g3,g4,g5,g27,g31A,g31B,nrow=2)
