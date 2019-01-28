options(stringsAsFactors=F)
a = read.csv('tmp/LFA27BycatchTMP/LFA_27_TRIPS EXCLUDED_2018.05.14.csv')

hist(a$RATIO[which(a$RATIO<0.01)],breaks=seq(0,0.01,by=0.001))
a$e = ifelse(a$RATIO<0.004,1,0)
a$t = 1
require(lubridate)
a$date = dmy(a$DATE)
a$y = year(a$date)

b= aggregate(cbind(e,t)~y,data=a,FUN=sum)




require(bio.lobster)
require(lubridate)

g = lobster.db('cris')

gg = merge(cris.samples,cris.traps,by=c('TRIPNO','PORT','TRAPNO'))

gg = merge(gg,cris.trips,by=c('TRIPNO','PORT'))
gg$I = 1
gg = merge(gg,g$crports.csv,by='PORT')

#How many nonlobster for the 0.4% cutoff

gt = aggregate(I~TRIPNO+STARTDATE+SPECIESCODE+LFA_ID,data=gg,FUN=sum)

gt27 = subset(gt,LFA_ID==2)

gt27$IL = ifelse(gt27$SPECIESCODE==1,gt27$I,0)

gt27$IO = ifelse(gt27$SPECIESCODE>1,gt27$I,0)
gt27$y = year(gt27$STARTDATE)
g27 = aggregate(cbind(IO,IL)~TRIPNO+y,data=gt27,FUN=sum)

g27$rat = g27$IO/g27$IL
mean(subset(g27,rat<0.004 & y>2010)$IO) #on average this is only one non lobster measured per trip for the cutoff

#number of non lobster measured

hist(subset(g27,IO<100)$IO
abline(v=1,col='red')

g27$ids = ifelse(g27$rat<0.004,1,0)
g27$id3 = 1

kk = aggregate(cbind(ids,id3)~y,data=g27,FUN=sum)
with(subset(kk,y>2010),plot(y,ids/id3,type='b',xlab='year',ylab='Prop <0.004 by Number',ylim=c(0,1)))
x = aggregate(cbind(e,t)~y,data=a,FUN=sum)
x$r = x$e/x$t
with(x,lines(y,r,col='red'))


