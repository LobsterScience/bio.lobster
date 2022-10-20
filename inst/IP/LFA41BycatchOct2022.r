# LFA41 bycatch by effort
require(bio.lobster)
require(bio.utilities)
require(devtools)
require(lubridate)

lobster.db('atSea')

a = subset(atSea,LFA==41)
a = subset(a, STARTDATE>'2018-10-01')
#ntrip[s]
aggregate(TRIPNO~year(STARTDATE),data=a,FUN=function(x) length(unique(x)))


a$UID = paste(a$TRIPNO, a$STARTDATE,a$STRINGNO, a$TRAPNO,sep="-")
i = which(a$SPECIESCODE==2550 ) 
j = which(a$SEX==3 | !is.na(a$VNOTCH) | a$SHELL %in% 1:3 | a$CARLENGTH<82 | a$CARLENGTH>140)
k = intersect(i,j)
a$DiscardL=0
a$DiscardL[k] = a$CALWT[k]
a$Empty = ifelse(is.na(a$SPECIESCODE),1,0)
a$Jonah = ifelse(a$SPECIESCODE==2511,a$CALWT,0)
a$Haddock = ifelse(a$SPECIESCODE==11,a$CALWT,0)
a$Cusk = ifelse(a$SPECIESCODE==15,a$CALWT,0)
a$Cod = ifelse(a$SPECIESCODE==10,a$CALWT,0)
a$Raven = ifelse(a$SPECIESCODE==320,a$CALWT,0)
a$Whake = ifelse(a$SPECIESCODE==12,a$CALWT,0)
a$Rhake = ifelse(a$SPECIESCODE==13,a$CALWT,0)

a$yr = year(a$STARTDATE)
az = na.zero(a)

aa = aggregate(cbind(Empty, DiscardL, Jonah, Haddock, Cusk, Cod, Raven, Whake, Rhake)~yr+UID,data=az,FUN=sum, na.rm=T)
ar = subset(aa,yr>2017)


aggregate(cbind(DiscardL, Jonah, Haddock, Cusk, Cod, Raven, Whake, Rhake)~yr,data=ar,FUN=mean, na.rm=T)

#no year
xx = aggregate(cbind(DiscardL, Jonah, Haddock, Cusk, Cod, Raven, Whake, Rhake)~1,data=ar,FUN=mean, na.rm=T)/1000


#ntraps
aggregate(UID~yr,data=ar,FUN=function(x) length(unique(x)))


#logbooks

lobster.db('logs41')
x = subset(logs41, FV_FISHED_DATETIME>'2017-12-31' )

s = sum(as.numeric(x$NUM_OF_TRAPS),na.rm=T)

