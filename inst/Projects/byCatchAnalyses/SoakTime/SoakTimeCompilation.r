#using data compiled by CD April 5 2020

require(bio.lobster)
require(bio.utilities)
require(MASS)
require(lubridate)
options(stringsAsFactors=F)

fd = file.path(project.datadirectory('bio.lobster'),'data','SoakTimeData')
d = read.csv(file.path(fd,'SOAKTIME_CRCATCHES.csv')) #non lobster agg catches
f = read.csv(file.path(fd,'SOAKTIME_CRSAMPLES_20200407.csv'))


#prune species to crabs fish


f = subset(f,SPECIESCODE==2550)

#round soak days

d$SOAKDAYS = round(d$SOAKDAYS,0)
f$SOAKDAYS = round(f$SOAKDAYS,0)

#filter outliers

d = subset(d,SOAKDAYS>=0&SOAKDAYS<30 & COUNT<100)
f = subset(f,SOAKDAYS>=0&SOAKDAYS<30 & COUNT<100)
f$yr = year(f$STARTDATE)
d$yr = year(d$TRAPDATE)
#Total weight per trap haul

d$UID = paste(d$TRIPNO, d$TRAPNO, d$STRINGNO, sep="_")
f$UID = paste(f$TO_CHAR.TRIPNO., d$TRAPNO, sep="_")

#strong decreasing trend
fA = aggregate(COUNT~SOAKDAYS+UID+yr,data=f,FUN=sum)
plot(aggregate(COUNT~SOAKDAYS,data=fA,FUN=mean))
plot(aggregate(COUNT~yr,data=fA,FUN=mean))


dA = aggregate(COUNT~SOAKDAYS+UID,data=d,FUN=sum)
plot(aggregate(COUNT~SOAKDAYS,data=dA,FUN=mean))

