#ILTS survey ggmap

require(bio.lobster)
require(bio.utilities)
require(ggplot2)
require(sf)
require(lubridate)

lobster.db('survey')

sC = makePBS(surveyCatch, polygon=F, coords = c('SET_LONG','SET_LAT') )
sC$month = month(sC$BOARD_DATE)
sC$wts = apply(sC[,c('EST_KEPT_WT','EST_DISCARD_WT')],1,sum, na.rm=T)
stations = aggregate(cbind(X,Y)~TRIP_ID+FISHSET_ID+BOARD_DATE+YEAR+month,data=sC,FUN=mean)
pollock = aggregate(cbind(X,Y)~TRIP_ID+FISHSET_ID+BOARD_DATE+YEAR+month+wts,data=subset(sC,SPECCD_ID==16),FUN=mean)
haddock = aggregate(cbind(X,Y)~TRIP_ID+FISHSET_ID+BOARD_DATE+YEAR+month+wts,data=subset(sC,SPECCD_ID==11),FUN=mean)
cod = aggregate(cbind(X,Y)~TRIP_ID+FISHSET_ID+BOARD_DATE+YEAR+month+wts,data=subset(sC,SPECCD_ID==10),FUN=mean)
sCf <- stations %>%   st_as_sf(crs = 4326, coords = c("X", "Y"))
sCP <- pollock %>%   st_as_sf(crs = 4326, coords = c("X", "Y"))
sCH <- haddock %>%   st_as_sf(crs = 4326, coords = c("X", "Y"))
sCC <- cod %>%   st_as_sf(crs = 4326, coords = c("X", "Y"))

s = ggLobsterMap('34-38',addGrids = F,return.object = T)
s+geom_sf(data=subset(sCf,YEAR<2007),col='black')+facet_wrap(~YEAR)+geom_sf(data=subset(sCP,YEAR<2007 ),col='red')
s+geom_sf(data=subset(sCf,YEAR>2006 &YEAR<2022),col='black')+facet_wrap(~YEAR)+geom_sf(data=subset(sCP,YEAR>2006 &YEAR<2022),col='red')

s = ggLobsterMap('34-38',addGrids = F,return.object = T)
s+geom_sf(data=subset(sCf,YEAR<2007),col='black')+facet_wrap(~YEAR)+geom_sf(data=subset(sCH,YEAR<2007 ),col='red')
s+geom_sf(data=subset(sCf,YEAR>2006 &YEAR<2022),col='black')+facet_wrap(~YEAR)+geom_sf(data=subset(sCH,YEAR>2006 &YEAR<2022),col='red')

s = ggLobsterMap('34-38',addGrids = F,return.object = T)
s+geom_sf(data=subset(sCf,YEAR<2007),col='black')+facet_wrap(~YEAR)+geom_sf(data=subset(sCC,YEAR<2007 ),col='red')
s+geom_sf(data=subset(sCf,YEAR>2006 &YEAR<2022),col='black')+facet_wrap(~YEAR)+geom_sf(data=subset(sCC,YEAR>2006 &YEAR<2022),col='red')


#lenght freqs

x=subset(fishMeasurements,SPECCD_ID==16)
xx = merge(x,sC[,c('TRIP_ID','SET_NO','BOARD_DATE')],all.x=T,by=c('TRIP_ID','SET_NO'))

w = aggregate(NUM_AT_LENGTH~FISH_LENGTH+TRIP_ID+SET_NO+BOARD_DATE,data=xx,FUN=max)
w$YEAR = year(w$BOARD_DATE)
ww = aggregate(NUM_AT_LENGTH~FISH_LENGTH+YEAR,data=w,FUN=sum)

ggplot(ww,aes(x=FISH_LENGTH,y=NUM_AT_LENGTH))+geom_bar(stat='identity')+facet_wrap('YEAR',scales='free_y')

x=subset(fishMeasurements,SPECCD_ID==11)
xx = merge(x,sC[,c('TRIP_ID','SET_NO','BOARD_DATE')],all.x=T,by=c('TRIP_ID','SET_NO'))

w = aggregate(NUM_AT_LENGTH~FISH_LENGTH+TRIP_ID+SET_NO+BOARD_DATE,data=xx,FUN=max)
w$YEAR = year(w$BOARD_DATE)
ww = aggregate(NUM_AT_LENGTH~FISH_LENGTH+YEAR,data=w,FUN=sum)

ggplot(subset(ww),aes(x=FISH_LENGTH,y=NUM_AT_LENGTH))+geom_bar(stat='identity')+facet_wrap('YEAR',scales='free_y')


x=subset(fishMeasurements,SPECCD_ID==10)
xx = merge(x,sC[,c('TRIP_ID','SET_NO','BOARD_DATE')],all.x=T,by=c('TRIP_ID','SET_NO'))

w = aggregate(NUM_AT_LENGTH~FISH_LENGTH+TRIP_ID+SET_NO+BOARD_DATE,data=xx,FUN=max)
w$YEAR = year(w$BOARD_DATE)
ww = aggregate(NUM_AT_LENGTH~FISH_LENGTH+YEAR,data=w,FUN=sum)

ggplot(subset(ww),aes(x=FISH_LENGTH,y=NUM_AT_LENGTH))+geom_bar(stat='identity')+facet_wrap('YEAR',scales='free_y')


# catches
haddock$lwt = log(haddock$wts)
sc1=seq(0.01,300,by=10)
haddock$TotWgt = sc1[cut(haddock$wts,sc1,labels=F)]
haddock$N_Sets = 1
ha = aggregate(N_Sets~YEAR+TotWgt,data=haddock,FUN=sum)
ggplot(ha,aes(x=TotWgt,y=N_Sets))+geom_bar(stat='identity')+facet_wrap('YEAR',scales='free_y')


# catches

sc1=seq(0.01,100,by=10)
pollock$TotWgt = sc1[cut(pollock$wts,sc1,labels=F)]
pollock$N_Sets = 1
ha = aggregate(N_Sets~YEAR+TotWgt,data=pollock,FUN=sum)
ggplot(ha,aes(x=TotWgt,y=N_Sets))+geom_bar(stat='identity')+facet_wrap('YEAR',scales='free_y')





