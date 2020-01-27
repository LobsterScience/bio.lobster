require(bio.lobster)
la()
require(lubridate)
a = groundfish.db('gsinf.odbc')
b = groundfish.db('gscat.odbc')
d = groundfish.db('gsdet.odbc')

a = a[,c('mission','setno','sdate','time','strat','dist','depth','gear','bottom_temperature','type')]
a = subset(a,type=1)
a$mon = month(a$sdate)

aa = merge(b,a,by=c('mission','setno'))

aaa = subset(aa, year %in% c(2016,2017,2019) & strat %in% c(440:495) & mon %in% c(6,7,8)) 


aaaa = aggregate(totno~spec+strat+year,data=aaa,FUN=length)
ab = aggregate(totno~spec+year,data=aaaa,FUN=length)

aad = merge(d,a,by=c('mission','setno'))

aad$rflen = round(aad$flen/5)*5
aad$rflen = ifelse(aad$spec==60,round(aad$flen/10/5)*5,aad$rflen)

aad$year = year(aad$sdate)

aaad = subset(aad,year %in% c(2016,2017,2018) & strat %in% c(440:495) & mon %in% c(6,7,8))

size.d = aggregate(flen~spec,data=aaad,FUN=function(x) c(quantile(x,0.01,na.rm=T),quantile(x,0.99,na.rm=T)))

size.d = as.data.frame(cbind(size.d$spec,unlist(size.d$flen)))
names(size.d) = c('spec','min','max')

size.d$diff = size.d$max - size.d$min

size.d.spec = subset(size.d,diff>15)$spec


saaad = aaad #subset(aaad,spec %in% size.d.spec)
sa = aggregate(clen~mission+setno+spec+rflen+year,data=saaad,FUN=sum)
sa = aggregate(clen~spec+rflen+year, data=sa,FUN=length)
sa = aggregate(clen~spec+rflen,data=sa,FUN=mean)

bb = reshape(sa,timevar='rflen',idvar='spec',direction='wide')
ig = ncol(bb)
bb$sums25 = apply(bb[,2:ig],1,function(i) sum(i>=25,na.rm=T))
bb$sums20 = apply(bb[,2:ig],1,function(i) sum(i>=20,na.rm=T))
bb$sums15 = apply(bb[,2:ig],1,function(i) sum(i>=15,na.rm=T))
bb$sums10 = apply(bb[,2:ig],1,function(i) sum(i>=10,na.rm=T))

bb = bb[order(bb$sums25,decreasing=T),]
write.csv(bb,'~/tmp/SpeciesAtLengthsetsavg.csv')


