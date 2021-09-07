require(bio.lobster)
require(bio.utilities)

#load("C:/bio.data/bio.lobster/data/ODBCDump/atSea.rdata") #Import AtSea data
#lobster.db('atSea.clean.redo')
lobster.db('atSea.redo')
lobster.db('atSea.clean.redo')

lobster.db('atSea.clean')


library(lubridate)

lfas= c("32", "31A", "31B", "27")
a=atSea.clean[atSea.clean$LFA %in% lfas,]
a$yr=year(a$STARTDATE)
b=a[a$yr>2009,]
b=b[is.finite(b$CARLENGTH) & is.finite(b$SEX),]

for (l in lfas){
c=b[b$LFA==l,]
print(l)
cu=c[!duplicated(c[,c('TRIPNO')]),]
yrs=hist(cu$yr, breaks=10, main=paste0("Samples in ",l)) #shows distribution of samples across time
yrs=hist(c$yr, breaks=10, main=paste0("Animals in ",l)) #shows distribution of samples across time

clr=ifelse(noegg$breaks < 100 | noegg$breaks > 109  , "grey", "red")[-length(noegg$breaks)]
noegg=hist(c$CARLENGTH[c$SEX==2], xlim=c(0,180), breaks=seq(0, 180, 5), main="Non-Ovigerous Females", col=clr)

clr2=ifelse(egg$breaks < 125, "grey", "red")[-length(egg$breaks)]
egg=hist(c$CARLENGTH[c$SEX==3], xlim=c(0,180), breaks=seq(0, 180, 5), main="Ovigerous Females", col=clr2)

}