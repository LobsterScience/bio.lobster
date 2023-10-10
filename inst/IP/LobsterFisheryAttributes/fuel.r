#fuel
#from https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=1810000101

x = read.csv(file.path(project.datadirectory('bio.lobster'),'data','EconomicData','fuelData.csv'))
xx = subset(x, GEO=='Halifax, Nova Scotia' & Type.of.fuel=="Diesel fuel at full service filling stations")
xx$date = as.Date(paste(xx$REF_DATE,'14',sep="-"), '%Y-%m-%d')

xy = subset(x, GEO=='Halifax, Nova Scotia' & Type.of.fuel=="Diesel fuel at self service filling stations")
xy$date = as.Date(paste(xy$REF_DATE,'14',sep="-"), '%Y-%m-%d')

xx = rbind(xx[,c('date','VALUE')],xy[,c('date','VALUE')])

plot(xx$date,xx$VALUE,xlim=c(min(xx$date),max(xy$date)),ylim=c(min(xx$VALUE),max(xy$VALUE)))
points(xy$date,xy$VALUE,col='red')

xx$year = year(xx$date)

inf = lobster.db('inflation')

y = min(xx$year)
inf = subset(inf, year>=y)
inf$amount = c(1,rep(NA, nrow(inf)-1))
for(i in 2:(nrow(inf))){inf$amount[i]=inf$amount[i-1] + inf$amount[i-1]*inf$inflation.rate[i]}

si = merge(xx,inf,by.x='year',by.y='year')
si$PriceCorr = si$VALUE/si$amount


with(si, plot(date,xx$VALUE,type='l',col='red',xlab='Date',ylab='Diesel Fuel Cost (cents per L) ',ylim=c(35,250)))
with(si,lines(date, PriceCorr,col='black'))