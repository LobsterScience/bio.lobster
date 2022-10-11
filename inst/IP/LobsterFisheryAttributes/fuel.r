#fuel

x = read.csv(file.path(project.datadirectory('bio.lobster'),'data','EconomicData','fuelData.csv'))
xx = subset(x, GEO=='Halifax, Nova Scotia' & Type.of.fuel=="Diesel fuel at full service filling stations")
xx$date = as.Date(paste(xx$REF_DATE,'14',sep="-"), '%Y-%m-%d')

plot(xx$date,xx$VALUE)
xx$year = year(xx$date)

inf = lobster.db('inflation')

y = min(xx$year)
inf = subset(inf, year>=y)
inf$amount = c(1,rep(NA, nrow(inf)-1))
for(i in 2:(nrow(inf))){inf$amount[i]=inf$amount[i-1] + inf$amount[i-1]*inf$inflation.rate[i]}

si = merge(xx,inf,by.x='year',by.y='year')
si$PriceCorr = si$VALUE/si$amount


with(si, plot(date,xx$VALUE,type='l',col='red',xlab='Date',ylab='Diesel Fuel Cost (cents per L) ',ylim=c(35,164)))
with(si,lines(date, PriceCorr,col='black'))