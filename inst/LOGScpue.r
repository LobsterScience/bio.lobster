p = bio.lobster::load.environment()


## TOTAL LANDINGS
AnnualLand.dat = lobster.db('annual.landings')
HistoricLand.dat = lobster.db('historical.landings')
SeasonalLand.dat = lobster.db('seasonal.landings')

Annual.dat = reshape(AnnualLand.dat,idvar="YEAR",varying=names(AnnualLand.dat)[-1],times=substr(names(AnnualLand.dat)[-1],4,6),direction='long',timevar="LFA",v.names="CATCH")
Season.dat = reshape(SeasonalLand.dat,idvar="SEASON",varying=names(SeasonalLand.dat)[-1],times=substr(names(SeasonalLand.dat)[-1],4,6),direction='long',timevar="LFA",v.names="CATCH")
Annual.dat$SYEAR = Annual.dat$YEAR
Season.dat$SYEAR = as.numeric(substr(Season.dat$SEASON,6,9))
Landings.dat = rbind(subset(Annual.dat,LFA<33&YEAR>1975,c("SYEAR","LFA","CATCH")),subset(Season.dat,select=c("SYEAR","LFA","CATCH")))

write.csv(Landings.dat,file.path( project.datadirectory("lobster"), "data","products","TotalLandings.csv"),row.names=F)





## LOGS - basic summary
logsInSeason = lobster.db('process.logs.redo')
logsInSeason = lobster.db('process.logs')
	
#### Plotting

lfas = c("27", "28", "29", "30", "31A", "31B", "32", "33", "34", "35", "36", "38")
cpue.dat = CPUEplot(logsInSeason,lfa=lfas,yrs=2001:2017)

subareas = c("27N","27S", "28", "29", "30", "31A", "31B", "32", "33E", "33W", "34", "35", "36", "38")
cpue.dat = CPUEplot(logsInSeason,subarea=subareas,yrs=2001:2017)



cpuegrids = lobGridPlot(subset(logsInSeason,LFA=='34'&SYEAR==2014,c("LFA","GRID_NUM","TOTAL_WEIGHT_KG")),FUN=sum,place=1)
LobsterMap('34',poly.lst=cpuegrids)




####### 2014 CATCH with survey location LFA 34

catchgrids = lobGridPlot(subset(logsInSeason,LFA=='33'&SYEAR==2014,c("LFA","GRID_NUM","WEIGHT_KG")),lvls=c(1000,50000,100000,200000,400000,600000,800000,1000000),FUN=sum,border=NA)
	
pdf(file.path( project.datadirectory("lobster"), "figures","LFA34.pdf"),8,11)

LobsterMap('33',poly.lst=catchgrids[1:2],title="2014 Lobster Catch")
ContLegend("bottomleft",lvls=catchgrids$lvls/1000,Cont.data=catchgrids,title="Catch (t)",inset=0.02,cex=0.8,bg='white')
ss2015 = read.csv(file.path( project.datadirectory("lobster"), "data","products","LFA34TrawlStations2015.csv"))
with(subset(ss2015,TYPE%in%c('index','2014_index')),points(DDLON,DDLAT,pch=16,col='red'))
with(subset(ss2015,TYPE%in%c('2014','2014_index')),points(DDLON,DDLAT))
legend('topleft',c('index','2014'),col=c('red','black'),pch=c(16,1),inset=0.02,cex=0.8,bg='white')

dev.off()


loadfunctions('lobster')
lvls34 = c(1000,50000,100000,200000,400000,600000,800000,1000000)
lvls33 = c(10000,35000,70000,105000,140000,175000,210000,300000)
catchgrids = lobGridPlot(subset(logsInSeason,LFA=='33'&SYEAR==2014,c("LFA","GRID_NUM","WEIGHT_KG")),lvls=lvls33,FUN=sum,border=NA)
	
pdf(file.path( project.datadirectory("lobster"), "figures","LFA34.pdf"),8,11)

LobsterMap('33',poly.lst=catchgrids[1:2],title="2014 Lobster Catch")
ContLegend("bottomright",lvls=catchgrids$lvls/1000,Cont.data=catchgrids,title="Catch (t)",inset=0.02,cex=0.8,bg='white')
ss2015 = read.csv(file.path( project.datadirectory("lobster"), "data","products","LFA34TrawlStations2015.csv"))
with(subset(ss2015,TYPE%in%c('index','2014_index')),points(DDLON,DDLAT,pch=16,col='red'))
with(subset(ss2015,TYPE%in%c('2014','2014_index')),points(DDLON,DDLAT))
legend('topleft',c('index','2014'),col=c('red','black'),pch=c(16,1),inset=0.02,cex=0.8,bg='white')

dev.off()


	lvls=c(10,50,100,500,1000,5000,10000,50000)
		windows(12,8)
		BBnpolys = gridPlot(BBnfish.dat,BBnBoundPoly,lvls,border=NA,FUN=sum,grid.size=1/60)
		ScallopMap(ylim=c(42.5,42.95),xlim=c(-66.5,-65.7),bathy.source='USGS',plot.lines=T,bathcol=rgb(0,0,1,0.3),poly.lst=BBnpolys[1:2],title="",cex=1.2)

	
############### voluntary logs ####################


lobster.db('vlog')

vlog$SYEAR = as.numeric(substr(vlog$SEASON,6,9))
vlog$W_KG = vlog$W_TOT*0.4536
vlog$CPUE = vlog$W_KG/vlog$N_TRP

vlog$X = convert.dd.dddd(vlog$LONGITUDE)*-1
vlog$Y = convert.dd.dddd(vlog$LATITUDE)

Ports = read.csv(file.path( project.datadirectory("lobster"), "data","inputs","Ports.csv"))

ports31A = subset(Ports,LFA=='31A')$Port_Code
ports31B = c(subset(Ports,LFA=='31B')$Port_Code,11799)
stat33E = c(18,22,23,25,26)
stat33W = c(27,28,30,31)
stat27N = c(1,4)
stat27S = c(6,7)



vlog$LFA[vlog$STAT%in%stat27N] = "27N"
vlog$LFA[vlog$STAT%in%stat27S] = "27S"
vlog$LFA[vlog$STAT%in%stat33E] = "33E"
vlog$LFA[vlog$STAT%in%stat33W] = "33W"
vlog$LFA[vlog$PORT_CODE%in%ports31A] = "31A"
vlog$LFA[vlog$PORT_CODE%in%ports31B] = "31B"

lfas = sort(unique(vlog$LFA))
### by LFA
cpue3.lst = list()
for(i in 1:length(lfas)) {
	dat = na.omit(subset(vlog,LFA==lfas[i],c('SYEAR','W_KG','N_TRP')))
	catch = with(dat,tapply(W_KG,SYEAR,sum))
	effort = with(dat,tapply(N_TRP,SYEAR,sum))
	n = with(dat,tapply(N_TRP,SYEAR,length))

	cpue3.lst[[i]] = data.frame(lfa=lfas[i],year=sort(unique(dat$SYEAR)),n=n,catch=catch,effort=effort,cpue=catch/effort)
}

names(cpue3.lst) = lfas
VLOGcpue.dat = do.call("rbind",cpue3.lst)

write.csv(VLOGcpue.dat,file.path( project.datadirectory("lobster"), "data","products","VolLogsCPUE_LFA.csv"),row.names=F)
VLOGcpue.dat = read.csv(file.path( project.datadirectory("lobster"), "data","products","VolLogsCPUE_LFA.csv"))
LOGcpue.dat = read.csv(file.path( project.datadirectory("lobster"), "data","products","CommercialCPUE_LFA.csv"))

#lfas = c(27:30,32,33)
par(mfrow=c(3,3))
for(i in 1:length(lfa)) {
Vlogs = subset(VLOGcpue.dat,lfa==lfas[i])
Mlogs = subset(LOGcpue.dat,lfa==lfas[i])

plot(cpue~year,Vlogs,type='b',col='red',pch=16,xlim=c(1981,2014),ylim=c(0,2.5),ylab="Kg/TH",xlab='',main=lfas[i])
lines(cpue~year,Mlogs,type='b')
#with(Vlogs,lines(year-0.25,n/max(n,Mlogs$n)*2.5,type='h',col='red',lwd=2))
#with(Mlogs,lines(year+0.25,n/max(n,Vlogs$n)*2.5,type='h',lwd=2))

}

### by LFA by quater
catchquarter.lst = list()
cpuequarter.lst = list()
for(i in 1:length(lfa)) {
	for(q in 1:4){
		dat = na.omit(subset(logsInSeason,LFA==lfa[i]&TOTAL_NUM_TRAPS<max_trap[i]&quarter==q,c('SYEAR','WEIGHT_KG','NUM_OF_TRAPS')))
		tmp = data.frame(year=sort(unique(dat$SYEAR)),catch=with(dat,tapply(WEIGHT_KG,SYEAR,sum)))
		names(tmp)[2] = paste0('q',q)
		if(q==1)catch = tmp
		catch = merge(catch,tmp,all=T)
		tmp = data.frame(year=sort(unique(dat$SYEAR)),cpue=with(dat,tapply(WEIGHT_KG,SYEAR,sum)/tapply(NUM_OF_TRAPS,SYEAR,sum)))
		names(tmp)[2] = paste0('q',q)
		if(q==1)cpue = tmp
		cpue = merge(cpue,tmp,all=T)
	}
	catch[is.na(catch)] = 0
	catchquarter.lst[[i]] = catch
	cpuequarter.lst[[i]] = cpue
}

colMeans(catchquarter.lst[[9]][-(1:2),-1]/rowSums(catchquarter.lst[[9]][-(1:2),-1]))

names(cpue2.lst) = lfa
LOGcpue.dat = do.call("rbind",cpue2.lst)

write.csv(cpue2.dat,file.path( project.datadirectory("lobster"), "data","products","CommercialCPUE_LFA.csv"),row.names=F)

LOGcpue.dat = read.csv(file.path( project.datadirectory("lobster"), "data","products","CommercialCPUE_LFA.csv"))




##### Seasonal GAM




lfas = c("27", "28", "29", "30", "31A", "31B", "32", "33", "34", "35", "36", "38")
daily.dat = CPUEplot(logsInSeason,lfa=lfas,yrs=2001:2014)

#daily.dat = read.csv(file.path(project.datadirectory('lobster'),'data',"DailyCPUE.csv"))

data = subset(daily.dat,LFA==34)


data$SYEAR = year(data$DATE)
data$SYEAR[month(data$DATE)>8] = data$SYEAR[month(data$DATE)>8]+1

syr = sort(unique(data$SYEAR))

for(i in 1:length(syr)){
data$TIME[data$SYEAR==syr[i]] = julian(data$DATE[data$SYEAR==syr[i]],origin=as.Date(paste(syr[i]-1,'08','31',sep='-')))/365

}

mf1 = 'CPUE ~ s(SYEAR) + s(TIME, bs="cc")'

CPUEgam(data,mf1)

data$sin.w = sin(2*pi*(data$SYEAR+data$TIME))
data$cos.w = cos(2*pi*(data$SYEAR+data$TIME))

mf2 = 'CPUE  ~ s(SYEAR) + s(SYEAR, cos.w) + s(SYEAR, sin.w) + s(cos.w) + s(sin.w)'
mf3 = 'CPUE  ~ s(SYEAR) + s(cos.w) + s(sin.w)'

CPUEgam(data,mf2)

CPUEgam(data,mf3)



