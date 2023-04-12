#lfa glorys data

require(bio.lobster)
require(bio.utilities)
require(sf)

cl=readRDS(file.path(project.datadirectory('bio.lobster'),'analysis','ClimateModelling','GlorysClimatologies1993-2022byDOY.rds'))



rL = readRDS(file.path( project.datadirectory("bio.lobster"), "data","maps","LFAPolysSF.rds"))
rL = st_as_sf(rL)
st_crs(rL) <- 4326
rL = st_transform(rL,32620) 
st_geometry(rL) <- st_geometry(st_as_sf(rL$geometry/1000)) 
st_crs(rL) <- 32620


ff = st_join(cl,rL,join=st_within)
gsf = subset(ff,!is.na(LFA))
saveRDS(gsf,file.path(project.datadirectory('bio.lobster'),'analysis','ClimateModelling','GlorysClimatologies1993-2022byDOYWithLFA.rds'))

gsR = subset(gsf,z>2 & z<30)

gsR29 = subset(gsR, LFA==29 & doy %in% 165:175)
gsR311 = subset(gsR, LFA==311 & doy %in% 165:175)
gsR312 = subset(gsR, LFA==312 & doy %in% 165:175)

gsR30 = subset(gsR, LFA %in% c(30) & doy %in% 190:200)

#lfa27

l27=read.csv(file.path(project.datadirectory('bio.lobster'),'data','maps','areas.27.csv'))
l27$label = l27$area
r = pbs.2.gis(l27,make.sf = T,env.object = T,type='polygon',spdf = F)
r = bio.utilities::list.names.to.columns(r)
r$area =r$V2
st_crs(r) <- 4326
r = st_transform(r,32620) 
st_geometry(r) <- st_geometry(st_as_sf(r$geometry/1000)) 
st_crs(r) <- 32620

gs27 = st_join(subset(gsR,LFA==27 & doy %in% 190:200),r,join=st_within)

##fsrs data

lobster.db('fsrs')
fsrs$DEPTH_M = fsrs$DEPTH*1.8288
fsrs$doy = yday(fsrs$HAUL_DATE)
x = subset(fsrs, TEMP> -99 & TEMP<30 & !is.na(TEMP) & HAUL_YEAR>2001)
fsR29 = aggregate(TEMP~HAUL_YEAR,data=subset(x, LFA==29 & doy %in% 165:175),FUN=median)
fsR311 = aggregate(TEMP~HAUL_YEAR,data=subset(x, LFA==31.1 & doy %in% 165:175),FUN=median)
fsR312 = aggregate(TEMP~HAUL_YEAR,data=subset(x, LFA==31.2 & doy %in% 165:175),FUN=median)

fsR30 = aggregate(TEMP~HAUL_YEAR,data=subset(x, LFA==30 & doy %in% 190:200),FUN=median)

fsR27 = subset(x, LFA==27 & doy %in% 190:200)
fsR27 = st_as_sf(fsR27,coords=c('LONG_DD','LAT_DD'),crs=4326)
fsR27 = st_transform(fsR27,32620) 
st_geometry(fsR27) <- st_geometry(st_as_sf(fsR27$geometry/1000)) 
st_crs(fsR27) <- 32620

fs27 = st_join(subset(fsR27),r,join=st_within)
fs27 = aggregate(TEMP~HAUL_YEAR+area,data=fs27,FUN=median)


xx=as_tibble(gsR29)
xs = grep('BT',names(xx))
xx1 = data.frame(apply(xx[xs],2,mean,na.rm=T))
xx1$yr = na.omit(as.numeric(unlist(strsplit(row.names(xx1),'BT.'))))
names(xx1)=c('Temp','yr')
plot(xx1$yr,xx1$Temp,type='b',ylim=c(4,12))
lines(fsR29$HAUL_YEAR,fsR29$TEMP,type='b',col='red')
names(fsR29)=c('yr','fsrstemp')
cTemp = merge(xx1,fsR29,all.x=T)
write.csv(cTemp,'/home/adam/dellshared/LFA29GloryFsrs.csv')

savePlot('/home/adam/dellshared/LFA29Temp.png')


xx=as_tibble(gsR311)
xs = grep('BT',names(xx))
xx1 = data.frame(apply(xx[xs],2,mean,na.rm=T))
xx1$yr = na.omit(as.numeric(unlist(strsplit(row.names(xx1),'BT.'))))
names(xx1)=c('Temp','yr')
plot(xx1$yr,xx1$Temp,type='b',ylim=c(4,12))
lines(fsR311$SYEAR,fsR311$TEMP,type='b',col='red')
names(fsR311)=c('yr','fsrstemp')
cTemp = merge(xx1,fsR311,all.x=T)
write.csv(cTemp,'/home/adam/dellshared/LFA31AGloryFsrs.csv')
savePlot('/home/adam/dellshared/LFA31ATemp.png')


xx=as_tibble(gsR312)
xs = grep('BT',names(xx))
xx1 = data.frame(apply(xx[xs],2,mean,na.rm=T))
xx1$yr = na.omit(as.numeric(unlist(strsplit(row.names(xx1),'BT.'))))
names(xx1)=c('Temp','yr')
plot(xx1$yr,xx1$Temp,type='b',ylim=c(4,12))
lines(fsR312$SYEAR,fsR312$TEMP,type='b',col='red')
names(fsR312)=c('yr','fsrstemp')
cTemp = merge(xx1,fsR312,all.x=T)
write.csv(cTemp,'/home/adam/dellshared/LFA31BGloryFsrs.csv')

savePlot('/home/adam/dellshared/LFA31BTemp.png')


xx=as_tibble(gsR30)
xs = grep('BT',names(xx))
xx1 = data.frame(apply(xx[xs],2,mean,na.rm=T))
xx1$yr = na.omit(as.numeric(unlist(strsplit(row.names(xx1),'BT.'))))
names(xx1)=c('Temp','yr')
plot(xx1$yr,xx1$Temp,type='b',ylim=c(4,15))
lines(fsR30$HAUL_YEAR,fsR30$TEMP,type='b',col='red')
names(fsR30)=c('yr','fsrstemp')
cTemp = merge(xx1,fsR30,all.x=T)
write.csv(cTemp,'/home/adam/dellshared/LFA30GloryFsrs.csv')

savePlot('/home/adam/dellshared/LFA30Temp.png')


#27
xx=as_tibble(subset(gs27,area=='27.South' & doy %in% 190:200))
xs = grep('BT',names(xx))
xx1 = data.frame(apply(xx[xs],2,mean,na.rm=T))
xx1$yr = na.omit(as.numeric(unlist(strsplit(row.names(xx1),'BT.'))))
names(xx1)=c('Temp','yr')
xx1$area='27.South'
xxS = xx1

xx=as_tibble(subset(gs27,area=='27.Central' & doy %in% 190:200))
xs = grep('BT',names(xx))
xx1 = data.frame(apply(xx[xs],2,mean,na.rm=T))
xx1$yr = na.omit(as.numeric(unlist(strsplit(row.names(xx1),'BT.'))))
names(xx1)=c('Temp','yr')
xx1$area='27.Central'
xxC = xx1

xx=as_tibble(subset(gs27,area=='27.North' & doy %in% 190:200))
xs = grep('BT',names(xx))
xx1 = data.frame(apply(xx[xs],2,mean,na.rm=T))
xx1$yr = na.omit(as.numeric(unlist(strsplit(row.names(xx1),'BT.'))))
names(xx1)=c('Temp','yr')
xx1$area='27.North'
xxN = xx1

xx=rbind(xxS,rbind(xxC,xxN))

names(fs27) = c('yr','area','fsrsTemp')

cTemp = merge(xx,fs27,all.x=T)
write.csv(cTemp,'/home/adam/dellshared/LFA27GloryFsrs.csv')

with(subset(cTemp,area=='27.South'),{
	plot(yr,Temp,type='b',ylim=c(3,16))
	lines(yr,fsrsTemp,type='b',col='red')
}

ggplot(data=cTemp) + geom_line(aes(x=yr,y=Temp,col='black')) +geom_line(aes(x=yr,y=fsrsTemp,col='red'))+ facet_wrap(~area)
lines(fsR30$SYEAR,fsR30$TEMP,type='b',col='red')
savePlot('/home/adam/dellshared/LFA30Temp.png')


#####standardized anomalies of temp within season

gsR311 = subset(gsR, LFA==311 & doy %in% 120:181)
gsR312 = subset(gsR, LFA==312 & doy %in% 120:181)


gsR29 = subset(gsR, LFA==30 & doy %in% 120:181)
st_geometry(gsR29) <- NULL
ss = as_tibble(subset(gsR29,select=c(-z,-LFA,-V2))) %>% group_by(doy) %>% summarise(across(everything(),list(median)))
ss1 = ss %>% 	pivot_longer(cols=starts_with('BT'),names_to='yr',values_to='temp')
##compare recent years to climatology for 1993-2002
ss1$yr = substr(ss1$yr,4,7)
ssa = ss1 %>% group_by(doy) %>% filter(yr > 2002 & yr <2008 ) %>% summarise(mtemp = median(temp),stemp = sd(temp))
ssm = full_join(ssa,subset(ss1,yr>2008))

ssm$anom = (ssm$temp - ssm$mtemp) / ssm$stemp
ssm$sign = sign(ssm$anom)

 ggplot(ssm,aes(x=doy,y=anom,colour=sign))+geom_line()+facet_wrap(~yr)

#unbiased cpues
a = lobster.db('process.logs')
a = subset(a,SYEAR %in% 2006:2022 & LFA %in% 30)
aa = split(a,f=list(a$LFA,a$SYEAR))
cpue.lst<-list()

for(i in 1:length(aa)){
  tmp<-aa[[i]]
  tmp = tmp[,c('DATE_FISHED','WEIGHT_KG','NUM_OF_TRAPS')]
  names(tmp)<-c('time','catch','effort')
  tmp$date<-as.Date(tmp$time)
  first.day<-min(tmp$date)
  tmp$time<-julian(tmp$date,origin=first.day-1)
  g<-as.data.frame(biasCorrCPUE(tmp,by.time=T,min.sample.size=5))
  g$lfa=unique(aa[[i]]$LFA)
  g$yr = unique(aa[[i]]$SYEAR)
  cpue.lst[[i]] <- g
}

cc =as.data.frame(do.call(rbind,cpue.lst))
cc$doy = cc$t+119

#merge temp and cpue
sc = full_join(cc,subset(ss1,yr>2001),by=c('yr','doy'))

ggplot(sc,aes(x=temp,y=unBCPUE)) +geom_line() +facet_wrap(~yr)

ggplot() +geom_line(data=sc,mapping = aes(x=doy,y=unBCPUE)) + 
			geom_line(data=sc,mapping = aes(x=doy,y=temp))+
			facet_wrap(~yr) +
			scale_y_continuous(name='CPUE',sec.axis=sec_axis(~.+10,name='Temperature'))

cca = cc %>% group_by(doy) %>% filter(yr > 2002 & yr <2008 ) %>% summarise(mcpue = median(unBCPUE),stemp = sd(unBCPUE))
ccm = full_join(cca,subset(cc,yr>2008))

ssm$anom = (ssm$temp - ssm$mtemp) / ssm$stemp
