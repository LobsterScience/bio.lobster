require(bio.lobster)
require(devtools)
require(sf)
require(bio.utilities)
require(ggplot2)
require(dplyr)

la()

a = lobster.db('greyzone_logs')
a$Date = lubridate::ymd(a$DATE_SAILED)
a$yr = lubridate::year(a$Date)

a = a%>%
    group_by(yr) %>%
    mutate(dos = as.numeric(Date-min(Date)), NUM_OF_TRAPS=as.numeric(NUM_OF_TRAPS), wt_kg = EST_WEIGHT_LOG_LBS/2.2046) %>%
    ungroup()

#n_vessels
g = aggregate(LICENCE_ID~yr,data=a,FUN=function(x) length(unique(x)))

aa = split(a,f=list(a$yr))
aa = rm.from.list(aa)
aa = aa[-1]
cpue.lst<-list()
#by time
for(i in 1:length(aa)){
  tmp<-aa[[i]]
  tmp = tmp[,c('Date','wt_kg','NUM_OF_TRAPS')]
  names(tmp)<-c('time','catch','effort')
  tmp$date<-as.Date(tmp$time)
  first.day<-min(tmp$date)
  tmp$time<-julian(tmp$date,origin=first.day-1)
  tmp = tmp[order(tmp$time),]
  tmp$time = ceiling(tmp$time) #convert to week of season
  tmp = as.data.frame(tmp)
  tmp = subset(tmp,!is.na(effort))
  g<-as.data.frame(biasCorrCPUE(tmp,by.time=T,min.sample.size = 5))
  g$yr = unique(aa[[i]]$yr)
  # g = t(g)[,1]
  cpue.lst[[i]] <- g
}

cc =as.data.frame(do.call(rbind,cpue.lst))
#cc$CPUE = as.numeric(cc$`biasCorrCPUE(tmp, by.time = F)`)
#plot(aggregate(CPUE~t,data=subset(cc,
cc <- cc %>%
  mutate(date = as.Date("30-JUN", format = "%d-%b") + t + years(2001-2025))

# View result
print(df)

ggplot(subset(cc),aes(x=date,y=unBCPUE))+geom_point()+geom_smooth()+facet_wrap(~yr)+lims(y=c(0,3.4))+labs(x='Date', y='CPUE')

#not time

cpue.ann<-list()
#by time
for(i in 1:length(aa)){
  tmp<-aa[[i]]
  tmp = tmp[,c('Date','wt_kg','NUM_OF_TRAPS')]
  names(tmp)<-c('time','catch','effort')
  tmp$date<-as.Date(tmp$time)
  first.day<-min(tmp$date)
  tmp$time<-julian(tmp$date,origin=first.day-1)
  tmp = tmp[order(tmp$time),]
  tmp$time = ceiling(tmp$time) #convert to week of season
  tmp = as.data.frame(tmp)
  tmp = subset(tmp,!is.na(effort))
  g<-as.data.frame(t(biasCorrCPUE(tmp,by.time=F,min.sample.size = 5)))
  g$yr = unique(aa[[i]]$yr)
  # g = t(g)[,1]
  cpue.ann[[i]] <- g
}

ca =as.data.frame(do.call(rbind,cpue.ann))

ggplot(subset(ca,yr>2009),aes(x=yr,y=unBCPUE))+geom_point()+geom_smooth()+theme_minimal()

#####spatial logs

a = lobster.db('greyzone_logs')
a$Date = lubridate::ymd(a$DATE_SAILED)
a$yr = lubridate::year(a$Date)


a$DDLAT = round((((a$ENT_LATITUDE /100/100-trunc(a$ENT_LATITUDE/100/100))*100)/60)+trunc(a$ENT_LATITUDE/100/100),4)
a$DDLON = round((((a$ENT_LONGITUDE/100/100-trunc(a$ENT_LONGITUDE/100/100))*100)/60)+trunc(a$ENT_LONGITUDE/100/100),4)


