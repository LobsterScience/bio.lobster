require(bio.lobster)
require(devtools)
require(dplyr)
require(ggplot2)
require(bio.utilities)
la()


setwd(file.path(project.datadirectory('bio.lobster'),'data','sGSL'))

x = readxl::read_xlsx("GFL2022_086_Lobster logs_2014-2021_Natalie Asselin.xlsx")
x1 = readxl::read_xlsx("GLF2024_068_2021-2024 - Lobster Logbook - GSSSB377_Natalie_Asselin.xlsx")

x  = bind_rows(x,x1)
names(x) = gsub(" ","_",names(x))

x$id = paste(x$Log_Date,x$Lic.Id,x$Vessel,sep="_")


#how many unique log entries per lfa and year

n_ent = aggregate(id~Log_Year+Licensed_Fishing_Area,data=x,FUN=function (x) length(unique(x)))
n_traps = aggregate(Hauled~Licensed_Fishing_Area+Log_Year,data=x,FUN=sum)

n_traps$src = 'RawLogs_NA'
sss = read.csv('PrivacyProtectedGulfLogsbyPort.csv')
ss = aggregate(total_trap_numbers~lfa+Year,data=sss,FUN=sum)
ss$src = 'RolledUpLogs_Stats'

names(ss) = names(n_traps)
st = bind_rows(ss,n_traps)

ggplot(st,aes(x=Log_Year,y=Hauled,colour=src))+geom_point()+facet_wrap(~Licensed_Fishing_Area)


####CPUEs
x$Market_kg = ifelse(x$Market_UM=='KGS',x$Market,x$Market/2.204)
x$Canner_kg = ifelse(x$Canner_UM=='KGS',x$Canner,x$Canner/2.204)
x = na.zero(x,cols=c('Market_kg','Canner_kg'))
x$totalwt = x$Market_kg+x$Canner_kg
x = subset(x,Hauled>10 & !is.na(Hauled))
x$CPUE = x$totalwt/x$Hauled

x = subset(x,CPUE<quantile(x$CPUE,0.9995) & Log_Year<2024)#remove the really high

aa = split(x,f=list(x$Licensed_Fishing_Area,x$Log_Year))
aa = rm.from.list(aa)
cpue.lst<-list()
#by time
for(i in 1:length(aa)){
  tmp<-aa[[i]]
  tmp = tmp[,c('Log_Date','totalwt','Hauled')]
  names(tmp)<-c('time','catch','effort')
  tmp$date<-as.Date(tmp$time)
  tt = aggregate(effort~date,data=tmp,FUN=length)
  first.day = min(tt$date[which(tt$effort>50)])
  tmp = subset(tmp,date>=first.day)
  tmp$time<-julian(tmp$date,origin=first.day-1)
  tmp = tmp[order(tmp$time),]
  tmp$time = ceiling(tmp$time) #convert to week of season
  g<-as.data.frame(bio.lobster::biasCorrCPUE(tmp,by.time=T))
  g$lfa=unique(aa[[i]]$Licensed_Fishing_Area)
  g$yr = unique(aa[[i]]$Log_Year)
  g$Date =  as.Date(first.day, format = "%d-%b") + g$t 

  cpue.lst[[i]] <- g
}

cc =as.data.frame(do.call(rbind,cpue.lst))

ggplot(subset(cc,lfa==23),aes(x=t,y=unBCPUE))+geom_point()+geom_smooth()+facet_wrap(~yr)+labs(x='Day of Season',y='CPUE',title='LFA23')
ggplot(subset(cc,lfa==24),aes(x=t,y=unBCPUE))+geom_point()+geom_smooth()+facet_wrap(~yr)+labs(x='Day of Season',y='CPUE',title='LFA24')
ggplot(subset(cc,lfa==25),aes(x=t,y=unBCPUE))+geom_point()+geom_smooth()+facet_wrap(~yr)+labs(x='Day of Season',y='CPUE',title='LFA25')
ggplot(subset(cc,lfa=='26A'),aes(x=t,y=unBCPUE))+geom_point()+geom_smooth()+facet_wrap(~yr)+labs(x='Day of Season',y='CPUE',title='LFA26A')
ggplot(subset(cc,lfa=='26B'),aes(x=t,y=unBCPUE))+geom_point()+geom_smooth()+facet_wrap(~yr)+labs(x='Day of Season',y='CPUE',title='LFA26B')


#not time

cpue.ann<-list()
#by time
for(i in 1:length(aa)){
  tmp<-aa[[i]]
  tmp = tmp[,c('Log_Date','totalwt','Hauled')]
  names(tmp)<-c('time','catch','effort')
  tmp$date<-as.Date(tmp$time)
  first.day<-min(tmp$date)
  tmp$time<-julian(tmp$date,origin=first.day-1)
  tmp = tmp[order(tmp$time),]
  tmp$time = ceiling(tmp$time) #convert to week of season
  tmp = as.data.frame(tmp)
  tmp = subset(tmp,!is.na(effort))
  g<-as.data.frame(t(bio.lobster::biasCorrCPUE(tmp,by.time=F,min.sample.size = 5)))
  g$yr = unique(aa[[i]]$Log_Year)
  g$lfa = unique(aa[[i]]$Licensed_Fishing_Area)
  cpue.ann[[i]] <- g
}

ca =as.data.frame(do.call(rbind,cpue.ann))

ggplot(ca,aes(x=yr,y=unBCPUE))+geom_point()+geom_smooth()+facet_wrap(~lfa)+labs(x='Year',y='CPUE kg/TH')+theme_test(base_size = 14)


